(in-package #:numpy-npy)

(defun load-array-metadata (stream)
  ;; The first 6 bytes are a magic string: exactly \x93NUMPY.
  (unless (and (eql (read-byte stream) #x93)
               (eql (read-byte stream) 78)  ; N
               (eql (read-byte stream) 85)  ; U
               (eql (read-byte stream) 77)  ; M
               (eql (read-byte stream) 80)  ; P
               (eql (read-byte stream) 89)) ; Y
    (error "Not a Numpy file."))
  (let* (;; The next 1 byte is an unsigned byte: the major version number
         ;; of the file format, e.g. \x01.
         (major-version (read-byte stream))
         ;; The next 1 byte is an unsigned byte: the minor version number
         ;; of the file format, e.g. \x00.
         (minor-version (read-byte stream))
         (header-len
          (if (= major-version 1)
              ;; Version 1.0: The next 2 bytes form a little-endian
              ;; unsigned int: the length of the header data HEADER_LEN.
              (logior (ash (read-byte stream) 0)
                      (ash (read-byte stream) 8))
              ;; Version 2.0: The next 4 bytes form a little-endian
              ;; unsigned int: the length of the header data HEADER_LEN.
              (logior (ash (read-byte stream) 0)
                      (ash (read-byte stream) 8)
                      (ash (read-byte stream) 16)
                      (ash (read-byte stream) 24)))))
    (declare (ignore minor-version))
    ;; The next HEADER_LEN bytes form the header data describing the
    ;; array’s format. It is an ASCII string which contains a Python
    ;; literal expression of a dictionary. It is terminated by a newline
    ;; (\n) and padded with spaces (\x20) to make the total of len(magic
    ;; string) + 2 + len(length) + HEADER_LEN be evenly divisible by 64
    ;; for alignment purposes.
    (let ((dict (read-python-object-from-string
                 (let ((buffer (make-string header-len :element-type 'base-char)))
                   (loop for index from 0 below header-len do
                         (setf (schar buffer index) (code-char (read-byte stream))))
                   buffer))))
      (values
       (gethash "shape" dict)
       (dtype-from-code (gethash "descr" dict))
       (gethash "fortran_order" dict)
       (+ header-len (if (= 1 major-version) 10 12))))))

(defun load-array (filename)
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (multiple-value-bind (dimensions dtype fortran-order-p header-octets)
        (load-array-metadata stream)
      (let* ((dimensions (if fortran-order-p (reverse dimensions) dimensions))
             (element-type (dtype-type dtype))
             (array (make-array dimensions :element-type element-type))
             (total-size (array-total-size array))
             (reader (dtype-reader dtype)))
        ;; Skip the header
        (file-position stream header-octets)
        (cond
          ((not (dtype-by-elt-io-p dtype))
           (funcall reader (flatten array) stream))
          ((subtypep element-type 'complex)
           (loop for index below total-size do
                 (setf (row-major-aref array index)
                       (complex
                        (funcall reader stream)
                        (funcall reader stream)))))
          (t
           (loop for index below total-size do
                 (setf (row-major-aref array index)
                       (funcall reader stream)))))
        (if fortran-order-p
            (aops:permute
             (reverse (loop for i below (array-rank array) collect i))
             array)
            array)))))
