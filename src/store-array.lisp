(in-package #:numpy-npy)

(defun array-metadata-string (array)
  (with-output-to-string (stream nil :element-type 'base-char)
    (format stream "{'descr': '~A', ~
                     'fortran_order': ~:[False~;True~], ~
                     'shape': (~{~D,~^ ~}), }"
            (dtype-code (dtype-from-type (array-element-type array)))
            nil
            (array-dimensions array))))

(defun store-array (array filename)
  ;; We open the file twice - once with a stream element type of
  ;; (unsigned-byte 8) to write the header, and once with a stream element
  ;; type suitable for writing the array content.
  (let* ((dtype (dtype-from-type (array-element-type array)))
         (writer (dtype-writer dtype))
         (metadata (array-metadata-string array))
         (metadata-length (- (* 64 (ceiling (+ 10 (length metadata)) 64)) 10)))
    (with-open-file (stream filename :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
      (write-sequence #(#x93 78 85 77 80 89) stream) ; The magic string.
      (write-byte 1 stream) ; Major version.
      (write-byte 0 stream) ; Minor version.
      ;; Write the length of the metadata string (2 bytes, little endian).
      (write-byte (ldb (byte 8 0) metadata-length) stream)
      (write-byte (ldb (byte 8 8) metadata-length) stream)
      ;; Write the metadata string.
      (loop for char across metadata do
            (write-byte (char-code char) stream))
      ;; Pad the header with spaces for 64 byte alignment.
      (loop repeat (- metadata-length (length metadata) 1) do
            (write-byte (char-code #\space) stream))
      (write-byte (char-code #\newline) stream) ; Finish with a newline.
        (if (subtypep (array-element-type array) 'complex)
            (loop for index below (array-total-size array)
                  for c = (row-major-aref array index) do
                  (funcall writer (realpart c) stream)
                  (funcall writer (imagpart c) stream))
            (loop for index below (array-total-size array)
                  for c = (row-major-aref array index) do
                  (funcall writer c stream))))))
