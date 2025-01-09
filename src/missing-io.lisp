;; Stuff missing in nibbles

(in-package #:numpy-npy)

(declaim (inline flatten))
(defun flatten (array)
  (make-array (array-total-size array)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset 0))

(serapeum:-> unsigned->signed ((unsigned-byte 8))
             (values (signed-byte 8) &optional))
(declaim (inline unsigned->signed))
(defun unsigned->signed (byte)
  (if (< byte 128)
      byte
      (logxor #xff (lognot byte))))

(serapeum:-> signed->unsigned ((signed-byte 8))
             (values (unsigned-byte 8) &optional))
(declaim (inline signed->unsigned))
(defun signed->unsigned (byte)
  (if (< byte 0)
      (1+ (logxor #xff (- byte)))
      byte))


(serapeum:-> read-bit-sequence ((array bit (*)) stream)
             (values (array bit (*)) &optional))
(defun read-bit-sequence (array stream)
  (declare (optimize (speed 3)))
  ;; Read into %ARRAY results in only one call to read(2)
  (let ((%array (make-array (length array) :element-type '(unsigned-byte 8))))
    (read-sequence %array stream)
    (map-into array #'identity %array))
  array)

(serapeum:-> read-ub8-sequence ((array (unsigned-byte 8) (*)) stream)
             (values (array (unsigned-byte 8) (*)) &optional))
(defun read-ub8-sequence (array stream)
  (read-sequence array stream)
  array)

(serapeum:-> read-sb8-sequence ((array (signed-byte 8) (*)) stream)
             (values (array (signed-byte 8) (*)) &optional))
(defun read-sb8-sequence (array stream)
  (declare (optimize (speed 3)))
  (let ((%array (make-array (length array) :element-type '(unsigned-byte 8))))
    (read-sequence %array stream)
    (map-into array #'unsigned->signed %array)))

(serapeum:-> write-bit-sequence ((array bit (*)) stream)
             (values (array bit (*)) &optional))
(defun write-bit-sequence (array stream)
  (write-sequence (map '(vector (unsigned-byte 8)) #'identity array) stream)
  array)

(serapeum:-> write-ub8-sequence ((array (unsigned-byte 8) (*)) stream)
             (values (array (unsigned-byte 8) (*)) &optional))
(defun write-ub8-sequence (array stream)
  (write-sequence array stream))

(serapeum:-> write-sb8-sequence ((array (signed-byte 8) (*)) stream)
             (values (array (signed-byte 8) (*)) &optional))
(defun write-sb8-sequence (array stream)
  (declare (optimize (speed 3)))
  (write-sequence
    (map '(vector (unsigned-byte 8)) #'signed->unsigned array)
    stream)
  array)
