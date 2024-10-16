(in-package #:numpy-file-format)

(deftype endianness () '(member :little-endian :big-endian))

(declaim (type endianness +endianness+))
(defconstant +endianness+
  #+little-endian :little-endian
  #+big-endian  :big-endian)

(declaim (inline select-io-function))
(defun select-io-function (le be)
  (declare (ignorable le be))
  #+little-endian le
  #+big-endian    be)

(defparameter *dtypes* '())

(defstruct (dtype
             (:constructor dtype (code type reader writer endianness)))
  (code       ""           :type string)
  (type       nil          :type (or symbol list))
  (reader     #'identity   :type function)
  (writer     #'identity   :type function)
  (endianness +endianness+ :type endianness))

(defmethod print-object ((dtype dtype) stream)
  (print-unreadable-object (dtype stream :type t)
    (prin1 (dtype-code dtype) stream)))

(defun dtype-from-code (code)
  (or (find code *dtypes* :key #'dtype-code :test #'string=)
      (error "Cannot find dtype for the code ~S." code)))

(defun dtype-from-type (type)
  (or (find-if
       (lambda (dtype)
         (and (eq (dtype-endianness dtype) +endianness+)
              (subtypep type (dtype-type dtype))))
       *dtypes*)
      (error "Cannot find dtype for type ~S." type)))

(defun define-dtype (code type reader writer &optional (endianness +endianness+))
  (let ((dtype (dtype code type reader writer endianness)))
    (pushnew dtype *dtypes* :key #'dtype-code :test #'string=)
    dtype))

(defun define-multibyte-dtype (code type reader/le writer/le reader/be writer/be)
  (define-dtype (concatenate 'string "<" code) type
    reader/le writer/le :little-endian)
  (define-dtype (concatenate 'string ">" code) type
    reader/be writer/be :big-endian)
  (let ((default-reader (select-io-function reader/le reader/be))
        (default-writer (select-io-function writer/le writer/be)))
    (define-dtype code type default-reader default-writer +endianness+)
    (define-dtype (concatenate 'string "|" code)
        type default-reader default-writer +endianness+)
    (define-dtype (concatenate 'string "=" code) type
      default-reader default-writer +endianness+)))

(declaim (inline unsigned->signed))
(defun unsigned->signed (byte)
  (if (< byte 128)
      byte
      (logxor #xff (lognot byte))))

(defun read-signed-byte (stream)
  (declare (optimize (speed 3)))
  (let ((byte (read-byte stream)))
    (declare (type (unsigned-byte 8) byte))
    (unsigned->signed byte)))

(declaim (inline signed->unsigned))
(defun signed->unsigned (byte)
  (if (< byte 0)
      (1+ (logxor #xff (- byte)))
      byte))

(defun write-signed-byte (byte stream)
  (declare (optimize (speed 3))
           (type (signed-byte 8) byte))
  (write-byte (signed->unsigned byte) stream))

(define-dtype "?" 'bit #'read-byte #'write-byte)
(define-dtype "b" '(unsigned-byte 8) #'read-byte #'write-byte)
(define-multibyte-dtype "i1" '(signed-byte 8)
  #'read-signed-byte #'write-signed-byte
  #'read-signed-byte #'write-signed-byte)
(define-multibyte-dtype "i2" '(signed-byte 16)
  #'nibbles:read-sb16/le #'nibbles:write-sb16/le
  #'nibbles:read-sb16/be #'nibbles:write-sb16/be)
(define-multibyte-dtype "i4" '(signed-byte 32)
  #'nibbles:read-sb32/le #'nibbles:write-sb32/le
  #'nibbles:read-sb32/be #'nibbles:write-sb32/be)
(define-multibyte-dtype "i8" '(signed-byte 64)
  #'nibbles:read-sb64/le #'nibbles:write-sb64/le
  #'nibbles:read-sb64/be #'nibbles:write-sb64/be)
(define-multibyte-dtype "u1" '(unsigned-byte 8)
  #'read-byte #'write-byte
  #'read-byte #'write-byte)
(define-multibyte-dtype "u2" '(unsigned-byte 16)
  #'nibbles:read-ub16/le #'nibbles:write-ub16/le
  #'nibbles:read-ub16/be #'nibbles:write-ub16/be)
(define-multibyte-dtype "u4" '(unsigned-byte 32)
  #'nibbles:read-ub32/le #'nibbles:write-ub32/le
  #'nibbles:read-ub32/be #'nibbles:write-ub32/be)
(define-multibyte-dtype "u8" '(unsigned-byte 64)
  #'nibbles:read-ub64/le #'nibbles:write-ub64/le
  #'nibbles:read-ub64/be #'nibbles:write-ub64/be)
(define-multibyte-dtype "f4" 'single-float
  #'nibbles:read-ieee-single/le #'nibbles:write-ieee-single/le
  #'nibbles:read-ieee-single/be #'nibbles:write-ieee-single/be)
(define-multibyte-dtype "f8" 'double-float
  #'nibbles:read-ieee-double/le #'nibbles:write-ieee-double/le
  #'nibbles:read-ieee-double/be #'nibbles:write-ieee-double/be)
(define-multibyte-dtype "c8" '(complex single-float)
  #'nibbles:read-ieee-single/le #'nibbles:write-ieee-single/le
  #'nibbles:read-ieee-single/be #'nibbles:write-ieee-single/be)
(define-multibyte-dtype "c16" '(complex double-float)
  #'nibbles:read-ieee-double/le #'nibbles:write-ieee-double/le
  #'nibbles:read-ieee-double/be #'nibbles:write-ieee-double/be)

;; Finally, let's sort *dtypes* such that type queries always find the most
;; specific entry first.
(setf *dtypes* (stable-sort *dtypes* #'subtypep :key #'dtype-type))
