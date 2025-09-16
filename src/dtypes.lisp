(in-package #:numpy-npy)

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

(serapeum:defconstructor dtype
  (code        string)
  (type        (or symbol list))
  (reader      function)
  (writer      function)
  (endianness  endianness))

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

(defun define-dtype (code type reader writer
                     &optional (endianness +endianness+))
  (let ((dtype (dtype code type reader writer endianness)))
    (pushnew dtype *dtypes* :key #'dtype-code :test #'string=)
    dtype))

(defun define-multibyte-dtype (code type
                               reader/le writer/le
                               reader/be writer/be)
  (define-dtype (concatenate 'string "<" code) type
    reader/le writer/le :little-endian)
  (define-dtype (concatenate 'string ">" code) type
    reader/be writer/be :big-endian)
  (let ((default-reader (select-io-function reader/le reader/be))
        (default-writer (select-io-function writer/le writer/be)))
    (define-dtype code type default-reader default-writer +endianness+)
    (define-dtype (concatenate 'string "|" code)
        type default-reader default-writer +endianness+)
    (define-dtype (concatenate 'string "=" code)
        type default-reader default-writer +endianness+)))

(define-dtype "?" 'bit #'read-bit-sequence #'write-bit-sequence)
(define-dtype "b" '(unsigned-byte 8) #'read-ub8-sequence #'write-ub8-sequence)
;; This is how numpy really saves arrays of bits
(define-multibyte-dtype "b1" 'bit
  #'read-bit-sequence #'write-bit-sequence
  #'read-bit-sequence #'write-bit-sequence)
(define-multibyte-dtype "i1" '(signed-byte 8)
  #'read-sb8-sequence #'write-sb8-sequence
  #'read-sb8-sequence #'write-sb8-sequence)
(define-multibyte-dtype "i2" '(signed-byte 16)
  #'nibbles:read-sb16/le-into-sequence #'nibbles:write-sb16/le-sequence
  #'nibbles:read-sb16/be-into-sequence #'nibbles:write-sb16/be-sequence)
(define-multibyte-dtype "i4" '(signed-byte 32)
  #'nibbles:read-sb32/le-into-sequence #'nibbles:write-sb32/le-sequence
  #'nibbles:read-sb32/be-into-sequence #'nibbles:write-sb32/be-sequence)
(define-multibyte-dtype "i8" '(signed-byte 64)
  #'nibbles:read-sb64/le-into-sequence #'nibbles:write-sb64/le-sequence
  #'nibbles:read-sb64/be-into-sequence #'nibbles:write-sb64/be-sequence)
(define-multibyte-dtype "u1" '(unsigned-byte 8)
  #'read-ub8-sequence #'write-ub8-sequence
  #'read-ub8-sequence #'write-ub8-sequence)
(define-multibyte-dtype "u2" '(unsigned-byte 16)
  #'nibbles:read-ub16/le-into-sequence #'nibbles:write-ub16/le-sequence
  #'nibbles:read-ub16/be-into-sequence #'nibbles:write-ub16/be-sequence)
(define-multibyte-dtype "u4" '(unsigned-byte 32)
  #'nibbles:read-ub32/le-into-sequence #'nibbles:write-ub32/le-sequence
  #'nibbles:read-ub32/be-into-sequence #'nibbles:write-ub32/be-sequence)
(define-multibyte-dtype "u8" '(unsigned-byte 64)
  #'nibbles:read-ub64/le-into-sequence #'nibbles:write-ub64/le-sequence
  #'nibbles:read-ub64/be-into-sequence #'nibbles:write-ub64/be-sequence)
(define-multibyte-dtype "f4" 'single-float
  #'nibbles:read-ieee-single/le-into-sequence #'nibbles:write-ieee-single/le-sequence
  #'nibbles:read-ieee-single/be-into-sequence #'nibbles:write-ieee-single/be-sequence)
(define-multibyte-dtype "f8" 'double-float
  #'nibbles:read-ieee-double/le-into-sequence #'nibbles:write-ieee-double/le-sequence
  #'nibbles:read-ieee-double/be-into-sequence #'nibbles:write-ieee-double/be-sequence)
(define-multibyte-dtype "c8" '(complex single-float)
  #'nibbles:read-ieee-single/le-into-sequence #'nibbles:write-ieee-single/le-sequence
  #'nibbles:read-ieee-single/be-into-sequence #'nibbles:write-ieee-single/be-sequence)
(define-multibyte-dtype "c16" '(complex double-float)
  #'nibbles:read-ieee-double/le-into-sequence #'nibbles:write-ieee-double/le-sequence
  #'nibbles:read-ieee-double/be-into-sequence #'nibbles:write-ieee-double/be-sequence)

;; Finally, let's sort *dtypes* such that type queries always find the most
;; specific entry first.
(setf *dtypes* (stable-sort *dtypes* #'subtypep :key #'dtype-type))
