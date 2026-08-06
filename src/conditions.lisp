(in-package :numpy-npy)

(define-condition npy-error (error)
  ()
  (:documentation "Generic NPY IO error"))

(define-condition npy-eof (npy-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "EOF reached")))
  (:documentation "Signaled when reading and the end of file is
reached without all data being read"))

(define-condition npy-invalid (npy-error)
  ((character :initarg  :character
              :initform nil
              :reader   npy-invalid-character))
  (:report (lambda (c s)
             (if (npy-invalid-character c)
                 (format s "Invalid character: ~s"
                         (npy-invalid-character c))
                 (format s "Not a NPY file"))))
  (:documentation "Signaled when reading an invalid file"))

(define-condition wrong-dtype (npy-error)
  ((type :initarg  :type
         :initform nil
         :reader   wrong-dtype-type)
   (code :initarg  :code
         :initform nil
         :reader   wrong-dtype-code))
  (:report (lambda (c s)
             (if (wrong-dtype-code c)
                 (format s "Unknown dtype code: ~a"
                         (wrong-dtype-code c))
                 (format s "Unknown dtype for lisp type: ~a"
                         (wrong-dtype-type c)))))
  (:documentation "Signaled when unknown dtype is encountered"))

(define-condition npy-warning (warning)
  ()
  (:documentation "Generic NPY warning"))

(define-condition column-major-order (npy-warning)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "An array is in column-major order.
 Transposition is currently slow.")))
  (:documentation "Signaled when reading an array in column-major order"))
