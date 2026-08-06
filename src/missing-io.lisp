;; Stuff missing in nibbles

(in-package #:numpy-npy)

#+sbcl
(deftype io-vector (type) `(simple-array ,type (*)))
#-sbcl
(deftype io-vector (type) `(array ,type (*)))

(declaim (inline flatten))
(defun flatten (array)
  #+sbcl
  (sb-ext:array-storage-vector array)
  #-sbcl
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

(serapeum:-> aref-bit ((simple-array (unsigned-byte 8) (*)) unsigned-byte)
             (values bit &optional))
(declaim (inline aref-bit))
(defun aref-bit (v i)
  (aref v i))

(serapeum:-> (setf aref-bit) (bit (simple-array (unsigned-byte 8) (*)) unsigned-byte)
             (values bit &optional))
(declaim (inline (setf aref-bit)))
(defun (setf aref-bit) (x v i)
  (setf (aref v i) x))

(serapeum:-> sb8ref ((simple-array (unsigned-byte 8) (*)) unsigned-byte)
             (values (signed-byte 8) &optional))
(declaim (inline sb8ref))
(defun sb8ref (v i)
  (unsigned->signed
   (aref v i)))

(serapeum:-> (setf sb8ref) ((signed-byte 8)
                            (simple-array (unsigned-byte 8) (*))
                            unsigned-byte)
             (values (signed-byte 8) &optional))
(declaim (inline (setf sb8ref)))
(defun (setf sb8ref) (x v i)
  (setf (aref v i)
        (signed->unsigned x))
  x)

(serapeum:-> read-ub8-into-vector ((io-vector (unsigned-byte 8)) stream)
             (values (io-vector (unsigned-byte 8)) &optional))
(defun read-ub8-into-vector (array stream)
  (declare (optimize (speed 3)))
  (unless (= (read-sequence array stream)
             (length array))
    (error 'npy-eof))
  array)

(serapeum:-> write-ub8-vector ((io-vector (unsigned-byte 8)) stream)
             (values (io-vector (unsigned-byte 8)) &optional))
(defun write-ub8-vector (array stream)
  (write-sequence array stream))

;; All writes from sequence / read to sequence stuff is shit in nibbles
(defconstant +buffer-length+ 1000)

(macrolet ((def-reader (name type elt-size f)
             `(progn
                (serapeum:-> ,name ((io-vector ,type) stream)
                             (values (io-vector ,type) &optional))
                (defun ,name (vector stream)
                  (declare (optimize (speed 3)))
                  (let* ((element-size ,elt-size)
                         (input-length  (length vector))
                         (buffer-length (min input-length +buffer-length+))
                         (buffer (make-array (* buffer-length element-size)
                                             :element-type '(unsigned-byte 8)))
                         (read-elements 0))
                    (loop for bytes-read = (read-sequence
                                            buffer stream
                                            :end (min (length buffer)
                                                      (* (- input-length read-elements)
                                                         element-size)))
                          until (zerop bytes-read) do
                            (assert (zerop (mod bytes-read element-size)))
                            (loop repeat (floor bytes-read element-size)
                                  for j from 0 by element-size
                                  do (setf (aref vector read-elements)
                                           (,f buffer j))
                                     (incf read-elements))
                          finally (unless (= read-elements input-length)
                                    (error 'npy-eof)))
                    vector)))))

  ;; Bits
  (def-reader read-bit-into-vector bit 1 aref-bit)

  ;; Unsigned
  (def-reader read-ub16/le-into-vector (unsigned-byte 16) 2 nibbles:ub16ref/le)
  (def-reader read-ub16/be-into-vector (unsigned-byte 16) 2 nibbles:ub16ref/be)
  (def-reader read-ub32/le-into-vector (unsigned-byte 32) 4 nibbles:ub32ref/le)
  (def-reader read-ub32/be-into-vector (unsigned-byte 32) 4 nibbles:ub32ref/be)
  (def-reader read-ub64/le-into-vector (unsigned-byte 64) 8 nibbles:ub64ref/le)
  (def-reader read-ub64/be-into-vector (unsigned-byte 64) 8 nibbles:ub64ref/be)

  ;; Signed
  (def-reader read-sb8-into-vector     (signed-byte  8) 1 sb8ref)
  (def-reader read-sb16/le-into-vector (signed-byte 16) 2 nibbles:sb16ref/le)
  (def-reader read-sb16/be-into-vector (signed-byte 16) 2 nibbles:sb16ref/be)
  (def-reader read-sb32/le-into-vector (signed-byte 32) 4 nibbles:sb32ref/le)
  (def-reader read-sb32/be-into-vector (signed-byte 32) 4 nibbles:sb32ref/be)
  (def-reader read-sb64/le-into-vector (signed-byte 64) 8 nibbles:sb64ref/le)
  (def-reader read-sb64/be-into-vector (signed-byte 64) 8 nibbles:sb64ref/be)

  ;; Float
  (def-reader read-single/le-into-vector single-float 4 nibbles:ieee-single-ref/le)
  (def-reader read-single/be-into-vector single-float 4 nibbles:ieee-single-ref/be)
  (def-reader read-double/le-into-vector double-float 8 nibbles:ieee-double-ref/le)
  (def-reader read-double/be-into-vector double-float 8 nibbles:ieee-double-ref/be))


(macrolet ((def-writer (name type elt-size f)
             `(progn
                (serapeum:-> ,name ((io-vector ,type) stream)
                             (values (io-vector ,type) &optional))
                (defun ,name (vector stream)
                  (declare (optimize (speed 3)))
                  (let* ((element-size ,elt-size)
                         (input-length  (length vector))
                         (buffer (make-array (* +buffer-length+ element-size)
                                             :element-type '(unsigned-byte 8)))
                         (written-elements 0))
                    (declare (type fixnum written-elements))
                    (loop for need-to-write = (min +buffer-length+
                                                   (- input-length written-elements))
                          until (= written-elements input-length) do
                            (loop for i below need-to-write
                                  for j from 0 by element-size do
                                    (setf (,f buffer j)
                                          (aref vector (+ i written-elements))))
                            (write-sequence buffer stream
                                            :end (* need-to-write element-size))
                            (incf written-elements need-to-write)))
                  vector))))

  ;; Bits
  (def-writer write-bit-vector bit 1 aref-bit)

  ;; Unsigned
  (def-writer write-ub16/le-vector (unsigned-byte 16) 2 nibbles:ub16ref/le)
  (def-writer write-ub16/be-vector (unsigned-byte 16) 2 nibbles:ub16ref/be)
  (def-writer write-ub32/le-vector (unsigned-byte 32) 4 nibbles:ub32ref/le)
  (def-writer write-ub32/be-vector (unsigned-byte 32) 4 nibbles:ub32ref/be)
  (def-writer write-ub64/le-vector (unsigned-byte 64) 8 nibbles:ub64ref/le)
  (def-writer write-ub64/be-vector (unsigned-byte 64) 8 nibbles:ub64ref/be)

  ;; Signed
  (def-writer write-sb8-vector     (signed-byte  8) 1 sb8ref)
  (def-writer write-sb16/le-vector (signed-byte 16) 2 nibbles:sb16ref/le)
  (def-writer write-sb16/be-vector (signed-byte 16) 2 nibbles:sb16ref/be)
  (def-writer write-sb32/le-vector (signed-byte 32) 4 nibbles:sb32ref/le)
  (def-writer write-sb32/be-vector (signed-byte 32) 4 nibbles:sb32ref/be)
  (def-writer write-sb64/le-vector (signed-byte 64) 8 nibbles:sb64ref/le)
  (def-writer write-sb64/be-vector (signed-byte 64) 8 nibbles:sb64ref/be)

  ;; Float
  (def-writer write-single/le-vector single-float 4 nibbles:ieee-single-ref/le)
  (def-writer write-single/be-vector single-float 4 nibbles:ieee-single-ref/be)
  (def-writer write-double/le-vector double-float 8 nibbles:ieee-double-ref/le)
  (def-writer write-double/be-vector double-float 8 nibbles:ieee-double-ref/be))
