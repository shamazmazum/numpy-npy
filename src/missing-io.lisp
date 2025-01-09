;; Stuff missing in nibbles

(in-package #:numpy-npy)

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
