(defsystem :numpy-npy
  :description "Read and write Numpy .npy and .npz files."
  :author "Marco Heisig <marco.heisig@fau.de>, Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "MIT"
  :serial t
  :pathname "src"
  :version "0.3"
  :components ((:file "package")
               (:file "missing-io")
               (:file "dtypes")
               (:file "python-parser")
               (:file "load-array")
               (:file "store-array"))
  :depends-on (:nibbles :trivial-features :serapeum :array-operations)
  :in-order-to ((test-op (test-op :numpy-npy/tests))))

(defsystem :numpy-npy/tests
  :depends-on (:numpy-npy :fiveam :uiop)
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :numpy-npy/tests '#:run-tests)))
