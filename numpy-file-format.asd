(defsystem :numpy-file-format
  :description "Read and write Numpy .npy and .npz files."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "dtypes")
               (:file "python-parser")
               (:file "load-array")
               (:file "store-array"))
  :depends-on (:nibbles :trivial-features :serapeum)
  :in-order-to ((test-op (test-op :numpy-file-format/tests))))

(defsystem :numpy-file-format/tests
  :depends-on (:numpy-file-format :uiop)
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :numpy-file-format/tests '#:run)))
