(defun do-all()
  (ql:quickload :numpy-npy/tests)
  (uiop:quit
   (if (uiop:call-function "numpy-npy/tests:run-tests")
       0 1)))

(do-all)
