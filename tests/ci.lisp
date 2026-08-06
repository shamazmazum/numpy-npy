(defun do-all()
  (handler-case
      (asdf:load-system :numpy-npy/tests)
    (error () (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "numpy-npy/tests:run-tests")
       0 1)))

(do-all)
