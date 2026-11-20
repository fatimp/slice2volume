(defun do-all()
  (handler-case
      (asdf:load-system :slice2volume/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "slice2volume/tests:run-tests" t)
       0 1)))

(do-all)
