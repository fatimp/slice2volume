(defpackage slice2volume/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:noise #:cl-value-noise)
                    (#:norm  #:slice2volume/normalize)
                    (#:mi    #:slice2volume/mutual-information))
  (:export #:run-tests))
