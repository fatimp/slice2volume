(defpackage slice2volume/cli
  (:use #:cl #:command-line-parse)
  (:local-nicknames (#:pf   #:parse-float)
                    (#:load #:slice2volume/loaders)
                    (#:tran #:slice2volume/transforms)
                    (#:opt  #:slice2volume/optimize)
                    (#:norm #:slice2volume/normalize))
  (:export #:slice2volume))
