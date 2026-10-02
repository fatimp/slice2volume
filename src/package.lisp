(defpackage slice2volume
  (:use #:cl)
  (:local-nicknames (#:tiff #:cl-libtiff)
                    (#:v3   #:rtg-math.vector3)
                    (#:m3   #:rtg-math.matrix3))
  (:export #:slice2volume))
