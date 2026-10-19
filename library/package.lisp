(defpackage slice2volume/library
  (:use #:cl)
  (:local-nicknames (#:tiff #:cl-libtiff)
                    (#:v3   #:rtg-math.vector3)
                    (#:m3   #:rtg-math.matrix3)
                    (#:m4   #:rtg-math.matrix4)
                    (#:ff   #:float-features))
  (:export
   ;; Types
   #:image
   #:loader
   #:slice-loader
   #:volume-loader
   ;; Conditions
   #:slice2volume-error
   #:loader-error
   ;; Functions
   #:downsample-3d
   #:loaders
   #:load-volume
   #:load-slice
   #:loader-type
   #:loader-description
   #:plane-world-transform
   #:plane-normal
   #:choose-tangent-vector
   #:world-transform-matrix
   ;; Macros
   #:loop-ranges))
