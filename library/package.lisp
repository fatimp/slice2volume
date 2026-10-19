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
   #:trans-param
   ;; Constructors
   #:make-trans-param
   ;; Accessors
   #:trans-param-α
   #:trans-param-Δu
   #:trans-param-Δv
   #:trans-param-d
   #:trans-param-ϕ
   #:trans-param-ψ
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
   #:store-trans-param
   #:load-trans-param
   ;; Macros
   #:loop-ranges))
