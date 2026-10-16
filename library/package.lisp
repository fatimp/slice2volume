(defpackage slice2volume/library
  (:use #:cl)
  (:local-nicknames (#:tiff #:cl-libtiff))
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
   ;; Macros
   #:loop-ranges))
