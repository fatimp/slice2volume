(defsystem :slice2volume
    :name :slice2volume
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool for slice 2 volume registration"
    :license "2-clause BSD"
    :serial t
    :pathname "library"
    :class :package-inferred-system
    :depends-on (:serapeum
                 :alexandria
                 :numpy-npy
                 :cl-libtiff
                 :rtg-math
                 :float-features
                 :cl-store
                 :slice2volume/loaders
                 :slice2volume/transforms
                 :slice2volume/downsample
                 :slice2volume/slicer
                 :slice2volume/histogram))

(defsystem :slice2volume/ui
    :name :slice2volume/ui
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool for slice 2 volume registration (UI)"
    :license "2-clause BSD"
    :serial t
    :pathname "ui"
    :components ((:file "package")
                 (:file "util")
                 (:file "shaders")
                 (:file "cube-vertices")
                 (:file "renderer")
                 (:file "application"))
    :depends-on (:cl-gtk4
                 :varjo
                 :cl-opengl
                 :serapeum
                 :alexandria
                 :slice2volume)
    :build-operation program-op
    :build-pathname "slice2volume-ui"
    :entry-point "slice2volume/ui:slice2volume")
