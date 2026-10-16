(defsystem :slice2volume/ui
    :name :slice2volume/ui
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool for slice 2 volume registration (UI)"
    :license "2-clause BSD"
    :serial t
    :pathname "ui"
    :components ((:file "package")
                 (:file "conditions")
                 (:file "util")
                 (:file "loaders")
                 (:file "shaders")
                 (:file "downsample")
                 (:file "cube-vertices")
                 (:file "renderer")
                 (:file "application"))
    :depends-on (:cl-gtk4
                 :varjo
                 :cl-opengl
                 :rtg-math
                 :serapeum
                 :alexandria
                 :numpy-npy
                 :cl-libtiff)
    :build-operation program-op
    :build-pathname "slice2volume-ui"
    :entry-point "slice2volume/ui:slice2volume")

(defsystem :slice2volume
  :name :slice2volume
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A tool for slice 2 volume registration (UI)"
  :license "2-clause BSD"
  :depends-on (:slice2volume/ui))
