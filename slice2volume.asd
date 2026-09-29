(defsystem :slice2volume
    :name :slice2volume
    :version "0.1.0"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool for slice 2 volume registration"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "application"))
    :depends-on (:cl-gtk4
                 :varjo
                 :cl-opengl
                 :rtg-math
                 :serapeum
                 :numpy-npy)
    :build-operation program-op
    :build-pathname "slice2volume"
    :entry-point "slice2volume:slice2volume")
