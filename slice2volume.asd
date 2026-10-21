(defsystem :slice2volume
    :name :slice2volume
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool for slice 2 volume registration (Library)"
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
                 :cl-optim
                 :slice2volume/loaders
                 :slice2volume/transforms
                 :slice2volume/downsample
                 :slice2volume/slicer
                 :slice2volume/normalize
                 :slice2volume/mutual-information
                 :slice2volume/optimize)
    :in-order-to ((test-op (load-op "slice2volume/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :slice2volume/tests))))))

(defsystem :slice2volume/tests
  :name :slice2volume/tests
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:slice2volume :fiveam :cl-value-noise))

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

(defsystem :slice2volume/cli
    :name :slice2volume/cli
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool for slice 2 volume registration (CLI tool)"
    :license "2-clause BSD"
    :serial t
    :pathname "cli"
    :components ((:file "package")
                 (:file "cli"))
    :depends-on (:command-line-parse
                 :parse-float
                 :slice2volume)
    :build-operation program-op
    :build-pathname "slice2volume-cli"
    :entry-point "slice2volume/cli:slice2volume")
