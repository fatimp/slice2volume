(in-package :slice2volume/ui)

(alexandria:define-constant +cube-vertices+
  #a(24 single-float
        . (
           -1.0 -1.0 -1.0
           +1.0 -1.0 -1.0
           -1.0 +1.0 -1.0
           +1.0 +1.0 -1.0
           -1.0 -1.0 +1.0
           +1.0 -1.0 +1.0
           -1.0 +1.0 +1.0
           +1.0 +1.0 +1.0))
  :test #'equalp)

(alexandria:define-constant +cube-indices+
  #a(24 (unsigned-byte 32)
        . (
           0 1 1 5 5 4 4 0
           2 3 3 7 7 6 6 2
           0 2 1 3 4 6 5 7))
  :test #'equalp)
