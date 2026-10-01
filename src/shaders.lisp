(in-package :slice2volume)

(varjo:define-vari-function space2texture ((coord :vec2))
  (/ (1+ coord) 2))

(declaim (type varjo.internals:vertex-stage *slice-vertex-shader*))
(defparameter *slice-vertex-shader*
  (varjo:make-stage
   :vertex
   '() ; No inputs
   '() ; No uniforms
   '(:330)
   `((let* ((square (vector
                     (vari:vec2 -1 -1)
                     (vari:vec2 +1 -1)
                     (vari:vec2 -1 +1)
                     (vari:vec2 +1 +1)))
            (vertex2 (aref square vari:gl-vertex-id))
            (vertex4 (vari:vec4 vertex2 0 1)))
       (values vertex4 (space2texture vertex2))))))

(declaim (type varjo.internals:fragment-stage *slice-fragment-shader*))
(defparameter *slice-fragment-shader*
  (varjo:make-stage
   :fragment
   '((coord :vec2))         ; Texture coordinate
   '((sampler :sampler-2d)) ; Texture sampler
   '(:330)
   `((let ((color (vari:swizzle (vari:texture sampler coord) :r)))
       (vari:vec4 color color color 1)))))

(defparameter *slice-program*
  (varjo:rolling-translate
   (list *slice-vertex-shader*
         *slice-fragment-shader*)))
