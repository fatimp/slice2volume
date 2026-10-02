(in-package :slice2volume)

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
         (values vertex4 vertex2)))))

(declaim (type varjo.internals:fragment-stage *slice-fragment-shader*))
(defparameter *slice-fragment-shader*
  (varjo:make-stage
   :fragment
   '((coord :vec2))         ; Texture coordinate
   '((sampler :sampler-2d)) ; Texture sampler
   '(:330)
   `((let ((color (vari:swizzle (vari:texture sampler (/ (1+ coord) 2)) :r)))
       (vari:vec4 color color color 1)))))

(defparameter *slice-program*
  (varjo:rolling-translate
   (list *slice-vertex-shader*
         *slice-fragment-shader*)))

(declaim (type varjo.internals:vertex-stage *volume-vertex-shader*))
(defparameter *volume-vertex-shader*
  (varjo:make-stage
   :vertex
   '()                          ; No inputs
   '((planar-transform :mat3)   ; Planar space transform
     (world-transform  :mat3)   ; Planar space to world coordinates
     (dist             :float)) ; Distance to the cutting plane from the origin
   '(:330)
   `((let* ((square (vector
                     (vari:vec2 -1 -1)
                     (vari:vec2 +1 -1)
                     (vari:vec2 -1 +1)
                     (vari:vec2 +1 +1)))
            (v (aref square vari:gl-vertex-id))
            (vaug  (vari:vec3 v 1))
            (vtran (* planar-transform vaug))
            (world (* world-transform (* vtran (vari:vec3 1 1 dist)))))
       (values (vari:vec4 v 0 1)
               world)))))

(declaim (type varjo.internals:fragment-stage *volume-fragment-shader*))
(defparameter *volume-fragment-shader*
  (varjo:make-stage
   :fragment
   '((coord :vec3))         ; Texture coordinate
   '((sampler :sampler-3d)) ; Texture sampler
   '(:330)
   `((let ((color (vari:swizzle (vari:texture sampler (/ (1+ coord) 2)) :r)))
       (vari:vec4 color color color 1)))))

(defparameter *volume-program*
  (varjo:rolling-translate
   (list *volume-vertex-shader*
         *volume-fragment-shader*)))
