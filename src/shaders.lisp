(in-package :slice2volume)

(macrolet ((frob (type)
             `(varjo:define-vari-function texcoord ((space-coord ,type))
                (let ((res (/ (1+ space-coord) 2)))
                  (setf (vari:swizzle res :y)
                        (- 1 (vari:swizzle res :y)))
                  res))))
  (frob :vec2)
  (frob :vec3))

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
   '((coord   :vec2))       ; Texture coordinate
   '((sampler :sampler-2d)) ; Texture sampler
   '(:330)
   `((let ((color (vari:swizzle (vari:texture sampler (texcoord coord)) :r)))
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
   '((coord   :vec3))       ; Texture coordinate
   '((sampler :sampler-3d)) ; Texture sampler
   '(:330)
   `((let* ((coord (texcoord coord) 2)
            (clamped (vari:clamp coord (vari:vec3 0) (vari:vec3 1))))
       (if (= coord clamped)
           (let ((color (vari:swizzle (vari:texture sampler coord) :r)))
             (vari:vec4 color color color 1))
           (vari:discard))))))

(defparameter *volume-program*
  (varjo:rolling-translate
   (list *volume-vertex-shader*
         *volume-fragment-shader*)))

(declaim (type varjo.internals:vertex-stage *cube-vertex-shader*))
(defparameter *cube-vertex-shader*
  (varjo:make-stage
   :vertex
   '((vertex     :vec3)) ; Cube vertex coordinate
   '((projection :mat4)) ; Projection to the screen
   '(:330)
   `((* projection (vari:vec4 vertex 1)))))

(declaim (type varjo.internals:fragment-stage *cube-fragment-shader*))
(defparameter *cube-fragment-shader*
  (varjo:make-stage
   :fragment
   '() ; No inputs
   '() ; No uniforms
   '(:330)
   `((vari:vec4 1.0 0.0 0.0 1.0))))

(defparameter *cube-program*
  (varjo:rolling-translate
   (list *cube-vertex-shader*
         *cube-fragment-shader*)))

(declaim (type varjo.internals:vertex-stage *plane-vertex-shader*))
(defparameter *plane-vertex-shader*
  (varjo:make-stage
   :vertex
   '()                        ; No inputs
   '((world-transform :mat3)  ; Planar space to world coordinates
     (dist            :float) ; Distance to the cutting plane from the origin
     (projection      :mat4)) ; Projection to the screen
   '(:330)
   `((let* ((square (vector
                     (vari:vec2 -3 -3)
                     (vari:vec2 +3 -3)
                     (vari:vec2 -3 +3)
                     (vari:vec2 +3 +3)))
            (v (aref square vari:gl-vertex-id))
            (vaug  (vari:vec3 v dist))
            (world (* world-transform vaug)))
       (values (* projection (vari:vec4 world 1))
               world)))))

(declaim (type varjo.internals:fragment-stage *plane-fragment-shader*))
(defparameter *plane-fragment-shader*
  (varjo:make-stage
   :fragment
   '((coord :vec3)) ; Fragment coordinate
   '()
   '(:330)
   `((let ((clamped (vari:clamp coord (vari:vec3 -1) (vari:vec3 1))))
       (if (= coord clamped)
           (vari:vec4 0 0 1 1)
           (vari:discard))))))

(defparameter *plane-program*
  (varjo:rolling-translate
   (list *plane-vertex-shader*
         *plane-fragment-shader*)))
