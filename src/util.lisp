(in-package :slice2volume)

(defun create-shader (stage compiled-shader)
  (let ((shader (gl:create-shader stage)))
    (gl:shader-source shader (varjo:glsl-code compiled-shader))
    (gl:compile-shader shader)
    (let ((status (gl:get-shader shader :compile-status)))
      (unless status
        (error "Shader compile failure: ~a ~a"
               shader
               (gl:get-shader-info-log shader))))
    shader))

(defun create-program (shaders)
  (destructuring-bind (vertex fragment)
      shaders
    (let ((program   (gl:create-program))
          (svertex   (create-shader :vertex-shader   vertex))
          (sfragment (create-shader :fragment-shader fragment)))
      (gl:attach-shader program svertex)
      (gl:attach-shader program sfragment)
      (gl:link-program  program)
      (gl:detach-shader program svertex)
      (gl:detach-shader program sfragment)
      (gl:delete-shader svertex)
      (gl:delete-shader sfragment)

      (let ((status (gl:get-program program :link-status)))
        (unless status
          (error "Program linkage failure: ~a"
                 (gl:get-program-info-log program))))
      program)))

(serapeum:-> fast-upload-slice ((simple-array (unsigned-byte 8) (* *)))
             (values &optional))
(defun fast-upload-slice (array)
  (declare (optimize (speed 3)))
  (let ((h (array-dimension array 0))
        (w (array-dimension array 1)))
    (gl:tex-storage-2d :texture-2d 1 :r8 w h)
    (cffi:with-pointer-to-vector-data (ptr (sb-ext:array-storage-vector array))
      (gl:tex-sub-image-2d :texture-2d 0 0 0 w h
                           :red :unsigned-byte ptr))))

(serapeum:-> fast-upload-volume ((simple-array (unsigned-byte 8) (* * *)))
             (values &optional))
(defun fast-upload-volume (array)
  (declare (optimize (speed 3)))
  (let ((d (array-dimension array 0))
        (h (array-dimension array 1))
        (w (array-dimension array 2)))
    (gl:tex-storage-3d :texture-3d 1 :r8 w h d)
    (cffi:with-pointer-to-vector-data (ptr (sb-ext:array-storage-vector array))
      (gl:tex-sub-image-3d :texture-3d 0 0 0 0 w h d
                           :red :unsigned-byte ptr))))

(serapeum:-> planar-transform-matrix (single-float single-float single-float)
             (values rtg-math.types:mat3 &optional))
(defun planar-transform-matrix (ϕ Δu Δv)
  (let ((cos (cos ϕ))
        (sin (sin ϕ)))
    (m3:make cos (- sin) Δu (+ sin) cos Δv 0.0 0.0 1.0)))

(serapeum:-> world-transform-matrix (rtg-math.types:vec3 rtg-math.types:vec3)
             (values rtg-math.types:mat3 &optional))
(defun world-transform-matrix (v n)
  (let* ((n  (v3:normalize n))
         (v2 v)
         (τ2 (v3:normalize (v3:- v2 (v3:*s n (v3:dot n v2)))))
         (τ1 (v3:cross τ2 n)))
    (m3:from-columns τ1 τ2 n)))

(serapeum:-> randomization-needed-p (rtg-math.types:vec3 rtg-math.types:vec3)
             (values boolean &optional))
(defun randomization-needed-p (v n)
  (let ((n (v3:normalize n))
        (v (v3:normalize v)))
    (> (abs (v3:dot n v)) 0.95)))

(serapeum:-> set-int-uniform (t string integer)
             (values &optional))
(defun set-int-uniform (program uniform value)
  (gl:uniformi
   (gl:get-uniform-location program uniform)
   value)
  (values))

(serapeum:-> set-mat3-uniform (t string rtg-math.types:mat3)
             (values &optional))
(defun set-mat3-uniform (program uniform m)
  (let ((location (gl:get-uniform-location program uniform)))
    (gl:uniform-matrix location 3 (vector m) nil))
  (values))

(serapeum:-> set-float-uniform (t string single-float)
             (values &optional))
(defun set-float-uniform (program uniform x)
  (let ((location (gl:get-uniform-location program uniform)))
    (gl:uniformf location x))
  (values))

(serapeum:-> random-vec3 ()
             (values rtg-math.types:vec3 &optional))
(defun random-vec3 ()
  (v3:make (random 1.0)
           (random 1.0)
           (random 1.0)))

(defmacro loop-ranges (ranges &body body)
  (reduce
   (lambda (entry acc)
     (destructuring-bind (val below)
         entry
       `(dotimes (,val ,below)
          ,acc)))
   ranges
   :from-end t
   :initial-value `(progn ,@body)))
