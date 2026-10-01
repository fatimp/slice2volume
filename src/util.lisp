(in-package :slice2volume)

(deftype getter () '(function ( ) (values t &optional)))
(deftype setter () '(function (t) (values t &optional)))

(defmacro with-place ((getter setter) &body body)
  (let ((var (gensym)))
    `(let (,var)
       (flet ((,getter ()  (assert ,var) ,var)
              (,setter (x) (setq ,var x)))
         ,@body))))

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

(serapeum:-> fast-upload-slice ((simple-array single-float (* *)))
             (values &optional))
(defun fast-upload-slice (array)
  (declare (optimize (speed 3)))
  #-sbcl
  (gl:tex-image-3d :texture-3d 0 :red
                   (array-dimension array 2)
                   (array-dimension array 1)
                   (array-dimension array 0)
                   0 :red :float (flatten array))
  #+sbcl
  (cffi:with-pointer-to-vector-data (ptr (sb-ext:array-storage-vector array))
    (gl:tex-image-2d :texture-2d 0 :red
                     (array-dimension array 1)
                     (array-dimension array 0)
                     0 :red :float ptr)))

(serapeum:-> set-int-uniform (t string integer)
             (values &optional))
(defun set-int-uniform (program uniform value)
  (gl:uniformi
   (gl:get-uniform-location program uniform)
   value)
  (values))
