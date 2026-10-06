(in-package :slice2volume)

(deftype slice-gpu-uploader  () '(function ((image 2)) (values &optional)))
(deftype volume-gpu-uploader () '(function ((image 3)) (values &optional)))

(defstruct scene
  (slice-loaded-p  nil :type boolean)
  (volume-loaded-p nil :type boolean)
  (ϕ               0.0 :type single-float)
  (Δu              0.0 :type single-float)
  (Δv              0.0 :type single-float)
  ;; Random vector not collinear with n
  (v (v3:make 0.0 1.0 0.0) :type rtg-math.types:vec3)
  ;; And a normal to the cutting plane
  (n  (v3:make 0.0 0.0 1.0) :type rtg-math.types:vec3)
  ;; Distance to the cutting plane
  (d               0.0 :type single-float))

(defstruct context
  (slice-program  -1 :type fixnum)
  (slice-texture  -1 :type fixnum)
  (volume-program -1 :type fixnum)
  (volume-texture -1 :type fixnum)
  (vao            -1 :type fixnum))

(serapeum:defconstructor renderer
  (area            gir::object-instance)
  (slice-uploader  slice-gpu-uploader)
  (volume-uploader volume-gpu-uploader))

;; TODO: Determine it at runtime
(defconstant +max-size+ 512)

(serapeum:-> downsample-scale (simple-array)
             (values (or null (and (unsigned-byte 8) (integer 1))) &optional))
(defun downsample-scale (array)
  (let ((max-dim (reduce #'max (array-dimensions array) :initial-value 0)))
    (if (> max-dim +max-size+)
        (nth-value 0 (ceiling (/ max-dim +max-size+))))))

(serapeum:-> make-slice-uploader (gir::object-instance context scene)
             (values slice-gpu-uploader &optional))
(defun make-slice-uploader (area context scene)
  (lambda (slice)
    (gtk4:gl-area-make-current area)
    (gl:bind-texture :texture-2d (context-slice-texture context))
    (fast-upload-slice slice)
    (setf (scene-slice-loaded-p scene) t)
    (values)))

(serapeum:-> make-volume-uploader (gir::object-instance context scene)
             (values volume-gpu-uploader &optional))
(defun make-volume-uploader (area context scene)
  (lambda (volume)
    (let* ((scale (downsample-scale volume))
           (volume (if scale (downsample3d volume scale) volume)))
      (gtk4:gl-area-make-current area)
      (gl:bind-texture :texture-3d (context-volume-texture context))
      (fast-upload-volume volume)
      (setf (scene-volume-loaded-p scene) t)
      (values))))

(serapeum:-> make-realize-handler (context)
             (values (function (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (context)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (gl:clear-color 0.0 0.0 0.0 0.0)

    (let ((slice-texture   (gl:gen-texture))
          (volume-texture  (gl:gen-texture))
          (slice-program   (create-program *slice-program*))
          (volume-program  (create-program *volume-program*))
          (vao             (gl:gen-vertex-array)))
      (gl:bind-texture  :texture-2d slice-texture)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:bind-texture  :texture-3d volume-texture)
      (gl:tex-parameter :texture-3d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-3d :texture-min-filter :linear)
      (setf (context-vao            context) vao
            (context-slice-texture  context) slice-texture
            (context-slice-program  context) slice-program
            (context-volume-program context) volume-program
            (context-volume-texture context) volume-texture))
    (values)))

(serapeum:-> make-unrealize-handler (context)
             (values (function (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (context)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (gl:delete-vertex-array (context-vao context))
    (gl:delete-program (context-slice-program  context))
    (gl:delete-program (context-volume-program context))
    (gl:delete-textures (list (context-slice-texture  context)
                              (context-volume-texture context)))
    (values)))

(serapeum:-> make-draw-handler (context scene)
             (values (function (gir::object-instance gir::object-instance)
                               (values boolean &optional))
                     &optional))
(defun make-draw-handler (context scene)
  (lambda (area ctx)
    (declare (ignore ctx))
    (gl:clear :color-buffer-bit)
    (let ((width   (gtk4:widget-width  area))
          (height  (gtk4:widget-height area)))
      (when (scene-slice-loaded-p scene)
        ;; Render slice
        (gl:viewport 0 0 (floor width 2) height)
        (gl:use-program (context-slice-program context))
        (set-int-uniform (context-slice-program context) "SAMPLER" 0)
        (gl:active-texture :texture0)
        (gl:bind-texture :texture-2d (context-slice-texture context))
        (gl:bind-vertex-array (context-vao context))
        (gl:draw-arrays :triangle-strip 0 4))
      (when (scene-volume-loaded-p scene)
        ;; Render volume
        (gl:viewport (floor width 2) 0 (floor width 2) height)
        (let ((program (context-volume-program context)))
          (gl:use-program program)
          (set-int-uniform   program "SAMPLER" 0)
          (set-float-uniform program "DIST" (scene-d scene))
          (set-mat3-uniform  program "PLANAR_TRANSFORM"
                             (planar-transform-matrix
                              (scene-ϕ scene)
                              (scene-Δu scene)
                              (scene-Δv scene)))
          (set-mat3-uniform  program "WORLD_TRANSFORM"
                             (world-transform-matrix
                              (scene-v scene)
                              (scene-n scene))))
        (gl:active-texture :texture0)
        (gl:bind-texture :texture-3d (context-volume-texture context))
        (gl:bind-vertex-array (context-vao context))
        (gl:draw-arrays :triangle-strip 0 4)))
    (values)))

(serapeum:-> make-drawing-area (scene)
             (values renderer &optional))
(defun make-drawing-area (scene)
  (let ((area    (gtk4:make-gl-area))
        (context (make-context)))
    (setf (gtk4:gl-area-allowed-apis area) 1) ; OpenGL Only
    (gtk4:connect area "realize"   (make-realize-handler   context))
    (gtk4:connect area "unrealize" (make-unrealize-handler context))
    (gtk4:connect area "render"    (make-draw-handler      context scene))
    (renderer
     area
     (make-slice-uploader  area context scene)
     (make-volume-uploader area context scene))))
