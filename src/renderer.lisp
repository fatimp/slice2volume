(in-package :slice2volume)

(deftype slice-gpu-uploader  () '(function ((image 2)) (values &optional)))
(deftype volume-gpu-uploader () '(function ((image 3)) (values &optional)))

(defstruct scene
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
  (vao            -1 :type fixnum)
  (texture-3d-size 0 :type (unsigned-byte 32)))

(serapeum:defconstructor renderer
  (area            gir::object-instance)
  (slice-uploader  slice-gpu-uploader)
  (volume-uploader volume-gpu-uploader))

(serapeum:-> volume-loaded-p (context)
             (values boolean &optional))
(defun volume-loaded-p (context)
  (/= (context-volume-texture context) -1))

(serapeum:-> slice-loaded-p (context)
             (values boolean &optional))
(defun slice-loaded-p (context)
  (/= (context-slice-texture context) -1))

(defconstant +max-size-fallback+ 512)

(serapeum:-> maximal-texture-dimension-3d ()
             (values (unsigned-byte 32) &optional))
(defun maximal-texture-dimension-3d ()
  (flet ((texture-bad-p (n)
           (gl:tex-storage-3d :proxy-texture-3d 1 :r8 n n n)
           (let ((width  (gl:get-tex-level-parameter :proxy-texture-3d 0 :texture-width))
                 (height (gl:get-tex-level-parameter :proxy-texture-3d 0 :texture-height))
                 (depth  (gl:get-tex-level-parameter :proxy-texture-3d 0 :texture-depth)))
             (or (zerop width)
                 (zerop height)
                 (zerop depth)))))
    ;; We don't need bigger than 1000^3
    (let ((size (loop for n from 1000 downto 200 by 100
                      unless (texture-bad-p n) do (return n))))
      (if size size
          ;; Try it anyway. What else can we do?
          +max-size-fallback+))))

(serapeum:-> maybe-downsample-3d ((simple-array (unsigned-byte 8) (* * *))
                                  (unsigned-byte 32))
             (values (simple-array (unsigned-byte 8) (* * *)) &optional))
(defun maybe-downsample-3d (array max-size)
  (let ((max-dim (reduce #'max (array-dimensions array) :initial-value 0)))
    (if (> max-dim max-size)
        (downsample-3d array (ceiling (/ max-dim max-size)))
        array)))

(serapeum:-> make-slice-uploader (gir::object-instance context)
             (values slice-gpu-uploader &optional))
(defun make-slice-uploader (area context)
  (lambda (slice)
    (declare (type (image 2) slice))
    (gtk4:gl-area-make-current area)
    ;; Delete an old texture if any
    (when (slice-loaded-p context)
      (gl:delete-texture (context-slice-texture context)))
    ;; Create and upload a new texture
    (let ((texture (gl:gen-texture)))
      (gl:bind-texture  :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (setf (context-slice-texture context) texture))
    (fast-upload-slice slice)
    (values)))

(serapeum:-> make-volume-uploader (gir::object-instance context)
             (values volume-gpu-uploader &optional))
(defun make-volume-uploader (area context)
  (lambda (volume)
    (declare (type (image 3) volume))
    (let ((volume (maybe-downsample-3d volume (context-texture-3d-size context))))
      (gtk4:gl-area-make-current area)
      ;; Delete an old texture if any
      (when (volume-loaded-p context)
        (gl:delete-texture (context-volume-texture context)))
      ;; Create and upload a new texture
      (let ((texture (gl:gen-texture)))
        (gl:bind-texture :texture-3d texture)
        (gl:tex-parameter :texture-3d :texture-mag-filter :linear)
        (gl:tex-parameter :texture-3d :texture-min-filter :linear)
        (setf (context-volume-texture context) texture))
      (fast-upload-volume volume)
      (values))))

(serapeum:-> make-realize-handler (context)
             (values (function (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (context)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (gl:clear-color 0.0 0.0 0.0 0.0)

    (let ((slice-program  (create-program *slice-program*))
          (volume-program (create-program *volume-program*))
          (vao            (gl:gen-vertex-array)))
      (setf (context-vao             context) vao
            (context-slice-program   context) slice-program
            (context-volume-program  context) volume-program
            (context-texture-3d-size context)
            (maximal-texture-dimension-3d)))
    (values)))

(serapeum:-> make-unrealize-handler (context)
             (values (function (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (context)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (gl:delete-vertex-array (context-vao context))
    (gl:delete-program (context-slice-program  context))
    (gl:delete-program (context-volume-program context))
    (when (volume-loaded-p context)
      (gl:delete-texture (context-volume-texture context)))
    (when (slice-loaded-p context)
      (gl:delete-texture (context-slice-texture context)))
    (values)))

(serapeum:-> make-draw-handler (context scene)
             (values (function (gir::object-instance gir::object-instance)
                               (values boolean &optional))
                     &optional))
(defun make-draw-handler (context scene)
  (lambda (area ctx)
    (declare (ignore ctx))
    (gl:clear :color-buffer-bit)
    (let ((area-width  (floor    (gtk4:widget-width  area) 2))
          (area-height (identity (gtk4:widget-height area))))
      (when (slice-loaded-p context)
        ;; Render slice
        (gl:bind-texture :texture-2d (context-slice-texture context))
        (multiple-value-bind (x y width height)
            (viewport-dimensions
             (gl:get-tex-level-parameter :texture-2d 0 :texture-width)
             (gl:get-tex-level-parameter :texture-2d 0 :texture-height)
             area-width area-height)
          (gl:viewport x y width height))
        (gl:use-program (context-slice-program context))
        (set-int-uniform (context-slice-program context) "SAMPLER" 0)
        (gl:active-texture :texture0)
        (gl:bind-vertex-array (context-vao context))
        (gl:draw-arrays :triangle-strip 0 4))
      (when (volume-loaded-p context)
        ;; Render volume
        (gl:bind-texture :texture-3d (context-volume-texture context))
        (multiple-value-bind (x y width height)
            (viewport-dimensions
             (gl:get-tex-level-parameter :texture-3d 0 :texture-width)
             (gl:get-tex-level-parameter :texture-3d 0 :texture-height)
             area-width area-height)
          (gl:viewport (+ area-width x) y width height))
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
        (gl:bind-vertex-array (context-vao context))
        (gl:draw-arrays :triangle-strip 0 4)))
    (values)))

(serapeum:-> make-drawing-area (scene)
             (values renderer &optional))
(defun make-drawing-area (scene)
  (let ((area    (gtk4:make-gl-area))
        (context (make-context)))
    (setf (gtk4:gl-area-allowed-apis area) 1) ; OpenGL Only
    (gir:invoke (area "set_required_version") 4 2)
    (gtk4:connect area "realize"   (make-realize-handler   context))
    (gtk4:connect area "unrealize" (make-unrealize-handler context))
    (gtk4:connect area "render"    (make-draw-handler      context scene))
    (renderer
     area
     (make-slice-uploader  area context)
     (make-volume-uploader area context))))
