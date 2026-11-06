(defpackage slice2volume/slicer
  (:use #:cl)
  (:local-nicknames (#:ff   #:float-features)
                    (#:tran #:slice2volume/transforms)
                    (#:view #:slice2volume/viewport))
  (:export #:slice))
(in-package :slice2volume/slicer)

(declaim (inline interp1d))
(defun interp1d (v0 v1 x)
  (+ v0 (* (- v1 v0) x)))

(declaim (inline safe-aref))
(defun safe-aref (a i j k b)
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (if (array-in-bounds-p a i j k)
      (aref a i j k) b))

(serapeum:-> interpolate ((simple-array single-float 3) rtg-math.types:vec3 single-float)
             (values single-float &optional))
(defun interpolate (array v background)
  "Interpolate F in the point (X, Y, Z)"
  (declare (optimize (speed 3)))
  (flet ((coord (x d)
           (* d (/ (1+ x) 2))))
    (serapeum:mvlet ((qi ri (floor (coord (aref v 2) (array-dimension array 0))))
                     (qj rj (floor (coord (aref v 1) (array-dimension array 1))))
                     (qk rk (floor (coord (aref v 0) (array-dimension array 2)))))
      (declare (type fixnum qi qj qk))
      ;; For code formatting
      (flet ((id (x) x))
        (declare (inline id))
        (let* ((v000 (safe-aref array (id qi) (id qj) (id qk) background))
               (v001 (safe-aref array (id qi) (id qj) (1+ qk) background))
               (v010 (safe-aref array (id qi) (1+ qj) (id qk) background))
               (v011 (safe-aref array (id qi) (1+ qj) (1+ qk) background))
               (v100 (safe-aref array (1+ qi) (id qj) (id qk) background))
               (v101 (safe-aref array (1+ qi) (id qj) (1+ qk) background))
               (v110 (safe-aref array (1+ qi) (1+ qj) (id qk) background))
               (v111 (safe-aref array (1+ qi) (1+ qj) (1+ qk) background))

               (v00 (interp1d v000 v001 rk))
               (v01 (interp1d v010 v011 rk))
               (v10 (interp1d v100 v101 rk))
               (v11 (interp1d v110 v111 rk))

               (v0 (interp1d v00 v01 rj))
               (v1 (interp1d v10 v11 rj))

               (v (interp1d v0 v1 ri)))
          v)))))

(serapeum:-> slice ((simple-array single-float 3)
                    alexandria:positive-fixnum alexandria:positive-fixnum
                    rtg-math.types:mat3)
             (values (simple-array single-float 2) &optional))
(defun slice (volume height width transform)
  (declare (optimize (speed 3)))
  (multiple-value-bind (x-start y-start vp-width vp-height)
      (multiple-value-call
          #'view:viewport-dimensions
        (view:slice-viewport-size
         transform
         (array-dimension volume 2)
         (array-dimension volume 1)
         (array-dimension volume 0))
        width height)
    (flet ((normalize-coord (x d)
             (float (1- (/ (* 2 x) (1- d))))))
    (let ((x-end (+ x-start vp-width))
          (y-end (+ y-start vp-height))
          (res (make-array (list height width) :element-type 'single-float)))
      (loop for y below height do
        (loop for x below width do
          (setf (aref res y x)
                (if (and (>= y y-start) (< y y-end)
                         (>= x x-start) (< x x-end))
                    (let* ((x (normalize-coord (- x x-start) vp-width))
                           (y (normalize-coord (- y y-start) vp-height))
                           (coord (m3:*v transform (v3:make x y 1.0))))
                      (interpolate volume coord ff:single-float-nan))
                    ff:single-float-nan))))
      res))))
