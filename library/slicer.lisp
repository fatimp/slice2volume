(in-package :slice2volume/library)

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
  (serapeum:mvlet ((qi ri (floor (aref v 0)))
                   (qj rj (floor (aref v 1)))
                   (qk rk (floor (aref v 2))))
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
        v))))

(serapeum:-> slice ((simple-array single-float 3)
                    alexandria:positive-fixnum alexandria:positive-fixnum
                    rtg-math.types:mat3)
             (values (simple-array single-float 2) &optional))
(defun slice (volume height width transform)
  (declare (optimize (speed 3)))
  (let ((res (make-array (list height width) :element-type 'single-float)))
    (loop for i below height do
      (loop for j below width do
        (let* ((coord (m3:*v transform (v3:make (float j) (float i) 1.0)))
               (pixel (interpolate volume coord ff:single-float-nan)))
          (setf (aref res i j) pixel))))
    res))
