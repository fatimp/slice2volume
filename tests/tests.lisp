(in-package :slice2volume/tests)

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(mi))))

(def-suite mi :description "Mutual information function")

(serapeum:-> make-noise (alexandria:positive-fixnum single-float fixnum)
             (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun make-noise (n z seed)
  (declare (optimize (speed 3)))
  (let ((noise (make-array (list n n)
                           :element-type '(unsigned-byte 8))))
    (loop for i below n do
      (loop for j below n do
        (let ((v (noise:value-noise
                  (* 15.0 (/ i n))
                  (* 15.0 (/ j n))
                  (* 15.0 z)
                  :seed seed)))
          (setf (aref noise i j)
                (floor (* 255 v))))))
    noise))

(serapeum:-> gamma-correction ((simple-array single-float (* *))
                               single-float)
             (values (simple-array single-float (* *)) &optional))
(defun gamma-correction (array γ)
  (declare (optimize (speed 3)))
  (flet ((pow (x)
           (declare (type (single-float 0.0) x))
           (expt x γ)))
    (let ((result (make-array (array-dimensions array)
                              :element-type 'single-float)))
      (loop for i below (array-total-size result) do
        (setf (row-major-aref result i)
              (pow (row-major-aref array i))))
      result)))

(defun gen-single-float (&key min max)
  (assert (<= min max))
  (lambda ()
    (+ min (random (- max min)))))

(in-suite mi)

(test mi-gamma-correction
  (for-all ((seed  (gen-integer      :min 0   :max 1000000))
            (size  (gen-integer      :min 800 :max 2500))
            (gamma (gen-single-float :min 0.7 :max 1.3)))
    (let* ((noise (norm:normalize-image
                   (make-noise size 0.0 seed)
                   0.02))
           (corrected (gamma-correction noise gamma)))
      (is (> (mi:mutual-information noise corrected 15) 1.6)))))

(test mi-shift
  (for-all ((seed  (gen-integer      :min 0   :max 1000000))
            (size  (gen-integer      :min 800 :max 2500))
            (shift (gen-single-float :min 0.0 :max 0.01)))
    (let* ((noise1 (norm:normalize-image
                    (make-noise size 0.0 seed)
                    0.02))
           (noise2 (norm:normalize-image
                    (make-noise size shift seed)
                    0.02)))
      ;; FIXME: Too small value
      (is (> (mi:mutual-information noise1 noise2 15) 1.2)))))

(test mi-prop
  (for-all ((seed1 (gen-integer      :min 0   :max 1000000))
            (seed2 (gen-integer      :min 0   :max 1000000))
            (size  (gen-integer      :min 800 :max 2500)))
    (let* ((noise1 (norm:normalize-image
                    (make-noise size 0.0 seed1)
                    0.02))
           (noise2 (norm:normalize-image
                    (make-noise size 0.0 seed2)
                    0.02)))
      (is (< (mi:mutual-information noise1 noise2 15) 1.05))
      (is (> (mi:mutual-information noise1 noise1 15) (- 2 1f-4))))))
