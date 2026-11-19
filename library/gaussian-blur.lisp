;; TODO: Part of this code is used in shamazmazum/sift. Move this code to a separate library?
(defpackage slice2volume/gaussian-blur
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria))
  (:export #:gaussian-blur))
(in-package :slice2volume/gaussian-blur)

(defmacro loop-array ((array indices) &body body)
  (reduce
   (lambda (entry acc)
     (destructuring-bind (d . idx)
         entry
       `(loop for ,idx below (array-dimension ,array ,d) do
              ,acc)))
   (loop for idx in indices
         for d from 0 by 1
         collect (cons d idx))
   :from-end t
   :initial-value `(progn ,@body)))

(sera:-> gaussian-kernel-1d ((single-float 0f0))
         (values (simple-array single-float (*)) &optional))
(defun gaussian-kernel-1d (σ)
  (loop with length = (1+ (* (ceiling σ) 4))
        with w = (floor length 2)
        with kernel = (make-array length :element-type 'single-float)
        with σ = (if (zerop σ) 1.0 σ)
        for i below length
        for x = (- i w)
        for v = (exp (- (/ (expt x 2) (expt σ 2) 2)))
        do (setf (aref kernel i) v)
        sum v into sum
        finally (return (map-into kernel (lambda (x) (/ x sum)) kernel))))

(sera:-> gaussian-kernel ((single-float 0f0))
         (values (simple-array single-float (* *)) &optional))
(defun gaussian-kernel (σ)
  (let* ((kernel-1d (gaussian-kernel-1d σ))
         (length (length kernel-1d))
         (kernel (make-array (list length length)
                             :element-type 'single-float)))
    (loop-array (kernel (i j))
     (setf (aref kernel i j)
           (* (aref kernel-1d i)
              (aref kernel-1d j))))
    kernel))

(declaim (inline safe-aref))
(defun safe-aref (a i j b)
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (if (array-in-bounds-p a i j)
      (aref a i j) b))

(sera:-> gaussian-blur ((simple-array single-float (* *))
                        (single-float 0f0))
         (values (simple-array single-float (* *)) &optional))
(defun gaussian-blur (array σ)
  (declare (optimize (speed 3)))
  (let* ((kernel (gaussian-kernel σ))
         (w (floor (array-dimension kernel 0) 2))
         (result (make-array (array-dimensions array)
                             :element-type 'single-float
                             :initial-element 0.0)))
    (loop-array (result (i j))
      (loop-array (kernel (k l))
        (incf (aref result i j)
              (* (aref kernel k l)
                 (safe-aref array
                            (+ i k (- w))
                            (+ j l (- w))
                            ;; Background pixel
                            0)))))
    result))
