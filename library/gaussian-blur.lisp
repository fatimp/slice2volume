;; TODO: This code is used in shamazmazum/sift. Move this code to a separate library?
(defpackage slice2volume/gaussian-blur
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:norm #:slice2volume/normalize)
                    (#:fftw #:cl-fftw/single))
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

(sera:-> gaussian-kernel-padded (alex:positive-fixnum
                                 alex:positive-fixnum
                                 (single-float 0f0))
         (values (simple-array single-float (* *)) &optional))
(defun gaussian-kernel-padded (h w σ)
  (let* ((kernel (make-array (list h w)
                             :element-type 'single-float
                             :initial-element 0f0))
         (kernel-tight (gaussian-kernel σ))
         (size (array-dimension kernel-tight 0))
         (width (floor size 2)))
    (flet ((centered-idx (i q)
             (mod (- i width) q)))
      ;; TODO: Check that tight-dimensions <= dimensions
      (loop-array (kernel-tight (i j))
        (setf (aref kernel (centered-idx i h) (centered-idx j w))
              (aref kernel-tight i j))))
    kernel))

(sera:-> extract-overlap-mask ((simple-array single-float (* *)))
         (values (simple-array single-float (* *))
                 (simple-array single-float (* *))
                 &optional))
(defun extract-overlap-mask (array)
  (declare (optimize (speed 3)))
  (let ((image (make-array (array-dimensions array)
                           :element-type 'single-float))
        (mask  (make-array (array-dimensions array)
                           :element-type 'single-float)))
    (loop for i below (array-total-size array)
          for x = (row-major-aref array i)
          do (setf (row-major-aref mask i)
                   (if (= x x) 1.0 x)
                   (row-major-aref image i)
                   (if (= x x) x 0.0)))
    (values image mask)))

(sera:-> apply-overlap-mask! ((simple-array single-float (* *))
                              (simple-array single-float (* *)))
         (values (simple-array single-float (* *)) &optional))
(defun apply-overlap-mask! (image mask)
  (declare (optimize (speed 3)))
  (loop for i below (array-total-size image)
        do (setf (row-major-aref image i)
                 (* (norm:clamp (row-major-aref image i) 0 1)
                    (row-major-aref mask  i))))
  image)

(sera:-> gaussian-blur ((simple-array single-float (* *))
                        (single-float 0f0))
         (values (simple-array single-float (* *)) &optional))
(defun gaussian-blur (array σ)
  (declare (optimize (speed 3)))
  (let ((dimensions (array-dimensions array))
        (size       (array-total-size array))
        (kernel (gaussian-kernel-padded
                 (array-dimension array 0)
                 (array-dimension array 1)
                 σ)))
    (multiple-value-bind (image mask)
        (extract-overlap-mask array)
      (apply-overlap-mask!
       (fftw:%irfft
        (fftw:with-rfft-plan (plan dimensions)
          (let ((image-fft  (fftw:rfft plan image))
                (kernel-fft (fftw:rfft plan kernel)))
            (loop for i below (array-total-size image-fft) do
              (setf (row-major-aref image-fft i)
                    (/ (* (row-major-aref image-fft  i)
                          (row-major-aref kernel-fft i))
                       ;; Normalize
                       size)))
            image-fft))
        dimensions)
       mask))))
