(defpackage slice2volume/mutual-information
  (:use #:cl)
  (:export #:mutual-information))
(in-package :slice2volume/mutual-information)

(declaim (inline bin-index))
(defun bin-index (v nbins)
  (min
   (floor (* v nbins))
   ;; FIXME: the value 1.0 maps onto the last bin
   (1- nbins)))

(serapeum:-> normalize ((simple-array (unsigned-byte 64) (* *)))
             (values (simple-array single-float (* *)) &optional))
(defun normalize (histogram)
  (declare (optimize (speed 3)))
  (let ((result (make-array (array-dimensions histogram)
                            :element-type 'single-float))
        (total (reduce #'+ (sb-ext:array-storage-vector histogram))))
    (map-into (sb-ext:array-storage-vector result)
              (lambda (x)
                (float (/ x total)))
              (sb-ext:array-storage-vector histogram))
    result))

(serapeum:-> joint-histogram ((simple-array single-float (* *))
                              (simple-array single-float (* *))
                              alexandria:positive-fixnum)
             (values (simple-array single-float (* *)) &optional))
(defun joint-histogram (img1 img2 nbins)
  (declare (optimize (speed 3)))
  (unless (equalp (array-dimensions img1)
                  (array-dimensions img2))
    (error "Images must be of the same dimensions"))
  (let ((histogram (make-array (list nbins nbins)
                               :element-type '(unsigned-byte 64)
                               :initial-element 0)))
    (loop for i below (array-total-size img1) do
      (let ((v1 (row-major-aref img1 i))
            (v2 (row-major-aref img2 i)))
        (when (and (= v1 v1)
                   (= v2 v2)); No NaNs which means the images overlap
          (let ((i1 (bin-index v1 nbins))
                (i2 (bin-index v2 nbins)))
            (incf (aref histogram i1 i2))))))
    (normalize histogram)))

(serapeum:-> sum-row ((simple-array single-float (* *))
                      alexandria:array-index)
             (values single-float &optional))
(defun sum-row (hist idx)
  (declare (optimize (speed 3)))
  (loop for i below (array-dimension hist 1) sum
        (aref hist idx i)
        single-float))

(serapeum:-> sum-column ((simple-array single-float (* *))
                         alexandria:array-index)
             (values single-float &optional))
(defun sum-column (hist idx)
  (declare (optimize (speed 3)))
  (loop for i below (array-dimension hist 0) sum
        (aref hist i idx)
        single-float))

(serapeum:-> info ((single-float 0.0 1.0))
             (values single-float &optional))
(declaim (inline info))
(defun info (p)
  (if (< p 1f-6) 0.0
      (- (* p (log p)))))

(serapeum:-> mutual-information ((simple-array single-float (* *))
                                 (simple-array single-float (* *))
                                 alexandria:positive-fixnum)
             (values single-float &optional))
(defun mutual-information (img1 img2 nbins)
  (declare (optimize (speed 3)))
  (let* ((hist (joint-histogram img1 img2 nbins))
         (m1 (loop for i below nbins sum
                   (info (sum-row hist i))
                   single-float))
         (m2 (loop for i below nbins sum
                   (info (sum-column hist i))
                   single-float))
         (j  (loop for i below (array-total-size hist) sum
                   (info (row-major-aref hist i))
                   single-float)))
    (/ (+ m1 m2) j)))
