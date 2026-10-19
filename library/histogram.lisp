(defpackage slice2volume/histogram
  (:use #:cl)
  (:export #:normalize-image))
(in-package :slice2volume/histogram)

(serapeum:-> scan-vector (vector)
             (values vector &optional))
(declaim (inline scan-vector))
(defun scan-vector (v)
  (let ((s (make-array (length v) :element-type (array-element-type v))))
    (setf (aref s 0) (aref v 0))
    (loop for i from 1 below (length v) do
      (setf (aref s i)
            (+ (aref s (1- i))
               (aref v i))))
    s))

(serapeum:-> image-cdf ((simple-array (unsigned-byte 8)))
             (values (simple-array (unsigned-byte 64) (256)) &optional))
(defun image-cdf (image)
  (declare (optimize (speed 3)))
  (let ((histogram (make-array 256
                               :element-type '(unsigned-byte 64)
                               :initial-element 0)))
    (loop for i below (array-total-size image) do
      (incf (aref histogram (row-major-aref image i))))
    (scan-vector histogram)))

(serapeum:-> image-intensity-bounds ((simple-array (unsigned-byte 8)) single-float)
             (values (unsigned-byte 8) (unsigned-byte 8) &optional))
(defun image-intensity-bounds (image threshold)
  (declare (optimize (speed 3)
                     (space 2)))
  (let* ((cdf  (image-cdf image))
         (size (array-total-size image))
         (min  (ceiling (* size      threshold)))
         (max  (floor   (* size (- 1 threshold)))))
    (declare (type (unsigned-byte 64) min max))
    (values
     (or
      (position-if
       (lambda (v) (< v min))
       cdf :from-end t)
      0)
     (or
      (position-if
       (lambda (v) (> v max))
       cdf :from-end nil)
      255))))

(declaim (inline clamp))
(defun clamp (x min max)
  (min (max x min) max))

(serapeum:-> normalize-image ((simple-array (unsigned-byte 8)) single-float)
             (values (simple-array single-float) &optional))
(defun normalize-image (image threshold)
  (declare (optimize (speed 3)))
  (let ((result (make-array (array-dimensions image)
                            :element-type 'single-float)))
    (multiple-value-bind (min max)
        (image-intensity-bounds image threshold)
      (loop with div = (float (- max min))
            for i below (array-total-size image)
            for v = (clamp (row-major-aref image i) min max) do
              (setf (row-major-aref result i)
                    (/ (- v min) div))))
    result))
