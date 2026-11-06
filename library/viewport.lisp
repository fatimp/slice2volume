(defpackage slice2volume/viewport
  (:use #:cl)
  (:local-nicknames (#:tran #:slice2volume/transforms))
  (:export #:viewport-dimensions
           #:slice-viewport-size))
(in-package :slice2volume/viewport)

(serapeum:-> slice-viewport-size
    (rtg-math.types:mat3
     alexandria:positive-fixnum
     alexandria:positive-fixnum
     alexandria:positive-fixnum)
    (values alexandria:positive-fixnum
            alexandria:positive-fixnum
            &optional))
(defun slice-viewport-size (transform width height depth)
  (let* ((transform (m3:*
                     (tran:diagonal3 (/ width  2.0)
                                     (/ height 2.0)
                                     (/ depth  2.0))
                     transform))
         (p1 (m3:*v transform (v3:make -1.0 -1.0 1.0)))
         (p2 (m3:*v transform (v3:make +1.0 -1.0 1.0)))
         (p3 (m3:*v transform (v3:make -1.0 +1.0 1.0))))
    (values (ceiling (v3:length (v3:- p2 p1)))
            (ceiling (v3:length (v3:- p3 p1))))))

(serapeum:-> viewport-dimensions ((integer 1) (integer 1) (integer 1) (integer 1))
             (values (integer 0) (integer 0) (integer 1) (integer 1) &optional))
(defun viewport-dimensions (image-width image-height area-width area-height)
  "Returns X, Y, WIDTH and HEIGHT parameters of a viewport."
  (let ((image-ratio (/ image-width image-height))
        (area-ratio  (/ area-width  area-height)))
    (if (< area-ratio image-ratio)
        (let ((viewport-width  area-width)
              (viewport-height (floor (/ area-width image-ratio))))
          (values 0 (floor (- area-height viewport-height) 2)
                  viewport-width viewport-height))
        (let ((viewport-width  (floor (* area-height image-ratio)))
              (viewport-height area-height))
          (values (floor (- area-width viewport-width) 2) 0
                  viewport-width viewport-height)))))
