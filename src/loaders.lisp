(in-package :slice2volume)

(deftype image (n) `(simple-array (unsigned-byte 8) ,n))

(deftype slice-loader ()
  '(function (pathname) (values (image 2) &optional)))

(deftype volume-loader ()
  '(function (pathname) (values (image 3) &optional)))

(serapeum:-> rgba-intensities ((simple-array (unsigned-byte 32)))
             (values (simple-array (unsigned-byte 8)) &optional))
(defun rgba-intensities (xs)
  (declare (optimize (speed 3)))
  (let ((result (make-array (array-dimensions xs)
                            :element-type '(unsigned-byte 8))))
    (map-into
     (sb-ext:array-storage-vector result)
     (lambda (color)
       (multiple-value-bind (r g b)
           (tiff:split-rgba color)
         (floor (+ r g b) 3)))
     (sb-ext:array-storage-vector xs))
    result))

;; Numpy format
(declaim (ftype slice-loader numpy-slice-loader))
(defun numpy-slice-loader (pathname)
  (declare (optimize (speed 3)))
  (let ((array (numpy-npy:load-array pathname)))
    (unless (and (= (array-rank array) 2)
                 (equalp (array-element-type array)
                         '(unsigned-byte 8)))
      (error 'loader-error
             :slicep       t
             :dimensions   (array-dimensions array)
             :element-type (array-element-type array)))
    array))

(declaim (ftype volume-loader numpy-volume-loader))
(defun numpy-volume-loader (pathname)
  (declare (optimize (speed 3)))
  (let ((array (numpy-npy:load-array pathname)))
    (unless (and (= (array-rank array) 3)
                 (equalp (array-element-type array)
                         '(unsigned-byte 8)))
      (error 'loader-error
             :slicep       nil
             :dimensions   (array-dimensions array)
             :element-type (array-element-type array)))
    array))

;; TIFF
(serapeum:-> read-tiff-slice (tiff:tiff-handle unsigned-byte unsigned-byte)
             (values (simple-array (unsigned-byte 32) (* *)) &optional))
(defun read-tiff-slice (handle width height)
  (tiff:read-rgba-image-oriented handle width height :topleft))

(declaim (ftype slice-loader tiff-slice-loader))
(defun tiff-slice-loader (pathname)
  (rgba-intensities
   (tiff:with-open-tiff (handle pathname :input)
     (let ((width  (tiff:width  handle))
           (height (tiff:height handle)))
       (read-tiff-slice handle width height)))))

;; General loader functions
(serapeum:defconstructor loader
  (type        string)
  (description string)
  (slice       (or slice-loader  null))
  (volume      (or volume-loader null)))

(defparameter *loaders*
  (list (loader "npy"  "Numpy array format" #'numpy-slice-loader #'numpy-volume-loader)
        (loader "tif"  "TIFF image format"  #'tiff-slice-loader  nil)
        (loader "tiff" "TIFF image format"  #'tiff-slice-loader  nil)))

(serapeum:-> slice-loaders () (values list &optional))
(defun slice-loaders ()
  (remove-if
   (lambda (loader)
     (null (loader-slice loader)))
   *loaders*))

(serapeum:-> volume-loaders () (values list &optional))
(defun volume-loaders ()
  (remove-if
   (lambda (loader)
     (null (loader-volume loader)))
   *loaders*))

(deftype loader-type () '(member :slice :volume))

(serapeum:-> loaders (loader-type)
             (values list &optional))
(defun loaders (type)
  (case type
    (:slice  (slice-loaders))
    (:volume (volume-loaders))))

(serapeum:-> load-volume (pathname)
             (values (or (image 3) null) &optional))
(defun load-volume (pathname)
  (let ((loader (find (pathname-type pathname)
                      (loaders :volume)
                      :test #'string=
                      :key  #'loader-type)))
    (when loader
      (funcall (loader-volume loader) pathname))))

(serapeum:-> load-slice (pathname)
             (values (or (image 2) null) &optional))
(defun load-slice (pathname)
  (let ((loader (find (pathname-type pathname)
                      (loaders :slice)
                      :test #'string=
                      :key  #'loader-type)))
    (when loader
      (funcall (loader-slice loader) pathname))))
