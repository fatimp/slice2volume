(defpackage slice2volume/loaders
  (:use #:cl)
  (:local-nicknames (#:tiff #:cl-libtiff))
  (:export
   ;; Conditions
   #:loader-error
   ;; Types
   #:image
   #:slice-loader
   #:volume-loader
   #:loader
   ;; Functions
   #:loader-type
   #:loader-description
   #:loaders
   #:load-volume
   #:load-slice))
(in-package :slice2volume/loaders)

(define-condition loader-error (error)
  ((slicep       :reader  loader-error-slice-p
                 :initarg :slicep)
   (dimensions   :reader  loader-error-dimensions
                 :initarg :dimensions)
   (element-type :reader  loader-error-element-type
                 :initarg :element-type))
  (:documentation "Signalled when a loaded image has wrong dimensions and/or element-type.")
  (:report
   (lambda (c s)
     (format s
             #.(concatenate
                'string
                "Wrong array with dimensions ~a and element type ~a.~%"
                "Should be a ~d-dimensional array of dtype='uint8'")
             (loader-error-dimensions   c)
             (loader-error-element-type c)
             (if (loader-error-slice-p  c)
                 2 3)))))

(deftype image (n) `(simple-array (unsigned-byte 8) ,n))

(deftype slice-loader ()
  '(function (pathname) (values (image 2) &optional)))

(deftype volume-loader ()
  '(function (pathname) (values (image 3) &optional)))

(serapeum:-> rgba-intensities ((simple-array (unsigned-byte 32) (* *)))
             (values (simple-array (unsigned-byte 8) (* *)) &optional))
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
(serapeum:defconstructor tiff-dimensions
  (width  (unsigned-byte 32))
  (height (unsigned-byte 32))
  (depth  (unsigned-byte 32)))

(serapeum:-> image-dimensions (tiff:tiff-handle)
             (values (or tiff-dimensions null) &optional))
(defun image-dimensions (tiff)
  (let ((width  (tiff:width  tiff))
        (height (tiff:height tiff)))
    (labels ((%go (n)
               (declare (type fixnum n))
               (if (and (= (tiff:width  tiff) width)
                        (= (tiff:height tiff) height))
                   (let ((m (1+ n))
                         (nextp (tiff:read-directory tiff)))
                     (if nextp (%go m) m)))))
      (let ((depth (%go 0)))
        (if depth (tiff-dimensions width height depth))))))

(serapeum:-> read-tiff-slice (tiff:tiff-handle unsigned-byte unsigned-byte)
             (values (simple-array (unsigned-byte 8) (* *)) &optional))
(defun read-tiff-slice (handle width height)
  (rgba-intensities
   (tiff:read-rgba-image-oriented handle width height :topleft)))

(declaim (ftype slice-loader tiff-slice-loader))
(defun tiff-slice-loader (pathname)
  (tiff:with-open-tiff (handle pathname :input)
    (let ((width  (tiff:width  handle))
          (height (tiff:height handle)))
      (read-tiff-slice handle width height))))

(declaim (ftype volume-loader tiff-volume-loader))
(defun tiff-volume-loader (name)
  (declare (optimize (speed 3)))
  (tiff:with-open-tiff (tiff name :input)
    (let ((dimensions (image-dimensions tiff)))
      (unless dimensions
        (error "Image pages must have the same dimension"))
      (let* ((depth  (tiff-dimensions-depth  dimensions))
             (width  (tiff-dimensions-width  dimensions))
             (height (tiff-dimensions-height dimensions))
             (image (make-array (list depth height width)
                                :element-type '(unsigned-byte 8))))
        (tiff:set-directory tiff 0)
        (loop for i below depth
              for slice = (read-tiff-slice tiff width height)
              do (loop with idx = (array-row-major-index image i 0 0)
                       for j below (array-total-size slice) do
                         (setf (row-major-aref image (+ idx j))
                               (row-major-aref slice j)))
                   (tiff:read-directory tiff))
        image))))

;; General loader functions
(serapeum:defconstructor loader
  (type        string)
  (description string)
  (slice       (or slice-loader  null))
  (volume      (or volume-loader null)))

(defparameter *loaders*
  (list (loader "npy"  "Numpy array format" #'numpy-slice-loader #'numpy-volume-loader)
        (loader "tif"  "TIFF image format"  #'tiff-slice-loader  #'tiff-volume-loader)
        (loader "tiff" "TIFF image format"  #'tiff-slice-loader  #'tiff-volume-loader)))

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
