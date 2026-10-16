(in-package :slice2volume/ui)

(define-condition slice2volume-error (error)
  ()
  (:documentation "Generic Slice2Volume error."))

(define-condition loader-error (slice2volume-error)
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
