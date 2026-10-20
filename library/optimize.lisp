(defpackage slice2volume/optimize
  (:use #:cl)
  (:local-nicknames (#:tran  #:slice2volume/transforms)
                    (#:slice #:slice2volume/slicer)
                    (#:mi    #:slice2volume/mutual-information))
  (:export #:register-slice))
(in-package :slice2volume/optimize)

(serapeum:-> vector->parameters ((simple-array double-float (6)))
             (values tran:trans-param &optional))
(defun vector->parameters (v)
  (tran:trans-param
   (float (aref v 0) 0f0)
   (float (aref v 1) 0f0)
   (float (aref v 2) 0f0)
   (float (aref v 3) 0f0)
   (float (aref v 4) 0f0)
   (float (aref v 5) 0f0)))

(serapeum:-> parameters->vector (tran:trans-param)
             (values (simple-array double-float (6)) &optional))
(defun parameters->vector (param)
  (let ((v (make-array 6 :element-type 'double-float)))
    (setf (aref v 0)
          (float (tran:trans-param-α  param) 0d0)
          (aref v 1)
          (float (tran:trans-param-Δu param) 0d0)
          (aref v 2)
          (float (tran:trans-param-Δv param) 0d0)
          (aref v 3)
          (float (tran:trans-param-d  param) 0d0)
          (aref v 4)
          (float (tran:trans-param-ϕ  param) 0d0)
          (aref v 5)
          (float (tran:trans-param-ψ  param) 0d0))
    v))

(serapeum:-> register-slice ((simple-array single-float (* *))
                             (simple-array single-float (* * *))
                             tran:trans-param)
             (values tran:trans-param double-float alexandria:positive-fixnum &optional))
(defun register-slice (slice volume initial-guess)
  (let ((height (array-dimension slice 0))
        (width  (array-dimension slice 1)))
    (flet ((target (trans)
             (let ((%slice (slice:slice
                            volume height width
                            (tran:plane-world-transform
                             (vector->parameters trans)))))
               (* (mi:mutual-information slice %slice 15)) -1d0)))
      (multiple-value-bind (result cost steps)
          (cl-optim:nelder-mead
           #'target
           (cl-optim:make-initial-simplex
            (parameters->vector initial-guess)
            5d-2)
           :max-iterations 1000
           :ftol  1d-6
           :xstol 1d-6)
        (values (vector->parameters result) cost steps)))))
