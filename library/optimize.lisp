(defpackage slice2volume/optimize
  (:use #:cl)
  (:local-nicknames (#:tran  #:slice2volume/transforms)
                    (#:slice #:slice2volume/slicer)
                    (#:mi    #:slice2volume/mutual-information))
  (:export #:register-slice))
(in-package :slice2volume/optimize)

(serapeum:-> vector->parameters ((simple-array double-float (7)))
             (values tran:trans-param &optional))
(defun vector->parameters (v)
  (tran:trans-param
   (float (aref v 0) 0f0)
   (float (aref v 1) 0f0)
   (float (aref v 2) 0f0)
   (float (aref v 3) 0f0)
   (float (aref v 4) 0f0)
   (float (aref v 5) 0f0)
   (float (aref v 6) 0f0)))

(serapeum:-> parameters->vector (tran:trans-param)
             (values (simple-array double-float (7)) &optional))
(defun parameters->vector (param)
  (let ((v (make-array 7 :element-type 'double-float)))
    (setf (aref v 0)
          (float (tran:trans-param-s  param) 0d0)
          (aref v 1)
          (float (tran:trans-param-Δu param) 0d0)
          (aref v 2)
          (float (tran:trans-param-Δv param) 0d0)
          (aref v 3)
          (float (tran:trans-param-d  param) 0d0)
          (aref v 4)
          (float (tran:trans-param-α  param) 0d0)
          (aref v 5)
          (float (tran:trans-param-ϕ  param) 0d0)
          (aref v 6)
          (float (tran:trans-param-ψ  param) 0d0))
    v))

(serapeum:-> simplex-Δ (double-float double-float double-float)
             (values (simple-array double-float (*)) &optional))
(defun simplex-Δ (Δangle Δshift Δscale)
  (make-array 7
              :element-type 'double-float
              :initial-contents (list Δscale
                                      Δshift Δshift Δshift
                                      Δangle Δangle Δangle)))

(deftype registration-logger ()
  '(function (tran:trans-param single-float) (values &optional)))

(defun dummy-logger (parameters mi)
  (declare (ignore parameters mi))
  (values))

(serapeum:-> register-slice ((simple-array single-float (* *))
                             (simple-array single-float (* * *))
                             tran:trans-param &key
                             (:nbins          alexandria:positive-fixnum)
                             (:max-iterations alexandria:positive-fixnum)
                             (:ftol           double-float)
                             (:xstol          double-float)
                             (:Δangle         double-float)
                             (:Δshift         double-float)
                             (:Δscale         double-float)
                             (:logger         registration-logger))
             (values tran:trans-param
                     double-float double-float
                     alexandria:positive-fixnum &optional))
(defun register-slice (slice volume initial-guess
                       &key
                         (nbins          15)
                         (max-iterations 1000)
                         (ftol           1d-6)
                         (xstol          1d-6)
                         (Δangle         5d-2)
                         (Δshift         5d-2)
                         (Δscale         1d-1)
                         (logger #'dummy-logger))
  (let ((height (array-dimension slice 0))
        (width  (array-dimension slice 1)))
    (labels ((%target (params)
               (let* ((%slice (slice:slice
                               volume height width
                               (tran:plane-world-transform params)))
                      (mi (mi:mutual-information slice %slice nbins)))
                 (funcall logger params mi)
                 (* mi -1d0)))
             (target (trans)
               (%target(vector->parameters trans))))
      (declare (inline %target))
      (let ((initial-cost (%target initial-guess)))
        (multiple-value-bind (result final-cost steps)
            (cl-optim:nelder-mead
             #'target
             (cl-optim:make-initial-simplex
              (parameters->vector initial-guess)
              (simplex-Δ Δangle Δshift Δscale))
             :max-iterations max-iterations
             :ftol           ftol
             :xstol          xstol)
          (values (vector->parameters result)
                  (- initial-cost)
                  (- final-cost)
                  steps))))))
