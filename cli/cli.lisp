(in-package :slice2volume/cli)

(defparameter *clip-ratio*     0.02)
(defparameter *max-iterations* 1000)
(defparameter *xstol*          1d-6)
(defparameter *ftol*           1d-6)
(defparameter *nbins*          15)

(defun parse-positive-integer (string)
  (let ((n (parse-integer string)))
    (unless (plusp n)
      (error "Must be positive integer"))
    n))

(declaim (inline float-parser))
(defun float-parser (type)
  (lambda (string)
    (let ((x (pf:parse-float string :type type)))
      (unless (plusp x)
        (error "Must be positive float"))
      x)))

(defparameter *parser*
  (seq
   (optional
    (option :max-iter    "N"
            :long        "max-iterations"
            :short       #\i
            :fn          #'parse-positive-integer
            :description "Maximal number of iterations")
    (option :nbins       "M"
            :long        "nbins"
            :short       #\b
            :fn          #'parse-positive-integer
            :description "Number of histogram bins")
    (option :clip-ratio  "R"
            :long        "clip-ratio"
            :short       #\r
            :fn          (float-parser 'single-float)
            :description "Normalization clip ratio")
    (option :xstol       "XSTOL"
            :long        "xstol"
            :short       #\t
            :fn          (float-parser 'double-float)
            :description "Parameter variance tolerance")
    (option :xstol       "FTOL"
            :long        "ftol"
            :short       #\T
            :fn          (float-parser 'double-float)
            :description "Mutual information variance tolerance"))
   (argument :fixed   "fixed")
   (argument :moving  "moving")
   (argument :initial "initial-guess")
   (argument :output  "output")))

(defun parse-args ()
  (handler-case
      (parse-argv *parser*)
    (error (c)
      (princ c *error-output*)
      (terpri  *error-output*)
      (print-usage *parser*)
      (uiop:quit 1))))

(defun slice2volume ()
  (let* ((argv (parse-args))
         (fixed      (%assoc :fixed      argv))
         (moving     (%assoc :moving     argv))
         (output     (%assoc :output     argv))
         (initial    (%assoc :initial    argv))
         (max-iter   (%assoc :max-iter   argv 1000))
         (nbins      (%assoc :nbins      argv 15))
         (clip-ratio (%assoc :clip-ratio argv 0.02))
         (xstol      (%assoc :xstol      argv 1d-6))
         (ftol       (%assoc :ftol       argv 1d-6))

         (fixed  (norm:normalize-image
                  (load:load-slice  (pathname fixed))
                  clip-ratio))
         (moving (norm:normalize-image
                  (load:load-volume (pathname moving))
                  clip-ratio))
         (initial (tran:load-trans-param initial)))
    (multiple-value-bind (transform score steps)
        (opt:register-slice fixed moving initial
                            :nbins          nbins
                            :max-iterations max-iter
                            :ftol           ftol
                            :xstol          xstol)
      (declare (ignore steps))
      (tran:store-trans-param transform output)
      (print score))))
    
