(defpackage slice2volume/transforms
  (:use #:cl)
  (:export
   ;; Types
   #:trans-param
   ;; Accessors
   #:trans-param-α
   #:trans-param-Δu
   #:trans-param-Δv
   #:trans-param-d
   #:trans-param-ϕ
   #:trans-param-ψ
   ;; Functions
   #:trans-copy-into! ; Oh fuck
   #:diagonal3
   #:plane-world-transform
   #:store-trans-param
   #:load-trans-param))
(in-package :slice2volume/transforms)

(serapeum:-> diagonal3 (single-float single-float single-float)
             (values rtg-math.types:mat3 &optional))
(declaim (inline diagonal3))
(defun diagonal3 (a b c)
  (m3:make a 0.0 0.0 0.0 b 0.0 0.0 0.0 c))

(serapeum:-> plane-normal (single-float single-float)
             (values rtg-math.types:vec3 &optional))
(defun plane-normal (ϕ ψ)
  (declare (optimize (speed 3)))
  (let ((sinϕ (sin ϕ))
        (cosϕ (cos ϕ))
        (sinψ (sin ψ))
        (cosψ (cos ψ)))
    (v3:make (* cosψ cosϕ)
             (* cosψ sinϕ)
             sinψ)))

(serapeum:-> choose-tangent-vector (rtg-math.types:vec3)
             (values rtg-math.types:vec3 &optional))
(defun choose-tangent-vector (normal)
  (declare (optimize (speed 3)))
  (let ((orts (list #a(3 single-float . (0.0 0.0 1.0))
                    #a(3 single-float . (0.0 1.0 0.0))
                    #a(3 single-float . (1.0 0.0 0.0)))))
    (labels ((%go (orts best-vector best-dot-product)
               (if (null orts)
                   (values best-vector best-dot-product)
                   (let* ((vector (car orts))
                          (dot-product (v3:dot normal vector)))
                     (if (< dot-product (- best-dot-product 1f-1))
                         (%go (cdr orts) vector dot-product)
                         (%go (cdr orts) best-vector best-dot-product))))))
      (multiple-value-bind (vector dot)
          (%go orts normal 1.0)
        (v3:normalize (v3:- vector (v3:*s normal dot)))))))

(serapeum:-> world-transform-matrix (rtg-math.types:vec3 rtg-math.types:vec3)
             (values rtg-math.types:mat3 &optional))
(defun world-transform-matrix (v n)
  ;; V and N are already normalized
  (let* ((v2 v)
         (v1 (v3:cross v2 n)))
    (m3:from-columns v1 v2 n)))

(serapeum:-> planar-transform-matrix (single-float single-float single-float)
             (values rtg-math.types:mat3 &optional))
(defun planar-transform-matrix (ϕ Δu Δv)
  (let ((cos (cos ϕ))
        (sin (sin ϕ)))
    (m3:make cos (- sin) Δu (+ sin) cos Δv 0.0 0.0 1.0)))

(serapeum:-> %plane-world-transform (single-float single-float single-float
                                     single-float single-float single-float)
             (values rtg-math.types:mat3 &optional))
(defun %plane-world-transform (ϕ ψ d α Δu Δv)
  (let ((normal (plane-normal ϕ ψ)))
    (m3:*
     (world-transform-matrix
      (choose-tangent-vector normal)
      normal)
     (diagonal3 1.0 1.0 d)
     (planar-transform-matrix α Δu Δv))))

(defstruct (trans-param (:constructor trans-param (α Δu Δv d ϕ ψ)))
  (α  0.0 :type single-float)
  (Δu 0.0 :type single-float)
  (Δv 0.0 :type single-float)
  ;; Distance to the cutting plane
  (d  0.0 :type single-float)
  ;; Normal angles
  (ϕ  0f0 :type single-float)
  (ψ  0f0 :type single-float))

(serapeum:-> plane-world-transform (trans-param)
             (values rtg-math.types:mat3 &optional))
(defun plane-world-transform (param)
  (%plane-world-transform
   (trans-param-ϕ  param)
   (trans-param-ψ  param)
   (trans-param-d  param)
   (trans-param-α  param)
   (trans-param-Δu param)
   (trans-param-Δv param)))

(serapeum:-> store-trans-param (trans-param (or pathname string))
             (values &optional))
(defun store-trans-param (param name)
  (cl-store:store param name)
  (values))

(serapeum:-> load-trans-param ((or pathname string))
             (values trans-param &optional))
(defun load-trans-param (name)
  (cl-store:restore name))

(serapeum:-> trans-copy-into! (trans-param trans-param)
             (values &optional))
(defun trans-copy-into! (dst src)
  (setf (trans-param-α  dst)
        (trans-param-α  src)
        (trans-param-Δu dst)
        (trans-param-Δu src)
        (trans-param-Δv dst)
        (trans-param-Δv src)
        (trans-param-d  dst)
        (trans-param-d  src)
        (trans-param-ϕ  dst)
        (trans-param-ϕ  src)
        (trans-param-ψ  dst)
        (trans-param-ψ  src))
  (values))
