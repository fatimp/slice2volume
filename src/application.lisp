(in-package :slice2volume)

(serapeum:-> expand-widget (gir::object-instance)
             (values &optional))
(defun expand-widget (widget)
  (setf (gtk4:widget-hexpand-p widget) t
        (gtk4:widget-vexpand-p widget) t
        (gtk4:widget-halign    widget) gtk4:+align-fill+
        (gtk4:widget-valign    widget) gtk4:+align-fill+)
  (values))

(serapeum:-> box-append-with-label (gir::object-instance gir::object-instance
                                    string (member :h :v))
             (values &optional))
(defun box-append-with-label (parent widget label orientation)
  (declare (optimize (speed 3)))
  (flet ((get-orientation (o)
           (the (not null)
                (case o
                  (:h gtk4:+orientation-horizontal+)
                  (:v gtk4:+orientation-vertical+)))))
    (declare (inline get-orientation))
    (let ((box   (gtk4:make-box   :orientation (get-orientation orientation)
                                  :spacing 5))
          (label (gtk4:make-label :str label)))
      (gtk4:box-append box widget)
      (gtk4:box-append box label)
      (gtk4:box-append parent box)))
  (values))

(serapeum:-> scale (double-float double-float double-float double-float)
             (values gir::object-instance &optional))
(defun scale (min max value step)
  (let ((scale (gtk4:make-scale :min min :max max :step step
                                :orientation gtk4:+orientation-horizontal+)))
    (setf (gtk4:widget-size-request scale) '(200 10)
          (gtk4:widget-margin-start scale) 15
          (gtk4:widget-margin-end   scale) 15
          (gtk4:range-value         scale) (float value 0d0)
          (gtk4:scale-digits scale) 3
          (gtk4:scale-draw-value-p scale) t)
    scale))

(gtk4:define-application (:name slice2volume :id "org.fatimp.slice2volume")
  (gtk4:define-main-window (window (gtk4:make-application-window
                                    :application gtk4:*application*))
    (setf (gtk4:window-title window) "Slice2Volume")
    ;; KLUDGE: this setter is broken in cl-gtk4, so use gir directly
    (gir:invoke (window "set_size_request") 1200 700)
    (let ((toplevel-box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                       :spacing 2))
          (huge-box     (gtk4:make-box :orientation gtk4:+orientation-horizontal+
                                       :spacing 0))
          (big-box      (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                       :spacing 10))
          (load-box     (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                       :spacing 5))
          (cut-box      (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                       :spacing 5))
          (plane-box    (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                       :spacing 5))
          (load-frame  (gtk4:make-frame :label "Data Loading"))
          (cut-frame   (gtk4:make-frame :label "Cutting Plane Control"))
          (plane-frame (gtk4:make-frame :label "Planar Space Control"))

          (area (gtk4:make-gl-area))
          (status (gtk4:make-label :str "Welcome to Slice2Volume"))
          ;; Data loading
          (slice-button  (gtk4:make-button :label "Load Slice"))
          (volume-button (gtk4:make-button :label "Load Volume"))
          (basis-button  (gtk4:make-button :label "Randomize Basis"))
          ;; Cutting plane
          (nx-scale (scale  0d0 1d0 0d0 1d-1))
          (ny-scale (scale  0d0 1d0 0d0 1d-1))
          (nz-scale (scale  0d0 1d0 1d0 1d-1))
          (d-scale  (scale -1d0 1d0 0d0 1d-1))
          ;; Planar space
          (ϕ-scale  (scale  (- pi) (+ pi) 0d0 1d-1))
          (Δu-scale (scale -1d0 1d0 0d0 1d-1))
          (Δv-scale (scale -1d0 1d0 0d0 1d-1)))
      (setf (gtk4:window-child window)      toplevel-box
            (gtk4:frame-child  load-frame)  load-box
            (gtk4:frame-child  cut-frame)   cut-box
            (gtk4:frame-child  plane-frame) plane-box)
      (expand-widget area)
      (gtk4:box-append toplevel-box huge-box)
      (gtk4:box-append toplevel-box status)
      (gtk4:box-append huge-box area)
      (gtk4:box-append huge-box big-box)
      (gtk4:box-append big-box  load-frame)
      (gtk4:box-append big-box  cut-frame)
      (gtk4:box-append big-box  plane-frame)
      (gtk4:box-append load-box slice-button)
      (gtk4:box-append load-box volume-button)
      (box-append-with-label cut-box nx-scale "nx" :h)
      (box-append-with-label cut-box ny-scale "ny" :h)
      (box-append-with-label cut-box nz-scale "nz" :h)
      (box-append-with-label cut-box d-scale  "d"  :h)
      (box-append-with-label plane-box ϕ-scale  "ϕ"  :h)
      (box-append-with-label plane-box Δu-scale "Δu" :h)
      (box-append-with-label plane-box Δv-scale "Δv" :h)
      (gtk4:box-append plane-box basis-button))
    (unless (gtk4:widget-visible-p window)
      (gtk4:window-present window))))
