(in-package :slice2volume/ui)

(serapeum:-> add-filters-to-file-chooser-dialog (gir::object-instance lib:loader-type)
             (values &optional))
(defun add-filters-to-file-chooser-dialog (dialog type)
  (let ((loaders (lib:loaders type)))
    (loop for loader in loaders
          for filter = (gtk4:make-file-filter) do
            (gtk4:file-filter-add-suffix filter (lib:loader-type loader))
            (setf (gtk4:file-filter-name filter) (lib:loader-description loader))
            (gtk4:file-chooser-add-filter dialog filter))
    (when (cdr loaders)
      (loop with filter = (gtk4:make-file-filter)
            for loader in loaders do
              (gtk4:file-filter-add-suffix filter (lib:loader-type loader))
            finally
               (setf (gtk4:file-filter-name filter) "All supported formats")
               (gtk4:file-chooser-add-filter dialog filter))))
  (values))

(serapeum:-> file-chooser-dialog/open
    (gir::object-instance lib:loader-type
     (function (pathname) (values &optional)))
    (values &optional))
(defun file-chooser-dialog/open (parent-window type callback)
  (let ((dialog (gtk4:make-file-chooser-native
                 :title "Choose a model"
                 :parent parent-window
                 :action gtk4:+file-chooser-action-open+
                 :accept-label "Open"
                 :cancel-label "Cancel")))
    (add-filters-to-file-chooser-dialog dialog type)
    (gtk4:connect
     dialog "response"
     (lambda (widget response)
       (declare (ignore widget))
       (when (= response gtk4:+response-type-accept+)
         (let ((file (gio:file-path
                      (gtk4:file-chooser-file dialog))))
           (funcall callback (pathname file))))))
    (gtk4:native-dialog-show dialog)
    (values)))

(serapeum:-> file-chooser-dialog/save
    (gir::object-instance (function (pathname) (values &optional)))
    (values &optional))
(defun file-chooser-dialog/save (parent-window callback)
  (let ((dialog (gtk4:make-file-chooser-native
                 :title "Save a transform"
                 :parent parent-window
                 :action gtk4:+file-chooser-action-save+
                 :accept-label "Save"
                 :cancel-label "Cancel")))
    (let ((filter (gtk4:make-file-filter)))
      (gtk4:file-filter-add-suffix filter "npy")
      (setf (gtk4:file-filter-name filter) "Numpy array")
      (gtk4:file-chooser-add-filter dialog filter))
    (gtk4:connect
     dialog "response"
     (lambda (widget response)
       (declare (ignore widget))
       (when (= response gtk4:+response-type-accept+)
         (let ((file (gio:file-path
                      (gtk4:file-chooser-file dialog))))
           (funcall callback (pathname file))))))
    (gtk4:native-dialog-show dialog)
    (values)))

(serapeum:-> error-dialog (gir::object-instance condition)
             (values &optional))
(defun error-dialog (parent condition)
  (let ((dialog (gtk4:make-dialog)))
    (setf (gtk4:window-transient-for dialog) parent)
    (gtk4:dialog-add-button dialog "Close" gtk4:+response-type-close+)
    (let ((label (gtk4:make-label
                  :str (with-output-to-string (out)
                         (princ condition out)))))
      (gtk4:box-append (gtk4:dialog-content-area dialog) label))
    (gtk4:connect dialog "response"
                  (lambda (widget id)
                    (declare (ignore widget id))
                    (gtk4:window-destroy dialog)))
    (setf (gtk4:window-modal-p dialog) t
          (gtk4:window-resizable-p dialog) nil
          (gtk4:window-title dialog) "Error")
    (gtk4:widget-show dialog))
  (values))

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

(serapeum:-> scale (real real real real)
             (values gir::object-instance &optional))
(defun scale (min max value step)
  (flet ((d (x) (float x 0d0)))
    (declare (inline d))
    (let ((scale (gtk4:make-scale :min (d min) :max (d max) :step (d step)
                                  :orientation gtk4:+orientation-horizontal+)))
      (setf (gtk4:widget-size-request scale) '(200 10)
            (gtk4:widget-margin-start scale) 15
            (gtk4:widget-margin-end   scale) 15
            (gtk4:range-value         scale) (d value)
            (gtk4:scale-digits scale) 3
            (gtk4:scale-draw-value-p scale) t)
      scale)))

(gtk4:define-application (:name slice2volume :id "org.fatimp.slice2volume")
  (gtk4:define-main-window (window (gtk4:make-application-window
                                    :application gtk4:*application*))
    (setf (gtk4:window-title window) "Slice2Volume")
    ;; KLUDGE: this setter is broken in cl-gtk4, so use gir directly
    (gir:invoke (window "set_size_request") 1200 700)
    (let* ((scene (make-scene))
           (toplevel-box (gtk4:make-box :orientation gtk4:+orientation-vertical+
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
           (save-frame  (gtk4:make-frame :label "Result exporting"))
           (cut-frame   (gtk4:make-frame :label "Cutting Plane Control"))
           (plane-frame (gtk4:make-frame :label "Planar Space Control"))

           (renderer (make-drawing-area scene))
           (status (gtk4:make-label :str "Welcome to Slice2Volume"))
           ;; Data loading
           (slice-button   (gtk4:make-button :label "Load Slice"))
           (volume-button  (gtk4:make-button :label "Load Volume"))
           ;; Saving the result
           (save-transform (gtk4:make-button :label "Save transform"))
           ;; Cutting plane
           (nx-scale (scale  0f0 1f0 (aref (scene-n scene) 0) 1f-2))
           (ny-scale (scale  0f0 1f0 (aref (scene-n scene) 1) 1f-2))
           (nz-scale (scale  0f0 1f0 (aref (scene-n scene) 2) 1f-2))
           (d-scale  (scale -1f0 1f0 (scene-d scene) 1f-2))
           ;; Planar space
           (ϕ-scale  (scale  (- pi) (+ pi) (scene-ϕ scene) 1f-2))
           (Δu-scale (scale -1f0 1f0 (scene-Δu scene) 1f-2))
           (Δv-scale (scale -1f0 1f0 (scene-Δv scene) 1f-2)))
      (setf (gtk4:window-child window)      toplevel-box
            (gtk4:frame-child  load-frame)  load-box
            (gtk4:frame-child  cut-frame)   cut-box
            (gtk4:frame-child  plane-frame) plane-box
            (gtk4:frame-child  save-frame)  save-transform)
      (expand-widget (renderer-area renderer))
      (gtk4:box-append toplevel-box huge-box)
      (gtk4:box-append toplevel-box status)
      (gtk4:box-append huge-box (renderer-area renderer))
      (gtk4:box-append huge-box big-box)
      (gtk4:box-append big-box  load-frame)
      (gtk4:box-append big-box  cut-frame)
      (gtk4:box-append big-box  plane-frame)
      (gtk4:box-append big-box  save-frame)
      (gtk4:box-append load-box slice-button)
      (gtk4:box-append load-box volume-button)
      (box-append-with-label cut-box nx-scale "nx" :h)
      (box-append-with-label cut-box ny-scale "ny" :h)
      (box-append-with-label cut-box nz-scale "nz" :h)
      (box-append-with-label cut-box d-scale  "d"  :h)
      (box-append-with-label plane-box ϕ-scale  "ϕ"  :h)
      (box-append-with-label plane-box Δu-scale "Δu" :h)
      (box-append-with-label plane-box Δv-scale "Δv" :h)
      (flet ((%connect (scale f)
               (gtk4:connect
                scale "value-changed"
                (lambda (widget)
                  (declare (ignore widget))
                  (let ((x (float (gtk4:range-value scale) 0f0)))
                    (funcall f x))
                  (when (randomization-needed-p
                         (scene-n scene)
                         (scene-v scene))
                    (setf (scene-v scene) (random-vec3)))
                  (gtk4:gl-area-queue-render (renderer-area renderer))))))
        (macrolet ((connect (scale place)
                     `(%connect ,scale (lambda (x) (setf ,place x)))))
          (connect nx-scale (aref (scene-n  scene) 0))
          (connect ny-scale (aref (scene-n  scene) 1))
          (connect nz-scale (aref (scene-n  scene) 2))
          (connect  d-scale       (scene-d  scene))
          (connect  ϕ-scale       (scene-ϕ  scene))
          (connect Δu-scale       (scene-Δu scene))
          (connect Δv-scale       (scene-Δv scene))))
      (gtk4:connect
       volume-button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (file-chooser-dialog/open
          window :volume
          (lambda (pathname)
            (handler-case
                (let ((volume (lib:load-volume pathname)))
                  (when volume
                    (setf (gtk4:label-text status)
                          (format nil "Volume with dimensions ~a loaded"
                                  (array-dimensions volume)))
                    (funcall (renderer-volume-uploader renderer) volume)
                    (gtk4:gl-area-queue-render (renderer-area renderer))))
              (error (c) (error-dialog window c)))))))
      (gtk4:connect
       slice-button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (file-chooser-dialog/open
          window :slice
          (lambda (pathname)
            (handler-case
                (let ((slice (lib:load-slice pathname)))
                  (when slice
                    (setf (gtk4:label-text status)
                          (format nil "Slice with dimensions ~a loaded"
                                  (array-dimensions slice)))
                    (funcall (renderer-slice-uploader renderer) slice)
                    (gtk4:gl-area-queue-render (renderer-area renderer))))
              (error (c) (error-dialog window c)))))))
      (gtk4:connect
       save-transform "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (file-chooser-dialog/save
          window
          (lambda (pathname)
            (handler-case
                (numpy-npy:store-array
                 (plane-world-transform scene)
                 pathname)
              (error (c) (error-dialog window c))))))))
    (unless (gtk4:widget-visible-p window)
      (gtk4:window-present window))))
