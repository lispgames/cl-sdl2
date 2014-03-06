(in-package :sdl2.kit)

(defvar *all-windows* (make-hash-table))
(defvar *idle-render-windows* (make-hash-table))

 ;; Base Window Classes

(defclass window ()
  ((sdl-window :initform nil :reader sdl-window)))

(defclass gl-window (window)
  ((gl-context :initform nil :reader gl-context)))

(declaim (inline sdl-window-id))
(defun sdl-window-id (window)
  (sdl2:get-window-id (sdl-window window)))

(defun window-from-id (sdl-window-id)
  (gethash sdl-window-id *all-windows*))

(defun all-windows ()
  (loop for id being each hash-key in *all-windows*
        collect (gethash id *all-windows*)))

 ;; Window Protocol

(defgeneric window-event (window type timestamp data1 data2)
  (:documentation "Handle an SDL_WindowEvent with type `TYPE`.
`DATA1` and `DATA2` are `TYPE`-specific values."))

(defgeneric mousebutton-event (window state timestamp button x y))
(defgeneric mousemotion-event (window timestamp button-mask x y xrel yrel))
(defgeneric mousewheel-event (window timestamp x y))
(defgeneric textinput-event (window timestamp text))
(defgeneric keyboard-event (window state timestamp repeat-p keysym))

(defgeneric render (window)
  (:documentation "Render the contents of WINDOW."))

(defgeneric close-window (window)
  (:documentation "Close a window and destroy any related context"))

(defgeneric other-event (window event)
  (:documentation "Handle a message with an otherwise-unspecified protocol"))

 ;; Window Implementation

;;;; General

(defgeneric idle-render (window))
(defgeneric (setf idle-render) (v window))

(defmethod idle-render ((window window))
  (nth-value 1 (gethash (sdl-window-id window) *idle-render-windows*)))

(defmethod (setf idle-render) (v (window window))
  (if v
      (setf (gethash (sdl-window-id window) *idle-render-windows*)
            window)
      (remhash (sdl-window-id window) *idle-render-windows*)))

(defmethod render (window))
(defmethod close-window (window))

;;;; WINDOW

(defmethod initialize-instance :around ((window window) &key &allow-other-keys)
  (sdl2:in-main-thread ()
    (call-next-method)))

(defmethod initialize-instance
    ((window window)
     &key (title "SDL2 Window") (x :centered) (y :centered) (w 800) (h 600)
     (shown t) flags &allow-other-keys)
  (call-next-method)
  (when shown
    (pushnew :shown flags))
  (with-slots (sdl-window) window
    (setf sdl-window (sdl2:create-window :title title :x x :y y :w w :h h
                                         :flags flags))
    (setf (gethash (sdl2:get-window-id sdl-window) *all-windows*)
          window)))

;;; Protocol
(defmethod window-event ((window window) type timestamp data1 data2))

(defmethod window-event ((window window) (type (eql :exposed)) timestamp data1 data2)
  (render window))

(defmethod window-event ((window window) (type (eql :close)) timestamp data1 data2)
  (close-window window))

(defmethod mousebutton-event ((window window) state timestamp button x y))
(defmethod mousemotion-event ((window window) timestamp button-mask x y xrel yrel))
(defmethod mousewheel-event ((window window) timestamp x y))
(defmethod textinput-event ((window window) timestamp text))
(defmethod keyboard-event ((window window) state timestamp repeat-p keysym))

(defmethod render :around ((window window))
  (sdl2:in-main-thread () (call-next-method)))

(defmethod close-window :around ((window window))
  (sdl2:in-main-thread () (call-next-method)))

(defmethod close-window :after ((window window))
  (with-slots (sdl-window) window
    (let ((id (sdl-window-id window)))
      (remhash id *idle-render-windows*)
      (remhash id *all-windows*))
    (sdl2:destroy-window sdl-window)))

(defmethod other-event ((window window) event)
  #+nil
  (format *error-output* "Unhandled event: ~S~%" (sdl2:get-event-type event)))

;;;; GL-WINDOW

(defmethod initialize-instance
    ((window gl-window)
     &key (title "SDL2 Window") (x :centered) (y :centered) (w 800) (h 600)
     (shown t) &allow-other-keys)
  (call-next-method window :title title :x x :y y :w w :h h :shown shown
                           :flags '(:opengl))
  (with-slots (gl-context) window
    (setf gl-context (sdl2:gl-create-context (sdl-window window)))
    (sdl2:gl-make-current (sdl-window window) gl-context)))

;;; Protocol
(defmethod close-window :after ((window gl-window))
  (with-slots (gl-context) window
    (sdl2:gl-delete-context gl-context)))

(defmethod render :before ((window gl-window))
  (with-slots (gl-context) window
    (sdl2:gl-make-current (sdl-window window) gl-context)))

(defmethod render :after ((window gl-window))
  (gl:flush)
  (sdl2:gl-swap-window (sdl-window window)))
