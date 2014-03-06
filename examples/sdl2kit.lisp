;;; HOW TO USE:
;;;
;;; First, run this.  It is SAFE to run repeatedly:
;;;
;;;   (sdl2.kit:start)
;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:test-window)
;;;
;;; You can make multiple windows if you want.  Note that, despite not
;;; assigning the value, THIS IS NOT COLLECTED.  A reference is kept
;;; to all windows:
;;;
;;;   (all-windows)
;;;
;;; After you close a window, it will be collected at some point.
;;;
;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).

(defpackage :sdl2.kit.test
  (:use #:cl #:alexandria #:sdl2.kit)
  (:export #:test-window))

(in-package :sdl2.kit.test)

(defclass test-window (gl-window)
  ((rotation :initform 0.0)))

;;; All of these methods are OPTIONAL.  However, without a render
;;; method, your window will not look like much!

(defmethod initialize-instance ((w test-window) &key &allow-other-keys)
  ;; It is critical you call-next-method first, or you won't get a GL
  ;; context or window.
  (call-next-method)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod render ((window test-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW
  ;; after RENDER.
  (with-slots (rotation) window
    (gl:load-identity)
    (gl:rotate rotation 0 0 1)
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:clear :color-buffer)
    (gl:begin :triangles)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0 1.0)
    (gl:vertex -1.0 -1.0)
    (gl:vertex 1.0 -1.0)
    (gl:end)))

(defmethod close-window ((window test-window))
  ;; Closing the GL context and window are done in :AFTER
  (format t "Bye!~%"))

(defmethod mousewheel-event ((window test-window) ts x y)
  (with-slots (rotation) window
    (incf rotation (* 12 y))
    (render window)))

(defmethod textinput-event ((window test-window) ts text)
  (format t "You typed: ~S~%" text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event ((window test-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (unless repeat-p
      (format t "~A ~S~%" state scancode))
    (when (eq :scancode-escape scancode)
      (close-window window))))

(defmethod mousebutton-event ((window test-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

(defmethod mousemotion-event ((window test-window) ts mask x y xr yr)
  (when (> mask 0)
    (format t "Mouse motion, button-mask = ~A at ~A, ~A~%" mask x y)))

;;; (sdl2.kit:start)
;;; (make-instance 'test-window)
