(in-package :sdl2-examples)

(require :sdl2)
(require :cl-opengl)

(defun basic-test ()
  (sdl2:with-init (:everything)
    (let* ((win (sdl2:create-window :flags '(:shown :opengl)))
           (gl-context (sdl2:gl-create-context win))
           (controllers ()))

      ;; basic window/gl setup
      (sdl2:gl-make-current win gl-context)
      (gl:viewport 0 0 800 600)
      (gl:matrix-mode :projection)
      (gl:ortho -2 2 -2 2 -2 2)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:clear-color 0.0 0.0 1.0 1.0)
      (gl:clear :color-buffer)

      ;; open any game controllers
      (loop for i from 0 upto (- (sdl2:joystick-count) 1)
         do (when (sdl2:game-controller-p i)
              (format t "Found gamecontroller: ~a~%"
                      (sdl2:game-controller-name-for-index i))
              (setf controllers (acons i (sdl2:game-controller-open i) controllers))))
      
      (sdl2:with-event-loop (:method :poll)
        (:keydown (:keysym keysym)
          (let ((scancode (sdl2:scancode-value keysym))
                (sym (sdl2:sym-value keysym))
                (mod-value (sdl2:mod-value keysym)))
            (if (sdl2:scancode= scancode :scancode-w)
                (if (and
                     (sdl2::mod-value-p mod-value :kmod-rctrl :kmod-lctrl)
                     (sdl2::mod-value-p mod-value :kmod-rshift :kmod-lshift))
                    (print "BACKWARD")
                    (print "FORWARD"))
                (progn
                  (print (format nil "Key sym: ~a, code: ~a, mod: ~a"
                                 sym
                                 scancode
                                 mod-value))))))
        (:keyup (:keysym keysym)
          (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
            (sdl2:push-event :quit)))
        (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
          (print (format
                  nil
                  "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a"
                  x xrel y yrel state)))
        (:controlleraxismotion (:which controller-id :axis axis-id :value value)
           (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                   controller-id axis-id value))
        (:idle ()
          (gl:clear :color-buffer)
          (gl:begin :triangles)
          (gl:color 1.0 0.0 0.0)
          (gl:vertex 0.0 1.0)
          (gl:vertex -1.0 -1.0)
          (gl:vertex 1.0 -1.0)
          (gl:end)
          (gl:flush)
          (sdl2:gl-swap-window win))
        (:quit () t))

      ;; close any game controllers that were opened
      (loop for (_ . controller) in controllers
         do (sdl2:game-controller-close controller))
      
      (sdl2:gl-delete-context gl-context)
      (sdl2:destroy-window win))))
