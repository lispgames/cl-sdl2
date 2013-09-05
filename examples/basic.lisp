(in-package :sdl2-examples)

(require :sdl2)
(require :cl-opengl)

(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init (:everything)
    (let* ((win (sdl2:create-window :flags '(:shown :opengl)))
           (gl-context (sdl2:gl-create-context win))
           (controllers ())
           (haptic ()))

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
              (let* ((gc (sdl2:game-controller-open i))
                     (joy (sdl2:game-controller-get-joystick gc)))
                (setf controllers (acons i gc controllers))
                (when (sdl2:joystick-is-haptic-p joy)
                  (let ((h (sdl2:haptic-open-from-joystick joy)))
                    (setf haptic (acons i h haptic))
                    (sdl2:rumble-init h))))))

      ;; main loop
      (sdl2:with-event-loop (:method :poll)
        (:keydown
         (:keysym keysym)
         (let ((scancode (sdl2:scancode-value keysym))
               (sym (sdl2:sym-value keysym))
               (mod-value (sdl2:mod-value keysym)))
           (if (sdl2:scancode= scancode :scancode-w)
               (format t "~a~%"
                       (if (and (sdl2::mod-value-p mod-value :kmod-rctrl :kmod-lctrl)
                                (sdl2::mod-value-p mod-value :kmod-rshift :kmod-lshift))
                           "Forward"
                           "Backward"))
               (progn
                 (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                         sym
                         scancode
                         mod-value)))
           (when (sdl2:scancode= scancode :scancode-s)
             (sdl2:show-cursor))
           (when (sdl2:scancode= scancode :scancode-h)
             (sdl2:hide-cursor))))
        
        (:keyup
         (:keysym keysym)
         (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
           (sdl2:push-event :quit)))
        
        (:mousemotion
         (:x x :y y :xrel xrel :yrel yrel :state state)
         (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                 x xrel y yrel state))
        
        (:controlleraxismotion
         (:which controller-id :axis axis-id :value value)
         (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                 controller-id axis-id value))
        
        (:controllerbuttondown
         (:which controller-id)
         (let ((h (cdr (assoc controller-id haptic))))
           (when h
             (sdl2:rumble-play h 1.0 100))))
        
        (:idle
         ()
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
      ;; as well as any haptics
      (loop for (i . controller) in controllers
         do (progn
              (sdl2:game-controller-close controller)
              (sdl2:haptic-close (cdr (assoc i haptic)))))
      
      (sdl2:gl-delete-context gl-context)
      (sdl2:destroy-window win))))
