(in-package :sdl2-examples)

(require :sdl2)
(require :cl-opengl)

(defun test-render-clear (renderer)
  (progn (sdl2:set-render-draw-color renderer 0 0 0 255)
         (sdl2:render-clear renderer)))

(defun test-render-line (renderer)
  (progn (sdl2:set-render-draw-color renderer 255 0 0 255)
         (sdl2:render-draw-line renderer 2 2 30 200)))

(defun renderer-test ()
  "Test the SDL_render.h API"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:renderer-accelerated))

        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (test-render-clear renderer)
           (test-render-line renderer)
           (sdl2:render-present renderer))
          (:quit () t))))))
