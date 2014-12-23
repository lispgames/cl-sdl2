(in-package :sdl2-examples)

(require :sdl2)
(require :cl-opengl)

(defun test-render-clear (renderer)
  (progn (sdl2:set-render-draw-color renderer 0 0 0 255)
         (sdl2:render-clear renderer)))

(defun test-render-hello (renderer)
  (progn (sdl2:set-render-draw-color renderer 255 0 0 255)
         ;; H
         (sdl2:render-draw-line renderer 20 20 20 100)
         (sdl2:render-draw-line renderer 20 60 60 60)
         (sdl2:render-draw-line renderer 60 20 60 100)
         ;; E
         (sdl2:render-draw-line renderer 80 20 80 100)
         (sdl2:render-draw-line renderer 80 20 120 20)
         (sdl2:render-draw-line renderer 80 60 120 60)
         (sdl2:render-draw-line renderer 80 100 120 100)
         ;; L
         (sdl2:render-draw-line renderer 140 20 140 100)
         (sdl2:render-draw-line renderer 140 100 180 100)
         ;; L
         (sdl2:render-draw-line renderer 200 20 200 100)
         (sdl2:render-draw-line renderer 200 100 240 100)
         ;; O
         (sdl2:render-draw-line renderer 260 20 260 100)
         (sdl2:render-draw-line renderer 260 100 300 100)
         (sdl2:render-draw-line renderer 300 20 300 100)
         (sdl2:render-draw-line renderer 260 20 300 20)
         ))

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
           (test-render-hello renderer)
           (sdl2:render-present renderer))
          (:quit () t))))))
