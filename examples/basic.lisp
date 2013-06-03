(in-package :sdl2-examples)

(require :sdl2)

(defun basic-test ()
  (sdl2:with-init (:video)
    (let ((quit nil)
          (win (sdl2:create-window :flags '(:shown :opengl))))
      (sdl2:with-sdl-event (sdl-event))
      (sdl2:destroy-window win))))
