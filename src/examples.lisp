(in-package :sdl2-examples)

(require :sdl2)

(defun basic-test ()
  (sdl2:with-init (:video)
    (let ((quit nil)
          (win (sdl2:create-window :flags '(:shown :opengl))))
      (sdl2:with-sdl-event (sdl-event)
        (loop until quit
           do (let* ((event-data (sdl2:next-event sdl-event))
                     (event-type (getf (second event-data) :type)))
                (when (= 1 (first event-data))
                  (print event-type)
                  (if (eql :keyup event-type)
                      (sdl2:push-event :quitevent))))))
      (sdl2:destroy-window win))))
