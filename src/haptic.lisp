(in-package #:sdl2)


(defun joystick-is-haptic (joystick)
  (sdl-true-p (sdl-joystick-is-haptic joystick)))

(defun mouse-is-haptic ()
  (sdl-true-p (sdl-mouse-is-haptic)))

(defmacro %haptic-open (fn source)
  `(sdl-collect
    (check-null (,fn ,source))
    (lambda (h) (sdl-haptic-close h))))

;; TODO wth can't I use the macro above?
(defun haptic-open (source)
  (sdl-collect
   (check-null (sdl-haptic-open source))
   (lambda (h) (sdl-haptic-close h))))

(defun haptic-open-from-joystick (source)
  (sdl-collect
   (check-null (sdl-haptic-open-from-joystick source))
   (lambda (h) (sdl-haptic-close h))))

(defun haptic-open-from-mouse ()
  (sdl-collect
   (check-null (sdl-haptic-open-from-mouse))
   (lambda (h) (sdl-haptic-close h))))

(defun haptic-close (haptic)
  (sdl-haptic-close haptic)
  (sdl-cancel-collect haptic))

(defun rumble-init (haptic)
  (check-rc (sdl-haptic-rumble-init haptic)))

(defun rumble-play (haptic strength duration)
  (check-rc (sdl-haptic-rumble-play haptic strength duration)))

