(in-package #:sdl2)


(defun joystick-is-haptic-p (joystick)
  (sdl-true-p (sdl-joystick-is-haptic joystick)))

(defun mouse-is-haptic-p ()
  (sdl-true-p (sdl-mouse-is-haptic)))

;; TODO wth can't I use the macro above?
(defun haptic-open (source)
  "Use this function to open the N'th haptic device for use."
  (check-nullptr (sdl-haptic-open source)))

(defun haptic-open-from-joystick (source)
  "Use this function to open a joystick haptic device for use."
  (check-nullptr (sdl-haptic-open-from-joystick source)))

(defun haptic-open-from-mouse ()
  "Use this function to open the mouses haptic device for use."
  (check-nullptr (sdl-haptic-open-from-mouse)))

(defun haptic-close (haptic)
  "Use this function to close an opened haptic device."
  (sdl-haptic-close haptic))

(defun haptic-index (haptic)
  (sdl-haptic-index haptic))

(defun haptic-opened-p (haptic)
  (sdl-true-p (sdl-haptic-opened (haptic-index haptic))))

(defun rumble-supported-p (haptic)
  "Use this function to test whether rumble is supported on a haptic device."
  (sdl-true-p (sdl-haptic-rumble-supported haptic)))

(defun rumble-init (haptic)
  "Use this function to initialize a haptic device for simple rumble."
  (check-rc (sdl-haptic-rumble-init haptic)))

(defun rumble-play (haptic strength duration)
  "Use this function to play a simple rumble effect on a haptic device."
  (check-rc (sdl-haptic-rumble-play haptic strength duration)))

(defun rumble-stop (haptic)
  "Use this function to stop the rumble on a haptic device."
  (check-rc (sdl-haptic-rumble-stop haptic)))
