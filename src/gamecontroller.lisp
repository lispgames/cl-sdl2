(in-package #:sdl2)

;; SDL2 has some mappings built-in, but since I have a wonderful Logitech controller that isn't part
;; of the spec yet I embed these here. Also it's a good example of how to provide your own

(defparameter *default-controller-maps* ())

(defmacro add-to-default-controller-maps (&rest maps)
  `(setf *default-controller-maps*
         (append *default-controller-maps* ,@maps)))

(add-to-default-controller-maps
 ;; The primary mode for the F710 is basically an Xbox controller
 '((guid "030000006d0400001fc2000005030000")
   (name "Logitech Wireless Gamepad F710")
   (a b0)
   (b b1)
   (x b2)
   (y b3)
   (start b7)
   (guide b8)
   (back b6)
   (leftshoulder b4)
   (rightshoulder b5)
   (leftstick b9)
   (rightstick b10)
   (dpup h0.1)
   (dpdown h0.4)
   (dpleft h0.8)
   (dpright h0.2)
   (leftx a0)
   (lefty a1)
   (rightx a3)
   (righty a4)
   (lefttrigger a2)
   (righttrigger a5))
 ;; The F710 has a "legacy" mode for systems that don't support a controller that looks like an XBox
 ;; controller
 '((guid "6d0400000000000019c2000000000000")
   (name "Logitech Wireless Gamepad F710 (alt)")
   (a b0)
   (b b1)
   (x b2)
   (y b3)
   (start b7)
   (guide b8)
   (back b6)
   (leftshoulder b4)
   (rightshoulder b5)
   (leftstick b9)
   (rightstick b10)
   (dpup h0.1)
   (dpdown h0.4)
   (dpleft h0.8)
   (dpright h0.2)
   (leftx a0)
   (lefty a1)
   (rightx a4)
   (righty a3)
   (lefttrigger a2)
   (righttrigger a5)))

(defun game-controller-p (device-index)
  "Returns t if the device-index provided belongs to a joystick with a known gamecontroller
mapping."
  (sdl-true-p (sdl-is-game-controller device-index)))

(defun game-controller-name-for-index (device-index)
  "Return the human readable name for the device-index provided."
  (sdl-game-controller-name-for-index device-index))

(defun game-controller-open (device-index)
  (check-nullptr (sdl-game-controller-open device-index)))

(defun game-controller-close (gamecontroller)
  (sdl-game-controller-close gamecontroller))

(defun game-controller-attached-p (gamecontroller)
  (sdl-true-p (sdl-game-controller-get-attached gamecontroller)))

(defun game-controller-add-mapping (mapping-string)
  "Use this function to add support for controllers that SDL is unaware of or to cause an existing
controller to have a different binding."
  (sdl-game-controller-add-mapping mapping-string))

(defun game-controller-get-joystick (gamecontroller)
  (sdl-game-controller-get-joystick gamecontroller))

(defun game-controller-instance-id (c)
  (joystick-instance-id (game-controller-get-joystick c)))

(defun game-controller-from-instance-id (instance-id)
  (sdl-game-controller-from-instance-id instance-id))

(defun game-controller-add-mappings-from-file (file-name)
  (sdl-game-controller-add-mappings-from-rw
   (sdl-rw-from-file file-name "rb")
   1))

(defun game-controller-name (gamecontroller)
  (sdl-game-controller-name gamecontroller))
