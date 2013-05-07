(in-package :sdl2)

(defctype sdl2-ffi::size-t :unsigned-int)

(define-foreign-library libsdl2
  (t (:default "libSDL2")))

(use-foreign-library libsdl2)

(defconstant SDL-INIT-TIMER          #x00000001)
(defconstant SDL-INIT-AUDIO          #x00000010)
(defconstant SDL-INIT-VIDEO          #x00000020)
(defconstant SDL-INIT-JOYSTICK       #x00000200)
(defconstant SDL-INIT-HAPTIC         #x00001000)
(defconstant SDL-INIT-GAMECONTROLLER #x00002000)
(defconstant SDL-INIT-NOPARACHUTE    #x00100000)
(defconstant SDL-INIT-EVERYTHING     #x0000FFFF)


