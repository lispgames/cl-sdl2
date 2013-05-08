(in-package :sdl2)

(defctype sdl2-ffi::size-t :unsigned-int)

(define-foreign-library libsdl2
  (:unix (:or "libSDL2-2.0.so.0" "libSDL2"))
  (t (:default "libSDL2")))

(use-foreign-library libsdl2)

(defconstant SYS-TIMER          #x00000001)
(defconstant SYS-AUDIO          #x00000010)
(defconstant SYS-VIDEO          #x00000020)
(defconstant SYS-JOYSTICK       #x00000200)
(defconstant SYS-HAPTIC         #x00001000)
(defconstant SYS-GAMECONTROLLER #x00002000)
(defconstant NOPARACHUTE        #x00100000)
(defconstant EVERYTHING         #x0000FFFF)

(defun init (&rest flags)
  (let ((init-flags (if flags
                        (apply #'logior flags)
                        everything)))
    (let ((rc (sdl2-ffi::sdl-init init-flags)))
      (when (< rc 0)
        (error (sdl2-ffi::sdl-geterror))))))

(defun quit ()
  (sdl2-ffi::sdl-quit))
