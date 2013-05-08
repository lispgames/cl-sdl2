;;;; sdl2.lisp

(in-package #:sdl2)

;;; "sdl2" goes here. Hacks and glory await!

(defconstant SYS-TIMER          sdl2-ffi::+sdl-init-timer+)
(defconstant SYS-AUDIO          sdl2-ffi::+sdl-init-audio+)
(defconstant SYS-VIDEO          sdl2-ffi::+sdl-init-video+)
(defconstant SYS-JOYSTICK       sdl2-ffi::+sdl-init-joystick+)
(defconstant SYS-HAPTIC         sdl2-ffi::+sdl-init-haptic+)
(defconstant SYS-GAMECONTROLLER sdl2-ffi::+sdl-init-gamecontroller+)
(defconstant NOPARACHUTE        sdl2-ffi::+sdl-init-noparachute+)
(defconstant EVERYTHING         #x0000FFFF)

(defun init (&rest flags)
  "Initialize SDL2 with the specified subsystems. Initializes everything by default."
  (let ((init-flags (if flags
                        (apply #'logior flags)
                        everything)))
    (let ((rc (sdl2-ffi::sdl-init init-flags)))
      (when (< rc 0)
        (error (sdl2-ffi::sdl-geterror))))))

(defun quit ()
  "Shuts down SDL2."
  (sdl2-ffi::sdl-quit))
