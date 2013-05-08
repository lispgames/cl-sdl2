;;;; sdl2.lisp

(in-package #:sdl2)

;;; "sdl2" goes here. Hacks and glory await!

(define-condition sdl-error (error)
  ((code :initarg :rc :initform nil :accessor sdl-error-code)
   (string :initarg :string :initform nil :accessor sdl-error-string))
  (:report (lambda (c s)
             (with-slots (code string) c
              (format s "SDL Error (~A): ~A" code string)))))

(defbitfield* sdl-init-flags
  (:timer          sdl2-ffi:+sdl-init-timer+)
  (:audio          sdl2-ffi:+sdl-init-audio+)
  (:video          sdl2-ffi:+sdl-init-video+)
  (:joystick       sdl2-ffi:+sdl-init-joystick+)
  (:haptic         sdl2-ffi:+sdl-init-haptic+)
  (:gamecontroller sdl2-ffi:+sdl-init-gamecontroller+)
  (:noparachute    sdl2-ffi:+sdl-init-noparachute+)
  (:everything     #x0000FFFF))

(defmacro check-rc (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (< ,rc 0)
         (error 'sdl-error :rc ,rc :string (sdl2-ffi:sdl-geterror))))))

(defmacro check-null (form)
  (with-gensyms (ptr)
    `(let ((,ptr ,form))
       (if (null-pointer-p ,ptr)
           (error 'sdl-error :rc ,ptr :string (sdl2-ffi:sdl-geterror))
           ,ptr))))

(defun init (&rest sdl-init-flags)
  "Initialize SDL2 with the specified subsystems. Initializes everything by default."
  (let ((init-flags (foreign-bitfield-value 'sdl-init-flags sdl-init-flags)))
    (check-rc (sdl2-ffi:sdl-init init-flags))))

(defun quit ()
  "Shuts down SDL2."
  (sdl2-ffi::sdl-quit))
