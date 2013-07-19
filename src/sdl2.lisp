;;;; sdl2.lisp

(in-package #:sdl2)

;;; "sdl2" goes here. Hacks and glory await!

(define-condition sdl-error (error) ())

(define-condition sdl-rc-error (sdl-error)
  ((code :initarg :rc :initform nil :accessor sdl-error-code)
   (string :initarg :string :initform nil :accessor sdl-error-string))
  (:report (lambda (c s)
             (with-slots (code string) c
              (format s "SDL Error (~A): ~A" code string)))))

(defun sdl-collect (wrapped-ptr &optional (free-fun #'foreign-free))
  (let ((ptr (autowrap:ptr wrapped-ptr)))
    (tg:finalize wrapped-ptr (lambda () (funcall free-fun ptr)))
    wrapped-ptr))

(defun sdl-cancel-collect (wrapped-ptr)
  (tg:cancel-finalization wrapped-ptr)
  wrapped-ptr)

(autowrap:define-bitmask-from-constants (sdl-init-flags)
  sdl2-ffi:+sdl-init-timer+
  sdl2-ffi:+sdl-init-audio+
  sdl2-ffi:+sdl-init-video+
  sdl2-ffi:+sdl-init-joystick+
  sdl2-ffi:+sdl-init-haptic+
  sdl2-ffi:+sdl-init-gamecontroller+
  sdl2-ffi:+sdl-init-noparachute+
  '(:everything . #x0000FFFF))

(defmacro check-rc (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (< ,rc 0)
         (error 'sdl-rc-error :rc ,rc :string (sdl2-ffi:sdl-get-error))))))

(defmacro check-null (form)
  (with-gensyms (ptr)
    `(let ((,ptr ,form))
       (if (null-pointer-p ,ptr)
           (error 'sdl-rc-error :rc ,ptr :string (sdl2-ffi:sdl-get-error))
           ,ptr))))

(defun init (&rest sdl-init-flags)
  "Initialize SDL2 with the specified subsystems. Initializes everything by default."
  (let ((init-flags (apply #'autowrap:mask 'sdl-init-flags sdl-init-flags)))
    (check-rc (sdl2-ffi:sdl-init init-flags))))

(defun quit ()
  "Shuts down SDL2."
  (sdl2-ffi:sdl-quit))

(defmacro with-init ((&rest sdl-init-flags) &body body)
  `(progn
     (init ,@sdl-init-flags)
     (unwind-protect
          (progn ,@body)
       (quit))))
