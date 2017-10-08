;;;; sdl2.lisp
(in-package #:sdl2)

;;; "sdl2" goes here. Hacks and glory await!

(defvar *has-init* nil)
(defvar *wakeup-event* nil)
(defvar *lisp-message-event* nil)
(defvar *main-thread-channel* nil)

(autowrap:define-bitmask-from-constants (sdl-init-flags)
  sdl2-ffi:+sdl-init-timer+
  sdl2-ffi:+sdl-init-audio+
  sdl2-ffi:+sdl-init-video+
  sdl2-ffi:+sdl-init-joystick+
  sdl2-ffi:+sdl-init-haptic+
  sdl2-ffi:+sdl-init-gamecontroller+
  sdl2-ffi:+sdl-init-noparachute+
  '(:everything . #x0000FFFF))

(define-condition sdl-error (error)
  ((string :initarg :string :initform nil :accessor sdl-error-string))
  (:report (lambda (c s)
             (with-slots (string) c
               (format s "SDL Error: ~A" string)))))

(define-condition sdl-rc-error (sdl-error)
  ((code :initarg :rc :initform nil :accessor sdl-error-code))
  (:report (lambda (c s)
             (with-slots (code string) c
               (format s "SDL Error (~A): ~A" code string)))))

(defun get-and-handle-messages (&optional message)
  (format t "Lisp error: ~A~%" message))

(define-condition sdl-continue (condition) ())
(define-condition sdl-quit (condition) ())

(defun sdl-collect (wrapped-ptr &optional (free-fun #'foreign-free))
  (let ((ptr (autowrap:ptr wrapped-ptr)))
    (tg:finalize wrapped-ptr (lambda () (funcall free-fun ptr)))
    wrapped-ptr))

(defun sdl-cancel-collect (wrapped-ptr)
  (tg:cancel-finalization wrapped-ptr)
  wrapped-ptr)

(defun sdl-true-p (integer-bool)
  "Use this function to convert truth from a low level wrapped SDL function
returning an SDL_true into CL's boolean type system."
  (= (autowrap:enum-value 'sdl2-ffi:sdl-bool :true) integer-bool))

;;;
;;; NAMING CONVENTION: check-<foo>
;;;
;;; If <foo> names a specific value (true, false, zero, null, etc),
;;; check-<foo> shall error `(when <foo> ...)`.  E.g., `(check-false
;;; x)` will *error* when `x` is false.
;;;
;;; If <foo> names something that can have an error state (like a
;;; return code), `(check-<foo> x)` shall error when `x` is in that
;;; state.
;;;

(defmacro check-rc (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (< ,rc 0)
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-zero (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (= ,rc 0)
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-false (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (not (sdl-true-p ,rc))
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-nullptr (form)
  (with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (if (null-pointer-p (autowrap:ptr ,wrapper))
           (error 'sdl-error :string (sdl-get-error))
           ,wrapper))))

(defmacro check-nil (form)
  (with-gensyms (v)
    `(let ((,v ,form))
       (if (null ,v)
           (error 'sdl-error :string (sdl-get-error))
           ,v))))

(defun was-init (&rest flags)
  (/= 0 (sdl-was-init (autowrap:mask-apply 'sdl-init-flags flags))))

(defun niy (message)
  (error "SDL2 Error: Construct Not Implemented Yet: ~A" message))

(defun version ()
  (c-let ((ver sdl2-ffi:sdl-version :free t))
    (sdl-get-version (ver &))
    (values (ver :major) (ver :minor) (ver :patch))))

(defun version-wrapped ()
  (values sdl2-ffi:+sdl-major-version+
          sdl2-ffi:+sdl-minor-version+
          sdl2-ffi:+sdl-patchlevel+))

(defmacro without-fp-traps (&body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:underflow
                                    :overflow
                                    :inexact
                                    :invalid
                                    :divide-by-zero) ,@body)
  #-sbcl
  `(progn ,@body))

(defmacro in-main-thread (function &key blocking)
  `(without-fp-traps

     (let (#+sdl2::sdl2-swank (swank:*sldb-quit-restart* 'continue)
           #+sdl2::sdl2-slynk (slynk:*sly-db-quit-restart* 'continue))

       (restart-bind

           ((continue
              (lambda (&optional v)
                (declare (ignore v))
                (signal 'sdl-continue))

              :report-function
              (lambda (stream)
                (format stream "Return to the SDL2 main loop.")))

            (abort
              (lambda (&optional v)
                (declare (ignore v))
                (signal 'sdl-quit))

              :report-function
              (lambda (stream)
                (format stream "Abort, quitting SDL2 entirely."))))

         (tmt:call-in-main-thread ,function :blocking ,blocking)))))

(defmacro with-body-in-main-thread ((&key blocking) &body body)
  `(in-main-thread (lambda () ,@body) :blocking ,blocking))

(defun make-this-thread-main (function)
  "Depreciated, see (in-main-thread) or (with-body-in-main-thread)"
  (format t "#'make-this-thread-main is depreciated. Use (in-main-thread) or (with-body-in-main-thread)")
  (in-main-thread function))

(defun init (&rest sdl-init-flags)
  "Initialize SDL2 with the specified subsystems. Initializes everything by default."

  (unless *has-init* (return-from init))
  (setf *has-init* true)

  (with-body-in-main-thread ()
    (let ((init-flags (autowrap:mask-apply 'sdl-init-flags sdl-init-flags)))
      (check-rc (sdl-init init-flags))
      (unless *lisp-message-event*
        (setf *lisp-message-event* (sdl-register-events 1))))))

(defun quit ()
  "Shuts down SDL2."
  (when *has-init*
    (with-body-in-main-thread (:blocking t)
      (sdl-quit)
      (setf *has-init* nil))))

(defmacro with-init ((&rest sdl-init-flags) &body body)
  `(with-body-in-main-thread ()
     (init ,@sdl-init-flags)
     ,@body
     (quit)))
