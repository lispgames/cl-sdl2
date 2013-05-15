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

(define-condition sdl-invalid-object-error (sdl-error)
  ((object :initarg :object :initform nil :accessor sdl-invalid-object))
  (:report (lambda (c s)
             (with-slots (object) c
               (format s "SDL Error: Invalid/Destroyed Object: ~A" object)))))

(defstruct sdl-wrapped-ptr
  (ptr (null-pointer) :type #.(type-of (null-pointer)))
  (valid-p t :type boolean))

(declaim (inline sdl-ptr sdl-invalidate))
(defun sdl-ptr (wrapped-ptr)
  (if (sdl-wrapped-ptr-valid-p wrapped-ptr)
      (sdl-wrapped-ptr-ptr wrapped-ptr)
      (error 'sdl-invalid-object-error :object wrapped-ptr)))

(defun sdl-invalidate (wrapped-ptr)
  (setf (sdl-wrapped-ptr-valid-p wrapped-ptr) nil)
  (sdl-wrapped-ptr-ptr wrapped-ptr))

(defmethod print-object ((object sdl-wrapped-ptr) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "{#X~8,'0X}" (pointer-address (sdl-wrapped-ptr-ptr object)))))

(defun sdl-collect (wrapped-ptr &optional (free-fun #'foreign-free))
  (let ((ptr (sdl-ptr wrapped-ptr)))
    (trivial-garbage:finalize wrapped-ptr
                              (lambda () (funcall free-fun ptr)))
    wrapped-ptr))

(defun sdl-cancel-collect (wrapped-ptr)
  (trivial-garbage:cancel-finalization wrapped-ptr)
  wrapped-ptr)

(defmacro defwrapper (wrapped-struct (&optional ffi-type)
                      &body more-slots)
  `(progn
     (defstruct (,wrapped-struct (:include sdl-wrapped-ptr)
                                 (:constructor ,(symbolicate "%MAKE-" wrapped-struct)))
       ,@more-slots)
     ,@(when ffi-type
         `((make-struct-accessors ,ffi-type ,wrapped-struct)))))

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
         (error 'sdl-rc-error :rc ,rc :string (sdl2-ffi:sdl-geterror))))))

(defmacro check-null (form)
  (with-gensyms (ptr)
    `(let ((,ptr ,form))
       (if (null-pointer-p ,ptr)
           (error 'sdl-rc-error :rc ,ptr :string (sdl2-ffi:sdl-geterror))
           ,ptr))))

(defun init (&rest sdl-init-flags)
  "Initialize SDL2 with the specified subsystems. Initializes everything by default."
  (let ((init-flags (foreign-bitfield-value 'sdl-init-flags sdl-init-flags)))
    (check-rc (sdl2-ffi:sdl-init init-flags))))

(defun quit ()
  "Shuts down SDL2."
  (sdl2-ffi::sdl-quit))

(defmacro with-init ((&rest sdl-init-flags) &body body)
  `(progn
     (init ,@sdl-init-flags)
     (unwind-protect
          (progn ,@body)
       (quit))))
