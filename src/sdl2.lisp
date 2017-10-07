;;;; sdl2.lisp

(in-package #:sdl2)

;;; "sdl2" goes here. Hacks and glory await!

(defvar *main-thread* nil)
(defvar *wakeup-event* nil)
(defvar *the-main-thread* nil)
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

;;
;; Message handling

(defun recv-and-handle-message ()
  (let ((msg (recvmsg *main-thread-channel*)))
    (handle-message msg)))

(defun get-and-handle-messages ()
  (loop as msg = (and *main-thread-channel*
                      (getmsg *main-thread-channel*))
        while msg do
          (handle-message msg)))

(defmacro without-fp-traps (&body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:underflow
                                    :overflow
                                    :inexact
                                    :invalid
                                    :divide-by-zero) ,@body)
  #-sbcl
  `(progn ,@body))

(defun handle-message (msg)
  (let ((fun (car msg))
        (chan (cdr msg))
        (condition))
    (handler-bind ((sdl-continue
                     (lambda (c)
                       (declare (ignore c))
                       (when chan (sendmsg chan nil))
                       (return-from handle-message)))
                   (sdl-quit
                     (lambda (c)
                       (declare (ignore c))
                       (quit)
                       (return-from handle-message))))
      (handler-bind ((error (lambda (e) (setf condition e))))
        (if chan
            (sendmsg chan (multiple-value-list (funcall fun)))
            (funcall fun))))))

;;
;; Main thread helpers

(defun find-main-thread ()
  #+ccl ccl::*initial-process*
  #+sbcl (sb-thread:main-thread)
  #-(or sbcl ccl) (bt:current-thread))

(defun ensure-main-channel ()
  (unless *main-thread-channel*
    (setf *main-thread-channel* (make-channel))))

(defun sdl-main-thread ()
  (without-fp-traps
    (let ((*main-thread* (bt:current-thread))
          #+sdl2::sdl2-swank (swank:*sldb-quit-restart* 'continue)
          #+sdl2::sdl2-slynk (slynk:*sly-db-quit-restart* 'continue))
      (loop while *main-thread-channel* do
        (block loop-block
          (restart-bind ((continue (lambda (&optional v)
                                     (declare (ignore v))
                                     (signal 'sdl-continue))
                                   :report-function
                                   (lambda (stream)
                                     (format stream "Return to the SDL2 main loop.")))
                         (abort (lambda (&optional v)
                                  (declare (ignore v))
                                  (signal 'sdl-quit))
                                :report-function
                                (lambda (stream)
                                  (format stream "Abort, quitting SDL2 entirely."))))
            (recv-and-handle-message)))))))

(defun setup-main-thread ()

  ;; Prevent re-calls
  (when *the-main-thread*
    (return-from setup-main-thread))

  (unless *wakeup-event*
    (setf *wakeup-event* (alloc 'sdl2-ffi:sdl-event)))
  (unless *main-thread-channel*
    (ensure-main-channel)

    ;; If we did not have a main-thread, make one.
    ;; Events and funcalls will be piped into this thread
    #-darwin
    (setf *the-main-thread* (bt:make-thread #'sdl-main-thread :name "SDL2 Main Thread"))

    ;; On OSX, we need to run in the main thread
    #+darwin
    (setf *the-main-thread* (find-main-thread))))

    ;; We'll interrupt this thread now in CCL and replace it with the sdl-main-loop
    ;#+nil;(and darwin ccl)
    ;(bt:interrupt-thread *the-main-thread* #'sdl-main-thread)))

(defmacro in-main-thread ((&key background no-event) &body b)
  (with-gensyms (fun channel)
    `(let ((,fun (lambda () ,@b)))
       (if (or *main-thread-channel* *main-thread*)
           (if *main-thread*
               (funcall ,fun)
               ,(if background
                    `(progn
                       (sendmsg *main-thread-channel*
                                (cons ,fun nil))
                       (values))
                    `(let ((,channel (make-channel)))
                       (sendmsg *main-thread-channel*
                                (cons ,fun ,channel))
                       ,(unless no-event
                          '(push-event *wakeup-event*))
                       (let ((result (recvmsg ,channel)))
                         (etypecase result
                           (list (values-list result))
                           (error (error result)))))))
           (error "No main thread, did you call SDL_Init?")))))

(defun make-this-thread-main (&optional function)
  "Designate the current thread as the SDL2 main thread.  This
function will not return until `SDL2:QUIT` is handled.  Users of this
function will need to start other threads before this call, or specify
`FUNCTION`.

If `FUNCTION` is specified, it will be called when the main thread
channel is ensured.  This is like calling `IN-MAIN-THREAD`, except it
allows for a potentially single-threaded application.  This function
does **not** return just because `FUNCTION` returns; it still requires
`SDL2:QUIT` be processed.

This does **not** call `SDL2:INIT` by itself.  Do this either with
`FUNCTION`, or from a separate thread."
  (setup-main-thread)
  (ensure-main-channel)
  (when (functionp function)
    (sendmsg *main-thread-channel* (cons function nil)))
  (sdl-main-thread))

(defun init (&rest sdl-init-flags)
  "Initialize SDL2 with the specified subsystems. Initializes everything by default."

  (in-main-thread (:no-event t)
    (let ((init-flags (autowrap:mask-apply 'sdl-init-flags sdl-init-flags)))
      (check-rc (sdl-init init-flags))
      (unless *lisp-message-event*
        (setf *lisp-message-event* (sdl-register-events 1))
        (setf (c-ref *wakeup-event* sdl2-ffi:sdl-event :type) *lisp-message-event*)))))

(defun quit ()
  "Shuts down SDL2."
  (in-main-thread (:background t)
    (let ((mtc *main-thread-channel*))
      (sdl-quit)
      (setf *main-thread-channel* nil)
      (setf *lisp-message-event* nil)
      (when mtc (sendmsg mtc nil))))
  (when (and *the-main-thread*
             (not (eq *the-main-thread* (bt:current-thread))))
    (bt:join-thread *the-main-thread*)
    (setf *the-main-thread* nil))
  (when *the-main-thread*
    (setf *the-main-thread* nil)))

(defmacro with-init ((&rest sdl-init-flags) &body body)
  `(progn
     (unwind-protect
          (make-this-thread-main
           (lambda ()
             (progn
               (init ,@sdl-init-flags)
               ,@body
               (quit)))))))
