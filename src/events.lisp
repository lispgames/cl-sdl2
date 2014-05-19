(in-package #:sdl2)

(defun new-event (&optional (event-type :firstevent))
  (let ((event (alloc 'sdl2-ffi:sdl-event)))
    (sdl-collect event)
    (setf (sdl-event.type event)
          (enum-value 'sdl2-ffi:sdl-event-type event-type))
    event))

(defun free-event (event)
  (sdl-cancel-collect event)
  (foreign-free (ptr event))
  (invalidate event))

(defmacro with-sdl-event ((event &optional (event-type :firstevent))
                          &body body)
  "Allocate and automatically free an sdl event struct."
  `(let ((,event (new-event ,event-type)))
     (unwind-protect (progn ,@body)
       (free-event ,event))))

(defun get-event-type (event)
  (c-let ((event sdl2-ffi:sdl-event :from event))
    (if (eq (event :type) *lisp-message-event*)
        :lisp-message
        (or (enum-key 'sdl2-ffi:sdl-event-type (event :type))
            (event :type)))))

(defun pump-events ()
  (sdl-pump-events))

(defun push-event (event)
  "Allocates a new sdl event struct of the specified type and pushes it into the queue."
  (etypecase event
    (symbol
     (with-sdl-event (ev event)
       (setf (sdl-event.type ev)
             (enum-value 'sdl2-ffi:sdl-event-type event))
       (check-rc (sdl-push-event ev))))
    (sdl2-ffi:sdl-event
     (check-rc (sdl-push-event event)))))

(defun push-quit-event ()
  (push-event :quit))

(defun next-event (event &optional (method :poll) timeout)
  "Method can be either :poll, :wait.  If :wait is used,
`TIMEOUT` may be specified."
  (case method
    (:poll (sdl-poll-event event))
    (:wait
     (if timeout
         (sdl-wait-event-timeout event timeout)
         (sdl-wait-event event)))
    (:wait-with-timeout
     (sdl-wait-event-timeout event (or timeout 0)))
    (otherwise (error "Event method must be :poll or :wait"))))

(defun expand-idle-handler (event-handlers)
  (remove nil (mapcar #'(lambda (handler)
                          (cond ((eq (first handler) :idle)
                                 `(progn ,@(rest (rest handler))))))
                      event-handlers)))

(defun expand-quit-handler (sdl-event forms quit)
  (declare (ignore sdl-event))
  `(:quit (setf ,quit (funcall #'(lambda () ,@forms)))))

(defparameter *event-type-to-accessor*
  '((:controlleraxismotion . :caxis)
    (:controllerbuttondown . :cbutton)
    (:controllerbuttonup . :cbutton)
    (:controllerdeviceadded . :cdevice)
    (:controllerdeviceremapped . :cdevice)
    (:controllerdeviceremoved . :cdevice)
    (:dollargesture . :dgesture)
    (:dropfile . :drop)
    (:fingermotion . :tfinger)
    (:fingerdown . :tfinger)
    (:fingerup . :tfinger)
    (:joyaxismotion . :jaxis)
    (:joyballmotion . :jball)
    (:joybuttondown . :jbutton)
    (:joybuttonup . :jbutton)
    (:joydeviceadded . :jdevice)
    (:joydeviceremoved . :jdevice)
    (:joyhatmotion . :jhat)
    (:keydown . :key)
    (:keyup . :key)
    (:mousebuttondown . :button)
    (:mousebuttonup . :button)
    (:mousemotion . :motion)
    (:mousewheel . :wheel)
    (:multigesture . :mgesture)
    (:syswmevent . :syswm)
    (:textediting . :edit)
    (:textinput . :text)
    (:userevent . :user)
    (:lisp-message . :user)
    (:windowevent . :window)))

(defun unpack-event-params (event-var event-type params)
  (mapcar (lambda (param)
            (let* ((keyword (first param))
                   (binding (second param))
                   (ref (or (cdr (assoc event-type *event-type-to-accessor*))
                            :user)))
              `(,binding (c-ref ,event-var sdl2-ffi:sdl-event ,ref ,keyword))))
          params))

(defun expand-handler (sdl-event event-type params forms)
  (let ((parameter-pairs nil))
    (do ((keyword params (if (cdr keyword)
                             (cddr keyword)
                             nil)))
        ((null keyword))
      (push (list (first keyword) (second keyword)) parameter-pairs))
    `(,event-type
      (let (,@(unpack-event-params sdl-event event-type parameter-pairs))
        ,@forms))))

(defvar *event-loop* nil)

;; TODO you should be able to specify a target framerate
(defmacro with-event-loop ((&key background (method :poll) (timeout nil) recursive)
                           &rest event-handlers)
  (let ((quit (gensym "QUIT-"))
        (sdl-event (gensym "SDL-EVENT-"))
        (idle-func (gensym "IDLE-FUNC-"))
        (rc (gensym "RC-")))
    `(when (or ,recursive (not *event-loop*))
       (setf *event-loop* t)
       (in-main-thread (:background ,background)
         (let ((,quit nil)
               (,idle-func nil))
           (unwind-protect
                (with-sdl-event (,sdl-event)
                  (setf ,idle-func #'(lambda () ,@(expand-idle-handler event-handlers)))
                  (progn ,@(cddr (find :initialize event-handlers :key #'first)))
                  (loop :until ,quit
                     :do (loop :as ,rc = (next-event ,sdl-event ,method ,timeout)
                            ,@(if (eq :poll method)
                                  `(:until (= 0 ,rc))
                                  `(:until ,quit))
                            :do (case (get-event-type ,sdl-event)
                                  (:lisp-message () (get-and-handle-messages))
                                  ,@(loop :for (type params . forms) :in event-handlers
                                       :collect
                                       (if (eq type :quit)
                                           (expand-quit-handler sdl-event forms quit)
                                           (expand-handler sdl-event type params forms))
                                       :into results
                                       :finally (return (remove nil results)))))
                     (unless ,quit
                       (funcall ,idle-func))))
             (setf *event-loop* nil)))))))
