(in-package #:sdl2)

(defvar *user-event-types* (make-hash-table))
(defvar *user-event-codes* (make-hash-table))
(defvar *user-events* (make-hash-table))
(defvar *user-event-id*
  (c-let ((new-atomic sdl2-ffi:sdl-atomic-t))
    (setf (new-atomic :value) 0)
    new-atomic))
(defvar *event-loop* nil)

(defun new-event (&optional (event-type :firstevent))
  (c-let ((event sdl2-ffi:sdl-event))
    (setf (event :type) (get-event-code event-type))
    event))

(defun free-event (event)
  (foreign-free (ptr event))
  (invalidate event))

(defun register-user-event-type (user-event-type)
  "Register a new sdl event-type if it doesn't already exist"
  (when (not (keywordp user-event-type))
    (error "Event types must be given as keywords"))
  (multiple-value-bind (event-type-code event-type-found)
      (gethash user-event-type *user-event-codes*)
    (if event-type-found
        event-type-code
        (let ((new-event-code (sdl2-ffi.functions::sdl-register-events 1)))
          (if (not (= new-event-code 4294967295))
              (progn
                (setf (gethash user-event-type *user-event-codes*) new-event-code)
                (setf (gethash new-event-code *user-event-types*) user-event-type)
                new-event-code)
              (error (format nil "Failed to register new user-event type: ~a" user-event-type)))))))

(defmacro with-sdl-event ((event &optional (event-type :firstevent)) &body body)
  "Allocate and automatically free an sdl event struct."
  `(c-let ((,event sdl2-ffi:sdl-event :from (new-event ,event-type)))
     (unwind-protect (progn ,@body)
       (free-event ,event))))

(defun add-user-data (user-data)
  (let* ((event-id (sdl-atomic-add *user-event-id* 1))
         (id-in-use (nth-value 1 (gethash event-id *user-events*))))
    (when id-in-use
      (error "Event ID already in use"))
    (setf (gethash event-id *user-events*) user-data)
    event-id))

(defun free-user-data (event-id)
  (remhash event-id *user-events*))

(defun get-user-data (event-id)
  "Returns the user-data attached to an event-id and if the event-id was found"
  (gethash event-id *user-events*))

(defun get-event-code (event-type)
  (multiple-value-bind (user-event-code is-user-event) (gethash event-type *user-event-codes*)
    (cond
      (is-user-event
       user-event-code)
      ((eq :lisp-message event-type)
       *lisp-message-event*)
      (t
       (enum-value 'sdl2-ffi:sdl-event-type event-type)))))

(defun get-event-type (event)
  (c-let ((event sdl2-ffi:sdl-event :from event))
    (multiple-value-bind (user-event-type is-user-event) (gethash (event :type) *user-event-types*)
      (cond
        (is-user-event
         user-event-type)
        ((eq (event :type) *lisp-message-event*)
         :lisp-message)
        (t
         (or (enum-key 'sdl2-ffi:sdl-event-type (event :type))
             (event :type)))))))

(defun user-event-type-p (event-type)
  (nth-value 1 (gethash event-type *user-event-codes*)))

(defun pump-events ()
  (sdl-pump-events))

(defun push-event (event)
  "Allocates a new sdl event struct of the specified type and pushes it into the queue."
  (etypecase event
    (symbol
     (with-sdl-event (ev event)
       (setf (ev :type) (get-event-code event))
       (check-rc (sdl-push-event ev))))
    (sdl2-ffi:sdl-event
     (check-rc (sdl-push-event event)))
    (sdl2-ffi:sdl-user-event
     (check-rc (sdl-push-event event)))))

(defun push-user-event (user-event &optional user-data)
  "Allocates a new user-defined sdl event struct of the specified type and pushes it into the queue.
Stores the optional user-data in sdl2::*user-events*"
  (if (user-event-type-p user-event)
      (with-sdl-event (event user-event)
        (let ((event-id (add-user-data user-data)))
          (setf (event :user :code) event-id)
          (push-event event)))
      (error "Not a known user-event type")))

(defun push-quit-event ()
  (push-event :quit))

(defun next-event (event &optional (method :poll) timeout)
  "Method can be either :poll, :wait. If :wait is used, `TIMEOUT` may be specified."
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
            (let ((keyword (first param))
                  (binding (second param))
                  (ref (or (cdr (assoc event-type *event-type-to-accessor*))
                           :user)))
              (if (eql keyword :user-data)
                  `(,binding (get-user-data (c-ref ,event-var sdl2-ffi:sdl-event ,ref :code)))
                  (if (and (or (eql ref :text) (eql ref :edit)) (eql keyword :text))
                      `(,binding (c-ref ,event-var sdl2-ffi:sdl-event ,ref ,keyword string))
                      `(,binding (c-ref ,event-var sdl2-ffi:sdl-event ,ref ,keyword))))))
          params))

(defun expand-handler (sdl-event event-type params forms)
  (let ((parameter-pairs nil))
    (do ((keyword params (if (cdr keyword)
                             (cddr keyword)
                             nil)))
        ((null keyword))
      (push (list (first keyword) (second keyword)) parameter-pairs))
    `(,event-type
      (let (,@(unpack-event-params sdl-event
                                   event-type
                                   (nreverse parameter-pairs)))
        ,@forms))))

;; TODO you should be able to specify a target framerate
(defmacro with-event-loop ((&key background (method :poll) (timeout nil) recursive)
                           &body event-handlers)
  (let ((quit (gensym "QUIT-"))
        (sdl-event (gensym "SDL-EVENT-"))
        (sdl-event-type (gensym "SDL-EVENT-TYPE"))
        (sdl-event-id (gensym "SDL-EVENT-ID"))
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
                                  :do (let* ((,sdl-event-type (get-event-type ,sdl-event))
                                             (,sdl-event-id (and (user-event-type-p ,sdl-event-type)
                                                                 (,sdl-event :user :code))))
                                        (case ,sdl-event-type
                                          (:lisp-message () (get-and-handle-messages))
                                          ,@(loop :for (type params . forms) :in event-handlers
                                                  :collect
                                                  (if (eq type :quit)
                                                      (expand-quit-handler sdl-event forms quit)
                                                      (expand-handler sdl-event type params forms))
                                                    :into results
                                                  :finally (return (remove nil results))))
                                        (when (and ,sdl-event-id
                                                   (not (eq ,sdl-event-type :lisp-message)))
                                          (free-user-data ,sdl-event-id))))
                            (unless ,quit
                              (funcall ,idle-func))))
             (setf *event-loop* nil)))))))
