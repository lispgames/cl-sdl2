(in-package #:sdl2)

(defun hash-slot-names (slot-names)
  (let ((hash-table (make-hash-table :test #'eq)))
    (loop for slot-name in slot-names
       do (setf (gethash (alexandria:make-keyword slot-name) hash-table)
                slot-name))
    hash-table))

(defun add-event-struct-slots (event-descriptions)
  (let ((hash-table (make-hash-table)))
    (loop for (event-type description) in event-descriptions
       do (let* ((slot-name (first description))
                 (struct-type (second description))
                 (slots (cffi:foreign-slot-names struct-type)))
              (setf (gethash event-type hash-table)
                    (list slot-name
                          (list :type struct-type
                                :slots (hash-slot-names slots))))))
    hash-table))

(defparameter *event-struct-data*
  (add-event-struct-slots
   (list (list :controlleraxismotion
               (list 'sdl2-ffi::caxis
                     'sdl2-ffi::sdl-controlleraxisevent))
         (list :controllerbuttonup
               (list 'sdl2-ffi::cbutton
                     'sdl2-ffi::sdl-controllerbuttonevent))
         (list :controllerbuttondown
               (list 'sdl2-ffi::cbutton
                     'sdl2-ffi::sdl-controllerbuttonevent))
         (list :controllerdeviceremoved
               (list 'sdl2-ffi::cdevice
                     'sdl2-ffi::sdl-controllerdeviceevent))
         (list :controllerdeviceremapped
               (list 'sdl2-ffi::cdevice
                     'sdl2-ffi::sdl-controllerdeviceevent))
         (list :controllerdeviceadded
               (list 'sdl2-ffi::cdevice
                     'sdl2-ffi::sdl-controllerdeviceevent))
         (list :joyaxismotion
               (list 'sdl2-ffi::jaxis
                     'sdl2-ffi::sdl-joyaxisevent))
         (list :joyballmotion
               (list 'sdl2-ffi::jball
                     'sdl2-ffi::sdl-joyballevent))
         (list :joyhatmotion
               (list 'sdl2-ffi::jhat
                     'sdl2-ffi::sdl-joyhatevent))
         (list :joybuttonup
               (list 'sdl2-ffi::jbutton
                     'sdl2-ffi::sdl-joybuttonevent))
         (list :joybuttondown
               (list 'sdl2-ffi::jbutton
                     'sdl2-ffi::sdl-joybuttonevent))
         (list :joydeviceadded
               (list 'sdl2-ffi::jdevice
                     'sdl2-ffi::sdl-joydeviceevent))
         (list :joydeviceremoved
               (list 'sdl2-ffi::jdevice
                     'sdl2-ffi::sdl-joydeviceevent))
         (list :keyup
               (list 'sdl2-ffi::key
                     'sdl2-ffi::sdl-keyboardevent))
         (list :keydown
               (list 'sdl2-ffi::key
                     'sdl2-ffi::sdl-keyboardevent))
         (list :mousewheel
               (list 'sdl2-ffi::wheel
                     'sdl2-ffi::sdl-mousewheelevent))
         (list :mousebuttonup
               (list 'sdl2-ffi::button
                     'sdl2-ffi::sdl-mousebuttonevent))
         (list :mousebuttondown
               (list 'sdl2-ffi::button
                     'sdl2-ffi::sdl-mousebuttonevent))
         (list :mousemotion
               (list 'sdl2-ffi::motion
                     'sdl2-ffi::sdl-mousemotionevent))
         (list :windowevent
               (list 'sdl2-ffi::window
                     'sdl2-ffi::sdl-windowevent))
         (list :dropfile
               (list 'sdl2-ffi::drop
                     'sdl2-ffi::sdl-dropevent))
         (list :multigesture
               (list 'sdl2-ffi::mgesture
                     'sdl2-ffi::sdl-multigestureevent))
         (list :fingerup
               (list 'sdl2-ffi::tfinger
                     'sdl2-ffi::sdl-touchfingerevent))
         (list :fingerdown
               (list 'sdl2-ffi::tfinger
                     'sdl2-ffi::sdl-touchfingerevent))
         (list :textediting
               (list 'sdl2-ffi::text
                     'sdl2-ffi::sdl-textinputevent))
         (list :textinput
               (list 'sdl2-ffi::text
                     'sdl2-ffi::sdl-textinputevent))
         (list :quit
               (list 'sdl2-ffi::quit
                     'sdl2-ffi::sdl-quitevent)))))

(defun new-event (&optional (event-type :firstevent))
  (let ((event (alloc 'sdl2-ffi:sdl-event)))
    (sdl-collect event)
    (setf (sdl2-ffi:sdl-event.type event)
          (enum-value 'sdl2-ffi:sdl-event-type event-type))
    event))

(defun free-event (event)
  (sdl-cancel-collect event)
  (foreign-free (ptr event))
  (invalidate event))

(defmacro with-sdl-event ((event-ptr &optional (event-type :firstevent))
                          &body body)
  `(let* ((,event-ptr (new-event ,event-type))
          (result (progn ,@body)))
     (free-event ,event-ptr)
     result))

(defun get-event-type (event)
  (enum-key 'sdl2-ffi:sdl-event-type (sdl2-ffi:sdl-event.type event)))

(defun pump-events ()
  (sdl2-ffi:sdl-pump-events))

(defun push-event (event-type)
  (with-alloc (event 'sdl2-ffi:sdl-event)
    (setf (sdl2-ffi:sdl-event.type event)
          (enum-value 'sdl2-ffi:sdl-event-type event-type))
    (check-rc (sdl2-ffi::sdl-push-event event))))

(defun push-quit-event ()
  (push-event :quit))

(defun next-event (event &optional (method :poll) timeout)
  "Method can be either :poll, :wait.  If :wait is used,
`TIMEOUT` may be specified."
  (case method
    (:poll (sdl2-ffi:sdl-poll-event event))
    (:wait
     (if timeout
         (sdl2-ffi:sdl-wait-event-timeout event timeout)
         (sdl2-ffi:sdl-wait-event event)))
    (:wait-with-timeout
     (sdl2-ffi:sdl-wait-event-timeout event (or timeout 0)))
    (otherwise (error "Event method must be :poll or :wait"))))

(defun expand-idle-handler (event-handlers)
  (remove nil (mapcar #'(lambda (handler)
                          (cond ((eq (first handler) :idle)
                                 `(progn ,@(rest (rest handler))))))
                      event-handlers)))

(defun expand-quit-handler (sdl-event forms quit)
  (declare (ignore sdl-event))
  `(:quit (setf ,quit (funcall #'(lambda () ,@forms)))))

(defun unpack-event-params (event-ptr data params)
  (let ((event-slot (first data))
        (ffi-type (getf (second data) :type))
        (slots (getf (second data) :slots)))
    (mapcar (lambda (param)
              (let ((keyword (first param))
                    (binding (second param)))
                (when (gethash keyword slots)
                  `(,binding
                    (cffi:foreign-slot-value
                     (cffi:foreign-slot-value ,event-ptr 'sdl2-ffi::sdl-event (quote ,event-slot))
                     (quote ,ffi-type)
                     (quote ,(gethash keyword slots)))))))
            params)))

(defun expand-handler (sdl-event event-type params forms event-data)
  (let ((parameter-pairs nil))
    (do ((keyword params (if (cdr keyword)
                             (cddr keyword)
                             nil)))
        ((null keyword))
      (push (list (first keyword) (second keyword)) parameter-pairs))
    `(,event-type
      (let (,@(unpack-event-params sdl-event event-data parameter-pairs))
        ,@forms))))

;; TODO you should be able to specify a target framerate
(defmacro with-event-loop ((&key (method :poll) (timeout nil)) &rest event-handlers)
  (let ((quit (gensym "quit-"))
        (sdl-event (gensym "sdl-event-"))
        (idle-func (gensym "idle-func-")))
    `(let ((,quit nil)
           (,idle-func nil))
       (with-sdl-event (,sdl-event)
         (setf ,idle-func #'(lambda () ,@(expand-idle-handler event-handlers)))
         (loop until ,quit
            do (loop until (= 0 ,(sdl2::next-event sdl-event method timeout))
                  do (case (get-event-type ,sdl-event)
                       ,@(remove nil
                                 (mapcar
                                  #'(lambda (handler)
                                      (if (eq (first handler) :quit)
                                          (expand-quit-handler sdl-event
                                                               (rest (rest handler))
                                                               quit)
                                          (let* ((event-type (first handler))
                                                 (params (second handler))
                                                 (forms (rest (rest handler)))
                                                 (event-data (gethash event-type *event-struct-data*)))
                                            (when event-data
                                              (expand-handler sdl-event
                                                              event-type
                                                              params
                                                              forms
                                                              event-data)))))
                                  event-handlers))))
              (unless ,quit
                (funcall ,idle-func)))))))

