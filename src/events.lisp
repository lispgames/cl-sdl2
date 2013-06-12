(in-package #:sdl2)

(defsanecenum-from-cenum (event-type sdl2-ffi:sdl-eventtype "SDL-"))

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
  (let ((enum-value (foreign-enum-value 'event-type event-type))
        (event-ptr (foreign-alloc 'sdl2-ffi:sdl-event)))
    (setf (foreign-slot-value event-ptr
                              'sdl2-ffi:sdl-event
                              'sdl2-ffi::type) enum-value)
    event-ptr))

(defun free-event (event-ptr)
  (foreign-free event-ptr))

(defmacro with-sdl-event ((event-ptr &optional (event-type :firstevent))
                          &body body)
  `(let* ((,event-ptr (new-event ,event-type))
          (result (progn ,@body)))
     (free-event ,event-ptr)
     result))

(defun get-event-type (event-ptr)
  (let ((enum-value (foreign-slot-value event-ptr 'sdl2-ffi:sdl-event 'type)))
    (foreign-enum-keyword 'event-type enum-value)))

(defun get-event-struct-data (event-ptr event-type)
  (let* ((event-data (gethash event-type *event-struct-data*)))
    (if (equal nil event-data)
        (nil nil)
        event-data)))

(defun pump-events ()
  (sdl2-ffi:sdl-pumpevents))

(defun push-event (event-type)
  (check-rc (with-sdl-event (event-ptr event-type)
              (sdl2-ffi::sdl-pushevent event-ptr))))

(defmacro push-quit-event ()
  `(push-event :quit))

(defun next-event (event-ptr &optional (method :poll) (timeout 1))
  "Method can be either :poll, :wait, or :wait-with-timeout"
  (case method
    (:poll `(sdl2-ffi:sdl-pollevent ,event-ptr))
    (:wait `(sdl2-ffi:sdl-waitevent ,event-ptr))
    (:wait-with-timeout `(sdl2-ffi:sdl-waiteventtimeout ,event-ptr ,timeout))
    (otherwise (error "Event method must be :poll :wait or :wait-with-timeout"))))

(defun expand-idle-handler (event-handlers)
  `(lambda () ,@(remove nil (mapcar #'(lambda (handler)
                                        (cond ((eql :idle (first handler))
                                               `(progn ,@(rest handler)))))
                                    event-handlers))))

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
                     (cffi:foreign-slot-value ,event-ptr 'sdl2-ffi::sdl-event ,event-slot)
                     ,ffi-type (gethash keyword slots))))))
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
         (setf ,idle-func ,(sdl2::expand-idle-handler event-handlers))
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
                                  event-handlers)))))
         (unless ,quit
           (when ,idle-func
             (funcall ,idle-func)))))))

;;; The following three functions shouldn't be used in a real
;;; game loop. They are simply here for easier testing and
;;; experimentation.

(defun poll-event ()
  (with-sdl-event (event-ptr)
    (next-event event-ptr)))

(defun wait-event ()
  (with-sdl-event (event-ptr)
    (next-event event-ptr :wait)))

(defun wait-event-timeout (timeout)
  (with-sdl-event (event-ptr)
    (next-event event-ptr :wait-with-timeout timeout)))

