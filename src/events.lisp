(in-package #:sdl2)

(defsanecenum-from-cenum (event-type
                          sdl2-ffi:sdl-eventtype
                          "SDL-"))

(defun add-event-struct-slots (event-slots)
  (let ((hash-table (make-hash-table)))
    (loop for (event-type slot-name) in event-slots
       do (setf (gethash event-type hash-table) slot-name))
    hash-table))

(defvar *event-struct-slots*
  (add-event-struct-slots
   (list (list :controlleraxismotion 'sdl2-ffi::caxis)
         (list :controllerbuttonup 'sdl2-ffi::cbutton)
         (list :controllerbuttondown 'sdl2-ffi::cbutton)
         (list :controllerdeviceremoved 'sdl2-ffi::cdevice)
         (list :controllerdeviceremapped 'sdl2-ffi::cdevice)
         (list :controllerdeviceadded 'sdl2-ffi::cdevice)
         (list :joyaxismotion 'sdl2-ffi::jaxis)
         (list :joyballmotion 'sdl2-ffi::jball)
         (list :joyhatmotion 'sdl2-ffi::jhat)
         (list :joybuttonup 'sdl2-ffi::jbutton)
         (list :joybuttondown 'sdl2-ffi::jbutton)
         (list :joydeviceadded 'sdl2-ffi::jdevice)
         (list :joydeviceremoved 'sdl2-ffi::jdevice)
         (list :keyup 'sdl2-ffi::key)
         (list :keydown 'sdl2-ffi::key)
         (list :mousewheel 'sdl2-ffi::wheel)
         (list :mousebuttonup 'sdl2-ffi::button)
         (list :mousebuttondown 'sdl2-ffi::button)
         (list :mousemotion 'sdl2-ffi::motion)
         (list :userevent 'sdl2-ffi::user)
         (list :windowevent 'sdl2-ffi::window)
         (list :quit 'sdl2-ffi::quit))))

(defun new-event (&optional (event-type :firstevent))
  (let ((enum-value (foreign-enum-value 'event-type event-type))
        (event-ptr (foreign-alloc 'sdl2-ffi:sdl-event)))
    (setf (foreign-slot-value event-ptr 'sdl2-ffi:sdl-event 'type) enum-value)
    event-ptr))

(defun free-event (event-ptr)
  (foreign-free event-ptr))

(defun get-event-type (event-ptr)
  (let ((enum-value (foreign-slot-value event-ptr 'sdl2-ffi:sdl-event 'type)))
    (foreign-enum-keyword 'event-type enum-value)))

(defun get-event-struct (event-ptr)
  (let ((event-type (get-event-type event-ptr)))
    (foreign-slot-value event-ptr 'sdl2-ffi:sdl-event (gethash event-type *event-struct-slots*))))

(defun get-event-data (event-ptr)
  (let* ((event-struct (get-event-struct event-ptr))
         (event-struct-type (intern
                             (format nil "SDL-~a" (get-event-type event-ptr))
                             'sdl2-ffi))
         (slot-names (foreign-slot-names event-struct-type)))
    (mapcan #'list
            (mapcar #'alexandria:make-keyword slot-names)
            (loop for slot-name in slot-names
               collect
                 (foreign-slot-value event-struct event-struct-type slot-name)))))

(defun next-event (event-ptr &optional (method :poll) (timeout 1))
  "Method can be either :poll, :wait, or :wait-with-timeout"
  (flet ((event-fn (case method
                     (:poll #'sdl2-ffi:sdl-pollevent)
                     (:wait #'sdl2-ffi:sdl-waitevent)
                     (:wait-with-timeout
                      (lambda (e)
                        (sdl2-ffi:sdl-waiteventtimeout e timeout))))))
    (let* ((eventp (event-fn event-ptr))
           (event (list eventp (when eventp (get-event-data event-ptr)))))
      event)))

(defmacro with-foreign-event ((event-ptr) &body body)
  `(let ((,event-ptr (new-event))
         (result (progn ,@body)))
     (free-event ,event-ptr)
     result))

(defmacro poll-event ()
  `(with-foreign-event (event-ptr)
                       (next-event event-ptr)))

(defun wait-event ()
  (with-foreign-event (event-ptr)
    (next-event event-ptr :wait)))

(defun wait-event-timeout (timeout)
  (with-foreign-event (event-ptr)
    (next-event event-ptr :wait-with-timeout timeout)))

(defun pump-events ()
  (sdl2-ffi:sdl-pumpevents))

