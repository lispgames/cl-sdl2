(in-package #:sdl2)

(defsanecenum-from-cenum (event-type
                          sdl2-ffi:sdl-eventtype
                          "SDL-"))

(defwrapper event (sdl2-ffi:sdl-event))

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

(defun event-value (event-ptr slot)
  (foreign-slot-value event-ptr 'sdl2-ffi:sdl-event (intern (string slot) :sdl2-ffi)))

(defun get-event-data (event-ptr)
  (case (get-event-type event-ptr)
    (:windowevent ())
    (:keyboardevent ())
    (:mousemotion ())
    (:mousebuttonup ())
    (:mousebuttondown ())
    (:mousewheel ())
    (:joyaxismotion ())
    (:joyballmotion ())
    (:joyhatmotion ())
    (:joybuttonup ())
    (:joybuttondown ())
    (:joydeviceadded ())
    (:joydeviceremoved ())
    (:controlleraxismotion ())
    (:controllerbuttonup ())
    (:controllerbuttondown ())
    (:controllerdeviceadded ())
    (:controllerdeviceremoved ())
    (:controllerdeviceremapped ())
    (:quitevent ())
    (t ())))

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

(defmacro with-foreign-event (&body body)
  `(let ((event-ptr (new-event))
         (result (progn ,@body)))
     (free-event event-ptr)
     result))

(defmacro poll-event ()
  `(with-foreign-event
       (next-event event-ptr)))

(defmacro wait-event ()
  `(with-foreign-event
       (next-event event-ptr :wait)))

(defmacro wait-event-timeout (timeout)
  `(with-foreign-event
       (next-event event-ptr :wait-with-timeout timeout)))

(defun pump-events ()
  (sdl2-ffi:sdl-pumpevents))

