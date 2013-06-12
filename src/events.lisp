(in-package #:sdl2)

(defsanecenum-from-cenum (event-type sdl2-ffi:sdl-eventtype "SDL-"))

(defun add-event-struct-slots (event-descriptions)
  (let ((hash-table (make-hash-table)))
    (loop for (event-type description) in event-descriptions
       do (let* ((slot-name (first description))
                 (struct-type (second description))
                 (struct-slot-names (cffi:foreign-slot-names struct-type)))
              (setf (gethash event-type hash-table)
                    (list slot-name
                          (list :struct-type struct-type
                                :slot-names struct-slot-names)))))
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
         (list :dropevent
               (list 'sdl2-ffi::drop
                     'sdl2-ffi::sdl-dropevent))
         (list :dollargesture
               (list 'sdl2-ffi::dgesture
                     'sdl2-ffi::sdl-dollargestureevent))
         (list :multigesture
               (list 'sdl2-ffi::mgesture
                     'sdl2-ffi::sdl-multigestureevent))
         (list :touchfinger
               (list 'sdl2-ffi::tfinger
                     'sdl2-ffi::sdl-touchfingerevent))
         (list :text
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
  `(let* ((,event-ptr (new-event))
          (result (progn ,@body)))
     (free-event ,event-ptr)
     result))

(defun get-event-type (event-ptr)
  (let ((enum-value (foreign-slot-value event-ptr 'sdl2-ffi:sdl-event 'type)))
    (foreign-enum-keyword 'event-type enum-value)))

(defun get-event-struct-data (event-ptr)
  (let* ((event-type (get-event-type event-ptr))
         (event-data (gethash event-type *event-struct-data*)))
    (if (equal nil event-data)
        (nil nil)
        event-data)))

(defun unpack-event (event-ptr))

(defun pump-events ()
  (sdl2-ffi:sdl-pumpevents))

(defun push-event (event-type)
  (check-rc (with-sdl-event (event-ptr event-type)
              (sdl2-ffi:sdl-pushevent event-ptr))))

(defun next-event (event-ptr &optional (method :poll) (timeout 1))
  "Method can be either :poll, :wait, or :wait-with-timeout"
  (funcall (case method
             (:poll #'sdl2-ffi:sdl-pollevent)
             (:wait #'sdl2-ffi:sdl-waitevent)
             (:wait-with-timeout
              (lambda (e)
                (sdl2-ffi:sdl-waiteventtimeout e timeout)))
             (otherwise (error "Event method must be :poll :wait or :wait-with-timeout")))
           event-ptr))

(defmacro with-event-loop ((&key (framerate 60)) &body forms)
  (let ((quit nil))
    (with-sdl-event (sdl-event)
      `(loop until quit
          do ,@forms))))

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

