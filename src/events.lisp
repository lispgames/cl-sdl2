(in-package #:sdl2)

(defsanecenum-from-cenum (eventtype
                          sdl2-ffi:sdl-eventtype
                          "SDL-"))

(defwrapper event (sdl2-ffi:sdl-event))
(defwrapper window-event (sdl2-ffi:sdl-windowevent))
(defwrapper keyboard-event (sdl2-ffi:sdl-keyboardevent))
(defwrapper mouse-motion-event (sdl2-ffi:sdl-mousemotionevent))
(defwrapper mouse-button-event (sdl2-ffi:sdl-mousebuttonevent))
(defwrapper mouse-wheel-event (sdl2-ffi:sdl-mousewheelevent))
(defwrapper joy-axis-event (sdl2-ffi:sdl-joyaxisevent))
(defwrapper joy-ball-event (sdl2-ffi:sdl-joyballevent))
(defwrapper joy-hat-event (sdl2-ffi:sdl-joyhatevent))
(defwrapper joy-button-event (sdl2-ffi:sdl-joybuttonevent))
(defwrapper joy-device-event (sdl2-ffi:sdl-joydeviceevent))
(defwrapper controller-axis-event (sdl2-ffi:sdl-controlleraxisevent))
(defwrapper controller-button-event (sdl2-ffi:sdl-controllerbuttonevent))
(defwrapper controller-device-event (sdl2-ffi:sdl-controllerdeviceevent))
(defwrapper quit-event (sdl2-ffi:sdl-quitevent))

(defun get-event-contstructor (event)
  (case (foreign-enum-keyword
         'eventtype (foreign-slot-value event 'sdl2-ffi:sdl-event 'type))
    (:windowevent #'%make-window-event)
    (:keyboardevent #'%make-keyboard-event)
    (:mousemotion #'%make-mouse-motion-event)
    (:mousebuttonup #'%make-mouse-button-event)
    (:mousebuttondown #'%make-mouse-button-event)
    (:mousewheel #'%make-mouse-wheel-event)
    (:joyaxismotion #'%make-joy-axis-event)
    (:joyballmotion #'%make-joy-ball-event)
    (:joyhatmotion #'%make-joy-hat-event)
    (:joybuttonup #'%make-joy-button-event)
    (:joybuttondown #'%make-joy-button-event)
    (:joydeviceadded #'%make-joy-device-event)
    (:joydeviceremoved #'%make-joy-device-event)
    (:controlleraxismotion #'%make-controller-axis-event)
    (:controllerbuttonup #'%make-controller-button-event)
    (:controllerbuttondown #'%make-controller-button-event)
    (:controllerdeviceadded #'%make-controller-device-event)
    (:controllerdeviceremoved #'%make-controller-device-event)
    (:controllerdeviceremapped #'%make-controller-device-event)
    (:quitevent #'%make-quit-event)
    (t #'%make-event)))

(defun poll-event ()
  (let* ((event (foreign-alloc 'sdl2-ffi:sdl-event))
         (events-p (sdl2-ffi:sdl-pollevent event)))
    (list events-p (when events-p
                     (sdl-collect
                      (funcall (get-event-contstructor event) :ptr event)
                      #'foreign-free)))))

(defun pump-events ()
  (sdl2-ffi:sdl-pumpevents))


