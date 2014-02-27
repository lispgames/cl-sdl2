(in-package :sdl2.kit)

(defvar *main-loop* nil)
(defvar *main-loop-quit* nil)
(defvar *focused-window-id* nil)

(defvar *event* nil)

(defun main-loop-function (ev idle-p)
  "This is called every iteration of the main loop.  It exists
primarily so it can be easily redefined without starting/stopping."
  (plus-c:c-let ((event sdl2-ffi:sdl-event :from ev))
    ;; Note GET-EVENT-TYPE gives us :LISP-MESSAGE
    (let ((type (when ev (sdl2:get-event-type ev)))
          (*event* ev))
      (cond
        (idle-p
         (loop for id being each hash-key in *idle-render-windows*
               do (render (gethash id *all-windows*))))
        ((eq :lisp-message type)
         (sdl2::get-and-handle-messages))
        ((or (eq :mousebuttondown type)
             (eq :mousebuttonup type))
         (let ((window (window-from-id (event :button :window-id))))
           (when window
             (mousebutton-event window
                                type
                                (event :button :timestamp)
                                (event :button :button)
                                (event :button :x)
                                (event :button :y)))))
        ((eq :mousemotion type)
         (let ((window (window-from-id (event :motion :window-id))))
           (when window
             (mousemotion-event window
                                (event :motion :timestamp)
                                (event :motion :state)
                                (event :motion :x)
                                (event :motion :y)
                                (event :motion :xrel)
                                (event :motion :yrel)))))
        ((or (eq :keydown type)
             (eq :keyup type))
         (let ((window (window-from-id (event :key :window-id))))
           (when window
             (keyboard-event window
                             type
                             (event :key :timestamp)
                             (/= 0 (event :key :repeat))
                             (event :key :keysym)))))
        ((eq :textinput type)
         (let ((window (window-from-id (event :text :window-id))))
           (when window
             (textinput-event window
                              (event :text :timestamp)
                              (cffi:foreign-string-to-lisp
                               (event :text :text plus-c:&))))))
        ((eq :mousewheel type)
         (let ((window (window-from-id (event :wheel :window-id))))
           (when window
             (mousewheel-event window
                               (event :wheel :timestamp)
                               (event :wheel :x)
                               (event :wheel :y)))))
        ((eq :windowevent type)
         (let ((window (window-from-id (event :window :window-id)))
               (window-event-type
                 (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id
                                    (event :window :event))))
           (when window
             (case window-event-type
               (:focus-gained (setf *focused-window-id* (event :window :window-id)))
               (:focus-lost (setf *focused-window-id* nil)))
             (window-event window
                           window-event-type
                           (event :window :timestamp)
                           (event :window :data1)
                           (event :window :data2)))))
        (t
         (let ((focused-window (gethash *focused-window-id* *all-windows*)))
           (when focused-window
             (other-event focused-window ev))))))))

(defun main-loop ()
  (setf *main-loop* t)
  (let (*main-loop-quit*)
    (unwind-protect
         (sdl2:with-sdl-event (ev)
           (loop as method = (if (> (hash-table-count *idle-render-windows*) 0)
                                 :poll :wait)
                 as rc = (sdl2:next-event ev method)
                 as idle-p = (and (= 0 rc) (eq :poll method))
                 do (handler-case
                        ;; Note we do not want to pass the _prior_ event
                        ;; if we are idle
                        (main-loop-function (unless idle-p ev) idle-p)
                      (error (e)
                        (format *error-output*
                                "SDL2.KIT:MAIN-LOOP-FUNCTION Error: ~%  ~A~%"
                                e)))
                    (when *main-loop-quit*
                      (return))))
      (setf *main-loop* nil))))

(defun start ()
  (unless *main-loop*
    (sdl2:init :everything)
    (sdl2:in-main-thread (:background t :no-event t)
      (main-loop)
      (sdl2-ffi.functions:sdl-quit))))

(defun quit ()
  (sdl2:in-main-thread ()
    (setf *main-loop-quit* t)))
