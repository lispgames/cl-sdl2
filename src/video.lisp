(in-package #:sdl2)

(autowrap:define-bitmask-from-enum
    (sdl-window-flags sdl2-ffi:sdl-window-flags)
  '(:centered . #x0))

(autowrap:define-bitmask-from-enum
    (sdl-gl-attr sdl2-ffi:sdl-glattr))

(defun windowpos-undefined (&optional (display 0))
  (logior sdl2-ffi:+sdl-windowpos-undefined-mask+
          display))

(defun windowpos-centered (&optional (display 0))
  (logior sdl2-ffi:+sdl-windowpos-centered-mask+
          display))

(defun windowpos-from-coord (n)
  (case n
    (:undefined (windowpos-undefined))
    (:centered (windowpos-centered))
    (t n)))

(defun create-window (&key
                      (title "SDL2 Window")
                      (x :centered) (y :centered)
                      (w 800) (h 600) flags)
  (let ((window-flags (mask-apply 'sdl-window-flags flags))
        (x (windowpos-from-coord x))
        (y (windowpos-from-coord y)))
    (sdl-collect
     (check-null (sdl-create-window title x y w h window-flags))
     (lambda (w) (sdl-destroy-window w)))))

(defun destroy-window (win)
  (sdl-cancel-collect win)
  (sdl-destroy-window win)
  (autowrap:invalidate win)
  (values))

(defmacro with-window ((win &key (title "SDL2 Window")
                        (x :centered) (y :centered)
                        (w 800) (h 600) flags)
                       &body body)
  `(let ((,win (create-window :title ,title
                              :x ,x :y ,y :w ,w :h ,h
                              :flags ,flags)))
     (unwind-protect (progn ,@body)
       (destroy-window ,win))))

(defun hide-window (win)
  (sdl-hide-window win))

(defun show-window (win)
  (sdl-show-window win))

(defun maximize-window (win)
  (sdl-maximize-window win))

(defun minimize-window (win)
  (sdl-minimize-window win))

(defun raise-window (win)
  (sdl-raise-window win))

(defun restore-window (win)
  (sdl-restore-window win))

(defun update-window (win)
  (check-rc (sdl-update-window-surface win)))

(defun set-window-title (win title)
  (sdl-set-window-title win title))

(defun set-window-fullscreen (win fullscreen-p)
  (let ((fs (if fullscreen-p 1 0)))
    (check-rc (sdl-set-window-fullscreen win fs))))

(defun set-window-size (win w h)
  (sdl-set-window-size win w h))

(defun set-window-position (win x y)
  (sdl-set-window-position win
                           (windowpos-from-coord x)
                           (windowpos-from-coord y)))

(defun get-window-title (win)
  (sdl-get-window-title win))

(defun get-window-position (win)
  (with-foreign-objects ((xpos :int)
                         (ypos :int))
    (sdl-get-window-position win xpos ypos)
    (values (mem-ref xpos :int) (mem-ref ypos :int))))

(defun get-window-size (win)
  (with-foreign-objects ((width :int)
                         (height :int))
    (sdl-get-window-size win width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(defun get-window-flags (win)
  (let ((flags (sdl-get-window-flags win)))
    (autowrap:mask-keywords 'sdl-window-flags flags)))

(defun enable-screensaver ()
  (sdl-enable-screen-saver))

(defun disable-screensaver ()
  (sdl-disable-screen-saver))

(defun screensaver-enabled-p ()
  (sdl-is-screen-saver-enabled))

(defun gl-create-context (win)
  (sdl-collect
   (check-null (sdl-gl-create-context win))
   (lambda (x) (sdl-gl-delete-context x))))

(defun gl-delete-context (gl-context)
  (sdl-cancel-collect gl-context)
  (sdl-gl-delete-context gl-context)
  (autowrap:invalidate gl-context))

(defun gl-extension-supported-p (extension)
  (sdl-gl-extension-supported extension))

(defun gl-make-current (win gl-context)
  (check-rc (sdl-gl-make-current win gl-context)))

(defun gl-get-swap-interval ()
  (check-rc (sdl-gl-get-swap-interval)))

(defun gl-set-swap-interval (interval)
  "0 for immediate updates, 1 for updates synchronized with the vertical retrace"
  (check-rc (sdl-gl-set-swap-interval interval)))

(defun gl-swap-window (win)
  (sdl-gl-swap-window win))

(defun gl-get-attr (attr)
  (with-foreign-object (value :int)
    (check-rc (sdl-gl-get-attribute
               (autowrap:mask 'sdl-gl-attr attr) value))
    (mem-ref value :int)))

(defun gl-get-attrs (&rest attrs)
  (mapcan #'list attrs (mapcar #'gl-get-attr attrs)))

(defun gl-set-attr (attr value)
  (check-rc (sdl-gl-set-attribute
             (autowrap:mask 'sdl-gl-attr attr)
             value)))

(defun gl-set-attrs (&rest attr-plist)
  (loop for (attr value) on attr-plist by #'cddr
        do (gl-set-attr attr value)))
