(in-package #:sdl2)

(defbitfield-from-cenum (sdl-window-flags
                         sdl2-ffi:sdl-windowflags
                         "SDL-WINDOW-")
  (:centered #x0))

(defbitfield-from-cenum (sdl-gl-attr
                         sdl2-ffi:sdl-glattr
                         "SDL-GL-"))

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

(defwrapper sdl-window ())

(defun create-window (&key
                      (title "SDL2 Window")
                      (x :centered) (y :centered)
                      (w 800) (h 600) flags)
  (let ((window-flags (foreign-bitfield-value 'sdl-window-flags flags))
        (x (windowpos-from-coord x))
        (y (windowpos-from-coord y)))
    (sdl-collect
     (%make-sdl-window
      :ptr (check-null (sdl2-ffi:sdl-createwindow title x y w h window-flags)))
     #'sdl2-ffi:sdl-destroywindow)))

(defun destroy-window (win)
  (sdl-cancel-collect win)
  (sdl2-ffi:sdl-destroywindow (sdl-ptr win))
  (sdl-invalidate win))

(defun hide-window (win)
  (sdl2-ffi:sdl-hidewindow (sdl-ptr win)))

(defun show-window (win)
  (sdl2-ffi:sdl-showwindow (sdl-ptr win)))

(defun maximize-window (win)
  (sdl2-ffi:sdl-maximizewindow (sdl-ptr win)))

(defun minimize-window (win)
  (sdl2-ffi:sdl-minimizewindow (sdl-ptr win)))

(defun raise-window (win)
  (sdl2-ffi:sdl-raisewindow (sdl-ptr win)))

(defun restore-window (win)
  (sdl2-ffi:sdl-restorewindow (sdl-ptr win)))

(defun update-window (win)
  (check-rc (sdl2-ffi:sdl-updatewindowsurface (sdl-ptr win))))

(defun set-window-title (win title)
  (sdl2-ffi:sdl-setwindowtitle (sdl-ptr win) title))

(defun set-window-fullscreen (win fullscreen-p)
  (let ((fs (if fullscreen-p 1 0)))
    (check-rc (sdl2-ffi:sdl-setwindowfullscreen (sdl-ptr win) fs))))

(defun set-window-size (win w h)
  (sdl2-ffi:sdl-setwindowsize (sdl-ptr win) w h))

(defun set-window-position (win x y)
  (sdl2-ffi:sdl-setwindowposition (sdl-ptr win)
                                  (windowpos-from-coord x)
                                  (windowpos-from-coord y)))

(defun get-window-title (win)
  (sdl2-ffi:sdl-getwindowtitle (sdl-ptr win)))

(defun get-window-position (win)
  (with-foreign-objects ((xpos :int)
                         (ypos :int))
    (sdl2-ffi:sdl-getwindowposition (sdl-ptr win) xpos ypos)
    (values (mem-ref xpos :int) (mem-ref ypos :int))))

(defun get-window-size (win)
  (with-foreign-objects ((width :int)
                         (height :int))
    (sdl2-ffi:sdl-getwindowsize (sdl-ptr win) width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(defun get-window-flags (win)
  (let ((flags (sdl2-ffi:sdl-getwindowflags (sdl-ptr win))))
    (foreign-bitfield-symbols 'sdl-window-flags flags)))

(defun enable-screensaver ()
  (sdl2-ffi:sdl-enablescreensaver))

(defun disable-screensaver ()
  (sdl2-ffi:sdl-disablescreensaver))

(defun screensaver-enabled-p ()
  (sdl2-ffi:sdl-isscreensaverenabled))

(defwrapper gl-context ())

(defun gl-create-context (win)
  (sdl-collect
   (%make-gl-context
    :ptr (check-null (sdl2-ffi:sdl-gl-createcontext (sdl-ptr win))))
   #'sdl2-ffi:sdl-gl-deletecontext))

(defun gl-delete-context (gl-context)
  (sdl-cancel-collect gl-context)
  (sdl2-ffi:sdl-gl-deletecontext (sdl-ptr gl-context))
  (sdl-invalidate gl-context))

(defun gl-extension-supported-p (extension)
  (sdl2-ffi:sdl-gl-extensionsupported extension))

(defun gl-make-current (win gl-context)
  (check-rc (sdl2-ffi:sdl-gl-makecurrent (sdl-ptr win)
                                         (sdl-ptr gl-context))))

(defun gl-get-swap-interval ()
  (let ((rc (sdl2-ffi:sdl-gl-getswapinterval)))
    ;; Doing this funky thing because you want to get the value back from
    ;; the call, and still want to have the sdl-error condition if it's
    ;; not supported.
    (check-rc rc)
    rc))

(defun gl-set-swapinterval (interval)
  "0 for immediate updates, 1 for updates synchronized with the vertical retrace"
  (check-rc (sdl2-ffi:sdl-gl-setswapinterval interval)))

(defun gl-swap-window (win)
  (sdl2-ffi:sdl-gl-swapwindow (sdl-ptr win)))

(defun gl-get-attr (attr)
  (with-foreign-object (value :int)
    (check-rc (sdl2-ffi:sdl-gl-getattribute
               (foreign-bitfield-value 'sdl-gl-attr (list attr))
               value))
    (mem-ref value :int)))

(defun gl-get-attrs (&rest attrs)
  (mapcan #'list attrs (mapcar #'gl-get-attr attrs)))

(defun gl-set-attr (attr value)
  (check-rc (sdl2-ffi:sdl-gl-setattribute
             (foreign-bitfield-value 'sdl-gl-attr (list attr))
             value)))

(defun gl-set-attrs (&rest attr-plist)
  (loop for (attr value) on attr-plist by #'cddr
     do (gl-set-attr attr value)))
