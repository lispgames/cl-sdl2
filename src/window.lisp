(in-package #:sdl2)

(defbitfield-from-cenum (sdl-window-flags
                         sdl2-ffi:sdl-windowflags
                         "SDL-WINDOW-")
  (:centered #x0))

(defun create-window (&key
                        (title "SDL2 Window")
                        (x windowpos-centered) (y windowpos-centered)
                        (w 800) (h 600) flags)
  (let ((window-flags (foreign-bitfield-value 'sdl-window-flags flags)))
    (sdl2-ffi:sdl-createwindow title x y w h window-flags)))

(defun destroy-window (win)
  (sdl2-ffi:sdl-destroywindow win))
