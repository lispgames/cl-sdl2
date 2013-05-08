(in-package #:sdl2)

(defconstant windowpos-centered 0)

(defconstant window-fullscreen #x00000001)
(defconstant window-fullscreen-desktop (logior window-fullscreen #x00001000))
(defconstant window-opengl #x00000002)
(defconstant window-shown #x00000004)
(defconstant window-hidden #x00000008)
(defconstant window-borderless #x00000010)
(defconstant window-resizable #x00000020)
(defconstant window-minimized #x00000040)
(defconstant window-maximized #x00000080)
(defconstant window-input-grabbed #x00000100)
(defconstant window-input-focus #x00000200)
(defconstant window-mouse-focus #x00000400)


(defun create-window (&optional
                        (title "SDL2 Window")
                        (x windowpos-centered) (y windowpos-centered)
                        (w 800) (h 600)
                      &rest flags)
  (let ((window-flags (if flags (apply #'logior flags)
                          (list window-shown))))
    (sdl2-ffi::sdl-createwindow title x y w h window-flags)))

(defun destroy-window (win)
  (sdl2-ffi::sdl-destroywindow win))
