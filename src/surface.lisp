(in-package :sdl2)

(defun create-rgb-surface (width height depth
                           &key (r-mask 0) (g-mask 0) (b-mask 0) (a-mask 0)
                           (flags 0))
  (sdl-collect
   (sdl-create-rgb-surface flags width height depth
                           r-mask g-mask b-mask a-mask)
   (lambda (s) (sdl-free-surface s))))

(defun create-rgb-surface-from (pixels width height depth pitch
                                &key (r-mask 0) (g-mask 0) (b-mask 0) (a-mask 0))
  (sdl-collect
   (sdl-create-rgb-surface-from pixels width height depth pitch
                                r-mask g-mask b-mask a-mask)
   (lambda (s) (sdl-free-surface s))))

(defun free-surface (surface)
  (sdl-cancel-collect surface)
  (sdl-free-surface surface)
  (invalidate surface))

(c-let ((a :unsigned-char :count (* 128 128 4) :free t))
  (a &))
