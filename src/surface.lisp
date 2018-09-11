(in-package :sdl2)

(defun surface-width (surface)
  (c-ref surface sdl2-ffi:sdl-surface :w))

(defun surface-height (surface)
  (c-ref surface sdl2-ffi:sdl-surface :h))

(defun surface-pixels (surface)
  "Access raw pixel data from a surface object"
  (c-ref surface sdl2-ffi:sdl-surface :pixels))

(defun surface-format (surface)
  (c-ref surface sdl2-ffi:sdl-surface :format))

(defun surface-pitch (surface)
  (c-ref surface sdl2-ffi:sdl-surface :pitch))

(defun surface-format-format (surface)
  (enum-key '(:enum (sdl-pixel-format))
            (c-ref surface sdl2-ffi:sdl-surface :format :format)))

(defun create-rgb-surface (width height depth
                           &key (r-mask 0) (g-mask 0) (b-mask 0) (a-mask 0) (flags 0))
  (sdl-create-rgb-surface flags width height depth r-mask g-mask b-mask a-mask))

(defun create-rgb-surface-from (pixels width height depth pitch
                                &key (r-mask 0) (g-mask 0) (b-mask 0) (a-mask 0))
  (sdl-create-rgb-surface-from pixels width height depth pitch r-mask g-mask b-mask a-mask))

(defun create-rgb-surface-with-format-from (pixels width height depth pitch
                                            &key (format +pixelformat-rgba8888+))
  (sdl-create-rgb-surface-with-format-from pixels width height depth pitch format))

(defun free-surface (surface)
  (sdl-free-surface surface)
  (invalidate surface))

(defun load-bmp (filename)
  ;; Note, SDL_LoadBMP is a macro in SDL_surface.h that is exactly this
  (sdl-load-bmp-rw (sdl-rw-from-file (namestring (merge-pathnames filename)) "rb") 1))

(defun convert-surface (surface format &key (flags 0))
  (sdl-convert-surface surface format flags))

(defun convert-surface-format (surface pixel-format-enum &key (flags 0))
  (check-nullptr
   (sdl-convert-surface-format surface
                               (enum-value '(:enum (sdl-pixel-format)) pixel-format-enum)
                               flags)))

(defun blit-surface (surface-src src-rect surface-dst dst-rect)
  (sdl-upper-blit surface-src src-rect surface-dst dst-rect))

(defun blit-scaled (surface-src src-rect surface-dst dst-rect)
  (sdl-upper-blit-scaled surface-src src-rect surface-dst dst-rect))

(defun fill-rect (surface-dst rect color)
  (sdl-fill-rect surface-dst rect color))

(defun set-color-key (surface flag key)
  "Use this function to set the color key (transparent pixel) in a surface."
  (check-rc (sdl-set-color-key surface (autowrap:enum-value 'sdl2-ffi:sdl-bool flag) key)))

(defun get-color-key (surface)
  "Use this function to get the color key (transparent pixel) for a surface."
  (c-let ((key sdl2-ffi:uint32))
    (check-rc (sdl-get-color-key surface (key &)))
    key))

(defun set-alpha-mod (surface alpha)
  "Use this function to set an additional alpha value used in blit operations."
  (check-rc (sdl-set-surface-alpha-mod surface alpha)))

(defun get-alpha-mod (surface)
  "Use this function to get the additional alpha value used in blit operations."
  (c-let ((alpha sdl2-ffi:uint8))
    (check-rc (sdl-get-surface-alpha-mod surface (alpha &)))
    alpha))

(defun set-color-mod (surface r g b)
  "Use this function to set an additional color value multiplied into blit operations."
  (check-rc (sdl-set-surface-color-mod surface r g b)))

(defun get-color-mod (surface)
  "Use this function to get the additional color value multiplied into blit operations."
  (c-let ((r sdl2-ffi:uint8)
          (g sdl2-ffi:uint8)
          (b sdl2-ffi:uint8))
    (check-rc (sdl-get-surface-color-mod surface (r &) (g &) (b &)))
    (values r g b)))
