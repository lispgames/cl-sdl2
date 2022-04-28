(in-package :sdl2)

;;;; SDL_GetRendererOutputSize

(defun make-renderer-info ()
  "Return an uninitialized SDL_RendererInfo structure."
  (autowrap:alloc 'sdl2-ffi:sdl-renderer-info))

(defmethod print-object ((rinfo sdl2-ffi:sdl-renderer-info) stream)
  (c-let ((rinfo sdl2-ffi:sdl-renderer-info :from rinfo))
    (print-unreadable-object (rinfo stream :type t :identity t)
      (format stream "name ~S flags ~A num-texture-formats ~A texture-formats TBD max-texture-width ~
~A max-texture-height ~A"
              (rinfo :name)
              (rinfo :flags)
              (rinfo :num-texture-formats)
              (rinfo :max-texture-width)
              (rinfo :max-texture-height)))))

(defun free-render-info (rinfo)
  "Specifically free the SDL_RendererInfo structure."
  (foreign-free (ptr rinfo))
  (autowrap:invalidate rinfo))

;;;; And now the wrapping of the SDL2 calls

;; Create the keywords for the SDL_RendererFLags enum.
(autowrap:define-bitmask-from-enum (sdl-renderer-flags sdl2-ffi:sdl-renderer-flags))

;; Create the keywords for the SDL_TextureModulate enum.
(autowrap:define-bitmask-from-enum (sdl-texture-modulate sdl2-ffi:sdl-texture-modulate))

;; Create the keywords for the SDL_RendererFlip enum.
(autowrap:define-bitmask-from-enum (sdl-renderer-flip sdl2-ffi:sdl-renderer-flip))

(defun get-num-render-drivers ()
  "Return the number of 2D rendering drivers available for the current display."
  (sdl-get-num-render-drivers))

(defun get-render-driver-info (index)
  "Allocate and return a new SDL_RendererInfo structure and fill it with information relating to the
specific 2D rendering driver specified in the index."
  (let ((rinfo (make-renderer-info)))
    (check-rc (sdl-get-render-driver-info index rinfo))
    rinfo))

(defun create-window-and-renderer (width height flags)
  (c-let ((winptr :pointer :free t)
          (rendptr :pointer :free t))
    (check-rc (sdl-create-window-and-renderer
               width height
               (mask-apply 'sdl-window-flags flags)
               (winptr &) (rendptr &)))
    (let ((window (sdl2-ffi::make-sdl-window :ptr winptr))
          (renderer (sdl2-ffi::make-sdl-renderer :ptr rendptr)))
      (values window renderer))))

(defun create-renderer (window &optional index flags)
  "Create a 2D rendering context for a window."
  (check-nullptr (sdl-create-renderer
                  window (or index -1)
                  (mask-apply 'sdl-renderer-flags flags))))

(defun create-software-renderer (surface)
  "Create and return a 2D software rendering context for the surface."
  (check-nullptr (sdl-create-software-renderer surface)))

(defun destroy-renderer (r)
  (sdl-destroy-renderer r)
  (invalidate r))

(defmacro with-renderer ((renderer-sym window &key index flags) &body body)
  `(let ((,renderer-sym (sdl2:create-renderer ,window ,index ,flags)))
     (unwind-protect
          (progn ,@body)
       (sdl2:destroy-renderer ,renderer-sym))))

(defun get-renderer (window)
  "Return NIL if there is no renderer associated with the window, or otherwise the SDL_Renderer
structure."
  (let ((renderer (sdl-get-renderer window)))
    (if (null-pointer-p (autowrap:ptr renderer))
        nil
        renderer)))

(defun render-copy (renderer texture &key source-rect dest-rect)
  "Use this function to copy a portion of the texture to the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-render-copy renderer texture source-rect dest-rect)))

(defun render-copy-f (renderer texture &key source-rect dest-rect)
  "Copy a portion of the texture to the current rendering target at subpixel precision."
  (check-rc (sdl2-ffi.functions:sdl-render-copy-f renderer texture source-rect dest-rect)))

(defun render-copy-ex (renderer texture &key source-rect dest-rect angle center flip)
  "Use this function to copy a portion of the texture to the current rendering target, optionally
rotating it by angle around the given center and also flipping it top-bottom and/or left-right."
  (check-rc (sdl2-ffi.functions:sdl-render-copy-ex
             renderer
             texture
             source-rect
             dest-rect
             (coerce (or angle 0) 'double-float)
             center
             (mask-apply 'sdl-renderer-flip flip))))

(defun render-copy-ex-f (renderer texture &key source-rect dest-rect angle center flip)
  "Copy a portion of the source texture to the current rendering target, with rotation and flipping,
at subpixel precision."
  (check-rc (sdl2-ffi.functions:sdl-render-copy-ex-f
             renderer
             texture
             source-rect
             dest-rect
             (coerce (or angle 0) 'double-float)
             center
             (mask-apply 'sdl-renderer-flip flip))))

(defun set-render-draw-color (renderer r g b a)
  "Use this function to set the color used for drawing operations (Rect, Line and Clear)."
  (check-rc (sdl2-ffi.functions:sdl-set-render-draw-color renderer r g b a)))

(defun get-render-draw-color (renderer)
  "Use this function to get the current color used by renderer for drawing operations"
  (c-with ((r sdl2-ffi:uint8)
	   (g sdl2-ffi:uint8)
	   (b sdl2-ffi:uint8)
	   (a sdl2-ffi:uint8))
    (check-rc (sdl2-ffi.functions:sdl-get-render-draw-color renderer (r &) (g &) (b &) (a &)))
    (values r g b a)))

(defun set-texture-blend-mode (texture blend-mode)
  "Use this function to set the blend mode for a texture, used by SDL_RenderCopy()."
  (check-rc (sdl2-ffi.functions:sdl-set-texture-blend-mode texture blend-mode)))

(defun set-render-draw-blend-mode (renderer blend-mode)
  "Use this function to set the blend mode used for drawing operations (Fill and Line)."
  (check-rc (sdl2-ffi.functions:sdl-set-render-draw-blend-mode renderer blend-mode)))

(defun set-render-target (renderer texture)
  "Use this function to set a texture as the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-set-render-target renderer texture)))

(defun get-render-target (renderer)
  (sdl2-ffi.functions:sdl-get-render-target renderer))

(defun render-draw-line (renderer x1 y1 x2 y2)
  "Use this function to draw a line on the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-render-draw-line renderer x1 y1 x2 y2)))

(defun render-draw-lines (renderer points num-points)
  "Pass a pointer to SDL_Point to render connected lines on the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-render-draw-lines renderer points num-points)))

(defun render-draw-point (renderer x y)
  "Use this function to draw a point on the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-render-draw-point renderer x y)))

(defun render-draw-points (renderer points num-points)
  "Use this function to draw multiple points on the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-render-draw-points renderer points num-points)))

(defun render-draw-rect (renderer sdl-rect)
  "Use this function to draw a rectangle on the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-render-draw-rect renderer sdl-rect)))

(defun render-draw-rects (renderer rects num-rects)
  "Use this function to draw some number of rectangles on the current rendering target."
  (check-rc (sdl2-ffi.functions:sdl-render-draw-rects renderer rects num-rects)))

(defun render-fill-rect (renderer sdl-rect)
  "Use this function to fill a rectangle on the current rendering target with
the drawing color. "
  (check-rc (sdl2-ffi.functions:sdl-render-fill-rect renderer sdl-rect)))

(defun render-fill-rect-f (renderer sdl-rect)
  "Fill a rectangle on the current rendering target with the drawing color at subpixel precision."
  (check-rc (sdl2-ffi.functions:sdl-render-fill-rect-f renderer sdl-rect)))

(defun render-fill-rects (renderer rects num-rects)
  "Use this function to fill some number of rectangles on the current
rendering target with the drawing color."
  (check-rc (sdl2-ffi.functions:sdl-render-fill-rects renderer rects num-rects)))

(defun render-fill-rects-f (renderer rects num-rects)
  "Fill some number of rectangles on the current rendering target with the drawing color at subpixel
precision."
  (check-rc (sdl2-ffi.functions:sdl-render-fill-rects-f renderer rects num-rects)))

(defun render-set-viewport (renderer sdl-rect)
  "Use this function to set the drawing area for rendering on the current target."
  (check-rc (sdl2-ffi.functions:sdl-render-set-viewport renderer sdl-rect)))

(defun render-get-viewport (renderer)
  "Use this function to get the drawing area for the current target."
  (let-rects (rect) (sdl2-ffi.functions:sdl-render-get-viewport renderer (rect &)) rect))

(defun render-clear (renderer)
  "Use this function to clear the current rendering target with the drawing color."
  (check-rc (sdl2-ffi.functions:sdl-render-clear renderer)))

(defun render-present (renderer)
  "Use this function to update the screen with rendering performed."
  (sdl2-ffi.functions:sdl-render-present renderer))

(defun get-renderer-info (renderer)
  "Allocate a new SDL_RendererInfo structure, fill it in with information
about the specified renderer, and return it."
  (let ((rinfo (make-renderer-info)))
    (check-rc (sdl-get-renderer-info renderer rinfo))
    rinfo))

(defun get-renderer-max-texture-size (renderer)
  (c-let ((info sdl2-ffi:sdl-renderer-info :from (get-renderer-info renderer)))
    (unwind-protect
         (values (info :max-texture-width)
                 (info :max-texture-height))
      (free-render-info info))))

;; TODO SDL_GetRendererOutputSize
(defun get-renderer-output-size (renderer)
  (c-with ((x :int)
           (y :int))
    (sdl-get-renderer-output-size renderer (x &) (y &))
    (values x y)))

(defun query-texture (texture)
  (c-with ((texture-format sdl2-ffi:uint32)
           (access :int)
           (width  :int)
           (height :int))
    (check-rc (sdl-query-texture texture (texture-format &) (access &) (width &) (height &)))
    (values texture-format access width height)))

;;; Convenience functions to query only textures width and height
(defun texture-width (texture)
  (c-with ((width :int))
    (check-rc (sdl-query-texture texture nil nil (width &) nil))
    width))

(defun texture-height (texture)
  (c-with ((height :int))
    (check-rc (sdl-query-texture texture nil nil nil (height &)))
    height))

(defun update-texture (texture rect pixels pitch)
  "Use this function to update the given texture rectangle with new pixel data."
  (check-rc (sdl2-ffi.functions:sdl-update-texture texture rect pixels pitch)))

(defun create-texture (renderer pixel-format access width height)
  "Use this function to create a texture for a rendering context."
  (check-nullptr (sdl-create-texture renderer
                                     (enum-value 'sdl-pixel-format pixel-format)
                                     (enum-value 'sdl2-ffi:sdl-texture-access access)
                                     width height)))

(defun create-texture-from-surface (renderer surface)
  "Use this function to create a texture from sdl2 surface for a rendering context."
  (check-nullptr (sdl-create-texture-from-surface renderer surface)))

(defun set-texture-color-mod (texture r g b)
  "Use this function to set an additional color value multiplied into render copy operations."
  (check-rc (sdl-set-texture-color-mod texture r g b)))

(defun get-texture-color-mod (texture)
  "Use this function to get the additional color value multiplied into render copy operations."
  (c-with ((r :unsigned-short)
           (g :unsigned-short)
           (b :unsigned-short))
    (check-rc (sdl-get-texture-color-mod texture (r &) (g &) (b &)))
    (values r g b)))

(defun set-texture-alpha-mod (texture alpha)
  "Use this function to set an additional alpha value multiplied into render copy operations."
  (check-rc (sdl-set-texture-alpha-mod texture alpha)))

(defun get-texture-alpha-mod (texture)
  "Use this function to get the additional alpha value multiplied into render copy operations."
  (c-with ((alpha :unsigned-short))
    (check-rc (sdl-get-texture-alpha-mod texture (alpha &)))
    alpha))

(defun destroy-texture (texture)
  "Use this function to destroy the specified texture."
  (sdl-destroy-texture texture)
  (invalidate texture))

(defun lock-texture (texture &optional rect)
  "Use this function to lock a portion of the texture for write-only pixel access."
  (c-let ((pixels :pointer :free t)
          (pitch :int :free t))
    (check-rc (sdl-lock-texture texture rect (pixels &) (pitch &)))
    (values pixels pitch)))

(defun unlock-texture (texture)
  "Use this function to unlock a texture, uploading the changes to video memory, if needed. Warning:
See Bug No. 1586 before using this function!"
  (sdl-unlock-texture texture))

(defun gl-bind-texture (texture)
  (c-with ((texw :float)
           (texh :float))
    (check-rc (sdl-gl-bind-texture texture (texw &) (texh &)))
    (values texw texh)))

(defun gl-unbind-texture (texture)
  (check-rc (sdl-gl-unbind-texture texture)))
