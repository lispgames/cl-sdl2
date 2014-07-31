(in-package :sdl2)

;;;; TODO
;;;; SDL_CreateWindowAndRenderer
;;;; SDL_GetRendererOutputSize

(defun make-renderer-info ()
  "Return an uninitialized SDL_RendererInfo structure."
  (sdl-collect (autowrap:alloc 'sdl2-ffi:sdl-renderer-info)))

(defmethod print-object ((rinfo sdl2-ffi:sdl-renderer-info) stream)
  (c-let ((rinfo sdl2-ffi:sdl-renderer-info :from rinfo))
    (print-unreadable-object (rinfo stream :type t :identity t)
      (format stream "name ~S flags ~A num-texture-formats ~A texture-formats TBD max-texture-width ~A max-texture-height ~A"
              (rinfo :name)
              (rinfo :flags)
              (rinfo :num-texture-formats)
              (rinfo :max-texture-width)
              (rinfo :max-texture-height)))))

(defun free-render-info (rinfo)
  "Specifically free the SDL_RendererInfo structure which will do the right
thing with respect to the garbage collector. This is not required, but
may make garbage collection performance better if used in tight
SDL_RendererInfo allocating loops."
  (foreign-free (ptr rinfo))
  (sdl-cancel-collect rinfo)
  (autowrap:invalidate rinfo))

;;;; And now the wrapping of the SDL2 calls

;; Create the keywords for the SDL_RendererFLags enum.
(autowrap:define-bitmask-from-enum
    (sdl-renderer-flags sdl2-ffi:sdl-renderer-flags))

;; Create the keywords for the SDL_TextureModulate enum.
(autowrap:define-bitmask-from-enum
    (sdl-texture-modulate sdl2-ffi:sdl-texture-modulate))

;; Create the keywords for the SDL_RendererFlip enum.
(autowrap:define-bitmask-from-enum
    (sdl-renderer-flip sdl2-ffi:sdl-renderer-flip))

(defun get-num-render-drivers ()
  "Return the number of 2D rendering drivers available for the current
display."
  (sdl-get-num-render-drivers))

(defun get-render-driver-info (index)
  "Allocate and return a new SDL_RendererInfo structure and fill it
with information relating to the specific 2D rendering driver
specified in the index."
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
    (let ((window
            (sdl-collect
             (sdl2-ffi::make-sdl-window :ptr winptr)
             (lambda (w) (sdl-destroy-window w))))
          (renderer
            (sdl-collect
             (sdl2-ffi::make-sdl-renderer :ptr rendptr)
             (lambda (r) (sdl-destroy-renderer r)))))
      (values window renderer))))

(defun create-renderer (window index &optional flags)
  "Create a 2D rendering context for a window."
  (sdl-collect
   (check-null (sdl-create-renderer
                window index
                (mask-apply 'sdl-renderer-flags flags)))
   (lambda (r) (sdl-destroy-renderer r))))

(defun create-software-renderer (surface)
  "Create and return a 2D software rendering context for the surface."
  (check-null (sdl-create-software-renderer surface)))

(defun destroy-renderer (r)
  (sdl-cancel-collect r)
  (sdl-destroy-renderer r)
  (invalidate r))

(defun get-renderer (window)
  "Return NIL if there is no renderer associated with the window, or otherwise
the SDL_Renderer structure."
  (let ((renderer (sdl-get-renderer window)))
    (if (null-pointer-p (autowrap:ptr renderer))
        nil
        renderer)))

(defun get-renderer-info (renderer)
  "Allocate a new SDL_RendererInfo structure, fill it in with information
about the specified renderer, and return it."
  (let ((rinfo (make-renderer-info)))
    (check-rc (sdl-get-renderer-info renderer rinfo))
    rinfo))

;; TODO SDL_GetRendererOutputSize
(defun get-renderer-output-size (renderer)
  (niy "SDL_GetRendererOutputSize()"))

(defun create-texture (renderer pixel-format access width height)
  (sdl-collect
   (check-null (sdl-create-texture renderer
                                   (enum-value 'sdl-pixel-format pixel-format)
                                   (enum-value 'sdl-texture-access access)
                                   width height))
   (lambda (tex) (sdl-destroy-texture tex))))

(defun destroy-texture (texture)
  (sdl-cancel-collect texture)
  (sdl-destroy-texture texture)
  (invalidate texture))

(defun lock-texture (texture &optional rect)
  (c-let ((pixels :pointer :free t)
          (pitch :int :free t))
    (check-rc (sdl-lock-texture texture rect (pixels &) (pitch &)))
    (values pixels pitch)))

(defun unlock-texture (texture)
  (sdl-unlock-texture texture))

(defun gl-bind-texture (texture)
  (c-with ((texw :float)
           (texh :float))
    (check-rc (sdl-gl-bind-texture texture (texw &) (texh &)))
    (values texw texh)))

(defun gl-unbind-texture (texture)
  (check-rc (sdl-gl-unbind-texture texture)))
