(in-package #:sdl2)

(autowrap:define-enum-from-constants (sdl-pixel-format)
  sdl2-ffi:+sdl-pixelformat-unknown+
  sdl2-ffi:+sdl-pixelformat-index1lsb+
  sdl2-ffi:+sdl-pixelformat-index1msb+
  sdl2-ffi:+sdl-pixelformat-index4lsb+
  sdl2-ffi:+sdl-pixelformat-index4msb+
  sdl2-ffi:+sdl-pixelformat-index8+
  sdl2-ffi:+sdl-pixelformat-rgb332+
  sdl2-ffi:+sdl-pixelformat-rgb444+
  sdl2-ffi:+sdl-pixelformat-rgb555+
  sdl2-ffi:+sdl-pixelformat-bgr555+
  sdl2-ffi:+sdl-pixelformat-argb4444+
  sdl2-ffi:+sdl-pixelformat-rgba4444+
  sdl2-ffi:+sdl-pixelformat-abgr4444+
  sdl2-ffi:+sdl-pixelformat-bgra4444+
  sdl2-ffi:+sdl-pixelformat-argb1555+
  sdl2-ffi:+sdl-pixelformat-rgba5551+
  sdl2-ffi:+sdl-pixelformat-abgr1555+
  sdl2-ffi:+sdl-pixelformat-bgra5551+
  sdl2-ffi:+sdl-pixelformat-rgb565+
  sdl2-ffi:+sdl-pixelformat-bgr565+
  sdl2-ffi:+sdl-pixelformat-rgb24+
  sdl2-ffi:+sdl-pixelformat-bgr24+
  sdl2-ffi:+sdl-pixelformat-rgb888+
  sdl2-ffi:+sdl-pixelformat-rgbx8888+
  sdl2-ffi:+sdl-pixelformat-bgr888+
  sdl2-ffi:+sdl-pixelformat-bgrx8888+
  sdl2-ffi:+sdl-pixelformat-argb8888+
  sdl2-ffi:+sdl-pixelformat-rgba8888+
  sdl2-ffi:+sdl-pixelformat-abgr8888+
  sdl2-ffi:+sdl-pixelformat-bgra8888+
  sdl2-ffi:+sdl-pixelformat-argb2101010+
  sdl2-ffi:+sdl-pixelformat-yv12+
  sdl2-ffi:+sdl-pixelformat-iyuv+
  sdl2-ffi:+sdl-pixelformat-yuy2+
  sdl2-ffi:+sdl-pixelformat-uyvy+
  sdl2-ffi:+sdl-pixelformat-yvyu+)

(autowrap:define-enum-from-constants (sdl-pixel-type)
  sdl2-ffi:+sdl-pixeltype-unknown+
  sdl2-ffi:+sdl-pixeltype-index1+
  sdl2-ffi:+sdl-pixeltype-index4+
  sdl2-ffi:+sdl-pixeltype-index8+
  sdl2-ffi:+sdl-pixeltype-packed8+
  sdl2-ffi:+sdl-pixeltype-packed16+
  sdl2-ffi:+sdl-pixeltype-packed32+
  sdl2-ffi:+sdl-pixeltype-arrayu8+
  sdl2-ffi:+sdl-pixeltype-arrayu16+
  sdl2-ffi:+sdl-pixeltype-arrayu32+
  sdl2-ffi:+sdl-pixeltype-arrayf16+
  sdl2-ffi:+sdl-pixeltype-arrayf32+)

(autowrap:define-enum-from-constants (sdl-bitmap-order)
  sdl2-ffi:+sdl-bitmaporder-none+
  sdl2-ffi:+sdl-bitmaporder-4321+
  sdl2-ffi:+sdl-bitmaporder-1234+)

(autowrap:define-enum-from-constants (sdl-packed-order)
  sdl2-ffi:+sdl-packedorder-none+
  sdl2-ffi:+sdl-packedorder-xrgb+
  sdl2-ffi:+sdl-packedorder-rgbx+
  sdl2-ffi:+sdl-packedorder-argb+
  sdl2-ffi:+sdl-packedorder-rgba+
  sdl2-ffi:+sdl-packedorder-xbgr+
  sdl2-ffi:+sdl-packedorder-bgrx+
  sdl2-ffi:+sdl-packedorder-abgr+
  sdl2-ffi:+sdl-packedorder-bgra+)

(autowrap:define-enum-from-constants (sdl-array-order)
  sdl2-ffi:+sdl-arrayorder-none+
  sdl2-ffi:+sdl-arrayorder-rgb+
  sdl2-ffi:+sdl-arrayorder-rgba+
  sdl2-ffi:+sdl-arrayorder-argb+
  sdl2-ffi:+sdl-arrayorder-bgr+
  sdl2-ffi:+sdl-arrayorder-bgra+
  sdl2-ffi:+sdl-arrayorder-abgr+)

(autowrap:define-enum-from-constants (sdl-packed-layout)
  sdl2-ffi:+sdl-packedlayout-none+
  sdl2-ffi:+sdl-packedlayout-332+
  sdl2-ffi:+sdl-packedlayout-4444+
  sdl2-ffi:+sdl-packedlayout-1555+
  sdl2-ffi:+sdl-packedlayout-5551+
  sdl2-ffi:+sdl-packedlayout-565+
  sdl2-ffi:+sdl-packedlayout-8888+
  sdl2-ffi:+sdl-packedlayout-2101010+
  sdl2-ffi:+sdl-packedlayout-1010102+)

(defun map-rgb (pixel-format r g b)
  (sdl-map-rgb pixel-format r g b))

(defun get-pixel-format-name (format-integer)
  "Returns the human readable name for a surface's pixel format, useful for debugging."
  (sdl2-ffi.functions:sdl-get-pixel-format-name format-integer))
