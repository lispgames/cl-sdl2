(in-package :sdl2)

;; A start to the SDL2 audio interface.

;; These are #defines, but really should have been enums....

(autowrap:define-enum-from-constants (sdl-audio-mask)
  sdl2-ffi:+sdl-audio-mask-bitsize+
  sdl2-ffi:+sdl-audio-mask-datatype+
  sdl2-ffi:+sdl-audio-mask-endian+
  sdl2-ffi:+sdl-audio-mask-signed+)

(autowrap:define-enum-from-constants (sdl-audio)
  sdl2-ffi:+audio-u8+
  sdl2-ffi:+audio-s8+
  sdl2-ffi:+audio-u16lsb+
  sdl2-ffi:+audio-s16lsb+
  sdl2-ffi:+audio-u16msb+
  sdl2-ffi:+audio-s16msb+
  sdl2-ffi:+audio-u16+
  sdl2-ffi:+audio-s16+
  sdl2-ffi:+audio-s32lsb+
  sdl2-ffi:+audio-s32msb+
  sdl2-ffi:+audio-s32+
  sdl2-ffi:+audio-f32lsb+
  sdl2-ffi:+audio-f32msb+
  sdl2-ffi:+audio-f32+
  sdl2-ffi:+audio-u16sys+
  sdl2-ffi:+audio-s16sys+
  sdl2-ffi:+audio-s32sys+
  sdl2-ffi:+audio-f32sys+)

(autowrap:define-enum-from-constants (sdl-audio-allow)
  sdl2-ffi:+sdl-audio-allow-frequency-change+
  sdl2-ffi:+sdl-audio-allow-format-change+
  sdl2-ffi:+sdl-audio-allow-channels-change+
  sdl2-ffi:+sdl-audio-allow-any-change+)


(defun audio-bitsize (x)
  (logand x (enum-value 'sdl-audio-mask :bitsize)))

(defun audio-float-p (x)
  (logand x (enum-value 'sdl-audio-mask :datatype)))

(defun audio-big-endian-p (x)
  (logand x (enum-value 'sdl-audio-mask :endian)))

(defun audio-signed-p (x)
  (logand x (enum-value 'sdl-audio-mask :signed)))

(defun audio-int-p (x)
  (not (audio-float-p x)))

(defun audio-little-endian-p (x)
  (not (audio-big-endian-p x)))

(defun audio-unsigned-p (x)
  (not (audio-signed-p x)))

;; TODO, everything else. :)
