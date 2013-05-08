;;;; sdl2.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :c2ffi-cffi))

(asdf:defsystem #:sdl2
  :serial t
  :description "Bindings for SDL2 using c2ffi."
  :author "Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>"
  :license "MIT"

  :depends-on (:alexandria :cffi :cffi-libffi :c2ffi-cffi :cl-ppcre)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "library")
   (c2ffi-cffi:spec "sdl2-cffi"
                    :package :sdl2-ffi
                    :exclude-sources ("/usr/local/lib/clang/.*"
                                      "/usr/include/(?!stdint.h|bits/types.h|sys/types.h).*")
                    :exclude-definitions ("SDL_Log"
                                          "SDL_LogMessageV"
                                          "SDL_vsnprintf"))
   (c2ffi-cffi:spec "sdl2-macros"
                    :package :sdl2-ffi)
   (:file "util")
   (:file "sdl2")
   (:file "window")))

