;;;; sdl2.asd

(asdf:defsystem #:sdl2
  :serial t
  :description "Bindings for SDL2 using c2ffi."
  :author "Chip Collier <photex@lofidelitygames.com>"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi :c2ffi-cffi)
  :components ((c2ffi-cffi:spec "sdl2-cffi"
                                :exclude-sources ("/usr/local/lib/clang/.*"
                                                  "/usr/include/(?!stdint.h).*")
                                :exclude-definitions ("SDL_Log"
                                                      "SDL_LogMessageV"
                                                      "SDL_vsnprintf"))
               (c2ffi-cffi:spec "sdl2-macros")
               (:file "src/package")
               (:file "src/sdl2")))

