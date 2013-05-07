;;;; sdl2.asd

(asdf:defsystem #:sdl2
  :serial t
  :description "Bindings for SDL2 using c2ffi."
  :author "Chip Collier <photex@lofidelitygames.com>"
  :license "MIT"
  :depends-on (:cffi :cffi-libffi :c2ffi-cffi)
  :components ((:file "src/package")
               (:file "src/sdl2")))

