;;;; sdl2.asd

(asdf:defsystem #:sdl2
  :serial t
  :description "Bindings for SDL2 using c2ffi."
  :author "Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>, Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"

  :depends-on (:alexandria :cl-autowrap :cl-ppcre :trivial-garbage :cl-opengl)
  :pathname "src"
  :serial t

  :components
  ((:module autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "SDL2.h")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "util")
   (:file "sdl2")
   (:file "video")
   (:file "events")
   (:file "keyboard")
   (:file "mouse")
   (:file "joystick")
   (:file "gamecontroller")
   (:file "haptic")
   (:file "timer")
   (:file "rect")
   (:file "platform")))

(asdf:defsystem #:sdl2-examples
  :description "simple examples to demonstrate common usage of sdl2."
  :author "Chip Collier <photex@lofidelitygames.com>"
  :license "MIT"
  :depends-on (:sdl2)
  :pathname "examples"
  :serial t

  :components ((:file "basic")))

