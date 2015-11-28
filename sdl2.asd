;;;; sdl2.asd

(defsystem #:sdl2
  :serial t
  :description "Bindings for SDL2 using c2ffi."
  :author "Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>, Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"

  :depends-on (:alexandria
               :cl-autowrap
               :cl-plus-c
               :trivial-garbage
               :trivial-channels
               #+darwin :cl-glut)
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
   (:file "audio")
   (:file "platform")
   (:file "pixels")
   (:file "surface")
   (:file "render"
          :depends-on ("rect"))))

(defsystem #:sdl2/examples
  :description "simple examples to demonstrate common usage of sdl2."
  :author "Chip Collier <photex@lofidelitygames.com>"
  :license "MIT"
  :depends-on (:sdl2
               :cl-opengl
               :cl-ppcre)
  :pathname "examples"
  :serial t

  :components ((:file "basic")
               (:file "renderer")))
