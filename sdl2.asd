;;;; sdl2.asd

(asdf:defsystem #:sdl2
  :serial t
  :description "Bindings for SDL2 using c2ffi."
  :author "Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>, Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"

  :depends-on (:alexandria
               :cl-autowrap
               :cl-plus-c
               :cl-ppcre
               :trivial-garbage
               :trivial-channels
               :trivial-features
               #+darwin :cl-glut)
  :pathname "src"
  :serial t

  :components
  ((:module autowrap-spec
    :pathname "spec"
    :components
    ((:static-file "SDL2.h")
     (:static-file "SDL2.arm-pc-linux-gnu.spec")
     (:static-file "SDL2.i386-unknown-freebsd.spec")
     (:static-file "SDL2.i386-unknown-openbsd.spec")
     (:static-file "SDL2.i686-apple-darwin9.spec")
     (:static-file "SDL2.i686-pc-linux-gnu.spec")
     (:static-file "SDL2.i686-pc-windows-msvc.spec")
     (:static-file "SDL2.x86_64-apple-darwin9.spec")
     (:static-file "SDL2.x86_64-pc-linux-gnu.spec")
     (:static-file "SDL2.x86_64-pc-windows-msvc.spec")
     (:static-file "SDL2.x86_64-unknown-freebsd.spec")
     (:static-file "SDL2.x86_64-unknown-openbsd.spec")))
   (:file "package")
   (:file "optional-deps")
   (:file "library")
   (:file "autowrap")
   (:file "util")
   (:file "sdl2")
   (:file "rect")
   (:file "video")
   (:file "events")
   (:file "keyboard")
   (:file "mouse")
   (:file "syswm")
   (:file "joystick")
   (:file "gamecontroller")
   (:file "haptic")
   (:file "timer")
   (:file "audio")
   (:file "platform")
   (:file "pixels")
   (:file "surface")
   (:file "rwops")
   (:file "render"
          :depends-on ("rect"))))

(asdf:defsystem #:sdl2/examples
  :description "simple examples to demonstrate common usage of sdl2."
  :author "Chip Collier <photex@lofidelitygames.com>"
  :license "MIT"
  :depends-on (:sdl2 :cl-opengl)
  :pathname "examples"
  :serial t

  :components ((:file "basic")
               (:file "renderer")))
