#+swank
(when (asdf:find-system "swank")
  (asdf:load-system "swank")
  (pushnew 'sdl2::sdl2-swank *features*))

#+slynk
(when (asdf:find-system "slynk")
  (asdf:load-system "slynk")
  (pushnew 'sdl2::sdl2-slynk *features*))
