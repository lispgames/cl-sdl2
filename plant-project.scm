((#:quickloads
  "swank"
  "alexandria"
  "cl-fad"
  "cl-ppcre"
  "trivial-garbage"
  "cl-opengl"
  "optima"
  "cl-autowrap"
  "trivial-channels")
 (#:project-name . "cl-sdl2")
 (#:project-version . "0.0.1")
 (#:dependencies
  ((#:method . "git")
   (#:url
    .
    "git://github.com/rpav/trivial-channels.git"))
  ((#:method . "git")
   (#:url . "git://github.com/rpav/cl-autowrap.git"))))
