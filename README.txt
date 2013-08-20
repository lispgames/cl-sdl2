sdl2: A Common Lisp Wrapper for the SDL 2.0 C Library.

Written by
==========

    Chip Collier <photex@lofidelitygames.com>
    Ryan Pavlik <rpavlik@gmail.com>

License
=======

    MIT

Installation Instructions when using Quicklisp, SBCL, and Linux
===============================================================

    sdl2 is not yet in Quicklisp, but it is very easy to integrate with an
    existing Quicklisp install in one's home directory.

    SDL 2.0 C Library Install
    -------------------------

    The official version of the C SDL 2.0 library was recently released and
    might not yet be available in the package repositories of your Linux
    distribution. The source release can be found here:

    http://www.libsdl.org/download-2.0.php

    If you need to compile from source for your Linux platform:

    1. Download the source tar-ball.
    2. tar zxf SDL2-2.0.0.tar.gz
    3. ./configure
    4. make
    5. sudo make install

    This will install the SDL-2.0.0 C Library into your /usr/local location.
    
    Quicklisp Install
    -----------------

    sdl2 requires a Quicklisp install that distributes cl-autowrap such as
    Quicklisp distributions of August 2013 or later.

    If you don't have Quicklisp, then follow the directions here:

    http://www.quicklisp.org/beta/

    to install it. We assume you placed the Quicklisp repository in the default
    place as indicated by the directions and have added it to your lisp init
    file.

    sdl2 Install
    ------------

        1. cd $HOME/quicklisp/local-projects
        2. git clone https://github.com/lispgames/cl-sdl2.git

    Then, use quicklisp to install the libraries required by cl-sdl2:

    Start your lisp. You will only have to do this once for Quicklisp to
    download the required libraries.

    (ql:quickload "alexandria")
    (ql:quickload "cl-autowrap")
    (ql:quickload "cl-ppcre")
    (ql:quickload "trivial-garbage")
    (ql:quickload "cl-opengl")

    Then, to load the sdl2 system into the REPL:

    (asdf:load-system :sdl2)

Running the sdl2 examples
=========================

    Start your lisp:

    (asdf:load-system :sdl2)
    (asdf:load-system :sdl2-examples)

    (sdl2-examples::basic-test)

    This example will open a window with an opengl primitive in it. Any mouse
    movements or keystrokes are recorded in the terminal (or emacs SLIME output
    buffer). Hitting the ESCAPE key will terminate the example.


Thank you for using sdl2!
