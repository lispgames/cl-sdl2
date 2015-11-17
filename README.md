# sdl2

`cl-sdl2` is a Common Lisp wrapper for the SDL 2.0 C Library, with [many contributors](https://github.com/lispgames/cl-sdl2/graphs/contributors), maintained primarily by the following:

* Chip Collier <photex@lofidelitygames.com>
* Ryan Pavlik <rpavlik@gmail.com>
* Peter Keller <psilord@cs.wisc.edu>

It is licensed under the [MIT license](https://opensource.org/licenses/MIT).

# Installation

sdl2 is in Quicklisp, see below for instructions.

## SDL 2.0 C Library Install

On Linux, you can probably find SDL2 in your distribution's package
set.  For other platforms, or for building manually, [download the
source](http://www.libsdl.org/download-2.0.php).

If you need to compile from source for your Linux platform:

  0. Download the source tar-ball.
  0. tar zxf SDL2-0.0.0.tar.gz
  0. ./configure
  0. make
  0. sudo make install

This will install the SDL-2.0.3 C Library into your /usr/local location.

It's generally a good idea to install at a minimum the version of SDL2
that was wrapped; however, sub revisions should not introduce binary
incompatibility and should be fine.  If you install a different
version, certain features may not be available or may not work
correctly.

## Quicklisp Install

sdl2 is best installed via QuickLisp, though for cutting-edge changes,
you may want to install from github as below.

If you don't have Quicklisp, then follow [the
directions](http://www.quicklisp.org/beta/) to install it. We assume
you placed the Quicklisp repository in the default place as indicated
by the directions and have added it to your lisp init file.

## github install

  0. cd $HOME/quicklisp/local-projects
  0. git clone https://github.com/rpav/cl-autowrap.git
  0. git clone https://github.com/lispgames/cl-sdl2.git

Then, use quicklisp to install the libraries required by cl-sdl2:

Start your lisp. Then, just:

```lisp
(ql:quickload "sdl2")
```

# Running the sdl2 examples

Start your lisp:

```lisp
(asdf:load-system :sdl2-examples)
(sdl2-examples:basic-test)
```

This example will open a window with an opengl primitive in it. Any mouse
movements or keystrokes are recorded in the terminal (or emacs SLIME output
buffer). Hitting the ESCAPE key will terminate the example.

Thank you for using sdl2!
