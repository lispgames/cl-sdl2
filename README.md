# cl-sdl2

`cl-sdl2` is a Common Lisp wrapper for the SDL 2.0 C Library, with [many contributors](https://github.com/lispgames/cl-sdl2/graphs/contributors), currently maintained by Michael Fiano <mail@mfiano.net>.

It is licensed under the [MIT license](https://opensource.org/licenses/MIT).

# Installation

sdl2 is in Quicklisp, see below for instructions.

## SDL 2.0 C Library Install
See https://wiki.libsdl.org/Installation

On Linux, you can probably find SDL2 in your distribution's package
set.  For other platforms, or for building manually, [download the
source](http://www.libsdl.org/download-2.0.php).

### Package
* Debian based: Ubuntu, Mint etc
```bash
sudo apt-get install libsdl2-2.0
```
* Arch
```bash
sudo pacman -S sdl2
```

### Compilation

If you need to compile from source for your Linux platform:

1. Download [source code](https://www.libsdl.org/download-2.0.php)
2. Compile
3. Install

For example:
```bash
cd /tmp
wget https://www.libsdl.org/release/SDL2-2.0.4.tar.gz
tar -xzvf SDL2-2.?.?.tar.gz
cd SDL2-2.?.?
./configure
make
sudo make install
```

This will install the SDL-2.0.x C Library into your /usr/local location.

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
```bash
cd $HOME/quicklisp/local-projects
git clone https://github.com/rpav/cl-autowrap.git
git clone https://github.com/lispgames/cl-sdl2.git
```

Then, use quicklisp to install the libraries required by cl-sdl2:

Start your lisp. Then, just:

```lisp
(ql:quickload "sdl2")
```

## Swank/Slynk features

sdl2 enables certain restarts for friendly interaction with SLIME or
Sly if you have either properly installed.  "Proper installation" in
this case means `swank.asd` or `slynk.asd` is linked such that ASDF
can find and load it.

Note this is easily achieved even if you have installed them from
github or some other non-Quicklisp repository:

* Symlink the directory to `$HOME/quicklisp/local-projects/`
* Symlink the `.asd` to `$HOME/.local/common-lisp/sources/`

Similarly you could just clone into `~/quicklisp/local-projects` as
well; this should work on Windows as well.  There are numerous other
options for configuring and managing ASDs, as well.

# Running the sdl2 examples

Start your lisp:

```lisp
(ql:quickload :sdl2/examples)
(sdl2-examples:basic-test)
```

This example will open a window with an opengl primitive in it. Any mouse
movements or keystrokes are recorded in the terminal (or emacs SLIME output
buffer ```*inferior-lisp*```). Hitting the ESCAPE key will terminate the example.

## OSX

Newer versions of OSX have had some difficulties as calls which require
`nextEventMatchingMask` must be called from the main thread of your program.

This is especially relevant to SBCL, although issues have also been noticed in CCL.

Currently, initialisation must take place on your main thread:

```lisp
(ql:quickload :sdl2/examples)

;; We should be able to run examples as normal on CCL
#-sbcl (sdl2-examples:basic-test)

;; SBCL requires that we initialise in the main thread
#+sbcl (sdl2:make-this-thread-main #'sdl2-examples:basic-test)
```

Thank you for using sdl2!
