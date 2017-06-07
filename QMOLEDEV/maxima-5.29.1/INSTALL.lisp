Maxima can be built using a purely Lisp-based procedure.
This procedure is not yet as polished as the GNU Autotools system
described in the file INSTALL.
However, it may be more convenient on a system (e.g., Windows)
which does not have the GNU Autotools installed.

User feedback on this procedure would be greatly appreciated.

Note: xmaxima cannot be built using this procedure.

Note (2): Plotting on Windows does not (yet) work using this procedure.


To build Maxima:

(0) cd to the top-level Maxima directory (i.e., the directory 
    which contains src/, tests/, share/, and other directories).

(1) Launch your Lisp implementation.

(2) Load the file configure.lisp:

    (load "configure.lisp")

(3) Generate configuration files:

    (configure)

    You will be prompted for several inputs.
    Press carriage return to accept the default values.

    The configure process can be automated through the use
    of optional arguments to configure.
    See the file configure.lisp for details.

(4) Quit Lisp,
    
    (quit)

    and cd to the directory src/.

(4.1) GCL only: Create these directories if they do not already exist:

    binary-gcl
    binary-gcl/numerical
    binary-gcl/numerical/slatec

Maxima builds with defsystem. The file maxima-build.lisp is provided
for rudimentary guidance in using defsystem. Experts should feel free
to subsitute their knowledge of defsystem for the following steps.

(5) Restart Lisp, and load maxima-build.lisp:

    (load "maxima-build.lisp")

(6) Compile the Lisp source code:

    (maxima-compile)

    Clisp only: Clisp complains about SETQ applied to a symbol in
    a locked package when compiling src/cpoly.lisp.
    When Clisp asks you if you want to allow it, enter "continue" (without quote marks).

(7) Quit Lisp, and restart Lisp.

(8) Load the compiled Lisp files:

    (load "maxima-build.lisp")
    (maxima-load)

(9a) Run Maxima from the loaded image.

    (cl-user::run)

    That should bring up the Maxima input prompt.

(9b) Dump the image, and if the Lisp implementation allows one to
    specify a start-up function, specify USER::RUN.

    There is a function MAXIMA-DUMP in src/maxima-build.lisp to dump an image.
    At present it works for Clisp, SBCL, GCL, CMUCL, Scieneer, and Allegro.
    Reinhard Oldenburg writes, in reference to Lispworks:
    "(maxima-dump) works when threading is disabled."
    Some Lisp implementations (SBCL, GCL, CMUCL, Scieneer, maybe others)
    terminate after saving the image.

    (maxima-dump)

(10) Execute the saved image.

    Each Lisp implementation allows one to specify the name of the
    image to be executed in a slightly different way.
    Two scripts, maxima and maxima.bat,
    are provided to specify the command line options appropriately.

    Unix systems and Windows with Bourne shell:

    sh maxima
     --- or ---
    chmod a+x maxima
    ./maxima

    (Even if Bourne shell is not available on your system,
    it is worth looking at the way images are invoked at the end of the script.)

    Windows without Bourne shell:

    maxima.bat

(11) Test the build. At the Maxima prompt, enter:

    run_testsuite();

