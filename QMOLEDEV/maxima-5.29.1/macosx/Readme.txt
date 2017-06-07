MAXIMA.APP
===========

Quick instructions for building a Maxima.app on Mac OS X.

Platypus
---------

The package is built with the Platypus application. Install Platypus
from http://sveinbjorn.org/platypus. You also need to install the
command line tools (start the Platypus application and install command
line tools from the Preferences dialog).

Maxima.app
-----------

The default configuration uses sbcl which is installed as
/usr/local/bin/sbcl, but other lisps can be used too. It creates a
Maxima.app on the Desktop.

To create Maxima.app with default settings execute

   make -f macosx/Makefile

To use a different lisp and set the version execute

    make -f macosx/Makefile VERSION=5.24.0 LISP_NAME=cmucl \
      LISP_PROGRAM=/usr/local/bin/lisp

Maxima will be configured with correct options, built and installed
into Maxima.app. Some parts of Maxima.app are created with Platypus.

Running Maxima
---------------

Double-cliking Maxima.app starts maxima in a new Terminal window.

To run maxima from Terminal, execute
Maxima.app/Contents/Resources/maxima.sh
