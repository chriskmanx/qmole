**************************************************************************
*                                                                        *
*    TypeRex OCaml Studio                                                *
*      Thomas Gazagnaire, Fabrice Le Fessant                             *
*                                                                        *
*    OCaml                                                               *
*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *
*                                                                        *
*  Copyright 2011-2012 OCamlPro                                          *
*  Copyright 1996-2011 INRIA.                                            *
*  All rights reserved.  This file is distributed under the terms of     *
*  the GNU Public License version 3.0.                                   *
*                                                                        *
*  TypeRex is distributed in the hope that it will be useful,            *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
*  GNU General Public License for more details.                          *
*                                                                        *
**************************************************************************

				ocp-type
				========

Uses the same options as "ocamlc", but does not generate any
code. Used only to type and generate .cmt/.cmti files which contain
dumped typedtrees.


Using ocp-type:
===============


Simple way: ocp-wrapper
~~~~~~~~~~~~~~~~~~~~~~~

Setup your build process to prefix all compilations commands by
"ocp-wrapper -save-types -save-last-compiled". For example if you are
using make, this can be achieved by writing something like

        OCAMLC=ocp-ocamlc.opt
or
        OCAMLC=ocp-wrapper -with-ocp-type <path/to/ocp-type> -save-types ocamlc.opt

and similarly for ocamlopt.


Separate Makefile
~~~~~~~~~~~~~~~~~

Alternatively, ocp-type provides a file Makefile.ocp-type.template,
which is able to perform the ocp-type compilation automatically for
simple projects: you just have to

  - copy it in your project's root directory and rename it to
    Makefile.ocp-type
  - set the configuration variables appropriately
  - make -f Makefile.ocp-type

For example, the Makefile.ocp-type in this directory can be used to
type ocp-type itself using ocp-type.
