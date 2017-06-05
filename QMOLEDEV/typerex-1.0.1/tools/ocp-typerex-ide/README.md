**************************************************************************
*                                                                        *
*                        TypeRex OCaml Studio                            *
*                                                                        *
*                 Thomas Gazagnaire, Fabrice Le Fessant                  *
*                                                                        *
*  Copyright 2011-2012 OCamlPro                                          *
*  All rights reserved.  This file is distributed under the terms of     *
*  the GNU Public License version 3.0.                                   *
*                                                                        *
*  TypeRex is distributed in the hope that it will be useful,            *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
*  GNU General Public License for more details.                          *
*                                                                        *
**************************************************************************

## Install instructions

* Add `~/bin` to your `$PATH` variable; and

* Add to you `.emacs` files :

        (load "~/.emacs.d/typerex.el")
        (add-hook 'tuareg-mode-hook 'typerex)

* In the `tools/ocp-ide` directory, write :

        make -f Makefile.install install

## Owner

Thomas <thomas@ocamlpro.com>
