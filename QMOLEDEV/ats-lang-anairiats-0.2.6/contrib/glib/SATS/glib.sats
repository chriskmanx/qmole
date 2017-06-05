(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010
//
(* ****** ****** *)

%{#
#include "contrib/glib/CATS/glib.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

macdef GLIB_MAJOR_VERSION =
  $extval (int, "GLIB_MAJOR_VERSION")
macdef GLIB_MINOR_VERSION =
  $extval (int, "GLIB_MINOR_VERSION")
macdef GLIB_MICRO_VERSION =
  $extval (int, "GLIB_MICRO_VERSION")

macdef GLIB_VERSION =
  1000 * (1000 * GLIB_MAJOR_VERSION + GLIB_MINOR_VERSION) + GLIB_MICRO_VERSION
// end of [GLIB_VERSION]

(* ****** ****** *)

#include "contrib/glib/SATS/glib/gtypes.sats"

(* ****** ****** *)

#include "contrib/glib/SATS/glib/gbasics.sats"

(* ****** ****** *)

#include "contrib/glib/SATS/glib/gmem.sats"

(* ****** ****** *)

#include "contrib/glib/SATS/glib/garray.sats"
#include "contrib/glib/SATS/glib/ghash.sats"
#include "contrib/glib/SATS/glib/glist.sats"
#include "contrib/glib/SATS/glib/gqsort.sats"
#include "contrib/glib/SATS/glib/grand.sats"
#include "contrib/glib/SATS/glib/gslist.sats"
#include "contrib/glib/SATS/glib/gstring.sats"
#include "contrib/glib/SATS/glib/gutils.sats"

(* ****** ****** *)

(* end of [glib.sats] *)

////

glib:

galloca.h	 gdate.h       gmacros.h      gqsort.h	  gstrfuncs.h
garray.h	 gdir.h        gmain.h	      gquark.h	  gstring.h
gasyncqueue.h	 gerror.h      gmappedfile.h  gqueue.h	  gthread.h
gatomic.h	 gfileutils.h  gmarkup.h      grand.h	  gthreadpool.h
gbacktrace.h	 ghash.h       gmem.h	      grel.h	  gtimer.h
gbase64.h	 ghook.h       gmessages.h    gscanner.h  gtree.h
gbookmarkfile.h  gi18n-lib.h   gnode.h	      gshell.h	  gtypes.h
gcache.h	 gi18n.h       goption.h      gslice.h	  gunicode.h
gcompletion.h	 giochannel.h  gpattern.h     gslist.h	  gutils.h
gconvert.h	 gkeyfile.h    gprimes.h      gspawn.h	  gwin32.h
gdataset.h	 glist.h       gprintf.h      gstdio.h

gobject:

gboxed.h    gobjectalias.h    gtype.h	     gvaluecollector.h
gclosure.h  gparam.h	      gtypemodule.h  gvaluetypes.h
genums.h    gparamspecs.h     gtypeplugin.h  stamp-gmarshal.h
gmarshal.h  gsignal.h	      gvalue.h
gobject.h   gsourceclosure.h  gvaluearray.h
