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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "libc/CATS/termios.cats"
%} // end of [%{#]

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"
typedef pid_t = $TYPES.pid_t

(* ****** ****** *)

sta NCCS: int // = 32?
//
abst@ype cc_t = $extype"ats_cc_type"
castfn char_of_cc (x: cc_t):<> char
castfn cc_of_char (x: char):<> cc_t
//
abst@ype tcflag_t = $extype"ats_tcflag_type"
castfn uint_of_tcflag (x: tcflag_t):<> uint
castfn tcflag_of_uint (x: uint):<> tcflag_t
//
abst@ype speed_t = $extype"ats_speed_type"
castfn speed_of_uint (x: uint):<> speed_t
castfn uint_of_speed (x: speed_t):<> uint
//
abst@ype termios_rest
typedef termios_struct =
$extype_struct
"ats_termios_type" of {
  c_iflag= tcflag_t
, c_oflag= tcflag_t
, c_cflag= tcflag_t
, c_lflag= tcflag_t
, c_line= cc_t
, c_cc= @[cc_t][NCCS]
(*
, c_ispeed= speed_t // not documented
, c_ospeed= speed_t // not documented
*)
, _rest= undefined_t // unknown quantity
} // end of [termios_struct]
typedef termios = termios_struct

(* ****** ****** *)

(*
** c_cc characters
*)
macdef VINTR = $extval (cc_t, "VINTR")
macdef VQUIT = $extval (cc_t, "VQUIT")
macdef VERASE = $extval (cc_t, "VERASE")
macdef VKILL = $extval (cc_t, "VKILL")
macdef VEOF = $extval (cc_t, "VEOF")
macdef VTIME = $extval (cc_t, "VTIME")
macdef VMIN = $extval (cc_t, "VMIN")
macdef VSWTC = $extval (cc_t, "VSWTC")
macdef VSTART = $extval (cc_t, "VSTART")
macdef VSTOP = $extval (cc_t, "VSTOP")
macdef VSUSP = $extval (cc_t, "VSUSP")
macdef VEOL = $extval (cc_t, "VEOL")
macdef VREPRINT = $extval (cc_t, "VREPRINT")
macdef VDISCARD = $extval (cc_t, "VDISCARD")
macdef VWERASE = $extval (cc_t, "VWERASE")
macdef VLNEXT = $extval (cc_t, "VLNEXT")
macdef VEOL2 = $extval (cc_t, "VEOL2")

(* ****** ****** *)

(*
** c_iflag bits
*)
macdef IGNBRK = $extval (tcflag_t, "IGNBRK")
macdef BRKINT = $extval (tcflag_t, "BRKINT")
macdef IGNPAR = $extval (tcflag_t, "IGNPAR")
macdef PARMRK = $extval (tcflag_t, "PARMRK")
macdef INPCK = $extval (tcflag_t, "INPCK")
macdef ISTRIP = $extval (tcflag_t, "ISTRIP")
macdef INLCR = $extval (tcflag_t, "INLCR")
macdef IGNCR = $extval (tcflag_t, "IGNCR")
macdef ICRNL = $extval (tcflag_t, "ICRNL")
macdef IUCLC = $extval (tcflag_t, "IUCLC")
macdef IXON = $extval (tcflag_t, "IXON")
macdef IXANY = $extval (tcflag_t, "IXANY")
macdef IXOFF = $extval (tcflag_t, "IXOFF")
macdef IMAXBEL = $extval (tcflag_t, "IMAXBEL")
macdef IUTF8 = $extval (tcflag_t, "IUTF8")

(* ****** ****** *)

(*
** c_oflag bits
*)
macdef OPOST = $extval (tcflag_t, "OPOST")
macdef OLCUC = $extval (tcflag_t, "OLCUC")
macdef ONLCR = $extval (tcflag_t, "ONLCR")
macdef OCRNL = $extval (tcflag_t, "OCRNL")
macdef ONOCR = $extval (tcflag_t, "ONOCR")
macdef ONLRET = $extval (tcflag_t, "ONLRET")
macdef OFILL = $extval (tcflag_t, "OFILL")
macdef OFDEL = $extval (tcflag_t, "OFDEL")
macdef NLDLY = $extval (tcflag_t, "NLDLY")
macdef NL0 = $extval (tcflag_t, "NL0")
macdef NL1 = $extval (tcflag_t, "NL1")
macdef CRDLY = $extval (tcflag_t, "CRDLY")
macdef CR0 = $extval (tcflag_t, "CR0")
macdef CR1 = $extval (tcflag_t, "CR1")
macdef CR2 = $extval (tcflag_t, "CR2")
macdef CR3 = $extval (tcflag_t, "CR3")
macdef TABDLY = $extval (tcflag_t, "TABDLY")
macdef TAB0 = $extval (tcflag_t, "TAB0")
macdef TAB1 = $extval (tcflag_t, "TAB1")
macdef TAB2 = $extval (tcflag_t, "TAB2")
macdef TAB3 = $extval (tcflag_t, "TAB3")
macdef BSDLY = $extval (tcflag_t, "BSDLY")
macdef BS0 = $extval (tcflag_t, "BS0")
macdef BS1 = $extval (tcflag_t, "BS1")
macdef FFDLY = $extval (tcflag_t, "FFDLY")
macdef FF0 = $extval (tcflag_t, "FF0")
macdef FF1 = $extval (tcflag_t, "FF1")
macdef VTDLY = $extval (tcflag_t, "VTDLY")
macdef VT0 = $extval (tcflag_t, "VT0")
macdef VT1 = $extval (tcflag_t, "VT1")
macdef XTABS = $extval (tcflag_t, "XTABS")

(* ****** ****** *)

(*
** c_cflag bit meaning
*)
macdef CBAUD = $extval (tcflag_t, "CBAUD")
macdef B0 = $extval (tcflag_t, "B0")
macdef B50 = $extval (tcflag_t, "B50")
macdef B75 = $extval (tcflag_t, "B75")
macdef B110 = $extval (tcflag_t, "B110")
macdef B134 = $extval (tcflag_t, "B134")
macdef B150 = $extval (tcflag_t, "B150")
macdef B200 = $extval (tcflag_t, "B200")
macdef B300 = $extval (tcflag_t, "B300")
macdef B600 = $extval (tcflag_t, "B600")
macdef B1200 = $extval (tcflag_t, "B1200")
macdef B1800 = $extval (tcflag_t, "B1800")
macdef B2400 = $extval (tcflag_t, "B2400")
macdef B4800 = $extval (tcflag_t, "B4800")
macdef B9600 = $extval (tcflag_t, "B9600")
macdef B19200 = $extval (tcflag_t, "B19200")
macdef B38400 = $extval (tcflag_t, "B38400")
macdef EXTA = $extval (tcflag_t, "EXTA")
macdef EXTB = $extval (tcflag_t, "EXTB")
macdef CSIZE = $extval (tcflag_t, "CSIZE")
macdef CS5 = $extval (tcflag_t, "CS5")
macdef CS6 = $extval (tcflag_t, "CS6")
macdef CS7 = $extval (tcflag_t, "CS7")
macdef CS8 = $extval (tcflag_t, "CS8")
macdef CSTOPB = $extval (tcflag_t, "CSTOPB")
macdef CREAD = $extval (tcflag_t, "CREAD")
macdef PARENB = $extval (tcflag_t, "PARENB")
macdef PARODD = $extval (tcflag_t, "PARODD")
macdef HUPCL = $extval (tcflag_t, "HUPCL")
macdef CLOCAL = $extval (tcflag_t, "CLOCAL")
macdef CBAUDEX = $extval (tcflag_t, "CBAUDEX")
macdef B57600 = $extval (tcflag_t, "B57600")
macdef B115200 = $extval (tcflag_t, "B115200")
macdef B230400 = $extval (tcflag_t, "B230400")
macdef B460800 = $extval (tcflag_t, "B460800")
macdef B500000 = $extval (tcflag_t, "B500000")
macdef B576000 = $extval (tcflag_t, "B576000")
macdef B921600 = $extval (tcflag_t, "B921600")
macdef B1000000 = $extval (tcflag_t, "B1000000")
macdef B1152000 = $extval (tcflag_t, "B1152000")
macdef B1500000 = $extval (tcflag_t, "B1500000")
macdef B2000000 = $extval (tcflag_t, "B2000000")
macdef B2500000 = $extval (tcflag_t, "B2500000")
macdef B3000000 = $extval (tcflag_t, "B3000000")
macdef B3500000 = $extval (tcflag_t, "B3500000")
macdef B4000000 = $extval (tcflag_t, "B4000000")
macdef __MAX_BAUD = $extval (tcflag_t, "__MAX_BAUD")
macdef CIBAUD = $extval (tcflag_t, "CIBAUD")
macdef CMSPAR = $extval (tcflag_t, "CMSPAR")
macdef CRTSCTS = $extval (tcflag_t, "CRTSCTS")

(* ****** ****** *)

(*
** c_lflag bits
*)
macdef ISIG = $extval (tcflag_t, "ISIG")
macdef ICANON = $extval (tcflag_t, "ICANON")
macdef XCASE = $extval (tcflag_t, "XCASE")
macdef ECHO = $extval (tcflag_t, "ECHO")
macdef ECHOE = $extval (tcflag_t, "ECHOE")
macdef ECHOK = $extval (tcflag_t, "ECHOK")
macdef ECHONL = $extval (tcflag_t, "ECHONL")
macdef NOFLSH = $extval (tcflag_t, "NOFLSH")
macdef TOSTOP = $extval (tcflag_t, "TOSTOP")
macdef ECHOCTL = $extval (tcflag_t, "ECHOCTL")
macdef ECHOPRT = $extval (tcflag_t, "ECHOPRT")
macdef ECHOKE = $extval (tcflag_t, "ECHOKE")
macdef FLUSHO = $extval (tcflag_t, "FLUSHO")
macdef PENDIN = $extval (tcflag_t, "PENDIN")
macdef IEXTEN = $extval (tcflag_t, "IEXTEN")

(* ****** ****** *)

(*
** tcflow() and TCXONC use these
*)
macdef TCOOFF = $extval (int, "TCOOFF")
macdef TCOON = $extval (int, "TCOON")
macdef TCIOFF = $extval (int, "TCIOFF")
macdef TCION = $extval (int, "TCION")

(*
** tcflush() and TCFLSH use these
*)
macdef TCIFLUSH = $extval (int, "TCIFLUSH")
macdef TCOFLUSH = $extval (int, "TCOFLUSH")
macdef TCIOFLUSH = $extval (int, "TCIOFLUSH")

(*
** tcsetattr uses these
*)
macdef TCSANOW = $extval (int, "TCSANOW")
macdef TCSADRAIN = $extval (int, "TCSADRAIN")
macdef TCSAFLUSH = $extval (int, "TCSAFLUSH")

(* ****** ****** *)

fun tcgetattr // 0/-1 : succ/fail // set errno
  {fd:nat} (
  fd: int, tm: &termios
) : int
  = "mac#atslib_tcgetattr"
// end of [tcgetattr]

fun tcsetattr // 0/-1 : succ/fail // set errno
  {fd:nat} (
  fd: int fd, actions: int, tm: &termios
) : int
  = "mac#atslib_tcsetattr"
// end of [tcsetattr]

(* ****** ****** *)

fun cfgetispeed
  (tm: &termios):<> speed_t = "mac#atslib_cfgetispeed" // no error
fun cfsetispeed
  (tm: &termios, x: speed_t):<> int = "mac#atslib_cfsetispeed" // 0/-1: succ/fail
fun cfgetospeed
  (tm: &termios):<> speed_t = "mac#atslib_cfgetospeed"// no error
fun cfsetospeed
  (tm: &termios, x: speed_t):<> int = "mac#atslib_cfsetospeed"// 0/-1 : succ/fail

(* ****** ****** *)

fun tcflow
  {fd:nat} (
  fd: int fd, action: int
) : int
  = "mac#atslib_tcflow" // 0/-1 : succ/fail
// end of [fun]

fun tcdrain {fd:nat}
  (fd: int fd): int = "mac#atslib_tcdrain" // 0/-1 : succ/fail
// end of [fun]

fun tcflush
  {fd:nat} (
  fd: int fd, queue: int
) : int
  = "mac#atslib_tcflush" // 0/-1 : succ/fail
// end of [tcflush]

fun tcsendbreak
  {fd:nat} (
  fd: int fd, dura: int
) : int
  = "mac#atslib_tcsendbreak" // 0/-1 : succ/fail
// end of [fun]

(* ****** ****** *)
//
// HX-2010-09-27: only available on SUS systems; not on FreeBSD
//
fun tcgetsid {fd:nat}
  (fd: int fd): pid_t = "mac#atslib_tcgetsid" // -1 is returned on error
// end of [tcgetsid]

(* ****** ****** *)

(* end of [termios.sats] *)
