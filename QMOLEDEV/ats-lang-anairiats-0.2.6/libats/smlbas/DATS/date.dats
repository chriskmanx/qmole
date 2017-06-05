(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: Date (http://www.standardml.org/Basis/date.html)
//

(* ****** ****** *)

staload "libats/smlbas/SATS/date.sats"

(* ****** ****** *)

assume date = '{
  year= int, month= month, day= int
, hour= int, minute= int, second= int
, offset= option0 ($TIME.time)
} // end of [date]

(* ****** ****** *)

implement date (
    year
  , month
  , day
  , hour
  , minute
  , second
  , ofsopt
  ) : date = '{
  year= year, month= month, day= day
, hour= hour, minute= minute, second= second
, offset= ofsopt
} // end of [date]

(* ****** ****** *)

fn int_of_month (month: month): int = case+ month of
  | Jan () => 0
  | Feb () => 1
  | Mar () => 2
  | Apr () => 3
  | May () => 4
  | Jun () => 5
  | Jul () => 6
  | Aug () => 7 
  | Sep () => 8
  | Oct () => 9
  | Nov () => 10
  | Dec () => 11
// end of [int_of_month]

(* ****** ****** *)

implement year (date) = date.year
implement month (date) = date.month
implement day (date) = date.day
implement hour (date) = date.hour
implement minute (date) = date.minute
implement second (date) = date.second

(*
fun weekday (_: date): weekday
fun yearday (_: date): int
*)

implement offset (date) = date.offset

(*
fun isDst (_: date): option0 (bool)
fun localOffset (): $Time.time
*)

implement compare_date_date (d1, d2) = begin
  case+ 0 of
  | _ when d1.year > d2.year =>  1
  | _ when d1.year < d2.year => ~1
  | _ (* same year *) => let
    val m1 = int_of_month (d1.month)
    and m2 = int_of_month (d2.month) in case+ 0 of
    | _ when m1 > m2 =>  1
    | _ when m1 < m2 => ~1
    | _ (* same month *) => begin case+ 0 of
      | _ when d1.day > d2.day =>  1
      | _ when d1.day < d2.day => ~1
      | _ (* same day *) => begin case+ 0 of
        | _ when d1.hour > d2.hour =>  1
        | _ when d1.hour < d2.hour => ~1
        | _ (* same hour *) => begin case+ 0 of
          | _ when d1.minute > d2.minute =>  1
          | _ when d1.minute < d2.minute => ~1
          | _ (* same minute *) => begin case+ 0 of
            | _ when d1.second > d2.second =>  1
            | _ when d1.second < d2.second => ~1
            | _ (* same second *) => 0
            end
          end
        end
      end       
    end
end // end of [compare_date_date]

(* ****** ****** *)

(*

fun toString (d: date): string
fun fromString (s: string): date

*)

(* ****** ****** *)

(* end of [date.dats] *)
