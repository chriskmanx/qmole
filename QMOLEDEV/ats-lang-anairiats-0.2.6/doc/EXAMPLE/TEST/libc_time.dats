(*
** some testing code for functions declared in
** libc/SATS/time.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//

(* ****** ****** *)

staload "libc/SATS/time.sats"

(* ****** ****** *)

implement
main () = () where {
//
  var time: time_t
  val yn = time_get_and_set (time)
  val () = assertloc (yn)
  prval () = opt_unsome{time_t} (time)
//
  val (fpf_p | p) = ctime (time)
  val () = assertloc (strptr_isnot_null p)
  val str = strptr_dup (p)
  prval () = fpf_p (p)
  val () = (print "ctime = "; print str)
//
  var tm: tm_struct
  val fmt = "%a %b %d %T %Y"
  val perr = strptime (__cast str, fmt, tm) where {
    extern castfn __cast {l:agz} (x: !strptr l): string
  } // end of [val]
  val () = assertloc (perr > null)
  prval () = opt_unsome {tm_struct} (tm)
  val time2 = mktime (tm)
//
  val diff = difftime (time, time2)
  val () = println! ("diff = ", diff)
(*
  val () = assertloc ((lint_of)diff = 0L) // HX: why is it 3600?
*)
  val () = strptr_free (str)
//
  val (pfopt | p_tm) = localtime (time)
  val () = assert_errmsg (p_tm > null, #LOCATION)
  prval Some_v @(pf, fpf) = pfopt
  val (fpf_p | p) = asctime (!p_tm)
  prval () = fpf (pf)
  val () = (print "asctime(LOC) = "; print p)
  prval () = fpf_p (p)
//
  val (pfopt | p_tm) = gmtime (time)
  val () = assert_errmsg (p_tm > null, #LOCATION)
  prval Some_v @(pf, fpf) = pfopt
  val (fpf_p | p) = asctime (!p_tm)
  prval () = fpf (pf)
  val () = (print "asctime(GMT) = "; print p)
  prval () = fpf_p (p)
//
} // end of [main]

(* ****** ****** *)

(* end of [libc_time.dats] *)


