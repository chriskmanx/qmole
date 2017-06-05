/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi.
**
** ATS is  free software;  you can redistribute it and/or modify it under
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
*/

/* ****** ****** */

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010
//

/* ****** ****** */

#ifndef ATS_LIBC_SCHED_CATS
#define ATS_LIBC_SCHED_CATS

/* ****** ****** */

#include <sched.h>
#include "libc/sys/CATS/types.cats"

/* ****** ****** */

typedef struct sched_param ats_sched_param_type ;

/* ****** ****** */

#ifdef __USE_GNU

/* ****** ****** */

ATSinline()
ats_size_type
atslib_cpusetsize_get
  (ats_ref_type mask) { return sizeof(cpu_set_t) ; }
// end of [atslib_cpusetsize_get]

/* ****** ****** */

/*
fun sched_getaffinity {n:nat} (
  pid: pid_t, n: size_t n, cs: &cpu_set_t n? >> opt (cpu_set_t n, i==0)
) : #[i:int | i <= 0] int i(*err*) = "atslib_sched_getaffinity"
// end of [sched_getaffinity]
*/

ATSinline()
ats_int_type
atslib_sched_getaffinity (
  ats_pid_type pid, ats_size_type n, ats_ref_type mask
) {
  return sched_getaffinity (pid, n, (cpu_set_t*)mask) ;
} // end of [atslib_sched_getaffinity]

/*
fun sched_setaffinity {n:nat}
  (pid: pid_t, n: size_t n, mask: &cpu_set_t n): int(*err*)
  = "atslib_sched_setaffinity"
*/

ATSinline()
ats_int_type
atslib_sched_setaffinity (
  ats_pid_type pid, ats_size_type n, ats_ref_type mask
) {
  return sched_setaffinity (pid, n, (cpu_set_t*)mask) ;
} // end of [atslib_sched_setaffinity]

/* ****** ****** */

#define atslib_CPU_ZERO CPU_ZERO
#define atslib_CPU_CLR CPU_CLR
#define atslib_CPU_SET CPU_SET
#define atslib_CPU_ISSET CPU_ISSET

/* ****** ****** */

#endif // end of [__USE_GNU]

/* ****** ****** */

#endif /* ATS_LIBC_SCHED_CATS */
