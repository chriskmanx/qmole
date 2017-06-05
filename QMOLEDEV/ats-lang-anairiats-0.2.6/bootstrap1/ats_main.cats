/***********************************************************************/
/*                                                                     */
/*                        Applied Type System                          */
/*                                                                     */
/*                             Hongwei Xi                              */
/*                                                                     */
/***********************************************************************/

/*
** ATS/Anairiats - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
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
*/

/* ****** ****** */

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// September 2008

/* ****** ****** */

#ifndef ATS_SRC_MAIN_CATS
#define ATS_SRC_MAIN_CATS

/* ****** ****** */
//
// HX-2010-10-21: this one seems to be a macro
//
#ifndef strncmp
extern int strncmp (const char *s1, const char *s2, size_t n) ;
#endif // end of ...

/* ****** ****** */

extern
ats_void_type
ats_posmark_xref_flag_set (ats_ptr_type flag) ;

ATSinline()
ats_bool_type
atsopt_is_posmark_xref_prefix (ats_ptr_type s0) {
  int cmp, n1, n2, ln ; char *s, *flag ;
  static char* POSMARK_XREF = "--posmark_xref" ;
  s = (char*)s0 ;
  n1 = strlen (POSMARK_XREF) ;
  cmp = strncmp (POSMARK_XREF, s, n1) ;
  if (cmp == 0) {
    n2 = strlen (s) ;
    if (s[n1] == '=') n1 += 1 ;
    ln = n2 - n1 ;
    if (ln > 0) {
      if (s[n2-1] == '/') { ln -= 1 ; }
      flag = (char*)ATS_MALLOC(ln + 2) ;
      strncpy (flag, &s[n1], ln) ;
      flag[ln] = '/' ; flag[ln+1] = '\000' ;
    } else {
      flag = "" ;
    } // end of [if]
/*
    fprintf (stderr, "atsopt_is_posmark_xref_prefix: flag = %s\n", flag) ;
*/
    ats_posmark_xref_flag_set ((ats_ptr_type)flag) ;
  } // end of [if]
  return (cmp == 0 ? ats_true_bool : ats_false_bool) ;
} /* end of [atsopt_is_posmark_xref_prefix] */

/* ****** ****** */

static
int the_DATS_wait = 0 ;

ATSinline()
ats_void_type
atsopt_DATS_wait_set () {
  the_DATS_wait = 1 ; return ;
} // end of [atsopt_DATS_wait_set]

ATSinline()
ats_bool_type
atsopt_DATS_wait_is_set () {
  return (the_DATS_wait ? ats_true_bool : ats_false_bool) ;
} // end of [atsopt_DATS_wait_is_set]

ATSinline()
ats_void_type
atsopt_DATS_wait_clear () {
  the_DATS_wait = 0 ; return ;
} // end of [atsopt_DATS_wait_clear]

ats_bool_type
atsopt_is_DATS_flag (ats_ptr_type s0) { return (
  strncmp((char*)s0, "-DATS", 5)==0 ? ats_true_bool : ats_false_bool
) ; } // end of [atsopt_is_DATS_flag]

ats_ptr_type
atsopt_DATS_extract (ats_ptr_type s0) {
  int n ; char* s ;
  n = strlen ((char*)s0) - 5 ;
  if (n <= 0) return (ats_ptr_type)0 ;
  s = (char*)ATS_MALLOC(n + 1) ;
  memcpy (s, (char*)s0 + 5, n) ; s[n] = '\0' ;
  return (ats_ptr_type)s ;
} // end of [atsopt_DATS_extract]

/* ****** ****** */

static
int the_IATS_wait = 0 ;

ATSinline()
ats_void_type
atsopt_IATS_wait_set () {
  the_IATS_wait = 1 ; return ;
} // end of [atsopt_IATS_wait_set]

ATSinline()
ats_bool_type
atsopt_IATS_wait_is_set () {
  return (the_IATS_wait ? ats_true_bool : ats_false_bool) ;
} // end of [atsopt_IATS_wait_is_set]

ATSinline()
ats_void_type
atsopt_IATS_wait_clear () {
  the_IATS_wait = 0 ; return ;
} // end of [atsopt_IATS_wait_clear]

ats_bool_type
atsopt_is_IATS_flag (ats_ptr_type s0) {
  return (
    strncmp((char*)s0, "-IATS", 5)==0 ? ats_true_bool : ats_false_bool
  ) ; // end of [return]
} // end of [atsopt_is_IATS_flag]

ats_ptr_type
atsopt_IATS_extract (ats_ptr_type s0) {
  int n ; char* s ;
  n = strlen ((char*)s0) - 5 ;
  if (n <= 0) return (ats_ptr_type)0 ;
  s = (char*)ATS_MALLOC(n + 1) ;
  memcpy (s, (char*)s0 + 5, n) ; s[n] = '\0' ;
  return (ats_ptr_type)s ;
} // end of [atsopt_IATS_extract]

/* ****** ****** */
//
// HX: global
//
char *atsopt_ATSHOME = NULL ; // no need for marking as a root
int atsopt_ATSHOME_length = 0;

ats_ptr_type
atsopt_ATSHOME_getenv_exn () {
 char *value0 ;
 value0 = getenv ("ATSHOME") ; // this value cannot be GCed
 if (!value0) {
   fprintf (stderr, "The environment variable ATSHOME is undefined.\n") ;
   exit (1) ;
 }
 atsopt_ATSHOME = value0 ; atsopt_ATSHOME_length = strlen (value0) ;
 return (ats_ptr_type)value0 ;
} /* end of [atsopt_ATSHOME_getenv_exn] */

/* ****** ****** */
//
// HX: global
//
char *atsopt_ATSHOMERELOC = NULL ; // no need for marking as a root

ats_void_type
atsopt_ATSHOMERELOC_set () {
  atsopt_ATSHOMERELOC = getenv ("ATSHOMERELOC") ; // this value cannot be GCed
/*
  fprintf (stderr, "atsopt_ATSHOMERELOC_set: ATSHOMERELOC = %s\n", atsopt_ATSHOMERELOC) ;
*/
  return ;
} // end of [atsopt_ATSHOMERELOC_set]

/* ****** ****** */

#endif // [ATS_SRC_MAIN_CATS]

/* end of [ats_main.cats] */
