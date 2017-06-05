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

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef ATS_LIBC_PWD_CATS
#define ATS_LIBC_PWD_CATS

/* ****** ****** */

#include <sys/types.h>
#include <pwd.h>

/* ****** ****** */

typedef struct passwd ats_passwd_type ;

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_passwd_get_pw_name
  (ats_ptr_type pw) {
  return ((ats_passwd_type*)pw)->pw_name ;
} // end of [passwd_get_pw_name]

ATSinline()
ats_ptr_type
atslib_passwd_get_pw_passwd
  (ats_ptr_type pw) {
  return ((ats_passwd_type*)pw)->pw_passwd ;
} // end of [passwd_get_pw_passwd]

ATSinline()
ats_ptr_type
atslib_passwd_get_pw_gecos
  (ats_ptr_type pw) {
  return ((ats_passwd_type*)pw)->pw_gecos ;
} // end of [passwd_get_pw_gecos]

ATSinline()
ats_ptr_type
atslib_passwd_get_pw_dir
  (ats_ptr_type pw) {
  return ((ats_passwd_type*)pw)->pw_dir ;
} // end of [passwd_get_pw_dir]

ATSinline()
ats_ptr_type
atslib_passwd_get_pw_shell
  (ats_ptr_type pw) {
  return ((ats_passwd_type*)pw)->pw_shell ;
} // end of [passwd_get_pw_shell]

/* ****** ****** */

#define atslib_getpwnam getpwnam
#define atslib_getpwnam_r(nam, pwbuf, buf, n, ppwbuf) \
  getpwnam_r(nam, (ats_passwd_type*)pwbuf, (char*)buf, n, (ats_passwd_type**)ppwbuf)

/* ****** ****** */

#define atslib_getpwuid getpwuid
#define atslib_getpwuid_r(uid, pwbuf, buf, n, ppwbuf) \
  getpwuid_r(uid, (ats_passwd_type*)pwbuf, (char*)buf, n, (ats_passwd_type**)ppwbuf)

/* ****** ****** */

#endif /* ATS_LIBC_PWD_CATS */

/* end of [pwd.cats] */
