/* GnuTLS --- Guile bindings for GnuTLS.
   Copyright (C) 2007, 2009, 2010 Free Software Foundation, Inc.

   GnuTLS is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   GnuTLS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with GnuTLS; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA  */

/* Written by Ludovic Courtès <ludo@chbouib.org>.  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <libguile.h>
#include <gnutls/gnutls.h>

#include "errors.h"
#include "enums.h"

SCM_SYMBOL (gnutls_error_key, "gnutls-error");

void
scm_gnutls_error (int c_err, const char *c_func)
{
  SCM err, func;

  /* Note: If error code C_ERR is unknown, then ERR will be `#f'.  */
  err = scm_from_gnutls_error (c_err);
  func = scm_from_locale_symbol (c_func);

  (void) scm_throw (gnutls_error_key, scm_list_2 (err, func));

  /* XXX: This is actually never reached, but since the Guile headers don't
     declare `scm_throw ()' as `noreturn', we must add this to avoid GCC's
     complaints.  */
  abort ();
}


void
scm_init_gnutls_error (void)
{
#include "errors.x"
}

/* arch-tag: 48f07ecf-65c4-480c-b043-a51eab592d6b
 */
