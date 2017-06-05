/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2008, 2009, 2010
 * Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

#ifndef GNUTLS_ERRORS_H
#define GNUTLS_ERRORS_H

#include <gnutls_int.h>
#include <gnutls_global.h>

#define GNUTLS_E_INT_RET_0 -1251

#ifdef __FILE__
#ifdef __LINE__
#define gnutls_assert() _gnutls_debug_log( "ASSERT: %s:%d\n", __FILE__,__LINE__);
#else
#define gnutls_assert()
#endif
#else /* __FILE__ not defined */
#define gnutls_assert()
#endif

int _gnutls_asn2err (int asn_err);
void
_gnutls_log (int, const char *fmt, ...)
#ifdef __GNUC__
  __attribute__ ((format (printf, 2, 3)));
#else
 ;
#endif

     void _gnutls_mpi_log (const char *prefix, bigint_t a);

#ifdef C99_MACROS
#define LEVEL(l, ...) do { if (_gnutls_log_level >= l || _gnutls_log_level > 9) \
      _gnutls_log( l, __VA_ARGS__); } while(0)

#define LEVEL_EQ(l, ...) do { if (_gnutls_log_level == l || _gnutls_log_level > 9) \
      _gnutls_log( l, __VA_ARGS__); } while(0)

#define _gnutls_debug_log(...) LEVEL(2, __VA_ARGS__)
#define _gnutls_handshake_log(...) LEVEL(3, __VA_ARGS__)
#define _gnutls_io_log(...) LEVEL_EQ(5, __VA_ARGS__)
#define _gnutls_buffers_log(...) LEVEL_EQ(6, __VA_ARGS__)
#define _gnutls_hard_log(...) LEVEL(9, __VA_ARGS__)
#define _gnutls_record_log(...) LEVEL(4, __VA_ARGS__)
#define _gnutls_read_log(...) LEVEL_EQ(7, __VA_ARGS__)
#define _gnutls_write_log(...) LEVEL_EQ(7, __VA_ARGS__)
#define _gnutls_x509_log(...) LEVEL(1, __VA_ARGS__)
#else
#define _gnutls_debug_log _gnutls_null_log
#define _gnutls_handshake_log _gnutls_null_log
#define _gnutls_io_log _gnutls_null_log
#define _gnutls_buffers_log _gnutls_null_log
#define _gnutls_hard_log _gnutls_null_log
#define _gnutls_record_log _gnutls_null_log
#define _gnutls_read_log _gnutls_null_log
#define _gnutls_write_log _gnutls_null_log
#define _gnutls_x509_log _gnutls_null_log

     void _gnutls_null_log (void *, ...);

#endif /* C99_MACROS */

/* GCC won't inline this by itself and results in a "fatal warning"
   otherwise. Making this a macro has been tried, but it interacts
   badly with the do..while in the expansion. Welcome to the dark
   side. */
     static inline
#ifdef __GNUC__
       __attribute__ ((always_inline))
#endif
     int gnutls_assert_val_int (int val, const char *file, int line)
{
  _gnutls_debug_log ("ASSERT: %s:%d\n", file, line);
  return val;
}

#define gnutls_assert_val(x) gnutls_assert_val_int(x, __FILE__, __LINE__)

#endif /* GNUTLS_ERRORS_H */
