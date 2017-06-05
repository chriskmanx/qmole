/* misc.h */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002, 2003 Niels Möller
 *  
 * The nettle library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * The nettle library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with the nettle library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 */

#ifndef NETTLE_TOOLS_MISC_H_INCLUDED
#define NETTLE_TOOLS_MISC_H_INCLUDED

#if HAVE_CONFIG_H
# include "config.h"
#endif

void
die(const char *format, ...)
#if __GNUC___
     __attribute__((__format__ (__printf__,1, 2)))
     __attribute__((__noreturn__))
#endif
     ;

void
werror(const char *format, ...)
#if __GNUC___
     __attribute__((__format__ (__printf__,1, 2)))
#endif
     ;

void *
xalloc(size_t size);

enum sexp_mode
  {
    SEXP_CANONICAL = 0,
    SEXP_ADVANCED = 1,
    SEXP_TRANSPORT = 2,
  };

enum sexp_token
  {
    SEXP_STRING,
    SEXP_DISPLAY, /* Constructed by sexp_parse */
    SEXP_COMMENT,
    SEXP_LIST_START,
    SEXP_LIST_END,
    SEXP_EOF,

    /* The below types are internal to the input parsing. sexp_parse
     * should never return a token of this type. */
    SEXP_DISPLAY_START,
    SEXP_DISPLAY_END,
    SEXP_TRANSPORT_START,
    SEXP_CODING_END,
  };

extern const char
sexp_token_chars[0x80];

#define TOKEN_CHAR(c) ((c) < 0x80 && sexp_token_chars[(c)])

#endif /* NETTLE_TOOLS_MISC_H_INCLUDED */
