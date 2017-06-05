/*
 * Copyright (C) 2009 Free Software Foundation
 *
 * Author: Jonathan Bastien-Filiatrault
 *
 * This file is part of GNUTLS.
 *
 * The GNUTLS library is free software; you can redistribute it and/or
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

#ifndef GNUTLS_MBUFFERS_H
#define GNUTLS_MBUFFERS_H

#include <gnutls_int.h>
#include <gnutls_errors.h>

void _mbuffer_init (mbuffer_head_st * buf);
void _mbuffer_clear (mbuffer_head_st * buf);
void _mbuffer_enqueue (mbuffer_head_st * buf, mbuffer_st * bufel);
int _mbuffer_remove_bytes (mbuffer_head_st * buf, size_t bytes);
mbuffer_st *_mbuffer_alloc (size_t payload_size, size_t maximum_size);

mbuffer_st *_mbuffer_get_first (mbuffer_head_st * buf, gnutls_datum_t * msg);
mbuffer_st *_mbuffer_get_next (mbuffer_st * cur, gnutls_datum_t * msg);

/* This is dangerous since it will replace bufel with a new
 * one.
 */
int _mbuffer_append_data (mbuffer_st * bufel, void *newdata,
                          size_t newdata_size);
int _mbuffer_linearize (mbuffer_head_st * buf);


/* For "user" use. One can have buffer data and header.
 */

inline static void
_mbuffer_set_udata (mbuffer_st * bufel, void *data, size_t data_size)
{
  memcpy (bufel->msg.data + bufel->user_mark, data, data_size);
}

inline static void *
_mbuffer_get_uhead_ptr (mbuffer_st * bufel)
{
  return bufel->msg.data;
}

inline static void *
_mbuffer_get_udata_ptr (mbuffer_st * bufel)
{
  return bufel->msg.data + bufel->user_mark;
}

inline static void
_mbuffer_set_udata_size (mbuffer_st * bufel, size_t size)
{
  bufel->msg.size = size + bufel->user_mark;
}

inline static size_t
_mbuffer_get_udata_size (mbuffer_st * bufel)
{
  return bufel->msg.size - bufel->user_mark;
}

inline static size_t
_mbuffer_get_uhead_size (mbuffer_st * bufel)
{
  return bufel->user_mark;
}

inline static void
_mbuffer_set_uhead_size (mbuffer_st * bufel, size_t size)
{
  bufel->user_mark = size;
}



inline static mbuffer_st *
_gnutls_handshake_alloc (size_t size, size_t maximum)
{
  mbuffer_st *ret = _mbuffer_alloc (HANDSHAKE_HEADER_SIZE + size,
                                    HANDSHAKE_HEADER_SIZE + maximum);

  if (!ret)
    return NULL;

  _mbuffer_set_uhead_size (ret, HANDSHAKE_HEADER_SIZE);

  return ret;
}

/* Free a segment, if the pointer is not NULL
 *
 * We take a ** to detect and fix double free bugs (the dangling
 * pointer case). It also makes sure the pointer has a known value
 * after freeing.
 */
inline static void
_mbuffer_xfree (mbuffer_st ** bufel)
{
  if (*bufel)
    gnutls_free (*bufel);

  *bufel = NULL;
}

#endif
