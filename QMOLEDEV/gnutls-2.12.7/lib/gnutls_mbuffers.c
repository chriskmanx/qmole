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

#include "gnutls_mbuffers.h"
#include "gnutls_errors.h"

/* Here be mbuffers */

/* A note on terminology:
 *
 * Variables named bufel designate a single buffer segment (mbuffer_st
 * type). This type is textually referred to as a "segment" or a
 * "buffer element".
 *
 * Variables named buf desigate a chain of buffer segments
 * (mbuffer_head_st type).  This type is textually referred to as a
 * "buffer head" or simply as "buffer".
 *
 * Design objectives:
 *
 * - Make existing code easier to understand.
 * - Make common operations more efficient by avoiding unnecessary
 *    copying.
 * - Provide a common datatype with a well-known interface to move
 *    data around and through the multiple protocol layers.
 * - Enable a future implementation of DTLS, which needs the concept
 *    of record boundaries.
 */


/* Initialize a buffer head.
 *
 * Cost: O(1)
 */
void
_mbuffer_init (mbuffer_head_st * buf)
{
  buf->head = NULL;
  buf->tail = &buf->head;

  buf->length = 0;
  buf->byte_length = 0;
}

/* Deallocate all buffer segments and reset the buffer head.
 *
 * Cost: O(n)
 * n: Number of segments currently in the buffer.
 */
void
_mbuffer_clear (mbuffer_head_st * buf)
{
  mbuffer_st *bufel, *next;

  for (bufel = buf->head; bufel != NULL; bufel = next)
    {
      next = bufel->next;
      gnutls_free (bufel);
    }

  _mbuffer_init (buf);
}

/* Append a segment to the end of this buffer.
 *
 * Cost: O(1)
 */
void
_mbuffer_enqueue (mbuffer_head_st * buf, mbuffer_st * bufel)
{
  bufel->next = NULL;

  buf->length++;
  buf->byte_length += bufel->msg.size - bufel->mark;

  *(buf->tail) = bufel;
  buf->tail = &bufel->next;
}

/* Get a reference to the first segment of the buffer and its data.
 *
 * Used to start iteration or to peek at the data.
 *
 * Cost: O(1)
 */
mbuffer_st *
_mbuffer_get_first (mbuffer_head_st * buf, gnutls_datum_t * msg)
{
  mbuffer_st *bufel = buf->head;

  if (bufel)
    {
      msg->data = bufel->msg.data + bufel->mark;
      msg->size = bufel->msg.size - bufel->mark;
    }
  else
    {
      msg->data = NULL;
      msg->size = 0;
    }
  return bufel;
}

/* Get a reference to the next segment of the buffer and its data.
 *
 * Used to iterate over the buffer segments.
 *
 * Cost: O(1)
 */
mbuffer_st *
_mbuffer_get_next (mbuffer_st * cur, gnutls_datum_t * msg)
{
  mbuffer_st *bufel = cur->next;

  if (bufel)
    {
      msg->data = bufel->msg.data + bufel->mark;
      msg->size = bufel->msg.size - bufel->mark;
    }
  else
    {
      msg->data = NULL;
      msg->size = 0;
    }
  return bufel;
}

/* Remove the first segment from the buffer.
 *
 * Used to dequeue data from the buffer. Not yet exposed in the
 * internal interface since it is not yet needed outside of this unit.
 *
 * Cost: O(1)
 */
static inline void
remove_front (mbuffer_head_st * buf)
{
  mbuffer_st *bufel;

  if (!buf->head)
    return;

  bufel = buf->head;
  buf->head = bufel->next;

  buf->byte_length -= (bufel->msg.size - bufel->mark);
  buf->length -= 1;
  gnutls_free (bufel);

  if (!buf->head)
    buf->tail = &buf->head;
}

/* Remove a specified number of bytes from the start of the buffer.
 *
 * Useful for uses that treat the buffer as a simple array of bytes.
 *
 * Returns 0 on success or an error code otherwise.
 *
 * Cost: O(n)
 * n: Number of segments needed to remove the specified amount of data.
 */
int
_mbuffer_remove_bytes (mbuffer_head_st * buf, size_t bytes)
{
  size_t left = bytes;
  mbuffer_st *bufel, *next;

  if (bytes > buf->byte_length)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  for (bufel = buf->head; bufel != NULL && left > 0; bufel = next)
    {
      next = bufel->next;

      if (left >= (bufel->msg.size - bufel->mark))
        {
          left -= (bufel->msg.size - bufel->mark);
          remove_front (buf);
        }
      else
        {
          bufel->mark += left;
          buf->byte_length -= left;
          left = 0;
        }
    }

  return 0;
}

/* Allocate a buffer segment. The segment is not initially "owned" by
 * any buffer.
 *
 * maximum_size: Amount of data that this segment can contain.
 * size: Amount of useful data that is contained in this
 *  buffer. Generally 0, but this is a shortcut when a fixed amount of
 *  data will immediately be added to this segment.
 *
 * Returns the segment or NULL on error.
 *
 * Cost: O(1)
 */
mbuffer_st *
_mbuffer_alloc (size_t payload_size, size_t maximum_size)
{
  mbuffer_st *st;

  st = gnutls_malloc (maximum_size + sizeof (mbuffer_st));
  if (st == NULL)
    {
      gnutls_assert ();
      return NULL;
    }

  //payload points after the mbuffer_st structure
  st->msg.data = (opaque *) st + sizeof (mbuffer_st);
  st->msg.size = payload_size;
  st->mark = 0;
  st->user_mark = 0;
  st->next = NULL;
  st->maximum_size = maximum_size;

  return st;
}

/* Copy data into a segment. The segment must not be part of a buffer
 * head when using this function.
 *
 * Bounds checking is performed by this function.
 *
 * Returns 0 on success or an error code otherwise.
 *
 * Cost: O(n)
 * n: number of bytes to copy
 */
int
_mbuffer_append_data (mbuffer_st * bufel, void *newdata, size_t newdata_size)
{
  if (bufel->msg.size + newdata_size <= bufel->maximum_size)
    {
      memcpy (&bufel->msg.data[bufel->msg.size], newdata, newdata_size);
      bufel->msg.size += newdata_size;
    }
  else
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_REQUEST;
    }

  return 0;
}

/* Takes a buffer in multiple chunks and puts all the data in a single
 * contiguous segment.
 *
 * Returns 0 on success or an error code otherwise.
 *
 * Cost: O(n)
 * n: number of segments initially in the buffer
 */
int
_mbuffer_linearize (mbuffer_head_st * buf)
{
  mbuffer_st *bufel, *cur;
  gnutls_datum_t msg;
  size_t pos = 0;

  if (buf->length <= 1)
    /* Nothing to do */
    return 0;

  bufel = _mbuffer_alloc (buf->byte_length, buf->byte_length);
  if (!bufel)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  for (cur = _mbuffer_get_first (buf, &msg);
       msg.data != NULL; cur = _mbuffer_get_next (cur, &msg))
    {
      memcpy (&bufel->msg.data[pos], msg.data, cur->msg.size);
      pos += cur->msg.size;
    }

  _mbuffer_clear (buf);
  _mbuffer_enqueue (buf, bufel);

  return 0;
}
