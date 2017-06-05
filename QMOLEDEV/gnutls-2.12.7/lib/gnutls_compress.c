/*
 * Copyright (C) 2000, 2004, 2005, 2007, 2008, 2010 Free Software
 * Foundation, Inc.
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

/* This file contains the functions which convert the TLS plaintext
 * packet to TLS compressed packet.
 */

#include "gnutls_int.h"
#include "gnutls_compress.h"
#include "gnutls_errors.h"
#include "gnutls_constate.h"
#include <gnutls_algorithms.h>
#include <gnutls/gnutls.h>

/* These functions allocate the return value internally
 */
int
_gnutls_m_plaintext2compressed (gnutls_session_t session,
                                gnutls_datum_t * compressed,
                                const gnutls_datum_t * plaintext,
                                const record_parameters_st * params)
{
  int size;
  opaque *data;

  size =
    _gnutls_compress (params->write.compression_state,
                      plaintext->data, plaintext->size, &data,
                      MAX_RECORD_SEND_SIZE + EXTRA_COMP_SIZE);
  if (size < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_COMPRESSION_FAILED;
    }
  compressed->data = data;
  compressed->size = size;

  return 0;
}

int
_gnutls_m_compressed2plaintext (gnutls_session_t session,
                                gnutls_datum_t * plain,
                                const gnutls_datum_t * compressed,
                                const record_parameters_st * params)
{
  int size;
  opaque *data;

  size =
    _gnutls_decompress (params->read.compression_state,
                        compressed->data, compressed->size, &data,
                        MAX_RECORD_RECV_SIZE);
  if (size < 0)
    {
      gnutls_assert ();
      return GNUTLS_E_DECOMPRESSION_FAILED;
    }
  plain->data = data;
  plain->size = size;

  return 0;
}


/* Compression Section */
#define GNUTLS_COMPRESSION_ENTRY(name, id, wb, ml, cl)	\
  { #name, name, id, wb, ml, cl}


#define MAX_COMP_METHODS 5
const int _gnutls_comp_algorithms_size = MAX_COMP_METHODS;

gnutls_compression_entry _gnutls_compression_algorithms[MAX_COMP_METHODS] = {
  GNUTLS_COMPRESSION_ENTRY (GNUTLS_COMP_NULL, 0x00, 0, 0, 0),
#ifdef HAVE_LIBZ
  /* draft-ietf-tls-compression-02 */
  GNUTLS_COMPRESSION_ENTRY (GNUTLS_COMP_DEFLATE, 0x01, 15, 8, 3),
#endif
  {0, 0, 0, 0, 0, 0}
};

static const gnutls_compression_method_t supported_compressions[] = {
#ifdef USE_LZO
  GNUTLS_COMP_LZO,
#endif
#ifdef HAVE_LIBZ
  GNUTLS_COMP_DEFLATE,
#endif
  GNUTLS_COMP_NULL,
  0
};

#define GNUTLS_COMPRESSION_LOOP(b)	   \
  const gnutls_compression_entry *p;					\
  for(p = _gnutls_compression_algorithms; p->name != NULL; p++) { b ; }
#define GNUTLS_COMPRESSION_ALG_LOOP(a)					\
  GNUTLS_COMPRESSION_LOOP( if(p->id == algorithm) { a; break; } )
#define GNUTLS_COMPRESSION_ALG_LOOP_NUM(a)				\
  GNUTLS_COMPRESSION_LOOP( if(p->num == num) { a; break; } )

/* Compression Functions */

/**
 * gnutls_compression_get_name:
 * @algorithm: is a Compression algorithm
 *
 * Convert a #gnutls_compression_method_t value to a string.
 *
 * Returns: a pointer to a string that contains the name of the
 *   specified compression algorithm, or %NULL.
 **/
const char *
gnutls_compression_get_name (gnutls_compression_method_t algorithm)
{
  const char *ret = NULL;

  /* avoid prefix */
  GNUTLS_COMPRESSION_ALG_LOOP (ret = p->name + sizeof ("GNUTLS_COMP_") - 1);

  return ret;
}

/**
 * gnutls_compression_get_id:
 * @name: is a compression method name
 *
 * The names are compared in a case insensitive way.
 *
 * Returns: an id of the specified in a string compression method, or
 *   %GNUTLS_COMP_UNKNOWN on error.
 **/
gnutls_compression_method_t
gnutls_compression_get_id (const char *name)
{
  gnutls_compression_method_t ret = GNUTLS_COMP_UNKNOWN;

  GNUTLS_COMPRESSION_LOOP (if
                           (strcasecmp
                            (p->name + sizeof ("GNUTLS_COMP_") - 1,
                             name) == 0) ret = p->id);

  return ret;
}

/**
 * gnutls_compression_list:
 *
 * Get a list of compression methods.  Note that to be able to use LZO
 * compression, you must link to libgnutls-extra and call
 * gnutls_global_init_extra().
 *
 * Returns: a zero-terminated list of #gnutls_compression_method_t
 *   integers indicating the available compression methods.
 **/
const gnutls_compression_method_t *
gnutls_compression_list (void)
{
  return supported_compressions;
}

/* return the tls number of the specified algorithm */
int
_gnutls_compression_get_num (gnutls_compression_method_t algorithm)
{
  int ret = -1;

  /* avoid prefix */
  GNUTLS_COMPRESSION_ALG_LOOP (ret = p->num);

  return ret;
}

#ifdef HAVE_LIBZ

static int
get_wbits (gnutls_compression_method_t algorithm)
{
  int ret = -1;
  /* avoid prefix */
  GNUTLS_COMPRESSION_ALG_LOOP (ret = p->window_bits);
  return ret;
}

static int
get_mem_level (gnutls_compression_method_t algorithm)
{
  int ret = -1;
  /* avoid prefix */
  GNUTLS_COMPRESSION_ALG_LOOP (ret = p->mem_level);
  return ret;
}

static int
get_comp_level (gnutls_compression_method_t algorithm)
{
  int ret = -1;
  /* avoid prefix */
  GNUTLS_COMPRESSION_ALG_LOOP (ret = p->comp_level);
  return ret;
}

#endif

/* returns the gnutls internal ID of the TLS compression
 * method num
 */
gnutls_compression_method_t
_gnutls_compression_get_id (int num)
{
  gnutls_compression_method_t ret = -1;

  /* avoid prefix */
  GNUTLS_COMPRESSION_ALG_LOOP_NUM (ret = p->id);

  return ret;
}

int
_gnutls_compression_is_ok (gnutls_compression_method_t algorithm)
{
  ssize_t ret = -1;
  GNUTLS_COMPRESSION_ALG_LOOP (ret = p->id);
  if (ret >= 0)
    ret = 0;
  else
    ret = 1;
  return ret;
}



/* For compression  */

#define MIN_PRIVATE_COMP_ALGO 0xEF

/* returns the TLS numbers of the compression methods we support
 */
#define SUPPORTED_COMPRESSION_METHODS session->internals.priorities.compression.algorithms
int
_gnutls_supported_compression_methods (gnutls_session_t session,
                                       uint8_t ** comp)
{
  unsigned int i, j;

  *comp = gnutls_malloc (sizeof (uint8_t) * SUPPORTED_COMPRESSION_METHODS);
  if (*comp == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  for (i = j = 0; i < SUPPORTED_COMPRESSION_METHODS; i++)
    {
      int tmp =
        _gnutls_compression_get_num (session->internals.
                                     priorities.compression.priority[i]);

      /* remove private compression algorithms, if requested.
       */
      if (tmp == -1 || (tmp >= MIN_PRIVATE_COMP_ALGO &&
                        session->internals.enable_private == 0))
        {
          gnutls_assert ();
          continue;
        }

      (*comp)[j] = (uint8_t) tmp;
      j++;
    }

  if (j == 0)
    {
      gnutls_assert ();
      gnutls_free (*comp);
      *comp = NULL;
      return GNUTLS_E_NO_COMPRESSION_ALGORITHMS;
    }
  return j;
}


#ifdef USE_LZO
#ifdef USE_MINILZO
/* Get the prototypes only.  Since LZO is a GPLed library, the
 * gnutls_global_init_extra() has to be called, before LZO compression
 * can be used.
 */
#include "../libextra/minilzo/minilzo.h"
#elif HAVE_LZO_LZO1X_H
#include <lzo/lzo1x.h>
#elif HAVE_LZO1X_H
#include <lzo1x.h>
#endif

typedef int (*LZO_FUNC) ();

LZO_FUNC _gnutls_lzo1x_decompress_safe = NULL;
LZO_FUNC _gnutls_lzo1x_1_compress = NULL;

#endif

/* The flag d is the direction (compress, decompress). Non zero is
 * decompress.
 */
comp_hd_t
_gnutls_comp_init (gnutls_compression_method_t method, int d)
{
  comp_hd_t ret;

  ret = gnutls_malloc (sizeof (struct comp_hd_t_STRUCT));
  if (ret == NULL)
    {
      gnutls_assert ();
      return NULL;
    }

  ret->algo = method;
  ret->handle = NULL;

  switch (method)
    {
    case GNUTLS_COMP_DEFLATE:
#ifdef HAVE_LIBZ
      {
        int window_bits, mem_level;
        int comp_level;
        z_stream *zhandle;
        int err;

        window_bits = get_wbits (method);
        mem_level = get_mem_level (method);
        comp_level = get_comp_level (method);

        ret->handle = gnutls_malloc (sizeof (z_stream));
        if (ret->handle == NULL)
          {
            gnutls_assert ();
            goto cleanup_ret;
          }

        zhandle = ret->handle;

        zhandle->zalloc = (alloc_func) 0;
        zhandle->zfree = (free_func) 0;
        zhandle->opaque = (voidpf) 0;

        if (d)
          err = inflateInit2 (zhandle, window_bits);
        else
          {
            err = deflateInit2 (zhandle,
                                comp_level, Z_DEFLATED,
                                window_bits, mem_level, Z_DEFAULT_STRATEGY);
          }
        if (err != Z_OK)
          {
            gnutls_assert ();
            gnutls_free (ret->handle);
            goto cleanup_ret;
          }
      }
      break;
#endif
    case GNUTLS_COMP_LZO:
#ifdef USE_LZO
      /* LZO does not use memory on decompressor */
      if (!d)
        {
          ret->handle = gnutls_malloc (LZO1X_1_MEM_COMPRESS);

          if (ret->handle == NULL)
            {
              gnutls_assert ();
              goto cleanup_ret;
            }
        }
      break;
#endif
    case GNUTLS_COMP_NULL:
    case GNUTLS_COMP_UNKNOWN:
      break;
    }

  return ret;

cleanup_ret:
  gnutls_free (ret);
  return NULL;
}

/* The flag d is the direction (compress, decompress). Non zero is
 * decompress.
 */
void
_gnutls_comp_deinit (comp_hd_t handle, int d)
{
  if (handle != NULL)
    {
      switch (handle->algo)
        {
#ifdef HAVE_LIBZ
        case GNUTLS_COMP_DEFLATE:
          {
            int err;

            if (d)
              err = inflateEnd (handle->handle);
            else
              err = deflateEnd (handle->handle);
            break;
          }
#endif
        default:
          break;
        }
      gnutls_free (handle->handle);
      gnutls_free (handle);

    }
}

/* These functions are memory consuming 
 */

int
_gnutls_compress (comp_hd_t handle, const opaque * plain,
                  size_t plain_size, opaque ** compressed,
                  size_t max_comp_size)
{
  int compressed_size = GNUTLS_E_COMPRESSION_FAILED;

  /* NULL compression is not handled here
   */
  if (handle == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  switch (handle->algo)
    {
#ifdef USE_LZO
    case GNUTLS_COMP_LZO:
      {
        lzo_uint out_len;
        size_t size;
        int err;

        if (_gnutls_lzo1x_1_compress == NULL)
          return GNUTLS_E_COMPRESSION_FAILED;

        size = plain_size + plain_size / 64 + 16 + 3;
        *compressed = gnutls_malloc (size);
        if (*compressed == NULL)
          {
            gnutls_assert ();
            return GNUTLS_E_MEMORY_ERROR;
          }

        err = _gnutls_lzo1x_1_compress (plain, plain_size, *compressed,
                                        &out_len, handle->handle);

        if (err != LZO_E_OK)
          {
            gnutls_assert ();
            gnutls_free (*compressed);
            *compressed = NULL;
            return GNUTLS_E_COMPRESSION_FAILED;
          }

        compressed_size = out_len;
        break;
      }
#endif
#ifdef HAVE_LIBZ
    case GNUTLS_COMP_DEFLATE:
      {
        uLongf size;
        z_stream *zhandle;
        int err;

        size = (plain_size + plain_size) + 10;
        *compressed = gnutls_malloc (size);
        if (*compressed == NULL)
          {
            gnutls_assert ();
            return GNUTLS_E_MEMORY_ERROR;
          }

        zhandle = handle->handle;

        zhandle->next_in = (Bytef *) plain;
        zhandle->avail_in = plain_size;
        zhandle->next_out = (Bytef *) * compressed;
        zhandle->avail_out = size;

        err = deflate (zhandle, Z_SYNC_FLUSH);

        if (err != Z_OK || zhandle->avail_in != 0)
          {
            gnutls_assert ();
            gnutls_free (*compressed);
            *compressed = NULL;
            return GNUTLS_E_COMPRESSION_FAILED;
          }

        compressed_size = size - zhandle->avail_out;
        break;
      }
#endif
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }                           /* switch */

#ifdef COMPRESSION_DEBUG
  _gnutls_debug_log ("Compression ratio: %f\n",
                     (float) ((float) compressed_size / (float) plain_size));
#endif

  if ((size_t) compressed_size > max_comp_size)
    {
      gnutls_free (*compressed);
      *compressed = NULL;
      return GNUTLS_E_COMPRESSION_FAILED;
    }

  return compressed_size;
}



int
_gnutls_decompress (comp_hd_t handle, opaque * compressed,
                    size_t compressed_size, opaque ** plain,
                    size_t max_record_size)
{
  int plain_size = GNUTLS_E_DECOMPRESSION_FAILED;

  if (compressed_size > max_record_size + EXTRA_COMP_SIZE)
    {
      gnutls_assert ();
      return GNUTLS_E_DECOMPRESSION_FAILED;
    }

  /* NULL compression is not handled here
   */

  if (handle == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }

  switch (handle->algo)
    {
#ifdef USE_LZO
    case GNUTLS_COMP_LZO:
      {
        lzo_uint out_size;
        lzo_uint new_size;
        int err;

        if (_gnutls_lzo1x_decompress_safe == NULL)
          return GNUTLS_E_DECOMPRESSION_FAILED;

        *plain = NULL;
        out_size = compressed_size + compressed_size;
        plain_size = 0;

        do
          {
            out_size += 512;
            *plain = gnutls_realloc_fast (*plain, out_size);
            if (*plain == NULL)
              {
                gnutls_assert ();
                return GNUTLS_E_MEMORY_ERROR;
              }

            new_size = out_size;
            err =
              _gnutls_lzo1x_decompress_safe (compressed,
                                             compressed_size, *plain,
                                             &new_size, NULL);

          }
        while ((err == LZO_E_OUTPUT_OVERRUN && out_size < max_record_size));

        if (err != LZO_E_OK)
          {
            gnutls_assert ();
            gnutls_free (*plain);
            *plain = NULL;
            return GNUTLS_E_DECOMPRESSION_FAILED;
          }

        plain_size = new_size;
        break;
      }
#endif
#ifdef HAVE_LIBZ
    case GNUTLS_COMP_DEFLATE:
      {
        uLongf out_size;
        z_stream *zhandle;
        int cur_pos;
        int err;

        *plain = NULL;
        out_size = compressed_size + compressed_size;
        plain_size = 0;

        zhandle = handle->handle;

        zhandle->next_in = (Bytef *) compressed;
        zhandle->avail_in = compressed_size;

        cur_pos = 0;

        do
          {
            out_size += 512;
            *plain = gnutls_realloc_fast (*plain, out_size);
            if (*plain == NULL)
              {
                gnutls_assert ();
                return GNUTLS_E_MEMORY_ERROR;
              }

            zhandle->next_out = (Bytef *) (*plain + cur_pos);
            zhandle->avail_out = out_size - cur_pos;

            err = inflate (zhandle, Z_SYNC_FLUSH);

            cur_pos = out_size - zhandle->avail_out;

          }
        while ((err == Z_BUF_ERROR && zhandle->avail_out == 0
                && out_size < max_record_size)
               || (err == Z_OK && zhandle->avail_in != 0));

        if (err != Z_OK)
          {
            gnutls_assert ();
            gnutls_free (*plain);
            *plain = NULL;
            return GNUTLS_E_DECOMPRESSION_FAILED;
          }

        plain_size = out_size - zhandle->avail_out;
        break;
      }
#endif
    default:
      gnutls_assert ();
      return GNUTLS_E_INTERNAL_ERROR;
    }                           /* switch */

  if ((size_t) plain_size > max_record_size)
    {
      gnutls_assert ();
      gnutls_free (*plain);
      *plain = NULL;
      return GNUTLS_E_DECOMPRESSION_FAILED;
    }

  return plain_size;
}
