/* 
 * Copyright (C) 2002 Red Hat, Inc.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */


#ifndef __SN_UTIL_H__
#define __SN_UTIL_H__

/* Guard C code in headers, while including them from C++ */
#ifdef  __cplusplus
# define SN_BEGIN_DECLS  extern "C" {
# define SN_END_DECLS    }
#else
# define SN_BEGIN_DECLS
# define SN_END_DECLS
#endif

SN_BEGIN_DECLS

typedef unsigned long sn_size_t;
typedef int           sn_bool_t;

/* Padding in vtables */
typedef void (* SnPaddingFunc) (void);
/* Free data */
typedef void (* SnFreeFunc)    (void *data);

void* sn_malloc      (sn_size_t  n_bytes);
void* sn_malloc0     (sn_size_t  n_bytes);
void* sn_realloc     (void      *mem,
                      sn_size_t  n_bytes);
void  sn_free        (void      *mem);
void* sn_try_malloc  (sn_size_t  n_bytes);
void* sn_try_realloc (void      *mem,
                      sn_size_t  n_bytes);

/* Convenience memory allocators
 */
#define sn_new(struct_type, n_structs)		\
    ((struct_type *) sn_malloc (((sn_size_t) sizeof (struct_type)) * ((sn_size_t) (n_structs))))
#define sn_new0(struct_type, n_structs)		\
    ((struct_type *) sn_malloc0 (((sn_size_t) sizeof (struct_type)) * ((sn_size_t) (n_structs))))
#define sn_renew(struct_type, mem, n_structs)	\
    ((struct_type *) sn_realloc ((mem), ((sn_size_t) sizeof (struct_type)) * ((sn_size_t) (n_structs))))


/* Memory allocation virtualization, so you can override memory
 * allocation behavior.  sn_mem_set_vtable() has to be the very first
 * libsn function called if being used
 */
typedef struct
{
  void* (*malloc)      (sn_size_t    n_bytes);
  void* (*realloc)     (void        *mem,
                        sn_size_t    n_bytes);
  void  (*free)        (void        *mem);
  /* optional */
  void* (*calloc)      (sn_size_t    n_blocks,
                        sn_size_t    n_block_bytes);
  void* (*try_malloc)  (sn_size_t    n_bytes);
  void* (*try_realloc) (void        *mem,
                        sn_size_t    n_bytes);
  SnPaddingFunc        padding1;
  SnPaddingFunc        padding2;
} SnMemVTable;

void      sn_mem_set_vtable       (SnMemVTable *vtable);
sn_bool_t sn_mem_is_system_malloc (void);

typedef sn_bool_t (* SnUtf8ValidateFunc) (const char *str,
                                          int         max_len);

void sn_set_utf8_validator (SnUtf8ValidateFunc validate_func);

SN_END_DECLS

#endif /* __SN_UTIL_H__ */
