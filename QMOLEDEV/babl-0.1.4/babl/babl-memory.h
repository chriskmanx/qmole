/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005, Øyvind Kolås.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 */

#ifndef _BABL_MEMORY_H
#define _BABL_MEMORY_H


typedef void * (* BablMallocFunc) (size_t size);
typedef void   (* BablFreeFunc)   (void  *ptr);


void   babl_set_malloc     (BablMallocFunc malloc_function);
void   babl_set_free       (BablFreeFunc   free_function);
int    babl_memory_sanity  (void);

void * babl_malloc         (size_t      size);
void   babl_set_destructor (void       *ptr,
                            int       (*destructor)(void *ptr));

void   babl_free           (void       *ptr,
                            ...);
void * babl_calloc         (size_t      nmemb,
                            size_t      size);
void * babl_realloc        (void       *ptr,
                            size_t      size);

size_t babl_sizeof         (void       *ptr);
void * babl_dup            (void       *ptr);

char * babl_strdup         (const char *s);
char * babl_strcat         (char       *dest,
                            const char *src);

#endif
