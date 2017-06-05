/* $Id: stroke.h 2790 2013-10-09 13:00:04Z tpgww $

libstroke2 - a pointer-device gesture-management library for *NIX
Derived from libstroke 0.5.1 @ www.etla.net/libstroke
Copyright (C) 2008-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1996-1999  Mark F. Willey, ETLA Technical

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this file. If not, see http://www.gnu.org/licenses.
*/

#ifndef __STROKE_H__
#define __STROKE_H__

#define EMBEDDED
#ifndef EMBEDDED
# include <config.h>
#endif

/* opaque "handle" for interacting with low-level functionalty */
typedef struct _Stroke Stroke;

/* The various #defines here and in the corresponsing .c file are not intended
   for build-time configuration */

/* largest number of independent points-lists per Stroke. No major advantage
   from setting this > 1 ? */
#define STROKE_MAX_DEVICES 1

Stroke *stroke_new (int xmax, int ymax);
void stroke_destroy (Stroke *handle);
void stroke_clear (Stroke *handle
#if STROKE_MAX_DEVICES > 1
  , void *device
#endif
);
int stroke_get_count (Stroke *handle
#if STROKE_MAX_DEVICES > 1
  , void *device
#endif
);
void stroke_scale (Stroke *handle, int xmax, int ymax);
void stroke_get_scale (Stroke *handle, int *xmax, int *ymax);
void stroke_limit (Stroke *handle, int binmax);
int stroke_get_limit (Stroke *handl);
int stroke_translate (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  int clear, int readable, char **sequence);
void stroke_fake (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  const char *sequence);
void stroke_record (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  int x, int y);
int stroke_replay (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  void **array);

/* convenience functions */
char *stroke_make_ascii_sequence (const char *raw);
char *stroke_make_raw_sequence (const char *ascii);
char *stroke_verify_sequence (const char *ascii);

#endif /* __STROKE_H__ */
