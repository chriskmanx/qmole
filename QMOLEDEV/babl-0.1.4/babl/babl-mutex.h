/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2009, Øyvind Kolås.
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

#ifndef _BABL_MUTEX_H
#define _BABL_MUTEX_H

#ifndef _WIN32
#define __USE_GNU 1
#include <pthread.h>
#else
#include <windows.h>
#endif

#ifdef _WIN32
  typedef  CRITICAL_SECTION   BablMutex;
#else
  typedef  pthread_mutex_t   BablMutex;
#endif

BablMutex* babl_mutex_new     (void);
void       babl_mutex_destroy (BablMutex *mutex);
void       babl_mutex_lock    (BablMutex *mutex);
void       babl_mutex_unlock  (BablMutex *mutex);

#endif
