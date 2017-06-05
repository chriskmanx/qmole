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

#include "config.h"
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "babl-mutex.h"

BablMutex  *
babl_mutex_new (void)
{
  BablMutex *mutex = malloc (sizeof (BablMutex));
#ifdef _WIN32
  InitializeCriticalSection (mutex);
#else
  pthread_mutex_init (mutex, NULL);
#endif
  return mutex;
}

void
babl_mutex_destroy (BablMutex *mutex)
{
#ifdef _WIN32
  DeleteCriticalSection (mutex);
#else
  pthread_mutex_destroy(mutex);
#endif
  free (mutex);
}

void
babl_mutex_lock (BablMutex *mutex)
{
#ifdef _WIN32
  EnterCriticalSection (mutex);
#else
  pthread_mutex_lock (mutex);
#endif
}

void
babl_mutex_unlock (BablMutex *mutex)
{
#ifdef _WIN32
  LeaveCriticalSection (mutex);
#else
  pthread_mutex_unlock (mutex);
#endif
}
