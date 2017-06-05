/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2009 Martin Nordholts
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

#include <math.h>
#include <pthread.h>

#include "babl.h"


#define N_THREADS               10
#define N_ITERATIONS_PER_THREAD 100


static void *
babl_fish_path_stress_test_thread_func (void *not_used)
{
  int i;

  for (i = 0; i < N_ITERATIONS_PER_THREAD; i++)
    {
      /* Try to get a fish with an as complex conversion path as
       * possible
       */
      Babl *fish = babl_fish ("R'G'B'A u16", "YA double");

      /* Just do something random with the fish */
      babl_get_name (fish);
    }

  return NULL;
}

int
main (int    argc,
      char **argv)
{
  pthread_t threads[N_THREADS];
  int       i;

  babl_init ();

  /* Run a few threads at the same time */
  for (i = 0; i < N_THREADS; i++)
    {
      pthread_create (&threads[i],
                      NULL, /* attr */
                      babl_fish_path_stress_test_thread_func,
                      NULL /* arg */);
     }

  /* Wait for them all to finish */
  for (i = 0; i < N_THREADS; i++)
    {
      pthread_join (threads[i],
                    NULL /* thread_return */);
    }

  babl_exit ();

  /* If we didn't crash we assume we're OK. We might want to add more
   * asserts in the test later
   */
  return 0;
}
