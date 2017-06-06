/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _PROFILE_H
# define _PROFILE_H

#ifdef ENABLE_PROFILE

# include <sys/time.h>
# include <unistd.h>

typedef struct {
  const char *func_name;
  struct timeval start_time, end_time;
} prof_time_t;

# define PROF_INIT(f)     prof_time_t f##_prof_time = { #f, { 0, 0 }, { 0, 0 } }; gettimeofday(& f##_prof_time.start_time, NULL)
# define PROF_DONE(f)     gettimeofday(& f##_prof_time.end_time, NULL)
# define PROF_TIME(f)     do {prof_time_t t = f##_prof_time; unsigned long s, u; s = t.end_time.tv_sec - t.start_time.tv_sec; u = t.end_time.tv_usec - t.start_time.tv_usec; \
                              if (u > 1000000UL) {s--; u += 1000000UL;} D_PROFILE(("Elapsed time for function %s:  %d.%06d seconds.\n", #f, s, u));} while (0)
# define PROF_FUNC(f, c)  do {PROF_INIT(f); c; PROF_DONE(f); PROF_TIME(f);} while (0)
#else
# define PROF_INIT(f)     NOP
# define PROF_DONE(f)     NOP
# define PROF_TIME(f)     NOP
# define PROF_FUNC(f, c)  c;
#endif /* ENABLE_PROFILE */

#endif	/* _PROFILE_H */
