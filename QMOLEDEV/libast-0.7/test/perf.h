/*
 * Copyright (C) 1997-2004, Michael Jennings
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

#ifndef _LIBAST_PERF_H_
#  define _LIBAST_PERF_H_

#  define PERF_SET_REPS(c)     do {rep_cnt = (c) * rep_mult; if (rep_cnt < (c)) {rep_cnt = (size_t) -1;}} while (0)
#  define TDIFF(t1, t2)        (((t2).tv_sec + ((double) (t2).tv_usec / 1000000.0)) \
                                 - ((t1).tv_sec + ((double) (t1).tv_usec / 1000000.0)))
#  define PERF_BEGIN(s)        do { \
                                   tnum = 0; \
                                   printf("Profiling " s "..."); \
                                   fflush(stdout); \
                                   gettimeofday(&time1, NULL); \
                                   for (prof_counter = 0; prof_counter < rep_cnt; prof_counter++) { 
#  define PERF_TEST(x)             do { tnum++; x ; } while (0)
#  define PERF_END()               } \
                                   gettimeofday(&time2, NULL); \
                                   time_diff = TDIFF(time1, time2); \
                                   printf("%lu iterations in %6.5g seconds, %6.5g seconds per iteration\n", \
                                          tnum, time_diff, time_diff / tnum); \
                               } while (0)
#  define PERF_ENDED(s)        printf(s " profiling done.\n\n"); return 0;
#  define PERF_NOTICE(s)       printf("%s\n", s)

#endif
