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

#ifndef _LIBAST_TEST_H_
#  define _LIBAST_TEST_H_

#  define TEST_NOTICE(s)                       do {printf("%s\n", s); fflush(stdout);} while (0)
#  define TEST_BEGIN(s)                        do {tnum = 1; printf("Testing %s...", s); fflush(stdout);} while (0)
#  define TEST_PASS()                          do {printf("passed (%hu).\n", tnum - 1); fflush(stdout);} while (0)
#  define TEST_FAIL(t)                         do { \
                                                   printf("failed at %s:%d (test #%hu):  Test \"%s\" returned %d.\n", \
                                                          __FILE__, __LINE__, tnum, #t, (t)); \
                                                   fflush(stdout); \
                                                   return 1; \
                                               } while (0)
#  define TEST_FAIL_IF(t)                      do {if (t) {TEST_FAIL(t);} tnum++;} while (0)
#  define TEST_EXPECT(t)                       do {if (t) {tnum++; TEST_PASS();} else {TEST_FAIL(t);}} while (0)
#  define TEST_PASSED(s)                       do {printf("All %s tests passed.\n\n", s); fflush(stdout); return 0;} while (0)

#endif
