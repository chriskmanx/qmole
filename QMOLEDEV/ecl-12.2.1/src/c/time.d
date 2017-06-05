/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    time.c -- Time routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <limits.h>
#include <time.h>
#ifndef _MSC_VER
# include <unistd.h>
# include <errno.h>
#endif
#if defined(_MSC_VER) || defined(__MINGW32__)
# include <windows.h>
# include <winsock.h>
#endif

#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#include <ecl/internal.h>
#ifdef HAVE_TIMES
# include <sys/times.h>
#endif
#ifdef HAVE_GETRUSAGE
# include <sys/time.h>
# include <sys/resource.h>
#endif
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#include <ecl/impl/math_fenv.h>

#if !defined(HAVE_GETTIMEOFDAY) && !defined(HAVE_GETRUSAGE) && !defined(ECL_MS_WINDOWS_HOST)
struct timeval {
	long tv_sec;
	long tv_usec;
};
#endif

static struct timeval beginning;

static void
get_real_time(struct timeval *tv)
{
#if defined(HAVE_GETTIMEOFDAY) && !defined(ECL_MS_WINDOWS_HOST)
	struct timezone tz;
	gettimeofday(tv, &tz);
#else
# if defined(ECL_MS_WINDOWS_HOST)
	DWORD x = GetTickCount();
	tv->tv_sec = x / 1000;
	tv->tv_usec = (x % 1000) * 1000;
# else
	time_t = time(0);
	tv->tv_sec = time_t;
	tv->tv_usec = 0;
# endif
#endif
}

static void
get_run_time(struct timeval *tv)
{
#ifdef HAVE_GETRUSAGE
	struct rusage r;
	getrusage(RUSAGE_SELF, &r);
	*tv = r.ru_utime;
#else
# ifdef HAVE_TIMES
	struct tms buf;
	times(&buf);
	tv->tv_sec = buf.tms_utime / CLK_TCK;
	tv->tv_usec = (buf.tms_utime % CLK_TCK) * 1000000;
# else
#  if defined(ECL_MS_WINDOWS_HOST)
	FILETIME creation_time;
	FILETIME exit_time;
	FILETIME kernel_time;
	FILETIME user_time;
	ULARGE_INTEGER ui_kernel_time;
	ULARGE_INTEGER ui_user_time;
	if (!GetProcessTimes(GetCurrentProcess(),
	                     &creation_time,
	                     &exit_time,
	                     &kernel_time,
	                     &user_time))
	    FEwin32_error("GetProcessTimes() failed", 0);
	ui_kernel_time.HighPart = kernel_time.dwHighDateTime;
	ui_kernel_time.LowPart  = kernel_time.dwLowDateTime;
	ui_user_time.HighPart = user_time.dwHighDateTime;
	ui_user_time.LowPart  = user_time.dwLowDateTime;
	ui_kernel_time.QuadPart += ui_user_time.QuadPart;
	ui_kernel_time.QuadPart /= 10000;
	tv->tv_sec = ui_kernel_time.QuadPart / 1000;
	tv->tv_usec = (ui_kernel_time.QuadPart % 1000) * 1000;
#  else
	get_real_time(tv);
#  endif
# endif
#endif
}

cl_fixnum
ecl_runtime(void)
{
	struct timeval tv;
	get_run_time(&tv);
	return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

cl_object
cl_sleep(cl_object z)
{
#ifdef HAVE_NANOSLEEP
	struct timespec tm;
#endif
        double time;
	/* INV: ecl_minusp() makes sure `z' is real */
	if (ecl_minusp(z))
		cl_error(9, @'simple-type-error', @':format-control',
			    make_constant_base_string("Not a non-negative number ~S"),
			    @':format-arguments', cl_list(1, z),
			    @':expected-type', @'real', @':datum', z);
        /* Compute time without overflows */
        ECL_WITHOUT_FPE_BEGIN {
                time = ecl_to_double(z);
                if (isnan(time) || !isfinite(time) || (time > INT_MAX)) {
                        time = INT_MAX;
                } else if (time < 1e-9) {
                        time = 1e-9;
                }
        } ECL_WITHOUT_FPE_END;
#ifdef HAVE_NANOSLEEP
        {
                int code;
                tm.tv_sec = (time_t)floor(time);
                tm.tv_nsec = (long)((time - floor(time)) * 1e9);
        AGAIN:
                ecl_disable_interrupts();
                code = nanosleep(&tm, NULL);
                {
                        int old_errno = errno;
                        ecl_enable_interrupts();
                        if (code < 0 && old_errno == EINTR) {
                                goto AGAIN;
                        }
                }
        }
#else
#if defined (ECL_MS_WINDOWS_HOST)
        {
                SleepEx((long)(time * 1000), TRUE);
        }
#else
        {
                int t = (int)time;
                for (t = (time + 0.5); t > 1000; t -= 1000)
                        sleep(1000);
                sleep(t);
        }
#endif
#endif
	@(return Cnil)
}

static cl_object
timeval_to_time(long sec, long usec)
{
	/* This can be probably improved.  Right now, too many
	   rounding errors, but if we use the lisp routines then it
	   slows downs and measures become too imprecise. */
	double x = sec * 1000.0 + usec / 1000.0;
	return MAKE_FIXNUM((cl_fixnum)x);
}

cl_object
cl_get_internal_run_time()
{
	struct timeval tv;
	get_run_time(&tv);
	@(return timeval_to_time(tv.tv_sec, tv.tv_usec))
}

cl_object
cl_get_internal_real_time()
{
	struct timeval tv;
	get_real_time(&tv);
	@(return timeval_to_time(tv.tv_sec - beginning.tv_sec,
				 tv.tv_usec - beginning.tv_usec))
}

cl_object
cl_get_universal_time()
{
	cl_object utc = ecl_make_integer(time(0));
	@(return ecl_plus(utc, cl_core.Jan1st1970UT))
}

void
init_unixtime(void)
{
	get_real_time(&beginning);

	ECL_SET(@'internal-time-units-per-second', MAKE_FIXNUM(1000));

	cl_core.Jan1st1970UT =
	    ecl_times(MAKE_FIXNUM(24 * 60 * 60),
			 MAKE_FIXNUM(17 + 365 * 70));
}
