/*
 * timeval.h    1.0 01/12/19
 *
 * Defines gettimeofday, timeval, etc. for Win32
 *
 * By Wu Yongwei
 *
 */
#include "config.h"

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <time.h>

#ifndef __GNUC__
#define EPOCHFILETIME    (116444736000000000i 64)
#else
#define EPOCHFILETIME    (116444736000000000LL)
#endif

struct timeval
{
  long tv_sec;          /* seconds */
  long tv_usec;    /* microseconds */
};

struct timezone
{
  int tz_minuteswest;   /* minutes W of Greenwich */
  int tz_dsttime;       /* type of dst correction */
};


int gettimeofday (struct timeval *tv, struct timezone *tz)
{
  FILETIME      ft;
  LARGE_INTEGER li;
  __int64       t;
  static int    tzflag;

  if (tv)
    {
      GetSystemTimeAsFileTime (&ft);
      li.LowPart  = ft.dwLowDateTime;
      li.HighPart = ft.dwHighDateTime;
      t           = li.QuadPart;/* In 100-nanosecond intervals */
      t          -= EPOCHFILETIME;/* Offset to the Epoch time */
      t          /= 10;         /* In microseconds */
      tv->tv_sec  = (long) (t / 1000000);
      tv->tv_usec = (long) (t % 1000000);
    }

  if (tz)
    {
      if (!tzflag)
        {
          _tzset ();
          tzflag++;
        }
      tz->tz_minuteswest = _timezone / 60;
      tz->tz_dsttime     = _daylight;
    }

  return 0;
}

#endif /* _WIN32 */
