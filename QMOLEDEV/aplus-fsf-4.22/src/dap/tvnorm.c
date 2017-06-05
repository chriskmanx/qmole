/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * If sec_i and usec_i are the initial time value members, and sec_f and
 * usec_f are the final time value members, then the normalized time value
 * satisfies 1000000 * sec_f + usec_f = 1000000 * sec_i + usec_i and 0 <=
 * usec_f < 1000000 Normalization is optimized for normalizing sums and
 * differences of normalized time values.
 * 
 * returns 0 if successful returns 1 if normalization caused overflow of
 * positive time returns 2 if normalization caused overflow of negative time
 * 
 * The present implementation assumes a signed 2's complement integer
 * representation for type time_t.  Furthermore, it is assumed that integer
 * addition overflow is not trapped.
 * 
 * Upon overflow to either positive or negative values, a warning message is
 * logged and the maximum or minumum possible normalized timeval is
 * substituted.
 */

/* header file inclusions */
#include <limits.h>
#include <dap/Warn.h>
#include <dap/tv.h>

/* external function definitions */
int 
tvnorm(struct timeval * p)
{
  static char fnc[] = "tvnorm";
  long usec = p->tv_usec;

  if (usec >= MILLION_USECS) {
    long sec;

    if (usec >= (2 * MILLION_USECS)) {
      /*
       * usec is not the result of sum of two normalized time values.  We
       * therefore use slow division and remainder to normalize it.
       */
      sec = p->tv_sec + usec / MILLION_USECS;
      usec %= MILLION_USECS;
    } else {
      /* we use fast reduction */
      sec = p->tv_sec + 1;
      usec -= MILLION_USECS;
    }
    if (sec < p->tv_sec) {
      /* overflow positive */
      Warn("%t %s(): warning: positive overflow\n", fnc);
      p->tv_sec = INT_MAX;
      p->tv_usec = MILLION_USECS - 1;
      return 1;
    }
    p->tv_sec = sec;
    p->tv_usec = usec;
  } else if (usec < (long) (0)) {
    long sec;

    if ((usec + MILLION_USECS) < (long) (0)) {
      /*
       * usec is not the result of diff of two normalized time values.  We
       * therefore use slow division and remainder to normalize it.
       */
      usec = -1 - usec;		/* no overflow in two-complement */
      sec = (p->tv_sec - 1) - (usec / MILLION_USECS);
      usec = (MILLION_USECS - 1) - (usec % MILLION_USECS);
    } else {
      /* we use fast reduction */
      sec = p->tv_sec - 1;
      usec += MILLION_USECS;
    }
    if (sec > p->tv_sec) {
      /* overflow negative */
      Warn("%t %s(): warning: negative overflow\n", fnc);
      p->tv_sec = INT_MIN;
      p->tv_usec = (long) (0);
      return 2;
    }
    p->tv_sec = sec;
    p->tv_usec = usec;
  }
  return 0;
}

#ifdef UNITTEST
/* unit test header file inclusions */
#include <stdio.h>
#include <dap/dap.h>

/* unit test function declarations */
extern int main();
extern void unittest();

/* unit test function definitions */
int 
main(argc, argv)
  int argc;
  char *argv[];
{
  int count;
  int printit;

  if (argc < 2) {
    count = 1;
    printit = 1;
  } else {
    count = atoi(argv[1]);
    printit = 0;
  }

  while (count-- > 0) {
    unittest(printit, (long) (0), (long) (1000000));
    unittest(printit, (long) (0), (long) (1999999));
    unittest(printit, (long) (0), (long) (2000000));
    unittest(printit, (long) (0), (long) (2999999));
    unittest(printit, (long) (0), (long) (3000000));
    unittest(printit, (long) (0), (long) (12000000));
    unittest(printit, (long) (0), (long) (100000000));
    unittest(printit, (long) (0x7fffffff), (long) (1999999));
    unittest(printit, (long) (0x7ffffffe), (long) (1999999));
    unittest(printit, (long) (0x7ffffffd), (long) (2999999));
    unittest(printit, (long) (0), (long) (-1));
    unittest(printit, (long) (0), (long) (-999999));
    unittest(printit, (long) (0), (long) (-1000000));
    unittest(printit, (long) (0), (long) (-1000001));
    unittest(printit, (long) (0), (long) (-1999999));
    unittest(printit, (long) (0), (long) (-2000000));
    unittest(printit, (long) (0), (long) (-2000001));
    unittest(printit, (long) (0x80000000), (long) (999999));
    unittest(printit, (long) (0x80000000), (long) (1999999));
    unittest(printit, (long) (0x80000000), (long) (-1));
    unittest(printit, (long) (0x80000001), (long) (-1));
    unittest(printit, (long) (0x80000002), (long) (-1000001));
  }
  exit(0);
  /* NOTREACHED */
}

static void 
unittest(printit, sec, usec)
  int printit;
  long sec;
  long usec;
{
  struct timeval tv;
  int rc;

  tv.tv_sec = sec;
  tv.tv_usec = usec;
  rc = tvnorm(&tv);
  if (printit) {
    (void) fprintf(stdout,
		   "tvnorm((%ld, %ld)-->(%ld, %ld)) = %d\n",
		   sec, usec, tv.tv_sec, tv.tv_usec, rc);

/* Lexa redefines fflush in stdio.h      */
/* which results in an unresolved symbol */
#if defined(__edgfe) && defined(fflush)
#undef fflush
#endif
/******************************************/

    fflush(stdout);
  }
  return;
}
#endif
