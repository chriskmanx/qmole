/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * p2 is subtracted from p1 and the result is placed in p3. 0 is returned if
 * the operation succeeds.  1 is returned if a positive overflow was
 * encountered while preparing the result.  2 is returned if negative
 * overflow was encountered.  The inputs are assumed to be normalized.
 */

/* header file inclusions */
#include <limits.h>
#include <dap/Warn.h>
#include <dap/tv.h>

/* external function definitions */
int 
tvdiff(struct timeval * p1, struct timeval * p2, struct timeval * p3)
{
  static char fnc[] = "tvdiff";

  if ((p1->tv_sec < (long) (0)) && (p2->tv_sec > (long) (0))) {
    p3->tv_sec = p1->tv_sec - p2->tv_sec;
    if (p3->tv_sec >= (long) (0)) {
      Warn("%t %s(): warning: negative overflow\n", fnc);
      p3->tv_sec = INT_MIN;
      p3->tv_usec = (long) (0);
      return 2;
    }
    p3->tv_usec = p1->tv_usec - p2->tv_usec;
  } else if ((p1->tv_sec > (long) (0)) && (p2->tv_sec < (long) (0))) {
    p3->tv_sec = p1->tv_sec - 1;
    p3->tv_sec -= p2->tv_sec + 1;
    if (p3->tv_sec < (long) (0)) {
      /* overflow positive */
      Warn("%t %s(): warning: positive overflow\n", fnc);
      p3->tv_sec = INT_MAX;
      p3->tv_usec = MILLION_USECS - 1;
      return 1;
    }
    p3->tv_usec = p1->tv_usec + MILLION_USECS;
    p3->tv_usec -= p2->tv_usec - MILLION_USECS;
  } else {
    p3->tv_sec = p1->tv_sec - p2->tv_sec;
    p3->tv_usec = p1->tv_usec - p2->tv_usec;
  }
  return tvnorm(p3);
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
    unittest(printit,
	     (long) (0), (long) (0),
	     (long) (0), (long) (0));
    unittest(printit,
	     (long) (-1), (long) (999999),
	     (long) (0), (long) (999999));
    unittest(printit,
	     (long) (0), (long) (999999),
	     (long) (-1), (long) (0));
    unittest(printit,
	     (long) (-1), (long) (0),
	     (long) (0), (long) (999999));
    unittest(printit,
	     (long) (0), (long) (0),
	     (long) (-1), (long) (999999));
    unittest(printit,
	     (long) (2), (long) (0),
	     (long) (1), (long) (999999));
    unittest(printit,
	     (long) (0x3fffffff), (long) (0),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0x40000000), (long) (0),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0x40000001), (long) (0),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0x40000000), (long) (999999),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0x40000001), (long) (999999),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0x40000000), (long) (0),
	     (long) (0x3fffffff), (long) (999999));
    unittest(printit,
	     (long) (0xc0000000), (long) (0),
	     (long) (0x40000001), (long) (999999));
    unittest(printit,
	     (long) (0xc0000000), (long) (0),
	     (long) (0x40000000), (long) (999999));
    unittest(printit,
	     (long) (0xc0000000), (long) (0),
	     (long) (0x3fffffff), (long) (999999));
    unittest(printit,
	     (long) (0xc0000000), (long) (100),
	     (long) (0x40000000), (long) (101));
  }
  exit(0);
  /* NOTREACHED */
}

static void 
unittest(printit, sec1, usec1, sec2, usec2)
  int printit;
  long sec1;
  long usec1;
  long sec2;
  long usec2;
{
  struct timeval tv1, tv2, tv3;
  int rc;

  tv1.tv_sec = sec1;
  tv1.tv_usec = usec1;
  tv2.tv_sec = sec2;
  tv2.tv_usec = usec2;
  rc = tvdiff(&tv1, &tv2, &tv3);
  if (printit) {
    (void) fprintf(stdout,
		   "tvdiff((%ld, %ld), (%ld, %ld), (%ld, %ld)) = %d\n",
		   sec1, usec1, sec2, usec2, tv3.tv_sec, tv3.tv_usec, rc);

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
