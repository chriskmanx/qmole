/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * p1 is compared to p2.  If it is earlier, -1 is returned. If it is latter 1
 * is returned.  If it is the same, 0 is returned.  It is assumed that the
 * inputs are normalized.
 */

/* header file inclusions */
#include <limits.h>
#include <dap/tv.h>

/* external function definitions */
int 
tvcmp(struct timeval * p1, struct timeval * p2)
{
  if (p1->tv_sec < p2->tv_sec)
    return -1;
  else if (p1->tv_sec > p2->tv_sec)
    return 1;
  else if (p1->tv_usec < p2->tv_usec)
    return -1;
  else if (p1->tv_usec > p2->tv_usec)
    return 1;
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
    unittest(printit,
	     (long) (0), (long) (500000),
	     (long) (0), (long) (500000));
    unittest(printit,
	     (long) (1), (long) (999999),
	     (long) (12), (long) (0));
    unittest(printit,
	     (long) (12), (long) (0),
	     (long) (1), (long) (999999));
    unittest(printit,
	     (long) (-12), (long) (0),
	     (long) (-1), (long) (999999));
    unittest(printit,
	     (long) (-1), (long) (999999),
	     (long) (-12), (long) (0));
    unittest(printit,
	     (long) (12), (long) (100),
	     (long) (-12), (long) (99));
    unittest(printit,
	     (long) (12), (long) (1),
	     (long) (12), (long) (999999));
    unittest(printit,
	     (long) (0x7fffffff), (long) (0),
	     (long) (0), (long) (999999));
    unittest(printit,
	     (long) (0x40000000), (long) (0),
	     (long) (0x40000000), (long) (999999));
    unittest(printit,
	     (long) (0x3fffffff), (long) (0),
	     (long) (0x40000000), (long) (999999));
    unittest(printit,
	     (long) (0x3fffffff), (long) (999999),
	     (long) (0x40000000), (long) (999999));
    unittest(printit,
	     (long) (0x3ffffffe), (long) (999999),
	     (long) (0x40000000), (long) (999999));
    unittest(printit,
	     (long) (0x3fffffff), (long) (999999),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0xbffffffd), (long) (999999),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0xbffffffe), (long) (999999),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0xbfffffff), (long) (999999),
	     (long) (0xc0000000), (long) (999999));
    unittest(printit,
	     (long) (0xc0000000), (long) (999999),
	     (long) (0xc0000000), (long) (999999));
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
  struct timeval tv1, tv2;
  int rc;

  tv1.tv_sec = sec1;
  tv1.tv_usec = usec1;
  tv2.tv_sec = sec2;
  tv2.tv_usec = usec2;
  rc = tvcmp(&tv1, &tv2);
  if (printit) {
    (void) fprintf(stdout,
		   "tvcmp((%ld, %ld), (%ld, %ld)) = %d\n",
		   sec1, usec1, sec2, usec2, rc);

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
