/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* convert unsigned long integer to decimal character string */

/* head file inclusions */
#include <dap/ulto.h>

/* internal data definitions */
static unsigned long powers[ULTODEC_SZ] =
{
  0L,				/* zero */
  9L, 99L, 999L,		/* units, tens, hundreds */
  9999L, 99999L, 999999L,	/* thousands */
  9999999L, 99999999L, 999999999L,	/* millions */
  0xffffffffL			/* sentinel */
};

static char digits[10] =
{
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
};

/* external function definitions */
int 
ultodec(long unsigned int value, char *result, int length)
{
  int place;

  /* determine number of places to be used */
  if (value == 0) {
    place = 1;
  } else {
    for (place = 1; value > powers[place]; place++);
  }
  if (place < length) {
    int rc = place + 1;		/* return code */

    while (place-- > 0) {
      unsigned long p = powers[place] + 1;
      int v;

      for (v = 0; value >= p; value -= p, v++);
      *result++ = digits[v];
    }
    *result = '\0';

    return rc;
  } else {
    /* insufficent space in caller provided string */
    return -1;
  }

  /* NOTREACHED */
}

/* internal function definitions */

#ifdef UNITTEST
/* unit test header file inclusions */
#include <stdio.h>

/* unit test function declarations */
extern int main();
extern void unittest();

/* unit test function definitions */
int 
main(argc, argv)
  int argc;
  char *argv[];
{
  int length;
  int count;
  int printit;

  if (argc < 2) {
    length = ULTODEC_SZ;
  } else {
    length = atoi(argv[1]);
  }
  if (argc < 3) {
    count = 1;
    printit = 1;
  } else {
    count = atoi(argv[2]);
    printit = 0;
  }

  while (count-- > 0) {
    unittest((unsigned long) (0), length, printit);
    unittest((unsigned long) (1), length, printit);
    unittest((unsigned long) (2), length, printit);
    unittest((unsigned long) (3), length, printit);
    unittest((unsigned long) (4), length, printit);
    unittest((unsigned long) (5), length, printit);
    unittest((unsigned long) (6), length, printit);
    unittest((unsigned long) (7), length, printit);
    unittest((unsigned long) (8), length, printit);
    unittest((unsigned long) (9), length, printit);
    unittest((unsigned long) (10), length, printit);
    unittest((unsigned long) (19), length, printit);
    unittest((unsigned long) (28), length, printit);
    unittest((unsigned long) (37), length, printit);
    unittest((unsigned long) (46), length, printit);
    unittest((unsigned long) (55), length, printit);
    unittest((unsigned long) (64), length, printit);
    unittest((unsigned long) (73), length, printit);
    unittest((unsigned long) (82), length, printit);
    unittest((unsigned long) (91), length, printit);
    unittest((unsigned long) (99), length, printit);
    unittest((unsigned long) (100), length, printit);
    unittest((unsigned long) (109), length, printit);
    unittest((unsigned long) (280), length, printit);
    unittest((unsigned long) (372), length, printit);
    unittest((unsigned long) (406), length, printit);
    unittest((unsigned long) (525), length, printit);
    unittest((unsigned long) (640), length, printit);
    unittest((unsigned long) (731), length, printit);
    unittest((unsigned long) (802), length, printit);
    unittest((unsigned long) (911), length, printit);
    unittest((unsigned long) (999), length, printit);
    unittest((unsigned long) (1000), length, printit);
    unittest((unsigned long) (9999), length, printit);
    unittest((unsigned long) (10000), length, printit);
    unittest((unsigned long) (99999), length, printit);
    unittest((unsigned long) (100000), length, printit);
    unittest((unsigned long) (999999), length, printit);
    unittest((unsigned long) (1000000), length, printit);
    unittest((unsigned long) (9999999), length, printit);
    unittest((unsigned long) (10000000), length, printit);
    unittest((unsigned long) (99999999), length, printit);
    unittest((unsigned long) (100000000), length, printit);
    unittest((unsigned long) (999999999), length, printit);
    unittest((unsigned long) (1000000000), length, printit);
    unittest((unsigned long) (2000000000), length, printit);
    unittest((unsigned long) (3000000000), length, printit);
    unittest((unsigned long) (4000000000), length, printit);
  }
  exit(0);
  /* NOTREACHED */
}

static void 
unittest(value, length, printit)
  unsigned long value;
  int length;
  int printit;
{
  int rc;
  static char result[ULTODEC_SZ];

  result[0] = '\0';
  rc = ultodec(value, result, length);
  if (printit) {
    (void) fprintf(stdout,
		   "ultodec(%lu, %s, %d) = %d\n",
		   value, result, length, rc);

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
