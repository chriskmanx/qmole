/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * Returns true if test lies in the range from low to high, inclusively. If
 * low equals high, test must be the same as this common value. If high is
 * less than low, then it is assumed that high has wrapped through a top
 * value (ULONG_MAX) and so test must be greater than or equal to low or less
 * then or equal to high.  This is implemented by assuming that we are
 * dealing with unsigned values.  Then the test range is shift back to zero
 * by subtracting low from test and high.  Then is the shifted test value is
 * less than or equal to the shifted high value, the betweenness test is
 * passed.  Otherwise, it is failed.
 */

/* external function definitions */
int 
ulbetween(long unsigned int test, long unsigned int low, long unsigned int high)
{
  if ((test - low) <= (high - low))
    return 1;
  return 0;
}
