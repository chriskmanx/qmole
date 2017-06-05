/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* external function definitions */
void slpqimmed(void (*func) (), void *arg) {
  if (func != (void (*) ()) (0))
    (*func) (arg);
}
