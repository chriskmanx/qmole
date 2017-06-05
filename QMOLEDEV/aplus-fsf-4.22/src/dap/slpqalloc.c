/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * used to create a new sleep queue.  The initial signal count and maximum
 * over count may be specified.
 */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/node.h>
#include <dap/slpq.h>

/* external function definitions */
struct slpq *
slpqalloc(int initwakes, int maxwakes)
{
  struct slpq *p = (struct slpq *) balloc(sizeof(*p));

  /* ensure wakes is never greater than maxwakes */
  if (initwakes > maxwakes)
    initwakes = maxwakes;

  p->wq = nodealloc();		/* allocate head of slpqent wait queue */
  p->wakes = initwakes;		/* set wakes to initwakes value */
  p->maxwakes = maxwakes;	/* set maxwakes value as given */

  return p;			/* return a pointer to the sleep queue */
}
