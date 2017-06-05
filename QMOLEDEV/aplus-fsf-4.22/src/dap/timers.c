/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * For timernext() to work, the d member of timers node must be a 0 pointer.
 */

/* header file inclusions */
#include <dap/node.h>
#include <dap/timer.h>

/* external data definitions */
struct node timers = {&timers, &timers, (void *) (0)};
