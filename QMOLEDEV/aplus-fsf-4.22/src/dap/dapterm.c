/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/tod.h>
#include <dap/node.h>
#include <dap/fds.h>
#include <dap/chan.h>
#include <dap/sgnl.h>
#include <dap/slpq.h>
#include <dap/timer.h>
#include <dap/misc.h>

/* external function definitions */
void 
dapterm(void)
{
  struct node *hp;
  struct node *np;
  int i;
  char *s;

  dapInitialized = 0;
  dapZeroTimeout = 0;
  dapbreak = 0;
  hp = &timers;
  while ((np = hp->f) != hp)
    timerclr(TIMERAT(np));
  hp = &chans;
  while ((np = hp->f) != hp)
    chanclose(CHANAT(np));
  hp = &slpqents;
  while ((np = hp->f) != hp)
    slpqgiveup(SLPQENTAT(np));
  for (i = 1; i < NSIG; i++)
    sgnloriginal(i);
  fdsterm();
  s = "UNKNOWN";
  bcopy(s, hostname_l, strlen(s) + 1);
  dap_tod_last.tv_sec = (long) (0);
  dap_tod_last.tv_usec = (long) (0);

  return;
}
