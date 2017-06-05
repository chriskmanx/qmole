/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <stdio.h>
#include <dap/sgnl.h>

/* external function definitions */
int 
sgnlproc(void)
{
  int n;
  struct sgnl *p;
#if defined(HAVE_SVR4)
  sigset_t savemask;
  struct sigaction act;
#else
  int savemask;
#endif
  int didwork = 0;

  for (n = 1; n < NSIG; n++) {
    if ((p = sgnls + n)->flag) {
#if defined(HAVE_SVR4)
      sigaction(n, NULL, &act);
      savemask = act.sa_mask;
      sigemptyset(&act.sa_mask);
      sigaddset(&act.sa_mask, n);
      sigaction(n, &act, NULL);
      p->flag = 0;
      act.sa_mask = savemask;
      sigaction(n, &act, NULL);
#else
      savemask = sigblock(sigmask(n));
      p->flag = 0;
      (void) sigsetmask(savemask);
#endif
      (*(p->func)) (n);
      didwork = 1;
    }
  }
  return didwork;
}
