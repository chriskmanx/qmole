/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/Warn.h>
#include <dap/misc.h>
#include <dap/sgnl.h>

/* external function definitions */
void 
sgnlignore(int n)
{
  static char fnc[] = "sgnlignore";

  if ((n >= 1) && (n < NSIG)) {
    struct sgnl *p = sgnls + n;
#if defined(HAVE_SVR4)
    struct sigaction *ovp;
    struct sigaction vec;

    ovp = (p->set == 0) ? &(p->orig) : (struct sigaction *) (0);
    vec.sa_handler = SIG_IGN;
    sigemptyset(&vec.sa_mask);
    vec.sa_flags = SA_RESTART;
    if (sigaction(n, &vec, ovp) != 0) {
      Abort("%t %s(): abort: sigaction(%d): %m\n", fnc, n);
    }
#else
    struct sigvec *ovp;
    struct sigvec vec;

    ovp = (struct sigvec *)((p->set == 0) ? &(p->orig) :  0);
    vec.sv_handler = SIG_IGN;
    vec.sv_mask = 0;
    vec.sv_flags = 0;
    if (sigvec(n, &vec, ovp) != 0) {
      Abort("%t %s(): abort: sigvec(%d): %m\n", fnc, n);
    }
#endif
    p->set = 1;
    p->func = noop;
  }
  return;
}
