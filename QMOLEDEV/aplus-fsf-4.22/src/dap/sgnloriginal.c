/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <a/development.h>
#include <dap/Warn.h>
#include <dap/sgnl.h>

/* external function definitions */
void 
sgnloriginal(int n)
{
  static char fnc[] = "sgnloriginal";

  if ((n >= 1) && (n < NSIG)) {
    struct sgnl *p = sgnls + n;

    if (p->set != 0) {
#if defined(HAVE_SVR4)
      struct sigaction *ovp = &(p->orig);

      if (sigaction(n, ovp, (struct sigaction *) (0)) != 0) {
	Abort("%t %s(): abort: sigaction(%d): %m\n", fnc, n);
      }
#else
      struct sigvec *ovp = (struct sigvec *)&(p->orig);

      if (sigvec(n, ovp, (struct sigvec *) (0)) != 0) {
	Abort("%t %s(): abort: sigvec(%d): %m\n", fnc, n);
      }
#endif
      p->set = 0;
      bzero((char *) ovp, sizeof(*ovp));
      p->flag = 0;
      p->func = (void (*) ()) (0);
    }
  }
  return;
}
