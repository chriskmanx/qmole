/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <sys/types.h>
#include <dap/notsunos4.h>
#include <dap/Warn.h>
#include <dap/misc.h>
#include <dap/sgnl.h>

/* internal macro declarations */
#ifdef PRESUNOS4
#define HANDLER_RETURN_TYPE		int
#define HANDLER_RETURN			return 0
#else
#define HANDLER_RETURN_TYPE		void
#define HANDLER_RETURN			return
#endif

/* internal function declarations */
static HANDLER_RETURN_TYPE handler(int n);

/* external function definitions */
void 
sgnlcatch(int n, void (*func) ())
{
  static char fnc[] = "sgnlcatch";

  if ((n >= 1) && (n < NSIG)) {
    struct sgnl *p = sgnls + n;
#if defined(HAVE_SVR4)  
    struct sigaction *ovp;
    struct sigaction vec;
    ovp = (p->set == 0) ? &(p->orig) : (struct sigaction *) (0);
    vec.sa_handler = handler;
    sigemptyset(&vec.sa_mask);
    vec.sa_flags = 0;
    if (sigaction(n, &vec, ovp) != 0) {
      Abort("%t %s(): abort: sigaction(%d): %m\n", fnc, n);
    }
#else
    struct sigvec *ovp;
    struct sigvec vec;
    ovp = (struct sigvec *)((p->set == 0) ? &(p->orig) :  0);
    vec.sv_handler = handler;
    vec.sv_mask = 0;
    vec.sv_flags = SV_INTERRUPT;
    if (sigvec(n, &vec, ovp) != 0) {
      Abort("%t %s(): abort: sigvec(%d): %m\n", fnc, n);
    }
#endif

    p->set = 1;
    p->func = (func == (void (*) ()) (0)) ? noop : func;
  }
  return;
}

/* internal function definitions */
static HANDLER_RETURN_TYPE 
handler(int n)
{
  sgnls[n].flag = 1;
  HANDLER_RETURN;
}
