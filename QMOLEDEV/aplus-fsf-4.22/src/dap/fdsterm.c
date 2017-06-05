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
#include <dap/fds.h>

/* external function definitions */
void 
fdsterm(void)
{
  static char fnc[] = "fdsterm";

  if (fdsanyset(fds_xa))
    Warn("%t %s(): warning: fds_xa is not empty\n", fnc);
  fdsfree(fds_xa);
  fds_xa = (fd_set *) (0);

  if (fdsanyset(fds_x))
    Warn("%t %s(): warning: fds_x is not empty\n", fnc);
  fdsfree(fds_x);
  fds_x = (fd_set *) (0);

  if (fdsanyset(fds_wa))
    Warn("%t %s(): warning: fds_wa is not empty\n", fnc);
  fdsfree(fds_wa);
  fds_wa = (fd_set *) (0);

  if (fdsanyset(fds_w))
    Warn("%t %s(): warning: fds_w is not empty\n", fnc);
  fdsfree(fds_w);
  fds_w = (fd_set *) (0);

  if (fdsanyset(fds_ra))
    Warn("%t %s(): warning: fds_ra is not empty\n", fnc);
  fdsfree(fds_ra);
  fds_ra = (fd_set *) (0);

  if (fdsanyset(fds_r))
    Warn("%t %s(): warning: fds_r is not empty\n", fnc);
  fdsfree(fds_r);
  fds_r = (fd_set *) (0);

  if (fdsanyset(fds_none))
    Warn("%t %s(): warning: fds_none is not empty\n", fnc);
  fdsfree(fds_none);
  fds_none = (fd_set *) (0);

  fdsfree(fds_all);
  fds_all = (fd_set *) (0);

  return;
}
