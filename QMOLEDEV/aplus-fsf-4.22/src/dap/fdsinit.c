/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

#include <a/development.h>
/* header file inclusions */
#include <unistd.h>
#include <dap/Warn.h>
#include <dap/fds.h>
#include <notsunos4.h>
#if defined(linux)
#include <sys/param.h>
#endif

/* external function definitions */
void 
fdsinit(void)
{
  static char fnc[] = "fdsinit";
  int i;

  if ((i = getdtablesize()) < 0) {
    Warn("%t %s(): warning: getdtablesize(): %m\n", fnc);
  } else {
    fds_size = i;
  }

  fds_howmany = howmany(fds_size, NFDBITS);
  fds_sizeof = fds_howmany * sizeof(fd_mask);

  fds_all = fdsalloc();
  for (i = 0; i < fds_howmany; i++) {
    __FDS_BITS(fds_all)[i] = (fd_mask) (~0);
  }
  fds_none = fdsalloc();
  fds_r = fdsalloc();
  fds_ra = fdsalloc();
  fds_w = fdsalloc();
  fds_wa = fdsalloc();
  fds_x = fdsalloc();
  fds_xa = fdsalloc();

  return;
}
