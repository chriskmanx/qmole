/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/fds.h>
#include <notsunos4.h>
#if defined(linux)
#include <sys/param.h>
#endif

/* external data definitions */
int fds_howmany = howmany(FD_SETSIZE, NFDBITS);
int fds_size = FD_SETSIZE;
int fds_sizeof = howmany(FD_SETSIZE, NFDBITS) * sizeof(fd_mask);
fd_set *fds_all = (fd_set *) (0);
fd_set *fds_none = (fd_set *) (0);
fd_set *fds_r = (fd_set *) (0);
fd_set *fds_ra = (fd_set *) (0);
fd_set *fds_w = (fd_set *) (0);
fd_set *fds_wa = (fd_set *) (0);
fd_set *fds_x = (fd_set *) (0);
fd_set *fds_xa = (fd_set *) (0);
