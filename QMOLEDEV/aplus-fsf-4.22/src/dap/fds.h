#ifndef included_dap_fds_h
#define included_dap_fds_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
#include <sys/types.h>
#include <dap/notsunos4.h>

/* external data declarations */
extern int fds_howmany;
extern int fds_size;
extern int fds_sizeof;
extern fd_set *fds_all;
extern fd_set *fds_none;
extern fd_set *fds_r;
extern fd_set *fds_ra;
extern fd_set *fds_w;
extern fd_set *fds_wa;
extern fd_set *fds_x;
extern fd_set *fds_xa;

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX) || defined(linux)
# ifdef __cplusplus
extern "C" {
# endif
  extern fd_set *fdsalloc( void );
  extern fd_set *fdsalloc_r( void );
  extern int fdsanyset( fd_set * );
  extern void fdsclr( fd_set *, int );
  extern void fdscopy( fd_set *, fd_set * );
  extern fd_set *fdsdup( fd_set * );
  extern fd_set *fdsdup_r( fd_set * );
  extern void fdsfree( fd_set * );
  extern void fdsfresh( int fd );
  extern void fdsinit( void );
  extern int fdsisset( fd_set *, int );
  extern void fdsset( fd_set *, int );
  extern void fdsterm( void );
  extern void fdszero( fd_set *  );
# ifdef __cplusplus
}
# endif
#else
  extern fd_set *fdsalloc();
  extern fd_set *fdsalloc_r();
  extern int fdsanyset();
  extern void fdsclr();
  extern void fdscopy();
  extern fd_set *fdsdup();
  extern fd_set *fdsdup_r();
  extern void fdsfree();
  extern void fdsfresh();
  extern void fdsinit();
  extern int fdsisset();
  extern void fdsset();
  extern void fdsterm();
  extern void fdszero();
#endif

#endif

#ifndef __FDS_BITS
#define __FDS_BITS(set)   ((set)->fds_bits)
#endif
