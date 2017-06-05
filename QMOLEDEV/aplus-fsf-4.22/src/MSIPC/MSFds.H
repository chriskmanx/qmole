#ifndef MSFdsHEADER
#define MSFdsHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <sys/types.h>

#if defined(MS_FDSET_IN_SYS_SELECT)
#include <sys/select.h>
#endif

class MSIPCExport MSFds
{
private:
  int    _howmany;
  int    _size;
  int    _sizeof;
  fd_set _all;
  fd_set _none;
  fd_set _r;
  fd_set _ra;
  fd_set _w;
  fd_set _wa;
  fd_set _x;
  fd_set _xa;

  void init(void);

public:
  MSFds(void);
  ~MSFds(void);

  int howMany(void)  { return _howmany; }
  int size(void)     { return _size; }
  int sizeOf(void)   { return _sizeof; }
  fd_set *all(void)  { return &_all; }
  fd_set *none(void) { return &_none; }
  fd_set *r(void)    { return &_r; }
  fd_set *ra(void)   { return &_ra; }
  fd_set *w(void)    { return &_w; }
  fd_set *wa(void)   { return &_wa; }
  fd_set *x(void)    { return &_x; }
  fd_set *xa(void)   { return &_xa; }

  int fdsanyset(fd_set *p);
  void fdsclr(fd_set *p,int fd);
  void fdscopy(fd_set *p1,fd_set *p2);
  fd_set *fdsdup(fd_set *p);
  void fdsfree(fd_set *p);
  void fdsfresh(int fd);
  void fdsinit(void);
  int fdsisset(fd_set *p,int fd);
  void fdsset(fd_set *p,int fd);
  void fdszero(fd_set *p);
  void fdsand(fd_set *p1,fd_set *p2,fd_set *r);
  void fdsor(fd_set *p1,fd_set *p2,fd_set *r);
  void fdscopy(void);
  void fdszero(void);
};

#endif 
