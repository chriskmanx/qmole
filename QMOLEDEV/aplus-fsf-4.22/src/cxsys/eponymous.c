/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/development.h>
#include <unistd.h>
#include <stdlib.h>
#if defined(__osf__)
#undef _AIX
#endif
#if defined(_AIX)
extern closelog(),fsync(),getsockopt(),setsockopt();
#endif
#include <stdio.h>
#if defined(HAVE_SVR4)
#  include <netdb.h>
#endif
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <dap/dap.h>
#include <fcntl.h>
#include <dirent.h>
#include <memory.h>
#include <syslog.h>
#include <pwd.h>
#include <errno.h>
#include <signal.h>

#include <sys/param.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/arthur.h>
#include <cxc/cxc.h>
#include <sys/times.h>
#include <limits.h>

#ifndef _AIX
  extern void perror();
#endif
#if defined(_AIX) || defined(__osf__)
#  include <time.h>
#  include <sys/select.h>
#  include <sys/ioctl.h>
#  include <sys/termio.h>
#elif defined(linux)
#  include <time.h>
#  include <sys/select.h>
#  include <sys/ioctl.h>
#  include <sys/termios.h>
#else
#  include <sys/termios.h>
#  include <sys/filio.h>
#endif

/* #  if defined(__cplusplus) || defined(_LCC_LIB) */
/* #    undef __STDC__ */
/* #    include <sys/termios.h> */
/* #    include <sys/filio.h> */
/* #  else */
/* #    include <sys/termios.h> */
/* #    include <sys/filio.h> */
/* #  endif */

/* *************************************************
 *
 * Error Code Table and Functions
 */

#if !defined(__osf__)

static EnumTable ErrnoTable[] = {
  { 0,          "OK",           0},          
#if defined(_AIX) || defined (__osf__)
  { EPERM,	"EPERM",	0},	/* Operation not permitted */
  { ENOENT,	"ENOENT",	0},	/* No such file or directory */
  { ESRCH,	"ESRCH",	0},	/* No such process */
  { EINTR,	"EINTR",	0},	/* interrupted system call */
  { EIO,	"EIO",		0},	/* I/O error */
  { ENXIO,	"ENXIO",	0},	/* No such device or address */
  { E2BIG,	"E2BIG",	0},	/* Arg list too long */
  { ENOEXEC,	"ENOEXEC",	0},	/* Exec format error */
  { EBADF,	"EBADF",	0},	/* Bad file descriptor */
  { ECHILD,	"ECHILD",	0},	/* No child processes */
  { EAGAIN,	"EAGAIN",	0},	/* Resource temporarily unavailable */
  { ENOMEM,	"ENOMEM",	0},	/* Not enough space */
  { EACCES,	"EACCES",	0},	/* Permission denied */
  { EFAULT,	"EFAULT",	0},	/* Bad address */
  { ENOTBLK,	"ENOTBLK",	0},	/* Block device required */
  { EBUSY,	"EBUSY",	0},	/* Resource busy */
  { EEXIST,	"EEXIST",	0},	/* File exists */
  { EXDEV,	"EXDEV",	0},	/* Improper link */
  { ENODEV,	"ENODEV",	0},	/* No such device */
  { ENOTDIR,	"ENOTDIR",	0},	/* Not a directory */
  { EISDIR,	"EISDIR",	0},	/* Is a directory */
  { EINVAL,	"EINVAL",	0},	/* Invalid argument */
  { ENFILE,	"ENFILE",	0},	/* Too many open files in system */
  { EMFILE,	"EMFILE",	0},	/* Too many open files */
  { ENOTTY,	"ENOTTY",	0},	/* Inappropriate I/O control op */
  { ETXTBSY,	"ETXTBSY",	0},	/* Text file busy */
  { EFBIG,	"EFBIG",	0},	/* File too large */
  { ENOSPC,	"ENOSPC",	0},	/* No space left on device */
  { ESPIPE,	"ESPIPE",	0},	/* Invalid seek */
  { EROFS,	"EROFS",	0},	/* Read only file system */
  { EMLINK,	"EMLINK",	0},	/* Too many links */
  { EPIPE,	"EPIPE",	0},	/* Broken pipe */
  { EDOM,	"EDOM",		0},	/* Domain error within math function */
  { ERANGE,	"ERANGE",	0},	/* Result too large */
  { ENOMSG,	"ENOMSG",	0},	/* No message of desired type */
  { EIDRM,	"EIDRM",	0},	/* Identifier removed */
  { ECHRNG,	"ECHRNG",	0},	/* Channel number out of range */
  { EL2NSYNC,	"EL2NSYNC",	0},	/* Level 2 not synchronized */
  { EL3HLT,	"EL3HLT",	0},	/* Level 3 halted */
  { EL3RST,	"EL3RST",	0},	/* Level 3 reset */
  { ELNRNG,	"ELNRNG",	0},	/* Link number out of range */
  { EUNATCH,	"EUNATCH",	0},	/* Protocol driver not attached */
  { ENOCSI,	"ENOCSI",	0},	/* No CSI structure available */
  { EL2HLT,	"EL2HLT",	0},	/* Level 2 halted */
  { EDEADLK,	"EDEADLK",	0},	/* Resource deadlock avoided */
  { ENOTREADY,	"ENOTREADY",	0},	/* Device not ready */
  { EWRPROTECT,	"EWRPROTECT",	0},	/* Write-protected media */
  { EFORMAT,	"EFORMAT",	0},	/* Unformatted media */
  { ENOLCK,	"ENOLCK",	0},	/* No locks available */
  { ENOCONNECT,	"ENOCONNECT",	0},	/* no connection */
  { ESTALE,	"ESTALE",	0},	/* no filesystem */
  { EDIST,	"EDIST",	0},	/* old, currently unused AIX errno*/ 
  { EWOULDBLOCK,"EWOULDBLOCK",	0},	/* Operation would block */
  { EINPROGRESS,"EINPROGRESS",	0},	/* Operation now in progress */
  { EALREADY,	"EALREADY",	0},	/* Operation already in progress */
  { ENOTSOCK,	"ENOTSOCK",	0},	/* Socket operation on non-socket */
  { EDESTADDRREQ,"EDESTADDRREQ",0},	/* Destination address required */
  { EMSGSIZE,	"EMSGSIZE",	0},	/* Message too long */
  { EPROTOTYPE,	"EPROTOTYPE",	0},	/* Protocol wrong type for socket */
  { ENOPROTOOPT,"ENOPROTOOPT",	0},	/* Protocol not available */
  { EPROTONOSUPPORT,"EPROTONOSUPPORT",0},/* Protocol not supported */
  { ESOCKTNOSUPPORT,"ESOCKTNOSUPPORT",0},/* Socket type not supported */
  { EOPNOTSUPP,	"EOPNOTSUPP",	0},	/* Operation not supported on socket */
  { EPFNOSUPPORT,"EPFNOSUPPORT",0},	/* Protocol family not supported */
  { EAFNOSUPPORT,"EAFNOSUPPORT",0},	/* Address family not supported */
  { EADDRINUSE,	"EADDRINUSE",	0},	/* Address already in use */
  { EADDRNOTAVAIL,"EADDRNOTAVAIL",0},	/* Can't assign requested address */
  { ENETDOWN,	"ENETDOWN",	0},	/* Network is down */
  { ENETUNREACH,"ENETUNREACH",	0},	/* Network is unreachable */
  { ENETRESET,	"ENETRESET",	0},	/* Network dropped connection on res */
  { ECONNABORTED,"ECONNABORTED",0},	/* Software caused connection abort */
  { ECONNRESET,	"ECONNRESET",	0},	/* Connection reset by peer */
  { ENOBUFS,	"ENOBUFS",	0},	/* No buffer space available */
  { EISCONN,	"EISCONN",	0},	/* Socket is already connected */
  { ENOTCONN,	"ENOTCONN",	0},	/* Socket is not connected */
  { ESHUTDOWN,	"ESHUTDOWN",	0},	/* Can't send after socket shutdown */
  { ETIMEDOUT,	"ETIMEDOUT",	0},	/* Connection timed out */
  { ECONNREFUSED,"ECONNREFUSED",0},	/* Connection refused */
  { EHOSTDOWN,	"EHOSTDOWN",	0},	/* Host is down */
  { EHOSTUNREACH,"EHOSTUNREACH",0},	/* No route to host */
  { ERESTART,	"ERESTART",	0},	/* restart the system call */
  { EPROCLIM,	"EPROCLIM",	0},	/* Too many processes */
  { EUSERS,	"EUSERS",	0},	/* Too many users */
  { ELOOP,	"ELOOP",	0},	/* Too many levels of symbolic links */
  { ENAMETOOLONG,"ENAMETOOLONG",0},	/* File name too long */
  { ENOTEMPTY,	"ENOTEMPTY",	0},	/* Directory not empty */      
  { EDQUOT,	"EDQUOT",	0},	/* Disc quota exceeded */
  { EREMOTE,	"EREMOTE",	0},	/* Item is not local to host */
  { ENOSYS,	"ENOSYS",	0},	/* Function not implemented  POSIX */
  { EMEDIA,	"EMEDIA",	0},	/* media surface error */
  { ESOFT,	"ESOFT",	0},	/* I/O completed, but needs relocat */
  { ENOATTR,	"ENOATTR",	0},	/* no attribute found */
  { ESAD,	"ESAD",		0},	/* security authentication denied */
  { ENOTRUST,	"ENOTRUST",	0},	/* not a trusted program */ 
  { ETOOMANYREFS,"ETOOMANYREFS",0},	/* Too many references: can't splice */
  { EILSEQ,	"EILSEQ",	0},	/* Invalid wide character */
  { ECANCELED,	"ECANCELED",	0},	/* asynchronous i/o cancelled */
  { ENOSR,	"ENOSR",	0},	/* temp out of streams resources */
  { ETIME,	"ETIME",	0},	/* I_STR ioctl timed out */
  { EBADMSG,	"EBADMSG",	0},	/* wrong message type at stream head */
  { EPROTO,	"EPROTO",	0},	/* STREAMS protocol error */
  { ENODATA,	"ENODATA",	0},	/* no message ready at stream head */
  { ENOSTR,	"ENOSTR",	0},	/* fd is not a stream */
  { ECLONEME,	"ECLONEME",	0},	/* this is the way we clone a stream */
#else
  { EPERM,	"EPERM",	0},	/* Not owner */ 
  { ENOENT,	"ENOENT",	0},	/* No such file or directory */
  { ESRCH,	"ESRCH",	0},	/* No such process */
  { EINTR,	"EINTR",	0},	/* Interrupted system call */
  { EIO,        "EIO",	        0},	/* I/O error */
  { ENXIO,	"ENXIO",	0},	/* No such device or address */
  { E2BIG,	"E2BIG",	0},	/* Arg list too long */  
  { ENOEXEC,	"ENOEXEC",	0},	/* Exec format error */             
  { EBADF,	"EBADF",	0},	/* Bad file number */ 
  { ECHILD,	"ECHILD",	0},	/* No children */                    
  { EAGAIN,	"EAGAIN",	0},	/* No more processes */              
  { ENOMEM,	"ENOMEM",	0},	/* Not enough core */                
  { EACCES,	"EACCES",	0},	/* Permission denied */              
  { EFAULT,	"EFAULT",	0},	/* Bad address */                    
  { ENOTBLK,	"ENOTBLK",	0},	/* Block device required */        
  { EBUSY,	"EBUSY",	0},	/* Mount device busy */                
  { EEXIST,	"EEXIST",	0},	/* File exists */                    
  { EXDEV,	"EXDEV",	0},	/* Cross-device link */                
  { ENODEV,	"ENODEV",	0},	/* No such device */                 
  { ENOTDIR,	"ENOTDIR",	0},	/* Not a directory*/               
  { EISDIR,	"EISDIR",	0},	/* Is a directory */                 
  { EINVAL,	"EINVAL",	0},	/* Invalid argument */               
  { ENFILE,	"ENFILE",	0},	/* File table overflow */            
  { EMFILE,	"EMFILE",	0},	/* Too many open files */            
  { ENOTTY,	"ENOTTY",	0},	/* Not a typewriter */               
  { ETXTBSY,	"ETXTBSY",	0},	/* Text file busy */               
  { EFBIG,	"EFBIG",	0},	/* File too large */                   
  { ENOSPC,	"ENOSPC",	0},	/* No space left on device */        
  { ESPIPE,	"ESPIPE",	0},	/* Illegal seek */                   
  { EROFS,	"EROFS",	0},	/* Read-only file system */            
  { EMLINK,	"EMLINK",	0},	/* Too many links */                 
  { EPIPE,	"EPIPE",	0},	/* Broken pipe */                      
  { EDOM,       "EDOM", 	0},	/* Argument too large */
  { ERANGE,	"ERANGE",	0},	/* Result too large */               
  { EWOULDBLOCK,"EWOULDBLOCK",	0},	/* Operation would block */
  { EINPROGRESS,"EINPROGRESS",	0},	/* Operation now in progress */  
  { EALREADY,	"EALREADY",	0},	/* Operation already in progress */    
  { ENOTSOCK,	"ENOTSOCK",	0},	/* Socket operation on non-socket */
  { EDESTADDRREQ,"EDESTADDRREQ",0},	/* Destination address required */
  { EMSGSIZE,	"EMSGSIZE",	0},	/* Message too long */           
  { EPROTOTYPE,	"EPROTOTYPE",	0},	/* Protocol wrong type for socket */
  { ENOPROTOOPT,"ENOPROTOOPT",	0},	/* Protocol not available */     
  { EPROTONOSUPPORT,"EPROTONOSUPPORT",0},/* Protocol not supported */
  { ESOCKTNOSUPPORT,"ESOCKTNOSUPPORT",0},/* Socket type not supported */
  { EOPNOTSUPP,	"EOPNOTSUPP",	0},	/* Operation not supported on socket */
  { EPFNOSUPPORT,"EPFNOSUPPORT",0},	/* Protocol family not supporte */
  { EAFNOSUPPORT,"EAFNOSUPPORT",0},	/* Address family not supported */
  { EADDRINUSE,	"EADDRINUSE",	0},	/* Address already in use */ 
  { EADDRNOTAVAIL,"EADDRNOTAVAIL",0},	/* Can't assign requested add */
  { ENETDOWN,	"ENETDOWN",	0},	/* Network is down */            
  { ENETUNREACH,"ENETUNREACH",	0},	/* Network is unreachable */     
  { ENETRESET,	"ENETRESET",	0},	/* Network dropped connection on res*/
  { ECONNABORTED,"ECONNABORTED",0},	/* Software caused connection a */
  { ECONNRESET,	"ECONNRESET",	0},	/* Connection reset by peer */     
  { ENOBUFS,	"ENOBUFS",	0},	/* No buffer space available */    
  { EISCONN,	"EISCONN",	0},	/* Socket is already connected */  
  { ENOTCONN,	"ENOTCONN",	0},	/* Socket is not connected */
  { ESHUTDOWN,	"ESHUTDOWN",	0},	/* Can't send after socket shutdown */
  { ETOOMANYREFS,"ETOOMANYREFS",0},	/* Too many references */
  { ETIMEDOUT,	"ETIMEDOUT",	0},	/* Connection timed out */     
  { ECONNREFUSED,"ECONNREFUSED",0},	/* Connection refused */ 
  { ELOOP,	"ELOOP",	0},	/* Too many levels of symbolic links */
  { ENAMETOOLONG,"ENAMETOOLONG",0},	/* File name too long */
  { EHOSTDOWN,	"EHOSTDOWN",	0},    /* Host is down */
  { EHOSTUNREACH,"EHOSTUNREACH",0},	/* No route to host */   
  { ENOTEMPTY,	"ENOTEMPTY",	0},	/* Directory not empty */      
  { EUSERS,	"EUSERS",	0},	/* Too many users */            
  { ESTALE,	"ESTALE",	0},	/* Stale NFS file handle */          
  { EREMOTE,	"EREMOTE",	0},	/* Too many levels of remote in path */
  { EDEADLK,	"EDEADLK",	0},	/* Deadlock condition. */          
  { ENOLCK,	"ENOLCK",	0},	/* No record locks available. */     
  { ENOSYS,	"ENOSYS",	0},	/* function not implemented */
#endif
  { 0,          (char *)0,      0}
};

#endif

I sysErrno() { R errno; }

#if defined (__osf__)
	A sysErrsym(n)
		{
		A r ;
		r = gs(Et) ;
		*r->p = MS(si(strerror(n))) ;
		R r; 
		}
		
#else
A sysErrsym(n){A z=EnumToSymbol(ErrnoTable,n);R qz(z)?(A)gsym("unknown"):z;}
#endif

A aselect(rfd, wfd, xfd, tmo)
  A rfd, wfd, xfd, tmo;
{
  A z, retcode, errcode, arfd, awfd, axfd;
  int width;
  fd_set *rfds, *wfds, *xfds;
  struct timeval *tvp;
  int rc;
  I *p, *q;
  I n, i, j;
  struct timeval tv;

  width = -1;

  retcode = gs(It);
  errcode = gs(It);

  n = rfd->n; p = rfd->p;
  for (i = 0; i < n; i++) if (p[i] > width) width = p[i];
  arfd = gv(It, n); arfd->n = arfd->d[0] = 0;

  n = wfd->n; p = wfd->p;
  for (i = 0; i < n; i++) if (p[i] > width) width = p[i];
  awfd = gv(It, n); awfd->n = awfd->d[0] = 0;

  n = xfd->n; p = xfd->p;
  for (i = 0; i < n; i++) if (p[i] > width) width = p[i];
  axfd = gv(It, n); axfd->n = axfd->d[0] = 0;

  z = gv(Et, (I)5);
  p = z->p;
  *(A *)(p) = retcode;
  *(A *)(p + 1) = errcode;
  *(A *)(p + 2) = arfd;
  *(A *)(p + 3) = awfd;
  *(A *)(p + 4) = axfd;

  width += 1;

  if (width > 0)
  {
    int sz;
    I *tmp;

    sz = howmany(width, NFDBITS);
    tmp = k_tm(3 * sz);
    bzero((char *)tmp, 3 * sizeof(I) * sz);
    rfds = (fd_set *)(tmp);
    wfds = (fd_set *)(tmp + sz);
    xfds = (fd_set *)(tmp + (2 * sz));
  }
  else
  {
    rfds = (fd_set *)(0);
    wfds = (fd_set *)(0);
    xfds = (fd_set *)(0);
  }

  n = rfd->n; p = rfd->p;
  for (i = 0; i < n; i++) FD_SET(p[i], rfds);
  n = wfd->n; p = wfd->p;
  for (i = 0; i < n; i++) FD_SET(p[i], wfds);
  n = xfd->n; p = xfd->p;
  for (i = 0; i < n; i++) FD_SET(p[i], xfds);

  n = tmo->n; p = tmo->p;
  if (n > 0)
  {
    tv.tv_sec = p[0];
    tv.tv_usec = (n > 1) ? p[1] : 0;
    tvp = &tv;
  }
  else
  {
    tvp = (struct timeval *)(0);
  }

  rc = select(width, rfds, wfds, xfds, tvp);
  retcode->p[0] = rc;
  errcode->p[0] = rc < 0 ? errno : 0;
  if (rc > 0)
  {
    n = rfd->n; p = rfd->p; q = arfd->p;
    for (i = 0, j = 0; i < n; i++)
    {
      if (FD_ISSET(p[i], rfds)) q[j++] = p[i];
    }
    arfd->n = arfd->d[0] = j;

    n = wfd->n; p = wfd->p; q = awfd->p;
    for (i = 0, j = 0; i < n; i++)
    {
      if (FD_ISSET(p[i], wfds)) q[j++] = p[i];
    }
    awfd->n = awfd->d[0] = j;

    n = xfd->n; p = xfd->p; q = axfd->p;
    for (i = 0, j = 0; i < n; i++)
    {
      if (FD_ISSET(p[i], xfds)) q[j++] = p[i];
    }
    axfd->n = axfd->d[0] = j;
  }
  return z;
}

A cpu()
{
  I i;
  struct tms aTms;
  static int clkTck=0;
  A r;
  if(!clkTck) clkTck=sysconf(_SC_CLK_TCK);
  times(&aTms);
  r=gv(It,4);
  r->p[0]=(1000*aTms.tms_utime)/clkTck;
  r->p[1]=(1000*aTms.tms_stime)/clkTck;
  r->p[2]=(1000*aTms.tms_cutime)/clkTck;
  r->p[3]=(1000*aTms.tms_cstime)/clkTck;
  R r;
}

#ifndef _AIX
static MaskTable MsyncMasks[] = {

  { MS_ASYNC,     "MS_ASYNC",     0,      0 },
  { MS_SYNC,      "MS_SYNC",      0,      0 },
  { MS_INVALIDATE,"MS_INVALIDATE",0,      0 },
  { 0,            (char *)0,      0,      0 }
};
#endif

I amsync(A a, A aflags)
{
#ifdef _AIX
  R 0;
#else
  int bytes;

  unsigned long flags;

  if (SymbolsToMask(MsyncMasks, aflags, &flags) == -1) {
    (void)pa((V)aflags);
    q = ERR_DOMAIN;
    R 0;
  }
  bytes=mf_length(a);
  R bytes?msync((caddr_t)a, AH+Tt(a->t,a->n), (int)flags):0;
#endif /* _AIX */
}

A gettod(atz)
  A	atz;
{
  struct timeval tv;
  struct timezone tz;
  A z;

  z = gv(It, 2);

  if (atz->n != 2)
    gettimeofday(&tv,NULL);
  else {
    tz.tz_minuteswest = atz->p[0];
    tz.tz_dsttime = atz->p[1];
    gettimeofday(&tv, &tz);
  }
  z->p[0] = tv.tv_sec;
  z->p[1] = tv.tv_usec;
  return z;
}

A ts()
{
  struct timeval tp;
  struct tm *lt;
  A z=gv(It,7);                 /* 7-element integer vector */
#ifdef APLUS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

  gettimeofday(&tp,NULL);
  lt = APLUS_LOCALTIME((const time_t *)&tp.tv_sec,&tmp);
  z->p[0] = 1900 + lt->tm_year; /* year        */
  z->p[1] = 1 + lt->tm_mon;     /* month       */
  z->p[2] = lt->tm_mday;        /* day         */
  z->p[3] = lt->tm_hour;        /* hour        */
  z->p[4] = lt->tm_min;         /* minute      */
  z->p[5] = lt->tm_sec;         /* second      */
  z->p[6] = tp.tv_usec / 1000;  /* millisecond */
  return z;
}

A tsgmt()
{
  struct timeval tp;
  struct tm *lt;
  A z=gv(It,7);                 /* 7-element integer vector */
#ifdef APLUS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

  gettimeofday(&tp,NULL);
  lt = APLUS_GMTIME((const time_t *)&tp.tv_sec,&tmp);
  z->p[0] = 1900 + lt->tm_year; /* year        */
  z->p[1] = 1 + lt->tm_mon;     /* month       */
  z->p[2] = lt->tm_mday;        /* day         */
  z->p[3] = lt->tm_hour;        /* hour        */
  z->p[4] = lt->tm_min;         /* minute      */
  z->p[5] = lt->tm_sec;         /* second      */
  z->p[6] = tp.tv_usec / 1000;  /* millisecond */
  return z;
}

I mkts1(a)
  A a;
{
  struct tm lt;
  A z;
  Q(a->t!=It,ERR_TYPE)
  Q(a->r!=1,ERR_RANK)
  Q(a->n!=7,ERR_LENGTH)
  z=gs(It);  
  lt.tm_year=a->p[0]-1900; /* year             */
  lt.tm_mon=a->p[1]-1;     /* month            */
  lt.tm_mday=a->p[2];      /* day              */
  lt.tm_hour=a->p[3];      /* hour             */
  lt.tm_min=a->p[4];       /* minute           */
  lt.tm_sec=a->p[5];       /* second           */
  lt.tm_isdst=-1;  	   /* daylight savings */
  z->p[0]=mktime(&lt); 
  return (I) z;
}

I mkts1gmt(a)
  A a;
{
  struct tm lt;
  A z;
  Q(a->t!=It,ERR_TYPE)
  Q(a->r!=1,ERR_RANK)
  Q(a->n!=7,ERR_LENGTH)
  z=gs(It);  
  lt.tm_year=a->p[0]-1900; /* year             */
  lt.tm_mon=a->p[1]-1;     /* month            */
  lt.tm_mday=a->p[2];      /* day              */
  lt.tm_hour=a->p[3];      /* hour             */
  lt.tm_min=a->p[4];       /* minute           */
  lt.tm_sec=a->p[5];       /* second           */
  lt.tm_isdst=0;  	   /* daylight savings NOT in effect */
/*   z->p[0]=mktime(&lt)-altzone;  */
#if defined(_AIX) || defined(HAVE_SVR4) || defined (__osf__) || defined(HAVE_MKTIME)
  z->p[0]=mktime(&lt); 
  if( z->p[0] != -1) 
    z->p[0]=z->p[0]-timezone;  
#else
  z->p[0]=timegm(&lt);
#endif
  return (I) z;
}

A ts1(clock)
  long	clock;
{
  struct tm *lt;
  A z=gv(It,7);                 /* 7-element integer vector */
#ifdef APLUS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

  time_t    clockx=clock;

  lt = APLUS_LOCALTIME(&clockx,&tmp);
  z->p[0] = 1900 + lt->tm_year; /* year        */
  z->p[1] = 1 + lt->tm_mon;     /* month       */
  z->p[2] = lt->tm_mday;        /* day         */
  z->p[3] = lt->tm_hour;        /* hour        */
  z->p[4] = lt->tm_min;         /* minute      */
  z->p[5] = lt->tm_sec;         /* second      */
  z->p[6] = 0;  		/* millisecond */
  return z;
}

A ts1gmt(clock)
  long	clock;
{
  struct tm *lt;
  A z=gv(It,7);                 /* 7-element integer vector */
#ifdef APLUS_THREAD_SAFE_FUNCTIONS
  struct tm tmp;
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

  time_t  clockx=clock;

  lt = APLUS_GMTIME(&clockx,&tmp);
  z->p[0] = 1900 + lt->tm_year; /* year        */
  z->p[1] = 1 + lt->tm_mon;     /* month       */
  z->p[2] = lt->tm_mday;        /* day         */
  z->p[3] = lt->tm_hour;        /* hour        */
  z->p[4] = lt->tm_min;         /* minute      */
  z->p[5] = lt->tm_sec;         /* second      */
  z->p[6] = 0;  		/* millisecond */
  return z;
}

time_t secs_in_epoch ()
{
  return time(NULL);
}

A updtime (name) 
char *name;
{
  A z = gv(It,1);
  struct stat stbuf;

  if (stat(name, &stbuf) == -1) {
    z->p[0] = -1;
    return z;
  }
  z->p[0] = stbuf.st_mtime;
  return z;
}

A filesize (name) 
char *name;
{
  A z = gv(It,1);
  struct stat stbuf;

  if (stat(name, &stbuf) == -1) {
    z->p[0] = -1;
    return z;
  }
  z->p[0] = stbuf.st_size;
  return z;
}

fflush_stdout()
{
  fflush(stdout);
}

I ep_setenv(e)C*e;{R putenv(strdup(e));}

A readenv(e)
C*e;
{
  extern C*getenv();
  if((e=getenv(e))==NULL)R(A)gz();R(A)gsv(0,e);
}

A pathfind(pathVariable, defaultPath, fileName, mode)
  char	*pathVariable;	/* environment variable containing path */
  char	*defaultPath;	/* default path if no environment variable */
  char	*fileName;	/* name of file to search for in path */
  int	mode;		/* flag to test access mode a la access(2V) */
{
  extern char *pfind();
  A	r;
  char	*p;
  if ((p = pfind(pathVariable, defaultPath, fileName, mode)) == NULL)
    r = (A)gz();
  else {
    r = (A)gsv(0, p);
  }
  R r;
}

/*
 * From Robert Gusick (cleaned up by Chuck Ocheret)
 */
#ifndef MAX
#define MAX(a,b) ((a)>(b))?(a):(b)
#endif /* MAX */

A readmat(name)
  char *name;
{
  A result;		/* 'a' character matrix */
  int num_lines;	/* number of rows of 'result' */
  int max_line_length;	/* number of columns of 'result' */
  int this_line_length;	/* current line length */
  int descriptor;	/* file descriptor */
  char *c;		/* index into mapped file */
  char *start_c;	/* start of mapped file */
  char *c_start_line;	/* start of current line of 'result' */     
  char *this_char;	/* index into 'result' */
  struct stat stbuf;	/* file status */
  
  if ((descriptor = open(name, O_RDONLY)) < 0) {
    perror("readmat open");
    R (A)gz();
  }
  if (fstat(descriptor, &stbuf) == (-1)) {
    perror("readmat fstat");
    (void)close(descriptor);
    R (A)gz();
  }
  if (S_ISDIR(stbuf.st_mode)) {
    H("readmat error: is directory\n");
    R (A)gz();
  }
  if (stbuf.st_size != 0)
  {
    start_c = mmap(0, stbuf.st_size, PROT_READ, MAP_SHARED, descriptor, 0);
    (void)close(descriptor);
    if ((int)start_c == -1) {
      perror("readmat mmap");
      R (A)gz();
    }
    max_line_length = num_lines = 0;
    this_line_length = 0;
    for (c = start_c; c != start_c + stbuf.st_size; c++)
      if (*c == '\n') {
        num_lines++;
        max_line_length = MAX(max_line_length, this_line_length);
        this_line_length = 0;
      } else this_line_length++;
  
    if (this_line_length != 0) {
      num_lines++;
      max_line_length = MAX(max_line_length, this_line_length);
    }
    if ((result = gm(Ct, num_lines, max_line_length)) == 0) {
      (void)munmap(start_c, stbuf.st_size);
      R 0;
    }
    this_char = c_start_line = (C *)result->p;
    for (c = start_c; c != start_c + stbuf.st_size; c++) {
      if (*c == '\n') {
        c_start_line += max_line_length;
        while (this_char != c_start_line)
          *this_char++ = ' ';
      } else *this_char++ = *c;
    }
    if (this_line_length != 0) {
      c_start_line += max_line_length;
      while (this_char != c_start_line)
        *this_char++ = ' ';
    };
    (void)munmap(start_c, stbuf.st_size);
    R result;
  }
  else
  {
    (void) close(descriptor);
    R gm(Ct, 0, 0);
  }
}

/*
 * utilities from kwi (cleaned up by chuck ocheret)
 */
typedef struct stat   SSTAT;
typedef struct dirent DENT;
typedef struct node   NODE;

typedef struct fs{I l;char n[MAXNAMLEN+1];} FS;

DIR  *opendir();

A agetdents(a)
  C *a;
{
  A r; C *cp; DIR *f; FS *fs; DENT *e; NODE *hp=(NODE *)0,*np; 
  I i,j,d[MAXR+1];
  APLUS_DECLARE_DIRENT(dirEntry);  /* see comment in a/k.h on usage of this macro */

  bzero((C *)(d), sizeof(d));
  if ((f = opendir(a)) == (DIR *)0) R ga(Ct, 2, 0, d);
  hp = nodealloc();
  while (APLUS_READDIR(f,dirEntry,e)!=0) {
#if defined(HAVE_SVR4) || defined(linux)
    fs = (FS *)balloc(sizeof(FS)); fs->l = strlen(e->d_name);
#else
    fs = (FS *)balloc(sizeof(FS)); fs->l = e->d_namlen;
#endif
    if((fs->l == 1) && (e->d_name[0] == '.')) continue;
    if((fs->l == 2) && (e->d_name[0] == '.') && (e->d_name[1] == '.'))
      continue;
    bcopy(e->d_name, fs->n,fs->l); d[1] = (d[1]>fs->l)?d[1]:fs->l;
    d[0]++; np = nodealloc(); np->d = (void *)(fs); nodeinsert(hp, np);
  }
  (void)closedir(f);
  j = d[1];
  r = ga(Ct, 2, d[0]*j, d);
  memset((C *)(r->p), ' ', r->n);
  for (i=0, np=hp->f, cp=(C *)(r->p); i<d[0]; i++, np=hp->f, cp+=j) {
    fs = (FS *)(np->d);
    bcopy(fs->n, cp, fs->l); bfree((C *)(np->d)); noderemove(np); nodefree(np);
  }
  nodefree(hp);
  R r;
}

Z A auxstat(a, statfunc)
  A a;
  int (*statfunc)();
{
  A z; SSTAT st; C *c,*f,*t; I i,j,n,r,ad[2],d[MAXR+1];

  bzero((C *)(d), sizeof(d));
/*  if(a->t != Ct) {z=gv(It,1); z->p[0] = -1; R z;}*/
  switch (a->r) {
  case 0:ad[0]=1; ad[1]=1; r=1; n=13; d[0]=13; break;
  case 1:ad[0]=1; ad[1]=a->d[0]; r=1; n=13; d[0]=13; break;
  case 2:d[0]=ad[0]=a->d[0]; ad[1]=a->d[1]; d[1]=13; r=2; n=d[0]*d[1]; break;
  default: d[0] = ad[0] = a->d[0];
    for(i=1;i<(a->r-1);i++){d[i]=a->d[i];ad[0]*=a->d[i];} 
    ad[1]=a->d[i]; d[i]=13; r=a->r; n=13*ad[0]; break;
  }
  z=ga(It,r,n,d); bzero((C *)(z->p),4*z->n); f=(C *)balloc(ad[1]+1);
  for(i=0,n=0,c=(C *)(a->p);i<ad[0];i++,c+=ad[1]){
    bcopy(c,f,ad[1]); t=f+ad[1]; *(t--)='\0';
    while((t>=f)&&(*t==' ')) *(t--)='\0';
    if((*statfunc)(f,&st)==-1){for(j=0;j<13;j++)z->p[n++]=0;
    } else {
      z->p[n++]=(I)(st.st_dev); z->p[n++]=(I)(st.st_ino);
      z->p[n++]=(I)(st.st_mode); z->p[n++]=(I)(st.st_nlink);
      z->p[n++]=(I)(st.st_uid); z->p[n++]=(I)(st.st_gid);
      z->p[n++]=(I)(st.st_rdev); z->p[n++]=(I)(st.st_size);
      z->p[n++]=(I)(st.st_atime); z->p[n++]=(I)(st.st_mtime);
      z->p[n++]=(I)(st.st_ctime); z->p[n++]=(I)(st.st_blksize);
      z->p[n++]=(I)(st.st_blocks);
    }
  }
  bfree(f);
  R z;
}

A astat(a)
  A a;
{
  extern int stat();
  R auxstat(a, stat);
}

A alstat(a)
  A a;
{
  extern int lstat();
  R auxstat(a, lstat);
}

A areadlink(a)
  C *a;
{
  A r; C *b; I i,d[MAXR+1]; SSTAT st;

#ifndef S_ISLNK
#define S_ISLNK(m)      (((m)&S_IFMT) == S_IFLNK)
#endif /* S_ISLNK */

  bzero((C *)(d), sizeof(d));
  if (((lstat(a,&st)) == -1) || (!S_ISLNK(st.st_mode))) {
    r = ga(It,0,1,d);
    r->p[0]=-1;
    R r;
  }
  b=(C *)balloc(st.st_size+1);
  if((i=readlink(a,b,st.st_size))==-1) R gi(-1);
  b[i]='\0';
  r=gv(Ct,i);
  bcopy(b,(C *)(r->p),st.st_size);
  bfree(b);
  R r;
}

static MaskTable SyslogMasks[] = {
{ LOG_EMERG,	"LOG_EMERG",	0,	0 },
{ LOG_ALERT,	"LOG_ALERT",	0,	0 },
{ LOG_CRIT,	"LOG_CRIT",	0,	0 },
{ LOG_ERR,	"LOG_ERR",	0,	0 },
{ LOG_WARNING,	"LOG_WARNING",	0,	0 },
{ LOG_NOTICE,	"LOG_NOTICE",	0,	0 },
{ LOG_INFO,	"LOG_INFO",	0,	0 },
{ LOG_DEBUG,	"LOG_DEBUG",	0,	0 },
{ LOG_KERN,	"LOG_KERN",	0,	0 },
{ LOG_USER,	"LOG_USER",	0,	0 },
{ LOG_MAIL,	"LOG_MAIL",	0,	0 },
{ LOG_DAEMON,	"LOG_DAEMON",	0,	0 },
{ LOG_AUTH,	"LOG_AUTH",	0,	0 },
{ LOG_SYSLOG,	"LOG_SYSLOG",	0,	0 },
{ LOG_LPR,	"LOG_LPR",	0,	0 },
{ LOG_NEWS,	"LOG_NEWS",	0,	0 },
{ LOG_UUCP,	"LOG_UUCP",	0,	0 },
#ifndef _AIX
{ LOG_CRON,	"LOG_CRON",	0,	0 },
#endif /* _AIX */
{ LOG_LOCAL0,	"LOG_LOCAL0",	0,	0 },
{ LOG_LOCAL1,	"LOG_LOCAL1",	0,	0 },
{ LOG_LOCAL2,	"LOG_LOCAL2",	0,	0 },
{ LOG_LOCAL3,	"LOG_LOCAL3",	0,	0 },
{ LOG_LOCAL4,	"LOG_LOCAL4",	0,	0 },
{ LOG_LOCAL5,	"LOG_LOCAL5",	0,	0 },
{ LOG_LOCAL6,	"LOG_LOCAL6",	0,	0 },
{ LOG_LOCAL7,	"LOG_LOCAL7",	0,	0 },
{ 0,		(char *)0,	0,	0 }
};

static MaskTable SyslogOptMasks[] = {
{ LOG_PID,	"LOG_PID",	0,	0 },
{ LOG_CONS,	"LOG_CONS",	0,	0 },
{ LOG_NDELAY,	"LOG_NDELAY",	0,	0 },
{ LOG_NOWAIT,	"LOG_NOWAIT",	0,	0 },
{ 0,		(char *)0,	0,	0 }
};

I syssyslog(apriority, message)
  A	apriority;
  C	*message;
{
  unsigned long	priority;

  if (SymbolsToMask(SyslogMasks, apriority, &priority) == -1) {
    (void)pa((V)apriority);
    q = ERR_DOMAIN;
    R 0;
  }
  syslog(priority, message);
  R 1;
}


I sysopenlog(ident, alogopt, afacility)
  A	ident, alogopt, afacility;
{
  unsigned long	logopt, facility;

  if (SymbolsToMask(SyslogMasks, afacility, &facility) == -1) {
    (void)pa((V)afacility);
    q = ERR_DOMAIN;
    R 0;
  }
  if (SymbolsToMask(SyslogOptMasks, alogopt, &logopt) == -1) {
    (void)pa((V)alogopt);
    q = ERR_DOMAIN;
    R 0;
  } else {
    /* Don't dc() in closelog, since even on closelog, the
     * library keeps a reference to the ident string
     */
    Z A saved_ident = (A)(0);

    if (saved_ident != (A)(0)) dc(saved_ident);
    saved_ident = (A)ic(ident);
    openlog((C *)(ident->p), logopt, facility); 
    R 1;
  }
}

static EnumTable FcntlEnums[] = {
{ F_DUPFD,	"F_DUPFD",	0	},
{ F_GETFD,	"F_GETFD",	0	},
{ F_SETFD,	"F_SETFD",	0	},
{ F_GETFL,	"F_GETFL",	0	},
{ F_SETFL,	"F_SETFL",	0	},
{ F_GETLK,	"F_GETLK",	0	},
{ F_SETLK,	"F_SETLK",	0	},
{ F_SETLKW,	"F_SETLKW",	0	},
{ F_GETOWN,	"F_GETOWN",	0	},
{ F_SETOWN,	"F_SETOWN",	0	},
#ifdef _AIX
{ F_CLOSEM,	"F_CLOSEM",	0	},
#endif
{ 0,		(char *)0,	0	}
};

I sysfcntl(fd, acmd, arg)
  I	fd;
  A	acmd;
  I	arg;
{
  unsigned long	cmd;
  if (SymbolToEnum(FcntlEnums, acmd, &cmd)) {
    (void)pa((V)acmd);
    q = ERR_DOMAIN;
    R 0;
  } else
    R (I)fcntl((int)fd, (int)cmd, (int)arg);
}
#ifndef LEXABUG
static EnumTable IoctlEnums[] = {
{ FIOCLEX,	"FIOCLEX",	0	},
{ FIONCLEX,	"FIONCLEX",	0	},
{ FIONREAD,	"FIONREAD",	0	},
{ FIONBIO,	"FIONBIO",	0	},
{ FIOASYNC,	"FIOASYNC",	0	},
{ FIOSETOWN,	"FIOSETOWN",	0	},
{ FIOGETOWN,	"FIOGETOWN",	0	},
{ TIOCGPGRP,	"TIOCGPGRP",	0	},
{ TIOCSPGRP,	"TIOCSPGRP",	0	},
{ TIOCOUTQ,	"TIOCOUTQ",	0	},
{ TIOCSTI,	"TIOCSTI",	0	},
{ TIOCGWINSZ,	"TIOCGWINSZ",	0	},
{ TIOCSWINSZ,	"TIOCSWINSZ",	0	},
{ TIOCMGET,	"TIOCMGET",	0	},
{ TIOCMBIS,	"TIOCMBIS",	0	},
{ TIOCMBIC,	"TIOCMBIC",	0	},
{ TIOCMSET,	"TIOCMSET",	0	},
{ 0,		(char *)0,	0	}
};
#else
static EnumTable IoctlEnums[] = {
{ 0,  "FIOCLEX",	0	},
{ 0,  "FIONCLEX",	0	},
{ 0,  "FIONREAD",	0	},
{ 0,  "FIONBIO",	0	},
{ 0,  "FIOASYNC",	0	},
{ 0,  "FIOSETOWN",	0	},
{ 0,  "FIOGETOWN",	0	},
{ 0,  "TCGETS",	0	},
{ 0,  "TCSETS",	0	},
{ 0,  "TCSETSW",	0	},
{ 0,  "TCSETSF",	0	},
{ 0,  "TCXONC",	0	},
{ 0,  "TCFLSH",	0	},
{ 0,  "TIOCSCTTY",	0	},
{ 0,  "TIOCGPGRP",	0	},
{ 0,  "TIOCSPGRP",	0	},
{ 0,  "TIOCOUTQ",	0	},
{ 0,  "TIOCSTI",	0	},
{ 0,  "TIOCGWINSZ",	0	},
{ 0,  "TIOCSWINSZ",	0	},
{ 0,  "TIOCMGET",	0	},
{ 0,  "TIOCMBIS",	0	},
{ 0,  "TIOCMBIC",	0	},
{ 0,  "TIOCMSET",	0	},
{ 0,  "TIOCGSOFTCAR",	0	},
{ 0,  "TIOCSSOFTCAR",	0	},
{ 0,		(char *)0,	0	}
};

int initIoctlTable(void)
{
  EnumTable *EnumTablePtr = IoctlEnums;
  EnumTablePtr->value = FIOCLEX; EnumTablePtr ++;	
  EnumTablePtr->value = FIONCLEX; EnumTablePtr ++;	
  EnumTablePtr->value = FIONREAD; EnumTablePtr ++;	
  EnumTablePtr->value = FIONBIO; EnumTablePtr ++;	
  EnumTablePtr->value = FIOASYNC; EnumTablePtr ++;	
  EnumTablePtr->value = FIOSETOWN; EnumTablePtr ++;	
  EnumTablePtr->value = FIOGETOWN; EnumTablePtr ++;	
  EnumTablePtr->value = TCGETS; EnumTablePtr ++;	
  EnumTablePtr->value = TCSETS; EnumTablePtr ++;	
  EnumTablePtr->value = TCSETSW; EnumTablePtr ++;	
  EnumTablePtr->value = TCSETSF; EnumTablePtr ++;	
  EnumTablePtr->value = TCXONC; EnumTablePtr ++;	
  EnumTablePtr->value = TCFLSH; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCSCTTY; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCGPGRP; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCSPGRP; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCOUTQ; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCSTI; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCGWINSZ; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCSWINSZ; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCMGET; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCMBIS; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCMBIC; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCMSET; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCGSOFTCAR; EnumTablePtr ++;	
  EnumTablePtr->value = TIOCSSOFTCAR;
  return 0;	
}
#endif

I sysioctl(fd, arequest, arg)
  I	fd;
  A	arequest;
  I	arg;
{
  unsigned long	request;
#ifdef LEXABUG
  {
    static int init = 1;
    if (init) init = initIoctlTable();
  }
#endif
  if (SymbolToEnum(IoctlEnums, arequest, &request)) {
    (void)pa((V)arequest);
    q = ERR_DOMAIN;
    R 0;
  } else
    R (I)ioctl((int)fd, (int)request, (caddr_t)arg);
}

#if defined(HAVE_SVR4) || defined(_AIX)
#  ifndef LOCK_SH
#    define	LOCK_SH		1	/* shared lock */
#    define	LOCK_EX		2	/* exclusive lock */
#    define	LOCK_NB		4	/* don't block when locking */
#    define	LOCK_UN		8	/* unlock */
#  endif
int flock(fd, operation)
int fd, operation;
{
        struct flock fl;
        int cmd = F_SETLKW;
        fl.l_whence = 0;
        fl.l_start = 0;
        fl.l_len = 0;
        fl.l_type = 0;
        if (operation & LOCK_UN)
                fl.l_type |= F_UNLCK;
        if (operation & LOCK_SH)
                fl.l_type |= F_RDLCK;
        if (operation & LOCK_EX)
                fl.l_type |= F_WRLCK;
        if (operation & LOCK_NB)
                cmd = F_SETLK;
        return(fcntl(fd, cmd, &fl));
}
#endif
static MaskTable FlockMasks[] = {

{ LOCK_SH,	"LOCK_SH",	0,	0	},
{ LOCK_EX,	"LOCK_EX",	0,	0	},
{ LOCK_NB,	"LOCK_NB",	0,	0	},
{ LOCK_UN,	"LOCK_UN",	0,	0	},
{ 0,		(char *)0,	0,	0	}
};

I sysflock(fd, aoperation)
  I	fd;
  A	aoperation;
{
  unsigned long	operation;

  if (SymbolsToMask(FlockMasks, aoperation, &operation)) {
    q = ERR_DOMAIN;
    R -1;
  } else
    R (I)flock((int)fd, (int)operation);
}

I sysgetdtablesize()
 {
  R sysconf(_SC_OPEN_MAX);
 }

static MaskTable AccessMasks[] = {
{ R_OK,	"R_OK",		0,	0},
{ W_OK,	"W_OK",		0,	0},
{ X_OK,	"X_OK",		0,	0},
{ F_OK,	"F_OK",		0,	0},
{ 0,	(char *)0,	0,	0}
};

I sysaccess(apath, amode)
  A	apath, amode;
{
  char		*path;
  unsigned long	mode;

  if ((path = AToString(apath)) == (char *)(-1) ||
      SymbolsToMask(AccessMasks, amode, &mode)) {
    q = ERR_DOMAIN;
    R 0;
  } else
    R (I)access(path, (int)mode);
}

static MaskTable OpenMasks[] = {
{ O_RDONLY,	"O_RDONLY",	0,	0	},
{ O_WRONLY,	"O_WRONLY",	0,	0	},
{ O_RDWR,	"O_RDWR",	0,	0	},
{ O_NDELAY,	"O_NDELAY",	0,	0	},
{ O_NOCTTY,	"O_NOCTTY",	0,	0	},
{ O_NONBLOCK,	"O_NONBLOCK",	0,	0	},
{ O_APPEND,	"O_APPEND",	0,	0	},
{ O_CREAT,	"O_CREAT",	0,	0	},
{ O_TRUNC,	"O_TRUNC",	0,	0	},
{ O_EXCL,	"O_EXCL",	0,	0	},
{ 0,		(char *)0,	0,	0	}
};

I sysopen(afilename, aflags, mode)
  A	afilename;
  A	aflags;
  I	mode;
{
  char		*filename;
  unsigned long	flags;

  if ((filename = AToString(afilename)) == (char *)(-1) ||
      SymbolsToMask(OpenMasks, aflags, &flags)) {
    q = ERR_DOMAIN;
    R 0;
  } else
    R (I)open(filename, (int)flags, (int)mode);
}

A sysgetdomainname()
{
  A	r;
  char	name[64];
  r = (getdomainname(name, 64)) ? (A)gz() : (A)gsv(0, name);
  R r;
}

A sysgethostname()
{
  A	r;
  char	name[MAXHOSTNAMELEN];
  r = (gethostname(name, MAXHOSTNAMELEN)) ? (A)gz() : (A)gsv(0, name);
  R r;
}


static A username(uid)
I uid;
{
  struct passwd *pwd;
#if defined(APLUS_THREAD_SAFE_FUNCTIONS)
  struct passwd pwdStruct;
  char charBuf[1024];
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

  APLUS_GETPWUID(uid,&pwdStruct,charBuf,1024,pwd);
  if (pwd!=0 && pwd->pw_name)
  {
    R (A)gsv(0,pwd->pw_name);
  }
  else
  {
    R (A)gz();
  }
}


static A pwinfo(aobj)
  A aobj;
{
  I uid;
  I isint=0;
  C *name;
  struct passwd *pwd;
#if defined(APLUS_THREAD_SAFE_FUNCTIONS)
  struct passwd pwdStruct;
  char charBuf[1024];
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

  if(Et==aobj->t && 1==aobj->n && QS(*aobj->p)) name=XS(*aobj->p)->n;
  else if (Ct==aobj->t && 0<aobj->n) name=(C *)aobj->p;
  else if (It==aobj->t && 1==aobj->n) {isint=1; uid=*aobj->p; }
  else ERROUT(ERR_DOMAIN);

  if (isint)
    {
      APLUS_GETPWUID(uid,&pwdStruct,charBuf,1024,pwd);
    }
  else
    {
      APLUS_GETPWNAM(name,&pwdStruct,charBuf,1024,pwd);
    }

  if (pwd)
  {
    A z=gv(Et,9);
    I *pp=z->p;
    pp[0]=(I)gsv(0,(pwd->pw_name)?pwd->pw_name:"");
    pp[1]=(I)gsv(0,(pwd->pw_passwd)?pwd->pw_passwd:"");
    pp[2]=(I)gi(pwd->pw_uid);
    pp[3]=(I)gi(pwd->pw_gid);
#if defined(_AIX) || defined(__osf__) || defined(linux) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
    pp[4]=(I)gsv(0,"");
    pp[5]=(I)gsv(0,"");
#else
    pp[4]=(I)gsv(0,(pwd->pw_age)?pwd->pw_age:"");
    pp[5]=(I)gsv(0,(pwd->pw_comment)?pwd->pw_comment:"");
#endif
    pp[6]=(I)gsv(0,(pwd->pw_gecos)?pwd->pw_gecos:"");
    pp[7]=(I)gsv(0,(pwd->pw_dir)?pwd->pw_dir:"");
    pp[8]=(I)gsv(0,(pwd->pw_shell)?pwd->pw_shell:"");
    R z;
  }
  R (A)gz();
}


static A getusername()
{
  struct passwd *pwd;
#if defined(APLUS_THREAD_SAFE_FUNCTIONS)
  struct passwd pwdStruct;
  char charBuf[1024];
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

  APLUS_GETPWUID(getuid(),&pwdStruct,charBuf,1024,pwd);
  if (pwd!=0 && pwd->pw_name)
  {
    R (A)gsv(0,pwd->pw_name);
  }
  else
  {
    R (A)gz();
  }
}

static I syschdir(str)C *str;
{
  I rc=chdir(str);
  if(!rc)setPWD();
  R rc;
}

ENTRYPOINT
I ep_syssleep(aobj)A aobj;
{
  F fsecs;
  I isecs;
  if(!aobj->n)ERROUT(ERR_LENGTH);
  switch(aobj->t)
  {
  case It:isecs=*aobj->p;if(0>isecs)isecs=0;R sleep(isecs);
  case Ft:
    fsecs=*(F*)aobj->p;
    if(0.0>fsecs)fsecs=0.0;
    if (2147.0<fsecs){
      sleep((int)fsecs);
    } else {
      usleep((int)(1000000.0*fsecs));
    }
    
  R 0;
  default:ERROUT(ERR_TYPE);
  }
}

ENTRYPOINT
I zombiekiller()
{
  int statusp, count=0;
  while (waitpid(-1, &statusp, WNOHANG) > 0) ++count;
  R count;
}

void sysLoop()
{
  int didwork;
  int saveZeroTimeout = dapZeroTimeout;
  
  dapZeroTimeout = 0;
  while (!q && dapbreak == 0)
  {
     dapselect();
     sgnlproc();
     chanproc();
     timerproc();
     slpqproc();
  }
  didwork = 1;
  dapZeroTimeout = 1;
  while (didwork)
  {
     didwork = 0;
     dapselect();
     didwork |= sgnlproc();
     didwork |= chanproc();
     didwork |= timerproc();
     didwork |= slpqproc();
  }
  dapZeroTimeout = saveZeroTimeout;
}

static EnumTable SignalEnums[] = {
{ SIGABRT,      "SIGABRT",      0       },
{ SIGALRM,      "SIGALRM",      0       },
{ SIGBUS,       "SIGBUS",       0       },
{ SIGCHLD,      "SIGCHLD",      0       },
{ SIGCONT,      "SIGCONT",      0       },
{ SIGFPE,       "SIGFPE",       0       },
{ SIGHUP,       "SIGHUP",       0       },
{ SIGILL,       "SIGILL",       0       },
{ SIGINT,       "SIGINT",       0       },
{ SIGIO,        "SIGIO",        0       },
{ SIGIOT,       "SIGIOT",       0       },
{ SIGKILL,      "SIGKILL",      0       },
{ SIGPIPE,      "SIGPIPE",      0       },
{ SIGPROF,      "SIGPROF",      0       },
{ SIGQUIT,      "SIGQUIT",      0       },
{ SIGSEGV,      "SIGSEGV",      0       },
{ SIGSTOP,      "SIGSTOP",      0       },
{ SIGTERM,      "SIGTERM",      0       },
{ SIGTRAP,      "SIGTRAP",      0       },
{ SIGTSTP,      "SIGTSTP",      0       },
{ SIGTTIN,      "SIGTTIN",      0       },
{ SIGTTOU,      "SIGTTOU",      0       },
{ SIGURG,       "SIGURG",       0       },
{ SIGUSR1,      "SIGUSR1",      0       },
{ SIGUSR2,      "SIGUSR2",      0       },
{ SIGVTALRM,    "SIGVTALRM",    0       },
{ SIGWINCH,     "SIGWINCH",     0       },
{ SIGXCPU,      "SIGXCPU",      0       },
{ SIGXFSZ,      "SIGXFSZ",      0       },
{ 0,            (char *)0,      0       }
};
 
static I sysKill(pid_, signal_)
int pid_;
A signal_;
{
  int aSignal;
  if (SymbolToEnum(SignalEnums, signal_, &aSignal)) {
    (void)pa((V)signal_);
    q = ERR_DOMAIN;
    R 0;
  } else
    R (I)kill((pid_t)pid_, aSignal);
}

void eponymousInstall()
{
  CX saveCx=Cx;
  Cx=cx("sys");

  install((PFI)sysErrno, "errno", IV,0,0,0,0,0,0,0,0,0);
  install((PFI)sysErrsym, "errsym", A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)sysaccess, "access", IV,2,A_,A_,0,0,0,0,0,0);
  install((PFI)agetdents, "agetdents", A_, 1, CP,0,0,0,0,0,0,0);
  install((PFI)alstat, "alstat", A_, 1, CA,0,0,0,0,0,0,0);
  install((PFI)amsync,"amsync", IV,2,A_,A_,0,0,0,0,0,0);
  install((PFI)areadlink, "areadlink", A_, 1, CP,0,0,0,0,0,0,0);
  install((PFI)aselect, "aselect", A_,4,IA,IA,IA,IA,0,0,0,0);
  install((PFI)astat, "astat", A_, 1, CA,0,0,0,0,0,0,0);
  install((PFI)syschdir, "chdir", IV,1,CP,0,0,0,0,0,0,0);
  install((PFI)chmod, "chmod", IV,2,CP,IV,0,0,0,0,0,0);
  install((PFI)chown, "chown", IV,3,CP,IV,IV,0,0,0,0,0);
  install((PFI)close, "close", IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)closelog, "closelog", V_, 0,0,0,0,0,0,0,0,0);
  install((PFI)cpu, "cpu", A_,0,0,0,0,0,0,0,0,0);
  install((PFI)creat, "creat", IV,2,CP,IV,0,0,0,0,0,0);
  install((PFI)dup, "dup", IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)dup2, "dup2", IV,2,IV,IV,0,0,0,0,0,0);
  install((PFI)exit, "exit", IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)fchmod, "fchmod", IV,2,IV,IV,0,0,0,0,0,0);
  install((PFI)fchown, "fchown", IV,3,IV,IV,IV,0,0,0,0,0);
  install((PFI)sysfcntl, "fcntl", IV,3,IV,A_,IV,0,0,0,0,0);
  install((PFI)fflush_stdout, "fflush_stdout", IV,0,0,0,0,0,0,0,0,0);
  install((PFI)filesize, "filesize", A_,1,CP,0,0,0,0,0,0,0);
  install((PFI)sysflock, "flock", IV,2,IV,A_,0,0,0,0,0,0);
  install((PFI)fsync, "fsync", IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)ftruncate, "ftruncate", IV, 2, IV, IV,0,0,0,0,0,0);
  install((PFI)sysgetdtablesize, "getdtablesize", IV,0,0,0,0,0,0,0,0,0);
  install((PFI)getgid, "getgid", IV, 0,0,0,0,0,0,0,0,0);
  install((PFI)getpid, "getpid", IV, 0,0,0,0,0,0,0,0,0);
  install((PFI)getppid, "getppid", IV, 0,0,0,0,0,0,0,0,0);
  install((PFI)getsockopt, "getsockopt", IV, 5, IV, IV, IV, IP, IP,0,0,0);
  install((PFI)gettod, "gettod", A_, 1, IA,0,0,0,0,0,0,0);
  install((PFI)getuid, "getuid", IV, 0,0,0,0,0,0,0,0,0);
  install((PFI)geteuid, "geteuid", IV, 0,0,0,0,0,0,0,0,0);
  install((PFI)getusername, "getusername", A_, 0,0,0,0,0,0,0,0,0);
  install((PFI)sysioctl, "ioctl", IV,3,IV,A_,IV,0,0,0,0,0);
  install((PFI)link, "link", IV,2,CP,CP,0,0,0,0,0,0);
  install((PFI)lseek, "lseek", IV,3,IV,IV,IV,0,0,0,0,0);
  install((PFI)mkdir, "mkdir", IV,2,CP,IV,0,0,0,0,0,0);
  install((PFI)mkts1, "mkts1", A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)mkts1gmt, "mkts1gmt", A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)pathfind, "pathfind", A_, 4, CP, CP, CP, IV,0,0,0,0);
  install((PFI)perror,"perror", V_,1,CP,0,0,0,0,0,0,0);
  install((PFI)read, "read", IV,3,IV,P_,IV,0,0,0,0,0);
  install((PFI)readenv,"readenv", A_, 1, CP,0,0,0,0,0,0,0);
  install((PFI)read, "readinto", IV,3,IV,IV,IV,0,0,0,0,0);
  install((PFI)readmat, "readmat", A_, 1, CP,0,0,0,0,0,0,0);
  install((PFI)rename, "rename", IV,2,CP,CP,0,0,0,0,0,0);
  install((PFI)rmdir, "rmdir", IV,1,CP,0,0,0,0,0,0,0);
  install((PFI)secs_in_epoch, "secs_in_epoch", IV,0,0,0,0,0,0,0,0,0);
  install((PFI)ep_setenv,"setenv", IV,1,CP,0,0,0,0,0,0,0);
  install((PFI)setsockopt, "setsockopt", IV, 5, IV, IV, IV, IP, IV,0,0,0);
  install((PFI)ep_syssleep, "sleep", IV,1,A_,0,0,0,0,0,0,0);
  install((PFI)symlink, "symlink", IV,2,CP,CP,0,0,0,0,0,0);
  install((PFI)sysgetdomainname, "getdomainname", A_,0,0,0,0,0,0,0,0,0);
  install((PFI)sysgethostname, "gethostname", A_,0,0,0,0,0,0,0,0,0);
  install((PFI)sysopen, "open", IV,3,A_,A_,IV,0,0,0,0,0);
  install((PFI)sysopenlog, "openlog", V_, 3, CA, A_, A_,0,0,0,0,0);
  install((PFI)syssyslog, "syslog", V_, 2, A_, CP,0,0,0,0,0,0);
  install((PFI)syst, "system", IV,1,CP,0,0,0,0,0,0,0);
  install((PFI)truncate, "truncate", IV, 2, CP, IV,0,0,0,0,0,0);
  install((PFI)ts, "ts", A_,0,0,0,0,0,0,0,0,0);
  install((PFI)tsgmt, "tsgmt", A_,0,0,0,0,0,0,0,0,0);
  install((PFI)ts1, "ts1", A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)ts1gmt, "ts1gmt", A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)tzset, "tzset", V_,0,0,0,0,0,0,0,0,0);
  install((PFI)umask, "umask", IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)unlink, "unlink", IV,1,CP,0,0,0,0,0,0,0);
  install((PFI)updtime, "updtime", A_,1,CP,0,0,0,0,0,0,0);
  install((PFI)username, "username", A_, 1, IV,0,0,0,0,0,0,0);
  install((PFI)pwinfo, "pwinfo", A_, 1, A_,0,0,0,0,0,0,0);
  install((PFI)write, "write", IV,3,IV,P_,IV,0,0,0,0,0);
  install(zombiekiller, "zombiekiller", IV,0,0,0,0,0,0,0,0,0);
  install(sysKill, "kill", IV,2,IV,A_,0,0,0,0,0,0);
  Cx = saveCx;
  R;
}

