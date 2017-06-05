/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/arthur.h>
#include <a/ik.h>
#include <dap/balloc.h>
#ifndef __s390__
#include <string.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <syslog.h>
#ifdef __sgi
#include <setjmp.h>
#endif

extern I dbg_twa;

/* ----------------------------------------------------  */
/* reliable version of signal(), using POSIX sigaction().*/
/*  From: "Advanced Programming in the Unix Environment  */
/* Author  W. Richard Stevens, 1992 page 298             */

typedef void Sigfunc(int);

/* 4.3BSD Reno <signal.h>  doesn't define SIG_ERR */
#if defined(SIG_IGN) && !defined(SIG_ERR)
#define SIG_ERR ((Sigfunc *)-1)
#endif

Sigfunc * 
aplus_signal(int signo, Sigfunc *func)
{
  struct sigaction act, oact;

  act.sa_handler = func;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  if(signo == SIGALRM) {
#ifdef SA_INTERRUPT
    act.sa_flags |= SA_INTERRUPT;	/* SunOS */
#endif
  } else {
#ifdef SA_RESTART
    act.sa_flags |= SA_RESTART;	/* SVR4, 4.3+BSD */
#endif
  }
  if(sigaction(signo, &act, &oact) < 0)
    return(SIG_ERR);
  else
    return(oact.sa_handler);
}

/* ----------------------------------------------------  */

I ep_all(void);
Z int mkt(C *);
I map(int,int);
void aplus_nan(void);

#ifndef HAVE_STRERROR
  extern char *sys_errlist[];
  extern int sys_nerr;
#endif
I log_EWouldBlock(I i,I rc,I nern,C *path,C *fcn)
{ /* NOTE: use "tries" instead of "EWOULDBLOCKs" for user messages */
  if(-1==rc){
    if(EWOULDBLOCK==nern){
      syslog(LOG_NOTICE,
	     "A+ %s failed for '%s' after %d EWOULDBLOCKs",fcn,path,i);
    H("\343 A+ %s failed for '%s' after %ld tries\n",fcn,path,i);
    } else {
#ifndef HAVE_STRERROR
      syslog(LOG_INFO,
	     "A+ %s failed for '%s' after %d EWOULDBLOCKs with: %m",fcn,path,i);
      H("\343 A+ %s failed for '%s' after %ld tries with: %s\n",
	fcn,path,i,(nern<sys_nerr)?sys_errlist[nern]:"unknown system error");
#else
      char *errstr=strerror(nern);
      syslog(LOG_INFO,
	     "A+ %s failed for '%s' after %d EWOULDBLOCKs with: %m",fcn,path,i);
      H("\343 A+ %s failed for '%s' after %ld tries with: %s\n",
	fcn,path,i,errstr?errstr:"unknown system error");
#endif
    }
  } else { 
    syslog(LOG_INFO,
	   "A+ %s succeeded for '%s' after %d EWOULDBLOCKs",fcn,path,i);
    H("\343 A+ %s succeeded for '%s' after %ld tries\n",fcn,path,i);
  }
  R nern;			/* Return error so errno can be restored */
}

Z int loaccess(C *path, int mode)
{
  int i=0,st=1,rc=0;
  static C fcn[] = "access";
  if(!path) R -1;
  while(i<10&&-1==(rc=access(path,mode))&&EWOULDBLOCK==errno)
  {sleep(st);if(8>st)st*=2;++i;}
  if (i) errno=log_EWouldBlock(i,rc,errno,path,fcn);
  R rc;
}

Z int lostat(C *path,struct stat *buf)
{
  int i=0,st=1,rc=0;
  static C fcn[] = "stat";
  if(!path) R -1;
  while(i<10&&-1==(rc=stat(path,buf))&&EWOULDBLOCK==errno)
  {sleep(st);if(8>st)st*=2;++i;}
  if (i) errno=log_EWouldBlock(i,rc,errno,path,fcn);
  R rc;
}

Z int lofstat(int fd,struct stat *buf)
{
  int i=0,st=1,rc=0;
  static C fcn[] = "fstat";
  while(i<10&&-1==(rc=fstat(fd,buf))&&EWOULDBLOCK==errno)
  {sleep(st);if(8>st)st*=2;++i;}
  if (i) errno=log_EWouldBlock(i,rc,errno,"...",fcn);
  R rc;
}

Z int lofchmod(int fd, mode_t mode)
{
  I i=0,st=1,rc=0;
  static C fcn[] = "fchmod";
  while(i<10&&-1==(rc=fchmod(fd,mode))&&EWOULDBLOCK==errno)
  {sleep(st);if(8>st)st*=2;++i;}
  if (i) errno=log_EWouldBlock(i,rc,errno,"...",fcn); 
  R rc;
}

#ifdef __sgi
jmp_buf ovli_b; /* stuffed by setjmp in ovli_mips.s */

Z void sigfpe(int i) /* enabled by "add" instruction in ovli_mips.s */
{
  siglongjmp(ovli_b, 1);
} /* jss */
#endif

Z I qw;
Z void sigi(int i){q=1;}
Z void sigv(int i){qs="segv";aplus_err(-1,0);}
Z void sigb(int i){qs="bus";aplus_err(-1,0);}
I syst(C *s){I r;qw=0,r=system(s),qw=1,r;if(r==-1)H("%ld\n",r);R r;}
Z struct stat b;

void gwd(C *s)
{
  Z C r[99];
  Z dev_t d;
  Z ino_t i;
  lostat(".",&b);
  if(d!=b.st_dev||i!=b.st_ino)
    d=b.st_dev,i=b.st_ino,qw=0,getcwd(r,99),qw=1;
  strcpy(s,r);
}

I setStickyBit(int f,int x)
{
  lofstat(f,&b);
  R lofchmod(f,(x?S_ISVTX:0x00)|b.st_mode);
}

void siginit(void)
{
  coreLimSet(0);
  aplus_signal(SIGPIPE,SIG_IGN);
  aplus_signal(SIGINT,sigi);
  aplus_signal(SIGSEGV,sigv);
  aplus_signal(SIGBUS,sigb);
#if defined(__sgi)
  aplus_signal(SIGFPE,sigfpe); /* jss */
#endif
#if defined(__osf__)
  aplus_signal(SIGFPE, SIG_IGN) ;
#endif
  aplus_nan();
}

Z int sigvFlag=0, sigbFlag=0;
A getSigv(void){R gi(sigvFlag);}
A getSigb(void){R gi(sigbFlag);}

#if defined(_AIX)
Z sigFullDump(int signo)
{
  struct sigaction s;
  s.sa_handler = SIG_DFL;
  s.sa_mask.losigs = 0;
  s.sa_mask.hisigs = 0;
  s.sa_flags = SA_FULLDUMP;
  sigaction(signo,&s,(struct sigaction *) NULL);
}
void setSigv(I flag){sigvFlag=(0==flag)?0:(2==flag)?2:1;
  switch(sigvFlag){CS(0,aplus_signal(SIGSEGV,sigv))CS(1,aplus_signal(SIGSEGV,SIG_DFL))
		     CS(2,sigFullDump(SIGSEGV))}}
void setSigb(I flag){sigbFlag=(0==flag)?0:(2==flag)?2:1;
  switch(sigbFlag){CS(0,aplus_signal(SIGBUS,sigv))CS(1,aplus_signal(SIGBUS,SIG_DFL))
		     CS(2,sigFullDump(SIGBUS))}}
#else
void setSigv(I flag){sigvFlag=(0==flag)?0:(2==flag)?2:1;
  if(sigvFlag)aplus_signal(SIGSEGV,SIG_DFL);else aplus_signal(SIGSEGV,sigv);}
void setSigb(I flag){sigbFlag=(0==flag)?0:(2==flag)?2:1;
  if(sigbFlag)aplus_signal(SIGBUS ,SIG_DFL);else aplus_signal(SIGBUS ,sigv);}
#endif

Z int f[9]; Z I j=0,k=0;
Z C z[]="/var/atmp/0/aXXXXXX",c[]="/var/atmp/0";

I suppressFpeDomain=0;
I nExternalFPE=0;
#if !defined(_AIX) && !defined(HAVE_SVR4)
Z void sigf(void){if(suppressFpeDomain)++nExternalFPE; else q=9;}
#endif

#include <sys/mman.h>
#if defined(_AIX) || defined(HAVE_SVR4) || defined(linux) || defined(_HP) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#ifdef __sgi
Z unsigned k1=0x20000000,k2=0x100000;
#else
Z unsigned k1=0x40000000,k2=0x100000;
#endif
void aplus_nan(void){}

I map(int f,int mode)
{
  I junk=0, *p;
  I iseek ;

/*read(f,&junk,sizeof(I));
 *printf("%ld\n", junk) ;
 */
  /* cokelley 64 hack */
#if  (_MIPS_SZLONG == 64)
/*	printf("64 bit mips version of mmap\n") ; */
  iseek = lseek64(f, 0, SEEK_END) ;
#else
  iseek = lseek(f, 0, SEEK_END) ;
#endif
/*	printf("Calling mmap(0, %ld, ..., %d, 0\n", iseek, f) ; */
#if defined(__osf__)
  p=(I *)mmap(0,iseek,PROT_READ|(BEAM_RO==mode?0:PROT_WRITE),
	      MAP_FILE||(BEAM_LOCAL==mode)?MAP_PRIVATE:MAP_SHARED,f,0);
#else
  p=(I *)mmap(0,iseek,PROT_READ|(BEAM_RO==mode?0:PROT_WRITE),
	      (BEAM_LOCAL==mode)?MAP_PRIVATE:MAP_SHARED,f,0);
#endif
/*  printf("mmap returned: %ld\n", p) ; */
  if ( p == MAP_FAILED)
    {
      perror("mmap failed") ;
      p=0;			/* fail return value */
    }
  
/*  printf("%ld\n", p[0]) ;
 * if(BEAM_RO!=mode)p[0]=junk;
 *  printf("%ld\n", p[0]) ;
 */
  close(f);R (I)p;
}
#else
Z unsigned k1=0x4000000,k2=0x100000;
#ifdef __VISUAL_C_2_0__
void aplus_nan(void) { }
#elif defined(__osf__)
void aplus_nan(void) { }
#else
void aplus_nan(void){ieee_handler("set","invalid",sigf);/*ieee_handler("set","division",sigf);*/}
#endif
/* I map(int f,int i){I p=(I)mmap(0,lseek(f,0,2),PROT_READ|(i?PROT_WRITE:0),i&2?MAP_PRIVATE:MAP_SHARED,f,0);close(f);R p;} */
#endif

I setk1(I megs){k1=megs<<20;R k1;}

/* Set Default atmp flags */
#if defined(_WS_ATMP_NORESERVE)
static int mmapAtmpFlags= MAP_PRIVATE|MAP_NORESERVE|MAP_FIXED;
#else
static int mmapAtmpFlags= MAP_SHARED|MAP_FIXED; 
#endif
static int atmpFromHeap=0;

void setAtmpMmapFlags(atmpMode)
int atmpMode;
{
  if( atmpMode == WS_ATMP_HEAP )
    atmpFromHeap=1;
  else if( atmpMode == WS_ATMP_SHARED )
    mmapAtmpFlags= MAP_SHARED|MAP_FIXED;
  else if( atmpMode == WS_ATMP_PRIVATE )
    mmapAtmpFlags= MAP_PRIVATE|MAP_FIXED; 
  else if( atmpMode == WS_ATMP_NORESERVE )
#if defined(_AIX) 
    mmapAtmpFlags= MAP_PRIVATE|MAP_FIXED;
#elif defined(__osf__)
	mmapAtmpFlags = MAP_PRIVATE | MAP_FIXED ;
#elif defined(__sgi)
     mmapAtmpFlags= MAP_PRIVATE|MAP_AUTORESRV|MAP_FIXED;
#else
  mmapAtmpFlags= MAP_PRIVATE|MAP_NORESERVE|MAP_FIXED;
#endif 
}

int flen(int f,off_t n)
{
  int z;
  if(-1==(z=ftruncate(f,n)))
    perror("ftruncate failed");
  R z;
}

int atmpMissing() { c[10]='0'; return loaccess(c,6) ? 1 : 0; }

void wi(void)
{
  if( atmpFromHeap )
    return;
  for(;!loaccess(c,6);z[10]=c[10]='0'+ ++j)
    f[j]=mkt(z);
}

/* #define mapf(f,o) (I)mmap((caddr_t)k1,(size_t)k2,PROT_READ|PROT_WRITE, \ */
/* 			  MAP_SHARED|MAP_FIXED,f,(off_t)o) */

/* Change from MAP_SHARED to MAP_PRIVATE to improve performance */
#define mapf(f,o) mmap((caddr_t)k1,(size_t)k2,PROT_READ|PROT_WRITE, \
			  mmapAtmpFlags,f,(off_t)o)

Z I wsm(I m)
{
/*   I p=k1,z=(k+m+j-1)/j*k2; */
/*   DO(j,flen(f[i],z)); */
/*   DO(m,if(-1==mapf(f[k%j],k2*(k/j)))R -1;++k;k1+=k2); */
/*   R p; */

  I i,p=k1;
  off_t z=(k+m+j-1)/j*k2;

  for(i=0; i<j; i++)
    if(-1==flen(f[i],z)) R -1;
  
  for(i=0; i<m; i++)
    {
      if(MAP_FAILED==mapf(f[k%j],k2*(k/j)))
	{
	  perror("mmap failed");
	  R -1;
	}
      ++k;
      k1+=k2;
    }
  R p;
}

#if defined(HAVE_SVR4) || defined(__VISUAL_C_2_0__)
Z int mkt(C *b)
{
  int f=open((mktemp(b),b),O_RDWR | O_CREAT,0600);
  setStickyBit(f,1);
  unlink(b);
  strcpy(b+strlen(b)-6,"XXXXXX");
  R fcntl(f,F_SETFD,1|fcntl(f,F_GETFD,0)),f;
}
#else
Z int mkt(C *b)
{
  int f=mkstemp(b);
  setStickyBit(f,1);
  unlink(b);
  strcpy(b+strlen(b)-6,"XXXXXX");
  R fcntl(f,F_SETFD,1|fcntl(f,F_GETFD,0)),f;
}
#endif
Z I mal(I n){I p=(I)malloc(n);if(!p)perr("malloc");R p;}
Z I tw;

/*---------------------------*/

#ifdef BSTUB
I tmp(I n){return 1;}
#else
I tmp_malloc(I n){R 1;}	/* pretend like it worked, let malloc do an sbrk */

I tmp_atmp(I n)
{
  I m,p=j||k?(m=1+(n-1)/k2,n=m*k2,wsm(m)):mal(n);
  R tw+=n,p?(mb((long *)p,n/sizeof(long)),1):0;
}

I (*pf_tmp)(I)=tmp_atmp; 

I tmp(I n){return pf_tmp(n);}
#endif
/*---------------------------*/

#ifdef BSTUB
I wa(I k){printf("\343 wa(%ld) called: no-op\n",k);return k<<20;}
#else

extern unsigned long MZ[];
I wa(I k)
{
  I j,n=0,*p;
  if(dbg_twa) watrc(k);
  k_tm(0);
  if(k>0)R tmp(k<<20);
  if(k==-2)mc();
  p=(I*)mz();
  DO(31,j=p[i];n+=j*MZ[i];if(k!=-1)H("%d ",j));
  if(k!=-1)H("\n%u %u: ",tw,ep_all());
  H("%u\n",n<<2);
  R -1;
}

#endif

I twGet(void){R tw;}
I ep_all(void){I s=0;if(!j)R tw;DO(j,lofstat(f[i],&b);s+=512*b.st_blocks)R s;}
/*!!*/

I unloadable(C *s,I m)
{
  struct stat ss;
  if(loaccess(s,m))R 1;
  if(lostat(s,&ss))R 1;
  if(S_ISDIR(ss.st_mode))R 1;
  R 0;
}

#ifdef _WIN32
C *pfind(C *v,C *d,C *f,I m)
{
  Z C s[MAXPATHLEN];
  C pathDelim=';';		/* default path delimiter */
  C subDirDelim[2]={"\\"};	/* default dir  delimiter */

  if( f[1]==':' || *f=='/' )	/* Fully qualified /... or d:... */
    R unloadable(f,m)?0:f;

  if( v && (v=getenv(v)) )	/* Retrieve PATH environment var */
    d=v;

  if( v && strchr(v,'/') ){	/* Change to Unix style */
    pathDelim=':';
    dirDelm='/';
  }

  while(d) {
    if(v=(C *)strchr(d,pathDelim))
      *s=0,strncat(s,d,v-d),d=v+1;
    else 
      strcpy(s,d),d=0; 
    strcat(s,subDirDelim),strcat(s,f); 
    if(!unloadable(s,m))
      R s;
  }
  R 0;
}
#else
C *pfind(C *v,C *d,C *f,I m)
{
  Z C s[MAXPATHLEN];
  if(*f=='/')R unloadable(f,m)?0:f;
  for((v&&(v=getenv(v)))?d=v:0;d;) {
    if(v=(C *)strchr(d,':'))*s=0,strncat(s,d,v-d),d=v+1;
    else strcpy(s,d),d=0; 
    strcat(s,"/"),strcat(s,f); 
    if(!unloadable(s,m))R s;
  }
  R 0;
}
#endif
