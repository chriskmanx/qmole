/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/param.h>

#include <dap/balloc.h>

#include <development.h>

#include <a/f.h>
#include <a/fncdcls.h>
#include <a/ik.h>
#include <a/arthur.h>
#include <a/fir.h>
#include <a/beam.h>

extern I dbg_tb, dbg_tnan;

extern C *findMapped32FileName();
extern C *findMapped64FileName();
extern C *findFileName();

Z I items(I n,A z);

/* flag to control auto/endain conversion of the .m file  */
Z I autoBeamConvert=0;		/* Default is off  */
I getAutoBeamConvert(void)   {R autoBeamConvert;}
void setAutoBeamConvert(I m) {autoBeamConvert=m;}

Z I mapOut(C *s,A z) ;

int isWrongEndian(A aobj)
{
  return (aobj->r)?(!(1<=aobj->r&&MAXR>=aobj->r)):1!=aobj->n;
}

typedef union {char *c;unsigned char *uc;u_long *u;I *i;F *f;} EndianUnion;

static EndianUnion EndianTag={"abcd"};

static C *endianString(I endian)
{
  return (endian==ENDIAN_BIG)?"big":(endian==ENDIAN_LITTLE)?"little":
    (endian==ENDIAN_UNDEF)?"undef":"unknown";
}

static I hostEndian(void)
{
  return (0x61626364==*EndianTag.u)?ENDIAN_BIG:
    (0x64636261==*EndianTag.u)?ENDIAN_LITTLE:
    ENDIAN_UNDEF;
}

/* non-overlapping endian-swapping copy!! - Method 1 - one loop */
static void ndnicopy(I *from,I *to,I nints)
{
  unsigned char *cfrom=(unsigned char *)from, *cto=(unsigned char *)to;
  for(;nints--;cfrom+=4,cto+=4)
  {
    cto[3]=cfrom[0];cto[2]=cfrom[1];cto[1]=cfrom[2];cto[0]=cfrom[3];
  }
}

static void ndnfcopy(F *from,F *to,I nfs)
{
  unsigned char *cfrom=(unsigned char *)from, *cto=(unsigned char *)to;
  for(;nfs--;cfrom+=8,cto+=8)
  {
    cto[7]=cfrom[0];cto[6]=cfrom[1];cto[5]=cfrom[2];cto[4]=cfrom[3];
    cto[3]=cfrom[4];cto[2]=cfrom[5];cto[1]=cfrom[6];cto[0]=cfrom[7];
  }
}


/* in-place endian swap */
static void ndniswap(I *start,I nints)
{
  I tempi;
  EndianUnion tu, eu;

  tu.i=&tempi;
  for(eu.i=start;nints--;++eu.i)
  {
    tempi=*eu.i;
    eu.uc[3]=tu.uc[0];
    eu.uc[2]=tu.uc[1];
    eu.uc[1]=tu.uc[2];
    eu.uc[0]=tu.uc[3];
  }
}

static void ndnfswap(F *start,I nfs)
{
  F tempf;
  EndianUnion tu, eu;

  tu.f=&tempf;
  for(eu.f=start;nfs--;++eu.f)
  {
    tempf=*eu.f;
    eu.uc[7]=tu.uc[0];
    eu.uc[6]=tu.uc[1];
    eu.uc[5]=tu.uc[2];
    eu.uc[4]=tu.uc[3];
    eu.uc[3]=tu.uc[4];
    eu.uc[2]=tu.uc[5];
    eu.uc[1]=tu.uc[6];
    eu.uc[0]=tu.uc[7];
  }
}

static I aobjEndian(A aobj)
{
  int result=ENDIAN_UNDEF;
  I tempint;
  int hostndn=hostEndian();
  if(!isWrongEndian(aobj))result=hostndn;
  else 
  {
    ndnicopy(&aobj->r,&tempint,1);
    if(0<=tempint&&9>=tempint)
      result=(ENDIAN_BIG==hostndn)?ENDIAN_LITTLE:ENDIAN_BIG;
  }
  return(result);
}


static void doSwapEndianInPlace(A aobj, I targetndn)  
{
  I n,t;
  I sourcendn=aobjEndian(aobj);
  I hostndn=hostEndian();
  if(targetndn==sourcendn)return;
  if(targetndn==hostndn)
  {
    ndniswap((I *)aobj,AH/sizeof(I));
    n=aobj->n; t=aobj->t;
  } else {
    n=aobj->n; t=aobj->t;
    ndniswap((I *)aobj,AH/sizeof(I));
  }
  switch(t)
  {
  case It:
    ndniswap(aobj->p,n);
    break;
  case Ft:
    ndnfswap((F *)aobj->p,n);
    break;
  case Ct:
    break;
  default:
    printf("\343 error!!: doSwapEndianInPlace encountered bad type:%ld\n",t);
    break;
  }
}

static A doCopySwitchEndian(A aobj,I targetndn)
{
  static struct a tempAobj;
  A z;
  I n,t;
  I sourcendn=aobjEndian(aobj);
  I hostndn=hostEndian();
  if(targetndn==sourcendn)return (A)ic(aobj);
  if(targetndn==hostndn)
  {
    ndnicopy((I *)aobj,(I *)&tempAobj,AH/sizeof(I));
    z=gd(tempAobj.t,&tempAobj);
    n=z->n; t=z->t;
  } else {
    n=aobj->n; t=aobj->t;
    z=gd(aobj->t,aobj);
    ndnicopy((I *)aobj,(I *)z,AH/sizeof(I));
  }
  switch(t)
  {
  case It:
    ndnicopy(aobj->p,z->p,n);
    break;
  case Ft:
    ndnfcopy((F *)aobj->p,(F *)z->p,n);
    break;
  case Ct:
    bcopy(aobj->p,z->p,n+1);
    break;
  default:
    printf("\343 error!!: doCopySwitchEndian encountered bad type:%ld\n",t);
    break;
  }
  return z;
}

A ep_hostEndian(void)
{
  return gsym(endianString(hostEndian()));
}

A ep_aobjEndian(A aobj)
{
  return gsym(endianString(aobjEndian(aobj)));
}

A ep_CopyRightEndian(A aobj)
{
  return doCopySwitchEndian(aobj,hostEndian());
}

void ep_SwapRightEndian(A aobj)  /* must use argument type 12, not 0!!! */
{
  if(!isWrongEndian(aobj))return;
  doSwapEndianInPlace(aobj,hostEndian());
}

A ep_CopyToEndian(A aobj,A andn)
{
  I targetndn;
  if(Et!=andn->t||1!=andn->n||!QS(*andn->p))ERROUT(ERR_TYPE);
  targetndn=(*andn->p==MS(si("big"))) ? ENDIAN_BIG :
    (*andn->p==MS(si("little")))? ENDIAN_LITTLE : ENDIAN_UNDEF;
  if(ENDIAN_UNDEF==targetndn)ERROUT(ERR_DOMAIN);
  return doCopySwitchEndian(aobj, targetndn);
}

A ep_SwapToEndian(A aobj,A andn) /* must use argument type 12, not 0!!! */
{
  I targetndn, sourcendn;
  if(Et!=andn->t||1!=andn->n||!QS(*andn->p))ERROUT(ERR_TYPE);
  targetndn=(*andn->p==MS(si("big"))) ? ENDIAN_BIG :
    (*andn->p==MS(si("little")))? ENDIAN_LITTLE : ENDIAN_UNDEF;
  if(ENDIAN_UNDEF==targetndn)ERROUT(ERR_DOMAIN);
  sourcendn=aobjEndian(aobj);
  if(targetndn!=sourcendn) doSwapEndianInPlace(aobj, targetndn);
  return aplus_nl;
}

Z int loopen(C *path,int flags,mode_t mode)
{
  int i=0,st=1,fd=-1;
  static C fcn[]="open";
  if(!path) R -1;
  while(i<10&&-1==(fd=open(path,flags,mode))&&EWOULDBLOCK==errno)
    {sleep(st);if(8>st)st*=2;++i;}
  if (i) errno=log_EWouldBlock(i,fd,errno,path,fcn);
  R fd;
}

C *tmv32(int t,I *d,int *s,int n) {
  int i;

  switch(t) {
    case It:
    for (i = 0; i < n; i++) {
          *d++ = *s++;
        }
        R(C*)d;

    case Et:
        fprintf(stderr, "\343 Internal Error:beam.c/tmv32 - can't do enclosed types\n");
        R 0;

    case Ft: {
              F *a=(F*)d;
              F *b=(F*)s;
              DO(n,*a++=*b++)
              R(C*)a;
             }
    case Ct: {
              C *a=(C*)d;
              C *b=(C*)s;
              DO(n,*a++=*b++)
              R(C*)a;
             }
  }
  R 0;
}

void
ttmv32(int t,I *d,int *s,int n) {
  C *x=tmv32(t,d,s,n);

  if (x == 0) {
    fprintf(stderr, "\343 Internal Error:beam.c/ttmv32 tmv32 failed\n");
    R;
  }

  if(Ct==t)
    *x='\0';
}

void mv32(I *dest64,int *src32,int n) {
  int i;

  for (i = 0; i < n; i++) {
    *dest64++ = *src32++;
  }
}

A
gc32(int t, int r, int n, int d[], int *p) {
  I f ;
  A z ;

  /*printf("In gc32 (%d, %d, %d ...)\n", t, r, n) ; */
  f = (t==Ct) ;
  z = (A)mab(f+AH+Tt(t,n));

  z->c=1;
  z->t=t;
  z->r=r;
  z->n=n;

  mv32(z->d,d,r);
  ttmv32(t,z->p,p,n);
  if(f)
    ((C*)z->p)[n]=0;
  R z;
}

A
cvt64(A32 *aobj) {   /* mapped objects only - floats, chars and longs */
    A newa;
    I i = aobj->i ;
    /*printf("%d %d %d %d %d\n", aobj->c, aobj->t, aobj->r, aobj->n, aobj->i) ; */
    newa = gc32(aobj->t,aobj->r,aobj->n,aobj->d,aobj->p);
	newa->i = i ;
    R newa;
}


Z A vetteMappedFile(I fsize, I aarg, int mode, C *name, int *rc, I cvtInPlace)
{
  A aobj=(A)aarg,z;

/*   return  0, no conversion,         z is set to aobj */
/*   return  1, conversion successful, z is new object in memory */
/*   return -1, conversion failed,     z is set to NULL */

  *rc=cvtIfNeeded(aobj, &z, fsize, cvtInPlace);
  return z;
}

typedef struct{I a,c,n,w;C *s,*t;}MFInfo;  
  /* n field used for "next" in freelist */

Z I maxMFAlimit=2000;
Z I maxMFAindex=0;
Z I freeMFAindex=0;
Z MFInfo *MappedFileArray=0;
Z C MFAErrorMsg[128];

Z int MFArealloc(I newlim)
{
  Z MFInfo *newMFA;
  I i, count=0;
  if(0==newlim||newlim<maxMFAindex)
  {
    strcpy(MFAErrorMsg,"New limit is less than Mapped File Array length");
    R -1;
  }
  newMFA=(MFInfo *)ma(newlim*(sizeof(MFInfo)/sizeof(I)));
  if(0==newMFA)
  {
    strcpy(MFAErrorMsg,"Error allocating new Mapped File Array");
    R -1;
  }
  if(MappedFileArray) 
  {
    for(i=0;i<maxMFAindex;i++)if(MappedFileArray[i].a)
      tmv(It,(I*)(newMFA+count++),(I*)(MappedFileArray+i),
	  sizeof(MFInfo)/sizeof(I));
  }    
  if (newlim>count)
  {
    bzero(newMFA+count,sizeof(MFInfo)*(newlim-count));
    for(i=count;i<newlim;i++)newMFA[i].n=i+1;
  }
  if(0!=MappedFileArray)mf((I*)MappedFileArray);
  MappedFileArray=newMFA;
  maxMFAlimit=newlim;
  freeMFAindex=maxMFAindex=count;
  R 0;
}

void MFALimitSysCmd(I newlim)
{
  int rc;
  if(0>newlim)
  {
    H("%ld\n",maxMFAlimit);
    R;
  }
  if(0==MappedFileArray){maxMFAlimit=newlim;R;}
  rc=MFArealloc(newlim);
  if(rc) H("\343  maplim error: %s\n",MFAErrorMsg);
}

I MFALimitGet(void){R maxMFAlimit;}

void MFALimitSet(I newlim)
{
  int rc;
  rc=MFArealloc(newlim);
  if(rc) {H("\343  maplim error: %s\n",MFAErrorMsg);qs="maplim";q=-1;R;}
  R;
}

Z MFInfo *findMFInfoStruct(A aobj)
{
  if(aplus_nl==aobj||0==MappedFileArray)R 0;
  DO(maxMFAindex,if(MappedFileArray[i].a==(I)aobj)R MappedFileArray+i);
  R 0;
}

Z MFInfo *getFreeMFInfoStruct(void)
{
  MFInfo *p;
  if (0==MappedFileArray&&0!=maxMFAlimit) MFArealloc(maxMFAlimit);
  if(freeMFAindex>=maxMFAlimit)R 0;
  p=MappedFileArray+freeMFAindex;
  if(freeMFAindex>=maxMFAindex)maxMFAindex=freeMFAindex+1;
  freeMFAindex=p->n;
  R p;
}

Z void unmapDotMFile(A a,MFInfo *p)
{
  /* printf("In unmapDotMFile(%s, %lx, %ld)\n", p->t, a, p->n) ; */
  if(dbg_tb) beamtrc(p->t,2,0);
  if(munmap((void*)a,(size_t)p->n)) 
  {
    I nern=errno;
#ifndef __ia64
    syslog(LOG_INFO,"A+ munmap() failed for %s with args (%#lx,%ld) with %m",
	   p->t,a,p->n,nern);
#endif
    H("\343 A+ munmap() failed for %s with args (%#lx,%ld) and errno=%ld\n",
      p->t,a,p->n,nern);
  }
  p->a=0;
  bfree(p->s);
  bfree(p->t);
  p->s=p->t=(C *)0;
  p->n=freeMFAindex;
  freeMFAindex=(p-MappedFileArray);
  if(p==MappedFileArray+(maxMFAindex-1))--maxMFAindex;
}

Z I mapDotMFile(int fd,int mode,C *fname,C *t)
{
  A aobj;
  I mfile; int rc;
  MFInfo *p;
  off_t n;
  n=lseek(fd,0,SEEK_END);
  mfile=map(fd,mode);		/* map returns a 0 on failure    */
  Q(!mfile,9);			/* Set domain error if map fails */
  /* printf("mfile = %ld, n = %ld\n", mfile, n) ; */
  aobj=vetteMappedFile(n, mfile, mode, fname, &rc,
		       mode==BEAM_RW && autoBeamConvert);
  if(!aobj || rc==-1) 
    {
      if(dbg_tb) beamtrc(t,2,0);
      munmap((caddr_t)mfile,n);
      R q=9,0;
    }

  if(rc==1 && mode==BEAM_RW && autoBeamConvert==0) 
    {
      if(dbg_tb) beamtrc(t,2,0);
      munmap((caddr_t)mfile,n);
      dc(aobj);
      printf("\343 Error: read/write, but conversion to local variable\n");
      R q=9,0;
    }

  /* The following if clause will convert the .m file to the native format */
  /* for 32 bit to 64 bit otherwise it is done in place */

  if(rc==1 && mode==BEAM_RW && autoBeamConvert==1) /* Convert .m file */
    {
      struct a a;
      I itemCount=aobj->i;	/* original item count */

      if(dbg_tb) beamtrc(t,3,0); /* 3==Converting */

      if(dbg_tb) beamtrc(t,2,0);
      munmap((caddr_t)mfile,n);	/* unmap previous */
      mapOut(t, aobj);		/* write new .m with converted data */
      dc(aobj);			/* free a object */
                                /* reset the items to match orignal*/
      {
	A mFile=gsv(0,t);
	I rc=items(itemCount, mFile);
	if(dbg_tb) printf("\343 Setting Items to %ld\n",itemCount);
	dc(mFile);
      }

				/* re-map it */
      ERR(t, fd=loopen(t,BEAM_RW==mode?O_RDWR:O_RDONLY,0666));
      n=lseek(fd,0,SEEK_END);
      mfile=map(fd,mode);	/* map returns a 0 on failure    */
      Q(!mfile,9);		/* Set domain error if map fails */

      /* Call  vetteMappedFile with autoBeamConvert off */
      aobj=vetteMappedFile(n, mfile, mode, fname, &rc, 0);

      if(!aobj || rc!=0)
	{
	  if(dbg_tb) beamtrc(t,2,0);
	  munmap((caddr_t)mfile,n);
	  R q=9,0;
	}
    }

  if(rc==1)			/* Conversion to regular variable*/
    {
      if(dbg_tb) beamtrc(t,2,0);
      munmap((caddr_t)mfile,n);	/* unmap previous */
    }
  else
    {
      p=getFreeMFInfoStruct();
      if(!p)
	{
	  R H("maplim\n"),dc(aobj),q=9,0;
	}
      p->c=1,p->n=n,p->w=mode,p->s=bstring(fname),p->t=bstring(t),p->a=mfile;
    }
  R (I)aobj;
}

I isWritableFile(I a){MFInfo *p=findMFInfoStruct((A)a);R p?p->w:0;}
I im(I a){MFInfo *p=findMFInfoStruct((A)a);if(p)++p->c;R a;}
void dm(A a)
{
  MFInfo *p=findMFInfoStruct(a);
  if(p){
    if(!--p->c){
      unmapDotMFile(a,p);
    } 
  } else if(0<a->c) if (!--a->c)dec(a);
}

I mf_length(A aobj){MFInfo *p=findMFInfoStruct(aobj);R p?p->n:0;}

I mf_info(A aobj,I *pw,C **pt)
{
  MFInfo *p=findMFInfoStruct(aobj);
  if(p){*pw=p->w;*pt=p->t;R 0;}
  R 1;
}

Z void dbg_mfapp(MFInfo *p)
{H(" [%s]  refcnt:%ld\n",p->t,p->c);}

void dbg_mfa(void)
{
  I i;
  H("\343 maplim:%ld  maxIndex:%ld  freeIndex:%ld\n",maxMFAlimit,maxMFAindex,
    freeMFAindex);
  if(0==MappedFileArray){H("\343 Mapped File Array not initialized.\n");R;}
  DO(maxMFAindex,H("\343  %ld: ",i);
     if(MappedFileArray[i].a)dbg_mfapp(MappedFileArray+i);
     else H("<free>  next:%ld\n",MappedFileArray[i].n));
  H("\343  --  -----------------\n");
  for(i=maxMFAindex;i<maxMFAindex+10;i++)
  {
    if(i>=maxMFAlimit)break;
    H("\343  %ld: ",i);
    if(MappedFileArray[i].a)dbg_mfapp(MappedFileArray+i);
    else H("<free>  next:%ld\n",MappedFileArray[i].n);
  }
}

Z void dbg_mfrpp(MFInfo *p)
{H("\343 %ld\340\"%s\": [%s]  addr:%lu  refcnt:%ld  bytes:%ld\n",
		      p->w,p->s,p->t,p->a,p->c,p->n);}
void dbg_mfr(void)
{
  if(0==MappedFileArray)R;
  DO(maxMFAindex,if(MappedFileArray[i].a)dbg_mfrpp(MappedFileArray+i));
}

A dbg_mfrsf(void)
{
  I n=0;
  A z,modes,args,fnames,addrs,refcnts,bytes;
  MFInfo *p;
  z=gv(Et,2);
  z->p[0]=(I)gvi(Et,6,MS(si("mode")),MS(si("arg")),MS(si("fname")),
		 MS(si("addr")),MS(si("refcnt")),MS(si("bytes")));
  if(MappedFileArray){DO(maxMFAindex,if(MappedFileArray[i].a)++n);}
  modes=gv(It,n);
  args=gv(Et,n);
  fnames=gv(Et,n);
  addrs=gv(It,n);
  refcnts=gv(It,n);
  bytes=gv(It,n);
  n=0;
  if(MappedFileArray)
  {
    DO(maxMFAindex,
       if(MappedFileArray[i].a){p=MappedFileArray+i;
				modes->p[n]=p->w;
				args->p[n]=(I)gsv(0,p->s);
				fnames->p[n]=(I)gsv(0,p->t);
				addrs->p[n]=p->a;
				refcnts->p[n]=p->c;
				bytes->p[n]=p->n;
				n++;});
  }
  z->p[1]=(I)gvi(Et,6,modes,args,fnames,addrs,refcnts,bytes);
  R z;
} 

I mapIn(C *name,I mode){
  int fd=-1; I z;
  int tmode = mode ;		/* Valid modes 0, 1, or 2  */
  C*t=0;
  /* printf("In mapIn(%s, %d)\n", name, mode) ;*/
  Q(!name||BEAM_RO>mode||BEAM_LOCAL<mode,9);
#if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__sparcv9) || defined(_ia64)
  /* First look for a 64 bit file */
  t = findMapped64FileName(name, (BEAM_RW==tmode)) ;
  if (!t)
    t = findMapped32FileName(name, (BEAM_RW==tmode)) ;
#else
  t = findMapped32FileName(name, (BEAM_RW==tmode)) ;
#endif
  Q(!t,9);
  if(dbg_tb)beamtrc(t,1,tmode);
  ERR(t,fd=loopen(t,BEAM_RW==tmode?O_RDWR:O_RDONLY,0666));
  z=mapDotMFile(fd, tmode, name, t) ;
  if(z && dbg_tnan )nanbeamchk(t,(A)z);
  R z;
}

Z ssize_t writeAobjToFile(int fd, C *s, size_t n, I c)
{
  ssize_t t; off_t lt; I newItems;
  A a=(A) s; 
  /* printf("in writeAobj - n = %u\n", n) ; */
  do t=write(fd,s,n); while(s+=t,t!=-1&&(n-=t));
  if((!c) && t!=-1 && a->i!=(newItems=a->r ? a->d[0] : a->n)) 
  { 
    /* if beaming out a mapped variable, writeover a->i with a->d[0]  */
    lt=lseek(fd,(AH-sizeof(I)),SEEK_SET);
    if(lt!=-1) t=write(fd,(C *)&newItems,sizeof(I));
  }
  fsync(fd);R t;
} /* IBM write fix */

Z I mapOut(C *s,A z)
{
  int fd; I rc,c=0;
  C r[MAXPATHLEN];
  C *dotPos, *slashPos;
/* #if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__sparcv9) || defined(_ia64) */
/*   static C defaultSuffix[]={"m64"} ; */
/* #else */
/*   static C defaultSuffix[]={"m"}; */
/* #endif */

  static C defaultSuffix[]={"m"};

  Q(Ct<z->t,6);

/* On solaris this comparison was failing when '.' was zero and the address */
/* returned from strrchr was a negative signed long */
/* This was caused by missing #include string.h */

/*   s=strrchr((DEV_STRARG)s,'.')>strrchr((DEV_STRARG)s,'/') ?  */
/*     s : findFileName(s,"m"); */

  dotPos=strrchr((DEV_STRARG)s,'.');
  if( dotPos==NULL )
    {
      s=findFileName(s,defaultSuffix);
    }
  else
    {
      slashPos=strrchr((DEV_STRARG)s,'/');
      if(slashPos!=NULL && dotPos<slashPos)
	s=findFileName(s,defaultSuffix);
    }

  if(dbg_tb)beamtrc(s,0,0);
  strcpy(r,(DEV_STRARG)s),strcat(r,(DEV_STRARG)"!@#");
  ERR(s,fd=loopen(r,O_CREAT|O_WRONLY,0666));
  if(z->c)c=z->c,z->c=0,z->i=z->r?*z->d:1;
  rc=writeAobjToFile(fd,(C *)z,AH+Tt(z->t,z->n)+(Ct==z->t?1:0),c);
  if(c)z->c=c;
  fsync(fd);
  close(fd);
  ERR(s,rc);
  ERR(s,rename(r,s));
  R 1;
}

C *stringFromAobj(A a){R a->t==Ct?(C*)a->p:a->t==Et&&QS(*a->p)?XS(*a->p)->n:0;}

H1(monadicBeam){ND1 R mapIn(stringFromAobj(a),0);}
H2(dyadicBeam){
  C *s;
  ND2;
  s=stringFromAobj(a);
  R !s?mapIn(stringFromAobj(w),*a->p):mapOut(s,w)?(I)aplus_nl:0;
}

Z I setSizeOfFile(int fd,off_t n)
{
#ifdef __VISUAL_C_2_0__
  I j=CLBYTES,k=lseek(fd,0,SEEK_END);
#else
  size_t j=getpagesize(),k=lseek(fd,0,SEEK_END);
#endif
  C junk[4];
  /* printf("In setSizeOfFile; n = %ld, j = %ld, k = %ld\n",
		n, j, k) ;*/
  junk[0]='\0';
  if(-1==k)
    R k;
  n=((n+j-1)/j)*j;
  for(;n<k;n+=j)
  {
    if(-1==lseek(fd,n,SEEK_SET))
      R -1;
    if(-1==write(fd,junk,1))
      R -1;
  }
  R 0;
}


Z I items(I n,A z)
{
  C *s;Z struct a a;int fd; I t,m,j,w=n!=-1;
  C *name ;
  Q(-2>n,9);
  NDC1(z);s=stringFromAobj(z);Q(!s,9);

#if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__sparcv9) || defined(__ia64)
  name = findMapped64FileName(s, w) ;
  if (!name)
	name = findMapped32FileName(s, w) ;
#else
  name = findMapped32FileName(s, w) ;
#endif

  ERR(s, fd=loopen(name,w?O_RDWR:O_RDONLY,0666));
  if(-1==read(fd,(C *)&a,AH)) 
    return perror(s),close(fd),q=9,0;

  {
    int ret; I totsize,rank,itemCount,items;
    struct stat statbuf;
    if ( -1==fstat(fd, &statbuf) ) /* To get size for getItems() */
      return perror(s), close(fd), q=9, 0;
    
    ret=getItems(&a, &itemCount, &rank, &items, statbuf.st_size);
    
    if( ret==-1 || rank==0 )	/* error conversion or scalar */
      {
	printf("\343 Error: %s [%s]\n", 
	       (ret==-1) ? "Conversion ":"Scalar ",name);
	return close(fd), q=(ret==-1)?9:7, 0;
      }

    if( n==-1 )			/* Query */
      return close(fd), itemCount>items?itemCount:items;

    if( ret==1 && !autoBeamConvert ) /* conversion required */
      {
	printf("\343 Error: requires conversion [%s]\n",name);
	return close(fd), q=9, 0;
      }

    /* try to set the items */
    if( ret==1 )		/* convert before setting items */
      {
	A aobj=(A)mapDotMFile(fd, BEAM_RW, s, name);
	if(aobj==0) 
	  return 0;
	else
	  dc(aobj);
#if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__sparcv9) || defined(__ia64)
	name = findMapped64FileName(s, w) ;
	if (!name)
	  name = findMapped32FileName(s, w) ;
#endif
				/* re-open converted file */
	if(-1==(fd=loopen(name,w?O_RDWR:O_RDONLY,0666))) 
	  return perror(s),q=9,0;
	if(-1==read(fd,(C *)&a,AH)) 
	  return perror(s),close(fd),q=9,0;
      }

    m=*a.d;			/* Set the items */
    j=a.i;
    if(m>j)j=m;
    
    if(w){
      t=a.t;
      if(n==-2)
	{
	  if(-1==setSizeOfFile(fd,AH+Tt(t,a.n)+(t==Ct)))
	    R perror(s),close(fd),q=9,0;
	}
      else
	{
	  a.i=n;
	  m=n*tr(a.r-1,a.d+1);
	  if(n<*a.d)	/* reset d[0] if greater than n */
	    *a.d=n,a.n=m;
	  if(-1==flen(fd,AH+Tt(t,m)+(t==Ct))) 
	    R perror(s),close(fd),q=9,0;
	  if(-1==lseek(fd,0,0))  
	    R perror(s),close(fd),q=9,0;
	  if(-1==write(fd,(C *)&a,AH))
	    R perror(s),close(fd),q=9,0;
	}
    }
    R close(fd),j;
  }
}

static long ep_isaDotM(char *fname)
{
  long rc=0;                    /* 0 == invalid dot M file */
  A aobj;
  off_t mlen;
  int fd;

  if(-1==(fd=open(fname,O_RDONLY)))
    {
      perror(fname);
      return rc;
    }

  mlen=lseek(fd,0,2);
  if(0==(aobj=(A)map(fd,0)))    /* map closes fd */
    {
      perror(fname);
      return rc;
    }

  if ( isAObject(*aobj) )
    {
      int dimsOK=1;
      /* Check dimensions */
      checkDims(*aobj);

      /* Check file length */
#if (HAS_64BIT_TYPE==1)
      if (dimsOK && checkFileSize64(*aobj) <= mlen)
        rc=1;
#else
      if (dimsOK && checkFileSize32(*aobj) <= mlen)
        rc=1;
#endif
    }

  munmap(aobj,mlen);
  return rc;
}

static void ep_msyncAll(A msyncMode_)
{
  int msyncFlag=0;
  long i;

  if(msyncMode_->t!=Et || !QS(*msyncMode_->p) )
    {
      q=ERR_DOMAIN;
      R;
    }

  for(i=0; i<msyncMode_->n; i++)
    {
      if(msyncMode_->p[i]==MS(si("MS_ASYNC")))
	{
          if(msyncFlag&MS_SYNC)
	    {
	      q=ERR_DOMAIN;
	      return;
	    }
	  else
	    {
	      msyncFlag|=MS_ASYNC;
	    }
	}
      else if(msyncMode_->p[i]==MS(si("MS_SYNC")))
	{
          if(msyncFlag&MS_ASYNC)
	    {
	      q=ERR_DOMAIN;
	      return;
	    }
	  else
	    {
	      msyncFlag|=MS_SYNC;
	    }
	}
      else if(msyncMode_->p[i]==MS(si("MS_INVALIDATE")))
	{
	  msyncFlag|=MS_INVALIDATE;
	}
      else
	{
	  q=ERR_DOMAIN;
	  return;
	}
    }

  for(i=0; i<maxMFAindex; i++)
    {
      MFInfo *p=MappedFileArray+i;
      if(p->a  && p->w==1 )
	{
	  A a=(A)p->a;
	  if(-1==msync((C*)a, AH+Tt(a->t,a->n), msyncFlag))
	    {
	      printf("\343 Error: %s\n",p->t); 
	      perror("ep_amsyncAll: msync");
	    }
	}
    }
} 


void beamInstall(void){
  install((PFI)items,        "_items",     9,2,9,0,0,0,0,0,0,0);
  install((PFI)ep_hostEndian,"_hostendian",0,0,0,0,0,0,0,0,0,0);
  install((PFI)ep_aobjEndian,"_endian",    0,1,0,0,0,0,0,0,0,0);
  install((PFI)ep_isaDotM,   "_isaDotM",   9,1,7,0,0,0,0,0,0,0);
  install((PFI)ep_msyncAll,  "_msyncAll",  8,1,0,0,0,0,0,0,0,0);
  R;
}

