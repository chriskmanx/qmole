/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/development.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/mman.h>

#include <dap/balloc.h>
#include <dap/buff.h>
#include <a/k.h>
#include <a/fncdcls.h>
#include <a/fir.h>
#include <a/arthur.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

extern CX cxlu();
extern V vlu();
extern S symsplit(),symjoin();
extern I dbg_tpack,dbg_tdef;

static I iced();
static I icedAtom();
static I thaw();


#define PKARG_ERROR   0
#define PKARG_NULL    1
#define PKARG_SYMVEC  2
#define PKARG_SLOT    3
#define PKARG_SYMDEST 4

#define PK_COOKIE "PKGmcatn"
#define PK_VERSION 1

#define PK_ZERO  0
#define PK_NULL  1
#define PK_SYM   2
#define PK_CX    3
#define PK_VREF  4
#define PK_AVAR  5
#define PK_AET   6
#define PK_AFUNC 7
#define PK_ABEAM 8
#define PK_STR   9
#define PK_PRIM  10
#define PK_KWORD 11
#define PK_XFUNC 12
#define PK_EXPR  13
#define PK_LOCAL 14
#define PK_ADF   15
#define PKE_VDAT  16
#define PKE_VDEP  17

#define PK_NTYPES 18

#define DH if(Opts.debug)H

#define PK_ICABLE(t)  (PK_AVAR==(t)||PK_AET==(t)||PK_AFUNC==(t)|| \
		       PK_ABEAM==(t)||PK_ADF==(t))

#define PK_CODEDPTR(t) (PK_SYM==(t)||PK_VREF==(t)||PK_EXPR==(t)|| \
			PKE_VDAT==(t)|| PKE_VDEP==(t))

#define PK_CODEDIDX(t) (PK_KWORD==(t)||PK_LOCAL==(t)||PK_PRIM==(t)|| \
			PK_XFUNC==(t))

#define PK_HDRSIZE 32
#define PK_HTSIZE 1024

#define EHT_HTSIZE (I)128

#define EHT_NONE    (I)0
#define EHT_DYNAMIC (I)1
#define EHT_ICE     (I)2
#define EHT_RUNTIME (I)3

/**/
/* Static Variables and typedefs */

static C *ZpkTypeName[]= {
  "Zero   ","Null   ","Symbol ","Context","V-Ref  ",
  "A:Data ","A:Nestd","A:Func ","A:Beam ","String ",
  "Primtv ","Keyword","XFunc  ","Expr   ","Local  ",
  "A:DFunc","Sym->Cx","*V-Data","*V-Dep ",
  "TOTAL: "};

#ifdef FUNCNOTUSED
static C *ZpkAttList[]= { "ls", "_opts" };
#endif

#define BUSTED(n) {Busted=(n);R 0;}

static int Busted=0;

typedef struct str_IdEntry{I p,type;long ftell,len;}IdEntry;
typedef IdEntry *IDE;

typedef struct str_EgoEntry{I scx,vname,glob,cvname;}EgoEntry;
typedef EgoEntry *EGOE;

static IdEntry TempId;
static EgoEntry TempEgo;

typedef struct str_blockInfo{long ftell,len;}BlockInfo;

static struct str_EgoHashTable{
  I nHash;
  I nBuckets;
  I *hashArray;
  I *bucketArray;
  struct buff *bucketBuff;
  I source;
} Eht = {0,0,0,0,0,EHT_NONE};
			      

static struct str_ZpkHeader{
  C cookie[8];
  I version;
  long hdrsize;   /* in words */
  BlockInfo File; /* File.len is in bytes. */
  BlockInfo Ego;  /* len for Ego and Id in # entries */
  BlockInfo Id;
  BlockInfo Ice;
  BlockInfo EgoHash;
  I reserved[PK_HDRSIZE-14]; /* adjust to keep header 32 words long */
} ZpkHeader;

static struct str_Zpack{
  IDE Id;
  I nId;
  EGOE Ego;
  I nEgo;
  struct buff *IdBuff;
  struct buff *EgoBuff;
  struct buff *ResBuff;
  C *ice;
  long flen;
  FILE *fp;
  HT IdHashTable;
} Zpack;

static struct str_ZpkStat{
  I refs;
  I objs;
  I size;
} ZpkStats[1+PK_NTYPES];

static struct str_Opts {
  I pieces;
  I verbose;
  I debug;
  I result;
  I stats;
  I hash;
} Opts = {0,0,0,0,0};

static A DefaultAOpts = 0;

SUBROUTINE
void
setOpts(opts)C *opts;
{
  Opts.pieces=(NULL==strchr((DEV_STRARG)opts,'p'))?0:1;
  Opts.verbose=(NULL==strchr((DEV_STRARG)opts,'v'))?0:1;
  Opts.debug=(NULL==strchr((DEV_STRARG)opts,'d'))?0:1;
  Opts.result=(NULL==strchr((DEV_STRARG)opts,'r'))?0:1;
  Opts.stats=(NULL==strchr((DEV_STRARG)opts,'s'))?0:1;
  Opts.hash=(NULL==strchr((DEV_STRARG)opts,'h'))?0:1;
}

/**/
/****************************************************************
 *
 * Modified hash functions -- variation of att.c with double key.
 *
 */

typedef struct str_zhtn{I idx,obj,type;struct str_zhtn *n;}*ZHTN;

SUBROUTINE
ZHTN hashget(ht,obj,type)HT ht;I obj,type;
{
  ZHTN *zhtn=(ZHTN*)HTHASH(ht,obj^type),n;
  for(n=*zhtn;n;n=n->n)if(obj==n->obj&&type==n->type)R n;
  R 0;
}

SUBROUTINE
I hashset(ht,obj,type,idx)HT ht;I obj,type,idx;
{
  ZHTN *zhtn=(ZHTN*)HTHASH(ht,obj^type),n,hd;
  for(n=*zhtn;n;n=n->n)if(obj==n->obj&&type==n->type){n->idx=idx;R 0;}
  n=(ZHTN)ma(4);n->obj=obj;n->type=type;n->idx=idx;
  if(hd=*zhtn){n->n=hd->n;hd->n=n;}else{*zhtn=n;n->n=0;}++ht->ni;R 1;
}

SUBROUTINE
I hashfree(ht)HT ht;
{
  ZHTN *zhtn,n,t;
  I i;
  for(i=0;i<ht->nb;++i) 
    if(*(zhtn=(ZHTN *)(ht->b+i)))
  {
    for(n=*zhtn;t=n->n;){n->n=t->n;mf((I *)t);}
    mf((I *)n);
    *zhtn=0;
  }
  bfree((char *)ht);
  R 0;
}

/**/
/****************************************************************
 *
 * Ego Hash Functions - these are very different from att.c because
 * the buckets are in a fixed array.
 */

/* egoha(0 is a copy of ha() from att.c.  It is copied here for
 * isolation from future changes in att.c.
 */
Z egoha(n)
C *n;
{
	unsigned long h = 0, c;

	while ((c = (unsigned long)*n++) != 0)
	{
		h += (h << 5) + c;
	}
	R h;
}


#define EGOHASH(x) ((egoha(x))&(EHT_HTSIZE-1))

SUBROUTINE
void EgoHashInitDynamic(nHA)I nHA;
{
  Eht.hashArray=ma(nHA);
  Eht.nHash=nHA;
  Eht.bucketBuff=buffalloc();
  Eht.nBuckets=0;
  Eht.bucketArray=0;
  bzero(Eht.hashArray,nHA*sizeof(I));
  Eht.source=EHT_DYNAMIC;
}

SUBROUTINE
void EgoHashInitIce()
{
  I *ehtBlock;
  struct buff *pb;
  if(ZpkHeader.EgoHash.ftell)
  {
    ehtBlock=(I*)(Zpack.ice+ZpkHeader.EgoHash.ftell);
    Eht.nHash=ehtBlock[0];
    Eht.nBuckets=ehtBlock[1];
    Eht.bucketBuff=pb=buffalloc();
    Eht.bucketArray=ehtBlock+2;
    pb->min=pb->get=(C*)(Eht.bucketArray);
    Eht.hashArray=Eht.bucketArray+(2*Eht.nBuckets);
    pb->max=pb->put=(C*)Eht.hashArray;
    Eht.source=EHT_ICE;
  }
  else Eht.source=EHT_NONE;
}

SUBROUTINE
void EgoHashCleanup()
{
  if(EHT_DYNAMIC==Eht.source||EHT_RUNTIME==Eht.source)
  {
    if (Eht.hashArray) mf(Eht.hashArray);
    if (Eht.bucketBuff) bufffree(Eht.bucketBuff);
  }
  else if (EHT_ICE==Eht.source)
  {
    if (Eht.bucketBuff) bfree((char *)Eht.bucketBuff);
    /* Note: do not call bufffree() here, as the buffer is from the ice. */
  }
  Eht.hashArray=0;
  Eht.bucketBuff=0;
  Eht.nHash=Eht.nBuckets=0;
}

SUBROUTINE
void EgoHashAdd(cvname,iego)S cvname;I iego;
{
  I haidx;
  DH("\343 z:EgoHashAdd(cvname:%ld [%s],iego:%ld)\n",cvname,cvname->n,iego);
  if(EHT_DYNAMIC==Eht.source||EHT_RUNTIME==Eht.source)
  {
    haidx=EGOHASH(cvname->n);
    buffstuff(Eht.bucketBuff,(char *)(Eht.hashArray+haidx),sizeof(I));
    buffstuff(Eht.bucketBuff,(char *)&iego,sizeof(iego));
    Eht.hashArray[haidx]=1+2*Eht.nBuckets++;
  }
  DH("\343 z:EgoHashAdd finished\n");
}

static S Sthaw();
SUBROUTINE
void EgoHashBuildIfNeeded(type)I type;
{
  I iego;
  DH("\343 z:EgoHashBuildIfNeeded(type:%ld)\n",type);
  if(EHT_NONE==Eht.source)
  {
    if(Opts.verbose)H("\343 ZPK: No hash table on file.  Building it.\n");
    EgoHashInitDynamic(EHT_HTSIZE);
    for(iego=0;iego<Zpack.nEgo;++iego)
      EgoHashAdd(Sthaw(Zpack.Ego[iego].cvname),iego);
    Eht.bucketArray=(I*)Eht.bucketBuff->min;
  }
  Eht.source=type;
  DH("\343 z:EgoHashBuildIfNeeded finished\n");
}

SUBROUTINE
I EgoHashLookup(cvname)S cvname;
{
  I haidx, idx, iego;
  DH("\343 z:EgoHashLookup(cvname:%ld [%s])\n",cvname,cvname->n);
  haidx=EGOHASH(cvname->n);
  if(EHT_DYNAMIC==Eht.source)
  {
    Eht.bucketArray=(I*)Eht.bucketBuff->min;
    Zpack.Ego=(EGOE)Zpack.EgoBuff->min;
    Zpack.Id=(IDE)Zpack.IdBuff->min;
  }
  for(idx=Eht.hashArray[haidx];idx;idx=Eht.bucketArray[idx-1])
  {
    iego=Eht.bucketArray[idx];
    if(cvname==Sthaw(Zpack.Ego[iego].cvname))R iego;
  }
  R -1;
}

/**/
/****************************************************************
 *
 * Other utility functions
 *
 */


SUBROUTINE
I AddIdEntry(tid)IDE tid;
{
  buffstuff(Zpack.IdBuff,(char *)tid,sizeof(IdEntry));
  hashset(Zpack.IdHashTable,tid->p,tid->type,Zpack.nId);
  R Zpack.nId++;
}

SUBROUTINE
I AddEgoEntry(cvname,tego)S cvname;EGOE tego;
{
  I iego;
  iego=EgoHashLookup(cvname);
  if(-1==iego){
    buffstuff(Zpack.EgoBuff,(char *)tego,sizeof(EgoEntry));
    EgoHashAdd(cvname,Zpack.nEgo);
    R Zpack.nEgo++;
  }
  else
  {
    ((EGOE)(Zpack.EgoBuff->min))[iego].glob=tego->glob;
    R iego;
  }
}

SUBROUTINE
void clearStats()
{
  I i;
  for(i=0;i<1+PK_NTYPES;++i)
    ZpkStats[i].refs=ZpkStats[i].objs=ZpkStats[i].size=0;
}

SUBROUTINE
I zargvette(alist)A alist;
{
  A a0,a1;
  if (qz(alist))R PKARG_NULL;
  else if(sym(alist))R PKARG_SYMVEC;
  else if(issfdups(alist))R PKARG_SLOT;
  else if(alist&&QA(alist)&&Et==alist->t&&1==alist->r&&2==alist->n&&
	  sym(a0=(A)alist->p[0])&&sym(a1=(A)alist->p[1])&&a0->n==a1->n)
    R PKARG_SYMDEST;
  R PKARG_ERROR;
}

extern C *stringFromAobj();
SUBROUTINE
C *MakeFileName(afn)A afn;
{
  Z C fnamebuf[MAXPATHLEN];
  C *rootname=stringFromAobj(afn);
  if(rootname==(C*)0)R(C*)0;
  if(strrchr((DEV_STRARG)rootname,'.')>strrchr((DEV_STRARG)rootname,'/'))R rootname;
  strcpy((DEV_STRARG)fnamebuf,(DEV_STRARG)rootname);
  strcat((DEV_STRARG)fnamebuf,(DEV_STRARG)".pkg");
  R fnamebuf;
}
    
SUBROUTINE
C *getfilename(afn)A afn;
{
  C *res=0;
  A p1=0;
  I haveOpts=0;
  if(Ct==afn->t) res=MakeFileName(afn);
  else if(Et!=afn->t) res=(C*)0;
  else if (1<=afn->n&&QS(afn->p[0])) res=MakeFileName(afn);
  else if (2==afn->n&&QA(afn->p[0])&&QA(afn->p[1]))
  {
    p1=(A)afn->p[1];
    res=MakeFileName((A)afn->p[0]);
    if(res!=(C*)0)
    {
      if(Ct==p1->t) haveOpts=1;
      else res=(C*)0;
    }
  }
  if(res!=(C*)0)
    setOpts(haveOpts?(C*)p1->p:DefaultAOpts?(C*)DefaultAOpts->p:"");
  R res;
}

SUBROUTINE
A getapackstr(astr)A astr;
{
  A p1=0,ares=(A)0;
  I haveOpts=0;

  if(Ct==astr->t)ares=astr;
  else if (Et==astr->t&&2==astr->n&&QA(astr->p[0])&&QA(astr->p[1]))
  {
    ares=(A)astr->p[0];
    p1=(A)astr->p[1];
    if(Ct==p1->t)haveOpts=1;
  }
  if(ares&&QA(ares)&&Ct==ares->t&&1==ares->r&&(PK_HDRSIZE*sizeof(I))<=ares->n)
  {
    setOpts(haveOpts?(C*)p1->p:DefaultAOpts?(C*)DefaultAOpts->p:"");
    R(A)ares;
  }
  R(A)0;
}
  
	
/**/
/****************************************************************
 *
 * Icer functions
 *
 */

static long IcePad[2]={0,0};

SUBROUTINE
void iceWrite(C *ptr,I sz,I n)
{
  if (Zpack.fp) fwrite(ptr,sz,n,Zpack.fp);
  else buffstuff(Zpack.ResBuff,ptr,sz*n);
}

SUBROUTINE
long iceTell()
{
  if (Zpack.fp) R ftell(Zpack.fp);
  else R Zpack.ResBuff->put-Zpack.ResBuff->get;
}

SUBROUTINE
void writeIce(C *ptr,I sz,I n)
{
  long cpos;
  iceWrite(ptr,sz,n);
  cpos=8L-iceTell()%8L;
  if(8>cpos)iceWrite((C *)IcePad,1,cpos);
}

SUBROUTINE
long packInIceString(plen,str)long *plen;C *str;
{
  int slen=strlen((DEV_STRARG)str);
  long tell=iceTell();
  writeIce(str,1,1+slen);
  *plen=(iceTell()-tell)/(long)sizeof(I);
  R tell;
}

SUBROUTINE
long packInIceArray(long *plen,C *ptr,I sz,I n)
{
  long tell=iceTell();
  writeIce(ptr,sz,n);
  *plen=(iceTell()-tell)/sizeof(I);
  R tell;
}

SUBROUTINE
void icerZero(tid,obj)IDE tid;I obj;
{
  tid->type=PK_ZERO;tid->p=0;
  tid->ftell=tid->len=0L;
}

SUBROUTINE
void icerNull(tid,obj)IDE tid;I obj;
{
  tid->type=PK_NULL;tid->p=(I)aplus_nl;
  tid->ftell=tid->len=0L;
}

SUBROUTINE
void icerS(tid,s)IDE tid;S s;
{
  tid->type=PK_SYM; tid->p=(I)s;
  tid->ftell=packInIceString(&tid->len,s->n);
}

SUBROUTINE
void icerCx(tid,cxt)IDE tid;CX cxt;
{
  I symiid=iced((I)cxt->s,(I)PK_SYM);
  tid->type=PK_CX; tid->p=(I)cxt;
  tid->ftell=symiid;tid->len=0L;
}

SUBROUTINE
void icerVref(tid,v)IDE tid;V v;
{
  long len=2;
  I *iceCube=ma(len);

  iceCube[0]=iced((I)v->s,(I)PK_SYM);
  iceCube[1]=iced((I)v->cx,(I)PK_CX);

  tid->type=PK_VREF;tid->p=(I)v;
  tid->ftell=packInIceArray(&tid->len,(C *)iceCube,sizeof(I),len);
  mf(iceCube);
} 

SUBROUTINE
void icerAvar(tid,aobj)IDE tid;A aobj;
{
  tid->ftell=iceTell();tid->len=0;
  iceWrite((C *)&aobj->t,sizeof(I),1);
  iceWrite((C *)&aobj->r,sizeof(I),1);
  iceWrite((C *)&aobj->n,sizeof(I),1);
  iceWrite((C *)aobj->d,sizeof(I),MAXR);
  tid->type=PK_AVAR;tid->p=(I)aobj;
  switch(aobj->t)
  {
  case It:
    packInIceArray(&tid->len,(C *)aobj->p,sizeof(I),aobj->n);
    break;
  case Ft:
    packInIceArray(&tid->len,(C *)aobj->p,sizeof(F),aobj->n);
    break;
  case Ct:
    packInIceArray(&tid->len,(C *)aobj->p,sizeof(C),aobj->n+1);
    break;
  default:
    H("\343 PKG: icerAvar: non-simple A object\n");
    R;
  }
  tid->len+=12;
}

SUBROUTINE
void icerAet(tid,aobj)IDE tid;A aobj;
{
  long len=3+MAXR+aobj->n;
  I *iceCube=ma(len);
  I idx=0,i;

  iceCube[idx++]=aobj->t;
  iceCube[idx++]=aobj->r;
  iceCube[idx++]=aobj->n;
  for(i=0;i<MAXR;++i)iceCube[idx++]=aobj->d[i];
  for(i=0;i<aobj->n;++i)iceCube[idx++]=icedAtom(aobj->p[i]);
  tid->type=PK_AET;tid->p=(I)aobj;
  tid->ftell=packInIceArray(&tid->len,(C *)iceCube,sizeof(I),len);
  mf(iceCube);
}

SUBROUTINE
void icerAfunc(tid,aobj)IDE tid;A aobj;
{
  long len=3+(MAXR+1)+(aobj->n)+2;
  I *iceCube=ma(len);
  I i,idx=0;

  for(i=0;i<len;++i)iceCube[i]=0;
  iceCube[idx++]=aobj->t;
  iceCube[idx++]=aobj->n;
  iceCube[idx++]=aobj->r;
  /* the Xt+2 kludge below is for monadic operators, where a->d[2] is junk */
  /* more kludge: d[3] is junk for dyadic ops where 5==aobj->r*/
  for(i=0;i<aobj->r;++i)
    iceCube[idx+i]=
      (2==i&&(Xt+2)==aobj->t||3==i&&5==aobj->r&&(Xt+3==aobj->t||Xt+4==aobj->t))
	?iced((I)0,(I)PK_ZERO):icedAtom(aobj->d[i]);
  idx+=MAXR+1;
  for(i=0;i<aobj->n;++i)iceCube[idx++]=icedAtom(aobj->p[i]);
  iceCube[idx++]=iced(aobj->p[1+aobj->n],(I)PK_STR);
  iceCube[idx++]=iced(aobj->p[2+aobj->n],(I)PK_CX);
  tid->type=PK_AFUNC;tid->p=(I)aobj;
  tid->ftell=packInIceArray(&tid->len,(C *)iceCube,sizeof(I),len);
  mf(iceCube);
}

SUBROUTINE
void icerAbeam(tid,aobj)IDE tid;A aobj;
{
  C *fname;
  I beammode,len=0;

  if(mf_info(aobj,&beammode,&fname))
  {
    H("\343 PKG: icerBeam: non-beamed object.  aobj:%ld\n",aobj);Busted=1;R;
  }
  if('.'==fname[0]&&'/'==fname[1])fname+=2;  /* Remove initial ./  */
  tid->type=PK_ABEAM;tid->p=(I)aobj;
  tid->ftell=iceTell();
  iceWrite((C *)&beammode,sizeof(I),1);
  iceWrite((C *)&len,sizeof(I),1); /* this is just filler */
  packInIceString(&len,fname);
  tid->len=2+len;
}


SUBROUTINE
void icerStr(tid,str)IDE tid;C *str;
{
  tid->type=PK_STR; tid->p=(I)str;
  tid->ftell=packInIceString(&tid->len,str);
}

extern C**get_primlist();
SUBROUTINE
void icerPrim(tid,pidx)IDE tid;I pidx;
{
  C **plist=get_primlist(1,0);
  tid->type=PK_PRIM; tid->p=pidx;
  tid->ftell=packInIceString(&tid->len,plist[pidx]);
}

SUBROUTINE
void icerKword(tid,nidx)IDE tid;I nidx;
{
  C **nlist=get_primlist(1,1);
  tid->type=PK_KWORD; tid->p=nidx;
  tid->ftell=packInIceString(&tid->len,nlist[nidx]);
}

extern C*xfs_name[];
SUBROUTINE
void icerXfunc(tid,xidx)IDE tid;I xidx;
{
  tid->type=PK_XFUNC; tid->p=xidx;
  tid->ftell=packInIceString(&tid->len,xfs_name[xidx]);
}

SUBROUTINE
void icerExpr(tid,expr)IDE tid;E expr;
{
  long len=2+expr->n;
  I *iceCube=ma(len);
  I i,idx=0;
  iceCube[idx++]=expr->n;
  iceCube[idx++]=icedAtom(expr->f);
  for(i=0;i<expr->n;++i)iceCube[idx++]=icedAtom(expr->a[i]);
  tid->type=PK_EXPR;tid->p=(I)expr;
  tid->ftell=packInIceArray(&tid->len,(C *)iceCube,sizeof(I),len);
  mf(iceCube);
}

SUBROUTINE
void icerLocal(tid,lidx)IDE tid;I lidx;
{
  tid->type=PK_LOCAL; tid->p=lidx;
  tid->ftell=lidx; tid->len=0;
}

/* icerAdf is for derived functions */
SUBROUTINE
void icerAdf(tid,aobj)IDE tid;A aobj;
{
  long len=3+(MAXR+1);
  I *iceCube=ma(len);
  I i,idx=0;

  for(i=0;i<len;++i)iceCube[i]=0;
  iceCube[idx++]=aobj->t;
  iceCube[idx++]=aobj->n;
  iceCube[idx++]=aobj->r;
  for(i=0;i<aobj->r;++i)iceCube[idx+i]=icedAtom(aobj->d[i]);
  tid->type=PK_ADF;tid->p=(I)aobj;
  tid->ftell=packInIceArray(&tid->len,(C *)iceCube,sizeof(I),len);
  mf(iceCube);
}

SUBROUTINE
void icerEpVdat(tid,v)IDE tid;V v;
{
  long len=sizeof(struct _v)/sizeof(I);
  I *iceCube=ma(len);
  I idx,isdep=v->e?1:0;

  for(idx=0;idx<len;++idx)iceCube[idx]=0;
  idx=0;
  iceCube[idx++]=iced((I)v->s,(I)PK_SYM);
  iceCube[idx++]=iced((I)v->cx,(I)PK_CX);
  iceCube[idx++]=v->t;
  iceCube[idx++]=icedAtom(isdep?v->e:v->a);

  tid->type=(isdep?PKE_VDEP:PKE_VDAT);tid->p=(I)v;
  tid->ftell=packInIceArray(&tid->len,(C *)iceCube,sizeof(I),len);
  mf(iceCube);
}

static void(*Icerfunc[])()={ 
  icerZero, icerNull, icerS, icerCx, icerVref,
  icerAvar, icerAet, icerAfunc, icerAbeam, icerStr,
  icerPrim, icerKword, icerXfunc, icerExpr, icerLocal,
  icerAdf, icerEpVdat, icerEpVdat,
  };

SUBROUTINE
I icer(obj,type)I obj,type;
{
  IDE tid=&TempId;
  if(0==obj&&
     !(PK_PRIM==type||PK_KWORD==type||PK_LOCAL==type||
       PK_XFUNC==type||PK_ZERO==type))
    R iced((I)obj,(I)PK_ZERO);
  (*Icerfunc[type])(tid,obj);
  ++ZpkStats[type].refs;++ZpkStats[type].objs;ZpkStats[type].size+=tid->len;
  R AddIdEntry(tid);
}

SUBROUTINE
I iced(obj,type)I obj,type;
{
  ZHTN zhtn;
  
  /* try to find obj in Id */
  if(zhtn=hashget(Zpack.IdHashTable,obj,type)){
    ++ZpkStats[zhtn->type].refs;
    R zhtn->idx;
  }
  
  /* here new entry.  Call appropriate icer */
  if(PK_NTYPES<=type)
  {
    H("\343 PKG: iced: unknown type:%ld (ntypes:%d)\n",type,PK_NTYPES);R 0;
  }
  R icer(PK_ICABLE(type)?ic((A)obj):obj,type);
}

SUBROUTINE
I icedAtom(arg)I arg;
{
  I type=PK_ZERO,obj=0;
  A aobj;
  switch(arg&aplusMask)
  {
  case 0: /* A-object */
    obj=arg;
    aobj=(A)arg;
    type=-1;
    if(0==aobj)type=PK_ZERO;
    else if (0==aobj->c)type=(aplus_nl==aobj)?PK_NULL:mf_length(aobj)?PK_ABEAM:-1;
    if(-1==type)
      type=(Et>aobj->t)?PK_AVAR:(Et==aobj->t)?PK_AET:
      (Xt==aobj->t)?PK_ADF:PK_AFUNC;
    break;
  case 1: /* global (V) */
    obj=(I)XV(arg);type=PK_VREF;break;
  case 2: /* symbol (S) */
    obj=(I)XS(arg);type=PK_SYM;break;
  case 3: /* expression (E) */
    obj=(I)XE(arg);type=PK_EXPR;break;
  case 4: /* flow-control/operator */
    obj=U(arg);type=PK_KWORD;break;
  case 5: /* local variable */
    obj=U(arg);type=PK_LOCAL;break;
  case 6: /* primitive */
    obj=U(arg);type=PK_PRIM;break;
  case 7: /* external/system function */
    obj=U(arg);type=PK_XFUNC;break;
  }
  if(0>type)
  {
    if(-1==type)
      H("\343 PKG: icedW: non-beamed non-null 0==a->c arg:%ld\n",arg);
    else if (-2==type) 
      H("\343 PKG: icedW: not yet implemented. arg:%ld\n",arg);
    BUSTED(2);
  }
  R iced(obj,type);
}   

/**/
/****************************************************************
 *
 * Thaw functions
 *
 */

SUBROUTINE S Sthaw(iid)I iid;{R XS(thaw(iid));}

SUBROUTINE CX CXthaw(iid)I iid;{R (CX)(thaw(iid));}

#ifdef FUNCNOTUSED
SUBROUTINE E Ethaw(iid)I iid;{R XE(thaw(iid));}
#endif

SUBROUTINE void thawZero(tid)IDE tid;{tid->p=0;}

SUBROUTINE void thawNull(tid)IDE tid;{tid->p=(I)aplus_nl;}

SUBROUTINE 
void thawS(tid)IDE tid;{tid->p=MS(si(Zpack.ice+tid->ftell));}

SUBROUTINE
void thawCx(tid)IDE tid;{tid->p=(I)cxi(Sthaw(tid->ftell));}

SUBROUTINE
void thawVref(tid)IDE tid;
{
  I *ptr=(I*)(Zpack.ice+tid->ftell);
  tid->p=MV(vi(Sthaw(ptr[0]),CXthaw(ptr[1])));
}

SUBROUTINE
void thawAvar(tid)IDE tid;
{
  I *ptr=(I*)(Zpack.ice+tid->ftell);
  tid->p=(I)gc(ptr[0],ptr[1],ptr[2],ptr+3,ptr+12);
}

SUBROUTINE
void thawAEt(tid)IDE tid;
{
  I i,*ptr=(I*)(Zpack.ice+tid->ftell);
  A z=ga(ptr[0],ptr[1],ptr[2],ptr+3);
  for(i=0;i<z->n;++i) z->p[i]=thaw(ptr[12+i]);
  tid->p=(I)z;
}

SUBROUTINE
void thawAfunc(tid)IDE tid;
{
  I i,idx=0,*ptr=(I*)(Zpack.ice+tid->ftell);
  A z=gv(Et,3+ptr[1]);
  z->t=ptr[idx++];
  z->n=ptr[idx++];
  z->r=ptr[idx++];
  for(i=0;i<MAXR+1;++i)z->d[i]=thaw(ptr[idx++]);
  for(i=0;i<z->n;++i)z->p[i]=thaw(ptr[idx++]);
  z->p[z->n]=0;
  z->p[z->n+1]=thaw(ptr[idx++]);
  z->p[z->n+2]=(I)CXthaw(ptr[idx++]);

  tid->p=(I)z;
}

SUBROUTINE
void thawAbeam(tid)IDE tid;
{
  I *ptr=(I*)(Zpack.ice+tid->ftell);
  tid->p=mapIn(ptr+2,ptr[0]);
}

SUBROUTINE 
void thawStr(tid)IDE tid;
{
  I *ptr=(I*)(Zpack.ice+tid->ftell);
  I *res=ma(tid->len);
  ttmv(It,res,ptr,tid->len);
  tid->p=(I)res;
}

SUBROUTINE
void thawPrim(tid)IDE tid;{tid->p=aplus_pi(Zpack.ice+tid->ftell);}
/* thawPrim used for types PK_PRIM and PK_KWORD */

SUBROUTINE
void thawExpr(tid)IDE tid;
{
  I *ptr=(I*)(Zpack.ice+tid->ftell);
  E expr=(E)ma(2+ptr[0]);
  I i;
  expr->n=ptr[0];
  expr->f=thaw(ptr[1]);
  for(i=0;i<expr->n;++i)expr->a[i]=thaw(ptr[2+i]);
  tid->p=ME(expr);
}

SUBROUTINE
void thawXfunc(tid)IDE tid;{tid->p=xslu(Zpack.ice+tid->ftell);}

SUBROUTINE 
void thawLocal(tid)IDE tid;{tid->p=ML(tid->ftell);}

SUBROUTINE
void thawAdf(tid)IDE tid;
{
  I i,idx=0,*ptr=(I*)(Zpack.ice+tid->ftell);
  A z=gv(Et,1);
  z->t=ptr[idx++];
  z->n=ptr[idx++];
  z->r=ptr[idx++];
  for(i=0;i<MAXR+1;++i)z->d[i]=thaw(ptr[idx++]);
  tid->p=(I)z;
}

/* thawNoop is used for to skip PKE_type objects when looping thru Id. */
SUBROUTINE
void thawNoop(tid)IDE tid;
{
}

static void(*Thawfunc[])()= {
  thawZero, thawNull, thawS, thawCx, thawVref,
  thawAvar, thawAEt, thawAfunc, thawAbeam, thawStr,
  thawPrim, thawPrim, thawXfunc, thawExpr, thawLocal,
  thawAdf, thawNoop, thawNoop
  };

SUBROUTINE
I thaw(iid)I iid;
{
  IDE tid=Zpack.Id+iid;
  DH("\343 thaw(iid:%ld)\n",iid);
  DH("\343 Zpack.Id:%ld tid:%ld\n",Zpack.Id,tid);
  DH("\343 thaw: tid->p:%ld tid->type:%ld\n",tid->p,tid->type);
  if(0==tid->p&&PK_ZERO!=tid->type) (*Thawfunc[tid->type])(tid);
  R PK_ICABLE(tid->type)?ic((A)tid->p):tid->p;
}


SUBROUTINE
void thawGlobalVdat(tid,cxt,v)IDE tid;CX cxt;V v;
{
  I *ptr=(I*)(Zpack.ice+tid->ftell);
  A dat=(A)thaw(ptr[3]);
  v->t=ptr[2];
  if(dbg_tdef&&v->t)deftrc(v,0);
  Cx=cxt;
  set(MV(v),(I)dat,1); 
  tid->p=MV(v);
}

SUBROUTINE
void thawGlobalVdep(tid,cxt,v)IDE tid;CX cxt;V v;
{
  I *ptr=(I*)(Zpack.ice+tid->ftell);
  A dat=(A)thaw(ptr[3]);
  v->t=ptr[2];
  if(dbg_tdef)deftrc(v,1);
  Cx=cxt;
  sad(v,dat);
  tid->p=MV(v);
}

SUBROUTINE
void thawGlobalData(iid,cxt,v)I iid;CX cxt;V v;
{
  I dat=thaw(iid);
  v->t=0;
  Cx=cxt;
  set(MV(v),dat,1); 
}

SUBROUTINE
I thawGlobal(iid,scx,vname)I iid;S scx;S vname;
{
  IDE tid=Zpack.Id+iid;
  CX cxt=cxi(scx);
  V v=vi(vname,cxt);
  if(Opts.verbose)H("\343 PKG: Installing %s.%s\n",scx->n,vname->n);
  switch(tid->type)
  {
  case PKE_VDAT:
    thawGlobalVdat(tid,cxt,v);
    break;
  case PKE_VDEP:
    thawGlobalVdep(tid,cxt,v);
    break;
  default:
    thawGlobalData(iid,cxt,v);
    break;
  }
  R tid->p;
}

SUBROUTINE
I thawSlot(iid)I iid;
{
  IDE tid=Zpack.Id+iid;
  I *ptr,result;
  switch(tid->type)
  {
  case PKE_VDAT:
    ptr=(I*)(Zpack.ice+tid->ftell);
    result=thaw(ptr[3]);
    break;
  case PKE_VDEP:
    result=(I)aplus_nl;
    break;
  default:
    result=thaw(iid);
    break;
  }
  return(result);
}
  
/**/
/************************************************************************
 *
 * Routines for storage.  (zstore and subroutines)
 * 
 *
 */

SUBROUTINE
void zstoreStats()
{
  I i;
  H("\343   Type      Objs    Refs    Size\n");
  for(i=0;i<PK_NTYPES;++i)
  {
    H("\343   %s%7ld %7ld %7ld\n",ZpkTypeName[i],
      ZpkStats[i].objs,ZpkStats[i].refs,4*ZpkStats[i].size);
    ZpkStats[PK_NTYPES].refs+=ZpkStats[i].refs;
    ZpkStats[PK_NTYPES].objs+=ZpkStats[i].objs;
    ZpkStats[PK_NTYPES].size+=ZpkStats[i].size;
  }
  H("\343   %s%7ld %7ld %7ld\n",ZpkTypeName[PK_NTYPES],
    ZpkStats[PK_NTYPES].objs,ZpkStats[PK_NTYPES].refs,
    4*ZpkStats[PK_NTYPES].size);
}

SUBROUTINE
I zstoreGlobals(alist)A alist;
{
  I i;
  S svar,scx,vname,cvname;
  CX cxt;
  V v;

  if(!sym(alist)){H("\343 PKG: zstore: bad namelist\n");R 1;}
  for(i=0;i<alist->n;++i)
  {
    svar=XS(alist->p[i]);
    vname=symsplit(svar,&scx);
    cxt=cxlu(scx);
    if(cxt==(CX)0)
    {
      H("\343 PKG: zstore: non-existant context:%s\n",scx->n);
      R 1;
    }
    v=vlu(vname,cxt);
    if(v==(V)0)
    {
      H("\343 PKG: zstore: non-existant global:%s.%s\n",scx->n,vname->n);
      R 1;
    }
    if(Opts.verbose) H("\343 PKG: %ld: Storing %s.%s\n", i,scx->n,vname->n);
    TempEgo.scx=iced((I)scx,(I)PK_SYM);
    TempEgo.vname=iced((I)vname,(I)PK_SYM);
    TempEgo.glob=iced((I)v,(I)PKE_VDAT);
    cvname=symjoin(scx,vname);
    TempEgo.cvname=iced((I)cvname,(I)PK_SYM);
    AddEgoEntry(cvname,&TempEgo);
  }
  R 0;
}

SUBROUTINE
I zstoreSlot(asf)A asf;
{
  A alist=(A)(*asf->p),adata=(A)(asf->p[1]);
  I i;
  S svar,scx,vname,cvname;

  for(i=0;i<alist->n;++i)
  {
    svar=XS(alist->p[i]);
    vname=symsplit(svar,&scx);
    if(Opts.verbose) H("\343 PKG: %ld: Storing %s.%s\n", i,scx->n,vname->n);
    TempEgo.scx=iced((I)scx,(I)PK_SYM);
    TempEgo.vname=iced((I)vname,(I)PK_SYM);
    TempEgo.glob=icedAtom(adata->p[i]);
    cvname=symjoin(scx,vname);
    TempEgo.cvname=iced((I)cvname,(I)PK_SYM);
    AddEgoEntry(cvname,&TempEgo);
  }
  R 0;
}

SUBROUTINE
void zstoreInitAbNihilo()
{
  /* initialize Id and Ego Buffers */
  Zpack.IdBuff=buffalloc();
  Zpack.EgoBuff=buffalloc();
  Zpack.nId=0;
  Zpack.nEgo=0;
  Zpack.IdHashTable=hti(PK_HTSIZE);
  clearStats();

  /* initialize Ego Hash Table */
  EgoHashInitDynamic(EHT_HTSIZE);

  /* Put first two (constant) records into Id. */
  iced((I)0,(I)PK_ZERO);
  iced((I)aplus_nl,(I)PK_NULL);

  /* Fill in header */
  bzero(&ZpkHeader,PK_HDRSIZE*sizeof(I));
  strncpy(ZpkHeader.cookie,PK_COOKIE,8);
  ZpkHeader.version=PK_VERSION;
  ZpkHeader.hdrsize=PK_HDRSIZE;

  /* Write header to ice */
  iceWrite((C *)&ZpkHeader,sizeof(I),ZpkHeader.hdrsize);
}

SUBROUTINE
I zstoreInitFromIce(astr)A astr;
{
  I ilen,iid;
  IDE tid;

  /* initialize Id and Ego Buffers */
  Zpack.IdBuff=buffalloc();
  Zpack.EgoBuff=buffalloc();
  Zpack.nId=0;
  Zpack.nEgo=0;
  Zpack.IdHashTable=hti(PK_HTSIZE);
  clearStats();

  /* initialize ice */
  Zpack.flen=ZpkHeader.File.len;
  if (Zpack.fp) 
  {
#if (_MIPS_SZLONG == 64)
	printf("mmap64: 0, %ld, PROT_READ, MAP_PRIVATE, %d, 0\n", 
		Zpack.flen, (int)fileno(Zpack.fp)) ;
	Zpack.ice=mmap64(0,Zpack.flen,PROT_READ,MAP_PRIVATE,(int)fileno(Zpack.fp),0);
#else
    Zpack.ice=mmap(0,Zpack.flen,PROT_READ,MAP_PRIVATE,(int)fileno(Zpack.fp),0);
#endif
    if(Zpack.ice==(C *)-1)
    {
      H("\343 PKG: zretrieve: failure mapping file.  errno=%d\n",errno);
      R -1;
    }
  }
  else 
  {
    Zpack.ice=(C*)astr->p;
  }

  /* Fill in Id and Ego From preexisting data. */
  Zpack.nEgo=ZpkHeader.Ego.len;
  ilen=Zpack.nEgo*sizeof(EgoEntry);
  buffstuff(Zpack.EgoBuff,Zpack.ice+ZpkHeader.Ego.ftell,ilen);
  Zpack.Ego=(EGOE)Zpack.EgoBuff->min;

  Zpack.nId=ZpkHeader.Id.len;
  ilen=Zpack.nId*sizeof(IdEntry);
  buffstuff(Zpack.IdBuff,Zpack.ice+ZpkHeader.Id.ftell,ilen);
  Zpack.Id=(IDE)Zpack.IdBuff->min;

  /* next, we have to set up ice in buffer correctly, or else position
   * stream if from file */
  if(Zpack.fp) { fseek(Zpack.fp,ZpkHeader.Ice.len,0); }
  else buffstuff(Zpack.ResBuff,Zpack.ice,ZpkHeader.Ice.len);

  /* now thaw everything */
  for(iid=0;iid<Zpack.nId;++iid) thaw(iid);

  /* now remove bit-coding on thawed items, and rebuild Id Hash Table */
  for(iid=0;iid<Zpack.nId;++iid) {
    tid=((IDE)Zpack.IdBuff->min)+iid;
    if(PK_CODEDPTR(tid->type)&&tid->p)tid->p=tid->p&~aplusMask;
    else if(PK_CODEDIDX(tid->type)&&tid->p)tid->p=U(tid->p);
    hashset(Zpack.IdHashTable,tid->p,tid->type,iid);
  }

  /* rebuild Ego Hash Table */
  Eht.source=EHT_NONE;
  EgoHashBuildIfNeeded(EHT_DYNAMIC);

  /* now unmap file (if mapped--we are done with pre-existing data) */
  if (Zpack.fp&&-1==munmap(Zpack.ice, Zpack.flen))
  {
    H("\343 PKG: zstore: error unmapping file.  errno=%d\n",errno);
  }

  /* print out some stuff */
  if (Opts.debug)
  {
    H("\343            Ice: n:%ld\n",Zpack.flen);
    H("\343            Ego: n:%ld  ftell:%ld\n",Zpack.nEgo,ZpkHeader.Ego.ftell);
    H("\343             Id: n:%ld  ftell:%ld\n",Zpack.nId,ZpkHeader.Id.ftell);
    H("\343 Ego Hash Table: %spresent.\n",ZpkHeader.EgoHash.ftell?"":"not ");
  }
  R 0;
}

SUBROUTINE
A zstoreFinishUp()
{
  IDE tid=&TempId;
  I i;
  A z;

  /* write out end-of-ice marker */
  ZpkHeader.Ice.ftell=0L;
  ZpkHeader.Ice.len=iceTell();

  /* zero out p field of Id table, and dc() all entries */
  tid=(IDE)Zpack.IdBuff->min;
  for(i=0;i<Zpack.nId;++i){if(PK_ICABLE(tid[i].type))dc((A)tid[i].p);tid[i].p=0;}

  /* write out Id. */
  ZpkHeader.Id.ftell=iceTell();
  ZpkHeader.Id.len=Zpack.nId;
  iceWrite(Zpack.IdBuff->min,sizeof(IdEntry),Zpack.nId);
  
  /* write out Ego. */
  ZpkHeader.Ego.ftell=iceTell();
  ZpkHeader.Ego.len=Zpack.nEgo;
  iceWrite(Zpack.EgoBuff->min,sizeof(EgoEntry),Zpack.nEgo);

  /* write out Ego Hash Table */
  if (Opts.hash)
  {
    if (Opts.debug) 
      H("\343 Storing Hash Table: nHash:%ld nBuckets:%ld\n",
	Eht.nHash,Eht.nBuckets);
    else if (Opts.verbose) H("\343 PKG: Storing hash table.\n");
    ZpkHeader.EgoHash.ftell=iceTell();
    ZpkHeader.EgoHash.len=2+Eht.nHash+2*Eht.nBuckets;
    iceWrite((C *)&Eht.nHash,sizeof(I),1);
    iceWrite((C *)&Eht.nBuckets,sizeof(I),1);
    iceWrite(Eht.bucketBuff->min,2*sizeof(I),Eht.nBuckets);
    iceWrite((C *)Eht.hashArray,sizeof(I),Eht.nHash);
  }

  /* write out file size information */
  ZpkHeader.File.ftell=0L;
  ZpkHeader.File.len=iceTell();

  /* cleanup: write header, now that block information is there,
   * and then close file and/or free buffers. */
  if(Zpack.fp) 
  {
    rewind(Zpack.fp);
    iceWrite((C *)&ZpkHeader,sizeof(I),ZpkHeader.hdrsize);
    fclose(Zpack.fp);
    z=aplus_nl;
  }
  else 
  {
    I itell=iceTell();
    Zpack.ResBuff->put=Zpack.ResBuff->min;
    iceWrite((C *)&ZpkHeader,sizeof(I),ZpkHeader.hdrsize);
    z=gc(Ct,1,itell,&itell,(I *)Zpack.ResBuff->min);
    bufffree(Zpack.ResBuff);
  }
  bufffree(Zpack.IdBuff);
  bufffree(Zpack.EgoBuff);
  hashfree(Zpack.IdHashTable);
  EgoHashCleanup();

  if(Opts.stats) zstoreStats();
  R z;
}

SUBROUTINE
A zstoreCreate(I zarg,A alist)
{
  Busted=0;
  zstoreInitAbNihilo();
  /* put the objects on ice */
  if (PKARG_SYMVEC==zarg) zstoreGlobals(alist);
  else if (PKARG_SLOT==zarg) zstoreSlot(alist);
  R zstoreFinishUp();
}

SUBROUTINE
A zstoreAdd(A astr,I zarg,A alist)
{
  Busted=0;
  if (zstoreInitFromIce(astr)) R aplus_nl;
  /* put the objects on ice */
  if (PKARG_SYMVEC==zarg) zstoreGlobals(alist);
  else if (PKARG_SLOT==zarg) zstoreSlot(alist);
  R zstoreFinishUp();
}
  
/**/
/************************************************************************
 *
 * Routines for retrieval.  (zretrieve and subroutines)
 * 
 *
 */

SUBROUTINE
void zretrieveGlobalsAll()
{
  I iego;S vname,scx;CX saveCx=Cx;
  for(iego=0;iego<Zpack.nEgo;++iego)
  {
    scx=Sthaw(Zpack.Ego[iego].scx);
    vname=Sthaw(Zpack.Ego[iego].vname);
    if(Opts.verbose)H("\343 PKG: Installing %s.%s\n",scx->n,vname->n);
    thawGlobal(Zpack.Ego[iego].glob,scx,vname);
  }
  Cx=saveCx;
}

SUBROUTINE
void zretrieveGlobalsSymvec(asym)A asym;
{
  I i,iego;S vname,cvname,scx;CX saveCx=Cx;
  A zn=gv(Et,asym->n),zd=gv(Et,asym->n);

  EgoHashBuildIfNeeded(EHT_RUNTIME);
  for(i=0;i<asym->n;++i)
  {
    vname=symsplit(XS(asym->p[i]),&scx);
    cvname=symjoin(scx,vname);
    iego=EgoHashLookup(cvname);
    if(-1==iego)
      H("\343 PKG: Warning: zretrieve: symbol not found:[%s]\n",cvname->n);
    else 
    {
      scx=Sthaw(Zpack.Ego[iego].scx);
      vname=Sthaw(Zpack.Ego[iego].vname);
      if(Opts.verbose)H("\343 PKG: Installing %s.%s\n",scx->n,vname->n);
      thawGlobal(Zpack.Ego[iego].glob,scx,vname);
    }
  }
  Cx=saveCx;
}

SUBROUTINE
void zretrieveGlobalsSymDest(alist)A alist;
{
  I i,iego;S vname,cvname,scx,destscx,destname;CX saveCx=Cx;
  A asym=(A)alist->p[0],adest=(A)alist->p[1];
  A zn=gv(Et,asym->n),zd=gv(Et,asym->n);

  EgoHashBuildIfNeeded(EHT_RUNTIME);
  for(i=0;i<asym->n;++i)
  {
    vname=symsplit(XS(asym->p[i]),&scx);
    cvname=symjoin(scx,vname);
    iego=EgoHashLookup(cvname);
    if(-1==iego)
      H("\343 PKG: Warning: zretrieve: symbol not found:[%s]\n",cvname->n);
    else 
    {
      destname=symsplit(XS(adest->p[i]),&destscx);
      if(Opts.verbose)H("\343 PKG: Installing %s.%s (%s on file)\n",
			destscx->n,destname->n,cvname->n);
      thawGlobal(Zpack.Ego[iego].glob,destscx,destname);
    }
  }
  Cx=saveCx;
}

SUBROUTINE
void
zretrieveGlobals(zarg,alist)I zarg;A alist;
{
  switch(zarg)
  {
  case PKARG_NULL:zretrieveGlobalsAll();break;
  case PKARG_SYMVEC:zretrieveGlobalsSymvec(alist);break;
  case PKARG_SYMDEST:zretrieveGlobalsSymDest(alist);break;
  default:break;
  }
}

SUBROUTINE
A zretrieveSlotAll()
{
  I iego;S cvname;
  A zn=gv(Et,Zpack.nEgo),zd=gv(Et,Zpack.nEgo);

  for(iego=0;iego<Zpack.nEgo;++iego)
  {
    cvname=Sthaw(Zpack.Ego[iego].cvname);
    if(Opts.verbose)
	{
    /* was  H("\343 PKG:  %ld: Retrieving %s for slot\n",iego,cvname->s); */
    H("\343 PKG:  %ld: Retrieving %ld for slot\n",iego,cvname->s);
	}
    zn->p[iego]=MS(cvname);
    zd->p[iego]=thawSlot(Zpack.Ego[iego].glob);
  }
  R (A)gvi(Et,2,zn,zd);
}

SUBROUTINE
A zretrieveSlotSymvec(alist)A alist;
{
  I i,iego;S vname,cvname,scx;
  A zn=gv(Et,alist->n),zd=gv(Et,alist->n);

  DH("\343 z:zretrieveSlotSymvec(alist:%ld)\n",alist);
  EgoHashBuildIfNeeded(EHT_RUNTIME);
  for(i=0;i<alist->n;++i)
  {
    DH("\343 z:zretrieveSlotSymvec: Loop: i:%ld\n",i);
    vname=symsplit(XS(alist->p[i]),&scx);
    cvname=symjoin(scx,vname);
    zn->p[i]=MS(cvname);
    iego=EgoHashLookup(cvname);
    if(-1==iego)
    {
      H("\343 PKG: Warning: zretrieve: symbol not found:[%s]\n",cvname->n);
      zd->p[i]=(I)aplus_nl;
    }
    else 
    {
      if(Opts.verbose)H("\343 PKG: %ld: Retrieving %s for slot\n",i,cvname->n);
      zd->p[i]=thawSlot(Zpack.Ego[iego].glob);
    }
  }
  DH("\343 z:zretrieveSlotSymvec (almost) finished");
  R (A)gvi(Et,2,zn,zd);
}

SUBROUTINE
A zretrieveSlot(zarg,alist)I zarg;A alist;
{
  switch(zarg)
  {
  case PKARG_NULL:R zretrieveSlotAll();
  case PKARG_SYMVEC:R zretrieveSlotSymvec(alist);
  default:R aplus_nl;
  }
}

SUBROUTINE
A zretrieveCatalog()
{
  A z;
  I iego;
  z=gv(Et,Zpack.nEgo);
  for(iego=0;iego<Zpack.nEgo;++iego)z->p[iego]=thaw(Zpack.Ego[iego].cvname);
  R z;
}

SUBROUTINE
void zretrieveSetup()
{
  I ilen;

  Busted=0;
  
  Zpack.nEgo=ZpkHeader.Ego.len;
  ilen=(Zpack.nEgo*sizeof(EgoEntry))/sizeof(I);
  Zpack.Ego=(EGOE)ma(ilen);
  tmv(It,(I *)Zpack.Ego,(I *)(Zpack.ice+ZpkHeader.Ego.ftell),ilen);

  Zpack.nId=ZpkHeader.Id.len;
  ilen=(Zpack.nId*sizeof(IdEntry))/sizeof(I);
  Zpack.Id=(IDE)ma(ilen);
  tmv(It,(I *)Zpack.Id,(I *)(Zpack.ice+ZpkHeader.Id.ftell),ilen);

  EgoHashInitIce();

  /* print out some stuff */
  if (Opts.debug)
  {
    H("\343            Ice: n:%ld\n",Zpack.flen);
    H("\343            Ego: n:%ld  ftell:%ld\n",Zpack.nEgo,ZpkHeader.Ego.ftell);
    H("\343             Id: n:%ld  ftell:%ld\n",Zpack.nId,ZpkHeader.Id.ftell);
    H("\343 Ego Hash Table: %spresent.\n",ZpkHeader.EgoHash.ftell?"":"not ");
  }
}

SUBROUTINE
A zretrievePieces()
{
  A aId,aEgo,aHead,aIce,aEgoHash;
  I dim[2];
  
  /* manufacture result for pieces */
  dim[1]=4;
  dim[0]=Zpack.nId;
  aId=gc(It,2,dim[0]*dim[1],dim,(I*)Zpack.Id);
  dim[0]=Zpack.nEgo;
  aEgo=gc(It,2,dim[0]*dim[1],dim,(I*)Zpack.Ego);
    dim[0]=PK_HDRSIZE;
  aHead=gc(It,1,dim[0],dim,(I *)&ZpkHeader);
  dim[0]=Zpack.flen;
  aIce=gc(Ct,1,dim[0],dim,(I *)Zpack.ice);
  if(EHT_NONE==Eht.source) aEgoHash=aplus_nl;
  else 
  {
    dim[1]=2;dim[0]=Eht.nBuckets;
    aEgoHash=
      (A) gvi(Et,5,gi(Eht.source),gi(Eht.nHash),gi(Eht.nBuckets),
	  gc(It,1,Eht.nHash,&Eht.nHash,Eht.hashArray),
	  gc(It,2,dim[0]*dim[1],dim,(I*)Eht.bucketBuff->min));
  }
  R (A)gvi(Et,5,aHead,aEgo,aId,aIce,aEgoHash);
} 

SUBROUTINE
int zretrieveInitFile(afn,cmdstr,pfname)A afn;C *cmdstr;C **pfname;
{
  C *fname=getfilename(afn);
  int fd;

  /* vette arguments */
  if(fname==(C *)0){H("\343 PKG: zretrieve: bad filename argument\n");R -1;}

  Busted=0;
  /* open file */
  if(-1==(fd=open(fname,O_RDONLY)))
  {
    H("\343 PKG: zretrieve: cannot open file [%s].  errno=%d\n",fname,errno);
    R -1;
  }

  /* read header */
  if(-1==(read(fd,&ZpkHeader,sizeof(ZpkHeader))))
  {
    H("\343 PKG: zretrieve: cannot read header.  errno=%d\n",errno);
    R -1;
  }

  /* map file, and load Zpack */
  Zpack.flen=ZpkHeader.File.len;
#if (_MIPS_SZLONG == 64)
    printf("mmap64: 0, %ld, PROT_READ, MAP_PRIVATE, %d, 0\n",
        Zpack.flen, fd) ;

  Zpack.ice=mmap64(0,Zpack.flen,PROT_READ,MAP_PRIVATE,fd,0);
#else
  Zpack.ice=mmap(0,Zpack.flen,PROT_READ,MAP_PRIVATE,fd,0);
#endif

  if(Zpack.ice==(C *)-1)
  {
    H("\343 PKG: zretrieve: failure mapping file.  errno=%d\n",errno);
    R -1;
  }

  if(dbg_tpack)packtrc(fname,cmdstr,1);
  if(pfname)(*pfname)=fname;
  zretrieveSetup();
  R fd;
}

SUBROUTINE
void zretrieveInitPackstr(astr)A astr;
{
  /* astr must be a valid packstring */

  /* read header */
  bcopy(astr->p,&ZpkHeader,sizeof(ZpkHeader));

  /* map file, and load Zpack */
  Zpack.flen=astr->n;
  Zpack.ice=(C*)astr->p;

  zretrieveSetup();
}

SUBROUTINE
void zretrieveUnmapFile(fd)int fd;
{
  if(-1==munmap(Zpack.ice, Zpack.flen))
  {
    H("\343 PKG: zretrieve: error unmapping file.  errno=%d\n",errno);
  }
  close(fd);
}

SUBROUTINE
void zretrieveCleanup()
{
  I iid;
  /* go through Id, and dc() everything */
  for(iid=0;iid<Zpack.nId;++iid)
    if(Zpack.Id[iid].p&&PK_ICABLE(Zpack.Id[iid].type))dc((A)Zpack.Id[iid].p);

  mf((I *)Zpack.Id);
  mf((I *)Zpack.Ego);
}

/**/
/************************************************************************
 *
 * Entrypoints for storage.  (ep_fnew, et al)
 * 
 *
 */

ENTRYPOINT
A ep_fnew(afn,alist)A afn;A alist;
{
  C *fname=getfilename(afn);
  A z;
  I zarg;
  Z C *cmdstr="fnew";

  /* vette file name */
  if(fname==(C *)0){H("\343 PKG: zfpack: bad first argument\n");R gi(1);}

  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_SYMVEC!=zarg&&PKARG_SLOT!=zarg)
  {
    H("\343 PKG: zstore: bad second argument\n");R gi(1);
  }

  /* open file */
  if(NULL==(Zpack.fp=fopen(fname,"w")))
  {
    H("\343 PKG: zfpack: cannot open file [%s]\n",fname);R gi(1);
  }

  if(dbg_tpack)packtrc(fname,cmdstr,1);
  z=zstoreCreate(zarg,alist);
  if(dbg_tpack)packtrc(fname,cmdstr,0);
  R z;
}

ENTRYPOINT
A ep_fadd(afn,alist)A afn;A alist;
{
  C *fname=getfilename(afn);
  A z;
  I zarg;
  Z C *cmdstr="fpack";

  /* vette file name */
  if(fname==(C *)0){H("\343 PKG: zfpack: bad first argument\n");R gi(1);}

  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_SYMVEC!=zarg&&PKARG_SLOT!=zarg)
  {
    H("\343 PKG: zfadd: bad second argument\n");R gi(1);
  }

  /* open file */
  if(NULL==(Zpack.fp=fopen(fname,"r+")))
  {
    H("\343 PKG: zfadd: cannot open file [%s]\n",fname);R gi(1);
  }

  if(dbg_tpack)packtrc(fname,cmdstr,1);
  z=zstoreAdd(NULL,zarg,alist);
  if(dbg_tpack)packtrc(fname,cmdstr,0);
  R z;
}

ENTRYPOINT
A ep_snew(alist)A alist;
{
  I zarg;
  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_SYMVEC!=zarg&&PKARG_SLOT!=zarg)
  {
    H("\343 PKG: zstore: bad second argument\n");R gi(1);
  }

  /* set up result buffer */
  Zpack.fp=0;
  Zpack.ResBuff=buffalloc();

  R zstoreCreate(zarg,alist);
}

ENTRYPOINT
A ep_sadd(astr,alist)A astr;A alist;
{
  I zarg;
  A aps;

  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_SYMVEC!=zarg&&PKARG_SLOT!=zarg)
  {
    H("\343 PKG: zstore: bad second argument\n");R gi(1);
  }
  if((aps=getapackstr(astr))==(A)0)
  {
    H("\343 PKG: strslot: bad packstring\n");R gi(1);
  }

  /* set up result buffer */
  Zpack.fp=0;
  Zpack.ResBuff=buffalloc();

  R zstoreAdd(aps,zarg,alist);
}

/**/
/************************************************************************
 *
 * Entrypoints for retrieval.  (ep_ffix, et al)
 * 
 *
 */

ENTRYPOINT
A ep_ffix(afn,alist)A afn;A alist;
{
  int fd ;
  I zarg;
  A z=aplus_nl;
  C *fname;
  Z C *cmdstr="ffix";

  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_NULL!=zarg&&PKARG_SYMVEC!=zarg&&PKARG_SYMDEST!=zarg)
  {
    H("\343 PKG: ffix: bad second argument\n");R gi(1);
  }
  fd=zretrieveInitFile(afn,cmdstr,&fname);
  if(-1==fd) R z;
  if(!Opts.pieces) zretrieveGlobals(zarg,alist);
  if (Opts.pieces||Opts.result) z=zretrievePieces();
  else z=(A)gz();
  zretrieveUnmapFile(fd);
  zretrieveCleanup();
  if(dbg_tpack)packtrc(fname,cmdstr,0);
  R z;
}

ENTRYPOINT
A ep_sfix(astr,alist)A astr;A alist;
{
  I zarg;
  A z=aplus_nl,aps;

  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_NULL!=zarg&&PKARG_SYMVEC!=zarg&&PKARG_SYMDEST!=zarg)
  {
    H("\343 PKG: ffix: bad second argument\n");R gi(1);
  }
  if((aps=getapackstr(astr))==(A)0)
  {
    H("\343 PKG: strslot: bad packstring\n");R gi(1);
  }
  zretrieveInitPackstr(aps);
  if(!Opts.pieces) zretrieveGlobals(zarg,alist);
  if (Opts.pieces||Opts.result) z=zretrievePieces();
  else z=(A)gz();
  zretrieveCleanup();
  R z;
}

ENTRYPOINT
A ep_fslot(afn,alist)A afn;A alist;
{
  int fd;
  A z=aplus_nl;
  I zarg;
  C *fname;
  Z C *cmdstr="fslot";

  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_NULL!=zarg&&PKARG_SYMVEC!=zarg)
  {
    H("\343 PKG: fslot: bad second argument\n");R gi(1);
  }
  fd=zretrieveInitFile(afn,cmdstr,&fname);
  if(-1==fd) R z;
  if(!Opts.pieces) z=zretrieveSlot(zarg,alist);
  else z=zretrievePieces();
  zretrieveUnmapFile(fd);
  zretrieveCleanup();
  if(dbg_tpack)packtrc(fname,cmdstr,0);
  R z;
}

ENTRYPOINT
A ep_sslot(astr,alist)A astr;A alist;
{
  A z=aplus_nl,aps;
  I zarg;

  /* vette argument */
  zarg=zargvette(alist);
  if(PKARG_NULL!=zarg&&PKARG_SYMVEC!=zarg)
  {
    H("\343 PKG: strslot: bad second argument\n");R gi(1);
  }
  if((aps=getapackstr(astr))==(A)0)
  {
    H("\343 PKG: strslot: bad packstring\n");R gi(1);
  }
  zretrieveInitPackstr(aps);
  if(!Opts.pieces) z=zretrieveSlot(zarg,alist);
  else z=zretrievePieces();
  zretrieveCleanup();
  R z;
}

/**/
/************************************************************************
 *
 * Other entrypoints and install function.  (ep_fcatalog, et al)
 * 
 *
 */

ENTRYPOINT
A ep_fcatalog(afn)A afn;
{
  int fd;
  A z=aplus_nl;
  C *fname;
  Z C *cmdstr="fcatalog";

  fd=zretrieveInitFile(afn,cmdstr,&fname);
  if(-1==fd) R z;
  if(!Opts.pieces) z=zretrieveCatalog();
  else z=zretrievePieces();
  zretrieveUnmapFile(fd);
  zretrieveCleanup();
  if(dbg_tpack)packtrc(fname,cmdstr,0);
  R z;
}
  
ENTRYPOINT
A ep_scatalog(astr)A astr;
{
  A z=aplus_nl,aps;

  if((aps=getapackstr(astr))==(A)0)
  {
    H("\343 PKG: strslot: bad packstring\n");R gi(1);
  }
  zretrieveInitPackstr(aps);
  if(!Opts.pieces) z=zretrieveCatalog();
  else z=zretrievePieces();
  zretrieveCleanup();
  R z;
}

ENTRYPOINT
void ep_opts(astr)A astr;
{
  if(DefaultAOpts)dc(DefaultAOpts);
  DefaultAOpts=(A)ic(astr);
}

void packInstall()
{
  CX saveCx=Cx;
  Cx=cx("p");

  install((PFI)ep_fnew, "fnew", 0, 2, 0, 0,0,0,0,0,0,0);
  install((PFI)ep_snew, "snew", 0, 1, 0,0,0,0,0,0,0,0);
  install((PFI)ep_fadd, "fadd", 0, 2, 0, 0,0,0,0,0,0,0);
  install((PFI)ep_sadd, "sadd", 0, 2, 0, 0,0,0,0,0,0,0);

  install((PFI)ep_ffix, "ffix", 0, 2, 0, 0,0,0,0,0,0,0);
  install((PFI)ep_sfix, "sfix", 0, 2, 0, 0,0,0,0,0,0,0);
  install((PFI)ep_fslot, "fslot", 0, 2, 0, 0,0,0,0,0,0,0);
  install((PFI)ep_sslot, "sslot", 0, 2, 0, 0,0,0,0,0,0,0);

  install((PFI)ep_fcatalog, "fcatalog", 0, 1, 0,0,0,0,0,0,0,0);
  install((PFI)ep_scatalog, "scatalog", 0, 1, 0,0,0,0,0,0,0,0);
  install((PFI)ep_opts, "opts", 8, 1, 0,0,0,0,0,0,0,0);

  Cx=saveCx;
  R;
}
