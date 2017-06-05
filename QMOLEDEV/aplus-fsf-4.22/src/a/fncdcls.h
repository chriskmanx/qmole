#ifndef included_a_fncdcls_h
#define included_a_fncdcls_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <sys/types.h>
#include <stdio.h>
#include <a/k.h>

#ifdef __cplusplus
extern "C" {
#endif

extern unsigned long hafn(unsigned long);		/* att */
extern HT hti(unsigned long);				/* att */
extern void mvht(HT,HT);				/* att */
extern I htgi(HT,S,PFI,I*);				/* att */
extern I htsi(HT,S,I,I,I(*)(void));			/* att */
extern I htxi(HT,S,I);					/* att */
extern void rmatb(V);					/* att */
extern void attInstall(void);				/* att */
extern void mi(void);					/* b */
/* extern unsigned long mb(long*,unsigned long);	   b */
extern C* mab(unsigned long);				/* b */
extern long* ma(unsigned long);				/* b */
extern void mf(I*);					/* b */
extern unsigned long mc(void);				/* b */
extern unsigned long *mz(void);				/* b */
/* Functions for controlling atmp */
extern long *(*pf_ma)(unsigned long);                   /* b */
extern long *ma_atmp(unsigned long);                    /* b */
extern long *ma_malloc(unsigned long);                  /* b */
extern void (*pf_mf)(long *);                           /* b */
extern void mf_atmp(long *);                            /* b */
extern void mf_malloc(long *);                          /* b */
extern void memStatsInstall(void);                      /* b */

extern I getAutoBeamConvert(void);			/* beam */
extern I mf_length(A aobj);                             /* beam */
extern I isWritableFile(I a);                           /* beam */
extern void MFALimitSysCmd(I newlim);                   /* beam */
extern void beamInstall(void);                          /* beam */
extern void setAutoBeamConvert(I m);                    /* beam */

extern void dst(V);					/* callouts */
extern I xf(void);					/* callouts */
extern void xup(V,A,A,A,I,I);				/* callouts */
extern I vfy(V,A);					/* callouts */
extern void disable(void);				/* callouts */
extern void enable(void);				/* callouts */

/* MicroSoft visual C++ mangles these variable names*/
#ifdef _MSC_VER
extern I dbg_tl;                                        /* dbg */
extern I dbg_tb;                                        /* dbg */
extern I dbg_tf;                                        /* dbg */
extern I dbg_tx;                                        /* dbg */
extern I dbg_ts;                                        /* dbg */
extern I dbg_tbwc;                                      /* dbg */
extern I dbg_tdef;                                      /* dbg */
extern I dbg_tdep;                                      /* dbg */
extern I dbg_tinv;                                      /* dbg */
extern I dbg_tnan;                                      /* dbg */
extern I dbg_tpcb;                                      /* dbg */
extern I dbg_tscb;                                      /* dbg */
extern I dbg_tpack;                                     /* dbg */
extern I dbg_tprcb;                                     /* dbg */
extern I dbg_trcb;                                      /* dbg */
extern I dbg_txeq;                                      /* dbg */
extern I dbg_tmdo;                                      /* dbg */
extern I dbg_tdyld;                                     /* dbg */
extern I dbg_depth;                                     /* dbg */
extern I dbg_tmstk;                                     /* dbg */
extern I dbg_tkerr;                                     /* dbg */
extern I dbg_xfpe;                                      /* dbg */
extern I dbg_tfmt;                                      /* dbg */
extern I dbg_twa;                                       /* dbg */
extern I dbg_tdoErrorStack;                             /* dbg */
#endif

extern I nanbeamchk(C *,A);				/* dbg */
extern I bitwisechk(A,A,I);				/* dbg */
extern I functrc(A,I);					/* dbg */
extern I xftrc(C *,I);					/* dbg */
extern I loadtrc(C *,I);				/* dbg */
extern I xeqtrc(C *,I);					/* dbg */
extern I mdotrc(I);					/* dbg */
extern I beamtrc(C *,I,I);				/* dbg */
extern I cbtrc(V,I);					/* dbg */
extern I deptrc(V,I);					/* dbg */
extern I invtrc(V,I);					/* dbg */
extern void watrc(I);					/* dbg */
extern void doErrorStacktrc(I, A);			/* dbg */
extern I deftrc(V,I);					/* dbg */
extern void dbg(C *,C *);				/* dbg */
extern C **get_dbglist(void);				/* dbg */
extern void dbgInstall(void);				/* dbg */
extern C *PIDcmd(pid_t);                                /* dyldSlow */
extern I rsh(A,I,I *);					/* f */
extern I ic_or_copy(A);                                 /* f */
extern I pck(I,A);					/* gpick */
extern I pka(A,A *);					/* gpick */
extern I ne(F *,F *);					/* i */
extern I sym(A);					/* i */
extern I fsy(A);					/* i */
extern I cm(I *,I *,I);					/* i */
extern I mt(A,A);					/* i */
extern I fnd(A,A);					/* i */
extern I index_of(A,A);					/* i */
extern I xin(A,I,A);					/* j */
extern I xr(A,A,A);					/* j */
extern void ki(void);					/* k */
extern I ic(A);						/* k */
extern void dc(A);					/* k */
extern void dec(A);					/* k */
extern void ef(I);					/* k */
extern I *k_tm(I);					/* k */
extern void mv(I *,I *,I);				/* k */
extern C *tst(I,I *,I,I *,I,I);				/* k */
extern C *tmv(I,I *,I *,I);				/* k */
extern C *trp(I,I *,I *,I);				/* k */
extern C *zer(I,I *,I);					/* k */
extern I zr(A);						/* k */
extern I tr(I,I *);					/* k */
extern I tr1(I,I *);					/* k */
extern A gm(I,I,I);					/* k */
extern A gv(I t,I n);					/* k */
extern A gd(I,A);					/* k */
extern A ga(I,I,I,I *);					/* k */
extern A gc(I,I,I,I *,I *);				/* k */
extern A gi(I);						/* k */
extern A gs(I);						/* k */
extern A gf(F);						/* k */
extern A ge(I);						/* k */
extern I ev(I);						/* k */
extern I ee(E);						/* k */
extern I fa(I,I,I);					/* k */
extern A af4(A,I,I,I,I,V);				/* k */
extern A un(A *);					/* k */
extern I set(I,I,I);					/* k */
extern I aset(I,I,I,I); 				/* k */
extern I xis(E);					/* k */
extern A getBeamMSyncMode(void);                        /* k */
extern void setBeamMSyncMode(A);                        /* k */
extern I log_EWouldBlock(I,I,I,C *,C *);		/* m */
extern I syst(C *);					/* m */
extern void gwd(C *s);					/* m */
extern void siginit(void);				/* m */
extern A getSigv(void);					/* m */
extern A getSigb(void);					/* m */
extern void setSigv(I);				        /* m */
extern void setSigb(I);				        /* m */
extern I wr(I);						/* m */
extern I im(I);						/* m */
extern void dm(A);					/* m */
extern I map(int,int);                                  /* m */
extern int nmap(int,int,C *,C *);			/* m */
extern void dbg_mfr(void);				/* m */
extern int flen(int,off_t);				/* m */
extern void wi(void);					/* m */
extern I tmp(I);					/* m */
extern I wa(I);						/* m */
extern I twGet(void);					/* m */
extern I ep_all(void);					/* m */
extern C *pfind(C *,C *,C *,I);				/* m */
extern I unloadable(C*,I);			        /* m */
/* Functions for controlling atmp */
extern void setAtmpMmapFlags(int);                      /* m */
extern I (*pf_tmp)(I);                                  /* m */
extern I tmp_atmp(I);                                   /* m */
extern I tmp_malloc(I);                                 /* m */
extern int atmpMissing(void);                           /* m */

extern void te(void);					/* n */
extern I t2(I,I);					/* n */
extern void xfs(void);					/* n */
extern void x_fs(void);					/* n */
extern void install(PFI,const C *,I,I,I,I,I,I,I,I,I,I);	/* n */
extern void xshti(void);				/* n */
extern A ep_xfsinfo(void);				/* n */
extern A gsym(const C*);                                /* nsf */
extern S symjoin(S,S);					/* nsf */
extern S symsplit(S,S *);				/* nsf */
#if (__sgi && ((_MIPS_SZLONG == 64) || (_MIPS_SIM == _ABIN32)))
extern void coreLimSet(long long);
#elif defined(__osf__)
extern void coreLimSet(long) ;
#else
extern void coreLimSet(I);				/* nsf */
#endif
extern void stdinFlagSet(I);				/* nsf */
extern A versGet(void);					/* nsf */
extern C *getaname(A);					/* nsf */
extern void dbg_flg(void);				/* nsf */
extern I excxt(CX);					/* nsf */
extern void setPWD(void);				/* nsf */
extern void nsfInstall(void);				/* nsf */
extern void versSet(C*);				/* nsf */
extern void phaseOfReleaseSet(I);			/* nsf */
extern void majorReleaseSet(I);				/* nsf */
extern void minorReleaseSet(I);				/* nsf */
extern I rk(I,A,A,A);					/* o */
extern I ea(I,A,A);					/* o */
extern A getCircleFuncSyms(void);			/* o */
extern F pif(I,F);					/* o */
extern C **get_primlist(int, int);			/* p */
extern I chtsi(HT,C *,I);				/* p */
extern I lu(C *,C **);					/* p */
extern I aplus_pi(C *);					/* p */
extern void sik(void);					/* p */
extern A sikAsAObj(void); 			        /* p */
extern I sik_exp(I);					/* p */
extern void sk(void);					/* p */
extern I paf(A,I,I);					/* p */
extern void pa(V);					/* p */
extern I mth(A);					/* p */
extern void p0hti(void);				/* p */
extern CX cxhti(void);					/* r */
extern CX cxi(S);					/* r */
extern CX cxlu(S);					/* r */
extern V vi(S,CX);					/* r */
extern V vlu(S,CX);					/* r */
extern CX cx(const C *);				/* r */
extern I gz(void);					/* r */
extern I qz(A);						/* r */
extern I peak(I);					/* r */
extern void f0(C *);					/* r */
extern void f1(C *);					/* r */
extern void inv(V,I,I);					/* r */
extern void val(V);					/* r */
extern I gt(V);						/* r */
extern void rmd(V);					/* r */
extern I rd(I *);					/* r */
extern int infi(void);					/* s */
extern I rm(I,I);					/* s */
extern I ds(A,A,I);					/* s */
extern I rs(A,I);					/* s */
extern I sc(A,I);					/* s */
extern C *searchPATH(C*);                               /* searchPATH */
extern A getSymKstack(void);				/* si */
extern void snapshotKstack(void);			/* si */
extern A showLastSavedKstack(void);			/* si */
extern void symhti(void);				/* u */
extern S si(const C *);					/* u */
extern A SymbolTableHashChainLengths(void);		/* u */
extern A SymbolTableBlockInfo(void);			/* u */
extern V sv(CX,S);					/* u */
extern V svlu(CX,S);					/* u */

extern I ispu(I);					/* u */
extern I isal(I);					/* u */
extern I isdi(I);					/* u */

extern C *dlb(C *);					/* u */
extern C *cl(C *);					/* u */
extern C *bl(C *);					/* u */
extern void tc(I *);					/* u */
extern A gsv(I,const C *);				/* u */
extern A gvi(I,I,...);					/* u */
extern void ff(A);					/* u */
extern C *nx(C *);					/* u */
extern C *sj(C *,I);					/* u */
extern void rf(C *,FILE *);				/* u */
extern I tf(void);					/* u */
extern void xrr(void);					/* u */
extern I aplus_err(I,A);				/* u */
extern void perr(C *);					/* u */
extern I ez(I);						/* u */
extern I exm(C *, I);					/* u */
extern I pev(I);					/* u */
extern I pexm(I,I);					/* u */
extern I af(I);						/* u */
extern void frep(A);					/* u */
extern C *doloadafile(C *,int);				/* y */
extern I exx(V);					/* y */
extern void sys(C*);					/* y */
extern A ci(I);						/* y */
extern A ep_cf(I);					/* y */
extern I cn(I,I);					/* y */
extern I bwcv(A,A);					/* y */
extern I loadafile(C*,int);				/* y */

/* ????
extern A gn(I);
extern A grc(A,int,int);
extern A ld1(A);
extern int isnan(double);
*/
#ifdef __cplusplus
}
#endif

#endif
