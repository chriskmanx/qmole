#ifndef included_pwr2hpc_pwr2hpc_h
#define included_pwr2hpc_pwr2hpc_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
extern long q;

#ifndef _AIX
typedef   struct  _quad { long val[2]; } quad;
#endif


/* external macro declarations */
#ifdef _AIXNO
#include <float.h>
#include <fpxcp.h>
extern int fpe_bits;
#define RESETXCP {fpe_bits=0;fp_clr_flag(FP_ALL_XCP);}
#define TESTXCP(x) (((x)&0x20000000)|(0x00011000==((x)&0x0001f000)))
#define CHECKXCP {BD f;int g;f.d=__readflm();g=f.i[1];if(fpe_bits||TESTXCP(g))q=9;}
#else
#define RESETXCP    
#define CHECKXCP   
#endif

/* external struct, union, typedef and enum declarations */
typedef union {int i[2]; short s[4]; double d;} BD;

/* external function declarations */
/* integer logical dyadic scalar cores */
extern void hpc_vvz0();
extern void hpc_svz0();
extern void hpc_vsz0();
extern void hpc_vvz1();
extern void hpc_svz1();
extern void hpc_vsz1();

/* integer arithmetic dyadic scalar cores */
extern void hpc_vvi0();
extern void hpc_svi0();
extern void hpc_vsi0();
extern void hpc_vvi1();
extern void hpc_svi1();
extern void hpc_vsi1();
extern void hpc_vvi2();
extern void hpc_svi2();
extern void hpc_vsi2();
extern void hpc_vvi3();
extern void hpc_svi3();
extern void hpc_vsi3();
extern void hpc_vvi4();
extern void hpc_svi4();
extern void hpc_vsi4();
extern void hpc_vvi5();
extern void hpc_svi5();
extern void hpc_vsi5();
extern void hpc_vvi6();
extern void hpc_svi6();
extern void hpc_vsi6();

/* integer comparison dyadic scalar cores */
extern void hpc_vvj0();
extern void hpc_svj0();
extern void hpc_vsj0();
extern void hpc_vvj1();
extern void hpc_svj1();
extern void hpc_vsj1();
extern void hpc_vvj2();
extern void hpc_svj2();
extern void hpc_vsj2();
extern void hpc_vvj3();
extern void hpc_svj3();
extern void hpc_vsj3();
extern void hpc_vvj4();
extern void hpc_svj4();
extern void hpc_vsj4();
extern void hpc_vvj5();
extern void hpc_svj5();
extern void hpc_vsj5();

/* float logical dyadic scalar cores */
extern void hpc_vvy0();
extern void hpc_svy0();
extern void hpc_vsy0();
extern void hpc_vvy1();
extern void hpc_svy1();
extern void hpc_vsy1();

/* float arithmetic dyadic scalar cores */
extern void hpc_vvs_f0();
extern void hpc_svs_f0();
extern void hpc_vss_f0();
extern void hpc_vvs_f1();
extern void hpc_svs_f1();
extern void hpc_vss_f1();
extern void hpc_vvf2();
extern void hpc_svf2();
extern void hpc_vsf2();
extern void hpc_vvf3();
extern void hpc_svf3();
extern void hpc_vsf3();
extern void hpc_vvf4();
extern void hpc_svf4();
extern void hpc_vsf4();
extern void hpc_vvf5();
extern void hpc_svf5();
extern void hpc_vsf5();
extern void hpc_vvf6();
extern void hpc_svf6();
extern void hpc_vsf6();

/* float comparison dyadic scalar cores */
extern void hpc_vvh0();
extern void hpc_svh0();
extern void hpc_vsh0();
extern void hpc_vvh1();
extern void hpc_svh1();
extern void hpc_vsh1();
extern void hpc_vvh2();
extern void hpc_svh2();
extern void hpc_vsh2();
extern void hpc_vvh3();
extern void hpc_svh3();
extern void hpc_vsh3();
extern void hpc_vvh4();
extern void hpc_svh4();
extern void hpc_vsh4();
extern void hpc_vvh5();
extern void hpc_svh5();
extern void hpc_vsh5();

/* float transcendental dyadic scalar cores */
extern void hpc_vvex();
extern void hpc_svex();
extern void hpc_vsex();
extern void hpc_vvlg();
extern void hpc_svlg();
extern void hpc_vslg();
extern void hpc_vvpi();
extern void hpc_svpi();
extern void hpc_vspi();

/* character comparison dyadic scalar cores */
extern void hpc_vvc0();
extern void hpc_svc0();
extern void hpc_vsc0();
extern void hpc_vvc1();
extern void hpc_svc1();
extern void hpc_vsc1();

/* integer vector reduction cores */
extern void hpc_vrsz0();
extern void hpc_vrsz1();
extern void hpc_vrsi0();
extern void hpc_vrsi1();
extern void hpc_vrsi2();
extern void hpc_vrsi3();

/* float vector reduction cores */
extern void hpc_vrsy0();
extern void hpc_vrsy1();
extern void hpc_vrsf0();
extern void hpc_vrsf1();
extern void hpc_vrsf2();
extern void hpc_vrsf3();

/* integer vector scan cores */
extern void hpc_vscz0();
extern void hpc_vscz1();
extern void hpc_vsci0();
extern void hpc_vsci1();
extern void hpc_vsci2();
extern void hpc_vsci3();

/* float vector scan cores */
extern void hpc_vscy0();
extern void hpc_vscy1();
extern void hpc_vscf0();
extern void hpc_vscf1();
extern void hpc_vscf2();
extern void hpc_vscf3();

/* vector casting pseudo cores */
extern void hpc_cvci();
extern void hpc_cvcf();
extern void hpc_cvic();
extern void hpc_cvif();
extern void hpc_cvfc();
extern void hpc_cvfi();

/* monadic scalar pseudo cores */
extern void hpc_mSgnII();
extern void hpc_mSgnFI();
extern void hpc_mClnFI();
extern void hpc_mClnFF();
extern void hpc_mFlrFI();
extern void hpc_mFlrFF();
extern void hpc_mNegII();
extern void hpc_mNegIF();
extern void hpc_mNegFF();
extern void hpc_mRecFF();
extern void hpc_mAabII();
extern void hpc_mAabIF();
extern void hpc_mAabFF();
extern void hpc_mAenFF();
extern void hpc_mAlnFF();
extern void hpc_mRanII();
extern void hpc_mNotII();
extern void hpc_mNotFI();
extern void hpc_mPitFF();

/* tmv cores */
extern char *hpc_mv_i();
extern char *hpc_mv_f();
extern char *hpc_mv_c();
extern char *hpc_rp_i();
extern char *hpc_rp_f();
extern char *hpc_rp_c();


#endif /* included_hpc_hpc_h */

