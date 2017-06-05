////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _DH_H_INCLUDED
#define _DH_H_INCLUDED

#include "sshbn.h"

void dh_setup_group1(void);
void dh_setup_group(Bignum pval, Bignum gval);
void dh_cleanup(void);
Bignum dh_create_e(int nbits);
Bignum dh_find_K(Bignum f);

#endif //_DH_H_INCLUDED
