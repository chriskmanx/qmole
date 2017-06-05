////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _DES_H_INCLUDED_
#define _DES_H_INCLUDED_

typedef struct {
    word32 k0246[16], k1357[16];
    word32 eiv0, eiv1;
    word32 div0, div1;
} DESContext;

#endif //_DES_H_INCLUDED_

