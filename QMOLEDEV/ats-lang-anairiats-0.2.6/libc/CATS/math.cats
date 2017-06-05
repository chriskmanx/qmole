/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi.
**
** ATS is  free software;  you can redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*/

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef ATS_LIBC_MATH_CATS
#define ATS_LIBC_MATH_CATS

/* ****** ****** */

#include <math.h>

/* ****** ****** */
//
// HX: fpclassify
//
#define atslib_fpclassify(x) fpclassify(x)
#define atslib_isfinite(x) isfinite(x)
#define atslib_isnormal(x) isnormal(x)

/* ****** ****** */

//
// HX: [isinf] and [isnan] are BSD provision
//
#define atslib_isinf isinf
#define atslib_isinff isinff
#define atslib_isinfl isinfl

#define atslib_isnan isnan
#define atslib_isnanf isnanf
#define atslib_isnanl isnanl

/* ****** ****** */

#define atslib_ceil ceil
#define atslib_ceilf ceilf
#define atslib_ceill ceill

#define atslib_floor floor
#define atslib_floorf floorf
#define atslib_floorl floorl

#define atslib_round round
#define atslib_roundf roundf
#define atslib_roundl roundl

#define atslib_trunc trunc
#define atslib_truncf truncf
#define atslib_truncl truncl

#define atslib_fmod fmod
#define atslib_fmodf fmodf
#define atslib_fmodl fmodl

/* ****** ****** */

#define atslib_fmax fmax
#define atslib_fmaxf fmaxf
#define atslib_fmaxl fmaxl

#define atslib_fmin fmin
#define atslib_fminf fminf
#define atslib_fminl fminl

#define atslib_fdim fdim
#define atslib_fdimf fdimf
#define atslib_fdiml fdiml

#define atslib_fma fma
#define atslib_fmaf fmaf
#define atslib_fmal fmal

/* ****** ****** */

#define atslib_sqrt sqrt
#define atslib_sqrtf sqrtf
#define atslib_sqrtl sqrtl

#define atslib_cbrt cbrt
#define atslib_cbrtf cbrtf
#define atslib_cbrtl cbrtl

#define atslib_pow pow
#define atslib_powf powf
#define atslib_powl powl

/* ****** ****** */

#define atslib_exp exp
#define atslib_expf expf
#define atslib_expl expl

/* ****** ****** */

#define atslib_log log
#define atslib_logf logf
#define atslib_logl logl

#define atslib_log10 log10
#define atslib_log10f log10f
#define atslib_log10l log10l

/* ****** ****** */

#define atslib_asin asin
#define atslib_asinf asinf
#define atslib_asinl asinl

#define atslib_acos acos
#define atslib_acosf acosf
#define atslib_acosl acosl

#define atslib_atan atan
#define atslib_atanf atanf
#define atslib_atanl atanl

#define atslib_atan2 atan2
#define atslib_atan2f atan2f
#define atslib_atan2l atan2l

/* ****** ****** */

#define atslib_asinh asinh
#define atslib_asinhf asinhf
#define atslib_asinhl asinhl

#define atslib_acosh acosh
#define atslib_acoshf acoshf
#define atslib_acoshl acoshl

/* ****** ****** */

#define atslib_sin sin
#define atslib_sinf sinf
#define atslib_sinl sinl

#define atslib_cos cos
#define atslib_cosf cosf
#define atslib_cosl cosl

#define atslib_tan tan
#define atslib_tanf tanf
#define atslib_tanl tanl

/* ****** ****** */

#define atslib_sinh sinh
#define atslib_sinhf sinhf
#define atslib_sinhl sinhl

#define atslib_cosh cosh
#define atslib_coshf coshf
#define atslib_coshl coshl

#define atslib_tanh tanh
#define atslib_tanhf tanhf
#define atslib_tanhl tanhl

/* ****** ****** */

#endif /* ATS_LIBC_MATH_CATS */

/* end of [math.cats] */
