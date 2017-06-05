#ifndef MSLimitsHEADER
#define MSLimitsHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#if !defined(MS_NO_LIMITS_HEADER)
#include <limits.h>
#else
#if defined(MS_HAS_SYS_LIMITS_HEADER)
#include <sys/limits.h>
#else
#define CHAR_BIT		(8)
#define CHAR_MAX		((char)(127))
#define CHAR_MIN		((char)(-128))
#define SCHAR_MAX		((char)(127))
#define SCHAR_MIN		((char)(-128))
#define UCHAR_MAX		((unsigned char)(255))
#define SHRT_MAX		((short)(32767))
#define SHRT_MIN		((short)(-32768))
#define USHRT_MAX		((unsigned short)(65535))
#define INT_MAX			((int)(2147483647))
#define INT_MIN			((int)(-2147483648))
#define UINT_MAX		((unsigned)(4294967295))
#define LONG_MAX		((long)(2147483647))
#define LONG_MIN		((long)(-2147483648))
#define ULONG_MAX		((unsigned long)(4294967295))
#endif // MS_HAS_SYS_LIMITS_HEADER
#endif //MS_NO_LIMITS_HEADER

#endif
