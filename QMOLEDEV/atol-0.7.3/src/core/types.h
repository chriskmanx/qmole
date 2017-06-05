////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Portable types definitions
//////////////////////////////////////////////////////////////////////////// 

#ifndef TYPES_H__
#define TYPES_H__

#ifdef _WIN32
 #define _WINSOCKAPI_   // Prevent inclusion of winsock.h in windows.h
 #include <windows.h>
 typedef __int64			INT64;
 typedef unsigned __int64	UINT64;
#else
 typedef long long			INT64;
 typedef unsigned long long UINT64;
#endif

#define UINT64_MAX UINT64(-1)
#define INT64_MAX  INT64(UINT64_MAX / 2)

#endif	// TYPES_H__
