// MIMECode.h: interface for the CMIMECode class.
// Author: Wes Clyburn (clyburnw@enmu.edu)
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MIMECODE_H__FD6A25D0_EE0E_11D1_870E_444553540001__INCLUDED_)
#define AFX_MIMECODE_H__FD6A25D0_EE0E_11D1_870E_444553540001__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef LPCTSTR
 #define LPCTSTR const char *	
#endif
#ifndef LPTSTR
 #define LPTSTR char *	
#endif
#ifndef ULONG
 #define ULONG unsigned long
#endif
#ifndef UINT
 #define UINT unsigned int
#endif

#include "../String.h"

// CMIMECode
// Abstract base class. CMIMECode objects encode and decode
// MIME contents (examples: Base64, Quoted-Printable)
//
class CMIMECode  
{
public:
	CMIMECode();
	virtual ~CMIMECode();

	virtual int Decode( LPCTSTR szDecoding, LPTSTR szOutput ) = 0;
	virtual String Encode( LPCTSTR szEncoding, int nSize ) = 0;
};

#endif // !defined(AFX_MIMECODE_H__FD6A25D0_EE0E_11D1_870E_444553540001__INCLUDED_)
