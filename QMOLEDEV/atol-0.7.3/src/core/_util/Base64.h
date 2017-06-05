// Base64.h: interface for the CBase64 class.
// Author: Wes Clyburn (clyburnw@enmu.edu)
//////////////////////////////////////////////////////////////////////
// WARNING: this class DOES NOT pad  with '=' as per RFC 1521
//    to the 4byte border (when encoding).
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BASE64_H__FD6A25D1_EE0E_11D1_870E_444553540001__INCLUDED_)
#define AFX_BASE64_H__FD6A25D1_EE0E_11D1_870E_444553540001__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "MIMECode.h"

#define BYTES_PER_LINE  72

// CBase64
// An encoding agent that handles Base64
//
class CBase64 : public CMIMECode  
{
public:
	CBase64();
	virtual ~CBase64();

	// Override the base class mandatory functions
	virtual int Decode( LPCTSTR szDecoding, LPTSTR szOutput );
	virtual String Encode( LPCTSTR szEncoding, int nSize );

protected:
	inline void write_bits(UINT nBits, int nNumBts, LPTSTR szOutput, int& lp);
	inline UINT read_bits(int nNumBits, int* pBitsRead, int& lp);

	int m_nInputSize;
	int m_nBitsRemaining;
	ULONG m_lBitStorage;
	LPCTSTR m_szInput;

	static int m_nMask[];
	static String m_sBase64Alphabet;
};

#endif // !defined(AFX_BASE64_H__FD6A25D1_EE0E_11D1_870E_444553540001__INCLUDED_)
