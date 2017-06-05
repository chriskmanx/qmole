////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Abstract base class to implement file formatting for file viewer
////////////////////////////////////////////////////////////////////////////

#ifndef _FORMAT_H__
#define _FORMAT_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Selection.h"
#include "../core/types.h"

#ifndef LPBYTE
 #define LPBYTE unsigned char *
#endif

class CStorage;

class Format
{
public:
	Format();
	virtual ~Format();

	virtual UINT64 GetLineCount() = 0;
	virtual UINT64 GetLineOffset(UINT64 nLine) = 0;
	virtual int GetLineSize(UINT64 nLine) = 0;

	virtual CMarker OffsetToScreen(INT64 nOffset) = 0;
	virtual INT64   ScreenToOffset(const CMarker &pos) = 0;

	virtual bool GetLineRaw(UINT64 nLine, LPBYTE &pBuffer, int &nLineSize) = 0;
	virtual bool GetLineFormated(UINT64 nLine, LPBYTE &pszLine, int &nLineSize) = 0;

	CStorage *m_pData;
};

#endif 
