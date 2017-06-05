////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to help formating file (for file viewer) as a bin file (fixed width view)
////////////////////////////////////////////////////////////////////////////

#ifndef _FORMATBIN_H__
#define _FORMATBIN_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Format.h"

class FormatBin : public Format  
{
public:
	FormatBin();
	virtual ~FormatBin();

	virtual UINT64 GetLineCount();
	virtual UINT64 GetLineOffset(UINT64 nLine);
	virtual int GetLineSize(UINT64 nLine);

	virtual CMarker OffsetToScreen(INT64 nOffset);
	virtual INT64 ScreenToOffset(const CMarker &pos);

	virtual bool GetLineRaw(UINT64 nLine, LPBYTE &pBuffer, int &nLineSize);
	virtual bool GetLineFormated(UINT64 nLine, LPBYTE &pszLine, int &nLineSize);

public:
	unsigned int m_nBinLineLength;
};

#endif
