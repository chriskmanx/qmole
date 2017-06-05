////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to help formating file (for file viewer) as a hex file
////////////////////////////////////////////////////////////////////////////

#ifndef FORMATHEX_H__
#define FORMATHEX_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Format.h"

class FormatHex : public Format  
{
public:
	FormatHex();
	virtual ~FormatHex();

	virtual UINT64 GetLineCount();
	virtual UINT64 GetLineOffset(UINT64 nLine);
	virtual int GetLineSize(UINT64 nLine);

	virtual CMarker OffsetToScreen(INT64 nOffset);
	virtual INT64 ScreenToOffset(const CMarker &pos);

	virtual bool GetLineRaw(UINT64 nLine, LPBYTE &pBuffer, int &nLineSize);
	virtual bool GetLineFormated(UINT64 nLine, LPBYTE &pszLine, int &nLineSize);

public:
	int  GetHexDataColumn();
	void HexFormatData(LPBYTE pBuffer, int nLen, int &nNewLen);
	unsigned int  m_nHexLineLength;
};

#endif // 
