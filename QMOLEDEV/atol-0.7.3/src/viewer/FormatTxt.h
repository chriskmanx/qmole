////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to help formating file (for file viewer) as a text file
////////////////////////////////////////////////////////////////////////////

#ifndef FORMATTXT_H__
#define FORMATTXT_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Format.h"
#include <vector>

class FormatTxt : public Format  
{
public:
	FormatTxt();
	virtual ~FormatTxt();

	virtual UINT64 GetLineCount();
	virtual UINT64 GetLineOffset(UINT64 nLine);
	virtual int GetLineSize(UINT64 nLine);

	virtual CMarker OffsetToScreen(INT64 nOffset);
	virtual INT64 ScreenToOffset(const CMarker &pos);

	virtual bool GetLineRaw(UINT64 nLine, LPBYTE &pBuffer, int &nLineSize);
	virtual bool GetLineFormated(UINT64 nLine, LPBYTE &pszLine, int &nLineSize);

	bool m_bAbortThread;

protected:
	void ParseLinesFromBuffer();
	void BreakLines(UINT64 nOffset);

public:
	inline bool GetWrap(){ return m_bWrap; }
	inline void SetWrap(bool bVal){ m_bWrap = bVal; }
	
	void DoWrap(int nLen);
	void ExpandTabs(LPBYTE pBuffer, unsigned int nLen, int &nNewLen);
	void TextLineCounter();

	int	m_nTabWidth;	// how many spaces is tab "worth"

	//TOFIX use line lengths instead of offsets for editor ?
	std::vector<UINT64>	m_lstRealOffsets;	//real line offsets
	std::vector<UINT64>	m_lstLineOffsets;	//offsets wrapped to MAX_TEXT_LINE
	
	//wrapping support
	std::vector<UINT64>	m_lstWrapOffsets;	//offsets wrapped to current wrap size
	bool m_bWrap;
	int  m_nWrapLength;
};

#endif // FORMATTXT_H__
