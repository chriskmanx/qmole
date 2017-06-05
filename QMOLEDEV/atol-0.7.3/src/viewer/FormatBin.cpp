////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to help formating file (for file viewer) as a bin file (fixed width view)
////////////////////////////////////////////////////////////////////////////

#include "FormatBin.h"
#include "Storage.h"

#ifndef min
 #define min(a,b) ((a)<(b))?(a):(b)
#endif

FormatBin::FormatBin()
{
	m_nBinLineLength = 80;
}

FormatBin::~FormatBin()
{
}

UINT64 FormatBin::GetLineCount()
{
	//ASSERT(NULL != m_pData);

	UINT64 nLineCount = m_pData->m_nFileSize / m_nBinLineLength;
	if((m_pData->m_nFileSize % m_nBinLineLength) > 0)
		nLineCount ++;

	return nLineCount;
}

int FormatBin::GetLineSize(UINT64 nLine)
{
	return m_nBinLineLength;
}

UINT64 FormatBin::GetLineOffset(UINT64 nLine)
{
	//TOFIX ASSERT()
	return m_nBinLineLength * nLine;
}

CMarker FormatBin::OffsetToScreen(INT64 nOffset)
{
	CMarker res;
	res.line	 = nOffset / m_nBinLineLength;
	res.column	 = nOffset % m_nBinLineLength;
	return res;
}

INT64 FormatBin::ScreenToOffset(const CMarker &pos)
{
	//ASSERT(NULL != m_pData);

	INT64 nOffset = pos.line * m_nBinLineLength + pos.column;
	if(nOffset > (INT64)m_pData->m_nFileSize)
		nOffset = -1;
	
	return nOffset;
}

bool FormatBin::GetLineRaw(UINT64 nLine, LPBYTE &pBuffer, int &nLineSize)
{
	//ASSERT(NULL != m_pData);

	UINT64 nOffset = GetLineOffset(nLine);
	if(nOffset < m_pData->m_nFileSize)
	{
		//TOFIX only if line not inside current buffer
		m_pData->FillBuffer(nOffset);
		pBuffer = m_pData->m_szBuffer;

		nLineSize = min(m_nBinLineLength, m_pData->m_nFileSize - nOffset);
		return true;
	}
	return false;
}

bool FormatBin::GetLineFormated(UINT64 nLine, LPBYTE &pszLine, int &nLineSize)
{
	LPBYTE pBuffer;

	if(!GetLineRaw(nLine, pBuffer, nLineSize))
	{
		//TRACE("No text for line %d\n", nLine);
		return false;
	}

	pszLine		= pBuffer;
	return true;
}
