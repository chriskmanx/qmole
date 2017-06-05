////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to help formating file (for file viewer) as a hex file
////////////////////////////////////////////////////////////////////////////

#include "FormatHex.h"
#include "Storage.h"

#ifndef min
 #define min(a,b) ((a)<(b))?(a):(b)
#endif

FormatHex::FormatHex()
{
	m_nHexLineLength = 16;	//16 chars	-> (4/8/16)
}

FormatHex::~FormatHex()
{
}

UINT64 FormatHex::GetLineCount()
{
	//ASSERT(NULL != m_pData);

	UINT64 nLineCount = m_pData->m_nFileSize / (UINT64)m_nHexLineLength;
	if((m_pData->m_nFileSize % m_nHexLineLength) > 0)
		nLineCount ++;

	return nLineCount;
}

int FormatHex::GetHexDataColumn()
{
	return	10 +					// memory addres size
			m_nHexLineLength * 3 +	// space taken by hex dump
			m_nHexLineLength/4;		// one divider space for each 4 chars dumped
}

int FormatHex::GetLineSize(UINT64 nLine)
{
	//NOTE: in hex mode all lines have equal length 
	//		('\n' is not used as line delimiter)
	return GetHexDataColumn() + m_nHexLineLength;	//TOFIX test
}

UINT64 FormatHex::GetLineOffset(UINT64 nLine)
{
	//TOFIX ASSERT()
	return m_nHexLineLength * nLine;
}

CMarker FormatHex::OffsetToScreen(INT64 nOffset)
{
	CMarker res;
	res.line	 = nOffset / m_nHexLineLength;
	res.column	 = GetHexDataColumn() + (nOffset % m_nHexLineLength);
	return res;
}

INT64 FormatHex::ScreenToOffset(const CMarker &pos)
{
	//ASSERT(NULL != m_pData);

	INT64 nOffset = pos.line * m_nHexLineLength + pos.column - GetHexDataColumn();
	if(nOffset > (INT64)m_pData->m_nFileSize)
		nOffset = -1;

	return nOffset;
}

bool FormatHex::GetLineRaw(UINT64 nLine, LPBYTE &pBuffer, int &nLineSize)
{
	//ASSERT(NULL != m_pData);

	UINT64 nOffset = GetLineOffset(nLine);
	if(nOffset < m_pData->m_nFileSize)
	{
		//TOFIX only if line not inside current buffer
		m_pData->FillBuffer(nOffset);
		pBuffer = m_pData->m_szBuffer;
		nLineSize = min(m_nHexLineLength, m_pData->m_nFileSize - nOffset);
		return true;
	}

	return false;
}

bool FormatHex::GetLineFormated(UINT64 nLine, LPBYTE &pszLine, int &nLineSize)
{
	//ASSERT(NULL != m_pData);

	LPBYTE pBuffer;
	if(!GetLineRaw(nLine, pBuffer, nLineSize))
	{
		//TRACE("No text for line %d\n", nLine);
		return false;
	}

	HexFormatData(pBuffer, nLineSize, nLineSize);
	pszLine		= (LPBYTE)(m_pData->m_szLineFmtBuf);
	return true;
}

//TOFIX format directly into the m_szLineBuf
void FormatHex::HexFormatData(LPBYTE pBuffer, int nLen, int &nNewLen) 
{
	//ASSERT(NULL != m_pData);
		
	unsigned int uOffset = m_pData->m_nFileOffset + (pBuffer - m_pData->m_szBuffer);

	// How many bytes will be in the next line?
	unsigned int uBytesInThisLine = nLen;
	if ( uBytesInThisLine > m_nHexLineLength )
		uBytesInThisLine = m_nHexLineLength;

	// First part of the line - the starting offset.
	String sWork;
	sWork.Printf ("%08X  ", uOffset );
	strcpy(m_pData->m_szLineFmtBuf, sWork);

	// Now loop through the data and add on the hex value of each byte.
	unsigned int uLineOffset;
	for (uLineOffset = 0; uLineOffset < uBytesInThisLine; uLineOffset++ )
	{
		sWork.Printf("%02X ", pBuffer[uLineOffset]);
		strcat(m_pData->m_szLineFmtBuf, sWork);

		// insert column spaces at 3,7,11,15,... line offsets
		if ( 3 == (uLineOffset%4))	//TOFIX uLineOffset -> nColumn
			strcat(m_pData->m_szLineFmtBuf, " ");
	}

	// If there were less than 8 bytes in this line, pad the line with
	// spaces so the ASCII representation will be in the right column.
	if ( uBytesInThisLine < m_nHexLineLength )
	{
		//TOFIX algoritam
		//16 -> 62
		int nDiff = m_nHexLineLength - uBytesInThisLine;
		int nMax = 3 * (nDiff) + nDiff/4 + 1;
		for(int i=0; i<nMax; i++)
			strcat(m_pData->m_szLineFmtBuf, " ");
	}

	//NOTE: until now all data was printable chars (classical string)
	nNewLen = strlen(m_pData->m_szLineFmtBuf);

	// Add on the ASCII representation of the line content
	for ( uLineOffset = 0; uLineOffset < uBytesInThisLine; uLineOffset++ )
	{
		m_pData->m_szLineFmtBuf[nNewLen] = (unsigned char) pBuffer[uLineOffset];
		nNewLen ++;

		// If the next byte isn't printable, show a period instead.
		//if ( _istprint ( pbyData[uOffset + uLineOffset] ))
		//	sLine += (TCHAR) pbyData[uOffset + uLineOffset];
		//else
		//	sLine += '.';
	}
}
