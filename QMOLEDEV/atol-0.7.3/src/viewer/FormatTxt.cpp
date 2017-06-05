////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to help formating file (for file viewer) as a text file
////////////////////////////////////////////////////////////////////////////

#include "FormatTxt.h"
#include "Storage.h"
#include "../core/debug.h"

#ifndef min
 #define min(a,b) ((a)<(b))?(a):(b)
#endif

#define MAX_TEXT_LINE	256
const char *my_memchrs(const char *buf, const char *chrs, size_t chrLen, size_t count);

int memrmvchr(int nChar, char *szBuf, int nLen);
int memfind(unsigned char *szBuf, int nLen, const unsigned char *szPtr, int nPtrLen);
void memrmv(int nCount, char *szBuf, int nLen);
void memins(int nChar, char *szBuf, int nLen);

FormatTxt::FormatTxt()
{
	m_nTabWidth		= 4;	// how many spaces is tab "worth"
	m_nWrapLength	= 0;
	m_bWrap			= false;
	m_bAbortThread  = false;
}

FormatTxt::~FormatTxt()
{
	m_lstLineOffsets.clear();
	m_lstRealOffsets.clear();
	m_lstWrapOffsets.clear();
}

UINT64 FormatTxt::GetLineCount()
{
	//TOFIX cache size() everywhere to speed-up
	return (m_bWrap)?  m_lstWrapOffsets.size() : m_lstLineOffsets.size();
}

int FormatTxt::GetLineSize(UINT64 nLine)
{
	//ASSERT(NULL != m_pData);
	//ASSERT(nLine < GetLineCount());

	int nSize = 0;
	UINT64 nStart = GetLineOffset(nLine);
	if(nLine+1 < GetLineCount())
		nSize = GetLineOffset(nLine+1) - nStart;
	else
		nSize = m_pData->m_nFileSize - nStart;

	//TOFIX ovo nije dobro jer ako skreemo veli�nu jedne linije ono iza nje � ostati nevidljivo
	//ASSERT(nSize < READ_CHUNK_SIZE);
	return min(READ_CHUNK_SIZE, nSize); 
}

UINT64 FormatTxt::GetLineOffset(UINT64 nLine)
{
	//ASSERT(nLine < GetLineCount());

	if(m_bWrap)
		return m_lstWrapOffsets[nLine];
	else
		return m_lstLineOffsets[nLine];
}

CMarker FormatTxt::OffsetToScreen(INT64 nOffset)
{
	//ASSERT(NULL != m_pData);
	//ASSERT(0 <= nOffset && nOffset <= m_pData->m_nFileSize);
	CMarker res;

	//TOFIX
	unsigned int nLine = 0;
	int nLineSize = GetLineSize(0);
	while(nLineSize < nOffset && nOffset > 0 && nLine+1 < GetLineCount()){
		nOffset	  -= nLineSize;
		nLineSize  = GetLineSize(nLine+1);
		nLine ++;
	}
	res.line	 = nLine;
	res.column	 = nOffset;

	return res;
}

INT64 FormatTxt::ScreenToOffset(const CMarker &pos)
{
	INT64 nOffset = 0;

	if((unsigned int)pos.line < GetLineCount())
	{
		if(m_bWrap)
			nOffset = m_lstWrapOffsets[pos.line];	
		else
			nOffset = m_lstLineOffsets[pos.line];
		
		nOffset += pos.column;

		//fix the file offset by taking into account that 1 tab character takes 4 columns
		/* TOFIX
		LPBYTE pBuffer;
		int nLineSize;
		if(GetLineRaw(pos.line, pBuffer, nLineSize))
		{
			int nMin = min(pos.column, nLineSize);
			for(int i=0; i<nMin; i++)
			{
				if(pBuffer[i] == '\t')
					nOffset -= m_nTabWidth-1;
			}
		}
		*/
	}
	else
		nOffset = -1;
	return nOffset;
}

//TOFIX ako �vam real offsets kako brojati tabove !!!
void FormatTxt::DoWrap(int nLen)
{
	//ASSERT(NULL != m_pData);
	//ASSERT(TRUE == m_bWrap);
	//TRACE("Wrapping len = %d\n", nLen);

	if(m_bWrap && nLen > 0)
	{
		UINT64 nCurRelOffset, nNextRealOffset;

		m_lstWrapOffsets.clear();

		//rebuild wrap offsets
		m_nWrapLength = nLen;
		nCurRelOffset = 0;	//1st line starts from 0 to m_lstRealOffsets[i]

		for(unsigned int i=0; i<=m_lstRealOffsets.size(); i++)
		{
			//init offsets
			if(i < m_lstRealOffsets.size())
				nNextRealOffset = m_lstRealOffsets[i];
			else
				nNextRealOffset = m_pData->m_nFileSize;
			
			int nRealLineLength = nNextRealOffset - nCurRelOffset;
			//TRACE("Original line #%d: (%d,%d) length = %d\n", i, nCurRelOffset, nNextRealOffset, nRealLineLength);

			//break single line until line too big
			while(nRealLineLength > nLen)
			{
				m_lstWrapOffsets.push_back(nCurRelOffset);
				//TRACE("Wrapped line     : (%d,%d) length = %d\n", nCurRelOffset, nCurRelOffset+nLen, nLen);

				nCurRelOffset += nLen;
				nRealLineLength = nNextRealOffset - nCurRelOffset;

				if(nCurRelOffset > m_pData->m_nFileSize)
					return;
			}

			if(nCurRelOffset != nNextRealOffset)
			{
				m_lstWrapOffsets.push_back(nCurRelOffset);
				//TRACE("Wrapped line     : (%d,%d) length = %d\n", nCurRelOffset, nNextRealOffset, nNextRealOffset - nCurRelOffset);
			}

			nCurRelOffset = nNextRealOffset;
		}
	}
}

void FormatTxt::ExpandTabs(LPBYTE pBuffer, unsigned int nLen, int &nNewLen) 
{
	ASSERT(NULL != m_pData);

	if(nLen < 1)
		return;

	nNewLen = min(nLen, sizeof(m_pData->m_szLineFmtBuf)-1);
	memcpy(m_pData->m_szLineFmtBuf, pBuffer, nNewLen);
	m_pData->m_szLineFmtBuf[nNewLen] = '\0';

	//cut line in place of first '\r' or '\n' character
	char *pszEnd = (char *)my_memchrs(m_pData->m_szLineFmtBuf, "\n\r", 2, nNewLen);
	if(NULL != pszEnd){
		int nLen = pszEnd - m_pData->m_szLineFmtBuf;
		//ASSERT(nNewLen - nLen <= 2); //chars being cut should be found at the end of line
		nNewLen = nLen;
		*pszEnd = '\0';
	}

	int nTabCount = 0;
	for(int i=0; i<nNewLen; i++)
	{
		if(pBuffer[i] == '\t')
			nTabCount++;
	}

	//replace tabs with spaces
	int nOffset = 0;
	char *pszPos = m_pData->m_szLineFmtBuf;
	while(NULL != (pszPos = (char *)memchr(pszPos, '\t', sizeof(m_pData->m_szLineFmtBuf)-nOffset)))
	{
		nOffset = pszPos - m_pData->m_szLineFmtBuf;
		int nSpaces = m_nTabWidth - (nOffset % m_nTabWidth);

		//erase '\t' character
		memrmv(1, pszPos, sizeof(m_pData->m_szLineFmtBuf)-nOffset);
		//insert spaces
		for(int i=0; i<nSpaces; i++)
			memins(' ', pszPos, sizeof(m_pData->m_szLineFmtBuf)-nOffset);

		nNewLen += nSpaces-1;
	}
}


bool FormatTxt::GetLineRaw(UINT64 nLine, LPBYTE &pBuffer, int &nLineSize)
{
	//ASSERT(NULL != m_pData);

	UINT64 nOffset = 0;
	if(nLine < GetLineCount())
	{
		nOffset	= GetLineOffset(nLine);
		
		//TOFIX only if line not inside current buffer
		m_pData->FillBuffer(nOffset);
		pBuffer   = m_pData->m_szBuffer;

		nLineSize = GetLineSize(nLine);
		return true;
	}
	return false;
}

bool FormatTxt::GetLineFormated(UINT64 nLine, LPBYTE &pszLine, int &nLineSize)
{
	//ASSERT(NULL != m_pData);

	LPBYTE pBuffer;
	if(!GetLineRaw(nLine, pBuffer, nLineSize))
	{
		//TRACE("No text for line %d\n", nLine);
		return false;
	}

	//
	ExpandTabs(pBuffer, nLineSize, nLineSize);
	pszLine		= (LPBYTE)(m_pData->m_szLineFmtBuf);
	return true;
}

void FormatTxt::TextLineCounter()
{
	//ASSERT(NULL != m_pData);
	
	m_lstLineOffsets.clear();
	m_lstLineOffsets.push_back(0);	//first line offset

	if(m_pData->m_nFileSize > 0)
	{
		//initialise file search
		m_pData->m_nFileOffset = -READ_CHUNK_SIZE;

		while(m_pData->FillBuffer(m_pData->m_nFileOffset + READ_CHUNK_SIZE))
		{
			if(m_bAbortThread)
				return;

			//TRACE("Lister: reading file at offset %I64u/%I64u succeeded\n", m_pData->m_nFileOffset, m_pData->m_nFileSize);
			ParseLinesFromBuffer();
		}

		//no line is allowed to be longer than given limit (256 chars)
		BreakLines(m_pData->m_nFileSize);
		//TRACE("Lister: Total text file lines: %d\n", m_lstLineOffsets.size());
	}
}

void FormatTxt::BreakLines(UINT64 nOffset)
{
	while(1)
	{
		if(m_bAbortThread)
			return;

		int nLastIdx = m_lstLineOffsets.size()-1;
		UINT64 nPrevLine = m_lstLineOffsets[nLastIdx];
		if(nOffset > nPrevLine+MAX_TEXT_LINE)
		{
			//TRACE("Lister: New line(1) at: %d\n", nPrevLine+MAX_TEXT_LINE);
			m_lstLineOffsets.push_back(nPrevLine+MAX_TEXT_LINE);
		}
		else
			break;
	}
}

//TOFIX separate modul
//find first of chars in a set inside given buffer
const char *my_memchrs(const char *buf, const char *chrs, size_t chrLen, size_t count)
{
	for(unsigned int i=0; i<count; i++)
		for(unsigned int j=0; j<chrLen; j++)
			if(buf[i] == chrs[j])
				return buf+i;
	return NULL;
}

//TOFIX testirati
//remove all instances of a character in buffer
int memrmvchr(int nChar, char *szBuf, int nLen)
{
	char *pszPos = szBuf;
	int	nSize = nLen; 
	int	nCount = 0;

	while(NULL != (pszPos = (char *)memchr(pszPos, nChar, nSize)))
	{
		nSize = nLen - (pszPos - szBuf) - nCount;
		if(nSize<1)
			break;
		memmove(pszPos, pszPos+1, nSize-1);
		nCount ++;
		pszPos ++;
	}

	return nCount;
}

//remove n chars in the begining of the buffer
void memrmv(int nCount, char *szBuf, int nLen)
{
	memmove(szBuf, szBuf+nCount, nLen-nCount);
}

//insert character at the begining of the buffer
void memins(int nChar, char *szBuf, int nLen)
{
	memmove(szBuf+1, szBuf, nLen);
	szBuf[0] = nChar;
}

int memfind(unsigned char *szBuf, int nLen, 
			   const unsigned char *szPtr, int nPtrLen)
{
	int nPos = 0;
	while(nPos <= nLen-nPtrLen)
	{
		if(0 == memcmp(szBuf+nPos, szPtr, nPtrLen))
			return nPos;
		nPos ++;
	}
	return -1;
}

/*
int memreplace(unsigned char *szBuf, int nLen, int nMaxLen,
			   const unsigned char *szPtr, int nPtrLen,
			   const unsigned char *szDst, int nDstLen)
{
	//memfind, 	memmove, memins
	int findLen		= strlen(find);
	int replaceLen	= (replace)? strlen(replace) : 0;
	
	const char *strLast	= string + strlen(string);
	
	while( offset && *offset && (offset < strLast) )
	{
		//int memfind(unsigned char *szBuf, int nLen, const unsigned char *szPtr, int nPtrLen)

		//offset = stricstr(offset, find);	// case insensitive
		offset = strstr(offset, find);	// case sensitive
		
		if(offset)
		{
			// 1. kill FIND string
			// 2. make space for REPLACE string
			// 3. insert REPLACE string
			memmove(offset, offset + findLen, strlen(offset + findLen) + 1 );
			
			//TOFIX if(replace == NULL)
			memmove(offset + replaceLen, offset, strlen(offset) + 1); 
			memcpy(offset, replace, replaceLen);
			
			offset += replaceLen;	// get next place to search from
		}
		else
			break;
	}
	
}
*/

void FormatTxt::ParseLinesFromBuffer()
{
	UINT64 nOffset = 0, nBufferOffset = 0;

	unsigned char *pszPos = m_pData->m_szBuffer;

	while(nBufferOffset < m_pData->m_nBuffSize)
	{
		//find first of these chars
		pszPos = (unsigned char *)my_memchrs((const char *)pszPos, "\n\r", 2, m_pData->m_nBuffSize - nBufferOffset);
		if(NULL == pszPos)
			break;

		//handle CR,LF pairs (point to other char)
		if(*pszPos == '\r' && *(pszPos+1) == '\n')
			pszPos++;
			
		pszPos++;	//point to start of new line 

		nBufferOffset = pszPos - m_pData->m_szBuffer;
		nOffset		  = m_pData->m_nFileOffset + nBufferOffset;
		if(nOffset >= m_pData->m_nFileSize)
			break;

		//ensure that line cannot be longer than MAX_TEXT_LINE (break line into more pieces)
		BreakLines(nOffset);

		//TRACE("Lister: New line at: %d\n", nOffset);
		m_lstLineOffsets.push_back(nOffset);
		m_lstRealOffsets.push_back(nOffset);
	}
}

