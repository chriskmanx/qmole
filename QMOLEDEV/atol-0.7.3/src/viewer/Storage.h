////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: For file viewer, handles file I/O and returns formatted lines (txt,bin,hex)
////////////////////////////////////////////////////////////////////////////

#ifndef STORAGE_H__
#define STORAGE_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../core/File64.h"
#include "../core/String.h"
#include "Selection.h"

#include "Format.h"
#include "FormatBin.h"
#include "FormatTxt.h"
#include "FormatHex.h"
//#include "FormatUnicode.h"
#include "../core/Thread.h"

#define READ_CHUNK_SIZE      8*1024

//TOFIX??? enums
#define DRAW_TEXT		0
#define DRAW_BIN		1
#define DRAW_HEX		2
#define DRAW_UNICODE	3

enum CRLFSTYLE
{
	CRLF_STYLE_AUTOMATIC	= -1,
	CRLF_STYLE_DOS			= 0,
	CRLF_STYLE_UNIX			= 1,
	CRLF_STYLE_MAC			= 2,
	CRLF_STYLE_BIN			= 3		//no line termination
};

class LineCountThread;

class CStorage
{
public:
	CStorage();
	virtual ~CStorage();

	bool Open(const char *szPath);
	void Close();
	bool IsOpen();

	bool SetDrawMode(int nMode);
	bool FillBuffer(UINT64 nOffset);

	int  GetLineCount();
	bool GetLineFormated(int nLine, LPBYTE &pszLine, int &nSize);
	int  Find(const char *szFile, int nLen, int nStartFrom, bool bForward = true);

	int	m_nDrawMode;	//TOFIX remove ?
	String m_strFilePath;

	//"buffer" variables
	bool	m_bBinaryFile;
	bool	m_bUnicodeText;
	File64	m_File;
	unsigned char m_szBuffer[READ_CHUNK_SIZE];
	char m_szLineBuf[255]; //TOFIX remove?
	char m_szLineFmtBuf[775]; //TOFIX remove?
	UINT64	m_nFileSize;
	UINT64	m_nFileOffset;	//file pos
	unsigned int m_nBuffSize;

	INT64 FileOffsetFromScreenPos(const CMarker &pos);
	CMarker ScreenPosFromFileOffset(INT64 nOffset);

	int  GetViewMode();
	bool IsHexMode();
	bool IsBinMode();
	bool IsTextMode();
	bool IsUnicodeMode();
	
	void CheckDataType();

	Format			*m_pFmt;
	FormatTxt		m_fmtTxt;
	FormatBin		m_fmtBin;
	FormatHex		m_fmtHex;
//	FormatUnicode	m_fmtUnicode;
	LineCountThread	*m_pThread;

//protected:	
	bool GetLineRaw(int nLine, LPBYTE &pBuffer, int &nLineSize);
};

#endif // STORAGE_H__
