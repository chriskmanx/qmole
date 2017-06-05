////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file operations with large file support (64-bit size)
////////////////////////////////////////////////////////////////////////////

#ifndef FILE64_H__
#define FILE64_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

//
// supports 64.bit file size (on Windows files can be larger than 2GB only on NTFS)
//

#include <string>
#include <string.h>
#include "types.h"	//INT64

#define F64_READ			0x0001
#define F64_WRITE			0x0002
#define F64_SHARE_READ		0x0004
#define F64_SHARE_WRITE		0x0008
#define F64_OPEN_NEW		0x0010
#define F64_OPEN_EXISTING	0x0020

class File64  
{
public:
	File64();
	virtual ~File64();

	bool Open(const char *szPath, unsigned long nFlags = F64_READ|F64_SHARE_READ|F64_OPEN_EXISTING);
	void Close();

	bool IsOpen() const;

	void Flush();
	virtual int	 Read(char *szBuffer, int nLen);
	virtual int	 Write(const char *szBuffer, int nLen);

	int  WriteString(const char *szBuffer){ return Write(szBuffer, strlen(szBuffer)); };

	INT64 GetPosition() const;
	INT64 Seek(INT64 offset);
	INT64 GetSize();

protected:
#ifdef _WIN32
	HANDLE  m_hFile;
#else
	int m_nFile;	
#endif
	std::string m_strFile;
};

#endif // FILE64_H__
