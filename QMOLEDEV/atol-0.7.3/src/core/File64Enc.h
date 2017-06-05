////////////////////////////////////////////////////////////////////////////
// NoteCase notes manager project <http://notecase.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Overrides large file access class to add reading/writing Blowfish encrypted data
////////////////////////////////////////////////////////////////////////////

#ifndef FILE64ENC_H__
#define FILE64ENC_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "File64.h"
#include "./_crypt/blowfish.h"

#define ENC_BUFFER_SIZE	1024

class File64Enc : public File64  
{
public:
	File64Enc();
	virtual ~File64Enc();

	void SetPassword(const char *szPass);
	virtual int	 Read(char *szBuffer, int nLen);
	virtual int	 Write(const char *szBuffer, int nLen);
	int WriteFinal();

protected:
	int  WriteBuffer();
	
protected:
	CBlowFish m_crypt;
	char m_szBuffer[ENC_BUFFER_SIZE];
	int  m_nDataSize;
};

#endif // FILE64ENC_H__
