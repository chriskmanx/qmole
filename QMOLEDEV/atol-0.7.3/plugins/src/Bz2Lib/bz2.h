////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file packing/unpacking using BZip algorithm (.bz2 archives)
////////////////////////////////////////////////////////////////////////////

/*
 BZClass
 Copyright (c) 2003 by Gilad Novik (gilad@bmidas.com)

 You are allowed to include the source code in any product (commercial, shareware, freeware or otherwise).
 You are allowed to modify the source code in any way you want.
 You can use this code without mention my name in your product, but it would be nice if you DO mention me.
*/
//////////////////////////////////////////////////////////////////////

#ifndef BZ2_H_
#define BZ2_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../plugin_defs.h"

#define BZ_NO_STDIO
#include "bzip2/bzlib.h"
#include <string>

#ifdef _WIN32
 #include <windows.h>
#endif

class CBZ2
{
public:
	CBZ2();
	virtual ~CBZ2(){};

	bool Open(const char *szFile, bool bRead = true);
	void Close();

	bool CompressFile(const char *szInFile, const char *szArchive);
	bool DecompressFile(const char *szArchive, const char *szOutFile);
	int  GetUnpackSize(const char *szArchive);

	//progress support
	tProcessDataProc m_pfnProgress;
	std::string m_strEntry;
	long m_dwUserData;

protected:
	bz_stream m_Stream;
	char* m_pWriteBuffer;
	unsigned int m_nWriteBufferSize;

	//TOFIX it doesn't work right when buffer has 2k size ? (is it related to nBlockSize?)
	char m_szReadBuffer[16384];
	char m_szWriteBuffer[16384];
	FILE *m_pArchive;
	FILE *m_pFile;

	std::string m_strFile;

	unsigned int Compress(int nBlockSize=9,int nWorkFactor=30);
	unsigned int Decompress(int nSmall=false);

	virtual unsigned int OnCompressRead(char* &pBuffer);
	virtual void OnCompressWrite(const char* pBuffer,unsigned int nLength);
	virtual unsigned int OnDecompressRead(char* &pBuffer);
	virtual void OnDecompressWrite(const char* pBuffer,unsigned int nLength);
};

#endif // BZ2_H_
