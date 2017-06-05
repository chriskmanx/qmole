////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Class to read/write .gz archive files (uses zlib library to do the job)
////////////////////////////////////////////////////////////////////////////

/* zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.4, March 11th, 2002

  Copyright (C) 1995-2002 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly jloup@gzip.org
  Mark Adler madler@alumni.caltech.edu

*/

#include <stdlib.h>
#include <stdio.h>
#include "zlib/zlib.h"
#include "gzip.h"
#include <limits.h>
#include <time.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

const char *CGZip::m_stsVersion= ZLIB_VERSION;

CGZip::CGZip()
: m_gzf(0),
m_eArchiveMode(ArchiveModeClosed),
m_eCompressionMode(CompressionModeDefault),
m_eStrategy(StrategyDefault)
{

}

CGZip::~CGZip()
{
	if (IsOpen())
		Close();
}

bool CGZip::Open(const char *szFileName, EArchiveMode eArchiveMode)
{
	if (IsOpen())
		return false;

	if (eArchiveMode == ArchiveModeWrite)
	{
		m_gzf = gzopen(szFileName,"wb");
		UpdateParams();
	}
	else if (eArchiveMode == ArchiveModeRead)
	{
		m_gzf = gzopen(szFileName,"rb");
	}

	if (m_gzf != 0)
		m_eArchiveMode=eArchiveMode;
	else
		m_eArchiveMode=ArchiveModeClosed;

	if(m_gzf != 0)
		m_strArchivePath = szFileName;	//remember archive name

	return m_gzf != 0;
}
	
bool CGZip::Close()
{
	if (!IsOpen())
		return false;

	int result = gzclose(m_gzf);
	m_gzf=0;

	m_strArchivePath = "";	//forget archive name

	return result == 0;
};

void CGZip::Rewind()
{
	if (!IsOpen())
		return;

	gzrewind(m_gzf);
}

bool CGZip::WriteBuffer(void* pBuffer, int nBytes)
{
	if (!IsOpen() || !IsWriting())
		return false;

	int written=gzwrite(m_gzf, pBuffer, nBytes);

	return written == nBytes;
};

bool CGZip::Flush( EFlushMode eFlush)
{
	if (!IsOpen() || !IsWriting())
		return false;

	return gzflush(m_gzf, eFlush)==Z_OK;
}
	
bool CGZip::WriteString( const char *str)
{
	return WriteBuffer( (void*)str, strlen(str));
};

int CGZip::ReadBufferSize(void*  pBuffer, int nBytes)
{
	if (!IsOpen() || !IsReading())
		return false;

	return gzread(m_gzf, pBuffer, nBytes);
}

void CGZip::UpdateParams()
{
	if (!IsOpen() || !IsWriting())
		return;

	gzsetparams( m_gzf, m_eCompressionMode, m_eStrategy);
};

bool CGZip::IsEOF() const
{
	if (!IsOpen())
		return true;

	return gzeof(m_gzf)==1;
}

//calculate size of uncompressed file
long  CGZip::GetUncompressSize()
{
	if (!IsOpen() || !IsReading())
		return 0;

	//store current file offset
	long nCurOffset = gztell(m_gzf);

	//seek to the end of the file, and read its offset
	gzseek(m_gzf, LONG_MAX, SEEK_SET);
	long nMaxOffset = gztell(m_gzf);

	//restore file offset
	gzseek(m_gzf, nCurOffset, SEEK_SET);

	return nMaxOffset;
}

typedef struct gz_stream {
    z_stream stream;
    int      z_err;   /* error code for last stream operation */
    int      z_eof;   /* set if end of input file */
    FILE     *file;   /* .gz file */
    Byte     *inbuf;  /* input buffer */
    Byte     *outbuf; /* output buffer */
    uLong    crc;     /* crc32 of uncompressed data */
    char     *msg;    /* error message */
    char     *path;   /* path name for debugging only */
    int      transparent; /* 1 if input file is not a .gz file */
    char     mode;    /* 'w' or 'r' */
    long     startpos; /* start of compressed data in file (header skipped) */
} gz_stream;

//get compressed file size
long  CGZip::GetCompressSize()
{
	if (!IsOpen() || !IsReading())
		return 0;
	
	FILE *pFile = ((gz_stream *)m_gzf)->file;

	//store current file offset
	long nCurOffset = ftell(pFile);

	//seek to the end of the file, and read its offset
	fseek(pFile, 0, SEEK_END);
	long nMaxOffset = ftell(pFile);

	//restore file offset
	fseek(pFile, nCurOffset, SEEK_SET);

	return nMaxOffset;
}

FILE *CGZip::GetFileStream()
{
	if (!IsOpen() || !IsReading())
		return 0;
	
	return ((gz_stream *)m_gzf)->file;
}

bool CGZip::GetFileDate(time_t &timeDest)
{
	if (!IsOpen() || !IsReading())
		return false;

	FILE *pFile = ((gz_stream *)m_gzf)->file;

	//store current file offset
	long nCurOffset = ftell(pFile);

	//skip first four bytes (2 bytes magic, 1 byte method, 1 byte flags)
	fseek(pFile, 4, SEEK_SET);

    //bytes number 5-8 are modified time
	fread(&timeDest, sizeof(timeDest), 1, pFile);

	//restore file offset
	fseek(pFile, nCurOffset, SEEK_SET);

	return true;
}

#define FEXTRA	(0x01<<2)
#define FNAME	(0x01<<3)

std::string CGZip::GetFileName()
{
	std::string strRes;

	if (!IsOpen() || !IsReading())
		return strRes;

	FILE *pFile = ((gz_stream *)m_gzf)->file;

	//store current file offset
	long nCurOffset = ftell(pFile);

	//skip first 3 bytes (2 bytes magic, 1 byte method)
	fseek(pFile, 3, SEEK_SET);

	unsigned char flags = 0;
	fread(&flags, sizeof(flags), 1, pFile);	//read flags byte

	if(0 != (FNAME & flags))	//original file name saved within the archive
	{
		//skip at the end of fixed part of .gz header
		fseek(pFile, 10, SEEK_SET);

		//skip extra bytes if they exist
		if(0 != (FEXTRA & flags))
		{
			//skip at the end of extra data
			unsigned short xtralen = 0;
			fread(&xtralen, sizeof(xtralen), 1, pFile);	//read flags byte
			fseek(pFile, 12+xtralen, SEEK_SET);
		}

		//read file name from file (read until NULL byte)
		unsigned char nByte = 0;
		fread(&nByte, sizeof(nByte), 1, pFile);	//read byte

		while(nByte != '\0')
		{
			strRes += nByte;
			fread(&nByte, sizeof(nByte), 1, pFile);	//read byte
		}
	}
	else
	{
		//calculate file name using archive name
		//(strip archive path and .gz extension)
		
		std::string strTmp = m_strArchivePath; 
		const char *szPos = strTmp.c_str();
		szPos = strrchr(szPos, '\\');		// find file name start
		if(szPos)
			szPos ++;
		else
			szPos = strTmp.c_str();
		char *szExt = (char *)strrchr(szPos, '.');	// strip .gz/.tgz extension
		if(szExt)
			*szExt = '\0';

		strRes = szPos;
	}

	//restore file offset
	fseek(pFile, nCurOffset, SEEK_SET);

	return strRes;
}
