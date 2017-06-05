////////////////////////////////////////////////////////////////////////////
// NoteCase notes manager project <http://notecase.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Overrides large file access class to add reading/writing Blowfish encrypted data
////////////////////////////////////////////////////////////////////////////

#include "File64Enc.h"

#ifndef min
 #define min(x,y) (((x)<(y))?(x):(y))
#endif

File64Enc::File64Enc()
{
	m_nDataSize = 0;
}

File64Enc::~File64Enc()
{

}

void File64Enc::SetPassword(const char *szPass)
{
	m_crypt.Initialize((BYTE *)szPass, strlen(szPass));
}

int File64Enc::Read(char *szBuffer, int nLen)
{
	int nRead = File64::Read(szBuffer, nLen);
	m_crypt.Decode((BYTE *)szBuffer, (BYTE *)szBuffer, nRead);
	szBuffer[nRead] = '\0';
	return nRead;
}

int File64Enc::Write(const char *szBuffer, int nLen)
{
	int nPos = 0;
	int nTotalSize = 0;
	int nAvailSpace = ENC_BUFFER_SIZE-m_nDataSize;
	while(nLen > 0)
	{
		//prepare new buffer (append new data to its end)
		int nAdd = min(nLen, nAvailSpace);
		memcpy(m_szBuffer+m_nDataSize, szBuffer+nPos, nAdd);

		//refresh variables
		m_nDataSize += nAdd;
		nLen -= nAdd;
		nPos += nAdd;
		nAvailSpace = ENC_BUFFER_SIZE-m_nDataSize;

		//flush data if buffer full
		if(0 == nAvailSpace)
			nTotalSize += WriteBuffer();
	}

	return nTotalSize;
}

int File64Enc::WriteFinal()
{
	//write anything left in the buffer
	return WriteBuffer();
}

int File64Enc::WriteBuffer()
{
	//encode and write buffer into the file
	if(m_nDataSize > 0)
	{
		int nOrigSize = m_nDataSize;
		int nEncSize  = m_crypt.Encode((BYTE *)m_szBuffer, (BYTE *)m_szBuffer, nOrigSize);
		File64::Write(m_szBuffer, nEncSize);
		m_nDataSize = 0;
		return nOrigSize;
	}
	return 0;
}
	
