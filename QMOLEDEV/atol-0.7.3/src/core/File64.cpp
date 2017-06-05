////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file operations with large file support (64-bit size)
////////////////////////////////////////////////////////////////////////////

#include "File64.h"

#ifdef _WIN32
#else
 #include <sys/types.h>
 #include <sys/stat.h>
 #include <fcntl.h>
 #include <unistd.h>
#endif

File64::File64()
{
#ifdef _WIN32
	m_hFile = INVALID_HANDLE_VALUE;
#else
	m_nFile	= -1;
#endif
}

File64::~File64()
{
	Close();
}

bool File64::Open(const char *szPath, unsigned long nFlags)
{
#ifdef _WIN32
	DWORD dwDesiredAccess = 0;
	if(nFlags & F64_READ)
		dwDesiredAccess |= GENERIC_READ;
	if(nFlags & F64_WRITE)
		dwDesiredAccess |= GENERIC_WRITE;

	DWORD dwShareMode = 0;
	if(nFlags & F64_SHARE_READ)
		dwShareMode |= FILE_SHARE_READ;
	if(nFlags & F64_SHARE_WRITE)
		dwShareMode |= FILE_SHARE_WRITE;

	DWORD dwCreationDistribution = 0;
	if(nFlags & F64_OPEN_NEW)
		dwCreationDistribution |= CREATE_ALWAYS;
	if(nFlags & F64_OPEN_EXISTING)
		dwCreationDistribution |= OPEN_EXISTING;
	if((nFlags & (F64_OPEN_NEW|F64_OPEN_EXISTING)) == (F64_OPEN_NEW|F64_OPEN_EXISTING))
		dwCreationDistribution = OPEN_ALWAYS;

	HANDLE hFile = ::CreateFile(szPath,	
								dwDesiredAccess,
								dwShareMode,
								NULL,	//lpSecurityAttributes,
								dwCreationDistribution,	
								0,		//dwFlagsAndAttributes,
								NULL);	//hTemplateFile);
	
	bool bSuccess = false;
	if (hFile != INVALID_HANDLE_VALUE)
	{
		m_strFile = szPath;
		bSuccess = true;
		m_hFile = hFile;
	}
	
	return bSuccess;
#else
	int nOpenFlags = 0;
	if(nFlags & F64_READ)
		nOpenFlags |= O_RDONLY;
	if(nFlags & F64_WRITE)
		nOpenFlags |= O_WRONLY;
	if((nFlags & (F64_READ|F64_WRITE)) == (F64_READ|F64_WRITE))
		nOpenFlags = O_RDWR;
	if(nFlags & F64_OPEN_NEW)
		nOpenFlags |= O_CREAT;
	if((nFlags & (F64_OPEN_NEW|F64_OPEN_EXISTING)) == (F64_OPEN_NEW|F64_OPEN_EXISTING))
		nOpenFlags |= O_TRUNC;

	nOpenFlags |= O_LARGEFILE;
	
	m_nFile	= open(szPath, nOpenFlags);
	if (m_nFile != -1)
		m_strFile = szPath;

	return (m_nFile != -1);
#endif
}

void File64::Close()
{
	//ASSERT(IsOpen());
#ifdef _WIN32
	if (IsOpen())
		::CloseHandle(m_hFile);
	m_hFile = INVALID_HANDLE_VALUE;
#else
	if(IsOpen())
		close(m_nFile);
	m_nFile = -1;
#endif
}

bool File64::IsOpen() const
{
#ifdef _WIN32
	return (m_hFile != INVALID_HANDLE_VALUE); 
#else
	return (m_nFile != -1);
#endif
}

void File64::Flush()
{
	//ASSERT(IsOpen());
#ifdef _WIN32	
	::FlushFileBuffers(m_hFile);
#else
	//TOFIX	
#endif
}

int	 File64::Read(char *szBuffer, int nLen)
{
	//ASSERT(IsOpen());
#ifdef _WIN32
	DWORD dwRead;
	if (!::ReadFile(m_hFile, (void *)szBuffer, nLen, &dwRead, NULL))
		return -1;

	return dwRead;
#else
	return read(m_nFile, (void *)szBuffer, nLen);
#endif
}

int	File64::Write(const char *szBuffer, int nLen)
{
	//ASSERT(IsOpen());
#ifdef _WIN32
	unsigned long nWritten;
	if (!::WriteFile(m_hFile, (void *)szBuffer, nLen, &nWritten, NULL))
		return -1;

	if (nWritten != (unsigned long)nLen)
		return -1;

	return nWritten;
#else
	return write(m_nFile, szBuffer, nLen);
#endif
}

INT64 File64::Seek(INT64 offset)
{
	//ASSERT(IsOpen());
#ifdef _WIN32
	LARGE_INTEGER li;
	li.QuadPart = offset;
	li.LowPart  = ::SetFilePointer(m_hFile, li.LowPart, &li.HighPart, FILE_BEGIN);

	DWORD dw = GetLastError();
	if ((li.LowPart == 0xFFFFFFFF) && (dw != NO_ERROR))
		return -1;

	return li.QuadPart;
#else
	return lseek(m_nFile, offset, SEEK_SET);
#endif
}

INT64 File64::GetPosition() const
{
	//ASSERT(IsOpen());
#ifdef _WIN32
	LARGE_INTEGER li;

	li.HighPart = 0;
	li.LowPart  = ::SetFilePointer(m_hFile, 0, (LONG*)&li.HighPart, FILE_CURRENT);
	if (li.LowPart  == 0xFFFFFFFF)
		return -1;

	return li.QuadPart;
#else
	//TOFIX	
	return 1;
#endif
}

INT64 File64::GetSize()
{
#ifdef _WIN32
	LARGE_INTEGER li;

	li.HighPart = 0;
	li.LowPart  = ::GetFileSize(m_hFile, (LPDWORD)&li.HighPart);
	if (li.LowPart  == 0xFFFFFFFF)
		return -1;

	return li.QuadPart;
#else
	//TOFIX	
	INT64 nSize = -1;
	struct stat64 st;
	if(0 == stat64(m_strFile.c_str(), &st))
		return st.st_size;

	return nSize;
#endif
}
