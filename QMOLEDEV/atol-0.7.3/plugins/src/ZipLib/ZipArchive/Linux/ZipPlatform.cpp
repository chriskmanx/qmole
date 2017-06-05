////////////////////////////////////////////////////////////////////////////////
// This source file is part of the ZipArchive library source distribution and
// is Copyrighted 2000 - 2006 by Tadeusz Dracz (http://www.artpol-software.com/)
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// For the licensing details see the file License.txt
////////////////////////////////////////////////////////////////////////////////


#include "stdafx.h"
#include "ZipPlatform.h"
#include "ZipFileHeader.h"
#include "ZipException.h"
#include "ZipAutoBuffer.h"


#include <utime.h>



#include "ZipPathComponent.h"


#include "ZipCompatibility.h"

#include <sys/types.h>

// uncomment on FreeBSD, Mac and comment sys/vfs.h
//#include <sys/param.h>
//#include <sys/mount.h>
#include <sys/vfs.h>
#include <sys/stat.h>

#include <unistd.h>
#include <stdio.h>

#include <fcntl.h>

const TCHAR CZipPathComponent::m_cSeparator = _T('/');

#ifndef _UTIMBUF_DEFINED
#define _utimbuf utimbuf
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
DWORD ZipPlatform::GetDeviceFreeSpace(LPCTSTR lpszPath)
{
	struct statfs sStats;

	#if defined (__SVR4) && defined (__sun)
		if (statvfs(lpszPath, &sStats) == -1) // Solaris
	#else
		if (statfs(lpszPath, &sStats) == -1)
	#endif
		return 0;

        return sStats.f_bsize * sStats.f_bavail;
}

bool ZipPlatform::GetFileSize(LPCTSTR lpszFileName, DWORD& dSize)
{
	int f = open(lpszFileName, O_RDONLY);
	if (f == -1)
		return false;
	int iSize = lseek(f, 0, SEEK_END);
	close(f);
	if (iSize == -1)
		return false;
	dSize = (DWORD)iSize;
	return true;
}

CZipString ZipPlatform::GetTmpFileName(LPCTSTR lpszPath, DWORD iSizeNeeded)
{
	TCHAR empty[] = _T(""), prefix [] = _T("zar");
	TCHAR* buf = NULL;
	CZipString tempPath = lpszPath;
	if (tempPath.IsEmpty())
		tempPath = "/tmp";
	if (ZipPlatform::GetDeviceFreeSpace(tempPath) < iSizeNeeded)
		return empty;
	CZipPathComponent::AppendSeparator(tempPath);
	tempPath += prefix;
	tempPath += _T("XXXXXX");
	int handle = mkstemp(tempPath.GetBuffer(tempPath.GetLength()));
	tempPath.ReleaseBuffer();
	if (handle != -1)
	{
		close(handle); // we just create the file and open it later
		return tempPath;
	}
	else
		return empty;		
}

bool ZipPlatform::GetCurrentDirectory(CZipString& sz)
{
	char* pBuf = getcwd(NULL, 0);
	if (!pBuf)
		return false;
	sz = pBuf;
	free(pBuf);
	return true;
}

bool ZipPlatform::SetFileAttr(LPCTSTR lpFileName, DWORD uAttr)
{
	return chmod(lpFileName, uAttr >> 16) == 0;

}

bool ZipPlatform::GetFileAttr(LPCTSTR lpFileName, DWORD& uAttr)
{
	struct stat sStats;
	if (stat(lpFileName, &sStats) == -1)
		return false;
  	uAttr = (sStats.st_mode & (S_IRWXU | S_IRWXG | S_IRWXO | S_IFMT)) << 16;
  	return true;


}

bool ZipPlatform::GetFileModTime(LPCTSTR lpFileName, time_t & ttime)
{

    	struct stat st;
	if (stat(lpFileName, &st) != 0)
		return false;

 	ttime = st.st_mtime;
	return ttime != -1;
}

bool ZipPlatform::SetFileModTime(LPCTSTR lpFileName, time_t ttime)
{
	struct utimbuf ub;
	ub.actime = time(NULL);
	ub.modtime = ttime == -1 ? time(NULL) : ttime; // if wrong file time, set it to the current
	return utime(lpFileName, &ub) == 0;
}


bool ZipPlatform::ChangeDirectory(LPCTSTR lpDirectory)
{
	return chdir(lpDirectory) == 0; 
}
int ZipPlatform::FileExists(LPCTSTR lpszName)
{
    	struct stat st;
	if (stat(lpszName, &st) != 0)
		return 0;
	else
	{
		if (S_ISDIR(st.st_mode))
			return -1;
		else
			return 1;
	}



}

ZIPINLINE  bool ZipPlatform::IsDriveRemovable(LPCTSTR lpszFilePath)
{
	// not implemmented
	return true;
}

ZIPINLINE  bool ZipPlatform::SetVolLabel(LPCTSTR lpszPath, LPCTSTR lpszLabel)
{
	// not implemmented
        return true;
}

ZIPINLINE void ZipPlatform::AnsiOem(CZipAutoBuffer& buffer, bool bAnsiToOem)
{
	// not implemmented
}

ZIPINLINE  bool ZipPlatform::RemoveFile(LPCTSTR lpszFileName, bool bThrow)
{
	if (unlink(lpszFileName) != 0)
		if (bThrow)
			CZipException::Throw(CZipException::notRemoved, lpszFileName);
		else 
			return false;
	return true;


}
ZIPINLINE  bool ZipPlatform::RenameFile( LPCTSTR lpszOldName, LPCTSTR lpszNewName , bool bThrow)
{

	if (rename(lpszOldName, lpszNewName) != 0)
		if (bThrow)
			CZipException::Throw(CZipException::notRenamed, lpszOldName);
		else 
			return false;
		return true;

}
ZIPINLINE  bool ZipPlatform::IsDirectory(DWORD uAttr)
{
	return S_ISDIR(uAttr >> 16) != 0;
}
ZIPINLINE  bool ZipPlatform::CreateDirectory(LPCTSTR lpDirectory)
{
	return mkdir(lpDirectory, S_IRWXU | S_IWUSR | S_IRGRP |S_IROTH) == 0;
}

ZIPINLINE  DWORD ZipPlatform::GetDefaultAttributes()
{
	return 0x81a40000;
}

ZIPINLINE  DWORD ZipPlatform::GetDefaultDirAttributes()
{
	return 0x41ff0010;
}


ZIPINLINE  int ZipPlatform::GetSystemID()
{
	return ZipCompatibility::zcUnix;
}

ZIPINLINE bool ZipPlatform::GetSystemCaseSensitivity()
{
	return true;
}


bool ZipPlatform::TruncateFile(int iDes, DWORD iSize)
{
	return ftruncate(iDes, iSize) == 0;

}

int ZipPlatform::OpenFile(LPCTSTR lpszFileName, UINT iMode, int iShareMode)
{
	return  open(lpszFileName, iMode, S_IRUSR | S_IWUSR | S_IRGRP |S_IROTH );
}

bool ZipPlatform::FlushFile(int iDes)
{
	return fsync(iDes) == 0;
}

int ZipPlatform::GetFileSystemHandle(int iDes)
{
        return iDes;
}


