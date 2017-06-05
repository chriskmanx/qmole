////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Platform independent part of System utility class implementation.
////////////////////////////////////////////////////////////////////////////

#include "System.h"
#include "util.h"
#include "debug.h"
#include "PathName.h"

#ifdef _WIN32
#include <windows.h>
#include <direct.h> //_mkdir
#include <shlobj.h>
#ifndef CSIDL_PROFILE
 #define CSIDL_PROFILE 40
#endif
#else
 #include<unistd.h>
#endif
#include <sys/stat.h>

int StrCommonPrefixLen(const char *szPath1, const char *szPath2);

#define PATH_DELIMITERS "\\/"

namespace
{

// Returns true if str is in ASCII, false otherwise.
bool IsASCII(const std::string& str)
{
	bool isASCII = true;
	for (std::string::const_iterator
		it = str.begin(), end = str.end();
		it != end; ++it)
	{
		if (static_cast<unsigned char>(*it) > 127)
		{
			isASCII = false;
			break;
		}
	}
	return isASCII;
}

} //anonymous namespace

std::string System::GetHomeDir()
{
	static bool found = false;
	static std::string strDir;

	if (found)
		return strDir;
	else
	{
		//first try to see if the user has set special home destination
		//through the ATOL_HOME environment variable
		const char *szEnv = getenv("ATOL_HOME");

		if(NULL != szEnv)
			strDir = szEnv;

		if(strDir.empty())
		{
		#ifdef _WIN32
			TCHAR szPath[MAX_PATH];
			if(SUCCEEDED(SHGetSpecialFolderPath(NULL, szPath, CSIDL_PROFILE, TRUE))) 
			{
				strDir = szPath;
			}
			else{
				//old code, not always correct
				int len = GetEnvironmentVariable("HOMEDRIVE", szPath, sizeof(szPath));
				int ret = GetEnvironmentVariable("HOMEPATH",  szPath + len, sizeof(szPath) - len);
				if (ret == 0) {               // probably win95; store in \WINDOWS
					GetWindowsDirectory(szPath, sizeof(szPath));
					len = strlen(szPath);
				} else
					len += ret;
			}
		
			strDir = szPath;
		#else
			//for differnet user use getpwnam(user)->pw_dir
			strDir = getenv("HOME");
		#endif
			found = true;
		}
	}
	return strDir;
}

std::string System::GetAppPath()
{
	static bool found = false;
	static std::string path;

	if (!found)
	{
	#ifdef _WIN32
		char buf[512];
		*buf = '\0';
		::GetModuleFileName(NULL, buf, 511);
		path = buf;
	#else
		//code adapted from public domain code by Nicolai Haehnle <prefect_@gmx.net>
 		char linkname[64]; /* /proc/<pid>/exe */
		int ret;
		
		/* Get our PID and build the name of the link in /proc */
		pid_t pid = getpid();
			
		if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
			return path; //error

		/* Now read the symbolic link */
		char buffer[2048];
		ret = readlink(linkname, buffer, sizeof(buffer));
		
		/* In case of an error, leave the handling up to the caller */
		if (ret == -1)
			return path;
		
		/* Report insufficient buffer size */
		if (ret >= (int)sizeof(buffer))
			return path;

		buffer[ret] = '\0';
		path = buffer;
#endif
		found = true;
	}

	return path; 
}

std::string System::FileNameToUTF8(const std::string& fileName)
{
	// For performance reasons skip conversion if the file name seems
	// to be in ASCII. This will work for all ASCII based encodings such
	// as UTF-8, CP1251, etc.
	// TODO: add ability to disable this feature in config.
	if (IsASCII(fileName))
		return fileName;

	std::string fileNameInUTF8;
	gsize bytesWritten = 0;

	// On Windows GLib uses UTF-8 for filenames, but as well as we operate
	// on native Windows API, conversion from the encoding used in current
	// locale is required.
#if defined(G_OS_WIN32)
	Glib::ScopedCharPtr pFileNameInUTF8(
		g_locale_to_utf8(fileName.c_str(), -1, 0, &bytesWritten, 0));
#else
	Glib::ScopedCharPtr pFileNameInUTF8(
		g_filename_to_utf8(fileName.c_str(), -1, 0, &bytesWritten, 0));
#endif

	if (!pFileNameInUTF8)
	{
		// In case of error leave the string intact and hope for the best.
		fileNameInUTF8 = fileName;
	}
	else 
		fileNameInUTF8 = pFileNameInUTF8.Get();

	return fileNameInUTF8;
}

std::string System::FileNameFromUTF8(const std::string& fileNameInUTF8)
{
	std::string fileName;
	gsize bytesWritten = 0;

	// On Windows GLib uses UTF-8 for filenames, but as well as we operate
	// on native Windows API, conversion to the encoding used in current
	// locale is required.
#if defined(G_OS_WIN32)
	Glib::ScopedCharPtr pFileName(
		g_locale_from_utf8(fileNameInUTF8.c_str(), -1, 0, &bytesWritten, 0));
#else
	Glib::ScopedCharPtr pFileName(
		g_filename_from_utf8(fileNameInUTF8.c_str(), -1, 0, &bytesWritten, 0));
#endif

	if (!pFileName)
	{
		// In case of error leave the string intact and hope for the best.
		fileName = fileNameInUTF8.c_str();
	}
	else 
		fileName = pFileName.Get();

	return fileName;
}

//check if two paths belong to the same partition
bool System::IsSamePartition(const char *szPath1, const char *szPath2)
{
#ifdef _WIN32
	if(0 == strncmp(szPath1, szPath2, 3))	//compare first 3 letters - drive characters (a:\)
		return true;
#else
	//TOFIX important is that the partitions are sorted by length?
	//LINUX check (match if the paths are within the same disk partition)
	std::vector<std::string> lstPartitions;
	GetPartitionList(lstPartitions);

	TRACE("OpMove:IsSamePartition(%s,%s)!\n", szPath1, szPath2);
#ifdef _DEBUG
	for(unsigned int nPos=0; nPos<lstPartitions.size(); nPos++)
		TRACE("OpMove:IsSamePartition partition%d:%s\n", nPos, lstPartitions[nPos].c_str());
#endif

	unsigned int i;
	unsigned int nCommonLen = 0;

	//find partition index for the first path
	int nPartitionIdx1 = -1;
	for(i=0; i<lstPartitions.size(); i++)
	{
		unsigned int nLen = StrCommonPrefixLen(lstPartitions[i].c_str(), szPath1);
		TRACE("Common len %s,%s = %d\n", lstPartitions[i].c_str(), szPath1, nLen);

		if(nLen >= lstPartitions[i].size() &&
		   nLen > nCommonLen)
		{
			nPartitionIdx1 = i;
			nCommonLen = nLen;
		}
	}

	TRACE("Path %s belongs to partition %d!\n", szPath1, nPartitionIdx1);

	//find partition index for the next path
	nCommonLen = 0;
	int nPartitionIdx2 = -1;
	for(i=0; i<lstPartitions.size(); i++)
	{
		unsigned int nLen = StrCommonPrefixLen(lstPartitions[i].c_str(), szPath2);
		TRACE("Common len %s,%s = %d\n", lstPartitions[i].c_str(), szPath2, nLen);

		if(nLen >= lstPartitions[i].size() &&
		   nLen > nCommonLen)
		{
			nPartitionIdx2 = i;
			nCommonLen = nLen;
		}
	}

	TRACE("Path %s belongs to partition %d!\n", szPath2, nPartitionIdx2);
	if(nPartitionIdx1 == nPartitionIdx2)
		return true;	//same partition
#endif
	
	return false;
}

void System::GetPartitionList(std::vector<std::string> &lstPartitions)
{
	lstPartitions.clear();

#ifdef _WIN32
	DWORD dwDrives   = ::GetLogicalDrives();
	int   nDriveIdx  = 0;
	char  szDrive[5] = "A:\\";

	while(nDriveIdx < 26)
	{
		if(dwDrives & 1)
		{
			szDrive[0] = 'A' + nDriveIdx;
			lstPartitions.push_back(szDrive);
		}
		nDriveIdx++;
		dwDrives >>= 1;
	}
#else
	//execute df and parse result
	std::string strOutput;
	RunCommand("/bin/df", &strOutput);

	//break result into the set of lines
	const char *szStart = strOutput.c_str();
	char *szPos = NULL;
	int nLine = 0;
	while(NULL != (szPos = strchr(szStart, '\n')))
	{
		nLine ++; //new line found from szStart to szPos

		if(nLine > 1) //skip first (header) line
		{
			//filter line starting with "none" 
			if (0 != strncmp(szStart, "none", 4))
			{
				szStart = strchr(szStart, '%');
				if(NULL != szStart)
				{
					szStart ++;	//skip % character
					szStart ++;	//skip space
		
					char szMnt[1024];
					int nLen = szPos-szStart;
					if(nLen > 0 && nLen < 1024)
					{
						strncpy(szMnt, szStart, nLen);
						szMnt[nLen]='\0';
						
						lstPartitions.push_back(szMnt);
					}
				}
			}
		}
		
		szStart = szPos+1; //next line
	}	
#endif
}

bool System::Rename(const char *szOld, const char *szNew)
{
#ifdef _WIN32
	return (::MoveFile(szOld, szNew) != FALSE);
#else
	struct stat64 st;
	if (lstat64(szOld, &st) == -1)
		return false;

	//special case: handle copying link files!!!
	if(S_ISLNK(st.st_mode))
	{
		char szLnkTarget[1024];
		int nBytes = readlink(szOld, szLnkTarget, sizeof(szLnkTarget));
		if(-1 == nBytes)
			return false;
		szLnkTarget[nBytes] = '\0';
		TRACE("Rename: link=%s\n", szLnkTarget);

		//convert target from relative to absolute name
 		//(rel. name is invalid within the context of the new directory)
		String strDst;
		if(szLnkTarget[0] !='/'){
			strDst = PathName::GetParentDirPath(szOld);
			PathName::EnsureTerminated(strDst);
		}
		strDst += szLnkTarget;

		if(0 != symlink(strDst.c_str(), szNew))
			return false;

		return (0 == remove(szOld));	//TOFIX use System::Remove
	}

	return (0 == rename(szOld, szNew));
#endif
}

bool System::MkDir(const char *szDir)
{
#ifdef _WIN32
 #define mkdir _mkdir
	return (0 == mkdir(szDir));
#else
	return (0 == mkdir(szDir, 0777));
#endif
}

bool System::IsReadOnly(const char *szFile)
{
#ifdef _WIN32
	DWORD dwAttr = GetFileAttributes(szFile);
	if((0xFFFFFFFF != dwAttr) && ((dwAttr & FILE_ATTRIBUTE_READONLY) == FILE_ATTRIBUTE_READONLY))
		return true;
#else
	//check if we have write access on the file ?
#endif
	return false;
}

//NOTE: only for local paths
bool System::EnsureDirExists(String &strDir)
{
	if (PathName::IsRootDir (strDir))
		return true;    //no more searching (root/volume level)
	// remove ending / if exists
	if( strDir.Right(1) == "/" || strDir.Right(1) == "\\" )
		strDir = strDir.Left(strDir.Length()-1);

	// check if the directory already exists
#ifdef _WIN32
 #define stat _stat
 #define S_IFDIR _S_IFDIR
#endif
	struct stat st;
	if(0 == stat(strDir.c_str(), &st)){
		//file exists, check if it is directory
		return (S_IFDIR == (st.st_mode & S_IFDIR));
	}

	// recursively check parent directory
	int nPos = strDir.find_last_of(PATH_DELIMITERS);
	if(-1 == nPos)
		return true;    //no more searching (root/volume level)

	String strParentDir = strDir.Left(nPos);
	if(!EnsureDirExists(strParentDir))
		return false;

	//now create this directory
	return MkDir(strDir.c_str());
}
