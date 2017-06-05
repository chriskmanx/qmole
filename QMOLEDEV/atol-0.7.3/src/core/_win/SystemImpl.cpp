////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: System utility class - Windows implementation.
////////////////////////////////////////////////////////////////////////////

#include <string>
#include <stdexcept>
#include <windows.h>
#include <shlguid.h>
#include <shlobj.h>
#include <comip.h>
#include <comdef.h>
#include <io.h>

#include "../System.h"
#include "../String.h"
#include "../PathName.h"
#include "../debug.h"
#include "../util.h"
#include "pidl.h"

#include <glib.h>
#include <glib/gstdio.h>

#ifndef INVALID_FILE_ATTRIBUTES 
 #define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#endif

// Link needed by PathQuoteSpaces.
#if _MSC_VER > 1000
 #pragma comment(lib, "Shlwapi")
#endif

// Free strategy that uses LocalFree.
template <typename T>
struct LocalFreeStrategy
{
	static void Free(T* ptr)
	{
		LocalFree(ptr);
	}
};

// Smart pointer to object or array of objects of type T,
// allocated using LocalAlloc.
typedef ScopedPtr< TCHAR, LocalFreeStrategy<TCHAR> > ScopedTStrPtr;

// Throws exception with message string that describes last error
// associated with the calling thread.
void ThrowLastError()
{
	const DWORD code = GetLastError();
	std::string message;
	ScopedTStrPtr buffer;
	if (FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER |
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		0, code, 0, reinterpret_cast<LPTSTR>(buffer.GetAddress()), 0, 0) != 0)
	{
		message = buffer.Get();
	}
	else 
		message = "Unknown error.";

	throw std::runtime_error(message);
}


// Windows shell link.
class ShellLink
{
public:
	enum { MAX_ARGUMENTS_SIZE = 4096 };

	ShellLink() : m_link(CLSID_ShellLink)
	{
		if (m_link == 0)
			ThrowLastError();
	}
	virtual ~ShellLink() {}

	// Returns true if loaded successfully, false otherwise.
	bool Load(const std::string& path);

	// Get methods return empty strings in case of errors.
	std::string GetPath() const;
	std::string GetArguments() const;
	std::string GetWorkingDirectory() const;

private:
	_COM_SMARTPTR_TYPEDEF(IShellLink, __uuidof(IShellLink));
	IShellLinkPtr m_link;
};

bool ShellLink::Load(const std::string& path)
{
	_COM_SMARTPTR_TYPEDEF(IPersistFile, __uuidof(IPersistFile));
	IPersistFilePtr file;
	if (FAILED(m_link.QueryInterface(IID_IPersistFile, &file))) return false;

	// convert path to UTF-16
	WCHAR pathInUTF16[MAX_PATH];
	pathInUTF16[0] = L'\0';
	if (MultiByteToWideChar(CP_ACP, 0, path.c_str(), -1, pathInUTF16, MAX_PATH) == 0)
		return false;

	// load link file
	if (FAILED(file->Load(pathInUTF16, STGM_READ))) return false;
	return true;
}

std::string ShellLink::GetPath() const
{
	std::string path;
	char buffer[MAX_PATH];
	buffer[0] = '\0';
	if (m_link->GetPath(buffer, MAX_PATH, 0, 0) == NOERROR)
		path = buffer;
	return path;
}

std::string ShellLink::GetArguments() const
{
	std::string args;
	char buffer[MAX_ARGUMENTS_SIZE];
	buffer[0] = '\0';
	if (m_link->GetArguments(buffer, sizeof(buffer)) == S_OK)
		args = buffer;
	return args;
}

std::string ShellLink::GetWorkingDirectory() const
{
	std::string dir;
	char buffer[MAX_PATH];
	buffer[0] = '\0';
	if (m_link->GetWorkingDirectory(buffer, MAX_PATH) == S_OK)
		dir = buffer;
	return dir;
}

class ScopedSearchHandle
{
private:
	typedef void (ScopedSearchHandle::*BoolType)() const;
	void ThisTypeDoesNotSupportComparisons() const {}

public:
	typedef long Handle;

	ScopedSearchHandle() : m_handle(0) {}
	ScopedSearchHandle(Handle handle) : m_handle(handle) {}
	~ScopedSearchHandle()
	{
		Reset();
	}

	void Reset(Handle handle = 0)
	{
		if (m_handle != handle)
		{
			if (m_handle != 0)
				_findclose(m_handle);
			m_handle = handle;
		}
	}

	operator BoolType() const
	{
		return m_handle != 0 ? &ScopedSearchHandle::ThisTypeDoesNotSupportComparisons : 0;
	}

	// Get stored pointer.
	Handle Get() const
	{
		return m_handle;
	}

	// Get address of stored pointer.
	Handle* GetAddress()
	{
		return &m_handle;
	}

private:
	// forbid copying
	ScopedSearchHandle(const ScopedSearchHandle& );
	const ScopedSearchHandle& operator=(const ScopedSearchHandle& );

	Handle m_handle;
};
 
namespace
{

// Formats and shows system error message.
void ShowSystemErrorMessage(UINT code)
{
	std::string message;
	ScopedTStrPtr buffer;
	if (FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER |
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		0, code, 0, reinterpret_cast<LPTSTR>(buffer.GetAddress()), 0, 0) != 0)
	{
		message = buffer.Get();
	}
	else message = "Unknown error.";
	// TOFIX gtkMessageBox((LPCTSTR)message.c_str()/*, _T("Error")*/);
}

long GetRegKey(HKEY key, LPCTSTR subkey, LPTSTR retdata)
{
	HKEY hkey;
	LONG retval = RegOpenKeyEx(key, subkey, 0, KEY_QUERY_VALUE, &hkey);

	if (retval == ERROR_SUCCESS)
	{
		long datasize = MAX_PATH;
		TCHAR data[MAX_PATH];
		RegQueryValue(hkey, NULL, data, &datasize);
		lstrcpy(retdata, data);
		RegCloseKey(hkey);
	}

	return retval;
}

bool FindExecutableKey(LPCTSTR szExt, LPTSTR szBuffer)
{
	return GetRegKey(HKEY_CLASSES_ROOT, szExt, szBuffer) == ERROR_SUCCESS;
}

bool GetDefaultVerb(const char *szExt, char *szVerb, char *szCmdLine)
{
	szVerb[0] = '\0';

	// File extension string equals to the registry key name.
	// Value of this key points to another registry key describing shell actions
	// for given file format (described by file extension string)
	char szKey[MAX_PATH] = "";
	if (FindExecutableKey(szExt, szKey))
	{
		char szKey2[MAX_PATH] = "";
		strcpy(szKey2, szKey);    //TOFIX lstrcpyn
		lstrcat(szKey2, "\\shell");

		// See if "shell" subkey has default value defined 
		// (default shell action verb for this format)
		if (GetRegKey(HKEY_CLASSES_ROOT, szKey2, szVerb) == ERROR_SUCCESS &&
			strlen(szVerb) > 0)
		{
			// test if the verb is valid (must have valid "command" subkey)
			lstrcat(szKey2, "\\");
			lstrcat(szKey2, szVerb);
			lstrcat(szKey2, "\\command");

			// default value of "\\shell\\VERB\\command" subkey (replace VERB with actual value)
			// gives us command line string (app + parameters)
			if (GetRegKey(HKEY_CLASSES_ROOT, szKey2, szCmdLine) == ERROR_SUCCESS)
				return true;
		}
		else //no default verb defined
		{
			// test for presence of standard "open" subkey
			strcpy(szKey2, szKey);    //TOFIX lstrcpyn
			lstrcat(szKey2, "\\shell\\open\\command");
			if (GetRegKey(HKEY_CLASSES_ROOT, szKey2, szCmdLine) == ERROR_SUCCESS)
			{
				lstrcpy(szVerb, "open");
				return true;
			}

			// else (last chance to find default verb)
			// take first available subkey under "shell"
			strcpy(szKey2, szKey);    //TOFIX lstrcpyn
			lstrcat(szKey2, "\\shell");

			// enumerate subkeys until we find first subkey name
			HKEY hkey;
			LONG retval = RegOpenKeyEx(HKEY_CLASSES_ROOT, szKey2, 0, KEY_ENUMERATE_SUB_KEYS, &hkey);
			if (retval == ERROR_SUCCESS)
			{
				DWORD datasize = MAX_PATH;
				retval = RegEnumKeyEx(hkey, 0, szVerb, &datasize, NULL, NULL, NULL, NULL);
				RegCloseKey(hkey);
				if (retval == ERROR_SUCCESS) 
				{
					// test if the verb is valid (must have valid "command" subkey)
					lstrcat(szKey2, "\\");
					lstrcat(szKey2, szVerb);
					lstrcat(szKey2, "\\command");

					// default value of "\\shell\\VERB\\command" subkey (replace VERB with actual value)
					// gives us command line string (app + parameters)
					if (GetRegKey(HKEY_CLASSES_ROOT, szKey2, szCmdLine) == ERROR_SUCCESS)
						return true;
				}
			}
		}
	}

	return false;
}

bool OpenWith(LPCSTR szApp, LPCSTR szArgs, LPCSTR szDir)
{
	// our last chance is to start "Open With" shell dialog
	String strParams;
	strParams = "shell32.dll,OpenAs_RunDLL ";
	strParams += szApp;

	HINSTANCE hInstance =
		ShellExecute(NULL, "open", "rundll32.exe", strParams, szDir, SW_SHOWNORMAL);

	DWORD result = reinterpret_cast<DWORD>(hInstance);
	if (result <= HINSTANCE_ERROR)
	{
		ShowSystemErrorMessage(result); // format and show system error message
		return false;
	}

	return true;
}

bool ExecuteFile(const char *szFile, const char *szArgs, const char *szDir, void *nData, bool bWaitForTermination = false)
{
	String strPath(szDir);

	PathName::EnsureTerminated(strPath, '\\');
	strPath += szFile;

	// determine what verb to use
	char szVerb[MAX_PATH] = "";
	char szCmdLine[MAX_PATH] = "";

	{
		/* TOFIX
		wxFileName path(szFile);
		if(!GetDefaultVerb(path.GetExt(), szVerb, szCmdLine))
			strcpy(szVerb, "open");//some cases (".avi" on XP) do not work without this set explicitly!!
			*/
	}

	SHELLEXECUTEINFO si;
	ZeroMemory(&si,sizeof(si));
	si.cbSize       = sizeof(si);
	si.lpVerb       = szVerb;
	si.nShow        = SW_SHOW;
	si.lpParameters = szArgs;
	si.lpDirectory  = szDir;
	si.fMask        = SEE_MASK_FLAG_NO_UI | SEE_MASK_FLAG_DDEWAIT | SEE_MASK_NOCLOSEPROCESS;
	si.hwnd         = static_cast<HWND>(nData);

	// FIX use pidl as primary execute way instead of path to prevent bug
	//      with files with name like "ftp.htm", "ftp.dat" where system
	//      tries to open some "fantom" ftp site
	HINSTANCE hInstance = 0;
	LPITEMIDLIST pidl = PIDL::GetFromPath(strPath);
	if (NULL != pidl)
	{
		si.lpIDList = (LPVOID)pidl;
		si.fMask   |= SEE_MASK_INVOKEIDLIST;
		ShellExecuteEx(&si);
		PIDL::Free(pidl);

		hInstance = si.hInstApp;
	}

	// if PIDL version fails revert to path based version
	// PIDL can fail:
	// 1. szFile is URL
	// 2. on WinXP failed with .avi file on CDROM drive!
	if (reinterpret_cast<UINT>(hInstance) <= HINSTANCE_ERROR)
	{
		si.lpIDList = NULL;
		si.fMask   &= ~SEE_MASK_INVOKEIDLIST;
		si.lpFile = szFile;
		ShellExecuteEx(&si);
		hInstance = si.hInstApp;
	}

	if(bWaitForTermination)
	{
		if (NULL == si.hProcess)
			return false;

		WaitForSingleObject(si.hProcess, INFINITE);
		return true;
	}

	UINT result = reinterpret_cast<UINT>(hInstance);

	// If it failed, get the .htm regkey and lookup the program
	if (result > HINSTANCE_ERROR)
		return true;

	if (SE_ERR_NOASSOC != result && SE_ERR_ASSOCINCOMPLETE != result)
	{
		ShowSystemErrorMessage(result);
		return false;
	}

	// use our own version of the FindExecutable() ???
	char szRes[MAX_PATH];
	if (FindExecutable(szFile, szDir, szRes))
	{
		char szAppPath[MAX_PATH];
		lstrcpyn(szAppPath, szDir, MAX_PATH);
		if(szAppPath[strlen(szAppPath)-1] != '\\')
			lstrcat(szAppPath, "\\");
		lstrcat(szAppPath, szFile);
		PathQuoteSpaces(szAppPath);

		// TOFIX ShellExecute?
		lstrcat(szRes, " ");
		lstrcat(szRes, szAppPath);
		result = WinExec(szRes, SW_SHOW);
		hInstance = reinterpret_cast<HINSTANCE>(result);
		if (result <= HINSTANCE_ERROR)
		{
			ShowSystemErrorMessage(result);
			return false;
		}
		return true;
	}

	// at this point there were no association and we failed to find it manually
	return OpenWith(szFile, szArgs, szDir);
}

} // anonymous namespace

EnumResult System::EnumDirectory(
	const std::string& nativePath,
	FN_ENUMDIR callback,
	void* callbackData,
	int flags)
{
	std::string nativePathWithSlash(nativePath);
	if (!nativePathWithSlash.empty() &&
		nativePathWithSlash[nativePathWithSlash.size() - 1] != '\\')
 	{ 
		nativePathWithSlash += '\\'; 
	}

	std::string filespec(nativePathWithSlash + "*.*"); 

	_finddata_t fileinfo;
	ScopedSearchHandle handle(_findfirst(filespec.c_str(), &fileinfo));
	int findResult = static_cast<int>(handle.Get());
	if (findResult == -1){
		if(0 == access(nativePathWithSlash.c_str(), 00))
			return ENUM_ERR_OK;	// no files on the drive, but no error

		return ENUM_ERR_DIR;
	}
 
	for ( ; findResult != -1; findResult = _findnext(handle.Get(), &fileinfo))
	{ 
		if (strcmp(fileinfo.name, ".") == 0 || strcmp(fileinfo.name, "..") == 0)
			continue; 

		// create full path
		std::string nativeChildPath(nativePathWithSlash + fileinfo.name);
		std::string childPath(nativeChildPath);
		  
		if (fileinfo.attrib & _A_SUBDIR) // if directory
		{
			if (flags & ENUM_LST_DIRS) 
			{
				if (!callback(childPath.c_str(), callbackData))
					return ENUM_ERR_OK; // abort request 
			}
			if (flags & ENUM_RECURSIVE)
			{
				EnumResult result =
					EnumDirectory(childPath.c_str(), callback, callbackData, flags);
				if (result < 0)
					return result; // error
			}
		}
		else // if file
		{
			if (flags & ENUM_LST_FILES)
			{
				if (!callback(childPath.c_str(), callbackData)) 
					return ENUM_ERR_OK; // abort request 
			}
		}
	}

	return ENUM_ERR_OK;
}

bool System::Execute(const std::string& path, const std::string& args, const std::string& dir, bool bWaitForTermination /* = false */)
{
	std::string nativeDir(dir);
	std::string nativePath(path); 
 
 	// TODO: execute bat files

	// check if the file is a link
	std::string::size_type extPos = path.rfind('.');
	if (extPos != std::string::npos &&
		String(path.substr(extPos).c_str()).CmpNoCase(".lnk") == 0)
	{
		try
 		{
			// load link
			ShellLink link;
			if (!link.Load(nativePath))
				throw std::runtime_error("Loading link failed.");

			// get link data
			std::string nativeTarget(link.GetPath());
			if (nativeTarget.empty())
				throw std::runtime_error("Getting link target path failed.");
			std::string linkArgs(link.GetArguments());
			nativeDir = link.GetWorkingDirectory();

			// merge arguments with link arguments
			if (!args.empty())
			{
				if (!linkArgs.empty())
					linkArgs += " ";
				linkArgs += args;
			}

			// execute link target
			return ExecuteFile(nativeTarget.c_str(), linkArgs.c_str(), nativeDir.c_str(), NULL, bWaitForTermination);
		}
		// if there was error processing link, ignore it and run ExecuteFile as is
		catch (...) {}
	}

	return ExecuteFile(nativePath.c_str(), args.c_str(), nativeDir.c_str(), NULL, bWaitForTermination);
}

bool System::Open(const std::string& path, bool bWaitForTermination /* = false */)
{
	return Execute(path, std::string(), PathName::GetParentDirPath(path.c_str()), bWaitForTermination);
}

bool System::Remove(const std::string& path)
{
	std::string nativePath(path);

	typedef BOOL (WINAPI *DeleteFunc)(LPCSTR );
	DeleteFunc deleteFunc = &DeleteFile;

	DWORD dwAttr = GetFileAttributes(nativePath.c_str());
	if (dwAttr != INVALID_FILE_ATTRIBUTES)
	{
		if ((dwAttr & FILE_ATTRIBUTE_READONLY) != 0)
		{
			// remove read-only attribute, otherwise delete API will fail
			dwAttr &= ~(FILE_ATTRIBUTE_READONLY);
			SetFileAttributes(nativePath.c_str(), dwAttr);
		}

		if ((dwAttr & FILE_ATTRIBUTE_DIRECTORY) != 0)
			deleteFunc = &RemoveDirectory;
	}

	if (deleteFunc(nativePath.c_str()) == FALSE)
	{
		TRACE("Failed to remove \"%s\". Error: %d.\n", path.c_str(), GetLastError());
		return false;
	}
	return true;
}

std::string System::GetLinkTarget(const std::string& linkPath)
{
	std::string nativePath(FileNameFromUTF8(linkPath));

	// load link
	ShellLink link;
	if (!link.Load(nativePath))
		return std::string();

	return FileNameToUTF8(link.GetPath());
}

bool System::MoveToTrash(const std::string& path)
{
	std::string nativePath(FileNameFromUTF8(path));

	// double-zero terminated string
	char szFile[1024];
	strncpy(szFile, nativePath.c_str(), sizeof(szFile) - 1);
	szFile[strlen(szFile) + 1] = '\0';

	// I found this very spooky problem when calling this function on the following file ararngement:
	// 1. inside some test directory create directory "VC6-ch04-Documents and Views_files"
	// 2. inside test directory create file "VC6-ch04-Documents and Views.htm"
	// 3. when trying to delete the file, as a bonus, function also deletes the directory !!!!
	//    (grrr, I've lost some important data this way).
	//
	// FIX: this fixes BIG MS shit with changing API SHFileOperation behaviour after Win2k
	// Windows 2k and later have moronic idea of connecting single .htm file with its file folder
	// (same name with "_files" suffix).
	// This means: when you delete file, they will, as a "bonus", also delete the directory !!!!
	// this flag prevents this behaviour !!!!
#ifndef FOF_NO_CONNECTED_ELEMENTS
	#define FOF_NO_CONNECTED_ELEMENTS 0x2000
#endif

	SHFILEOPSTRUCT fileop;
	ZeroMemory(&fileop, sizeof(SHFILEOPSTRUCT));
	fileop.wFunc    = FO_DELETE;
	fileop.pFrom    = szFile;
	fileop.pTo      = NULL;
	fileop.fFlags   = FOF_ALLOWUNDO | FOF_SILENT | FOF_NOERRORUI |
	                  FOF_NOCONFIRMATION | FOF_NO_CONNECTED_ELEMENTS;

	if (SHFileOperation(&fileop) != 0)
	{
		TRACE("Unable to send \"%s\" to the recycle bin. Error: %d.\n", nativePath.c_str(), GetLastError());
		return false;
	}
	return true;
}

INT64 System::GetAvailBytesOnFS(const std::string& path, INT64 &nTotal)
{
	typedef BOOL (CALLBACK* LPFNGETSPACEEX)(LPCTSTR, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER);
	typedef BOOL (CALLBACK* LPFNGETSPACE)  (LPCTSTR, LPDWORD, LPDWORD, LPDWORD, LPDWORD);

	INT64 avail = INT64_MAX;
	BOOL bResult = FALSE;

	// Load the kernel to gain access to the functions we want
	// If this fails, something's REALLY wrong
	HMODULE hKernel = ::GetModuleHandle("Kernel32.dll");
	ASSERT(NULL != hKernel);

	if (NULL != hKernel)
	{
#ifdef _UNICODE
		LPFNGETSPACEEX	pfnGetDiskFreeSpaceEx  = (LPFNGETSPACEEX)::GetProcAddress(hKernel, "GetDiskFreeSpaceExW");
		LPFNGETSPACE	pfnGetDiskFreeSpace    = (LPFNGETSPACE)::GetProcAddress(hKernel, "GetDiskFreeSpaceW");
#else
		LPFNGETSPACEEX	pfnGetDiskFreeSpaceEx  = (LPFNGETSPACEEX)::GetProcAddress(hKernel, "GetDiskFreeSpaceExA");
		LPFNGETSPACE	pfnGetDiskFreeSpace    = (LPFNGETSPACE)::GetProcAddress(hKernel, "GetDiskFreeSpaceA");
#endif

		if (NULL != pfnGetDiskFreeSpaceEx)
		{
			ULARGE_INTEGER nCallerFreeBytes;  // Receives the number of bytes on
			                                  // disk available to the caller
			ULARGE_INTEGER nDiskSize;         // Receives the number of bytes on disk
			ULARGE_INTEGER nTotalFreeBytes;   // Receives the total free bytes on the disk

			bResult = pfnGetDiskFreeSpaceEx( path.c_str(),
			                                 &nCallerFreeBytes,
			                                 &nDiskSize,
			                                 &nTotalFreeBytes);
			if (bResult)
			{
				avail  = nCallerFreeBytes.QuadPart;
				nTotal = nDiskSize.QuadPart;
			}
		}

		// Failing that try the old fashioned way...
		if (!bResult)
		{
			DWORD dwSectorsPerCluster;
			DWORD dwBytesPerSector;
			DWORD dwFreeClusters;
			DWORD dwTotalClusters;

			bResult = pfnGetDiskFreeSpace( path.c_str(),
			                               &dwSectorsPerCluster,
			                               &dwBytesPerSector,
			                               &dwFreeClusters,
			                               &dwTotalClusters);
			if (bResult)
			{
				avail  = INT64(dwFreeClusters) * dwSectorsPerCluster * dwBytesPerSector;
				nTotal = INT64(dwTotalClusters) * dwSectorsPerCluster * dwBytesPerSector;
			}
		}
	}

	return avail;
}

bool System::RunCommand(const std::string& , std::string*)
{
	// TODO
	return false;
} 

