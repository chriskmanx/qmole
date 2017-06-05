////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: PathName class implementation
////////////////////////////////////////////////////////////////////////////

#include "PathName.h"
#include "System.h"
#include <sys/stat.h>
#include <glib.h>
#include <glib/gprintf.h>
#include "debug.h"

PathName::PathName()
{
}

PathName::~PathName()
{
}

//TOFIX start using
String PathName::GetIniDirectory()
{
    String strDir;
    
	strDir = System::GetHomeDir().c_str();
	strDir += "/.atol";

    return strDir;
}

String PathName::GetDefaultStartDir()
{
#ifdef _WIN32
	static String strDir("C:\\");
#else
	static String strDir("/");
#endif
	
	return strDir;
}

bool PathName::IsRootDir(const String &strDir)
{
    #ifdef _WIN32
        if(strDir.Length() <= 3)
            return true;
    #else
        if(strDir.Length() < 2)
            return true;
    #endif

    return false;
}

bool PathName::IsAbsolutePath(const char *szPath)
{
	String strDir(szPath);

    #ifdef _WIN32
        if(strDir.Length() > 1 && strDir.GetAt(1) == ':')
			return true;
    #endif

	if(strDir.Length() > 0 && strDir.GetAt(0) == '/')
		return true;
	if(strDir.Length() > 0 && strDir.GetAt(0) == '\\') // just in case
		return true;

	return false;
}

String PathName::GetCanonicalPath(const char *szPath)
{
	String strPath(szPath);

	//for each ".." segment eat previous segment
	std::string::size_type nPos = strPath.find("..");
	while(nPos != std::string::npos)
	{
		//check if valid ".." segment
		unsigned int nLen = strPath.Length();
		if(nLen > nPos+3)
			if(strPath.GetAt(nPos+2) != '\\' && strPath.GetAt(nPos+2) != '/')
				return String();	//error, after .. must be delimiter
		if(nPos > 0) 
			if(strPath.GetAt(nPos-1) != '\\' && strPath.GetAt(nPos-1) != '/')
				return String();	//error, after .. must be delimiter

		//find previous segment
		String strPrev = strPath.Left(nPos-1);
		strPrev = GetParentDirPath(strPrev);
		EnsureNotTerminated(strPrev);

		String strNext = strPath.Right(strPath.Length()-nPos-2);

		strPath  = strPrev;
		strPath += strNext;
		
		nPos = strPath.find(".."); //keep looking
	}

	//TOFIX remove all "./" segments
	return strPath;
}

String PathName::GetRootDir(const String &strDir)
{
#ifdef _WIN32
	if(strDir.Length()>=3 && strDir.GetAt(1) == ':')
		return strDir.Left(3);
	else
		return strDir.Left(1);
#else
	return strDir.Left(1);
#endif
}

void PathName::StripExtension(String &strPath)
{
	const char *szPos = strrchr(strPath.c_str(), '.');
	if(szPos){
		int nPos = szPos - strPath.c_str();
		strPath  = strPath.Left(nPos);
	}
}

String PathName::GetExt(const char *szPath)
{
	const char *szPos = strrchr(szPath, '.');
	if(szPos)
		return String(szPos);

	return String("");	//TOFIX static
}

String PathName::GetBaseName(const char *szPath)
{
	static const String strDelimiters("/\\");
    
	String strPath(szPath);
	EnsureNotTerminated(strPath);

	size_t nPos = strPath.find_last_of(strDelimiters);
	if(nPos != std::string::npos)
		return strPath.Right(strPath.Length()-nPos-1);

	return strPath;	//no delimiter found
}

void PathName::EnsureTerminated(String &strPath, char cDelimiter)
{
	if(strPath.IsEmpty())
		strPath += cDelimiter;
	else{
		char cLast = strPath.GetAt(strPath.Length()-1);
		if(cLast != '\\' && cLast != '/')
			strPath += cDelimiter;
	}
}

bool PathName::IsTerminated(String &strPath)
{
	if(!strPath.IsEmpty())
	{
		char cLast = strPath.GetAt(strPath.Length()-1);
		if(cLast == '\\' || cLast == '/')
			return true;
	}
	return false;
}

void PathName::EnsureNotTerminated(String &strPath)
{
	if(!strPath.IsEmpty())
	{
		char cLast = strPath.GetAt(strPath.Length()-1);
		if(cLast == '\\' || cLast == '/')
			strPath = strPath.Left(strPath.Length()-1);
	}
}

String PathName::Path_TempDirectory()
{
#ifdef _WIN32
	TCHAR szPathBuffer[MAX_PATH] = "";
	GetTempPath(MAX_PATH, szPathBuffer);
	return String(szPathBuffer);
#else
	return String("/tmp");
#endif
}

String PathName::GetParentDirPath(const char *szPath)
{
	static const String strDelimiters("/\\");

	String strPath(szPath);
	EnsureNotTerminated(strPath);

	size_t nPos = strPath.find_last_of(strDelimiters);
	if(nPos != std::string::npos)
		strPath = strPath.Left(nPos + 1);
	else
		strPath.Empty();
	
	return strPath;
}

String PathName::ComposePath(const char *szDir, const char *szFile)
{
	String strResult(szDir);
	PathName::EnsureTerminated(strResult, '/');
	strResult += szFile;
	return strResult;
}


