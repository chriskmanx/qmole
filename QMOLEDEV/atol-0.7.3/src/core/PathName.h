////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: PathName class handles some useful file/filename operations
////////////////////////////////////////////////////////////////////////////

#ifndef PATHNAME_H__
#define PATHNAME_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "String.h"

class PathName
{
public:
	PathName();
	virtual ~PathName();

	static void EnsureTerminated(String &strPath, char cDelimiter = '/');
	static void EnsureNotTerminated(String &strPath);
	static bool IsTerminated(String &strPath);

	static String GetDefaultStartDir();
	static String GetRootDir(const String &strDir);
	static String GetParentDirPath(const char *szPath);
	static String GetBaseName(const char *szPath);
	static String GetExt(const char *szPath);
	static String GetIniDirectory();
	static bool IsRootDir(const String &strDir);
	static void StripExtension(String &strPath);
	static String ComposePath(const char *szDir, const char *szFile);

	static String Path_TempDirectory();

	static bool IsAbsolutePath(const char *szPath);
	static String GetCanonicalPath(const char *szPath);
};

#endif // PATHNAME_H__

