////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements Atol plugin interface for ArjLib plugin
////////////////////////////////////////////////////////////////////////////

#include "../plugin_defs.h"


ARCHIVE_API int  GetArchiverCaps();
	// get flags that describe archiver capabilities

ARCHIVE_API const char *GetExtensions();

ARCHIVE_API int OpenArchive (const char *szFile);
	// create new archive or open existing one

ARCHIVE_API int CloseArchive (int dwArchID);
	// close opened (existing) archive

//TOFIX skip file, test integritiy
//list operation
ARCHIVE_API void InitEntryEnum(int dwArchID);
ARCHIVE_API int  GetNextEntry(int dwArchID, tArchiveEntry *pInfo);

//other operations
ARCHIVE_API void ProcessMultiple(int dwArchID, int nOperation);
ARCHIVE_API void EndProcessMulti(int dwArchID);

ARCHIVE_API bool PackFile(int dwArchID, const char *szFile, const char *SubPath, const char*szDestName);
	//pack file into the archive

ARCHIVE_API bool UnpackFile(int dwArchID, const char *szEntry, const char *szDest);
	//unpack archive entry

ARCHIVE_API bool DeleteEntry(int dwArchID, const char *szEntry);
	//delete entry from open archive

ARCHIVE_API bool MakeDir(int dwArchID, const char *szDir);

ARCHIVE_API void ConfigurationDlg(long Parent, void *DllInstance);
	//

ARCHIVE_API void SetChangeVolProc(int dwArchID, tChangeVolProc pChangeVolProc1);
	//

ARCHIVE_API void SetProcessDataProc(int dwArchID, tProcessDataProc pProcessDataProc, long dwUser);
	//

ARCHIVE_API void SetPasswordProc(tPasswordProc pPwdProc, long dwUser);
