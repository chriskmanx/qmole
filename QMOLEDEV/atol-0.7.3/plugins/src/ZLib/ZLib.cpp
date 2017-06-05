// Bz2Lib.cpp : Defines the entry point for the DLL application.
//

#include "ArchiveInfo.h"
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include "../plugin_int.h"

#ifndef _WIN32
 #include <ctype.h>
#endif

//global data
COpenArchivesList	g_ArchList;
tPasswordProc		g_pfnPwdProc = NULL;
long				g_dwUser;

#ifdef _WIN32

BOOL APIENTRY DllMain( HANDLE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
    return TRUE;
}

#endif

//
//	API implemetation
//

unsigned long GetArchiverCaps(const char*szExt)
{
	//unpack only
	return	PK_CAPS_REAL; //PK_CAPS_NEW;
}

//return file extensions usually connected to this kind of archive
const char *GetExtensions()
{
	static const char *pszExt = ".z;.Z;.taz";
	return pszExt;
}

// create new archive or open existing one
int OpenArchive (const char *szFile)
{
	//create new CArchiveInfo
	CArchiveInfo *pArchInfo = new CArchiveInfo;
	if(NULL == pArchInfo)
		return 0;	//failure

	//TOFIX first open, then add to map ???
	//add CArchiveInfo to the map
	int dwID = g_ArchList.Add(pArchInfo);
	if(dwID > 0)
	{
		pArchInfo->m_dwID = dwID;

//TOFIX		if(!pArchInfo->m_objBZ2.Open(szFile))
//		{
//			g_ArchList.Remove(dwID);	//remove from map
//			return 0;	//failure
//		}

		pArchInfo->m_strFile = szFile;
		return dwID;
	}

	return 0;	//failure
}

// close opened (existing) archive
bool CloseArchive (int dwArchID)
{
	//close archive, if valid
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
//TOFIX		pArchInfo->m_objBZ2.Close();

		//remove from map
		g_ArchList.Remove(dwArchID);
		return true;
	}

	return false;
}

//list operations
void InitEntryEnum(int dwArchID)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		pArchInfo->m_bGzListed = false;
	}
}

int GetNextEntry(int dwArchID, tArchiveEntry *pInfo)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		if(!pArchInfo->m_bGzListed)
		{
			//std::string strEntry = pArchInfo->m_objBZ2.GetFileName();
			std::string strEntry = pArchInfo->m_strFile; //TOFIX

			//remove all except file name
			int nPos = strEntry.find_last_of('\\');
			if(nPos >= 0)
				strEntry = strEntry.substr(nPos+1);
			nPos = strEntry.find_last_of('/');	// Linux delimiter support
			if(nPos >= 0)
				strEntry = strEntry.substr(nPos+1);

			//remove last extension
			nPos = strEntry.find_last_of('.');
			if(nPos >= 0)
				strEntry = strEntry.substr(0, nPos);

			strncpy(pInfo->szPath, strEntry.c_str(), sizeof(pInfo->szPath)-1);
			pInfo->szPath[sizeof(pInfo->szPath)-1] = '\0';

			//if the archive had .taz extension, we must add .tar to the internal name
		#ifdef _WIN32
			if(0 == stricmp(pArchInfo->m_strFile.substr(pArchInfo->m_strFile.length()-4).c_str(), ".taz"))
		#else
			if(0 == strcasecmp(pArchInfo->m_strFile.substr(pArchInfo->m_strFile.length()-4).c_str(), ".taz"))
		#endif
				strcat(pInfo->szPath, ".tar");

			pArchInfo->m_strCurEntry = pInfo->szPath;

			pInfo->nUnpSize = 1000; //TOFIX

			//TOFIX
			//pInfo->nPackSize = pArchInfo->m_objBZ2.GetUnpackSize;

			pInfo->nUnpSize  = pArchInfo->m_objZ.GetUnpackSize(pArchInfo->m_strFile.c_str());

			//get file time using archive time
		#ifdef _WIN32
		  #define stat _stat
		#endif
			struct stat buf;
			if(0 == stat(pArchInfo->m_strFile.c_str(), &buf ))
				pInfo->tmModified = buf.st_mtime;

		#ifdef _WIN32
			//TOFIX portable flags, Linux version
			pInfo->dwAttribs = GetFileAttributes(pArchInfo->m_strFile.c_str());
		#endif
			pInfo->bDir = false;

			pArchInfo->m_bGzListed = true;
			return 1;	//done
		}
	}

	return 0;
}

//other operations
void ProcessMultiple(int dwArchID, int nOperation)
{
}

void EndProcessMulti(int dwArchID)
{
}

//pack file into the archive
bool PackFile(int dwArchID, const char *szFile, const char *SubPath, const char*szDestName)
{
	//not supported for RAR
	return false;
}

bool MakeDir(int dwArchID, const char *szDir)
{
	//not supported for RAR
	return false;
}

//TOFIX handle errors
//unpack archive entry
bool UnpackFile(int dwArchID, const char *szEntry, const char *szDest)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		pArchInfo->m_nProgressPos = 0;
		if(pArchInfo->m_objZ.m_pfnProgress)
		{
			pArchInfo->m_objZ.m_strEntry = pArchInfo->m_strCurEntry;
			pArchInfo->m_objZ.m_pfnProgress(pArchInfo->m_strCurEntry.c_str(), 0, pArchInfo->m_objZ.m_dwUserData);
		}

		//unpack with progress
		pArchInfo->m_objZ.Decompress(pArchInfo->m_strFile.c_str(), szDest);
		return true;
	}

	return false;
}

//delete entry from open archive
bool DeleteEntry(int dwArchID, const char *szEntry)
{
	//not supported for Z
	return false;	
}

void ConfigurationDlg (long hWndParent, void *hDllInstance)
{
	//TOFIX Mfc podrska ?
}

void SetChangeVolProc (int dwArchID, tChangeVolProc pChangeVolProc1)
{
	//TOFIX
}

void SetProcessDataProc (int dwArchID, tProcessDataProc pProcessDataProc, long dwUser)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		pArchInfo->m_objZ.m_pfnProgress = pProcessDataProc;
		pArchInfo->m_objZ.m_dwUserData	 = dwUser;
	}
}

//ubaciti u CPath ?
bool MatchPaths(const char *szPath1, const char *szPath2)
{
	std::string strPath1(szPath1);
	std::string strPath2(szPath2);

	//STEP 1. remove initial number of "./" segments
	while(0 == strncmp("./", strPath1.c_str(), 2))
		strPath1 = strPath1.substr(2, strPath1.size()-1);

	while(0 == strncmp("./", strPath2.c_str(), 2))
		strPath2 = strPath2.substr(2, strPath2.size()-1);

	//STEP 1. remove initial path segment delimiters
	while(strPath1.size() > 0 && (strPath1[0] == '\\' || strPath1[0] == '/' ))
		strPath1 = strPath1.substr(1, strPath1.size()-1);
		
	while(strPath2.size() > 0 && (strPath2[0] == '\\' || strPath2[0] == '/' ))
		strPath2 = strPath2.substr(1, strPath2.size()-1);

	//STEP 2: remove terminating path segment delimiters
	while(strPath1.size() > 0 && (strPath1[strPath1.size()-1] == '\\' || strPath1[strPath1.size()-1] == '/' ))
		strPath1 = strPath1.substr(0, strPath1.size()-1);
		
	while(strPath2.size() > 0 && (strPath2[strPath2.size()-1] == '\\' || strPath2[strPath2.size()-1] == '/' ))
		strPath2 = strPath2.substr(0, strPath2.size()-1);

	//STEP 3: convert all '/' delimiters to '\\'
	unsigned int i;
	for(i=0; i<strPath1.size(); i++)
		if(strPath1[i] == '/')
			strPath1[i] = '\\';

	for(i=0; i<strPath2.size(); i++)
		if(strPath2[i] == '/')
			strPath2[i] = '\\';

	//STEP 4: remove double path segment delimiters ?

	//STEP 5: make both paths lower case
	for(i=0; i<strPath1.size(); i++)
		strPath1[i] = tolower(strPath1[i]);

	for(i=0; i<strPath2.size(); i++)
		strPath2[i] = tolower(strPath2[i]);

	//STEP 6. return strcmp
	return (0 == strcmp(strPath1.c_str(), strPath2.c_str()));
}

void SetPasswordProc(tPasswordProc pPwdProc, long dwUser)
{
	g_pfnPwdProc = pPwdProc;
	g_dwUser	 = dwUser;
}

