////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements Atol plugin interface for GzLib plugin
////////////////////////////////////////////////////////////////////////////

#include "../plugin_int.h"
#include "ArchiveInfo.h"
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <stdio.h>
#include "zlib/zlib.h"

#ifdef _WIN32
 #include <windows.h>
#else
 #include <ctype.h>
#endif

#if _MSC_VER > 1000
 #pragma warning (disable: 4786)
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

unsigned long GetArchiverCaps(const char *szExt)
{
	return	PK_CAPS_REAL; //PK_CAPS_NEW;
}

//return file extensions usually connected to this kind of archive
const char *GetExtensions()
{
	static const char *pszExt = ".gz;.tgz;";
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

		if(!pArchInfo->m_objGzip.Open(szFile, CGZip::ArchiveModeRead/* || CGZip::ArchiveModeWrite*/))
		{
			g_ArchList.Remove(dwID);	//remove from map
			return 0;	//failure
		}

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
		pArchInfo->m_objGzip.Close();

		//remove from map
		g_ArchList.Remove(dwArchID);
		return false;
	}

	return true;
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
			//FIX: do not read file name stored within archive header (can be any garbage)
			//	   but construct your own name
			//std::string strEntry = pArchInfo->m_objGzip.GetFileName();
			std::string strEntry = pArchInfo->m_strFile;
			int nPos = strEntry.find_last_of('.');
			if(nPos > 0)
				strEntry = strEntry.substr(0, nPos);
			nPos = strEntry.find_last_of("\\/");
			if(nPos > 0)
				strEntry = strEntry.substr(nPos+1, 1000);
			
			strncpy(pInfo->szPath, strEntry.c_str(), sizeof(pInfo->szPath)-1);
			pInfo->szPath[sizeof(pInfo->szPath)-1] = '\0';

			//if the archive had .tgz extension, we must add .tar to the internal name
		#ifdef _WIN32
			if(0 == stricmp(pArchInfo->m_strFile.substr(pArchInfo->m_strFile.length()-4).c_str(), ".tgz"))
		#else
			if(0 == strcasecmp(pArchInfo->m_strFile.substr(pArchInfo->m_strFile.length()-4).c_str(), ".tgz"))
		#endif
				strcat(pInfo->szPath, ".tar");

			pArchInfo->m_strCurEntry = pInfo->szPath;

			pInfo->nPackSize = pArchInfo->m_objGzip.GetCompressSize();
			pInfo->nUnpSize  = pArchInfo->m_objGzip.GetUncompressSize();

			//get file time
			time_t timeDest;
			if(pArchInfo->m_objGzip.GetFileDate(timeDest))
				pInfo->tmModified = timeDest;

			//convert file attribute flags to internal portable flags
			pInfo->dwAttribs = 0;
			#ifdef _WIN32
				DWORD dwAttr = GetFileAttributes(pArchInfo->m_strFile.c_str());
				if(dwAttr & FILE_ATTRIBUTE_ARCHIVE)		pInfo->dwAttribs |= ATTR_ARCH;
				if(dwAttr & FILE_ATTRIBUTE_DIRECTORY)   pInfo->dwAttribs |= ATTR_DIR;
				if(dwAttr & FILE_ATTRIBUTE_HIDDEN)      pInfo->dwAttribs |= ATTR_HIDDEN;
				if(dwAttr & FILE_ATTRIBUTE_READONLY)    pInfo->dwAttribs |= ATTR_RONLY;
				if(dwAttr & FILE_ATTRIBUTE_SYSTEM)      pInfo->dwAttribs |= ATTR_SYSTEM;
			#else
				unsigned long dwAttr = 0;
				struct stat st;
				if(0 == stat(pArchInfo->m_strFile.c_str(), &st))
				{
					dwAttr = st.st_mode;

					pInfo->dwAttribs |= ATTR_UNIX;
					if(dwAttr & S_IFDIR)    pInfo->dwAttribs |= ATTR_DIR;
					if(S_ISLNK(dwAttr))     pInfo->dwAttribs |= ATTR_LINK;
					if(dwAttr & S_IRUSR)    pInfo->dwAttribs |= ATTR_R_USR;
					if(dwAttr & S_IWUSR)    pInfo->dwAttribs |= ATTR_W_USR;
					if(dwAttr & S_IXUSR)    pInfo->dwAttribs |= ATTR_X_USR;
					if(dwAttr & S_IRGRP)    pInfo->dwAttribs |= ATTR_R_GRP;
					if(dwAttr & S_IWGRP)    pInfo->dwAttribs |= ATTR_W_GRP;
					if(dwAttr & S_IXGRP)    pInfo->dwAttribs |= ATTR_X_GRP;
					if(dwAttr & S_IROTH)    pInfo->dwAttribs |= ATTR_R_OTH;
					if(dwAttr & S_IWOTH)    pInfo->dwAttribs |= ATTR_W_OTH;
					if(dwAttr & S_IXOTH)    pInfo->dwAttribs |= ATTR_X_OTH;
				}
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
		FILE *pFileOut = fopen(szDest, "wb");
		if(pFileOut)
		{
			pArchInfo->m_nProgressPos = 0;
			if(pArchInfo->m_pfnProgress)
				pArchInfo->m_pfnProgress(pArchInfo->m_strCurEntry.c_str(), 0, pArchInfo->m_dwUserData);

			char szBuffer[1024];
			int nTotalRead = 0;
			unsigned int nRead = 0;

			pArchInfo->m_objGzip.Rewind();	//reset to the beginning of the stream

			while(0 < (nRead = pArchInfo->m_objGzip.ReadBufferSize(szBuffer, sizeof(szBuffer)) ))
			{
				if(nRead != fwrite(szBuffer, 1, nRead, pFileOut)){
					fclose(pFileOut);
					return false;
				}

				nTotalRead += nRead;
				if(pArchInfo->m_pfnProgress)
				{
					int nRes = pArchInfo->m_pfnProgress(pArchInfo->m_strCurEntry.c_str(), nTotalRead, pArchInfo->m_dwUserData);
					if(0 == nRes) //abort request
					{
						fclose(pFileOut);
						remove(szDest);
						return false;
					}
				}
			}
		
			fclose(pFileOut);
			return true;
		}
	}

	return false;
}

//delete entry from open archive
bool DeleteEntry(int dwArchID, const char *szEntry)
{
	//not supported for RAR
	return false;
}

void ConfigurationDlg (long hWndParent, void *hDllInstance)
{
	//TOFIX Mfc support ?
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
		pArchInfo->m_pfnProgress = pProcessDataProc;
		pArchInfo->m_dwUserData	 = dwUser;
	}
}

//TOFIX add to PathName ?
bool MatchPaths(const char *szPath1, const char *szPath2)
{
	std::string strPath1(szPath1);
	std::string strPath2(szPath2);

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


