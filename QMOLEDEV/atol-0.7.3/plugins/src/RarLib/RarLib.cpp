// RarLib.cpp : Defines the entry point for the DLL application.
//

#include "../plugin_int.h"
#include "ArchiveInfo.h"
#include <string>
#include <windows.h>
#include <commdlg.h>

// RAR lib usage:
// it seems that when you open an archive you can not list or process it it twice?
// because of that we keep archive closed most of the time and open it before an operation

//TOFIX better error report

//global data
COpenArchivesList	g_ArchList;
tPasswordProc		g_pfnPwdProc = NULL;
long				g_dwUser;

int PASCAL CallbackProc(UINT msg,LONG UserData,LONG P1,LONG P2);
bool MatchPaths(const char *szPath1, const char *szPath2);

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
			break;

		case DLL_PROCESS_DETACH:
			g_ArchList.RemoveAll();		//close all open archives
			break;
    }
    return TRUE;
}

//
//	API implemetation
//

unsigned long GetArchiverCaps(const char *szExt)
{
	return	PK_CAPS_REAL|PK_CAPS_MULTIPLE;	//archive can contain multiple files
}

//return file extensions usually connected to this kind of archive
const char *GetExtensions()
{
	//NOTE: multiple extensions sample: ".gz;.tgz";
	static const char *pszExt = ".rar";
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
	DWORD dwID = g_ArchList.Add(pArchInfo);
	if(dwID > 0)
	{
		pArchInfo->m_dwID = dwID;

		//comments
		//char CmtBuf[16384];
		memset(&pArchInfo->m_rar,0,sizeof(pArchInfo->m_rar));
		pArchInfo->m_rar.ArcName = (char *)szFile;
		pArchInfo->m_rar.CmtBuf=NULL;	//CmtBuf
		pArchInfo->m_rar.CmtBufSize=0;	//sizeof(CmtBuf)
		pArchInfo->m_rar.OpenMode=RAR_OM_LIST|RAR_OM_EXTRACT;

		HANDLE hArchive = RAROpenArchiveEx(&(pArchInfo->m_rar));
		if(NULL == hArchive)
		{
			//if (OpenArchiveData.OpenResult!=0)
			//{
			//OutOpenArchiveError(OpenArchiveData.OpenResult,ArcName);
			//return;
			//}
			//TOFIX notify error (get error text?)
			g_ArchList.Remove(dwID);	//remove from map
			return 0;	//failure
		}

		 //ShowArcInfo(OpenArchiveData.Flags,ArcName);
		pArchInfo->m_strFile = szFile;
		RARCloseArchive(hArchive);
		pArchInfo->m_hArchive = NULL;
		//pArchInfo->m_hArchive = hArchive;
		return dwID;
	}
	
	return 0;	//failure
}

// close opened (existing) archive
bool CloseArchive (int dwArchID)
{
	//if archive valid
	if(g_ArchList.IdExists(dwArchID))
	{
		//close archive
		CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
		//if(NULL != pArchInfo)
		//	RARCloseArchive(pArchInfo->m_hArchive);

		//remove from map
		g_ArchList.Remove(dwArchID);
		return true;
	}
	return false;
}
	
//list operations
void InitEntryEnum(int dwArchID)
{
	//set enumeration initial position
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		//TOFIX zero memory ?
		memset(&pArchInfo->m_rar,0,sizeof(pArchInfo->m_rar));
		pArchInfo->m_rar.ArcName = (char *)pArchInfo->m_strFile.c_str();
		pArchInfo->m_rar.CmtBuf=NULL;	//CmtBuf
		pArchInfo->m_rar.CmtBufSize=0;	//sizeof(CmtBuf)
		pArchInfo->m_rar.OpenMode=RAR_OM_LIST;

		HANDLE hArchive = RAROpenArchiveEx(&(pArchInfo->m_rar));
		if(hArchive)
			pArchInfo->m_hArchive = hArchive;
	}
}

int GetNextEntry(int dwArchID, tArchiveEntry *pInfo)
{
	//enumeration - get next entry
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	//ASSERT + if

	if(NULL == pArchInfo->m_hArchive) 
		return 0;

	if(NULL != pInfo)
	{
		//comments
		//char CmtBuf[16384];
		//pArchInfo->m_HeaderData.CmtBuf=CmtBuf;
		//pArchInfo->m_HeaderData.CmtBufSize=sizeof(CmtBuf);

		int RHCode = RARReadHeaderEx(pArchInfo->m_hArchive, &(pArchInfo->m_HeaderData));
		if(0 == RHCode)
		{
			//copy file info
			strncpy(pInfo->szPath, pArchInfo->m_HeaderData.FileName, sizeof(pInfo->szPath)-1);
			pInfo->szPath[sizeof(pInfo->szPath)-1] = '\0';
			pInfo->nPackSize = pArchInfo->m_HeaderData.PackSize+(((__int64)pArchInfo->m_HeaderData.PackSizeHigh)<<32);
			pInfo->nUnpSize  = pArchInfo->m_HeaderData.UnpSize+(((__int64)pArchInfo->m_HeaderData.UnpSizeHigh)<<32);

			//convert file time
			pInfo->tmModified = pArchInfo->m_HeaderData.FileTime;
			pInfo->dwAttribs = pArchInfo->m_HeaderData.FileAttr;
			pInfo->bDir = ((pInfo->dwAttribs & FILE_ATTRIBUTE_DIRECTORY) != 0);

			//skip to next file
			int PFCode = RARProcessFile(pArchInfo->m_hArchive, RAR_SKIP, NULL, NULL);
			if (PFCode != 0)
			{
				RARCloseArchive(pArchInfo->m_hArchive);
				pArchInfo->m_hArchive = NULL;	//end enumerating
			}

			return 1;

		}
		else
		{
			RARCloseArchive(pArchInfo->m_hArchive);
			pArchInfo->m_hArchive = NULL;
		}
	}

	RARCloseArchive(pArchInfo->m_hArchive);
	pArchInfo->m_hArchive = NULL;	//end enumerating
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
		memset(&pArchInfo->m_rar,0,sizeof(pArchInfo->m_rar));
		pArchInfo->m_rar.ArcName = (char *)pArchInfo->m_strFile.c_str();
		pArchInfo->m_rar.CmtBuf=NULL;	//CmtBuf
		pArchInfo->m_rar.CmtBufSize=0;	//sizeof(CmtBuf)
		pArchInfo->m_rar.OpenMode=RAR_OM_EXTRACT;

		HANDLE hArchive = RAROpenArchiveEx(&(pArchInfo->m_rar));
		if(hArchive)
			pArchInfo->m_hArchive = hArchive;
		else
			return false;

		RARSetCallback(pArchInfo->m_hArchive, CallbackProc, (LONG)pArchInfo);

		ZeroMemory(&(pArchInfo->m_HeaderData), sizeof(pArchInfo->m_HeaderData));
		pArchInfo->m_HeaderData.CmtBuf=NULL;

		bool bRes = false;
		int RHCode, PFCode;
		while ((RHCode=RARReadHeaderEx(pArchInfo->m_hArchive,&(pArchInfo->m_HeaderData)))==0)
		{
			if(MatchPaths(pArchInfo->m_HeaderData.FileName, szEntry))
			{
				pArchInfo->m_nProgressPos = 0;
				pArchInfo->m_strCurEntry = pArchInfo->m_HeaderData.FileName;
				if(pArchInfo->m_pfnProgress)
					pArchInfo->m_pfnProgress(pArchInfo->m_strCurEntry.c_str(), 0, pArchInfo->m_dwUserData);

				char szOutName[MAX_PATH];
				CharToOem(szDest, szOutName);
				PFCode=RARProcessFile(pArchInfo->m_hArchive,RAR_EXTRACT, NULL, szOutName);
				if(0 == PFCode)
					bRes = true;

				pArchInfo->m_strCurEntry = "";
				break;	//found file, can exit now
			}
			else
				PFCode=RARProcessFile(pArchInfo->m_hArchive,RAR_SKIP,NULL,NULL);

			//exit on error
			if (PFCode!=0)	break;
		}

		RARCloseArchive(pArchInfo->m_hArchive);
		pArchInfo->m_hArchive = NULL;
		return bRes;
	}

	return false;
}

//delete entry from open archive
bool DeleteEntry(int dwArchID, const char *szEntry)
{
	//not supported for RAR
	return false;	
}

void ConfigurationDlg (long Parent, void *DllInstance)
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
		pArchInfo->m_pfnProgress = pProcessDataProc;
		pArchInfo->m_dwUserData	 = dwUser;
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

int PASCAL CallbackProc(UINT msg,LONG UserData,LONG P1,LONG P2)
{
	CArchiveInfo *pArchInfo	= (CArchiveInfo *)UserData;
	if(pArchInfo)
	{
		switch(msg){
		case UCM_PROCESSDATA:
			//if(pArchInfo->m_bUnpacking)
			if(pArchInfo->m_strCurEntry.size()>0)
			{
				//report progress
				pArchInfo->m_nProgressPos += P2;
				if(pArchInfo->m_pfnProgress)
				{
					int nRes = pArchInfo->m_pfnProgress(pArchInfo->m_strCurEntry.c_str(), pArchInfo->m_nProgressPos, pArchInfo->m_dwUserData);
					if(0 == nRes)	//abort request
					{
						return 0; //TOFIX test
					}
				}
			}
			return 1;

		case UCM_NEEDPASSWORD:
			if(g_pfnPwdProc)
			{
				//P1 is the buffer, P2 is the size
				if(g_pfnPwdProc((char *)P1, P2, pArchInfo->m_dwUserData))
					return 1;
				
				return 0;	//TOFIX used?
			}

		case UCM_CHANGEVOLUME:
			if(P2 == RAR_VOL_ASK)
			{
				char szTitle[256];
				sprintf(szTitle, "Can not find volume file %s, please find it!");
				
				//TOFIX use tChangeVolProc
				//volume not found, ask user to find it
				char szFilePath[MAX_PATH] = "";

				OPENFILENAME ofn;
				ZeroMemory(&ofn, sizeof(ofn));
				ofn.lStructSize = sizeof(ofn);
				ofn.lpstrFilter = "Any file (*.*)\0*.*\0\0";
				ofn.lpstrTitle	= szTitle;
				ofn.lpstrFile	= szFilePath;
				ofn.nMaxFile	= sizeof(szFilePath);

				if(GetOpenFileName(&ofn))
				{
					//managed to open new file
					strcpy((char *)P1, ofn.lpstrFile);
					return 1;
				}

				return -1;	//failed to get name, abort
			}
			else
				return 1;	//RAR_VOL_NOTIFY
		}
	}

	return 1;
}