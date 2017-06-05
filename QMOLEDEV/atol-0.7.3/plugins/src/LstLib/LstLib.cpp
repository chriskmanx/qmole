////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements Atol plugin interface for LstLib plugin
////////////////////////////////////////////////////////////////////////////

#include "../plugin_int.h"
#include "ArchiveInfo.h"
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

//global data
COpenArchivesList g_ArchList;
tPasswordProc     g_pfnPwdProc = NULL;
long              g_dwUser;

#ifdef _WIN32
 #define stat _stat
 #include <windows.h>
#else
 #include <ctype.h>
#endif

#ifdef _WIN32
BOOL APIENTRY DllMain( HANDLE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved )
{
	return TRUE;
}
#endif

//
//	API implemetation
//

unsigned long GetArchiverCaps(const char *szExt)
{
	return  PK_CAPS_NEW|
	        PK_CAPS_MODIFY|
	        PK_CAPS_MULTIPLE|
	        PK_CAPS_DELETE;
}

//return file extensions usually connected to this kind of archive
const char *GetExtensions()
{
	static const char *pszExt = ".lst;";
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

//		if(!pArchInfo->m_objLst.Open(szFile))
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
		pArchInfo->m_objLst.Close();

		//write content back to file
		if(pArchInfo->m_bChanged)
		{
			if(pArchInfo->m_objLst.Open(pArchInfo->m_strFile.c_str(), false))	//open to write
			{
				pArchInfo->m_objLst.WriteList();
				pArchInfo->m_objLst.Close();
			}
		}

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
		if(!pArchInfo->m_objLst.IsOpen())
		{
			pArchInfo->m_objLst.Open(pArchInfo->m_strFile.c_str());	//open to read
			pArchInfo->m_objLst.ReadList();
		}

		pArchInfo->m_nCurEntry = 0;
	}
}

int GetNextEntry(int dwArchID, tArchiveEntry *pInfo)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		int i = pArchInfo->m_nCurEntry;

		CEntry *pEntry = pArchInfo->m_objLst.m_lstFiles.GetEntryRecursive(i);
		if(pEntry)
		{
			std::string strEntry = pEntry->strFullPath;

			strncpy(pInfo->szPath, strEntry.c_str(), sizeof(pInfo->szPath)-1);
			pInfo->szPath[sizeof(pInfo->szPath)-1] = '\0';

			pInfo->nPackSize  = 0;
			pInfo->nUnpSize   = pEntry->nSize;
			pInfo->tmModified = pEntry->tmModified;
			pInfo->bDir       = pEntry->bDir;
			pInfo->dwAttribs  = 0;	//TOFIX not supported

			pArchInfo->m_nCurEntry ++;
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
	//NOTE: catalog file always stores original file name !!!!
	//		(SubPath, szDestName are not being used)

	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		//prepare item to add it into the file list
		std::string strName(szFile);
		int nPos = strName.find_last_of('\\');
		if(nPos >= 0)
			strName = strName.substr(nPos+1, strName.size());
		nPos = strName.find_last_of('/');	// Linux delimiter support
		if(nPos >= 0)
			strName = strName.substr(nPos+1, strName.size());

		std::string strPath(szFile);

		tItem item;
		item.strFile	 = strName;
		item.strFullPath = strPath;

		//TOFIX check if the root matches existing root 
		//if(!pArchInfo->m_objLst.Reorganize(szFile))
		//	return false;

		//TOFIX what if not local file but ftp?
		struct stat buf;
		if(0 == stat(szFile, &buf))
		{
			item.nSize = buf.st_size;
			item.tmModified = buf.st_mtime;
		#ifdef _WIN32
			item.bDir  = (0 != (_S_IFDIR & buf.st_mode));
		#else
			item.bDir  = S_ISDIR(buf.st_mode);
		#endif
		}

		//TOFIX: break path to file name item and subdir item relative to the root (if exists)
		CEntry *pEntry = pArchInfo->m_objLst.m_lstFiles.Insert(szFile);
		if(!pEntry)
			return false;

		// mark as changed
		pArchInfo->m_bChanged = true;

		int nRootSize = pArchInfo->m_objLst.m_lstFiles.m_strRoot.size();
		if(nRootSize > 0)
			item.strFullPath = item.strFullPath.substr(nRootSize, item.strFullPath.size());	//remove root part!

		*pEntry = item;

		//update all created parent directories 
		//"C:\dir1\dir2\dir3\file" -> "C:\dir1\dir2\dir3", "C:\dir1\dir2", ...
		strPath = item.strFullPath;

		while(strPath.size()>0)
		{
			bool bFound = false;
			int nPos = strPath.find_last_of('\\');
			if(nPos >= 0){
				strPath = strPath.substr(0, nPos);
				bFound = true;
			}
			else
			{
				nPos = strPath.find_last_of('/');	// Linux delimiter support
				if(nPos >= 0){
					strPath = strPath.substr(0, nPos);
					bFound = true;
				}
			}

			if(0 == strPath.size())
				break;

			CEntry *pEnt = pArchInfo->m_objLst.m_lstFiles.FindItem(strPath.c_str());
			if(pEnt)
			{
				pEnt->strFullPath = strPath;
				pEnt->bDir = true;
				pEnt->nSize = 0;

				struct stat buf;
				if(0 == stat(strPath.c_str(), &buf))
					pEnt->tmModified = buf.st_mtime;
			}

			if(!bFound)
				break;
		}

		//actual writing to the file is moved to CloseArchive
		return true;
	}

	return false;
}

bool MakeDir(int dwArchID, const char *szDir)
{
	//TOFIX
	return false;
}

//TOFIX handle errors
//unpack archive entry
bool UnpackFile(int dwArchID, const char *szEntry, const char *szDest)
{
	return false; //not supported (TOFIX support?)
}

//delete entry from open archive
bool DeleteEntry(int dwArchID, const char *szEntry)
{
	//currently not supported
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		std::string strDirPath(szEntry);
		int nPos = strDirPath.find_last_of('\\');
		if(nPos >= 0)
			strDirPath = strDirPath.substr(0, nPos);
		nPos = strDirPath.find_last_of('/');	// Linux delimiter support
		if(nPos >= 0)
			strDirPath = strDirPath.substr(0, nPos);

		//find proper list where entry is stored
		std::vector<CEntry> *pList = pArchInfo->m_objLst.m_lstFiles.FindList(strDirPath.c_str());
		if(pList)
		{
			//prepare item to add it into the file list
			std::string strName(szEntry);
			int nPos = strName.find_last_of('\\');
			if(nPos >= 0)
				strName = strName.substr(nPos+1, strName.size());
			nPos = strName.find_last_of('/');	// Linux delimiter support
			if(nPos >= 0)
				strName = strName.substr(nPos+1, strName.size());

			//find the entry itself
			CEntry item;
			item.strFile = strName;
     
			#ifdef _DEBUG
				//pArchInfo->m_objLst.m_lstFiles.Dump();
			#endif

			std::vector<CEntry>::iterator It = std::find(pList->begin(), pList->end(), item);
			if(It == pList->end())
				return false;

			//now delete the list entry at the given position
			pList->erase(It);

			pArchInfo->m_bChanged = true;	// mark modified
			return true;
		}
	}
	
	return false;	
}

void ConfigurationDlg (long Parent, void *DllInstance)
{
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
