////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: 
////////////////////////////////////////////////////////////////////////////

#include "../plugin_int.h"
#include "ArchiveInfo.h"
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#ifndef _WIN32
 #include <ctype.h>
 #include <utime.h>
#endif

//global data
COpenArchivesList	g_ArchList;
tPasswordProc		g_pfnPwdProc = NULL;
long				g_dwUser;

bool MatchPaths(const char *szPath1, const char *szPath2);

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
	//this version can only read
	return	PK_CAPS_REAL|PK_CAPS_MULTIPLE;	//archive can contain multiple files
}

//return file extensions usually connected to this kind of archive
const char *GetExtensions()
{
	static const char *pszExt = ".tar";
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

		if(!pArchInfo->m_objTar.Open(szFile))
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
		pArchInfo->m_objTar.Close();

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
		//reset listing
		pArchInfo->m_objTar.Reset();
	}
}

int GetNextEntry(int dwArchID, tArchiveEntry *pInfo)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		CTarRec Rec;
		if(pArchInfo->m_objTar.FindNext(Rec))
		{
			pArchInfo->m_strCurEntry = Rec.Name;
			strncpy(pInfo->szPath, Rec.Name.c_str(), sizeof(pInfo->szPath)-1);
			pInfo->szPath[sizeof(pInfo->szPath)-1] = '\0';
			
			pInfo->nPackSize	= Rec.Size;
			pInfo->nUnpSize		= Rec.Size;
			//pInfo->tmModified	= Rec.DateTime; //TOFIX
			pInfo->bDir			= (ftDirectory == (Rec.FileType & ftDirectory));

			//convert attributes to portable flags
			pInfo->dwAttribs	= ATTR_UNIX;
			if(pInfo->bDir)
				pInfo->dwAttribs |= ATTR_DIR;
			if((Rec.FileType & ftLink) || (Rec.FileType & ftSymbolicLink))
				pInfo->dwAttribs |= ATTR_LINK;

			if((Rec.Permissions & tpReadByOwner)    != 0 )  pInfo->dwAttribs |= ATTR_R_USR;
			if((Rec.Permissions & tpWriteByOwner)   != 0 )  pInfo->dwAttribs |= ATTR_W_USR;
			if((Rec.Permissions & tpExecuteByOwner) != 0 )  pInfo->dwAttribs |= ATTR_X_USR;
			if((Rec.Permissions & tpReadByGroup)    != 0 )  pInfo->dwAttribs |= ATTR_R_GRP;
			if((Rec.Permissions & tpWriteByGroup)   != 0 )  pInfo->dwAttribs |= ATTR_W_GRP;
			if((Rec.Permissions & tpExecuteByGroup) != 0 )  pInfo->dwAttribs |= ATTR_X_GRP;
			if((Rec.Permissions & tpReadByOther)    != 0 )  pInfo->dwAttribs |= ATTR_R_OTH;
			if((Rec.Permissions & tpWriteByOther)   != 0 )  pInfo->dwAttribs |= ATTR_W_OTH;
			if((Rec.Permissions & tpExecuteByOther) != 0 )  pInfo->dwAttribs |= ATTR_X_OTH;
			
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
	//currently not supported
	return false;
}

bool MakeDir(int dwArchID, const char *szDir)
{
	//currently not supported
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
			
			//position to the proper file in archive
			bool bFound = false;
			CTarRec Rec;
			pArchInfo->m_objTar.Reset();
			while(pArchInfo->m_objTar.FindNext(Rec)){
				if(MatchPaths(szEntry, Rec.Name.c_str())){
					bFound = true;
					break;
				}
			}
			if(!bFound){
				fclose(pFileOut);
				return false;
			}
			
			while(0 < (nRead = pArchInfo->m_objTar.ReadFile(szBuffer, sizeof(szBuffer))))
			{
				if(nRead != fwrite(szBuffer, 1, nRead, pFileOut)){
					fclose(pFileOut);
					return false;
				}

				nTotalRead += nRead;
				if(pArchInfo->m_pfnProgress)
				{
					int nRes = pArchInfo->m_pfnProgress(pArchInfo->m_strCurEntry.c_str(), nTotalRead, pArchInfo->m_dwUserData);
					if(0 == nRes){ //abort request
						fclose(pFileOut);
						remove(szDest); //remove partial file
						return false;
					}
				}
			}
		
			fclose(pFileOut);
		#ifndef _WIN32
			//set file permissions and modified time to match original file
			chmod(szDest, Rec.Permissions);
			struct utimbuf ub;
			ub.actime = time(NULL);
			ub.modtime = Rec.DateTime;
			utime(szDest, &ub);
		#endif
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

void ConfigurationDlg (long Parent, void* DllInstance)
{
	//TOFIX not supported?
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
