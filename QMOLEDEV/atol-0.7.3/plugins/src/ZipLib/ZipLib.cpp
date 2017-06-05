////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: <TOFIX>
////////////////////////////////////////////////////////////////////////////

#include "../plugin_int.h"
#include "ArchiveInfo.h"
#include "ZipArchive/ZipPlatform.h"
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef _WIN32
 #define stat _stat
#endif

//global data
COpenArchivesList  g_ArchList;
tPasswordProc      g_pfnPwdProc = NULL;
long               g_dwUser;

int MatchPaths(const char *szPath1, const char *szPath2);
int FindZipEntry(CZipArchive &zip, const char *szEntry);

#ifdef _WIN32
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
#endif

//
//	API implemetation
//

unsigned long GetArchiverCaps(const char *szExt)
{
	if(0 == strcmp(szExt, ".jar"))
		return PK_CAPS_MULTIPLE; //jar can only be read!

	return	PK_CAPS_NEW      |
	        PK_CAPS_MODIFY   |
	        PK_CAPS_MULTIPLE |
	        PK_CAPS_DELETE   |
	        PK_CAPS_REAL;
}

//return file extensions usually connected to this kind of archive
const char *GetExtensions()
{
	//NOTE: multiple extensions sample: ".gz;.tgz";
	static const char *pszExt = ".zip;.jar";
	return pszExt;
}

// create new archive or open existing one
int OpenArchive (const char *szFile)
{
	//create new CArchiveInfo
	CArchiveInfo *pArchInfo = new CArchiveInfo;
	if(NULL == pArchInfo)
		return 0;	//failure

	#ifdef _DEBUG
//		FILE *pOut = fopen("/atol_debug.log","a");
//		fprintf(pOut, "OpenArchive %s\n", szFile);
//		fclose(pOut);
	#endif

	//TOFIX first open, then add to map ???
	//add CArchiveInfo to the map
	int dwID = g_ArchList.Add(pArchInfo);
	if(dwID > 0)
	{
		int nMode = CZipArchive::zipOpen;
		
		// check if archive already exists
		if(0 != access(szFile, 00))
		{
			nMode = CZipArchive::zipCreate;
		}
		else
		{
		#ifdef _WIN32
			//archive exists, check if read only
			DWORD dwAttr = GetFileAttributes(szFile);
			if( 0xFFFFFFFF != dwAttr &&
				(dwAttr & FILE_ATTRIBUTE_READONLY))
			{
				nMode = CZipArchive::zipOpenReadOnly;
			}
		#else
			if(0 != access(szFile, W_OK))	//check write permission
				nMode = CZipArchive::zipOpenReadOnly;
		#endif
		}

		// open archive
		try{
			pArchInfo->m_zip.Open(szFile, nMode);
			return dwID;
		}
		catch(CZipException e)
		{
			//TOFIX notify error (get error text?)
			#ifdef _DEBUG
//				FILE *pOut = fopen("/atol_debug.log","a");
//				fprintf(pOut, "%s\n", e.GetErrorDescription().c_str());
//				fclose(pOut);
			#endif
			g_ArchList.Remove(dwID); //remove from map
			return 0;	//failure
		}
	}
	
	return 0;	//failure
}

// close opened (existing) archive
bool CloseArchive (int dwArchID)
{
	//if archive valid
	if(g_ArchList.IdExists(dwArchID))
	{
		//close zip file
		CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
		if(NULL != pArchInfo)
			pArchInfo->m_zip.Close();

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
		pArchInfo->m_nCurEntryPos = 0;
}

int GetNextEntry(int dwArchID, tArchiveEntry *pInfo)
{
	//enumeration - get next entry
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	//ASSERT + if

	if(NULL != pInfo)
	{
		CZipFileHeader fh;
		if(pArchInfo->m_zip.GetFileInfo(fh, pArchInfo->m_nCurEntryPos))
		{
			strncpy(pInfo->szPath, fh.GetFileName(), sizeof(pInfo->szPath)-1);
			pInfo->szPath[sizeof(pInfo->szPath)-1] = '\0';

			if(fh.IsDirectory())
			{
				pInfo->bDir = true;
			}
			else
			{
				pInfo->bDir		= false;
				pInfo->nUnpSize = fh.m_uUncomprSize;
			}
			
			pInfo->tmModified = fh.GetTime();

			//convert attributes to portable flags
			unsigned long dwAttr = fh.GetSystemAttr();
			pInfo->dwAttribs = 0;
#ifdef _WIN32
			if(dwAttr & FILE_ATTRIBUTE_ARCHIVE)		pInfo->dwAttribs |= ATTR_ARCH;
			if(dwAttr & FILE_ATTRIBUTE_DIRECTORY)   pInfo->dwAttribs |= ATTR_DIR;
			if(dwAttr & FILE_ATTRIBUTE_HIDDEN)      pInfo->dwAttribs |= ATTR_HIDDEN;
			if(dwAttr & FILE_ATTRIBUTE_READONLY)    pInfo->dwAttribs |= ATTR_RONLY;
			if(dwAttr & FILE_ATTRIBUTE_SYSTEM)      pInfo->dwAttribs |= ATTR_SYSTEM;
#else
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
#endif

			pInfo->nPackSize  = fh.m_uComprSize;

			pArchInfo->m_nCurEntryPos ++;
			return 1;
		}
	}
	
	//enumeration reinitialised
	pArchInfo->m_nCurEntryPos = 0;
	return 0;
}

//other operations
void ProcessMultiple(int dwArchID, int nOperation)
{

}

void EndProcessMulti(int dwArchID)
{
}

//TOFIX compression level 
//pack file into the archive
bool PackFile(int dwArchID, const char *szFile, const char *SubPath, const char*szDestName)
{
	#ifdef _DEBUG
//		FILE *pDbg = fopen("/atol_debug.log", "a");
//		fprintf(pDbg, "ZipLib: pack file %s\n", szFile);
//		fclose(pDbg);
	#endif

	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		//TOFIX ensure buffer not owerflowed
		char szPath[1256];
		strcpy(szPath, SubPath);
		strcat(szPath, szDestName);

		//FIX: paths stored in archive should be relative, not absolute paths
		char *szDstPath = szPath;
		while(*szDstPath == '/' || *szDstPath == '\\')
			szDstPath ++;

		#ifdef _DEBUG
//			FILE *pDbg = fopen("/atol_debug.log", "a");
//			fprintf(pDbg, "ZipLib: pack %s to %s\n", szPath, szDstPath);
//			fclose(pDbg);
		#endif

		CZipFileHeader fh;
		fh.SetFileName(szDstPath);	//define the name of the new zip entry

		//TOFIX 64 bit file support
		FILE *pInput = fopen(szFile, "rb");
		if(NULL != pInput)
		{
			char szBuffer[10000];
			UINT nRead, nTotal = 0;

			//NOTE: we pass local file path, to copy time/attributes from
			pArchInfo->m_zip.OpenNewFile(fh, Z_DEFAULT_COMPRESSION, szFile);

			if(NULL != pArchInfo->m_pfnProgress)
				pArchInfo->m_pfnProgress("", 0, pArchInfo->m_dwUserData);

			while(0 != (nRead = fread(szBuffer, 1, sizeof(szBuffer), pInput)))
			{
				//TOFIX check result
				pArchInfo->m_zip.WriteNewFile((void *)szBuffer, nRead);

				nTotal += nRead;

				if(NULL != pArchInfo->m_pfnProgress)
				{
					int nRes = pArchInfo->m_pfnProgress("", nTotal, pArchInfo->m_dwUserData);
					if(0 == nRes)	//abort request
					{
						pArchInfo->m_zip.CloseNewFile();

						//delete partial file
						int nIdx = pArchInfo->m_zip.FindFile(szDstPath);
						if(nIdx >= 0)
							pArchInfo->m_zip.DeleteFile(nIdx);

						fclose(pInput);
						return false;
					}
				}

			}

			pArchInfo->m_zip.CloseNewFile();
			fclose(pInput);
			return true;
		}
		else
		{
		#ifdef _DEBUG
			printf("ZipLib: failed to open file %s\n", szFile);
		#endif
		}
	}

	return false;
}

bool MakeDir(int dwArchID, const char *szDir)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		//NOTE my file matching -> / or \ allowed
		int nZipID = FindZipEntry(pArchInfo->m_zip, szDir);
		if(nZipID == -1)
		{
			CZipFileHeader fh;
			#ifdef _WIN32
			   pArchInfo->m_zip.SetFileHeaderAttr(fh, FILE_ATTRIBUTE_DIRECTORY);
			#else
			   unsigned long uAttr = (S_IFDIR|S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IWGRP|S_IWGRP|S_IROTH|S_IWOTH|S_IXOTH) << 16;
			   pArchInfo->m_zip.SetFileHeaderAttr(fh, uAttr);
			#endif

			//FIX: paths stored in archive should be relative, not absolute paths
			const char *szDstPath = szDir;
			while(*szDstPath == '/' || *szDstPath == '\\')
				szDstPath ++;

			fh.SetFileName(szDstPath);
			bool bRes = false;
			try{
				bRes = pArchInfo->m_zip.OpenNewFile(fh, 0);
				pArchInfo->m_zip.CloseNewFile();
			}
			catch(CZipException e)
			{
				//TOFIX notify error using callback
				#ifdef _DEBUG
//					FILE *pOut = fopen("/atol_debug.log","a");
//					fprintf(pOut, "%s\n", e.GetErrorDescription().c_str());
//					fclose(pOut);
				#endif
				//if(ex.m_iCause == CZipException::badPassword)
				return false;
			}
			return bRes;
		}
		else
			return true;	//already exists TOFIX what if not dir
	}

	return false;
}

//TOFIX handle errors
//unpack archive entry
bool UnpackFile(int dwArchID, const char *szEntry, const char *szDest/*, bool *bAbort*/)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		//NOTE my file matching -> / or \ allowed
		int nZipID = FindZipEntry(pArchInfo->m_zip, szEntry);

		if(nZipID != -1)
		{
			CZipFileHeader fh;
			if(pArchInfo->m_zip.GetFileInfo(fh, (WORD)nZipID))
			{
				//TOFIX podrska za encrypted files
				if(fh.IsEncrypted())
				{
					char szPwdChar[256] = "";
					if(NULL != g_pfnPwdProc)
					{
						g_pfnPwdProc(szPwdChar, sizeof(szPwdChar), g_dwUser);
						pArchInfo->m_zip.SetPassword(szPwdChar);
					}
				}

				//TOFIX ? pArchInfo->m_zip.ExtractFile(nZipID, szDest, true)
				try{
					if(!pArchInfo->m_zip.OpenFile(nZipID))
						return false;
				}
				catch(CZipException e)
				{
					//TOFIX notify error using callback
					#ifdef _DEBUG
//						FILE *pOut = fopen("/atol_debug.log","a");
//						fprintf(pOut, "%s\n", e.GetErrorDescription().c_str());
//						fclose(pOut);
					#endif
					//if(ex.m_iCause == CZipException::badPassword)
					return false;
				}

				//TOFIX 64 bit file support
				FILE *pOut = fopen(szDest, "wb");
				if(NULL != pOut)
				{
					char szBuffer[10000];
					UINT nRead, nTotal = 0;
				
					if(NULL != pArchInfo->m_pfnProgress)
						pArchInfo->m_pfnProgress("", 0, pArchInfo->m_dwUserData);

					int nOK = 1;

					while(0 != (nRead = pArchInfo->m_zip.ReadFile(szBuffer, sizeof(szBuffer))))
					{
						//TOFIX check result
						fwrite(szBuffer, 1, nRead, pOut);

						nTotal += nRead;

						if(NULL != pArchInfo->m_pfnProgress)
						{
							nOK = pArchInfo->m_pfnProgress("", nTotal, pArchInfo->m_dwUserData);
							if(0 == nOK)	//abort request
								break;
						}
					}
					
					fclose(pOut);

					if(0 == nOK)
					{
						remove(szDest); //clear partial result
						//TOFIX return false for this case
					}
					else
					{
						//copy attributes from zip entry to new file
						ZipPlatform::SetFileAttr(szDest, fh.GetSystemAttr());
						ZipPlatform::SetFileModTime(szDest, fh.GetTime());
					}
				}

				//close zip entry
				try{
					pArchInfo->m_zip.CloseFile(NULL);
				}
				catch(CZipException e)
				{
					//TOFIX print error?
					//TOFIX notify error (get error text?)
					#ifdef _DEBUG
//						FILE *pOut = fopen("/atol_debug.log","a");
//						fprintf(pOut, "%s\n", e.GetErrorDescription().c_str());
//						fclose(pOut);
					#endif
					//e.m_iCause;
					return false;
				}

				return true;
			}
		}
	}

	return false;
}

//delete entry from open archive
bool DeleteEntry(int dwArchID, const char *szEntry)
{
	CArchiveInfo *pArchInfo = g_ArchList.Find(dwArchID);
	if(NULL != pArchInfo)
	{
		//int nID = pArchInfo->m_zip.FindFile(szEntry, false);
		int nID = FindZipEntry(pArchInfo->m_zip, szEntry);
		if(nID != -1){
			try{
				pArchInfo->m_zip.DeleteFile(nID);
			}
			catch(CZipException e)
			{
				return false;
			}
			return true;
		}
		//else
		//	TRACE("ZIP error: Failed to find entry %s\n", szEntry);
	}

	return false;	
}

void ConfigurationDlg (long Parent, void *DllInstance)
{
	//TOFIX GUI supoprt?
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

//TOFIX very slow -> store map name->ID to spped up (must be refreshed at each change)
//my file matching -> / or \ allowed
int FindZipEntry(CZipArchive &zip, const char *szEntry)
{
	CZipFileHeader fh;
	int nSize = zip.GetCount();

	for(int i=0; i<nSize; i++)
	{
		if(zip.GetFileInfo(fh, i))
		{
			CZipString strPath = fh.GetFileName();
			if(MatchPaths(strPath.c_str(), szEntry))
				return i;
		}
	}

	return -1;
}

//add to CPath ?
int MatchPaths(const char *szPath1, const char *szPath2)
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

void SetPasswordProc(tPasswordProc pPasswdProc, long dwUser)
{
	g_pfnPwdProc = pPasswdProc;
	g_dwUser	 = dwUser;
}
