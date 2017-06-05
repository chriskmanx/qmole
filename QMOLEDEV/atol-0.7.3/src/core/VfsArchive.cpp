////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Vfs_Archive implementation
////////////////////////////////////////////////////////////////////////////

#include "VfsArchive.h"
#include "PathName.h"
#include "PluginManager.h"
#include <stdio.h>
#include "debug.h"
#include "File64.h"

#ifndef _WIN32
 #include<unistd.h>
#endif

extern PluginManager g_PlugManager;
int ProgressProc(const char *FileName,int Size, int dwUser);

Vfs_Archive::Vfs_Archive()
{
	m_nType = ARCHIVE;
	m_pProgress = NULL;
	m_bReadOnly = false;
	m_pPlugin = NULL;
	m_bDeleteFile = false;
	m_strCurDir = "";	
	m_hArchive = 0;
	m_bReadFile = true;
}

Vfs_Archive::~Vfs_Archive()
{
}

void Vfs_Archive::FixPath(String &strPath)
{
#ifdef _WIN32
	strPath.Replace("/", "\\");
#else
	strPath.Replace("\\", "/");
#endif
}

bool Vfs_Archive::Open()
{
	ASSERT(0 == m_hArchive);
	ASSERT(NULL != m_pPlugin);
	
	if(NULL != m_pPlugin)
	{
		//check if read only -> TOFIX move to system method?
	#ifdef _WIN32
		int dwAttr = GetFileAttributes(m_strArchiveFile);
		if( 0xFFFFFFFF != dwAttr && (dwAttr & FILE_ATTRIBUTE_READONLY))
			m_bReadOnly = true;
		else
			m_bReadOnly = false;
	#else
		if(0 == access(m_strArchiveFile.c_str(), 00))	//check exists
			m_bReadOnly = (0 != access(m_strArchiveFile.c_str(), W_OK));	//check write permission
		else
			m_bReadOnly = false;	//new archive
	#endif
		TRACE("Vfs_Archive::Open read-only=%d\n", m_bReadOnly);

		m_bReadFile = true;

		//tOpenArchiveData data;
		//strcpy(data.ArcName, m_strArchiveFile); //TOFIX?
		m_hArchive = m_pPlugin->m_pfnOpenArchive(m_strArchiveFile); //TOFIX readonly as param
		ASSERT(m_hArchive > 0);
        
		return (m_hArchive > 0);
    }
	
    return false;
}

bool Vfs_Archive::Close()
{
	//ASSERT(NULL != m_pPlugin);
	if(NULL != m_pPlugin)
	{
		m_pPlugin->m_pfnCloseArchive(m_hArchive);
		m_hArchive = 0;
		
		//TOFIX clean other data
		m_lstTree.Clear();
	}
	
	//for "archive in archive" delete file from temp dir after disconnect
	if(m_bDeleteFile){
		TRACE("Remove archive file %s\n", m_strArchiveFile.c_str());
		remove(m_strArchiveFile.c_str());
	}
	
	return false;
}

bool Vfs_Archive::SetDir(const char *szPath)
{
	String strDir(szPath);
	PathName::EnsureTerminated(strDir, '/');

	//refresh archive contents
	if(m_bReadFile || m_lstTree.IsEmpty())
		BuildTree();

	//check if directory exists
	if(NULL != m_lstTree.Find(strDir))
	{
		m_strCurDir = strDir;
		return true;
	}
	
	return false;
}

bool Vfs_Archive::Rename(const char *szFrom, const char *szTo)
{
	return false;    //not supported
}

//NOTE: single file / empty dir delete
bool Vfs_Archive::Delete(VfsItem &item, int &nOpSettings)
{
	//ASSERT(NULL != m_pPlugin);
	//ASSERT(0 != m_hArchive);
	
	//check if archiver supports operation 
	if(!CheckArchiverCaps(PK_CAPS_DELETE))
		return false;
	
	if(m_bReadOnly){
		//TOFIX threadsafe wxMessageBox(_("Read-only archive!"),_("Warning"));
		return false;
	}
	
	if(NULL != m_pPlugin)
	{
		//TOFIX stripPrefix
		PathName::EnsureTerminated(m_strCurDir, '/');
		String strPath(m_strCurDir);
		strPath += item.GetName();
        
		//TOFIX ensure not prefixed with \ or / -> to detect exit from archive
		if(strPath.Length()>0)
			if(strPath.GetAt(0) == '\\' ||
			   strPath.GetAt(0) == '/' )
				strPath = strPath.Right(strPath.Length()-1);
			
		//TOFIX
		strPath.Replace("/","\\");
			
		//init plugin progress callback
		if(NULL != m_pPlugin->m_pfnSetProcessDataProc)
			m_pPlugin->m_pfnSetProcessDataProc(m_hArchive, ProgressProc, (long)this);
			
		//init progress range indicator
		//ASSERT(NULL != m_pProgress);
		//NOTE do this before initsingle
		//m_pProgress->InitFileNames(strPath, NULL);
		//m_pProgress->InitSingle(1);
			
		bool bRes = false;
		if(NULL != m_pPlugin->m_pfnDeleteEntry){
			bRes = m_pPlugin->m_pfnDeleteEntry(m_hArchive, strPath);
			//if(!bRes)
			//    TRACE("ERROR: failed to delete %s from archive %s\n", strPath, m_strArchiveFile);
		}
			
		//m_pProgress->SingleSetPos(1);
		m_bReadFile = true;    //operation changed archive - read tree again
		return bRes;
	}
	
	return false;
}

bool Vfs_Archive::MkDir(const char *szDir)
{
	if(!CheckArchiverCaps(PK_CAPS_MODIFY))
		return false;
	
	if(m_bReadOnly){
		//TOFIX wxMessageBox(_("Read-only archive!"),_("Warning"));
		return false;
	}
	
	//ASSERT(NULL != m_pPlugin);
	//ASSERT(0 != m_hArchive);
	
	if(NULL != m_pPlugin)
	{
		String strPath = m_strCurDir;
		PathName::EnsureTerminated(strPath, '/');
		strPath += szDir;
		
		//force creating directory in archive
		bool bRes = false;
		if(m_pPlugin->m_pfnMakeDir)
			bRes = m_pPlugin->m_pfnMakeDir(m_hArchive, strPath);
        
		m_bReadFile = true;    //operation changed archive - read tree again
		return bRes;
	}
	
	return false;
}

bool Vfs_Archive::ListDir(VfsListing &list, bool &bAbort)
{
	list.Clear();
	
	if(m_bReadFile || m_lstTree.IsEmpty())
		BuildTree();
	
	//default entry -> TOFIX special fn to init default entry?
	VfsItem info;
	info.m_nAttrib = ATTR_DIR;
	info.SetName("..");
	list.Insert(info);
	
	//TOFIX support for apsolute zip entries like "C:\aaa\bb.txt"
	std::vector<VfsSelectionItem> *pListLevel = m_lstTree.Find(m_strCurDir);
	if(NULL != pListLevel)
	{
		int nCount = pListLevel->size();
		for(int i=0; i<nCount; i++)
		{
			//TRACE("Testing zip entry: %s\n", info.m_strName);
			info = (*pListLevel)[i];
			list.Insert(info);
		}
	}
	
	//TOFIX? keep archive closed most of the time
	return true;    //TOFIX
}

bool Vfs_Archive::InitArchiver(const char * szFileName, bool canReadFile)
{
	//store file name for later opening
	m_strArchiveFile = szFileName;
	
	//find plugin using extension (case insensitive)
	String strExt = PathName::GetExt(szFileName);
	strExt.Lower();
	
	m_pPlugin = g_PlugManager.FindArchiver(strExt);
	
	//try to recognise some archives by their header (.zip)
	if(m_pPlugin == NULL && canReadFile) {
		File64 myfile;
		if(myfile.Open(szFileName, F64_READ|F64_SHARE_READ|F64_OPEN_EXISTING)){
			char buf[2];
			if(myfile.Read(buf, 2) == 2 && buf[0] == 'P' && buf[1] == 'K') {
				m_pPlugin = g_PlugManager.FindArchiver(".zip");
			}
			myfile.Close();
		}
	}
	
	return (m_pPlugin != NULL);
}

void Vfs_Archive::BuildTree()
{
	m_lstTree.Clear();
	
	if(NULL != m_pPlugin)
	{
		m_pPlugin->m_pfnInitEntryEnum(m_hArchive);
		
		tArchiveEntry info;
		VfsItem fileInf;
		
		int i=0;
		while(1)
		{
			//init info 
			memset(&info, 0, sizeof(info));
			
			if(!m_pPlugin->m_pfnGetNextEntry(m_hArchive, &info))
				break;
			
			//TRACE("Archive entry: %s\n", info.szPath);
			
			//NOTE: it is extremely important that also MatchPaths() support this!!!! (so the file can be unpacked)
			//FIX for relative paths like "./dir/file"
			//FIX for strange entries found (like "././@LongLink")
			const char *pszEntry = info.szPath;
			while(0 == strncmp("./", pszEntry, 2))    
				pszEntry += 2;
			
			//
			VfsItem *pEntry = m_lstTree.Insert(pszEntry);
			if(NULL != pEntry)
			{
				String strName(pszEntry);
				PathName::EnsureNotTerminated(strName);
				
				pEntry->m_strName = PathName::GetBaseName(strName);
				ASSERT(pEntry->m_strName.Length()>0); //sanity check

				if(!pEntry->IsDir()){    //FIX for dir entries that have invalid attributes
					pEntry->m_nSize   = info.nUnpSize;
					pEntry->m_nAttrib = info.dwAttribs;
				}
				pEntry->m_nLastModDate = info.tmModified;

				//NOTE: FIX some "bad" archives have not directory flag set for some directories
				if( ('\\' == info.szPath[strlen(info.szPath)-1]  ||
				     '/'  == info.szPath[strlen(info.szPath)-1]) &&
				    !info.bDir)
				{
					pEntry->m_nAttrib = ATTR_DIR;
					pEntry->m_nSize      = -1;
				}
				pEntry->CalcExt();
			}
			
			i++;
		}
	}
	
	//TOFIX fix dates for "..", dirs, ...
	#ifdef _DEBUG
		m_lstTree.Dump();    //debuging
	#endif
	m_bReadFile = false; //done reading the tree
}

bool Vfs_Archive::Copy(VfsItem &item, const char *szNewName, Vfs *pVfsDest, INT64 nOffset)
{
	if(pVfsDest->GetType() == Vfs::LOCAL || pVfsDest->GetType() == Vfs::NET)
	{
		String strDest = PathName::ComposePath(pVfsDest->GetDir(), szNewName);
		return CopyToLocal(item, strDest);
	}
	else
	{
		//create temporary path name
		String strTmpPath = PathName::ComposePath(PathName::Path_TempDirectory(), item.GetName());
		//TOFIX strTmpPath = wxFileName::CreateTempFileName(strTmpPath);
		
		//TOFIX handle double progress (unpack + copy/pack)???
		if( CopyToLocal(item, strTmpPath) &&
		    pVfsDest->CopyFromLocal(strTmpPath, item))
		{
			return true;
		}
	}
	
	return false;
}

bool Vfs_Archive::CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset)
{
	if(!CheckArchiverCaps(PK_CAPS_MODIFY))
	{
		TRACE("Archiver: no modify capability for this format\n");
		return false;
	}
	
	if(m_bReadOnly){
		TRACE("Archiver: read only archive\n");
		//TOFIX wxMessageBox(_("Read-only archive!"),_("Warning"));
		return false;
	}
	
	ASSERT(NULL != m_pPlugin);
	ASSERT(0 != m_hArchive);

	if(NULL != m_pPlugin)
	{
		//init plugin progress callback
		if(NULL != m_pPlugin->m_pfnSetProcessDataProc)
			m_pPlugin->m_pfnSetProcessDataProc(m_hArchive, ProgressProc, (long)this);
		
		//NOTE do this before initsingle
		PathName::EnsureTerminated(m_strCurDir, '/');
		
		if(NULL != m_pProgress){
			String strDst = m_strCurDir;
			strDst += item.GetName();
			m_pProgress->InitCurrentFiles(szLocalPath, strDst);
			m_pProgress->InitCurrentProgress(0, item.m_nSize); //init progress range indicator
		}
		
		bool bRes = false;
		
		//TRACE("Archiver: pack %s to %s\n", szLocalPath, m_strCurDir.c_str());
		
		//PackFile(int dwArchID, const char *szFile, const char *SubPath, const char*szDestName);
		if(NULL != m_pPlugin->m_pfnPackFile)
		{
			//TRACE("Archiver: before pack\n");
			bRes = m_pPlugin->m_pfnPackFile(m_hArchive, szLocalPath, m_strCurDir.c_str(), item.GetName().c_str());
			//TRACE("Archiver: pack result %d\n", bRes);
		}
		else
		{
			TRACE("Archiver: Error! no pack method found!\n");
		}
		
		m_bReadFile = true;    //operation changed archive - read tree again
		return bRes;
	}
	
	return false;
}

bool Vfs_Archive::CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset)
{
	ASSERT(NULL != m_pPlugin);
	ASSERT(0 != m_hArchive);
	
	if(NULL != m_pPlugin)
	{
		PathName::EnsureTerminated(m_strCurDir, '/');  //TOFIX
		String strPath(m_strCurDir);
		strPath += item.GetName();
		strPath.Replace("\\","/");                     //TOFIX
		
		//init plugin progress callback
		if(NULL != m_pPlugin->m_pfnSetProcessDataProc)
			m_pPlugin->m_pfnSetProcessDataProc(m_hArchive, ProgressProc, (long)this);
		
		//init progress range indicator
		if(NULL != m_pProgress){
			//NOTE do this before initsingle
			m_pProgress->InitCurrentFiles(strPath, szLocalPath);
			m_pProgress->InitCurrentProgress(0, item.m_nSize /*not good for recursion -> GetItemSize(szItem)*/);
		}
		
		//UnpackFile(int dwArchID, const char *szEntry, const char *szDest);
		if(NULL != m_pPlugin->m_pfnUnpackFile)
			return m_pPlugin->m_pfnUnpackFile(m_hArchive, strPath, szLocalPath);
	}
	
	return false;
}

int ProgressProc(const char *FileName,int Size, int dwUser)
{
	Vfs_Archive *pArchive = (Vfs_Archive *)dwUser;
	
	if( NULL != pArchive && 
	    NULL != pArchive->m_pProgress)
	{
		pArchive->m_pProgress->SetPos(Size);
		
		//notify operation abort if requested
		if(pArchive->m_pProgress->IsAborted())
			return 0;
	}
	return 1;
}

INT64 Vfs_Archive::GetDriveFreeSpace()
{
	//TOFIX get free space for underlying local Vfs (unless some archive format has also size restrictions)
	return 1000000000; 
	//return RealGetDiskFreeSpace(m_strArchiveFile.Left(3));
}

bool Vfs_Archive::CheckArchiverCaps(int dwCapsRequested)
{
	//check if archiver supports operation 
	if(NULL != m_pPlugin && NULL != m_pPlugin->m_pfnGetArchiverCaps)
	{
		String strExt = PathName::GetExt(m_strArchiveFile);
		int dwCapsArchiver = m_pPlugin->m_pfnGetArchiverCaps(strExt.c_str());
		if(0 == (dwCapsRequested & dwCapsArchiver))
		{
			//TOFIX wxMessageBox(_("Archiver does not support this operation!"),_("Warning"));
			return false;
		}
		return true;
	}
	return false;
}

bool Vfs_Archive::IsRootDir()
{
#ifdef _WIN32
	if(m_strCurDir.Length() <= 3)
		return true;
#else
	if(m_strCurDir.Length() < 2)
		return true;
#endif
	
	return false;
}

bool Vfs_Archive::SetRootDir()
{
	m_strCurDir = "";	
	return true;
}

bool Vfs_Archive::UpDir()
{
    if(m_strCurDir.IsEmpty() || m_strCurDir.Length() <= 1)
        return false;

	//set path only if the new differs from old
    String strNewPath = PathName::GetParentDirPath(m_strCurDir);
    if(strNewPath != m_strCurDir)
    {
        SetDir(strNewPath);
        return true;
    }

    return false;
}

bool Vfs_Archive::Execute(const char *szItem, bool bLocalDir)
{
	return false;	//not supported
}

String Vfs_Archive::GetPathTitle()    //for display purposes
{
	String strTitle;
	
	//custom title is used when we want to mask archive file location
	//eg. archive inside archve unpacks inner archive into the temp directory
	if(m_strTitle.IsEmpty())
		strTitle += PathName::GetBaseName(m_strArchiveFile);
	else
		strTitle += m_strTitle;

	PathName::EnsureNotTerminated(strTitle);	
	strTitle += m_strCurDir;
	PathName::EnsureTerminated(strTitle, '/');
	return strTitle;
}

