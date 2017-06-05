////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsManager implementation
////////////////////////////////////////////////////////////////////////////

#ifdef _WIN32
 #include <winsock2.h>
 #include <io.h>
 #define _access access
#else
 #include<unistd.h>
#endif

#include "../callbacks.h"
#include "PathName.h"
#include "VfsManager.h"
#include "VfsLocal.h"
#include "VfsArchive.h"
#include "debug.h"
#include "VfsFtp.h"
#include "VfsSftp.h"
#include "System.h"

void RemoteTrace(const char *szMsg, int nType, unsigned long nData);

VfsManager::VfsManager()
{
}

VfsManager::~VfsManager()
{
}

void VfsManager::ClearVfsList()
{
	unsigned int i;

	//clear ordinary Vfs list
	for(i=0; i<m_lstVfs.size(); i++){
		int nType = m_lstVfs[i]->GetType();
		if(nType != Vfs::FTP &&
		   nType != Vfs::SFTP)
		{
			m_lstVfs[i]->Close();
			delete m_lstVfs[i];
		}
	}
	m_lstVfs.clear();

	//clear remote Vfs list
	for(i=0; i<m_lstVfsRemote.size(); i++){
		m_lstVfsRemote[i]->Close();
		delete m_lstVfsRemote[i];
	}
	m_lstVfsRemote.clear();
}

//TOFIX temporary, more abstract solution needed (VFS factory, ...)
void VfsManager::InitList(FileListController *pList, Vfs *pVfs)
{
    //create new Vfs stack for this list control object (if not already done)
    std::map<void *, int>::iterator It = m_mapStackIdx.find(pList);
    if(It == m_mapStackIdx.end())
    {
        //stack does not exist for this list, create new one
        tVfsStack stack;
        m_lstStacks.push_back(stack);

        //remember stack index
        m_mapStackIdx[pList] = m_lstStacks.size()-1;
    }

    if(NULL == pVfs)
        pVfs = new Vfs_Local;

    VfsStackPush(pList, pVfs);

    //TOFIX	pList->SetDirectory(PathName::GetDefaultStartDir());
}

//returns: 0 (do nothing), 1 (refresh new Vfs), -1 (no Vfs found, execute file)
int VfsManager::CheckForSubVfs(FileListController *pList, Vfs *pVfs, VfsItem &origItem, bool canReadFile)
{
	VfsItem target;
	String strFullPath;
	if (origItem.IsLink())
	{
		// dereference link
		target = pVfs->GetLinkTarget(origItem);
		if(target.GetName().Length() <= 0)
			return 0;

		strFullPath = target.GetPath();
	}
	else
	{
		strFullPath = pVfs->GetDir();
		PathName::EnsureTerminated(strFullPath);
		strFullPath += origItem.GetName();
		pVfs->FixPath(strFullPath);
	}

	VfsItem& item = origItem.IsLink() ? target : origItem;

	//TOFIX? simplify 
	//TOFIX? support for archives on remote Vfs (download/unpack)
	if(pVfs->GetType() == Vfs::LOCAL || pVfs->GetType() == Vfs::NET)
	{
		//support for ".enc" format
		String strExt = item.GetExt();
		if(0 == strExt.CmpNoCase(".enc"))
		{
			on_file_encrypt_decrypt_activate(NULL, 0);
			return 0;
		}

		//suport for compressed archives
		//TOFIX check magic numbers
		Vfs_Archive *pArchiveVFS = new Vfs_Archive;
		if(NULL != pArchiveVFS)
		{
			if(pArchiveVFS->InitArchiver(strFullPath, canReadFile))
			{
				//printf("Archive: %s\n", strFullPath.c_str());
				pArchiveVFS->m_strArchiveFile = strFullPath;
				if(!pArchiveVFS->Open())
				{
					//printf("Failed to open archive: %s\n", strFullPath.c_str());
					delete pArchiveVFS;
					//TOFIX wxMessageBox(_("Error in packed file!"),_("Error"));
					return 0;	//do nothing (error)
				}

				VfsStackPush(pList, pArchiveVFS);    //switch to new Vfs
				return 1;	//Vfs found (refresh)
			}
			else
			{
				//not an archive
				delete pArchiveVFS;
			}
		}
	}
	else if(pVfs->GetType() == Vfs::ARCHIVE)
	{
		//check for archive in archive -> extract selected archive item into temp directory
		Vfs_Archive *pArchiveVFS = new Vfs_Archive;
		if(NULL != pArchiveVFS)
		{
			if(pArchiveVFS->InitArchiver(strFullPath))
			{
				//TOFIX separate fn -> CreateTempPath
				//create temporary path
				int i=0;
				String strDestPath = PathName::Path_TempDirectory();
				PathName::EnsureTerminated(strDestPath, '/');
				strDestPath += item.GetName();
				//TOFIX while(PathName::FileExists(strDestPath))
				while(0 == access(strDestPath, 00))
				{
					strDestPath.Printf("%s%d%s", PathName::Path_TempDirectory().c_str(), i, item.GetName().c_str());
					i ++;
					if(i > 40){
						delete pArchiveVFS;
						return 0;	//do nothing (error)
					}
				}

				//TOFIX progress dialog
				pVfs->m_pProgress = NULL;

				TRACE("Archive in archive browsing: unpack to local %s\n", strDestPath.c_str());

				//extract file to temp under unique name
				if(!pVfs->CopyToLocal(origItem, strDestPath))
				{
					delete pArchiveVFS;
					return 0;	//do nothing (error)
				}

				//when having archive in archive, we don't want to show
				//true path of internal archive (unpacked in temp dir)
				//so we set custom title
				pArchiveVFS->m_strArchiveFile = strDestPath;
				pArchiveVFS->m_strTitle       = item.GetName();
				pArchiveVFS->m_bDeleteFile    = true;    //delete temporary archive file when Vfs closes

				if(!pArchiveVFS->Open())
				{
					delete pArchiveVFS;
					//TOFIX wxMessageBox(_("Error in packed file!"),_("Error"));
					return 0;	//do nothing (error)
				}

				VfsStackPush(pList, pArchiveVFS);    //switch browser
				return 1;	//refresh list (Vfs found)
			}
			else
			{
				//archiver not initialized - not an archive within archive
				delete pArchiveVFS;
			}
		}
	}

	return -1;	//execute file (no Vfs found)
}

bool VfsManager::CheckForUpVfs(FileListController *pList, Vfs *pVfs)
{
	if( (pVfs->GetType() == Vfs::ARCHIVE) || 
	    (pVfs->GetType() == Vfs::SITEMAN))
	{
		//switch to previous VFS
		if(VfsStackPop(pList))
		{
			if( pVfs->GetType() != Vfs::FTP &&
			    pVfs->GetType() != Vfs::SFTP )
			{
				pVfs->Close();
				delete pVfs;
			}
			return true;
		}
	}

	return false;
}

void VfsManager::VfsStackPush(FileListController *pList, Vfs *pVfs)
{
	//stop notifications inside the old Vfs
	if(pList->m_pVfs && pList->m_pVfs->GetType() == Vfs::LOCAL)
		((Vfs_Local *)pList->m_pVfs)->StopNotifications();

    //link Vfs to the list
    pList->m_pVfs = pVfs;

    //store Vfs pointer into global and per-list stack
    m_lstVfs.push_back(pVfs);

    int nPos = m_mapStackIdx[pList];
    m_lstStacks[nPos].push_back(pVfs);
}

int VfsManager::VfsStackSize(FileListController *pList)
{
    int nPos = m_mapStackIdx[pList];
    return m_lstStacks[nPos].size();
}

//remove top Vfs from stack, but do not delete it (API user will have to do it)
bool VfsManager::VfsStackPop(FileListController *pList)
{
    int nStIdx  = m_mapStackIdx[pList];
    int nStSize = m_lstStacks[nStIdx].size();
    if(nStSize > 1)    //at least one Vfs must stay in the list
    {
        Vfs *pVfs = m_lstStacks[nStIdx][nStSize-1]; //take top/last Vfs
        m_lstStacks[nStIdx].pop_back();                //shrink list stack

        //find and remove from global Vfs list
        unsigned int i;
        for(i=0; i<m_lstVfs.size(); i++)
            if(m_lstVfs[i] == pVfs)
                m_lstVfs.erase(m_lstVfs.begin()+i);

        //link new top Vfs for list control
        pList->m_pVfs = m_lstStacks[nStIdx][nStSize-2]; //-2 -> account for deleted Vfs

        return true;
    }

    return false;
}

void VfsManager::VfsStackClean(FileListController *pList)
{
	// clean Vfs stack, leave only default (oldest) Vfs
	Vfs *pVfs = pList->m_pVfs; 
	while(VfsStackPop(pList))
	{
		if( pVfs->GetType() != Vfs::FTP &&
			pVfs->GetType() != Vfs::SFTP)
		{
			pVfs->Close();
			delete pVfs;
		}
		pVfs = pList->m_pVfs; 
	}
}

String VfsManager::GetPathTitle(FileListController *pList)
{
	String strTitle;

	if(NULL == pList || NULL == pList->m_pVfs)
		return strTitle;

	//NET Vfs does not use stacked title path
	if( pList->m_pVfs->GetType() == Vfs::NET ||
		pList->m_pVfs->GetType() == Vfs::FTP ||
		pList->m_pVfs->GetType() == Vfs::SFTP)
	{
		//get path title of last stacked VFS
		strTitle = pList->m_pVfs->GetPathTitle();
	}
	else
	{
		TRACE("VfsManager::GetPathTitle\n");

		int nStIdx  = m_mapStackIdx[pList];
		if(nStIdx >=0)
		{
			int nStSize = m_lstStacks[nStIdx].size();
			if(nStSize > 0)
			{
				TRACE("VfsManager::GetPathTitle nStIdx=%d, nStSize=%d\n", nStIdx, nStSize);

				//sum paths of all stacked VFSs
				for(int i=0; i<nStSize; i++){
					String strSegment = m_lstStacks[nStIdx][i]->GetPathTitle();
					PathName::EnsureTerminated(strSegment);
					m_lstStacks[nStIdx][i]->FixPath(strSegment);

					strTitle += strSegment;
				}
			}
		}
	}

	std::string strTmp = strTitle;
#ifdef _WIN32	
	strTmp = System::FileNameToUTF8(strTmp);
#endif
	strTitle = strTmp.c_str();

	return strTitle;
}

void VfsManager::VfsStackKillTop(FileListController *pList)
{
    Vfs *pVfs = pList->m_pVfs;
    VfsStackPop(pList);

    //remove from main list
    for(unsigned int i=0; i<m_lstVfs.size(); i++){
        if(m_lstVfs[i] == pVfs){
            m_lstVfs.erase(m_lstVfs.begin()+i);
            break;
        }
    }

	//close and delete, but not remote Vfs  
	if( pVfs->GetType() != Vfs::FTP &&
		pVfs->GetType() != Vfs::SFTP )
	{
		pVfs->Close();
		delete pVfs;
	}
}

void VfsManager::SwapVfsStacks(FileListController *pList1, FileListController *pList2)
{
    if( NULL == pList1 ||
        NULL == pList2 ||
        pList1 == pList2)
        return;

    //swap two stacks (just swap the indexes in the map)
    int nIdxTmp = m_mapStackIdx[pList1];
    m_mapStackIdx[pList1] = m_mapStackIdx[pList2];
    m_mapStackIdx[pList2] = nIdxTmp;

    //exchange Vfs pointers for two lists
    Vfs *pVfsTmp = pList1->m_pVfs;
    pList1->m_pVfs = pList2->m_pVfs;
    pList2->m_pVfs = pVfsTmp;
}

Vfs *VfsManager::RemoteVfsAlloc(CNodeInfo &info)
{
	//create appropriate connection
	Vfs *pNewVFS = NULL;

	bool bFtp = true;
	if(info.m_dwProtocol == PROT_FTP){
		pNewVFS = new Vfs_Ftp;
	} else {
		pNewVFS = new Vfs_Sftp;
		bFtp	= false;
	}

	if(NULL != pNewVFS)
	{
		unsigned long dwTraceData = (unsigned long)pNewVFS;

		//TOFIX common VFS_Remote base class with SetConnectionInfo() fn.
		if(bFtp){
			//TOFIX better code
			((Vfs_Ftp *)pNewVFS)->m_ftpInfo	= info;
			((Vfs_Ftp *)pNewVFS)->m_ftpCore.m_nEncryptType = info.m_nEncryptType;
			((Vfs_Ftp *)pNewVFS)->m_ftpCore.m_nEncryptData = info.m_nEncryptData;
			((Vfs_Ftp *)pNewVFS)->m_ftpCore.m_nListLongFormat = info.m_nListLongFormat;
			((Vfs_Ftp *)pNewVFS)->m_ftpCore.m_nListShowHidden = info.m_nListShowHidden;
			((Vfs_Ftp *)pNewVFS)->m_ftpCore.m_nListResolveLinks = info.m_nListResolveLinks;
			((Vfs_Ftp *)pNewVFS)->m_ftpCore.m_nListCompleteTime = info.m_nListCompleteTime;

			((Vfs_Ftp *)pNewVFS)->SetTraceCallback(RemoteTrace, dwTraceData);
		}
		else
		{
			((Vfs_Sftp *)pNewVFS)->m_szHost		 = info.m_strHost;
			((Vfs_Sftp *)pNewVFS)->m_nPort		 = info.m_uPort;	
			((Vfs_Sftp *)pNewVFS)->m_szUser		 = info.m_strUser;
			((Vfs_Sftp *)pNewVFS)->m_szPwd		 = info.m_strPassword;
			((Vfs_Sftp *)pNewVFS)->m_strRemoteDir = info.m_strRemoteDir;

			((Vfs_Sftp *)pNewVFS)->SetTraceCallback(RemoteTrace, dwTraceData);
		}
	}

	return pNewVFS;
}

void VfsManager::RemoteVfsAdd(Vfs *pVfs)
{
	m_lstVfsRemote.push_back(pVfs);
}

unsigned int VfsManager::RemoteVfsCount()
{
	return m_lstVfsRemote.size();
}

void VfsManager::RemoteVfsRemove(Vfs *pVfs)
{
	for(unsigned int i=0; i<m_lstVfsRemote.size(); i++)
	{
		if(m_lstVfsRemote[i] == pVfs)
			m_lstVfsRemote.erase(m_lstVfsRemote.begin()+i);
	}
}

void VfsManager::VfsRemotePush(FileListController *pList, Vfs *pVfs)
{
	VfsStackPush(pList, pVfs);
}

void VfsManager::DriveHistoryAdd(const String& strPath)
{
#ifdef _WIN32
	m_mapDriveHistory[PathName::GetRootDir(strPath)] = strPath;
#endif
}

String VfsManager::DriveHistoryGet(const String& strPath)
{
#ifdef _WIN32
	std::map<String, String>::iterator It = m_mapDriveHistory.find(PathName::GetRootDir(strPath));
	if(It != m_mapDriveHistory.end())
		return It->second;
	else
		return PathName::GetRootDir(strPath);
#else
	return strPath;
#endif
} 

// searches all opened Vfs for the specified VfsArchive
// strArchivePath: Name of the archive file, e.g. C:\Tmp\Test.zip
// returns: pointer to VfsArchive or NULL if not found
Vfs_Archive *VfsManager::FindVfsArchive(const String& strArchivePath)
{
	for(int i1 = m_lstStacks.size() - 1; i1 >= 0; i1--)
	{
		for(int i2 = m_lstStacks[i1].size() - 1; i2 >= 0; i2--)
		{
			if(m_lstStacks[i1][i2]->GetType() == Vfs::ARCHIVE)
			{
				Vfs_Archive *pVfs = (Vfs_Archive *)m_lstStacks[i1][i2];
				if(pVfs->GetArchivePath() == strArchivePath)
					return pVfs;
			}
		}
	}
	return NULL;
}
