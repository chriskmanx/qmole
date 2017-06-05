////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Contains file listing with its browse history list
//////////////////////////////////////////////////////////////////////////// 

#include "FileListController.h"
#include "VfsLocal.h"
#include "PathName.h"
#include "VfsManager.h"

extern VfsManager g_VfsManager;

FileListController::FileListController()
{
	m_nSortColumn = -1;
	m_bSortAscending = false;
}

FileListController::~FileListController()
{
}

bool FileListController::GoRootDir()
{
	//calculate new path
	String strPath1 = m_pVfs->GetDir();
	String strPath2 = PathName::GetRootDir(strPath1);

	// do the listing
	if(strPath1 != strPath2){
		m_pVfs->SetDir(strPath2);
		return true;
	}
	else
	{
		//VFS is already at the root, check if there is a parent Vfs underneath
		if(g_VfsManager.VfsStackSize(this) > 1)
		{
			Vfs *pVfs = m_pVfs; 
			if(!g_VfsManager.VfsStackPop(this))
				return false;

			//remote Vfs is kept in remote pool, other Vfs' are destroyed when not needed
			if( pVfs->GetType() != Vfs::FTP &&
				pVfs->GetType() != Vfs::SFTP)
			{
					pVfs->Close();
					delete pVfs;
			}

			//do the same stuff one level up
			strPath1 = m_pVfs->GetDir();
			strPath2 = PathName::GetRootDir(strPath1);

			// do the listing
			if(strPath1 != strPath2){
				m_pVfs->SetDir(strPath2);
				return true;
			}
		}
	}

	return false;
}
