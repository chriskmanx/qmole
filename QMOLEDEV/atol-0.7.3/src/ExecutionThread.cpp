////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: 
//////////////////////////////////////////////////////////////////////////// 

#include "core/VfsManager.h"
#include "core/VfsLocal.h"
#include "core/VfsArchive.h"
#include "core/PathName.h"
#include "core/System.h"
#include "ExecutionThread.h"
#include "support.h"
#include "FileList.h"
#include "ThreadSafeMsgs.h"

extern VfsManager g_VfsManager;

ExecutionThread::ExecutionThread(FileList *pFileList, const char *pTempFile, const char *pProgram /* = NULL */)
{
	m_pFileList = pFileList;
	m_strTempFile = pTempFile;
	m_strProgram = pProgram;
}

void ExecutionThread::MainMethod()
{
	if(m_pFileList->m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
		return;

	if(g_VfsManager.VfsStackSize(&m_pFileList->m_ctrl) > 2)
	{
		MsgBox_ThrSafe(_("Modifying/Executing of files in nested archives is not supported."));
		return;
	}

	VfsItem itemBeforeExec, itemAfterExec;
	bool bDeleteTempFile = true;
	Vfs *pVfsOrig = m_pFileList->m_ctrl.m_pVfs;
	bool bArch = pVfsOrig->GetType() == Vfs::ARCHIVE;
	String strArchivePath("");
	if(bArch)
	{
		strArchivePath = ((Vfs_Archive *)pVfsOrig)->GetArchivePath();
	}
	String strDir = pVfsOrig->GetDir();
	Vfs_Local::MakeItem(m_strTempFile, itemBeforeExec);

	// launch external process and wait for it to finish
	if(m_strProgram.Length() > 0)
	{
		String strTempFileName = PathName::GetBaseName(m_strTempFile.c_str());
		String strTempFilePath = PathName::GetParentDirPath(m_strTempFile.c_str());
	
	#ifdef _WIN32
		// quote string if it contains the spaces
		if(strTempFileName.find(' ') != std::string::npos){
			strTempFileName.insert(0, "\"");
			strTempFileName += "\"";
		}
	#endif

		System::Execute(m_strProgram.c_str(), strTempFileName.c_str(), strTempFilePath.c_str(), true);
	}
	else
	{
		System::Open(m_strTempFile.c_str(), true);
	}

	// did the external process change the temp file?
	Vfs_Local::MakeItem(m_strTempFile, itemAfterExec);
	if(itemAfterExec.m_nLastModDate > itemBeforeExec.m_nLastModDate)
	{
		// yes. Ask if it shall be copied back
		int iResponse = MsgBox_ThrSafe(
			_("The file has been changed.\n\nDo you want to copy it back into the virtual file system?"),
			GTK_BUTTONS_YES_NO);

		if(GTK_RESPONSE_YES == iResponse) {
			Vfs *pVfsDst;
			Vfs_Archive *pVfsCleanup = NULL;

			if(bArch) {
				ASSERT(strArchivePath.length() > 0);
				// is the target archive already opened?
				pVfsDst = g_VfsManager.FindVfsArchive(strArchivePath);
				if(!pVfsDst)
				{
					// no, create a new Vfs
					pVfsDst = pVfsCleanup = CreateVfsArchive(strArchivePath);
					if(!pVfsDst)
					{
						int iResponse = MsgBox_ThrSafe(
							_("Error acessing the archive.\n\nDo you want to keep the temporary file?"),
							GTK_BUTTONS_YES_NO);
						bDeleteTempFile = iResponse == GTK_RESPONSE_NO;
					}
				}
			}
			else
			{
				pVfsDst = m_pFileList->m_ctrl.m_pVfs;
				if(pVfsDst != pVfsOrig) { //We are on wrong filesystem
					pVfsDst = NULL; //TODO: try to find vfs from currenlty opened remote filesystems
				}
			}
			if(pVfsDst)
			{
				bDeleteTempFile = CopyBack(pVfsDst, strDir, itemAfterExec);
			}
			else
			{
				int iResponse = MsgBox_ThrSafe(
					_("Replacing of the existing file failed.\n\nDo you want to keep the temporary file?"),
					GTK_BUTTONS_YES_NO);
				bDeleteTempFile = (iResponse == GTK_RESPONSE_NO);
			}
			if(pVfsCleanup)
			{
				pVfsCleanup->Close();
				delete pVfsDst;
			}

		}
	}

	if(bDeleteTempFile)
		System::Remove(m_strTempFile);
		
	delete this;	
}

bool ExecutionThread::CopyBack(Vfs *pVfsDst, const String& strDir, VfsItem& item)
{
	// save current directory of target vfs, for restoring later
	//pVfsDst->Lock();
	String strOldDir = pVfsDst->GetDir();
	bool bRet = true;

	if(pVfsDst->SetDir(strDir))
	{
		int i = 0;

		// delete existing file in archive
		if(pVfsDst->Delete(item, i))
		{
			// copy temp file into archive
			if(!pVfsDst->CopyFromLocal(m_strTempFile.c_str(), item))
			{
				int iResponse = MsgBox_ThrSafe(
					_("Inserting the modified file back into virtual filesystem failed.\n\nDo you want to keep the temporary file?"),
					GTK_BUTTONS_YES_NO);
				bRet = (iResponse == GTK_RESPONSE_NO);
			}
		}
		else
		{
			int iResponse = MsgBox_ThrSafe(
				_("Replacing of the existing file in the archive failed.\n\nDo you want to keep the temporary file?"),
				GTK_BUTTONS_YES_NO);
			bRet = (iResponse == GTK_RESPONSE_NO);
		}
	}
	else
	{
		int iResponse = MsgBox_ThrSafe(
			_("Setting target directory failed.\n\nDo you want to keep the temporary file?"),
			GTK_BUTTONS_YES_NO);
		bRet = (iResponse == GTK_RESPONSE_NO);
	}

	// restore working directory of target vfs
	pVfsDst->SetDir(strOldDir);

	//pVfsDst->Unlock();

	// refresh panels if necessary
	if(strDir == strOldDir)
	{
		if(pVfsDst == m_pFileList->m_ctrl.m_pVfs){
			Refresh_ThrSafe((long)m_pFileList);
		}
		if(pVfsDst == m_pFileList->m_pOtherList->m_ctrl.m_pVfs){
			Refresh_ThrSafe((long)m_pFileList->m_pOtherList);
		}
	}
	return bRet;
}

Vfs_Archive *ExecutionThread::CreateVfsArchive(const String& strArchivePath)
{
	Vfs_Archive *pVfs = new Vfs_Archive();

	if(pVfs)
	{
		if(pVfs->InitArchiver(strArchivePath))
		{
			if(pVfs->Open())
				return pVfs;
		}
		delete pVfs;
	}
	return NULL;
}
