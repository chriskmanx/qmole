////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Copy operation implementation
////////////////////////////////////////////////////////////////////////////

#include <gtk/gtk.h>	//TOFIX move GUI from operation to OpState
#include "../support.h"
#include "OpCopy.h"
#include "PathName.h"
#include "opcodes.h"
#include "../ThreadSafeMsgs.h"
#include "util.h"
#include "debug.h"
#include "System.h"
#include "VfsArchive.h"
#include "VfsLocal.h"
#include "VfsManager.h"
#include "../DualPanel.h"
  
extern VfsManager g_VfsManager;
extern DualPanel g_dp;
 
bool ForcePathExists(Vfs *pVfs, const char *szPath);

OpCopy::OpCopy()
{
	m_nOpSettings        = 0;
	m_bSingleRootItem    = false;
	m_bLocalDestination  = false;
	m_nOpType = OP_COPY;
}

OpCopy::~OpCopy()
{
}

//copy files from one VFS to the another
bool OpCopy::OpExecute()
{
	return Copy();
}

String OpCopy::GetFullPath(Vfs *pVfs)
{
	switch(pVfs->GetType()){
		case Vfs::ARCHIVE:
		{
			String str(((Vfs_Archive*)pVfs)->GetArchivePath().c_str());
			str += pVfs->GetDir();
			return str.c_str();
		}

		default:
			return pVfs->GetDir().c_str();
	}
}

//copy files from one VFS to the another
bool OpCopy::Copy()
{
	TRACE("OpCopy::Copy start\n");
	//TOFIX maybe checks should be done first, but they must use m_strDstPattern

	//STEP 2:  expand selection recursively into subdirectories (total size calculation)
	//(this must be done first or else PrepareInitialPath can create new items inside src items tree)
	m_pVfsSrc->ExpandSelection(m_objSrcItems, m_pStat->m_bAbort);    //TOFIX pass full Stat
	m_pStat->m_nTotBytesMax = m_objSrcItems.GetTotalSize();

	if(!PrepareInitialPath()){
	        //TOFIX MEssageBox()
		return false;
	}

	m_strOrigDest = m_pVfsDst->GetDir();

	//STEP 1: check if copy allowed (copy over itself is forbidden)
	if(m_pVfsSrc->GetType() == m_pVfsDst->GetType())
	{
		//check if copying file to itself (its own directory)
		String strSrc(GetFullPath(m_pVfsSrc).c_str());
		String strDst(GetFullPath(m_pVfsDst).c_str());

		int  nRootCnt = m_objSrcItems.GetRootCount();
		if( (m_bLocalDestination && strSrc == strDst && m_objSrcItems.m_lstRootItems[0].GetName() == m_strName) ||
			(!m_bLocalDestination && nRootCnt == 1 && strSrc == strDst && m_objSrcItems.m_lstRootItems[0].GetName() == m_strName) || 
			(!m_bLocalDestination && nRootCnt >  1 && strSrc == strDst))    //case when user selected custom destination dir
		{
			MsgBox_ThrSafe(_("You cannot copy a file to itself!"));
			return false;
		}

		//check if copying some content into its own subdirectory (forbidden) 
		String strSource;
		String strDest = m_pVfsDst->GetDir();
		int    nSrcLen;

		//if destination dir is child of any dirs in the source
		int nCount = m_objSrcItems.GetRootCount();
		for(int i=0; i<nCount; i++)
		{
			if(m_objSrcItems.m_lstRootItems[i].IsDir())
			{
				strSource = m_pVfsSrc->GetDir();
				PathName::EnsureTerminated(strSource);
				strSource += m_objSrcItems.m_lstRootItems[i].GetName();
				m_pVfsSrc->FixPath(strSource);

				nSrcLen = strSource.Length();    

				if(nSrcLen == StrCommonPrefixLen(strDest, strSource))
				{
					MsgBox_ThrSafe(_("You cannot copy directory into its own subdirectory!"));
					return false;
				}
			}
		}
	}

	m_pVfsDst->CachedListDir(m_lstDstDir, m_pStat->m_bAbort, true);

	//STEP 3: copy (recursively)
	int nRootCount = m_objSrcItems.GetRootCount();
	for(int i=0; i<nRootCount; i++)
	{
		//check for abort
		if(m_pStat->IsAborted())
			break;

		CopyRecursive(m_objSrcItems.m_lstRootItems[i]);
	}
    
	m_pVfsDst->SetDir(m_strOrigDest);
	if(m_bLocalDestination && NULL != m_pVfsDst)
		delete m_pVfsDst;

	return true;
}

void OpCopy::CopyRecursive(VfsSelectionItem &item)
{
	//check quit flags
	if(m_pStat->IsAborted())
		return;

	if(item.IsDir())
	{
		if(!item.IsDots())
		{
			//store current dir positions
			String strSrcDir = m_pVfsSrc->GetDir();
			String strDestDir = m_pVfsDst->GetDir();

			//create new directory
			//set both panels to matching dirs
			String strNewSrcDir = strSrcDir;
			PathName::EnsureTerminated(strNewSrcDir);
			strNewSrcDir += item.GetName();
			m_pVfsSrc->SetDir(strNewSrcDir);    //TOFIX check success

			String strDirName = item.GetName();
			if(m_bSingleRootItem){    //single item and the directory
				strDirName = m_strName;
				m_bSingleRootItem = false;    //no more custom naming (used only when one root item is being copied)
			}
			m_pVfsDst->MkDir(strDirName); //dir might have existed before

			String strPathDst(strDestDir);
			PathName::EnsureTerminated(strPathDst);
			strPathDst += strDirName;

			m_pVfsDst->SetDir(strPathDst);    //TOFIX check success
			m_pVfsDst->CachedListDir(m_lstDstDir, m_pStat->m_bAbort, true);

			//copy all entries to new dir
			int nCount = item.m_lstSubItems.size();
			for(int i=0; i<nCount; i++)
			{
				//check quit flags
				if(m_pStat->IsAborted())
					break;

				CopyRecursive(item.m_lstSubItems[i]);
			}

			//restore previous dirs
			m_pVfsSrc->SetDir(strSrcDir);    //VERIFY
			m_pVfsDst->SetDir(strDestDir);    //VERIFY
			if(!m_pStat->IsAborted()) 
				m_pVfsDst->CachedListDir(m_lstDstDir, m_pStat->m_bAbort, true);

			//TOFIX? copy directory properties to new created (time+attrib)
			//       (this requires VFS::SetAttributes(time, attrib);
		}
	}
	else
	{
		//NOTE: use file name only - path is known
		SingleFileCopy(item);

		//TOFIX
		// if(m_nOpSettings & OPF_CPY_ERR_SKIP_ALL)
		//	break;	//skip all erroneous and try to copy other files ?

	}
}

bool OpCopy::SingleFileCopy(VfsSelectionItem &item)
{
	//check quit flags
	if(m_pStat->IsAborted())
		return false;

	String strSearch;
	if(m_bSingleRootItem){
		strSearch = m_strName;
		m_bSingleRootItem = false;    //no more custom naming
	}
	else
		strSearch = item.GetName();

	bool bCaseSensitive = true;
#ifdef _WIN32
	if(m_pVfsDst->GetType() == Vfs::LOCAL)
		bCaseSensitive = false;
#endif

	//if item of same name exists at destination
	int nItem = m_lstDstDir.FindItem(strSearch, 0, bCaseSensitive);
	if(-1 != nItem)
	{
		//delete previous temporary flags
		m_nOpSettings &= ~(OPF_TMP_FLAGS_MASK);

		//if the global copy settings were not previously set
		if( !(m_nOpSettings & OPF_CPY_OVERWRITE_ALL)       &&
		    !(m_nOpSettings & OPF_CPY_SKIP_ALL)            && 
		    !(m_nOpSettings & OPF_CPY_OVERWRITE_ALL_OLDER))
		{
			//ask to overwrite file giving details on both files
			String strSrcPath = m_pVfsSrc->GetDir();
			PathName::EnsureTerminated(strSrcPath);
			strSrcPath += item.GetName();

			String strDstPath = m_pVfsDst->GetDir();
			PathName::EnsureTerminated(strDstPath);
			strDstPath += strSearch;

			String strTitle;
			strTitle.Printf(_("Overwrite: %s\n%s byte,%s\n\nWith file: %s\n%s byte,%s?"),
				#ifdef _WIN32
					System::FileNameToUTF8(strDstPath.c_str()).c_str(),
				#else
					strDstPath.c_str(),
				#endif
				m_lstDstDir.GetAt(nItem).GetSize().c_str(),
				m_lstDstDir.GetAt(nItem).GetDate().c_str(),
				#ifdef _WIN32
					System::FileNameToUTF8(strSrcPath.c_str()).c_str(),
				#else
					strSrcPath.c_str(),
				#endif
				item.GetSize().c_str(),
				item.GetDate().c_str());

			m_nOpSettings |= MsgOverwrite_ThrSafe(strTitle);
		}

		//check if rename required
		if(m_nOpSettings & OPF_CPY_RENAME)
		{
			while(true)
			{
				int nRes = MsgNameInput_ThrSafe(String(_("New file name")), strSearch);
				if(0 != nRes)
				{
					nItem = m_lstDstDir.FindItem(strSearch);
					if(-1 == nItem)
						break;    //valid non-overwriting name

					String strMsg;
					strMsg.Printf(_("File %s already exists!"), strSearch.c_str());
					MsgBox_ThrSafe(strMsg);
				}
				else{
					m_nOpSettings |= OPF_SKIP;    //skip if canceled
					break;
				}
			}
		}

		if(OPF_ABORT & m_nOpSettings){
			m_pStat->Abort();    //abort requested
			return false;
		}

		if( (OPF_SKIP         & m_nOpSettings) ||
		    (OPF_CPY_SKIP_ALL & m_nOpSettings))
			return false;

		//TOFIX? add support for OPF_CPY_OVERWRITE_ALL_OLDER
		if( (OPF_CPY_OVERWRITE_ALL & m_nOpSettings) || 
		    (OPF_OVERWRITE & m_nOpSettings)) 
		{
			VfsItem item1;
			item1.SetName(strSearch);

			int nRes = OPF_RETRY;
			while(nRes == OPF_RETRY)
			{
				if(!m_pVfsDst->Delete(item1, m_nOpSettings))
				{
					String strMsg;
					strMsg.Printf(_("Failed to overwrite %s!"), item.GetName().c_str());
					
					//ask user for Retry|Cancel|Abort
					nRes = MsgOperationError_ThrSafe(strMsg);
					if(nRes == OPF_ABORT)
						m_pStat->Abort();
					else if(nRes != OPF_RETRY)
						break;
					return false;               
				}
				else
					break;
			}
		}
	}

	//check for drive free space (after we have possibly deleted file with same name)
	INT64 nFreeSpace = m_pVfsDst->GetDriveFreeSpace();
	if(nFreeSpace < item.m_nSize)
	{
		String strMsg;
		strMsg.Printf(_("There is no free space on target drive\nto copy file %s.\nAborting operation!"), item.GetName().c_str());
		MsgBox_ThrSafe(strMsg);
		m_pStat->Abort();
		return false;
	}

	//in case of resume start copy from some file offset
	INT64 nOffset = 0;
	if(OPF_CPY_RESUME & m_nOpSettings)
		nOffset = item.m_nSize;

	//
	// copy operation itself
	//
	bool bRes = true;

	//TOFIX check if the user here inside required operation ABORT (pass m_nOpSettings)
	m_nOpSettings |= OPF_RETRY;  //make sure we will enter the loop
	while(m_nOpSettings & OPF_RETRY)
	{
		m_nOpSettings &= ~OPF_RETRY; //remove flag

		//TRACE("OpCopy: copy %s to dir %s\n", item.GetName().c_str(), m_pVfsDst->GetDir().c_str());

		if(!m_pVfsSrc->Copy(item, strSearch, m_pVfsDst, nOffset))
		{
			if(m_pStat->IsAborted())	//check for abort
		        	break;

			//skip all erroneous and try to copy other files ?
			if(m_nOpSettings & OPF_CPY_ERR_SKIP_ALL)
				break;

			TRACE("Copy error - ask user to decide on operation\n");

			String strMsg;
		#ifdef _WIN32
			strMsg.Printf(_("Failed to copy %s!"), System::FileNameToUTF8(item.GetName()).c_str());
		#else
			strMsg.Printf(_("Failed to copy %s!"), item.GetName().c_str());
		#endif	

			m_nOpSettings |= MsgCopyError_ThrSafe(strMsg);
			if(OPF_ABORT & m_nOpSettings){
				TRACE("Copy error - user aborted\n");
				m_pStat->Abort();    //abort requested
				return false;
			}
			bRes = false;
		}
		else
			bRes = true;
	}

	return bRes;
}

bool OpCopy::IsDirectCopy(unsigned int dwSrcType, unsigned int dwDstType)
{
	// folowing transfer combinations are indirect - copying through temp directory intermediate
	if( (dwSrcType == Vfs::ARCHIVE && dwDstType == Vfs::ARCHIVE)    ||
	    (dwSrcType == Vfs::ARCHIVE && dwDstType == Vfs::FTP)        ||
	    (dwSrcType == Vfs::ARCHIVE && dwDstType == Vfs::SFTP)       ||
	    (dwSrcType == Vfs::FTP     && dwDstType == Vfs::ARCHIVE)    ||
	    (dwSrcType == Vfs::FTP     && dwDstType == Vfs::SFTP)       ||
	    (dwSrcType == Vfs::SFTP    && dwDstType == Vfs::ARCHIVE)    ||
	    (dwSrcType == Vfs::SFTP    && dwDstType == Vfs::FTP)        ||
	    (dwSrcType == Vfs::SFTP    && dwDstType == Vfs::SFTP))
		return false;

	//all other transfers are direct
	return true;
}

//ensure user customized path exists
bool OpCopy::PrepareInitialPath()
{
	//TOFIX error handling (check names?)
	if(m_strDstPattern.IsEmpty())
		return true;    //no destination customizing

	String strPath, strName;
	int nRootCnt = m_objSrcItems.GetRootCount();
	if(nRootCnt == 1)
	{
		//single root item copying

		//if the single item pattern is terminated with delimiter, 
		//assume everything is a directory part (name not changed) 
		if(PathName::IsTerminated(m_strDstPattern))
		{
			strPath   = m_strDstPattern;
			m_strName = m_objSrcItems.m_lstRootItems[0].GetName();
		}
		else
		{
			strPath   = PathName::GetParentDirPath(m_strDstPattern);
			m_strName = PathName::GetBaseName(m_strDstPattern);
		}

		m_bSingleRootItem = true;
	}
	else{
		//multiple root items copying
		strPath = m_strDstPattern;
	}

	//when we change destination to be relative, we imply the src directory as destination
	if(!PathName::IsAbsolutePath(m_strDstPattern))
	{
		m_bLocalDestination = true;
		m_pVfsDst = new Vfs_Local;

		// TOFIX get this information in a cleaner way
		String strDir = m_pVfsSrc->GetDir();
		strPath = PathName::ComposePath(strDir, strPath);
		strPath = PathName::GetCanonicalPath(strPath.c_str());

		m_pVfsDst->SetDir(PathName::GetRootDir(strPath));	//set to the right drive path
	}
	TRACE("path %s, name %s\n", strPath.c_str(), m_strName.c_str());
		
	m_strDstPattern = ""; //clear after use

	return ForcePathExists(m_pVfsDst, strPath);
}

//TOFIX move somewhere - Vfs?
bool ForcePathExists(Vfs *pVfs, const char *szPath)
{
	if(NULL == pVfs)
		return false;

	VfsListing list;
	bool bAbort;
	String strPath(szPath), strDir;
	PathName::EnsureTerminated(strPath, '/');//TOFIX it is very important to use '//' for SFTP sites!!!
//TOFIX    pVfs->FixPath(strPath);
	String strCurDir = pVfs->GetDir();
	PathName::EnsureTerminated(strCurDir, '/');

	//skip all the trouble if the requested path is already set
	if(0 != strCurDir.Cmp(strPath))	// TOFIX create fn. PathName::CmpPaths which skips terminators diffs
	{
		//TOFIX start with taking current dir and seeing if we can advance from there
		int nPos, nStart = 0;
		while(-1 < (nPos = strPath.Right(strPath.Length()-nStart).find_first_of("/\\")))
		{
			nPos += nStart;    //since we counted inside partial string

			String strNewDir = strPath.Left(nPos+1);
			PathName::EnsureNotTerminated(strNewDir);
			if(strNewDir.IsEmpty())
				strNewDir = "/";

			bool bSkipSetDir = false;

			//this is a way to minimize number of setdir calls to directory that
			//may not exist - this is important for FTP code and similar
			pVfs->CachedListDir(list, bAbort, true);
			if(pVfs->GetDir() == PathName::GetParentDirPath(strNewDir))
				if(-1 == list.FindItem(PathName::GetBaseName(strNewDir)))
					bSkipSetDir = true;    //dir does not exist

			if(bSkipSetDir || !pVfs->SetDir(strNewDir))
			{
				strDir = strPath.Mid(nStart, nPos-nStart);
				if(pVfs->MkDir(strDir))
				{
					if(!pVfs->SetDir(strPath.Left(nPos+1)))
						return false;
				}
				else
					return false;
			}

			nStart = nPos+1;
		}
	}
	return true;
}
