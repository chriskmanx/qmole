////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Move operation implementation
////////////////////////////////////////////////////////////////////////////

#include "../ThreadSafeMsgs.h"
#include "../support.h"
#include "OpMove.h"
#include "opcodes.h"
#include "PathName.h"
#include "System.h"
#include "util.h"
#include "debug.h"

OpMove::OpMove()
{
	m_bMoveDirect = false;
	m_nOpType = OP_MOVE;
}

OpMove::~OpMove()
{
}

bool OpMove::OpExecute()
{
 	TRACE("OpMove::OpExecute\n");

	if(!PrepareInitialPath()){
		TRACE("OpMove::OpExecute failed to preprare initial path\n");
		return false;
	}

	//STEP 1: check if move allowed (copy over itself is forbidden)
	if(m_pVfsSrc->GetType() == m_pVfsDst->GetType())
	{
		//check if moving file to itself (its own directory)
		String strSrc(GetFullPath(m_pVfsSrc).c_str());
		String strDst(GetFullPath(m_pVfsDst).c_str());
		int  nRootCnt = m_objSrcItems.GetRootCount();
		if( (m_bLocalDestination && strSrc == strDst && m_objSrcItems.m_lstRootItems[0].GetName() == m_strName) ||
		    (!m_bLocalDestination && nRootCnt == 1 && strSrc == strDst && m_objSrcItems.m_lstRootItems[0].GetName() == m_strName) || 
		    (!m_bLocalDestination && nRootCnt >  1 && strSrc == strDst))    //case when user selected custom destination dir
		{
			MsgBox_ThrSafe(_("You cannot move a file to itself!"));
			return false;
		}

		//check if copying some content into its own subdirectory (forbidden) 
		String strSource;
		String strDest = m_pVfsDst->GetDir();
		int    nSrcLen;

		//if destination dir is child of any dirs in the source
		int nCount = m_objSrcItems.m_lstRootItems.size();
		for(int i=0; i<nCount; i++)
		{
			if(m_objSrcItems.m_lstRootItems[i].IsDir())
			{
				strSource = m_pVfsSrc->GetDir();
				PathName::EnsureTerminated(strSource);
				strSource += m_objSrcItems.m_lstRootItems[i].GetName();

				nSrcLen   = strSource.Length();    

				if(nSrcLen == StrCommonPrefixLen(strDest, strSource))
				{
					MsgBox_ThrSafe(_("You cannot move directory into its own subdirectory!"));
					return false;
				}
			}
		}
	}

	//STEP 2: check move type (direct or Copy()+Delete())
	m_bMoveDirect = false;

	if(m_pVfsSrc->GetType() == m_pVfsDst->GetType() && m_pVfsDst->GetType() != Vfs::ARCHIVE)
	{
		//check if two paths are within same disk partition
		if(System::IsSamePartition(m_pVfsSrc->GetDir(), m_pVfsDst->GetDir()))
		{
			TRACE("OpMove: direct move mode!\n");
			m_bMoveDirect = true;
		}
	}

	//STEP 3:  expand selection recursively into subdirectories (total size calculation)
	if(!m_bMoveDirect)	//not needed for direct rename
	{
		m_pVfsSrc->ExpandSelection(m_objSrcItems, m_pStat->m_bAbort);    //TOFIX pass full Stat
		m_pStat->m_nTotBytesMax = m_objSrcItems.GetTotalSize();
	}
	else
		m_pStat->m_nTotBytesMax = m_objSrcItems.m_lstRootItems.size();

	m_pVfsDst->CachedListDir(m_lstDstDir, m_pStat->m_bAbort, true);

	//STEP 3:  copy (recursively)
	int nRootCount = m_objSrcItems.m_lstRootItems.size();
	for(int i=0; i<nRootCount; i++)
	{
		//check for abort
		if(m_pStat->IsAborted())
			break;

		if(m_bMoveDirect)
			SingleFileMove(m_objSrcItems.m_lstRootItems[i]);
		else
			MoveRecursive(m_objSrcItems.m_lstRootItems[i]);
	}
    
	return true;
}

void OpMove::MoveRecursive(VfsSelectionItem &item)
{
	//check for abort, ...
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
			String strSrcNewDir = strSrcDir;
			PathName::EnsureTerminated(strSrcNewDir);
			strSrcNewDir += item.GetName();
			m_pVfsSrc->FixPath(strSrcNewDir);

			m_pVfsSrc->SetDir(strSrcNewDir);

			String strDirName = item.GetName();
			if(m_bSingleRootItem){    //single item and the directory
				strDirName = m_strName;
				m_bSingleRootItem = false;    //no more custom naming (used only when one root item is being copied)
			}
			m_pVfsDst->MkDir(strDirName);

			String strPathDst(strDestDir);
			PathName::EnsureTerminated(strPathDst);
			strPathDst += strDirName;
			m_pVfsDst->FixPath(strPathDst);

			TRACE("OpMove: set dest dir: %s\n", strPathDst.c_str());
			m_pVfsDst->SetDir(strPathDst);
			m_pVfsDst->CachedListDir(m_lstDstDir, m_pStat->m_bAbort, true);

			//move all entries to new dir
			int nCount = item.m_lstSubItems.size();
			for(int i=0; i<nCount; i++)
			{
				//check quit flags
				if(m_pStat->IsAborted())
					break;

				MoveRecursive(item.m_lstSubItems[i]);
			}

			//restore previous dirs
			m_pVfsSrc->SetDir(strSrcDir);
			m_pVfsDst->SetDir(strDestDir);

			//IMPORTANT: check for abort before deleting original directory !!!! 
			if(m_pStat->IsAborted())
				return;

			m_pVfsDst->CachedListDir(m_lstDstDir, m_pStat->m_bAbort, true);

			//delete directory
			m_pVfsSrc->Delete(item, m_nOpSettings);
		}
	}
	else
	{
		SingleFileMove(item);
        
		if(m_bSingleRootItem)
			m_bSingleRootItem = false;    //no more custom naming
	}
}

bool OpMove::SingleFileMove(VfsSelectionItem &item)
{
	//check quit flags
	if(m_pStat->IsAborted())
		return false;

	bool bError = false;

	//if the filename already exists at destination
	String strSearch;
	if(m_bSingleRootItem)
		strSearch = m_strName;
	else
		strSearch = item.GetName();

	String strDest(m_pVfsDst->GetDir());
	PathName::EnsureTerminated(strDest);
	strDest += strSearch;

	if(m_bMoveDirect)
	{
		int nItem = m_lstDstDir.FindItem(strSearch);
		if(-1 != nItem)
		{
			//delete previous temporary flags
			m_nOpSettings &= ~(OPF_TMP_FLAGS_MASK);

			//TOFIX disable Resume button
			//if the copy settings were not previously set
			if( !(m_nOpSettings & OPF_CPY_OVERWRITE_ALL)    &&
			    !(m_nOpSettings & OPF_CPY_SKIP_ALL)         && 
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
					strDstPath.c_str(),
					m_lstDstDir.GetAt(nItem).GetSize().c_str(),
					m_lstDstDir.GetAt(nItem).GetDate().c_str(),
					strSrcPath.c_str(),
					item.GetSize().c_str(),
					item.GetDate().c_str());

				m_nOpSettings |= MsgOverwrite_ThrSafe(strTitle);
			}

			//check if rename required
			if(OPF_CPY_RENAME & m_nOpSettings)
			{
				while(true)
				{
					int nRes = MsgNameInput_ThrSafe(_("New file name"), strSearch);
					if(0 != nRes)
					{
						nItem = m_lstDstDir.FindItem(strSearch);
						if(-1 == nItem)
							break;    //valid non-overwriting name

						MsgBox_ThrSafe(_("File with that name already exists!"));
					}
					else{
						m_nOpSettings |= OPF_SKIP;    //skip if canceled
						break;
					}
				}
			}

			if(OPF_ABORT & m_nOpSettings){
				m_pStat->Abort();
				return false;
			}

			if( (OPF_SKIP         & m_nOpSettings)    ||
			    (OPF_CPY_SKIP_ALL & m_nOpSettings))
			{
				return false;
			}
		}

		//check if overwrite requested
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

		//TOFIX check result, check dest owerwrite, , ..
		String strSrc(m_pVfsSrc->GetDir());
		PathName::EnsureTerminated(strSrc);
		strSrc += item.GetName();
		m_pVfsSrc->FixPath(strSrc);

		strDest = m_pVfsDst->GetDir();
		PathName::EnsureTerminated(strDest);
		strDest += strSearch;
		m_pVfsSrc->FixPath(strDest);

		if(NULL != m_pStat){
			m_pStat->InitCurrentFiles(strSrc, strDest);
			m_pStat->InitCurrentProgress(0, 1);
		}

		bError = !m_pVfsSrc->Rename(strSrc, strDest);
		TRACE("OpMove: rename %s to %s (err = %d)\n", strSrc.c_str(), strDest.c_str(), bError);

		if(bError)
		{
			TRACE("OpMove: file might be locked, fall back to non-direct move (copy+delete)\n");

			bool bOk = SingleFileCopy(item);
        
			//on abort, do not erase original file
			if(m_pStat->IsAborted()){
				TRACE("OpMove: op aborted\n");
				return false;
			}

			if(bOk)
			{
				TRACE("OpMove: delete phase: %s\n", item.GetName().c_str());
				bOk = m_pVfsSrc->Delete(item, m_nOpSettings);
			}
			else{
				TRACE("OpMove: failed to copy file %s\n", item.GetName().c_str());
				return false;	// error msg already shown if copy failed
			}

			bError = !bOk;

			//FIX: don't trigger "Failed ..." message if that was user choice
			if( (OPF_SKIP          & m_nOpSettings) ||
				(OPF_CPY_SKIP_ALL  & m_nOpSettings))
			{
				return false;
			}
		}
		else{
			if(NULL != m_pStat){
				m_pStat->InitCurrentProgress(1, 1);
			}
		}
	}
	else	// non direct = copy + delete old
	{
		//delete previous temporary flags
		m_nOpSettings &= ~(OPF_TMP_FLAGS_MASK);

		bool bOk = SingleFileCopy(item);
        
		//on abort, do not erase original file
		if(m_pStat->IsAborted()){
			TRACE("OpMove: op aborted\n");
			return false;
		}

		if(bOk)
		{
			TRACE("OpMove: delete phase: %s\n", item.GetName().c_str());
			bOk = m_pVfsSrc->Delete(item, m_nOpSettings);
		}
		else{
			TRACE("OpMove: failed to copy file %s\n", item.GetName().c_str());
			return false;	// error msg already shown if copy failed
		}

		bError = !bOk;

		//FIX: don't trigger "Failed ..." message if that was user choice
		if( (OPF_SKIP          & m_nOpSettings) ||
		    (OPF_CPY_SKIP_ALL  & m_nOpSettings))
		{
			return false;
		}
	}

	if(bError){
		String strMsg;
		strMsg.Printf(_("Failed to move %s!"), item.GetName().c_str());//TOFIX separate msg for copy and delete?
							
		//ask user for Retry|Cancel|Abort
		int nRes = MsgOperationError_ThrSafe(strMsg);
		if(nRes == OPF_ABORT)
			m_pStat->Abort();
	}

	return !bError;
}

