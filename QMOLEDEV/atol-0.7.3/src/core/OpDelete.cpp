////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Delete operation implementation
////////////////////////////////////////////////////////////////////////////

#include "../ThreadSafeMsgs.h"
#include "OpDelete.h"
#include "opcodes.h"
#include "PathName.h"
#include "VfsLocal.h"
#include "util.h"
#include "debug.h"
#include "System.h"
#ifndef _WIN32
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#endif

OpDelete::OpDelete()
{
	m_nOpSettings = 0;
	m_bRecycleBin = false;
	m_nOpType = OP_DELETE;
}

OpDelete::~OpDelete()
{
}

//copy files from one VFS to the another
bool OpDelete::OpExecute()
{
	TRACE("OpDelete::OnExecute\n");

	if(m_pVfsSrc->GetType() == Vfs::LOCAL)
		((Vfs_Local *)m_pVfsSrc)->m_bRecycleBin = m_bRecycleBin;
	
	//Step 1: Expand selection (NOTE: we use file count for delete progress info)
	m_pVfsSrc->ExpandSelection(m_objSrcItems, m_pStat->m_bAbort);    //TOFIX pass full Stat
	m_pStat->m_nTotBytesMax = m_objSrcItems.GetTotalCount();

	//STEP 2: delete reucrsively
	String strDir = m_pVfsSrc->GetDir();    //in case of abort

	//TOFIX detect and fix all the parent directories having no access rights for the children deletion?
	
	int nRootCount = m_objSrcItems.m_lstRootItems.size();
	for(int i=0; i<nRootCount; i++)
	{
		//check for abort, ...
		if(m_pStat->IsAborted())
			break;

		//delete previous temporary flags
		m_nOpSettings &= ~(OPF_TMP_FLAGS_MASK);

		// ask confirmation for each top level dir that is not empty
		if( !(m_nOpSettings & OPF_DEL_ALL_DIRS) &&
		      m_objSrcItems.m_lstRootItems[i].IsDir()            &&
		      m_objSrcItems.m_lstRootItems[i].m_lstSubItems.size() > 0)
		{
			String strCurPath = m_pVfsSrc->GetDir();
			PathName::EnsureTerminated(strCurPath);
			strCurPath += m_objSrcItems.m_lstRootItems[i].GetName();
			m_pVfsSrc->FixPath(strCurPath);

			m_nOpSettings |= MsgDeleteDir_ThrSafe(System::FileNameToUTF8(strCurPath).c_str());
		}

		if(OPF_ABORT & m_nOpSettings)
			break;
		if(OPF_SKIP & m_nOpSettings)
			continue;

		DeleteRecursive(m_objSrcItems.m_lstRootItems[i]);
	}

	m_pVfsSrc->SetDir(strDir);    //restore starting dir

	TRACE("OpDelete::OnExecute end\n");
	return true;
}

void OpDelete::DeleteRecursive(VfsSelectionItem &item)
{
	//check for abort, ...
	if(m_pStat->IsAborted())
		return;

	String strItem = item.GetName();

	if(item.IsDir())
	{
		if(!item.IsDots())
		{
			//store current dir position, set panels to new dir
			String strSrcDir = m_pVfsSrc->GetDir();

			String strNewDir = strSrcDir;
			PathName::EnsureTerminated(strNewDir);
			strNewDir += strItem;
			m_pVfsSrc->FixPath(strNewDir);

			if(!item.IsLink())	//linked dir are not deleted, just a link file is
			{
			#ifndef _WIN32
				//ensure directory has +xw privileges on Unix platforms
				//(needed for deletion of the directory and the files inside it)
				//TOFIX implement for other VFS tpyes?
				if(m_pVfsSrc->GetType() == Vfs::LOCAL)
				{
					if(0 != laccess(strNewDir, W_OK|X_OK))
					{
						//add +xw rights to allow file delete
						uid_t uid = getuid();
						struct passwd *pwd = getpwuid(uid);
						if(NULL != pwd && 0 == chown(strNewDir.c_str(), pwd->pw_uid, pwd->pw_gid)){
							struct stat64 st;
							if(0 == lstat64(strNewDir.c_str(), &st))
								chmod(strNewDir.c_str(), st.st_mode|S_IWUSR|S_IXUSR);
						}
					}
				}
			#endif

				m_pVfsSrc->SetDir(strNewDir);    //TOFIX verify success

				//delete directory contents
				int nCount = item.m_lstSubItems.size();
				for(int i=0; i<nCount; i++)
				{
					//check for abort, ...
					if(m_pStat->IsAborted())
						break;

					DeleteRecursive(item.m_lstSubItems[i]);
				}

				//restore previous dir
				m_pVfsSrc->SetDir(strSrcDir);
			}
           
			if(m_pStat->IsAborted())
				return;

			//NOTE do this before initsingle
			m_pStat->InitCurrentFiles(System::FileNameToUTF8(strNewDir).c_str(), NULL);
			m_pStat->InitCurrentProgress(0, 1);

			//next delete directory itself
			if(!m_pVfsSrc->Delete(item, m_nOpSettings))
			{
				if(0 == (m_nOpSettings & OPF_DEL_SKIP_ON_ERR))
				{
					//TOFIX cumulative error collection -> message "Some files could not be deleted" -> like deleting an current directory
					String strMsg;
					strMsg.Printf("Failed to delete %s! Abort?", System::FileNameToUTF8(strItem).c_str());

					//TOFIX support for retry
					int nRes = MsgDeleteError_ThrSafe(strMsg);
					if(OPF_ABORT == nRes)
					{
						m_pStat->Abort();
						return;
					}
					else if(OPF_CPY_SKIP_ALL == nRes) 
					{
						m_nOpSettings |= OPF_DEL_SKIP_ON_ERR;	//do not notify for the next file delete error
					}
				}
			}

			//check if user aborted inside delete
			if(m_nOpSettings & OPF_ABORT){
				m_pStat->Abort();
				return;
			}

			m_pStat->StepPos(1);
		}
	}
	else
	{
		//NOTE: do this before initsingle
		String strPath = m_pVfsSrc->GetDir();
		PathName::EnsureTerminated(strPath);
		strPath += strItem;
		m_pVfsSrc->FixPath(strPath);

		m_pStat->InitCurrentFiles(System::FileNameToUTF8(strPath).c_str(), NULL);
		m_pStat->InitCurrentProgress(0, 1);

		if(!m_pVfsSrc->Delete(item, m_nOpSettings))
		{
			if(0 == (m_nOpSettings & OPF_DEL_SKIP_ON_ERR))
			{
				//TOFIX error handling, error dialog, abort operation?
				String strMsg;
				strMsg.Printf("Failed to delete %s! Abort?", System::FileNameToUTF8(strItem).c_str());

				//TOFIX support for retry
				int nRes =  MsgDeleteError_ThrSafe(strMsg);
				if(OPF_ABORT == nRes)
				{
					m_pStat->Abort();
					return;
				}
				else if(OPF_CPY_SKIP_ALL == nRes) 
				{
					m_nOpSettings |= OPF_DEL_SKIP_ON_ERR;	//do not notify for the next file delete error
				}
			}
		}

		m_pStat->StepPos(1);
	}
}
