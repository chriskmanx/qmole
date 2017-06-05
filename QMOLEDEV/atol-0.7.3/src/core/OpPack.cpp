////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This class implements file compress operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#include "OpPack.h"
#include "OpDelete.h"
#include "opcodes.h"
#include "VfsArchive.h"
#include "PathName.h"
#include "System.h"
#include "debug.h"

//TOFIX GUI dependency in core code?
#include "../support.h"
#include "../ThreadSafeMsgs.h"

#ifdef _WIN32
 #include <io.h>  //access
#else
 #include<unistd.h>
#endif 

OpPack::OpPack()
{
	m_nOpType = OP_PACK;
}

OpPack::~OpPack()
{
}

bool OpPack::OpExecute()
{
	TRACE("PoPack::Execute start\n");

	if(NULL != m_pVfsSrc)
		return Pack();

	TRACE("PoPack::Execute end\n");
	return false;
}

//TOFIX add support for move to archive and sfx
bool OpPack::Pack()
{
	//ASSERT(m_pVfsSrc->GetType() == VFS::_DISC || m_pVfsSrc->GetType() == VFS::_NET);
	//ASSERT(m_pVfsDst->GetType() == VFS::_DISC || m_pVfsDst->GetType() == VFS::_NET);

	String strDir = PathName::GetParentDirPath(m_strArchive);
	if(!System::EnsureDirExists(strDir))
		return false;

	TRACE("PoPack::Pack start\n");

	//TOFIX use m_nPluginIdx 
	Vfs *pOrigDest = NULL;

	//TOFIX support for disk spanning see SetSpanCallback()
	Vfs_Archive Archiver;
	if(Archiver.InitArchiver(m_strArchive))
	{
		//check if archive already exists (file overwrite)
		if(0 == access(m_strArchive.c_str(), 0)){
			TRACE("PoPack::Pack archive %s already exists!\n", m_strArchive.c_str());

			//ask to overwrite
			String strMsg;
			strMsg.Printf(_("File %s already exists. Overwrite?"), m_strArchive.c_str());
			if(GTK_RESPONSE_YES != MsgBox_ThrSafe(strMsg, GTK_BUTTONS_YES_NO))
				return false;
			
			//remove file
			#ifdef _WIN32
				DeleteFile(m_strArchive);
			#else
				remove(m_strArchive.c_str());
			#endif
		}

		if(Archiver.Open())
		{
			TRACE("PoPack::Pack archive open\n");

			//link to progress display
			Archiver.m_pProgress = m_pStat;	

			//"repair" destination pointer
			pOrigDest    = m_pVfsDst;
			m_pVfsDst    = &Archiver;

			//2. classic copy (inherited class)
			Copy();

			Archiver.Close();

			TRACE("OpPack::Pack copy finished!\n");

			//"move to archive" feature, proceed only if all files copied successfully
			if(m_bMove && !m_bError)
			{
				//run delete operation (in this thread) on selected source files
				OpDelete objDel;
				objDel.m_pStat = new OpState; //TOFIX attach to progress?
				objDel.m_nOpSettings = OPF_DEL_ALL_DIRS | OPF_DEL_ALL_RO_FILES; //do not ask for confirmation
				objDel.m_pVfsSrc = m_pVfsSrc;
				objDel.m_objSrcItems = m_objSrcItems;
				objDel.RunOperation();
			}

			//TOFIX Copy() does not set m_bError
			//TOFIX what if copy failed to copy 1 of 100 files (delete only those files)
		}
	}

	TRACE("PoPack::Pack done!\n");
	return true;
}

