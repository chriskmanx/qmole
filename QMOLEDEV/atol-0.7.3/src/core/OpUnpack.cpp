////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This class implements file decompress operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#include "util.h"
#include "OpUnpack.h"
#include "VfsArchive.h"
#include "PluginManager.h"
#include "PathName.h"

extern PluginManager g_PlugManager;

OpUnpack::OpUnpack()
{
	m_nOpType = OP_UNPACK;
}

OpUnpack::~OpUnpack()
{
}

bool OpUnpack::OpExecute()
{
	if(NULL != m_pVfsSrc)
		return Unpack();

	return false;
}

bool OpUnpack::Unpack()
{
	//STEP 1. clear selection list -> anything not file and archive goes out
	std::vector<VfsSelectionItem>::iterator It = m_objSrcItems.m_lstRootItems.begin();
	while(It != m_objSrcItems.m_lstRootItems.end())
	{
		bool bArchive	= false;
		bool bDir		= It->IsDir();

		if(!bDir)
		{
			//check if one of the suppoerted archive types
			String strExt = It->GetExt();

			//fill all supported plugin extensions in the combo
			String strExtensions;
			std::vector<String> lstTokenized;

			//check if extension matches any of the plugin supported
			int nMax = g_PlugManager.GetCount();
			for(int i=0; i<nMax; i++)
			{
				if(bArchive) break;

				strExtensions = g_PlugManager[i].m_strExtensions;
				Tokenize(strExtensions, lstTokenized, ';');
				for(unsigned int j=0; j<lstTokenized.size(); j++)
				{
					//see if plugin extension matches our
					if(0 == strExt.CmpNoCase(lstTokenized[j]))
					{
						bArchive = true;
						break;
					}
				}
			}
		}
		
        	if(bDir || !bArchive)
        	{
        		//delete from selection list
			std::vector<VfsSelectionItem>::iterator It2 = It; //save iterator from becoming invalid soon
			It2--;
			m_objSrcItems.m_lstRootItems.erase(It);
			It = It2;
        	}

        	It ++;
	}

	//now copy the list into new object
	VfsSelection lstArchives = m_objSrcItems;

	Vfs *pOrigBrowser = m_pVfsSrc;
	Vfs_Archive Archiver;

	//STEP 2. for each archive file selected, do the unpacking
	unsigned int i;
	for(i=0; i<lstArchives.m_lstRootItems.size(); i++)
	{
		//check for abort, ...
		if(m_pStat->IsAborted())
			break;

		//"repair" source pointer
		m_pVfsSrc = &Archiver;
		m_pVfsSrc->m_pProgress = NULL;

		//TOFIX 3. list file content
		String strFile = pOrigBrowser->GetDir();
		PathName::EnsureTerminated(strFile);
		strFile	+= lstArchives.m_lstRootItems[i].GetName();

		if(Archiver.InitArchiver(strFile))
		{
			//link to progress display
			Archiver.m_pProgress = m_pStat;	

			if(m_pVfsSrc->Open())
			{
				//select entire zip contents
				m_pVfsSrc->SetDir("/");	//TOFIX set root dir

				VfsListing list;
				bool bAbort;
				m_pVfsSrc->CachedListDir(list, bAbort, true);

				//declare all items from the listing as the one to be unpacked
				m_objSrcItems = list;

				//NOTE: it would be nicer to calculate total od all archives 
				// that are going to be unpacked, but it would be slower
				//
				//refresh total 
				m_pVfsSrc->ExpandSelection(m_objSrcItems, m_pStat->m_bAbort); //TOFIX pass full Stat?
				m_pStat->Reset();
				m_pStat->m_nTotBytesMax = m_objSrcItems.GetTotalSize();

				//perform standard copy (inherited function)
				Copy();

				m_pVfsSrc->Close();
			}
		}
        	//TOFIX else
        	//wxMessageBox()
	}

	return true;
}
