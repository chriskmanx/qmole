////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Defines thread for a file search operation
//////////////////////////////////////////////////////////////////////////// 

#include "FileSearchDlg.h"
#include "FileSearchThread.h"
#include "core/pthread.h"
#include "core/VfsLocal.h"
#include "core/PathName.h"
#include "core/debug.h"

FileSearchThread::FileSearchThread()
{
	m_bRecursive = false;
	m_bAbort  = false;
	m_bDone   = false;
	m_pCaller = NULL;
}

FileSearchThread::~FileSearchThread()
{
}

void FileSearchThread::Abort()
{
	Mutex cs;
	cs.Lock();

	m_bAbort = true;    //signalize abort

	cs.Unlock();

	TRACE("FileSearchThread::Abort abort = %d\n", m_bAbort);
}

void FileSearchThread::MainMethod()
{
	TRACE("FileSearchThread: starting\n");

	m_bDone = false;

	TRACE("FileSearchThread: recursive=%d\n", m_bRecursive);

	Vfs *pVfs = NULL;

	//TOFIX separate Tokenize(str, delimiter, &list) method!
	//get next directory token (multiple names separated with ;)
	String strDir;

	int nLength   = m_strDirectory.Length();
	int nStartPos = 0;
	int nEndPos   = 0;

	while(nStartPos < nLength)
	{
		//check abort
		if(m_bAbort){
			m_bDone = true;
			//TOFIX use event to be multithread safe
			((FileSearchDlg *)m_pCaller)->m_evOpDone.Set();	//FIX OnSearchDone();
			TRACE("FileSearchThread: exiting\n");
			return;
		}

		//get next path token
		nEndPos = m_strDirectory.find(';', nStartPos);
		if(nEndPos > 0){
			//get the piece of the string
			strDir = m_strDirectory.substr(nStartPos, nEndPos-nStartPos).c_str();
			nStartPos = nEndPos + 1;
		}
		else{
			//get the rest of the string
			strDir = m_strDirectory.substr(nStartPos, nLength-nStartPos).c_str();
			nStartPos = nLength;
		}
		TRACE("FileSearchThread: search dir=%s\n", strDir.c_str());

		//delete previous VFS
		if(NULL != pVfs){
			delete pVfs;
			pVfs = NULL;
		}

		//TOFIX move to vfsManager, support for archives?
		//use different VFS objects depending on search path type
		//if(PathIsUNC(strPath))
		//    pVFS = new Vfs_Net;
		//else
			pVfs = new Vfs_Local;
        
		if(NULL == pVfs)
			break;

		//search in this path
		if(pVfs->SetDir(strDir))
		{
			VfsListing list;
			pVfs->CachedListDir(list, m_bAbort);

			//keep only items that satisfy the search terms
			int nSize = list.GetCount();
			for(int i=0; i<nSize; i++)
			{
				if(m_bAbort){
					TRACE("FileSearchThread: abort detected 1\n");
					break;
				}

				//pSheet->m_nFilesSearched ++;

				//dots are not being part of the search result
				if(!list.GetAt(i).IsDots())
				{
					//set item absolute path
					list.GetAt(i).m_strPath = pVfs->GetDir();    //TOFIX SetPath

					VfsItem item = list.GetAt(i);

					//TRACE("FileSearchThread: match item %s ...", item.GetName().c_str());

					//filter by search data (name, ...)
					// TOFIX set event instead of direct inter-thread call!!!
					if(m_objInfo.Match(item))
						((FileSearchDlg *)m_pCaller)->AddItem(item);

					//TRACE(" done\n");
				} 
			} // for

			//now use same directory and its list to do recursive subdir processing if needed
			if(m_bRecursive)
			{
				TRACE("FileSearchThread: recursing recursive=%d\n", m_bRecursive);
				if(m_bAbort){
					TRACE("FileSearchThread: abort detected 2\n");
					break;
				}

				RecursiveList(pVfs, list);
			}

			TRACE("FileSearchThread: done searching dir=%s\n", strDir.c_str());			
		}
	}

	TRACE("FileSearchThread: preparing to exit\n");			
	if(NULL != pVfs)
		delete pVfs;
	m_bDone = true;

	//TOFIX use event instead!!!
	TRACE("FileSearchThread: preparing to exit 2\n");
	((FileSearchDlg *)m_pCaller)->m_evOpDone.Set();
	TRACE("FileSearchThread: exiting\n");
}

//TOFIX? use VFS::ExpandList(CFileList &list, int nStart)
void FileSearchThread::RecursiveList(Vfs *pVFS, VfsListing &lstRoot)
{
	TRACE("FileSearchThread:RecursiveList\n");
	String strDir = pVFS->GetDir();

	//for each subdir
	int nSize = lstRoot.GetCountRaw();
	for(int i=0; i<nSize; i++)
	{
		if(m_bAbort){
			TRACE("FileSearchThread: abort detected 3\n");
			break;
		}

		//if subdir found
		//TOFIX if subdir OR archive found
		if(lstRoot.GetAt(i).IsDir() && !lstRoot.GetAt(i).IsDots())
		{
			// list subdir
			String strPath(strDir);
			PathName::EnsureTerminated(strPath);
			strPath += lstRoot.GetAt(i).GetName();
			pVFS->FixPath(strPath);

			if(pVFS->SetDir(strPath))
			{
				//TOFIX list into the temp
				VfsListing list;
				pVFS->CachedListDir(list, m_bAbort);

				//keep only ones that satisfy the search terms
				int nSizeNew = list.GetCountRaw();
				for(int j=0; j<nSizeNew; j++)
				{
					if(m_bAbort){
						TRACE("FileSearchThread: abort detected 4\n");
						return;
					}

					//m_nFilesSearched ++;

					//dots are not being part of the search result
					if(!list.GetAtRaw(j).IsDots())
					{
						//repair path
						list.GetAtRaw(j).m_strPath = pVFS->GetDir();

						VfsItem item;
						item = list.GetAtRaw(j);

						//filter by search data (name, ...)
						//TOFIX thread safe?
						if(m_objInfo.Match(item))
							((FileSearchDlg *)m_pCaller)->AddItem(item);
					}
				} // for

				if(m_bAbort){
					TRACE("FileSearchThread: abort detected 5\n");
					return;
				}

				//second pass to go deeper into recursion
				//now use same directory and its list to do recursive subdir processing if needed
				RecursiveList(pVFS, list); //TOFIX member of CBrowser?
			} // if SetDir
		}
	} //for
}

