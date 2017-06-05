#include "FileViewer.h"
#include "FileViewerWnd.h"
#include "../core/System.h"
#include <vector>
#include <algorithm>

std::vector<FileViewerWnd *> g_lstViewers;

bool Lister_ViewFile(const char *szFile, int nViewType, bool bDelete)
{
	//TOFIX handle file view request
	FileViewerWnd *pWnd = new FileViewerWnd;
	pWnd->m_bDeleteOnClose = bDelete;
	pWnd->Create();
	pWnd->FileLoad(szFile);
	
	g_lstViewers.push_back(pWnd);
	return true;
}

void Viewer_OnDestroy(FileViewerWnd *pObj)
{
	//destroy timer
	if(pObj->m_nTimer > 0)
		g_source_remove (pObj->m_nTimer);
	pObj->m_nTimer = 0;

	//handle request for handling temp file deletion
	if(pObj->m_bDeleteOnClose)
	{
		String strFile = pObj->GetFileName();
		pObj->FileClose();
		System::Remove(strFile);
	}

	//remove object from the list
	std::vector<FileViewerWnd *>::iterator It = 
		std::find(g_lstViewers.begin(), g_lstViewers.end(), pObj);

	if(It != g_lstViewers.end())
		g_lstViewers.erase(It);

	//TRACE("Delete the viewer object\n");
	delete pObj;
}

