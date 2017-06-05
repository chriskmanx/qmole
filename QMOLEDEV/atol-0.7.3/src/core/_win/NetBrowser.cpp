////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "NetBrowser.h"
#include "../PathName.h"
#include "Pidl.h"
#include "Shell.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifndef SHGDN_INCLUDE_NONFILESYS  // mingw does not define in shlobj following define
    #define SHGDN_INCLUDE_NONFILESYS 0x2000
#endif    

//TOFIX remove this?
long g_hWndMain = 0;

NetBrowser::NetBrowser()
{
	m_bGoUp = false;

	// get Network Neighbourhood pidl
	SHGetSpecialFolderLocation(NULL, CSIDL_NETWORK, &m_pidlNetRoot);
	m_pidlCurRoot = NULL;
}

NetBrowser::~NetBrowser()
{
	PIDL::Free(m_pidlNetRoot);
	FreePidlMap();
}

bool NetBrowser::ListDir(VfsListing &list, bool &bAbort)
{
	list.Clear();

	//NOTE: click on ".." is handled using special flag
	std::map<std::string, LPITEMIDLIST>::iterator It1;
	if(m_bGoUp)
	{
		It1 = m_mapItems.find("..");
		m_bGoUp = false;
	}
	else{
		String strItem = m_strCurDir;
		PathName::EnsureNotTerminated(strItem);
		strItem = PathName::GetBaseName(strItem);
		It1 = m_mapItems.find(strItem.c_str());
	}

	bool bRes = true;
	if(It1 == m_mapItems.end()){
		if(NULL == m_pidlCurRoot)
			bRes = Populate(m_pidlNetRoot, list);
		else
			bRes = Populate(m_pidlCurRoot, list);	//refresh list with current root
	}
	else
		bRes = Populate(It1->second, list);

	//if populate failed
	m_bGoUp = false;
	static bool bOnce = true;
	if(!bRes & bOnce){
		bOnce = false;
		UpDir();
		bool bAbort = false;
		ListDir(m_lstData, bAbort);
		RefreshPath();
	}
	bOnce = true;
	
	m_bGoUp = false;
	return bRes;
}

bool NetBrowser::SetDir(const char *szPath)
{
	if(m_bGoUp)
		return true;

	m_strCurDir = szPath;
	PathName::EnsureTerminated(m_strCurDir);
	m_strCurDir.Replace("/", "\\");

	//can we switch to new root
	if(strlen(szPath)>1)
	{
		LPITEMIDLIST pidl = PIDL::GetFromPath(szPath);
		if(NULL != pidl)
		{
			//free old root
			if(m_pidlCurRoot != m_pidlNetRoot)
				PIDL::Free(m_pidlCurRoot);
			FreePidlMap();
			m_pidlCurRoot = pidl;	//new root
		}
	}

	return true;
}

bool NetBrowser::Populate(LPITEMIDLIST pidlRoot, VfsListing &list)
{
	IShellFolder* pRootFolder = PIDL::GetFolderPtr(pidlRoot);
	if(NULL == pRootFolder)
		return false;

	//FIX: make code safe for pidlOld = pidlNew
	LPITEMIDLIST pidlOldRoot = m_pidlCurRoot;

	//save new root
	m_pidlCurRoot = PIDL::CopyPIDL(pidlRoot);	//copy so we can free list
	
	//free previous root (not before now in case that oldRoot = newRoot)
	if(m_pidlCurRoot != m_pidlNetRoot)
		PIDL::Free(pidlOldRoot);

	// free all previous PIDLs
	FreePidlMap();

	// calc and add upDir PIDL as item ".."
	if(!PIDL::CmpEqual(m_pidlCurRoot, m_pidlNetRoot)){
		//get parent of the root
		LPITEMIDLIST pidlUpDir = PIDL::GetParent(m_pidlCurRoot);
		m_mapItems[".."] = pidlUpDir;
		VfsItem rootInfo;
		rootInfo.m_strName	= "..";
		rootInfo.m_nAttrib	= ATTR_DIR;
		//TOFIX fill date?,...
		rootInfo.CalcExt();
		list.Insert(rootInfo);
	}

	// enum child pidls
	IEnumIDList* pEnumIDList = NULL;

	//NOTE: it is extremely important to send valid HWND inside or else
	//      we won't have chance to get network login dialog (if such needed)
	//ASSERT(::IsWindow(g_info.hwndMain));
	if(0 == g_hWndMain)
		g_hWndMain = (long)GetDesktopWindow();

	HWND hwndMain =	(HWND)g_hWndMain;
	HRESULT hr = pRootFolder->EnumObjects(hwndMain, SHCONTF_FOLDERS|SHCONTF_NONFOLDERS|SHCONTF_INCLUDEHIDDEN, &pEnumIDList);
	if (NOERROR == hr)
	{
		LPITEMIDLIST pidl = NULL;
		while (NOERROR == pEnumIDList->Next(1, &pidl, NULL))
		{
			VfsItem item;
			FillItem(item, pRootFolder, pidl);
			list.Insert(item);
		
			// free enumerated object
			PIDL::Free(pidl);
		}
		// free enum object
		pEnumIDList->Release();
	}
	else
	{
		std::string strErr = SHELL::GetSystemErrorMessge(HRESULT_CODE(hr));
		//TRACE("Error %s (%d) listing LAN directory\n", strErr, HRESULT_CODE(hr));

		//TOFIX ensure this does not loses focus
		if(strErr.size()>0)
			::MessageBox(NULL, strErr.c_str(), "Atol", MB_OK|MB_ICONEXCLAMATION|MB_SYSTEMMODAL);

		pRootFolder->Release();
		return false;	
	}

	pRootFolder->Release();
	RefreshPath();
	return true;
}

bool NetBrowser::FillItem(VfsItem &item, LPSHELLFOLDER pFolder, LPITEMIDLIST pidl)
{
	//TOFIX why relative version is working, but not with absPidl
	//test for some item attributes
	DWORD dwAttributes = SFGAO_FOLDER|SFGAO_LINK|SFGAO_FILESYSTEM;
	PIDL::GetAttributes(pFolder, pidl, dwAttributes);

	// get display name relative to its parent folder
	String sName;
	if(SFGAO_FILESYSTEM == (dwAttributes & SFGAO_FILESYSTEM))
		PIDL::GetDisplayName(pFolder, pidl, sName, SHGDN_INFOLDER|SHGDN_FORPARSING|SHGDN_INCLUDE_NONFILESYS);
	else
		PIDL::GetDisplayName(pFolder, pidl, sName, SHGDN_INFOLDER|SHGDN_INCLUDE_NONFILESYS);
	
	//TOFIX? better solution
	//FIX: Win9x gives empty names for shared folders (1st level) using SHGDN_FORPARSING flag 
	if(0 == sName.size())
		PIDL::GetDisplayName(pFolder, pidl, sName, SHGDN_INFOLDER|SHGDN_INCLUDE_NONFILESYS);

	item.m_strName = sName;

	// create absolute child PIDL by concatenating
	// absolute parent PIDL with relative child PIDL
	LPITEMIDLIST pidlAbs = PIDL::MergeIDLists(m_pidlCurRoot, pidl);
	//ASSERT(NULL != pidlAbs);

	//map name to its absolute pidl
	m_mapItems[item.m_strName.c_str()] = pidlAbs;

	//see if the item is file or attribute 
	BOOL bTypeSet = FALSE;
	if(SFGAO_FILESYSTEM == (dwAttributes & SFGAO_FILESYSTEM))
	{
		//TOFIX GetUncFromPidl()
		char szPath[256] = "";
		::SHGetPathFromIDList(pidlAbs, szPath);
		if(!PathIsUNC(szPath))
			szPath[0] = '\0';

		if(strlen(szPath)>0)
		{
			//bypass operating systems plugins (like show zip file as folder)
			DWORD dwAttr = ::GetFileAttributes(szPath);
			if(dwAttr != 0xFFFFFFFF)
			{
				if(dwAttr & FILE_ATTRIBUTE_DIRECTORY)
					item.m_nAttrib = ATTR_DIR;
				else
					item.m_nAttrib = 0;
			
				bTypeSet = TRUE;
			}
		}
	}

	if(!bTypeSet)
	{
		if(SFGAO_FOLDER == (dwAttributes & SFGAO_FOLDER))
			item.m_nAttrib = ATTR_DIR;
		else
			item.m_nAttrib = 0;
	}

	// get item icon index
	UINT uFlags = SHGFI_PIDL|SHGFI_SYSICONINDEX|SHGFI_SMALLICON;
	if (dwAttributes & SFGAO_LINK)
		uFlags |= SHGFI_LINKOVERLAY;

	//calculate item index
	item.m_nIconIdx = PIDL::GetItemIcon(pidlAbs, uFlags);

	//TOFIX temporary patch to override for our custom icons
	String strExt = PathName::GetExt(item.m_strName);
	if(0 == strExt.CmpNoCase(".zip") || 0 == strExt.CmpNoCase(".enc"))
		item.m_nIconIdx = -1;
	
	//get size
	String strPath;
	if(PIDL::PathFromPIDL(pidlAbs, strPath) && strPath.size()>0)
	{
		if(ATTR_DIR == item.m_nAttrib)
		{
			item.m_nSize = -1;

			//TOFIX use this code also for files 
			//get directory time
			WIN32_FIND_DATA findFileData;
			HANDLE hFind = FindFirstFile(strPath, &findFileData);
			if (hFind != INVALID_HANDLE_VALUE)
			{
				// convert times as appropriate
				//TOFIX
				//item.m_nLastModDate = findFileData.ftLastWriteTime;
				FindClose(hFind);
			}
			else
				return FALSE;
		}
		else
		{
			//get file size
			struct _stat st;
			if(0 == _stat(strPath, &st))
				item.m_nSize = st.st_size;

			//get file time
			HANDLE hFile = CreateFile( strPath.c_str(),
				GENERIC_READ,
				0,
				NULL,
				OPEN_EXISTING,
				FILE_ATTRIBUTE_NORMAL,
				NULL );

			if(INVALID_HANDLE_VALUE != hFile)
			{
				// the file time (in UTC)
				FILETIME ftLastWrite;
				GetFileTime( hFile, NULL, NULL, &ftLastWrite );
				//FileTimeToLocalFileTime( &ftLastWrite, &ftLocal ); //TOFIX?
				
				//TOFIX convert to time_t
				//item.m_nLastModDate = ftLastWrite;
				CloseHandle(hFile);
			}
		}

		//get attributes
		//TOFIX convert into my portable attributes, do not kill exiting attribs
		//item.m_nAttrib = GetFileAttributes(strPath);
		
		//recalculate icon for file system items (needed for our icon overrides)
		if(PathIsUNC(strPath))
			item.m_nIconIdx = -1;
	}
	else
	{
		item.m_nSize = -1;
	}

	item.CalcExt();
	return TRUE;
}

void NetBrowser::FreePidlMap()
{
	//free map
	std::map<std::string, LPITEMIDLIST>::iterator It = m_mapItems.begin();
	while(It != m_mapItems.end()){
		PIDL::Free(It->second);
		It++;
	}
	m_mapItems.clear();
}

int NetBrowser::Execute(const char *szItem, bool bLocalDir)
{
	std::map<std::string, LPITEMIDLIST>::iterator It1 = m_mapItems.find(szItem);
	if(It1 == m_mapItems.end())
		return -1;
	else
	{
		//TOFIX support for all other filtering (zip files, ...)
		//return Vfs_Local::Execute(szFileName);
		SHELLEXECUTEINFO si;
		ZeroMemory(&si,sizeof(si));
		si.cbSize	= sizeof(si);
		si.hwnd		= (HWND)g_hWndMain;;
		si.nShow	= SW_SHOW;
		si.lpIDList = (LPVOID)It1->second;
		si.fMask	= SEE_MASK_INVOKEIDLIST/*|SEE_MASK_FLAG_NO_UI*/;
		ShellExecuteEx(&si);

		return true;
	}
}

bool NetBrowser::UpDir()
{
	m_bGoUp = true;
	return true;
}

bool NetBrowser::SetRootDir()
{
	if(m_pidlCurRoot != m_pidlNetRoot)
		PIDL::Free(m_pidlCurRoot);

	m_pidlCurRoot = NULL;	//this will trigger root listing
	return true;
}
	
String NetBrowser::GetPathTitle()
{
	if(m_strCurDir.IsEmpty())
	{
		SHFILEINFO fi;
		fi.szDisplayName[0] = '\0';
		SHGetFileInfo((LPCTSTR)m_pidlCurRoot, 0, &fi, sizeof(fi), SHGFI_PIDL|SHGFI_DISPLAYNAME);

		String strRes("\\\\");
		if(strlen(fi.szDisplayName) > 0)
		{
			strRes += fi.szDisplayName;
			strRes += "\\";
		}
		return strRes;
	}
	else
		return m_strCurDir;
}

bool NetBrowser::IsRootDir()
{
	return PIDL::CmpEqual(m_pidlCurRoot, m_pidlNetRoot);
}

void NetBrowser::RefreshPath()
{
	//TOFIX GetUncFromPidl()
	char szPath[256] = "";
	::SHGetPathFromIDList(m_pidlCurRoot, szPath);
	m_strCurDir = szPath;
	PathName::EnsureTerminated(m_strCurDir);
	m_strCurDir.Replace("/", "\\");

	if(!PathIsUNC(m_strCurDir))
	{
		m_strCurDir.Empty();

		//if search failed try to calculate path using the path of directory child
		//find first child path
		IShellFolder* pRootFolder = PIDL::GetFolderPtr(m_pidlCurRoot);
		if(NULL == pRootFolder)
			return;

		// get first child pidl
		IEnumIDList* pEnumIDList = NULL;

		//NOTE: it is extremely important to send valid HWND inside or else
		//      we won't have chance to get network login dialog (if such needed)
		//ASSERT(::IsWindow(g_info.hwndMain));
		HWND hwndMain = (HWND)g_hWndMain;
		HRESULT hr = pRootFolder->EnumObjects(hwndMain, SHCONTF_FOLDERS|SHCONTF_NONFOLDERS|SHCONTF_INCLUDEHIDDEN, &pEnumIDList);
		if (NOERROR == hr)
		{
			LPITEMIDLIST pidl = NULL;
			if (NOERROR == pEnumIDList->Next(1, &pidl, NULL))
			{
				LPITEMIDLIST pidlAbs = PIDL::MergeIDLists(m_pidlCurRoot, pidl);
				//ASSERT(NULL != pidlAbs);

				::SHGetPathFromIDList(pidlAbs, szPath);
				
				if(strlen(szPath)>0)
				{
					m_strCurDir = PathName::GetParentDirPath(szPath);
					m_strCurDir.Replace("/", "\\");
				}

				// free enumerated object
				PIDL::Free(pidl);
				PIDL::Free(pidlAbs);
			}
			// free enum object
			pEnumIDList->Release();
		}
		pRootFolder->Release();
  }
}

void NetBrowser::ShowCtxMenu(VfsSelection &items, int x, int y)
{
/*
*/
	{
		int nCount = items.GetRootCount();
		//ASSERT(nCount > 0);

		std::vector<std::string> lstItems;
		for(int i=0; i<nCount; i++)
			lstItems.push_back(items.m_lstRootItems[i].GetName().c_str());

		if(nCount > 0)
		{
			//TOFIX separate code below into new function?
			//BOOL SHELL::MyContextMenu(LPITEMIDLIST pidlParent, std::vector<CString> &lstItems, UINT nFlags, LPPOINT ptPoint, HWND hWnd);
			HRESULT hr;
			IContextMenu  *pCtxMenu;
			int i;
			LPITEMIDLIST *pItems = NULL;
			BOOL bRes = FALSE;
			DWORD nFlags = TPM_LEFTBUTTON|TPM_LEFTALIGN|TPM_TOPALIGN;

			//TOFIX zasto kopiramo pidl
			// get the root's shell folder for the pidl. we need this for getting the relative pidls for all the files.
			LPSHELLFOLDER pParent = PIDL::GetFolderPtr(PIDL::CopyPIDL(m_pidlCurRoot));
			if(pParent == NULL)
				goto cleanup;

			//gather relative pidls
			pItems = new LPITEMIDLIST [nCount];
			if(NULL == pItems)
				goto cleanup;

			//NOTE: use path relative to parent directory
			for(i=0; i<nCount; i++){
				LPITEMIDLIST pidl = m_mapItems[lstItems[i]];
				pItems[i] = PIDL::CopyPIDL((LPITEMIDLIST)PIDL::GetAt(pidl, PIDL::GetItemCount(pidl)-1));
				//ASSERT(NULL != pItems[i]);
				if(NULL == pItems[i])	//if any of the pidls failed
					goto cleanup;
			}

			if( SUCCEEDED(pParent->GetUIObjectOf(NULL, nCount, (LPCITEMIDLIST *)pItems, 
				IID_IContextMenu, NULL, (void**)&pCtxMenu)) )
			{
				HMENU ctxMenu = CreatePopupMenu();
				if( !ctxMenu )
					return; // FALSE;
			//	
				if( FAILED(hr = pCtxMenu->QueryContextMenu(ctxMenu, 0,
					1, 10000, CMF_NORMAL)) )
					goto cleanup;

				nFlags &= ~TPM_NONOTIFY;
				nFlags |= TPM_RETURNCMD;

				HWND hWnd = (HWND)g_hWndMain;
				UINT nResult = ::TrackPopupMenuEx(ctxMenu, nFlags, x, y, hWnd, NULL);

				if( nResult > 0 && nResult < 10000 ) {
					//TRACE("Invoke cmd!");
					CMINVOKECOMMANDINFO cmi;
					ZeroMemory(&cmi, sizeof(cmi));
					cmi.cbSize  = sizeof(cmi);
					cmi.hwnd    = hWnd;
					cmi.lpVerb  = MAKEINTRESOURCE(nResult - 1);
					cmi.nShow   = SW_SHOWNORMAL;
					hr = pCtxMenu->InvokeCommand(&cmi);
					bRes = SUCCEEDED(hr);
				}
				//else
				//	TRACE("ERROR: Context menu command not supported\n");	//TOFIX ? Copy/Paste
			}

		cleanup:

			//TRACE("Cleanup!");
			//clean all data, delete PIDLS, delete pointers, ...
			if(NULL != pParent)
				pParent->Release();

			if(NULL != pItems){
				for(int i=0; i<nCount; i++)
					PIDL::Free( pItems[i] );
				delete [] pItems;
			}
		}
		//PIDL::GetLastChain
	}
}


