////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "Shell.h"
#include "../PathName.h"
#include <SHLOBJ.H>
#include <io.h>	//access
#include <direct.h>	//_mkdir
#include <string>

// global variables used for passing data to the subclassing wndProc
WNDPROC g_pOldWndProc = NULL;		// regular window proc
LPCONTEXTMENU2 g_pIContext2 = NULL;	// active shell context menu
LPCONTEXTMENU3 g_pIContext3 = NULL;	// active shell context menu

HRESULT GetSHContextMenu(LPSHELLFOLDER psfFolder, LPCITEMIDLIST *localPidls, int nCount,
                         void** ppCM, int* pcmType);

LRESULT CALLBACK CtxMenuHookWndProc(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp);

BOOL SHELL::MyContextMenu(LPCSTR szParentDir, std::vector<std::string> &lstItems, UINT nFlags, int x, int y, HWND hWnd)
{
	if(!IsWindow(hWnd))
		return FALSE;
	
	int nCount = lstItems.size();
	if(nCount < 1)
		return FALSE;
	
	LPITEMIDLIST pRootPidl = PIDL::GetFromPath(szParentDir);
	if(NULL == pRootPidl)
		return FALSE;
	
	BOOL bRes = SHELL::MyContextMenu(pRootPidl, lstItems, nFlags, x, y, hWnd);
	PIDL::Free(pRootPidl);
	return bRes;
}

BOOL SHELL::MyContextMenu(LPITEMIDLIST pidlParent, std::vector<std::string> &lstItems, UINT nFlags, int x, int y, HWND hWnd)
{
	if( !IsWindow(hWnd) )
		return FALSE;
	
	int nCount = lstItems.size();
	if(nCount < 1)
		return FALSE;
	
	HRESULT hr;
	IContextMenu  *pCtxMenu;
	
	int i;
	LPITEMIDLIST *pItems = NULL;
	
	BOOL bRes = FALSE;
	
	// get the root's shell folder for the pidl. we need this for getting the relative pidls for all the files.
	LPSHELLFOLDER pParent = PIDL::GetFolderPtr(pidlParent);
	if(pParent == NULL)
		return FALSE;
	
	//gather relative pidls
	//pItems = new LPITEMIDLIST [nCount];
	pItems = (LPITEMIDLIST *)PIDL::GetMalloc()->Alloc(nCount*sizeof(LPITEMIDLIST));
	if(NULL == pItems)
		goto cleanup;
	
	//NOTE: use path relative to parent directory
	for(i=0; i<nCount; i++){
		pItems[i] = PIDL::GetFromParentFolder(pParent, lstItems[i].c_str());
		//ASSERT(NULL != pItems[i]);
		if(NULL == pItems[i])	//if any of the pidls failed
			goto cleanup;
	}
	
	//GetSHContextMenu
	// m_psfFolder: active IShellFolder; m_localPidl: selected item
    int cmType; // "version" # of context menu
	hr = GetSHContextMenu(pParent, (LPCITEMIDLIST *)pItems, nCount, (void**)&pCtxMenu, &cmType);

	//if( SUCCEEDED(pParent->GetUIObjectOf(NULL, nCount, (LPCITEMIDLIST *)pItems, 
	//	IID_IContextMenu, NULL, (void**)&pCtxMenu)) )
	if( SUCCEEDED(hr) )
	{
		// install the subclassing "hook", for versions 2 or 3
		if(cmType > 1) {
			g_pOldWndProc = (WNDPROC)
				SetWindowLong(hWnd, GWL_WNDPROC, (DWORD)CtxMenuHookWndProc);
			
			if(2 == cmType)
				g_pIContext2 = (LPCONTEXTMENU2)pCtxMenu;
			else
				g_pIContext3 = (LPCONTEXTMENU3)pCtxMenu;
		}
		else g_pOldWndProc = NULL;
		
		HMENU menu = CreatePopupMenu();
		if( !menu ){
			bRes = FALSE;
			goto cleanup;
		}
		
		//define command ID range for shell's commands
		//(we can use all numbers outside of this range for ourselves)
#define MIN_SHELL_ID 1
#define MAX_SHELL_ID 30000
		
		try{
			//hr = pCtxMenu->QueryContextMenu(menu, 0, 1, 10000, CMF_NORMAL);
			hr = pCtxMenu->QueryContextMenu(menu, 0, MIN_SHELL_ID, MAX_SHELL_ID, CMF_NORMAL|CMF_EXPLORE);
		}
		catch(...)
		{
			//TRACE("Error: QueryContextMenu failed\n\n");
			goto cleanup;
		}
		
		if(FAILED(hr))
			goto cleanup;
		
		nFlags &= ~TPM_NONOTIFY;
		nFlags |= TPM_RETURNCMD;
		UINT iCmd = ::TrackPopupMenuEx(menu, nFlags, x, y, hWnd, NULL);
				
		//if the command is shell command (inside given range), execute it
		if( iCmd >= MIN_SHELL_ID && iCmd <= MAX_SHELL_ID ) 
		{
			//invoke command
			CMINVOKECOMMANDINFO cmi;
			ZeroMemory(&cmi, sizeof(cmi));
			cmi.cbSize  = sizeof(cmi);
			cmi.lpVerb  = (LPSTR)MAKEINTRESOURCE(iCmd - MIN_SHELL_ID);	//by position
			cmi.hwnd    = hWnd;
			cmi.nShow   = SW_SHOWNORMAL;

			hr = pCtxMenu->InvokeCommand(&cmi);
			bRes = SUCCEEDED(hr);
		}
		//else
		//	TRACE("ERROR: Context menu command not supported\n");	//TOFIX ? Copy/Paste
	}
	
cleanup:
	if(g_pOldWndProc) // restore old wndProc
		SetWindowLong(hWnd, GWL_WNDPROC, (DWORD)g_pOldWndProc);
	
	// prevents accidental use
	g_pIContext2 = NULL;
	g_pIContext3 = NULL;
	
	//clean all data, delete PIDLS, delete pointers, ...
	if(NULL != pParent)
		pParent->Release();
	
	if(NULL != pItems){
		for(int i=0; i<nCount; i++)
			PIDL::Free( pItems[i] );
		PIDL::Free(pItems);
	}
	
	if(pCtxMenu)
		pCtxMenu->Release();
	
	return bRes;
}

BOOL SHELL::TrackItemIDContextMenu(LPCTSTR pszPath, UINT nFlags, LPPOINT ptPoint, HWND hWnd)
{
	if( pszPath == NULL || pszPath[0] == 0 )
		return FALSE;
	
	LPITEMIDLIST pidl = PIDL::GetFromPath(pszPath);
	if(NULL != pidl)
	{
		BOOL bRes = TrackItemIDContextMenu(pidl, nFlags, ptPoint, hWnd);
		PIDL::Free(pidl);
		return bRes;
	}
	
	return FALSE;
}

BOOL SHELL::TrackItemIDContextMenu(LPITEMIDLIST pidl, UINT nFlags, LPPOINT ptPoint, HWND hWnd)
{
	typedef struct {
		IContextMenu2*	pContextMenu2;
		WNDPROC			pOldWndProc;
	} _SUBCLASSDATA;
	
	HRESULT hr;
	LPCITEMIDLIST pidlRel;
	IShellFolderPtr  pParent;
	IContextMenu  *pCtxMenu;
	IContextMenu2 *pCtxMenu2;
	
	if( NULL == pidl || !IsWindow(hWnd) )
		return FALSE;
	
	//TOFIX later free reference to parent and free pidlrel?
	if( SUCCEEDED(hr = SHBindToParent(pidl, __uuidof(pParent),
		(void**)&pParent, &pidlRel)) )
	{
		if( SUCCEEDED(pParent->GetUIObjectOf(NULL, 1, &pidlRel, 
			IID_IContextMenu, NULL, (void**)&pCtxMenu)) )
		{
			HMENU ctxMenu = CreatePopupMenu();
			if( !ctxMenu )
				return FALSE;
			//	
			if( FAILED(hr = pCtxMenu->QueryContextMenu(ctxMenu, 0,
				1, 10000, CMF_NORMAL)) )
				return FALSE;
			
			try{ pCtxMenu2 = (IContextMenu2 *)pCtxMenu; } catch( ... ) {/*do nothing*/}
			_SUBCLASSDATA *pSubclassData  = new _SUBCLASSDATA;
			pSubclassData->pContextMenu2 = (IContextMenu2*)pCtxMenu2;
			pSubclassData->pOldWndProc = (WNDPROC)::SetWindowLong(hWnd,
				GWL_WNDPROC, (LONG)_CtxMenuWndProc);
			::SetProp(hWnd, "PxShlApi::SubclassData", (HANDLE)pSubclassData);
			
			nFlags &= ~TPM_NONOTIFY;
			nFlags |= TPM_RETURNCMD;
			UINT nResult = ::TrackPopupMenuEx(ctxMenu, nFlags,
				ptPoint->x, ptPoint->y, hWnd, NULL);
			::SetWindowLong(hWnd,
				GWL_WNDPROC, (LONG)pSubclassData->pOldWndProc);
			::RemoveProp(hWnd, "PxShlApi::SubclassData");
			delete pSubclassData;
			
			if( nResult > 0 && nResult < 10000 ) {
				CMINVOKECOMMANDINFO cmi;
				ZeroMemory(&cmi, sizeof(cmi));
				cmi.cbSize  = sizeof(cmi);
				cmi.hwnd    = hWnd;
				cmi.lpVerb  = MAKEINTRESOURCE(nResult - 1);
				cmi.nShow   = SW_SHOWNORMAL;
				hr = pCtxMenu->InvokeCommand(&cmi);
				return SUCCEEDED(hr);
			}
		}
	}
	return FALSE;
}

HRESULT SHELL::SHBindToParent(LPITEMIDLIST pidl, REFIID riid, VOID **ppv, LPCITEMIDLIST *ppidlLast)
{
	HRESULT hr;
	if( NULL == pidl || !ppv )
		return E_POINTER;
	int nCount = PIDL::GetItemCount(pidl);
	if( nCount == 0 ) {
		//	this is desktop pidl or invalid PIDL
		return E_POINTER;
	}
	if( nCount == 1 ) try {
		//	this is desktop item
		if( SUCCEEDED(hr = PIDL::GetDesktopFolder()->QueryInterface(riid, ppv)) )
		{
			if( ppidlLast ) *ppidlLast = CopyItemID(pidl);
		}
		return hr;
	} catch ( _com_error e ) {
		return e.Error();
	}
	try {
		LPBYTE pRel = (LPBYTE)PIDL::GetAt(pidl, nCount-1);
		LPITEMIDLIST pidlPar = CopyItemID(pidl, pRel-(LPBYTE)(LPCITEMIDLIST)pidl);
		IShellFolderPtr pFolder;
		
		if( FAILED(hr = PIDL::GetDesktopFolder()->BindToObject(pidlPar, NULL,
			__uuidof(pFolder), (void**)&pFolder)) )
		{
			return hr;
		}
		if( SUCCEEDED(hr = pFolder->QueryInterface(riid, ppv)) )
		{
			if( ppidlLast ) *ppidlLast = CopyItemID((LPITEMIDLIST)pRel);
		}
		return hr;
	} catch ( _com_error e ) {
		return e.Error();
	}
}

LRESULT CALLBACK SHELL::_CtxMenuWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	typedef struct {
		IContextMenu2*	pContextMenu2;
		WNDPROC			pOldWndProc;
	} _SUBCLASSDATA;
	
	_SUBCLASSDATA *pSubclassData = (_SUBCLASSDATA*)::GetProp(hWnd, "PxShlApi::SubclassData");
	//ASSERT(pSubclassData);
	if( pSubclassData ) {
		switch( uMsg ) {
		case WM_INITMENUPOPUP:
		case WM_DRAWITEM:
		case WM_MENUCHAR:
		case WM_MEASUREITEM:
			if( pSubclassData->pContextMenu2 && 
				SUCCEEDED(pSubclassData->pContextMenu2->HandleMenuMsg(uMsg, wParam, lParam)) )
				return TRUE;
			return FALSE;
		}
		return CallWindowProc(pSubclassData->pOldWndProc,
			hWnd, uMsg, wParam, lParam);
	}
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

//TOFIX move
LPITEMIDLIST SHELL::CopyItemID(LPITEMIDLIST pidl, int cb/*=-1*/) 
{ 
	if(NULL == PIDL::GetMalloc())
		return NULL;
	
	//	Get the size of the specified item identifier. 
	if( cb == -1 )
		cb = PIDL::GetSize1(pidl); 
	
	//	Allocate a new item identifier list. 
	LPITEMIDLIST pidlNew = (LPITEMIDLIST)PIDL::GetMalloc()->Alloc(cb+sizeof(USHORT));
	if(pidlNew == NULL) return NULL; 
	
	//	Copy the specified item identifier. 
	CopyMemory(pidlNew, pidl, cb); 
	
	//	Append a terminating zero. 
	*((USHORT *)(((LPBYTE) pidlNew) + cb)) = 0; 
	
	return pidlNew; 
}

BOOL SHELL::DefaultAction(LPCSTR szPath)
{
	HRESULT hr;
	IContextMenu *pCtxMenu;
	LPITEMIDLIST *pItems = NULL;
	
	BOOL bRes = FALSE;
	
	LPITEMIDLIST pRootPidl = PIDL::GetFromPath(PathName::GetParentDirPath(szPath));
	if(NULL == pRootPidl)
		return FALSE;
	
	// get the root's shell folder for the pidl. we need this for getting the relative pidls for all the files.
	LPSHELLFOLDER pParent = PIDL::GetFolderPtr(pRootPidl);
	if(pParent == NULL)
		goto cleanup;
	
	//gather relative pidls
	pItems = new LPITEMIDLIST;
	if(NULL == pItems)
		goto cleanup;
	
	//NOTE: use path relative to parent directory
	*pItems = PIDL::GetFromParentFolder(pParent, PathName::GetBaseName(szPath));
	
	if( SUCCEEDED(pParent->GetUIObjectOf(NULL, 1, (LPCITEMIDLIST *)pItems, 
		IID_IContextMenu, NULL, (void**)&pCtxMenu)) )
	{
		HMENU ctxMenu = CreatePopupMenu();
		if( !ctxMenu )
			return FALSE;
		//	
		if( FAILED(hr = pCtxMenu->QueryContextMenu(ctxMenu, 0,
			1, 10000, CMF_NORMAL)) )
			goto cleanup;
		
		UINT nResult = GetMenuDefaultItem(ctxMenu, FALSE, GMDI_GOINTOPOPUPS);
		
		if( nResult > 0 && nResult < 10000 ) {
			CMINVOKECOMMANDINFO cmi;
			ZeroMemory(&cmi, sizeof(cmi));
			cmi.cbSize  = sizeof(cmi);
			cmi.hwnd    = NULL;
			cmi.lpVerb  = MAKEINTRESOURCE(nResult-1);
			cmi.nShow   = SW_SHOWNORMAL;
			hr = pCtxMenu->InvokeCommand(&cmi);
			bRes = SUCCEEDED(hr);
		}
		//else
		//	TRACE("ERROR: Context menu command not supported\n");	//TOFIX ? Copy/Paste
	}
	
cleanup:
	
	//clean all data, delete PIDLS, delete pointers, ...
	if(NULL != pParent)
		pParent->Release();
	
	if(NULL != pItems){
		PIDL::Free( *pItems );
		delete pItems;
	}
	
	if(NULL != pRootPidl)
		PIDL::Free(pRootPidl);
	
	return bRes;
}

void SHELL::WinExecute(LPCSTR szPath)
{
	TCHAR key[MAX_PATH + MAX_PATH];
	
    HINSTANCE result;
	
	if (GetRegKey(HKEY_CLASSES_ROOT, PathName::GetExt(szPath), key) == ERROR_SUCCESS) 
	{
        lstrcat(key, "\\shell\\open\\command");
		
        if (GetRegKey(HKEY_CLASSES_ROOT,key,key) == ERROR_SUCCESS) {
            TCHAR *pos;
            pos = strstr(key, "\"%1\"");
            if (pos == NULL) {                     // No quotes found
                pos = strstr(key, "%1");       // Check for %1, without quotes
                if (pos == NULL)                   // No parameter at all...
                    pos = key+lstrlen(key)-1;
                else
                    *pos = '\0';                   // Remove the parameter
            }
            else
                *pos = '\0';                       // Remove the parameter
			
            lstrcat(pos, " ");
            lstrcat(pos, szPath);
            result = (HINSTANCE) WinExec(key,SW_SHOW);
        }
    }
}

//NOTE: allocates buffer, requires LocalFree after using result
void SHELL::GetSystemErrorMessge(int nError, LPVOID &lpBuf)
{
	FormatMessage(	FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS, 
		NULL,
		nError,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR) &lpBuf, 0, NULL );
}

std::string SHELL::GetSystemErrorMessge(int nError)
{
	char szBuffer[256] = "";
	FormatMessage(	FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS, 
		NULL,
		nError,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR)szBuffer, 0, NULL );
	
	return std::string(szBuffer);
}

void SHELL::ShowSystemErrorMessge(int nError)
{
	//format and show system error message
	LPVOID lpMsgBuf;
	FormatMessage(	FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS, 
		NULL,
		nError,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR) &lpMsgBuf, 0, NULL );
	::MessageBox( NULL, (LPCTSTR)lpMsgBuf, "Error", MB_OK | MB_ICONINFORMATION);
	LocalFree( lpMsgBuf );
}

LONG SHELL::GetRegKey(HKEY key, LPCTSTR subkey, LPTSTR retdata)
{
    HKEY hkey;
    LONG retval = RegOpenKeyEx(key, subkey, 0, KEY_QUERY_VALUE, &hkey);
	
    if (retval == ERROR_SUCCESS) {
        long datasize = MAX_PATH;
        TCHAR data[MAX_PATH];
        RegQueryValue(hkey, NULL, data, &datasize);
        lstrcpy(retdata,data);
        RegCloseKey(hkey);
    }
	
    return retval;
}

bool SHELL::IsPathDisplayAttr(LPCSTR szPath, DWORD dwAttrTest)
{
	//DWORD dwAttributes = SFGAO_DISPLAYATTRMASK;
	//VERIFY(PIDL::GetAttributes(szPath, dwAttributes));
	
	LPITEMIDLIST pidlRel = NULL;
	DWORD dwAttributes = SFGAO_DISPLAYATTRMASK;
	
	bool bRes = false;
	
	std::string strParent = PathName::GetParentDirPath(szPath);
	std::string strPathRel = PathName::GetBaseName(szPath);
	if(0 == strPathRel.size())
		return FALSE;
	
	LPITEMIDLIST pRootPidl = PIDL::GetFromPath(strParent.c_str());
	if(NULL == pRootPidl)
		return false;
	
	// get the shell folder for the pidl. we need this for getting the relative pidls for all the files.
	LPSHELLFOLDER pParent = PIDL::GetFolderPtr(pRootPidl);
	if(pParent == NULL)
		goto cleanup;
	
	//NOTE: use path relative to parent directory
	pidlRel = PIDL::GetFromParentFolder(pParent, strPathRel.c_str());
	if(NULL == pidlRel)
		goto cleanup;
	
	// get some attributes
	pParent->GetAttributesOf(1, (LPCITEMIDLIST *)&pidlRel, &dwAttributes);
	bRes = (dwAttrTest == (dwAttributes & dwAttrTest)); 
	
cleanup:
	
	//clean all data, delete PIDLS
	if(NULL != pParent)
		pParent->Release();
	
	if(NULL != pRootPidl)
		PIDL::Free(pRootPidl);
	
	if(NULL != pidlRel)
		PIDL::Free(pidlRel);
	
	return bRes;
}

STDMETHODIMP SHPathToPidlEx(LPCTSTR szPath, LPITEMIDLIST* ppidl, LPSHELLFOLDER pFolder)
{
	OLECHAR wszPath[MAX_PATH] = {0};
	ULONG nCharsParsed = 0;
	LPSHELLFOLDER pShellFolder = NULL;
	BOOL bFreeOnExit = FALSE;
#ifdef UNICODE
	lstrcpy(wszPath,szPath);
#else
	MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, szPath, -1, wszPath, MAX_PATH);
#endif
	// Use the desktop's IShellFolder by default
	if(pFolder == NULL)
	{
		SHGetDesktopFolder(&pShellFolder);
		bFreeOnExit = TRUE;
	}
	else
		pShellFolder = pFolder;
	
	HRESULT hr = pShellFolder->ParseDisplayName(NULL, NULL, wszPath, &nCharsParsed, ppidl, NULL);
	
	if(bFreeOnExit)
		pShellFolder->Release();
	
	return hr;
}

//TOFIX da li app uvijek mora biti unutar working dira (bolji interface funkcije)
bool SHELL::Execute(LPCSTR szFile, LPCSTR szArgs, LPCSTR szDir, HANDLE *hProcess)
{
	if(hProcess)
		*hProcess = NULL;
	
	String strPath(szDir);
	PathName::EnsureTerminated(strPath, '\\');
	strPath += szFile;
	//PathQuoteSpaces()
	//ASSERT(0 == _access(strPath, 0));
	
	//determine what verb to use
	char szVerb[MAX_PATH]="";
	char szCmdLine[MAX_PATH]="";
	
	{
		if(!GetDefaultVerb(PathName::GetExt(szFile), szVerb, szCmdLine))
		{
			//Some cases (".avi" on XP) do not work without this set explicitly!!
			strcpy(szVerb, "open");
		}
	}
	
	SHELLEXECUTEINFO si;
	ZeroMemory(&si,sizeof(si));
	si.cbSize		= sizeof(si);
	si.lpVerb		= szVerb;
	si.nShow		= SW_SHOW;
	si.lpParameters	= szArgs;
	si.lpDirectory	= szDir;
	si.fMask		= SEE_MASK_FLAG_NO_UI|SEE_MASK_FLAG_DDEWAIT|SEE_MASK_NOCLOSEPROCESS;
	//    si.hwnd			= AfxGetMainWnd()->m_hWnd; //TOFIX
	
	//FIX use pidl as primary execute way instead of path to prevent bug
	//	  with files with name like "ftp.htm", "ftp.dat" where system
	//	  tries to open some "fantom" ftp site
	HINSTANCE result = 0;
	LPITEMIDLIST pidl = PIDL::GetFromPath(strPath.c_str());
	if(NULL != pidl)
	{
		si.lpIDList	= (LPVOID)pidl;
		si.fMask   |= SEE_MASK_INVOKEIDLIST;
		ShellExecuteEx(&si);
		PIDL::Free(pidl);
		
		result = si.hInstApp;
	}
	
	//if PIDL version fails revert to path based version
	//PIDL can fail:
	// 1. szFile is URL
	// 2. on WinXP failed with .avi file on CDROM drive!
	if ((UINT)result <= HINSTANCE_ERROR){
		si.lpIDList	= NULL;
		si.fMask   &= ~SEE_MASK_INVOKEIDLIST;
		si.lpFile = szFile;
		ShellExecuteEx(&si);
		result = si.hInstApp;
	}
	
	//TOFIX pojednostavniti!!!
    // If it failed, get the .htm regkey and lookup the program
    if ((UINT)result > HINSTANCE_ERROR){
		if(hProcess)
			*hProcess = si.hProcess;
		//else
		//	::CloseHandle(si.hProcess);
		return true;
	}
	
	if( SE_ERR_NOASSOC			!= (UINT)result &&
		SE_ERR_ASSOCINCOMPLETE	!= (UINT)result )
	{
		MessageBox(NULL, SHELL::GetShellError((int)result).c_str(), "Error", MB_OK);
		return false;
	}
	
	//use our own version of the FindExecutable() ???
	char szRes[MAX_PATH];
	if(FindExecutable(szFile, szDir, szRes))
	{
		char szAppPath[MAX_PATH];
        lstrcpyn(szAppPath, szDir, MAX_PATH);
		lstrcat(szAppPath, szFile);
		//TOFIX if szRes == szPath append params later
		PathQuoteSpaces(szAppPath);
		
		//TOFIX ShellExecute?
		lstrcat(szRes, " ");
		lstrcat(szRes, szAppPath);
		HINSTANCE result = (HINSTANCE) WinExec(szRes, SW_SHOW);
		if ((UINT)result > HINSTANCE_ERROR)
			return true;
		else{
			MessageBox(NULL, SHELL::GetShellError((int)result).c_str(), "Error", MB_OK);
			return false;
		}
		/*
		PathRemoveBlanks(szRes);
		PathUnquoteSpaces(szRes);
		
		  SHELLEXECUTEINFO si;
		  ZeroMemory(&si,sizeof(si));
		  si.cbSize		= sizeof(si);
		  si.lpVerb		= "open";
		  si.nShow		= SW_SHOW;
		  si.lpFile		= szRes;
		  si.lpParameters	= szApp;	//TOFIX addition for exe
		  si.lpDirectory	= szDir;
		  si.fMask		= SEE_MASK_FLAG_NO_UI|SEE_MASK_FLAG_DDEWAIT;
		  ShellExecuteEx(&si);
		  
			HINSTANCE result = si.hInstApp;
			if ((UINT)result > HINSTANCE_ERROR)
			return true;
		*/
	}
	
	//at this point there were no association and we failed to find it manually
	SHELL::OpenWith(szFile, szArgs, szDir);
	return true;	//fake true to differ from other errors ?? TOFIX???
}

//TOFIX use szArgs?
bool SHELL::OpenWith(LPCSTR szApp, LPCSTR szArgs, LPCSTR szDir)
{
	//our last chance is to start "Open With" shell dialog	
	std::string strParams;
	strParams = "shell32.dll,OpenAs_RunDLL ";
	strParams += szApp;
	
	HINSTANCE hInstance = 
		ShellExecute (NULL, "open", "rundll32.exe", strParams.c_str(), szDir, 1/*TOFIX CMD_SHOW?*/);
	
	if(hInstance <= (HINSTANCE)32){
		SHELL::ShowSystemErrorMessge((int)hInstance);	//format and show system error message
		return false;
	}
	
	return true;
}

std::string SHELL::GetShellError(int nError)
{
	std::string strRes;
	
	switch(nError){
	case 0: 
		strRes = "The operating system is out of memory or resources."; 
		break;
	case ERROR_FILE_NOT_FOUND:
		strRes = "File not found.";
		break; 
	case ERROR_PATH_NOT_FOUND:
		strRes = "Path not found."; 
		break; 
	case ERROR_BAD_FORMAT:
		strRes = "The .exe file is invalid."; 
		break; 
	case SE_ERR_ACCESSDENIED:
		strRes = "File access denied."; 
		break;
	case SE_ERR_ASSOCINCOMPLETE:
		strRes = "Incomplete file name association."; 
		break;
	case SE_ERR_DDEBUSY:
		strRes = "The DDE busy."; 
		break;
	case SE_ERR_DDEFAIL:
		strRes = "The DDE transaction failed.";
		break;
	case SE_ERR_DDETIMEOUT: 
		strRes = "The DDE request timed out.";
		break;
	case SE_ERR_DLLNOTFOUND:
		strRes = "The specified DLL was not found.";
		break;
		//	case SE_ERR_FNF:
		//		strRes = "The specified file was not found.";
		//		break;
	case SE_ERR_NOASSOC:
		strRes = "There is no application associated with the given file name extension.";
		break;
	case SE_ERR_OOM: 
		strRes = "There was not enough memory to complete the operation.";
		break;
		//	case SE_ERR_PNF:
		//		strRes = "The specified path was not found.";
		//		break;
	case SE_ERR_SHARE: 
		strRes = "A sharing violation occurred.";
		break; 
	default:
		strRes = "Unknown error";
	}
	
	return strRes;
}

//TOFIX move somewhere
//===============================================================================
//   IsWinNT - Returns true if we're running Windows NT.
//===============================================================================
BOOL SHELL::IsWinNT()
{
	// values:	-1 -> not checked
	//			 0 -> not NT
	//			 1 -> NT
	static int nIsNT = -1;
	
	if(-1 == nIsNT)
	{
		OSVERSIONINFO osvi;
		osvi.dwOSVersionInfoSize = sizeof(osvi);
		if(GetVersionEx(&osvi))
			if(VER_PLATFORM_WIN32_NT == osvi.dwPlatformId)
				nIsNT = 1;
			else
				nIsNT = 0;
	}
	
	return (1 == nIsNT);
}

//TOFIX return hRes ???? 
//	char szRes[MAX_PATH];
bool SHELL::FindExecutable(const char *szFile, const char *szDir, char *szRes)
{
	HINSTANCE hRes = ::FindExecutable(szFile, szDir, szRes);
    if ((UINT)hRes > HINSTANCE_ERROR)
		return true;
	
	TCHAR szVerb[MAX_PATH];
	TCHAR szCmdLine[MAX_PATH];
	
	if(GetDefaultVerb(PathName::GetExt(szFile), szVerb, szCmdLine))
	{
		//use returned command line string to extract application path name
		//TOFIX strip also parameters like "%L"
        TCHAR *pos;
        pos = strstr(szCmdLine, "\"%1\"");
        if (pos == NULL) {                     // No quotes found
            pos = strstr(szCmdLine, "%1");       // Check for %1, without quotes
            if (pos == NULL)                   // No parameter at all...
                pos = szCmdLine+lstrlen(szCmdLine)-1;
            else
                *pos = '\0';                   // Remove the parameter
        }
        else
            *pos = '\0';                       // Remove the parameter
		
		lstrcpyn(szRes, szCmdLine, MAX_PATH);
		return true;
	}
	
	return false;
}

bool SHELL::FindExecutableKey(LPCSTR szExt, char *szBuffer)
{
    if (GetRegKey(HKEY_CLASSES_ROOT, szExt, szBuffer) == ERROR_SUCCESS)
		return true;
	
	return false;
}

bool SHELL::KeyExists(HKEY key, LPCTSTR subkey)
{
    HKEY hkey;
    LONG retval = RegOpenKeyEx(key, subkey, 0, KEY_QUERY_VALUE, &hkey);
    if (retval == ERROR_SUCCESS) {
        RegCloseKey(hkey);
		return true;
    }
	return false;
}

bool SHELL::GetDefaultVerb(LPCSTR szExt, char *szVerb, char *szCmdLine)
{
	szVerb[0] = '\0';
	
	//File extension string equals to the registry key name.
	//Value of this key points to another registry key describing shell actions 
	//for given file format (described by file extension string)
	char szKey[MAX_PATH]="";
	if(FindExecutableKey(szExt, szKey))
	{
		char szKey2[MAX_PATH]="";
        strcpy(szKey2, szKey);	//TOFIX lstrcpyn
		lstrcat(szKey2, "\\shell");
		
		//See if "shell" subkey has default value defined 
		//(default shell action verb for this format)
		if (GetRegKey(HKEY_CLASSES_ROOT, szKey2, szVerb) == ERROR_SUCCESS &&
			strlen(szVerb)>0)
		{
			//test if the verb is valid (must have valid "command" subkey)
			lstrcat(szKey2, "\\");
			lstrcat(szKey2, szVerb);
			lstrcat(szKey2, "\\command");
			
			//default value of "\\shell\\VERB\\command" subkey (replace VERB with actual value) 
			//gives us command line string (app + parameters)
			if(GetRegKey(HKEY_CLASSES_ROOT, szKey2, szCmdLine) == ERROR_SUCCESS)
				return true;
		}
		else	//no default verb defined
		{
			//test for presence of standard "open" subkey
			strcpy(szKey2, szKey);	//TOFIX lstrcpyn
			lstrcat(szKey2, "\\shell\\open\\command");
			if (GetRegKey(HKEY_CLASSES_ROOT, szKey2, szCmdLine) == ERROR_SUCCESS)
			{
				lstrcpy(szVerb, "open");
				return true;
			}
			
			//else (last chance to find default verb) 
			//take first available subkey under "shell"
			strcpy(szKey2, szKey);	//TOFIX lstrcpyn
			lstrcat(szKey2, "\\shell");
			
			//enumerate subkeys until we find first subkey name
			HKEY hkey;
			LONG retval = RegOpenKeyEx(HKEY_CLASSES_ROOT, szKey2, 0, KEY_ENUMERATE_SUB_KEYS, &hkey);
			if (retval == ERROR_SUCCESS)
			{
				DWORD datasize = MAX_PATH;
				retval = RegEnumKeyEx(hkey, 0, szVerb, &datasize, NULL, NULL, NULL, NULL);
				RegCloseKey(hkey);
				if (retval == ERROR_SUCCESS) {
					
					//test if the verb is valid (must have valid "command" subkey)
					lstrcat(szKey2, "\\");
					lstrcat(szKey2, szVerb);
					lstrcat(szKey2, "\\command");
					
					//default value of "\\shell\\VERB\\command" subkey (replace VERB with actual value) 
					//gives us command line string (app + parameters)
					if(GetRegKey(HKEY_CLASSES_ROOT, szKey2, szCmdLine) == ERROR_SUCCESS)
						return true;
				}
			}
		}
	}
	
	return false;
}

HRESULT SHELL::CreateShortcut(LPCSTR szTarget, LPCSTR szArgs, LPCSTR szLinkFile, LPCSTR szWorkDir, WORD wHotkey)
{
	HRESULT hres;
	
	//if you want a desktop shortcut use this
	////std::string Desktop= CPath::Path_DesktopDirectory()
	std::string Link = /*Desktop +*/ szLinkFile;
	
	IShellLink* psl;
	hres = CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER, IID_IShellLink, (LPVOID*)&psl);
	if (SUCCEEDED(hres))
	{
		if(szTarget)
			psl->SetPath(szTarget);
		if(szArgs)
			psl->SetArguments(szArgs);
		if(szWorkDir)
			psl->SetWorkingDirectory(szWorkDir);
		psl->SetHotkey(wHotkey);
		//psl->SetDescription(strDesc);
		
		IPersistFile* ppf;	
		hres = psl->QueryInterface( IID_IPersistFile, (LPVOID *) &ppf);
		
		if (SUCCEEDED(hres))
		{
			//ensure that link file has proper extension
			String Temp = Link.c_str();
			Temp.Lower();
			if (Temp.find(".lnk")==-1)
				Link += ".lnk";  // Important !!!
			
			WORD wsz[MAX_PATH];
			MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Link.c_str(), -1, (LPWSTR)wsz,MAX_PATH);
			
			//save OLE link object to link file
			hres = ppf->Save((LPOLESTR)wsz, TRUE);
			
			ppf->Release();
		}
		psl->Release();
	}
	
	return hres;
}

HRESULT SHELL::GetShortcutData(LPCSTR szLinkFile, std::string &Target, std::string &Args, std::string &WorkDir, WORD &wHotkey)
{
	IShellLink* psl;
	HRESULT hres = CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER, IID_IShellLink, (LPVOID*)&psl);
	if (SUCCEEDED(hres))
	{
		IPersistFile* ppf;
		hres = psl->QueryInterface( IID_IPersistFile, (LPVOID *) &ppf);
		
		if (SUCCEEDED(hres))
		{
			WORD wsz[MAX_PATH];
			MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, szLinkFile, -1, (LPWSTR)wsz, MAX_PATH);
			
			//load link file into the OLE link object
			hres = ppf->Load((LPOLESTR)wsz, STGM_READ);
			if (SUCCEEDED(hres))
			{
				char szBuffer[MAX_PATH] = "";
				
				WIN32_FIND_DATA fd;
				ZeroMemory(&fd, sizeof(fd));
				strcpy(fd.cFileName, szLinkFile);
				
				psl->GetPath(szBuffer, MAX_PATH, &fd, 0);
				Target = szBuffer;	szBuffer[0] = '\0';
				
				psl->GetArguments(szBuffer, MAX_PATH);
				Args = szBuffer;	szBuffer[0] = '\0';
				
				psl->GetWorkingDirectory(szBuffer, MAX_PATH);
				WorkDir = szBuffer;	szBuffer[0] = '\0';
				
				psl->GetHotkey(&wHotkey);
			}
			
			ppf->Release();
		}
		psl->Release();
	}
	
	return hres;
}

LRESULT CALLBACK CtxMenuHookWndProc(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	UINT uItem;
	TCHAR szBuf[MAX_PATH];
	
	switch (msg) {
	case WM_MENUCHAR:	// only supported by IContextMenu3
		if (g_pIContext3)
		{
			LRESULT lResult = 0;
			g_pIContext3->HandleMenuMsg2 (msg, wp, lp, &lResult);
			return (lResult);
		}
		break;
		
	case WM_DRAWITEM:
	case WM_MEASUREITEM:
		if(wp) break; // not menu related
	case WM_INITMENUPOPUP:
		{
			LRESULT lResult = 0;
			if (g_pIContext2)
				g_pIContext2->HandleMenuMsg(msg, wp, lp);
			else
				g_pIContext3->HandleMenuMsg2(msg, wp, lp, &lResult);
		}
		return (msg==WM_INITMENUPOPUP ? 0 : TRUE); // handled
		
	case WM_MENUSELECT:
		// if this is a shell item, get it's descriptive text
		uItem = (UINT) LOWORD(wp);   
		if(0 == (MF_POPUP & HIWORD(wp)) && 
            uItem >= MIN_SHELL_ID && uItem <=  MAX_SHELL_ID) 
		{
			if (g_pIContext2)
				g_pIContext2->GetCommandString(uItem-MIN_SHELL_ID, GCS_HELPTEXT,
				NULL, szBuf, sizeof(szBuf)/sizeof(szBuf[0]) );
			else
				g_pIContext3->GetCommandString(uItem-MIN_SHELL_ID, GCS_HELPTEXT,
				NULL, szBuf, sizeof(szBuf)/sizeof(szBuf[0]) );
			
			//TOFIX set the status bar text
			//((CFrameWnd*)(AfxGetApp()->m_pMainWnd))->SetMessageText(szBuf);
			return 0;
		}
		break;
		
	default:
		break;
	}
	
	// for all untreated messages, call the original wndproc
	return ::CallWindowProc(g_pOldWndProc, hWnd, msg, wp, lp);
}

HRESULT GetSHContextMenu(LPSHELLFOLDER psfFolder, LPCITEMIDLIST *localPidls, int nCount,
                         void** ppCM, int* pcmType)
{
	*ppCM = NULL;
	LPCONTEXTMENU pICv1 = NULL; // plain version
	// try to obtain the lowest possible IContextMenu
	HRESULT hr = psfFolder->GetUIObjectOf(NULL, nCount, localPidls, 
		IID_IContextMenu, NULL, (void**)&pICv1);
	if(pICv1) { // try to obtain a higher level pointer, first 3 then 2
		hr = pICv1->QueryInterface(IID_IContextMenu3, ppCM);
		if(NOERROR == hr) *pcmType = 3;
		else {
			hr = pICv1->QueryInterface(IID_IContextMenu2, ppCM);
			if(NOERROR == hr) *pcmType = 2;
		}
		
		if(*ppCM) pICv1->Release(); // free initial "v1.0" interface
		else { // no higher version supported
			*pcmType = 1;
			*ppCM = pICv1;
			hr = NOERROR; // never mind the query failures, this'll do
		}
	}
	
	return hr;
}

/*
 #define LD_USEDESC     0x00000001
 #define LD_USEARGS     0x00000002
 #define LD_USEICON     0x00000004
 #define LD_USEWORKDIR  0x00000008
 #define LD_USEHOTKEY   0x00000010
 #define LD_USESHOWCMD  0x00000020
 
 typedef struct {  
	// Mandatory members  
   LPTSTR pszPathname; // Pathname of original object  
   DWORD fdwFlags;     // LD_* flags ORed together for optional members  
   
	 // Optional members  
   LPTSTR pszDesc;     // Description of link file (its filename)  
   LPTSTR pszArgs;     // command-line arguments  
   LPTSTR pszIconPath; // Pathname of file containing the icon  
   int  nIconIndex;    // Index of icon in pszIconPath  
   LPTSTR pszWorkingDir;// Working directory when process starts  
   int  nShowCmd;      // How to show the initial window  
   WORD  wHotkey;      // Hot key for the link
 } LINKDATA, *PLINKDATA;


 HRESULT WINAPI Shell_CreateLink (LPCTSTR pszLinkFilePathname, PLINKDATA pld) 
 { 
	HRESULT hres;
	IShellLink* psl;
	IPersistFile* ppf;
	
	  hres = CoInitialize(NULL);  // Create a shell link object
	  hres = CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
							IID_IShellLink, (PVOID *) &psl);
	  
		if (SUCCEEDED(hres)) {   // Initialize the shell link object  
		psl->SetPath(pld->pszPathname);   
		if (pld->fdwFlags & LD_USEARGS)      
			psl->SetArguments(pld->pszArgs);   
		if (pld->fdwFlags & LD_USEDESC)      
			psl->SetDescription(pld->pszDesc);   
		if (pld->fdwFlags & LD_USEICON)      
			psl->SetIconLocation(pld->pszIconPath, pld->nIconIndex);
		if (pld->fdwFlags & LD_USEWORKDIR)      
			psl->SetWorkingDirectory(pld->pszWorkingDir);   
		if (pld->fdwFlags & LD_USESHOWCMD)      
			psl->SetShowCmd(pld->nShowCmd);   
		if (pld->fdwFlags & LD_USEHOTKEY)      
			psl->SetHotkey(pld->wHotkey);   
			
		// Save the shell link object on the disk   
		hres = psl->QueryInterface(IID_IPersistFile, (PVOID *) &ppf);
		if (SUCCEEDED(hres)) {
		#ifndef UNICODE     // Convert the ANSI path to a Unicode path
			WCHAR szPath[_MAX_PATH] = { 0 };     
			MultiByteToWideChar(CP_ACP, 0, pszLinkFilePathname,
			strlen(pszLinkFilePathname), szPath, adgARRAY_SIZE(szPath));
			hres = ppf->Save(szPath, TRUE);
		#else     
			hres = ppf->Save(pszLinkFilePathname, TRUE);
		#endif     
		
			ppf->Release();
		}
	
		psl->Release();  
	}
	CoUninitialize();
	return(hres);
 
   }
*/
