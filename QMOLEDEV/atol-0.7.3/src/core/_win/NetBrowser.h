////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef NET_BROWSER_H__
#define NET_BROWSER_H__

#if _MSC_VER > 1000
 #pragma warning(disable: 4786)
#endif

#include <windows.h>
#include <shlobj.h>
#include <map>
#include <string>

#include "../VfsListing.h"
#include "../VfsSelection.h"
#include "../String.h"

class NetBrowser
{
public:
	NetBrowser();
	~NetBrowser();

	bool SetDir(const char *szPath);
	bool UpDir();

	bool ListDir(VfsListing &list, bool &bAbort);
	
	bool SetRootDir();
	bool IsRootDir();
	String GetPathTitle();

	void ShowCtxMenu(VfsSelection &items, int x, int y);
	int  Execute(const char *szItem, bool bLocalDir);

	String m_strCurDir;

protected:
	bool FillItem(VfsItem &item, LPSHELLFOLDER pFolder, LPITEMIDLIST pidlRel);
	bool Populate(LPITEMIDLIST pidlRoot, VfsListing &list);

	void FreePidlMap();
	void RefreshPath();
	

protected:
	VfsListing m_lstData;

	bool m_bGoUp;
	std::map<std::string, LPITEMIDLIST> m_mapItems; 

	LPITEMIDLIST m_pidlNetRoot;	//abs?
	LPITEMIDLIST m_pidlCurRoot; //abs?
};

#endif //NET_BROWSER_H__

