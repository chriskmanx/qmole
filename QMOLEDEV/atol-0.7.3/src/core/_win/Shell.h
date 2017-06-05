////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef __SHELL_H
#define __SHELL_H

#pragma warning(disable: 4786)

#include "pidl.h"
#include <vector>

//NOTE: most of this functions require OLE libraries to be initialised
//		(using AfxOleInit() call)

namespace SHELL
{
	BOOL TrackItemIDContextMenu(LPCTSTR pszPath, UINT nFlags, LPPOINT ptPoint, HWND hWnd);
	BOOL TrackItemIDContextMenu(LPITEMIDLIST pidl,	UINT nFlags, LPPOINT ptPoint, HWND hWnd);
	HRESULT SHBindToParent(LPITEMIDLIST pidl, REFIID riid, VOID **ppv, LPCITEMIDLIST *ppidlLast);
	static LRESULT CALLBACK _CtxMenuWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	LPITEMIDLIST CopyItemID(LPITEMIDLIST pidl, int cb =-1);

	void ShowSystemErrorMessge(int nError);
	void GetSystemErrorMessge(int nError, LPVOID &lpBuf);

	void WinExecute(LPCSTR szPath);
	LONG GetRegKey(HKEY key, LPCTSTR subkey, LPTSTR retdata);
	BOOL DefaultAction(LPCSTR szPath);

	BOOL MyContextMenu(LPCSTR szParentDir, std::vector<std::string> &lstItems, UINT nFlags, int x, int y, HWND hWnd);
	BOOL MyContextMenu(LPITEMIDLIST pidlParent, std::vector<std::string> &lstItems, UINT nFlags, int x, int y, HWND hWnd);

	bool IsPathDisplayAttr(LPCSTR szPath, DWORD dwAttrTest);

	bool Execute(LPCSTR szFile, LPCSTR szArgs, LPCSTR szDir, HANDLE *hProcess = NULL);
	bool OpenWith(LPCSTR szApp, LPCSTR szArgs, LPCSTR szDir);
	std::string GetShellError(int nError);
	std::string GetSystemErrorMessge(int nError);

	bool FindExecutable(const char *szFile, const char *szDir, char *szRes);
	bool FindExecutableKey(LPCSTR szExt, char *szBuffer);
	bool KeyExists(HKEY key, LPCTSTR subkey);
	bool GetDefaultVerb(LPCSTR szExt, char *szVerb, char *szCmdLine);

	HRESULT CreateShortcut(LPCSTR szTarget, LPCSTR szArgs, LPCSTR szLinkFile, LPCSTR szWorkDir, WORD wHotkey);
	HRESULT GetShortcutData(LPCSTR szLinkFile, std::string &Target, std::string &Args, std::string &WorkDir, WORD &wHotkey);

	void Test();

	BOOL IsWinNT();
	BOOL IsWinXP();
};

#endif  // __SHELL_H
