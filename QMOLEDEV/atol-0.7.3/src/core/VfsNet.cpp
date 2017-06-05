////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Vfs_Net is a VFS implementation for LAN shares browsing
////////////////////////////////////////////////////////////////////////////

#include "VfsNet.h"

Vfs_Net::Vfs_Net()
{
	m_nType = NET;
}

Vfs_Net::~Vfs_Net()
{
}

bool Vfs_Net::ListDir(VfsListing &list, bool &bAbort)
{
#ifdef _WIN32
	bool bRes = m_objBrowser.ListDir(list,bAbort);
	m_strCurDir = m_objBrowser.m_strCurDir;
	return bRes;
#else
	return false;
#endif
}

bool Vfs_Net::SetDir(const char *szPath)
{
#ifdef _WIN32
	bool bRes = m_objBrowser.SetDir(szPath);
	m_strCurDir = m_objBrowser.m_strCurDir;
	return bRes;
#else
	return false;
#endif
}

bool Vfs_Net::Execute(const char *szItem, bool bLocalDir)
{
#ifdef _WIN32
	int nRes = m_objBrowser.Execute(szItem, bLocalDir);
	if(-1 == nRes)
		return Vfs_Local::Execute(szItem);
	else
		return nRes > 0;
#else
	return false;
#endif
}

bool Vfs_Net::SetRootDir()
{
#ifdef _WIN32
	bool bRes = m_objBrowser.SetRootDir();
	m_strCurDir = m_objBrowser.m_strCurDir;
	return bRes;
#else
	return false;
#endif
}

String  Vfs_Net::GetPathTitle()
{
#ifdef _WIN32
	return m_objBrowser.GetPathTitle();
#else
	return String();
#endif
}

bool Vfs_Net::IsRootDir()
{
#ifdef _WIN32
	return m_objBrowser.IsRootDir();
#else
	return true;
#endif
}

void Vfs_Net::ShowCtxMenu(VfsSelection &items, int x, int y)
{
#ifdef _WIN32
	m_objBrowser.ShowCtxMenu(items, x, y);
#endif
}

INT64 Vfs_Net::GetDriveFreeSpace()
{
	//TOFIX use pidl for path?, GetRealDiskFreeSpace()
	return 1000000000; //TOFIX
}

bool Vfs_Net::UpDir()
{
#ifdef _WIN32
	return m_objBrowser.UpDir();
#else
	return false;
#endif
}

