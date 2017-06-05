////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsManager handles Vfs objects in the system
////////////////////////////////////////////////////////////////////////////

#ifndef VFSMANAGER_H_
#define VFSMANAGER_H_

#if _MSC_VER > 1000
 #pragma warning (disable : 4786)
#endif

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "FileListController.h"
#include "ConnectionInfoList.h"
#include "Vfs.h"
#include <map>

class Vfs_Archive;

class VfsManager
{
public:
	VfsManager();
	virtual ~VfsManager();

	void InitList(FileListController *pList, Vfs *pVfs = NULL);
	int  CheckForSubVfs(FileListController *pList, Vfs *pVfs, VfsItem &item, bool canReadFile = false);
	bool CheckForUpVfs(FileListController *pList, Vfs *pVfs);
	void VfsStackPush(FileListController *pList, Vfs *pVfs);
	bool VfsStackPop(FileListController *pList);
	void VfsStackClean(FileListController *pList);
	int  VfsStackSize(FileListController *pList);
	void VfsStackKillTop(FileListController *pList);    //pop+destroy
	void SwapVfsStacks(FileListController *pList1, FileListController *pList2);

	String GetPathTitle(FileListController *pList);
	void ClearVfsList();

	//remember per-drive last used directory
	void DriveHistoryAdd(const String& strPath);
	String DriveHistoryGet(const String& strPath);

	// remote Vfs list
	Vfs *RemoteVfsAlloc(CNodeInfo &info);
	unsigned int RemoteVfsCount();
	void RemoteVfsAdd(Vfs *pVfs);
	void RemoteVfsRemove(Vfs *pVfs);
	void VfsRemotePush(FileListController *pList, Vfs *pVfs);
	Vfs_Archive *FindVfsArchive(const String& strArchivePath);

	typedef std::vector<Vfs *> tVfsStack;
	tVfsStack m_lstVfsRemote; //remote-connection Vfs objects 

protected:
	tVfsStack                 m_lstVfs;       //all Vfs objects
	std::vector<tVfsStack>    m_lstStacks;    //one stack per each ListCtrl
	std::map<void *, int>     m_mapStackIdx;
	std::map<String, String>  m_mapDriveHistory; //stores last used dir for each local drive
};

#endif // VFSMANAGER_H_


