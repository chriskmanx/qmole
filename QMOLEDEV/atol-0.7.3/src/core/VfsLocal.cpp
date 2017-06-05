////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Vfs_Local class implementation
////////////////////////////////////////////////////////////////////////////

#include <sys/types.h> 
#include <sys/stat.h>
#include <fcntl.h>
#include <glib.h>
#include <glib/gstdio.h>

#ifdef _WIN32
 #include <io.h>
 #include <direct.h> //mkdir
 #define access _access
 #define mkdir _mkdir
 #include "_win/shell.h"
 #include <gdk/gdkwin32.h>
 #include "_win/ShellContextMenu.h"
#else
 #include <unistd.h> //readlink
 #include <utime.h> //utime
#endif

#include "../ThreadSafeMsgs.h"
#include "VfsLocal.h"
#include "System.h"
#include "PathName.h"
#include "opcodes.h"
#include "debug.h"
#include "../DualPanel.h"
#include "../MainWindow.h"
#include "../callbacks.h"
#include "../support.h"
#include "util.h"

extern DualPanel g_dp;
extern MainWindow g_wnd;

static bool FileCopy(const char *szSrc, const char *szDest, OpState *pProgress, INT64 nOffset = 0);
static bool OnEnumFile(const std::string& strNativePath, void* data);
static VfsItem GetLinkTargetHelper(const VfsItem& item, const char *pCurDir);
#ifdef _WIN32
static gboolean DirChangeEvent_CumulateTimer(gpointer data);
#endif

void OnDirChangeEvent(long lData);

Vfs_Local::Vfs_Local():
	m_bRecycleBin(false)
{
	m_nType = LOCAL;
	m_pProgress = NULL;
}

Vfs_Local::~Vfs_Local()
{
}

Vfs *Vfs_Local::Clone()
{
	Vfs_Local *pVfs = new Vfs_Local;
	pVfs->SetDir(m_strCurDir);
	return pVfs;
}

void Vfs_Local::FixPath(String &strPath)
{
#ifdef _WIN32
	strPath.Replace("/", "\\");
#else
	strPath.Replace("\\", "/");
#endif
}

bool Vfs_Local::Open()
{
	return true;
}

bool Vfs_Local::Close()
{
	return true;
}

bool Vfs_Local::SetDir(const char *szPath)
{
	ASSERT(szPath != 0);
	String strPath(szPath);

#ifdef _WIN32
	if(strPath.Length() <= 3)
#else
	if(strPath.Length() < 2)
#endif
		PathName::EnsureTerminated(strPath);
	else
		PathName::EnsureNotTerminated(strPath);

	//check read permission on the path
 #ifdef _WIN32
	if(0 != access(strPath.c_str(), 04))
 #else
	if(0 != access(strPath.c_str(), R_OK))
 #endif
		return false;

	// check if path is a directory
#ifdef _WIN32
	struct _stati64 st;
	if(_stati64(strPath.c_str(), &st) || !(st.st_mode & S_IFDIR))
#else
	struct stat64 st;
	if(lstat64(strPath.c_str(), &st) || !(st.st_mode & (S_IFDIR|S_IFLNK)))
#endif
		return false; 

	m_strCurDir = strPath;
	return true;
}

bool Vfs_Local::SetRootDir()
{
	String strPath = PathName::GetRootDir(m_strCurDir);
	if(SetDir(strPath))
		return true;
	return false;
}

bool Vfs_Local::UpDir()
{
	String strNewDir = PathName::GetParentDirPath(m_strCurDir);
	if(strNewDir != GetDir() && !strNewDir.IsEmpty()) //if path different
	{
		if(SetDir(strNewDir))
			return true;
	}
	return false;
}

bool Vfs_Local::IsRootDir()
{
#ifdef _WIN32
	if(m_strCurDir.Length() <= 3)
		return true;
#else
	if(m_strCurDir.Length() < 2)
		return true;
#endif

	return false;
}

bool Vfs_Local::ListDir(VfsListing &list, bool &bAbort)
{
	list.Clear();

	// get the file and dir lists
	if(ENUM_ERR_OK != System::EnumDirectory(m_strCurDir, OnEnumFile, &list, ENUM_LST_ALL))
		return false;

	// optionally append ".." item
	if (!IsRootDir())
	{
		VfsItem item;
		item.m_strName  = String("..");
		item.m_nAttrib  = ATTR_DIR;
	#ifndef _WIN32
		item.m_nAttrib |= ATTR_UNIX;
	#endif
		list.Insert(item);
	}

	return true;
}

bool Vfs_Local::MkDir(const char *szNameInUTF8)
{
	ASSERT(szNameInUTF8 != 0);
	std::string path(PathName::ComposePath(m_strCurDir, szNameInUTF8));
	return System::MkDir(path.c_str());
}

bool Vfs_Local::Delete(VfsItem &item, int &nOpSettings)
{
	String path = PathName::ComposePath(m_strCurDir, item.GetName());
	FixPath(path);
	std::string pathNative(path);

#ifdef _WIN32
	DWORD dwAttr = GetFileAttributes(pathNative.c_str());
	//TOFIX add FILE_ATTRIBUTE_SYSTEM support
	if((0xFFFFFFFF != dwAttr) && ((dwAttr & FILE_ATTRIBUTE_READONLY) == FILE_ATTRIBUTE_READONLY))
	{
		//if the style was not set, ask the user what to do
		if(!(nOpSettings & OPF_DEL_ALL_RO_FILES))
		{
			nOpSettings |= MsgDelete_ThrSafe(System::FileNameToUTF8(path).c_str());
			if((OPF_ABORT & nOpSettings) || (OPF_SKIP & nOpSettings))
				return false;
		}
	}
#else
	if(0 != laccess(pathNative.c_str(), W_OK))
	{
		//if the style was not set, ask the user what to do
		if(!(nOpSettings & OPF_DEL_ALL_RO_FILES))
		{
			nOpSettings |= MsgDelete_ThrSafe(System::FileNameToUTF8(path).c_str());
			if((OPF_ABORT & nOpSettings) || (OPF_SKIP & nOpSettings))
				return false;
		}
	}
#endif

	if (m_bRecycleBin)
	{
		if (System::MoveToTrash(path))
			return true;
		// TODO: ask user if she wants to remove instead of moving to trash
	}

	//TOFIX add wipe support (global ini settings)
	return System::Remove(path);  //hard delete (if required or recycle failed)
}

bool Vfs_Local::Rename(const char *szItem, const char *szNewItem)
{
	ASSERT(szItem != 0 && szNewItem != 0);
	return System::Rename(szItem, szNewItem);
}

bool Vfs_Local::Copy(VfsItem &item, const char *szNewName, Vfs *pVfsDest, INT64 nOffset)
{
	ASSERT(szNewName != 0);
	if(pVfsDest->GetType() == Vfs::LOCAL)
	{
		String strDest = PathName::ComposePath(
			pVfsDest->GetDir(), szNewName ? szNewName : item.GetName().c_str());
		FixPath(strDest);

		return CopyToLocal(item, strDest.c_str(), nOffset);
	}
	else
	{
		String strFullPath = PathName::ComposePath(m_strCurDir, item.GetName());
		FixPath(strFullPath);

		if(szNewName)
			item.SetName(szNewName);

		return pVfsDest->CopyFromLocal(strFullPath, item, nOffset);
	}

	return false;
}

bool Vfs_Local::Execute(const char *szItem, bool bLocalDir)
{
	ASSERT(szItem != 0);

	String path;
	if (bLocalDir)
	{
		path = m_strCurDir;
		PathName::EnsureTerminated(path, '/');
		FixPath(path);
	}
	path += szItem;

	return System::Open(path);
}

VfsItem GetLinkTargetHelper(const VfsItem& item, const char *pCurDir)
{
	VfsItem finalTarget(item);
	std::string curDir(pCurDir); 

	// try to follow entire link chain until we reach normal file
	for(int i = 0; finalTarget.IsLink(); i++)
	{
		if(i > 32) // avoid ininite loops
			return VfsItem(); // empty item means error

		std::string path(PathName::ComposePath(curDir.c_str(), finalTarget.GetName()));
		std::string targetPath(System::GetLinkTarget(path));
  
		if(!PathName::IsAbsolutePath(targetPath.c_str()))
		{
			String strTmp(curDir.c_str());
			PathName::EnsureTerminated(strTmp);
			strTmp += targetPath.c_str();
			targetPath = PathName::GetCanonicalPath(strTmp.c_str());
		}

		VfsItem target;
		Vfs_Local::MakeItem(targetPath, target);
		target.m_strPath.assign(targetPath);
		finalTarget = target;

		curDir = PathName::GetParentDirPath(targetPath.c_str());
	}
	return finalTarget; 
}

VfsItem Vfs_Local::GetLinkTarget(const VfsItem& item)
{
	return GetLinkTargetHelper(item, m_strCurDir);
}

bool Vfs_Local::CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset)
{
	String strSrc = PathName::ComposePath(m_strCurDir, item.GetName());
	FixPath(strSrc);

	return FileCopy(strSrc, szLocalPath, m_pProgress, nOffset);
}

bool Vfs_Local::CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset)
{
	String strDst = PathName::ComposePath(m_strCurDir, item.GetName());
	FixPath(strDst);

	return FileCopy(szLocalPath, strDst, m_pProgress, nOffset);
}

INT64 Vfs_Local::GetDriveFreeSpace()
{
	INT64 nTotal;
	return System::GetAvailBytesOnFS(m_strCurDir, nTotal);
}

INT64 Vfs_Local::GetDriveSize()
{ 
	INT64 nTotal;
	System::GetAvailBytesOnFS(m_strCurDir, nTotal);
	return nTotal;
}

void Vfs_Local::ShowCtxMenu(VfsSelection &items, int x, int y)
{
	FixPath(m_strCurDir);

#ifdef _WIN32
 #if 1
	std::vector<String> lstFiles;

	String strDir(GetDir().c_str());
	if (strDir.Right(1) != "\\" && strDir.Right(1) != "/")
	  strDir += "\\";
	FixPath(strDir);

	for (int i = items.GetTotalCount() - 1; i >= 0; i--)
	{
		if (!items.m_lstRootItems[i].IsDots())
		{
			String str(strDir);
			str += items.m_lstRootItems[i].GetName().c_str();
			lstFiles.push_back(str);
		}
	}

	if (lstFiles.size() <= 0)
		lstFiles.push_back(GetDir().c_str());

	CShellContextMenu scm;
	scm.SetObjects(lstFiles);

	POINT pt;
	pt.x = x;
	pt.y = y;
	HWND hWnd = (HWND)GDK_WINDOW_HWND(g_dp.GetActiveFileList().m_pWidget->window);
	ClientToScreen(hWnd, &pt);
	scm.ShowContextMenu(hWnd, pt.x, pt.y, 0);

 #else
	std::string strItem;
	std::vector<std::string> lstItems;
	for(unsigned int i=0; i<items.GetRootCount(); i++)
	{
		if(!items.m_lstRootItems[i].IsDots()) //this one can crash app
		{
			strItem = items.m_lstRootItems[i].GetName();
			lstItems.push_back(strItem);
		}
	}

	//get windows handle for the file list
	//GtkWidget *widget = g_dp.GetActiveFileList().m_pWidget;
	HWND hWnd = (HWND)GDK_WINDOW_HWND(widget->window);
	POINT pt;
	pt.x = x; pt.y = y;
	//ClientToScreen(hWnd, &pt); 

	if(lstItems.size()>0)
		SHELL::MyContextMenu(m_strCurDir, lstItems, TPM_LEFTBUTTON|TPM_LEFTALIGN|TPM_TOPALIGN, x, y, hWnd);
	else
		SHELL::TrackItemIDContextMenu(m_strCurDir, 0, &pt, hWnd);
 #endif
#else
	//prepare popup menu
	GtkWidget *menu;
	GtkWidget *menu_item;
	int button, event_time;
	
	menu = gtk_menu_new ();

	menu_item = gtk_menu_item_new_with_mnemonic(_("_Copy"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_copy1_activate), NULL);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 

	menu_item = gtk_menu_item_new_with_mnemonic(_("_Move"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_move1_activate), NULL);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 

	menu_item = gtk_menu_item_new_with_mnemonic(_("_Rename"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_rename1_activate), NULL);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 

	menu_item = gtk_menu_item_new_with_mnemonic(_("_Delete"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_delete1_activate), NULL);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 

	event_time = gtk_get_current_event_time ();
	button = 0;	//FIX: allow mouse button to trigger the submenu
	gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, button, event_time);
#endif
}

void Vfs_Local::ShowProperties(VfsSelection &items)
{
#ifdef _WIN32
	std::vector<String> lstFiles;

	String strDir(GetDir().c_str());
	if (strDir.Right(1) != "\\" && strDir.Right(1) != "/")
		strDir += "\\";
	FixPath(strDir);

	for (int i = items.GetTotalCount() - 1; i >= 0; i--)
	{
		if (!items.m_lstRootItems[i].IsDots())
		{
			String str(strDir);
			str += items.m_lstRootItems[i].GetName().c_str();
			lstFiles.push_back(str);
		}
	}

	if (lstFiles.size() > 0)
	{
		CShellContextMenu scm;
		scm.SetObjects(lstFiles);
		HWND hWnd = (HWND)GDK_WINDOW_HWND(g_dp.GetActiveFileList().m_pWidget->window);
		scm.ShowContextMenu(hWnd, 0, 0, "properties");
	}
#endif
}

void Vfs_Local::ClipboardCut(VfsSelection &items)
{
#ifdef _WIN32
	std::vector<String> lstFiles;

	String strDir(GetDir().c_str());
	if (strDir.Right(1) != "\\" && strDir.Right(1) != "/")
		strDir += "\\";
	FixPath(strDir);

	for (int i = items.GetTotalCount() - 1; i >= 0; i--)
	{
		if (!items.m_lstRootItems[i].IsDots())
		{
			String str(strDir);
			str += items.m_lstRootItems[i].GetName().c_str();
			lstFiles.push_back(str);
		}
	}

	if (lstFiles.size() > 0)
	{
		CShellContextMenu scm;
		scm.SetObjects(lstFiles);
		HWND hWnd = (HWND)GDK_WINDOW_HWND(g_dp.GetActiveFileList().m_pWidget->window);
		scm.ShowContextMenu(hWnd, 0, 0, "cut");
	}
#endif
}

void Vfs_Local::ClipboardCopy(VfsSelection &items)
{
#ifdef _WIN32
	std::vector<String> lstFiles;

	String strDir(GetDir().c_str());
	if (strDir.Right(1) != "\\" && strDir.Right(1) != "/")
		strDir += "\\";
	FixPath(strDir);

	for (int i = items.GetTotalCount() - 1; i >= 0; i--)
	{
		if (!items.m_lstRootItems[i].IsDots())
		{
			String str(strDir);
			 
			str += items.m_lstRootItems[i].GetName().c_str();
			lstFiles.push_back(str);
		}
	}

	if (lstFiles.size() > 0)
	{
		CShellContextMenu scm;
		scm.SetObjects(lstFiles);
		HWND hWnd = (HWND)GDK_WINDOW_HWND(g_dp.GetActiveFileList().m_pWidget->window);
		scm.ShowContextMenu(hWnd, 0, 0, "copy");
	}
#endif
}

void Vfs_Local::ClipboardPaste()
{
#ifdef _WIN32
	std::vector<String> lstFiles;
	lstFiles.push_back(GetDir().c_str());
	CShellContextMenu scm;
	scm.SetObjects(lstFiles);
	HWND hWnd = (HWND)GDK_WINDOW_HWND(g_dp.GetActiveFileList().m_pWidget->window);
	scm.ShowContextMenu(hWnd, 0, 0, "paste");

	g_dp.GetActiveFileList().PanelList();
#endif
}

void Vfs_Local::RegisterChangeNotify(void *list)
{
#ifdef _WIN32
	m_objWatcher.SetNotifyInfo(OnDirChangeEvent, (long)list);
#endif
}

void Vfs_Local::StopNotifications()
{
#ifdef _WIN32
	m_objWatcher.Stop();
#endif
}

void Vfs_Local::StartNotifications()
{
#ifdef _WIN32
	//watch directory for file change notifications
	m_objWatcher.Start(m_strCurDir);
#endif
}

//TOFIX move somewhere else ?
bool FileCopy(const char *szSrc, const char *szDest, OpState *pProgress, INT64 nOffset)
{
	ASSERT(szSrc != 0 && szDest != 0);
#ifdef _WIN32
	std::string srcUtf8(System::FileNameToUTF8(szSrc));
	std::string destUtf8(System::FileNameToUTF8(szDest));
#else
	std::string srcUtf8(szSrc);
	std::string destUtf8(szDest);
#endif
	std::string src(szSrc);
	std::string dest(szDest);

	bool bRes = false;

#define CPY_BUFF_SIZE	32*1024
#ifdef _WIN32
	//test if source file can be read
	if(0 != access(src.c_str(), 04))
		return false;

	int nSrcFile = _open(src.c_str(), _O_BINARY|_O_RDONLY, 0);
	if(nSrcFile > -1)
	{
		//if the destination file exists and is read-only
		if(0 == access(src.c_str(), 00)){
			DWORD dwAttr = GetFileAttributes(dest.c_str());
			if(0xFFFFFFFF != dwAttr && dwAttr & FILE_ATTRIBUTE_READONLY)
			{
				//remove read only flag
				dwAttr &= ~(FILE_ATTRIBUTE_READONLY);
				SetFileAttributes(dest.c_str(), dwAttr);
			}
		}

		//TOFIX _O_RDWR instead of _O_RDONLY for rollback segment checking
		int nDstFile = _open(dest.c_str(), _O_BINARY|_O_CREAT|_O_WRONLY, _S_IREAD);
		if(nDstFile > -1)
		{
			char szBuffer[CPY_BUFF_SIZE];
			int nRead;

			struct _stati64 st;
			_stati64(src.c_str(), &st);
			INT64 uSrcSize = st.st_size;    //__int64
			_stati64(dest.c_str(), &st);
			INT64 uDstSize = st.st_size;

			INT64 nStartProgress = 0;

			//TOFIX implement overlapping resuming for content checking
			if(nOffset>0 && uSrcSize > uDstSize){
				_lseeki64(nSrcFile, (int)uDstSize, SEEK_SET);
				nStartProgress = uDstSize;
			}

			if(NULL != pProgress){
				pProgress->InitCurrentFiles(srcUtf8.c_str(), destUtf8.c_str());
				pProgress->InitCurrentProgress(nStartProgress, uSrcSize);
			}

			//NOTE: nRead might be -1 for error
			while(0 < (nRead = _read(nSrcFile, szBuffer, sizeof(szBuffer))))
			{
				if(NULL != pProgress){
					if(pProgress->IsAborted())
						break;
					pProgress->StepPos(nRead);
				}

				_write(nDstFile, szBuffer, nRead);
			}

			//TOFIX what if user wants to keep partialy copied file ? (option?)
			if(pProgress && pProgress->IsAborted())
			{
				System::Remove(szDest);
			}
			else
			{
				//copy file attributes
				DWORD dwAttrib = GetFileAttributes(src.c_str());
				if(0xFFFFFFFF != dwAttrib)
					SetFileAttributes(dest.c_str(), dwAttrib);

				//before closing the file copy file dates
				FILETIME ftCreated, ftAccessed, ftModified;
				HANDLE hSrc = (HANDLE)_get_osfhandle(nSrcFile);
				HANDLE hDst = (HANDLE)_get_osfhandle(nDstFile);

				if(GetFileTime(hSrc, &ftCreated, &ftAccessed, &ftModified))
					SetFileTime(hDst, &ftCreated, &ftAccessed, &ftModified);
			}

			_close(nDstFile);
			if(nRead >= 0) //check for error
				bRes = true;
		}

		_close(nSrcFile);
	}
#else
	//test if source file can be read
	if(0 != laccess(src.c_str(), R_OK))
		return false;

	struct stat64 st;
	if(0 != lstat64(src.c_str(), &st))
		return false;

	//special case: handle copying link files!!!
	if(S_ISLNK(st.st_mode))
	{
		char szLnkTarget[1024];
		int nBytes = readlink(src.c_str(), szLnkTarget, sizeof(szLnkTarget));
		if(-1 == nBytes)
			return false;
		szLnkTarget[nBytes] = '\0';
		
		//convert target from relative to absolute name
 		//(rel. name is invalid within the context of the new directory)
		String strDst;
		if(szLnkTarget[0] !='/'){
			strDst = PathName::GetParentDirPath(src.c_str());
			PathName::EnsureTerminated(strDst);
		}
		strDst += szLnkTarget;
	
		if(0 != symlink(strDst.c_str(), dest.c_str()))
			return false;
		return true;
	}

	int nSrcFile = open(src.c_str(), O_RDONLY|O_LARGEFILE, 0);
	if(nSrcFile > -1)
	{
		//TOFIX if the destination file exists and is read-only
		//if(0 == access(dest.c_str(), 00)){}

		//TOFIX O_RDWR instead of O_RDONLY for rollback segment checking
		int nDstFile = open(dest.c_str(), O_CREAT|O_WRONLY|O_LARGEFILE, S_IREAD);
		if(nDstFile > -1)
		{
			char szBuffer[CPY_BUFF_SIZE];
			int nRead;

			struct stat64 st;
			stat64(dest.c_str(), &st);
			INT64 uDstSize = st.st_size;
			stat64(src.c_str(), &st);
			INT64 uSrcSize = st.st_size;    //__int64

			INT64 nStartProgress = 0;

			//TOFIX implement overlapping resuming for content checking
			if(nOffset>0 && uSrcSize > uDstSize){
				lseek64(nSrcFile, uDstSize, SEEK_SET);
				nStartProgress = uDstSize;
			}

			if(NULL != pProgress){
				pProgress->InitCurrentFiles(srcUtf8.c_str(), destUtf8.c_str());
				pProgress->InitCurrentProgress(nStartProgress, uSrcSize);
			}

			while(0 < (nRead = read(nSrcFile, szBuffer, sizeof(szBuffer))))
			{
				if(NULL != pProgress){
					if(pProgress->IsAborted())
						break;
					pProgress->StepPos(nRead);
				}

				write(nDstFile, szBuffer, nRead);
			}

			//TOFIX what if user wants to keep partial dst file? (option)
			if(pProgress && pProgress->IsAborted()){
				close(nDstFile);
				System::Remove(szDest);
			}
			else
			{
				//copy file permissions
				fchmod(nDstFile, st.st_mode & 00777);

				//copy last access/modified time
				struct utimbuf ut;
				ut.actime = st.st_atime;
				ut.modtime = st.st_mtime;
				utime(dest.c_str(), &ut);

				close(nDstFile);

				if(nRead >= 0) //check for error
					bRes = true;
			}
		}
		close(nSrcFile);
	}

#endif

	//on operation abort, remove partially copied file
	if( !bRes || (NULL != pProgress && pProgress->IsAborted()) ){
		System::Remove(szDest);
	}

	return bRes;
}

void Vfs_Local::MakeItem(const std::string& strNativePath, VfsItem& item)
{
	//remove path from file name 
	item.m_strName = PathName::GetBaseName(strNativePath.c_str());

#ifdef _WIN32
	struct _stati64 st;
	if(0 == _stati64(strNativePath.c_str(), &st))	//TOFIX lstat
#else
	struct stat64 st;
	if(0 == lstat64(strNativePath.c_str(), &st))	//TOFIX lstat
#endif
	{
		if(st.st_mode & S_IFDIR)
			item.m_nAttrib |= ATTR_DIR;

		//convert attributes to portable flags
#ifdef _WIN32
		if(0 == (st.st_mode & _S_IREAD))
			item.m_nAttrib |= ATTR_RONLY;
		
		DWORD dwAttr = GetFileAttributes(strNativePath.c_str());
		if(dwAttr != 0xFFFFFFFF)
		{
			//TOFIX ATTR_LINK
			if(dwAttr & FILE_ATTRIBUTE_DIRECTORY)   item.m_nAttrib |= ATTR_DIR;
			if(dwAttr & FILE_ATTRIBUTE_ARCHIVE)     item.m_nAttrib |= ATTR_ARCH;
			if(dwAttr & FILE_ATTRIBUTE_HIDDEN)      item.m_nAttrib |= ATTR_HIDDEN;
			if(dwAttr & FILE_ATTRIBUTE_READONLY)    item.m_nAttrib |= ATTR_RONLY;
			if(dwAttr & FILE_ATTRIBUTE_SYSTEM)      item.m_nAttrib |= ATTR_SYSTEM;
		}

		//if file has extension ".lnk" assume that it is a link
		if (strNativePath.size() >= 4 && String(strNativePath.c_str() + strNativePath.size() - 4).CmpNoCase(".lnk") == 0)
			item.m_nAttrib |= ATTR_LINK;
#else
		item.m_nAttrib |= ATTR_UNIX;
		if(st.st_mode & S_IFDIR)    item.m_nAttrib |= ATTR_DIR;
		if(S_ISLNK(st.st_mode))
		{
			item.m_nAttrib |= ATTR_LINK;
			VfsItem linkTarget(GetLinkTargetHelper(item, PathName::GetParentDirPath(strNativePath.c_str())));
			if(linkTarget.IsDir())
				item.m_nAttrib |= ATTR_DIR;
		}

		if(st.st_mode & S_IRUSR)    item.m_nAttrib |= ATTR_R_USR;
		if(st.st_mode & S_IWUSR)    item.m_nAttrib |= ATTR_W_USR;
		if(st.st_mode & S_IXUSR)    item.m_nAttrib |= ATTR_X_USR;
		if(st.st_mode & S_IRGRP)    item.m_nAttrib |= ATTR_R_GRP;
		if(st.st_mode & S_IWGRP)    item.m_nAttrib |= ATTR_W_GRP;
		if(st.st_mode & S_IXGRP)    item.m_nAttrib |= ATTR_X_GRP;
		if(st.st_mode & S_IROTH)    item.m_nAttrib |= ATTR_R_OTH;
		if(st.st_mode & S_IWOTH)    item.m_nAttrib |= ATTR_W_OTH;
		if(st.st_mode & S_IXOTH)    item.m_nAttrib |= ATTR_X_OTH;
#endif

		item.m_nSize         = item.IsDir() ? -1 : st.st_size;
		item.m_nLastModDate  = st.st_mtime;
	}

	item.CalcExt();
}

bool OnEnumFile(const std::string& strNativePath, void* data)
{
	VfsListing* list = static_cast<VfsListing*>(data);
	if (list)
	{
		VfsItem item;
		Vfs_Local::MakeItem(strNativePath, item);
		list->Insert(item);
	}

	return true; // keep enumerating
}

void OnDirChangeEvent(long lData)
{
	//NOTE: this is being called from a Watcher thread
	TRACE("Handler: Directory changed\n");

	// all you need to do is to store current time
	// into the list's last directory change date
	// - special timer will handle actual list refresh
	FileList *pList = (FileList *)lData;
	
	UINT64 nCurTimeMs;
	StopWatch::GetCurTime(nCurTimeMs);
	nCurTimeMs = nCurTimeMs / 10000;	//convert to Ms

	pList->m_nLastChangeEventMs = nCurTimeMs;

	//sleep a little to reduce process CPU usage
	//TOFIX separate function msSleep()
	#ifdef _WIN32
		Sleep(5);	   //5ms
	#else
		usleep(5000);  //5ms
	#endif
}

