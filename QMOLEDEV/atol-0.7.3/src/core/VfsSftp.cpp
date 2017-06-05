////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "VfsSftp.h"
#include "PathName.h"
#include "ProxyLayer.h"

//TOFIX move this (core dependent on non-core)
#include <gtk/gtk.h>
#include "../support.h"
#include "../callbacks.h"
#include "../ThreadSafeMsgs.h"

//TOFIX single import header
struct sftp_command {
    char **words;
    int nwords, wordssize;
    int (*obey) (CSshSession &session, struct sftp_command *);	/* returns <0 to quit */
};

struct fxp_attrs {
    unsigned long flags;
    uint64 size;
    unsigned long uid;
    unsigned long gid;
    unsigned long permissions;
    unsigned long atime;
    unsigned long mtime;
};

struct fxp_handle {
    char *hstring;
    int hlen;
};

struct fxp_name {
    char *filename, *longname;
    struct fxp_attrs attrs;
};

struct fxp_names {
    int nnames;
    struct fxp_name *names;
};

extern void fxp_free_names(struct fxp_names *names);
extern void fxp_free_name(struct fxp_name *name);
extern void sftp_progress_fn(CSshSession &session, int (* m_pProgress)(INT64, INT64, unsigned long), unsigned long data);
extern bool sftp_connected(CSshSession &session);
extern int sftp_debug_fn(CSshSession &session, void (*pfn)(const char *, unsigned long), unsigned long data);
extern int sftp_main(CSshSession &session, char *host, int port, char *user, const char *password);
extern int sftp_cmd_quit(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_ls(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_cd(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_rmdir(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_rm(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_put(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_get(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_mkdir(CSshSession &session, struct sftp_command *cmd);
extern int sftp_cmd_mv(CSshSession &session, struct sftp_command *cmd);
extern const char *fxp_error(void);

void DebugFn(const char *szText, unsigned long data);
int ProgressWrapper(INT64 pos, INT64 size, unsigned long data);
int Password(unsigned long data);
int HostKeyWarning(const char *message, int type, unsigned long data);
int Error(const char *message, unsigned long data);

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Vfs_Sftp::Vfs_Sftp()
{
	m_nType	= SFTP;
	m_nPort	= 0;
	m_pfnTrace = NULL;
}

Vfs_Sftp::~Vfs_Sftp()
{
}

void Vfs_Sftp::FixPath(String &strPath)
{
	strPath.Replace("\\", "/");
}

bool Vfs_Sftp::Open()
{
#ifdef _WIN32
	//TOFIX SetLoginCallback
	m_objSession.m_fnLogin = Password;
	m_objSession.m_fnLoginData = (unsigned long)this;
	m_objSession.m_fnKeyWarning = HostKeyWarning;
	m_objSession.m_fnKeyData = (unsigned long)this;
	m_objSession.m_fnError	= Error;
	m_objSession.m_fnErrData = (unsigned long)this;

	sftp_debug_fn(m_objSession, DebugFn, (unsigned long)this);

	//actual connection
	sftp_main(m_objSession, (char *)m_szHost.c_str(), m_nPort, (char *)m_szUser.c_str(), (char *)m_szPwd.c_str());

	//set initial dir
	if(IsConnected())
	{
		if(!m_strRemoteDir.IsEmpty())
			SetDir(m_strRemoteDir);

		//store current path
		m_strCurDir = m_objSession.pwd;
	}
	else
	{
		//connection error
		String strErr;
		strErr.Printf("Error: %s!", fxp_error());
		MsgBox_ThrSafe(strErr);
	}

#endif
	return IsConnected();
}

bool Vfs_Sftp::Close()
{
#ifdef _WIN32
	sftp_cmd_quit(m_objSession, NULL);
#endif
	return true; //TOFIX
}

bool Vfs_Sftp::IsConnected()
{
#ifdef _WIN32
	return sftp_connected(m_objSession);
#else
	return false;
#endif
}

bool Vfs_Sftp::ExecuteCmd(const char *szCmdLine)
{
	return false;
}

bool Vfs_Sftp::ExecuteItem(const char *szFileName)
{
	return false;
}

bool Vfs_Sftp::IsRootDir()
{
	return false;	//TOFIX
}

//browsing functions
bool Vfs_Sftp::SetRootDir()
{
	return false;  //TOFIX
}

bool Vfs_Sftp::UpDir()
{
	m_strCurDir.Replace("\\", "/");	// to prevent bugs
	String strNewPath = PathName::GetParentDirPath(GetDir());

	//if path valid and new
	if( strNewPath != GetDir() &&
	    !strNewPath.IsEmpty())
	{
		if(SetDir(strNewPath))
			return true;
	}
	return false;
}

bool Vfs_Sftp::SetDir(const char *szPath)
{
	bool bRes = false;

#ifdef _WIN32
	struct sftp_command cmd;
	cmd.nwords = cmd.wordssize = 2;
	cmd.words = (char **)malloc(cmd.wordssize * sizeof(char *));
	cmd.words[0] = "cs";
	cmd.words[1] = (char *)szPath;

	//try to change remote directory
	if(sftp_cmd_cd(m_objSession, &cmd))
		bRes = true;
	else
	{
		String strErr;
		strErr.Printf("Error: %s!", fxp_error());
		MsgBox_ThrSafe(strErr);
	}

	m_strCurDir = m_objSession.pwd;

	free(cmd.words);
#endif
	return bRes;
}

bool Vfs_Sftp::ListDir(VfsListing &list, bool &bAbort)
{
    list.Clear();

#ifdef _WIN32
	//test1: list directory
	{
		struct fxp_names *listing;
		struct sftp_command cmd;
		cmd.nwords = cmd.wordssize = 2;
		cmd.words = (char **)malloc(cmd.wordssize * sizeof(char *));
		cmd.words[0] = "ls";
		cmd.words[1] = ".";

		listing = (struct fxp_names *)sftp_cmd_ls(m_objSession, &cmd);
		if(listing) {
			int i;
			struct fxp_name **pNames = (struct fxp_name **)listing->names;

			//DebugFn("Listing files:", (unsigned long)this);
			VfsItem info;
			for(i=0; i<listing->nnames;i++){
				if(0 == strcmp(".", pNames[i]->filename)){
					fxp_free_name(pNames[i]);
					continue;
				}

				info.m_strName = pNames[i]->filename;
				if(pNames[i]->attrs.permissions & 0040000){
					info.m_nSize = -1;
					info.m_nAttrib = ATTR_DIR;
				}
				else{
					info.m_nSize  = pNames[i]->attrs.size.lo +
									pNames[i]->attrs.size.hi*UINT_MAX;
					info.m_nAttrib = 0;
				}
				info.m_nLastModDate	= (time_t)pNames[i]->attrs.mtime;

				//TOFIX change to portable flags
				//info.m_dwAttributes = pNames[i]->attrs.permissions;

				info.CalcExt();
				list.Insert(info);

				//DebugFn(pNames[i]->longname, (unsigned long)this);
				fxp_free_name(pNames[i]);
			}
			sfree(pNames);
			sfree(listing);
		}

		free(cmd.words);
		return true;
	}
#endif

	return false;
}

bool Vfs_Sftp::Copy(VfsItem &item, const char *szNewName, Vfs *pVfsDest, INT64 nOffset)
{
	//NOTE: do not pass entire path, only file name ?
	//TOFIX src and destination dirs must be used automatically
	if(pVfsDest->GetType() == Vfs::LOCAL || pVfsDest->GetType() == Vfs::NET)
	{
		String strDest = pVfsDest->GetDir();
		PathName::EnsureTerminated(strDest);
		strDest += (szNewName)?	szNewName : item.GetName().c_str();
		pVfsDest->FixPath(strDest);

		return CopyToLocal(item, strDest, nOffset);
	}
	else
	{
		//create temporary path name
		String strTmpPath;
		strTmpPath = PathName::Path_TempDirectory();

		//TOFIX
		char szNameBuffer[1024] = "";
		//GetTempFileName(strTmpPath, item.GetName(), 0, szNameBuffer);

		//NOTE: this operation will require double progress (unpack + copy/pack)
		//		see COperations::IsDirectCopy
		if( CopyToLocal(item, szNameBuffer) &&
		    pVfsDest->CopyFromLocal(szNameBuffer, item))
		{
			return true;
		}
	}

	return false;
}

bool Vfs_Sftp::CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset)
{
#ifdef _WIN32
	PathName::EnsureNotTerminated(m_strCurDir);
	PathName::EnsureTerminated(m_strCurDir, '/');
	m_strCurDir.Replace("\\", "/");	// to prevent bugs

	String strRemote;
	strRemote  = m_strCurDir;
	strRemote += item.GetName();

	sftp_progress_fn(m_objSession, ProgressWrapper, (unsigned long)this);
	if(m_pProgress)
		m_pProgress->InitCurrentFiles(szLocalPath, strRemote);

	struct sftp_command cmd;
	cmd.nwords = cmd.wordssize = 3;
	cmd.words = (char **)malloc(cmd.wordssize * sizeof(char *));
	cmd.words[0] = "";
	cmd.words[1] = (char *)szLocalPath;
	cmd.words[2] = (char *)strRemote.c_str();

	bool bRes = false;
	if(sftp_cmd_put(m_objSession, &cmd))	//TOFIX change interface to have both names inside
		bRes = true;
	else{
		String strErr;
		strErr.Printf("Error: %s!", fxp_error());
		MsgBox_ThrSafe(strErr);
	}

	//cleanup
	free(cmd.words);
	return bRes;
#else
	return false;
#endif
}

bool Vfs_Sftp::CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset)
{
#ifdef _WIN32
	PathName::EnsureNotTerminated(m_strCurDir);
	PathName::EnsureTerminated(m_strCurDir, '/');
	m_strCurDir.Replace("\\", "/");	// to prevent bugs

	String strRemote;
	strRemote  = m_strCurDir;
	strRemote += item.GetName();

	sftp_progress_fn(m_objSession, ProgressWrapper, (unsigned long)this);
	if(m_pProgress)
		m_pProgress->InitCurrentFiles(strRemote, szLocalPath);

	struct sftp_command cmd;
	cmd.nwords = cmd.wordssize = 3;
	cmd.words = (char **)malloc(cmd.wordssize * sizeof(char *));
	cmd.words[0] = "ls";
	cmd.words[1] = (char *)strRemote.c_str();
	cmd.words[2] = (char *)szLocalPath;

	bool bRes = false;
	if(sftp_cmd_get(m_objSession, &cmd))	//TOFIX change interface to have both names inside
		bRes = true;
	else{
		String strErr;
		strErr.Printf("Error: %s!", fxp_error());
		MsgBox_ThrSafe(strErr);
	}

	//cleanup
	free(cmd.words);
	return bRes;
#else
	return false;
#endif
}

//action functions
bool Vfs_Sftp::Rename(const char *szItem, const char *szNewItem)
{
#ifdef _WIN32
	PathName::EnsureNotTerminated(m_strCurDir);
	PathName::EnsureTerminated(m_strCurDir, '/');
	m_strCurDir.Replace("\\", "/");	// to prevent bugs

	String strSrc(m_strCurDir);
	strSrc  += szItem;
	String strDest(m_strCurDir);
	strDest += szNewItem;

	struct sftp_command cmd;
	cmd.nwords = cmd.wordssize = 3;
	cmd.words = (char **)malloc(cmd.wordssize * sizeof(char *));
	cmd.words[0] = "";
	cmd.words[1] = (char *)strSrc.c_str();
	cmd.words[2] = (char *)strDest.c_str();

	bool bRes = false;
	if(sftp_cmd_mv(m_objSession, &cmd))
		bRes = true;
	else
	{
		String strErr;
		strErr.Printf("Error: %s!", fxp_error());
		MsgBox_ThrSafe(strErr);
	}

	free(cmd.words);
	return bRes;
#else
	return false;
#endif
}

bool Vfs_Sftp::Delete(VfsItem &item, int &nOpSettings)
{
#ifdef _WIN32
	int nLen = item.GetName().size();
	if(nLen == 0)
		return false;
	char *szFile = (char *)malloc(nLen+1);
	if(!szFile)
		return false;
	strcpy(szFile, item.GetName().c_str());

	struct sftp_command cmd;
	cmd.nwords = cmd.wordssize = 2;
	cmd.words = (char **)malloc(cmd.wordssize * sizeof(char *));
	cmd.words[0] = "";
	cmd.words[1] = szFile;

	int nRes = 0;

    if(item.IsDir())
        nRes = sftp_cmd_rmdir(m_objSession, &cmd);
    else
        nRes = sftp_cmd_rm(m_objSession, &cmd);

	free(szFile);

	if(!nRes)
	{
		String strErr;
		strErr.Printf("Error: %s!", fxp_error());
		MsgBox_ThrSafe(strErr);
	}

	//cleanup
	free(cmd.words);
	return (nRes>0);
#else
	return false;
#endif
}

bool Vfs_Sftp::MkDir(const char *szName)
{
#ifdef _WIN32
	struct sftp_command cmd;
	cmd.nwords = cmd.wordssize = 2;
	cmd.words = (char **)malloc(cmd.wordssize * sizeof(char *));
	cmd.words[0] = "";
	cmd.words[1] = (char *)szName;

	bool bRes = false;
	if(sftp_cmd_mkdir(m_objSession, &cmd))
		bRes = true;
	else
	{
		String strErr;
		strErr.Printf("Error: %s!", fxp_error());
		MsgBox_ThrSafe(strErr);
	}


	free(cmd.words);
	return bRes;
#else
	return false;
#endif
}

void Vfs_Sftp::ShowCtxMenu(VfsSelection &items, int x, int y)
{
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
}

void DebugFn(const char *szText, unsigned long data)
{
	String strMsg(szText);
	strMsg += "\r\n";

	Vfs_Sftp *pVFS = (Vfs_Sftp *)data;
	if(NULL != pVFS && NULL != pVFS->m_pfnTrace)
		pVFS->m_pfnTrace(strMsg, TT_INFO, pVFS->m_dwTraceData);
	else
		TRACE(strMsg);
}

void Vfs_Sftp::SetTraceCallback(FTP_TRACE pfTrace, unsigned long dwData)
{
	m_pfnTrace		= pfTrace;
	m_dwTraceData	= dwData;
}

String Vfs_Sftp::GetPathTitle()
{
	String strRes("sftp://");
	strRes += m_szHost;
	strRes += m_strCurDir;
	return strRes;
}

int ProgressWrapper(INT64 pos, INT64 size, unsigned long data)
{
	Vfs_Sftp *pVFS = (Vfs_Sftp *)data;
	if(NULL != pVFS)
	{
		//handle abort request
		if(pVFS->m_pProgress->IsAborted())
			return 0;	//stop operation

		if(0 == pos)
			pVFS->m_pProgress->InitCurrentProgress(0, size);
		else
			pVFS->m_pProgress->SetPos(pos);
	}

	return 1;	//keeep going
}

int Password(unsigned long data)
{
	Vfs_Sftp *pVFS = (Vfs_Sftp *)data;
	if(NULL != pVFS)
	{
		String strTitle = _("Please enter correct password.");
		String strPass;
		int nRes = MsgNameInput_ThrSafe(strTitle, strPass, true);
		if(GTK_RESPONSE_OK == nRes)
		{
#ifdef _WIN32
			strcpy(pVFS->m_objSession.m_password, strPass.c_str());
#endif
			return 1;
		}
	}

	return 0;
}

int HostKeyWarning(const char *message, int type, unsigned long data)
{
	if(0 == type)
		MsgBox_ThrSafe(message);
	else{
		int nRes = MsgBox_ThrSafe(message, 0, true);  //true->Yes/No/Cancel
		if(GTK_RESPONSE_YES == nRes)
			return 1;
		else if(GTK_RESPONSE_NO == nRes)
			return 2;
	}

	return 0;
}

int Error(const char *message, unsigned long data)
{
	MsgBox_ThrSafe(message);

	Vfs_Sftp *pVFS = (Vfs_Sftp *)data;
	if(NULL != pVFS)
		pVFS->Close();
	return 0;
}

