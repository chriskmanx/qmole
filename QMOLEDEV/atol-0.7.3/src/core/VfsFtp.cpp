////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "VfsFtp.h"
#include "PathName.h"
#include "ProxyLayer.h"
#include "ProxyFtp.h"
#include "ProxySocks.h"
#include "ProxyHttp.h"
#include "FilterDesc.h"
#include <time.h>

//TOFIX move this our (Vfs should not depend on GUI)
#include <gtk/gtk.h>
#include "../support.h"
#include "../callbacks.h"
#include "String.h"
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
int MsgBox_ThrSafe(String strTitle, int nButtons = GTK_BUTTONS_OK, bool bYNC = false);


/// PROXY DATA ////
extern int g_nProxyType;
extern int g_nProxyPort;
extern int g_bPassiveMode;
extern String g_strProxyHost;
extern String g_strProxyUser;
extern String g_strProxyPass;
/// PROXY DATA ////

Vfs_Ftp::Vfs_Ftp()
{
	m_nType		= FTP;
	//m_dwStyle  &= ~(VS_DOS_ATTR); // by default FTP has UNIX style
}

Vfs_Ftp::~Vfs_Ftp()
{
	if(m_ftpCore.IsConnected())
		Close();
}

void Vfs_Ftp::FixPath(String &strPath)
{
	strPath.Replace("\\", "/");
}

bool Vfs_Ftp::IsRootDir()
{
	//TOFIX support for DOS paths in FTP servers
	if(m_strCurDir.Length() < 2)
		return true;

	return false;
}

bool Vfs_Ftp::Open()
{
	//ASSERT(!m_ftpCore.IsConnected());
	//TOFIX use log m_ftpCore.SetLogCallback();
	//TOFIX m_ftpCore.SetProgress(m_pProgress);

	int bPassiveMode = g_bPassiveMode || m_ftpInfo.m_bPassiveMode;

	//check if proxy required
	if(m_ftpInfo.m_bUseFirewall)
	{
		//TOFIX VfsManager PrepareProxy
		//TOFIX proxy must be set from outside into the Vfs

		int nSize = 11; //TOFIX sizeof(arFirewalls)/sizeof(arFirewalls[0]);
		if(0 <= g_nProxyType && g_nProxyType < nSize)	//check valid range
		{
			//if proxy type valid keep reading other data
			tProxyLoginData data;

			data.nProxyPort = g_nProxyPort;
			data.strProxyHost = g_strProxyHost.c_str();
			data.strProxyUser = g_strProxyUser.c_str();
			data.strProxyPass = g_strProxyPass.c_str();

			//copy ftp data
			data.nPeerPort		= m_ftpInfo.m_uPort;
			data.strPeerHost	= m_ftpInfo.m_strHost;
			data.strPeerUser	= m_ftpInfo.m_strUser;
			data.strPeerPass	= m_ftpInfo.m_strPassword;
			data.nProxyPassive	= g_bPassiveMode;
			//TOFIX account

			CProxyLayer *pProxy = NULL;
			if(4 == g_nProxyType){
				pProxy = new CProxyHttp;
			}
			else if(g_nProxyType>4)
			{
				pProxy = new CProxyFtp;
				((CProxyFtp*)pProxy)->m_nFtpProxyType = g_nProxyType-4;//TOFIX
			}
			else if(g_nProxyType>0){
				pProxy = new CProxySocks;
				((CProxySocks*)pProxy)->SetProxyVer(g_nProxyType);
			}

			if(pProxy){
				m_ftpCore.SetProxy(pProxy);

				//copy trace callback from ftpCore
				TRACE_FN pfTrace; unsigned long dwData;
				m_ftpCore.GetTraceCallback(pfTrace, dwData);
				pProxy->SetTraceCallback(pfTrace, dwData);

				pProxy->SetData(data);
			}
		}
		else{
			MsgBox_ThrSafe(_("Invalid proxy type!"));
			return false;
		}
	}

	m_ftpCore.SetPassiveMode(bPassiveMode != 0);
	m_ftpCore.DoOpen(m_ftpInfo);

	//set initial remote directory
	if(m_ftpCore.IsConnected())
		SetDir(m_ftpInfo.m_strRemoteDir);
	//else
	//	wxMessageBox(_("Connecting failed!"));

	return m_ftpCore.IsConnected();
}

bool Vfs_Ftp::Close()
{
	m_ftpCore.DoClose();
	return true;
}

bool Vfs_Ftp::IsConnected()
{
	return m_ftpCore.IsConnected();
}

bool Vfs_Ftp::SetRootDir()
{
	return SetDir("/");
}

bool Vfs_Ftp::UpDir()
{
	if(m_ftpCore.IsConnected())
	{
		//m_ftpCore.DoCdUp();
		String strDirOld = GetDir();
		String strNewPath = PathName::GetParentDirPath(strDirOld);
		if(strNewPath != strDirOld)
		{
			SetDir(strNewPath);
			return true;
		}
	}

	return false;
}

bool Vfs_Ftp::SetDir(const char *szPath)
{
    if(m_ftpCore.IsConnected())
    {
        //TOFIX? automatic recognition of path type (unix/dos)
        String strDir(szPath);
        strDir.Replace("\\", "/");
		PathName::EnsureTerminated(strDir, '/');

        if(!strDir.IsEmpty())
            m_ftpCore.DoCD(strDir);

        //refresh dir info
        m_ftpCore.DoPWD();
        m_strCurDir = m_ftpCore.GetDirectory();
        PathName::EnsureTerminated(m_strCurDir, '/');

        //check if dir is successfully set
        return (0 == m_strCurDir.CmpNoCase(strDir));	//TOFIX PathName::Compare
    }

    return false;
}

bool Vfs_Ftp::ListDir(VfsListing &list, bool &bAbort)
{
	list.Clear();

	if(m_ftpCore.DoList()){
		//TOFIX better mechanism
		//to prevent filter destruction save it temporary

//TOFIX
//		FilterDesc filter;
//		filter = list.GetFilter();

		list = m_ftpCore.m_Listing;

//		list.SetFilter(filter);
//		list.FilterList();
		return true;
	}

	return false;
}

bool Vfs_Ftp::Rename(const char *szItem, const char *szNewItem)
{
    return (m_ftpCore.DoRename(szItem, szNewItem) > 0);
}

bool Vfs_Ftp::Delete(VfsItem &item, int &nOpSettings)
{
    if(item.IsDir())
        return m_ftpCore.RemoveDirectory(item.GetName());
    else
        return m_ftpCore.DeleteFile(item.GetName());
	return false;
}

bool Vfs_Ftp::MkDir(const char *szName)
{
    if(m_ftpCore.IsConnected())
        return m_ftpCore.MakeDirectory(szName);
	return false;
}

bool Vfs_Ftp::ExecuteCmd(const char *szCmdLine)
{
    //TOFIX parse ftp commands
	if(m_ftpCore.IsConnected()){
		String strCmd("SITE ");
		strCmd += szCmdLine;
		m_ftpCore.SendFtpCmd(strCmd);

		//TOFIX read and write reply into special dialog box
		CStrList lstReply;
		m_ftpCore.ReadReply(lstReply, *m_ftpCore.m_pSockCtrl);
		String strReply("Reply:\n");
		for(unsigned int i=0; i<lstReply.size(); i++)
		{
			strReply += lstReply[i].c_str();
			strReply += "\n";
		}
		//wxMessageBox(strReply);
	}

    return true;
}

bool Vfs_Ftp::ExecuteItem(const char *szFileName)
{
    return false;	// not supported
}

void Vfs_Ftp::Abort()
{
    m_ftpCore.DoAbort();
}

bool Vfs_Ftp::Copy(VfsItem &item, const char *szNewName, Vfs *pVfsDest, INT64 nOffset)
{
    //NOTE: do not pass entire path, only file name ?
    //TOFIX src and destination dirs must be used automatically
    if(pVfsDest->GetType() == Vfs::LOCAL || pVfsDest->GetType() == Vfs::NET)
    {
		String strDest = pVfsDest->GetDir();
		PathName::EnsureTerminated(strDest);
		pVfsDest->FixPath(strDest);
		strDest += (szNewName)?	szNewName : item.GetName().c_str();

        return CopyToLocal(item, strDest, nOffset);
    }
    else if(pVfsDest->GetType() == Vfs::FTP)
    {
        //FTP to FTP transfer
        return Ftp2FtpCopy(item.GetName(), szNewName, pVfsDest);	//TOFIX nFromOffset
    }
    else
    {
#ifdef _WIN32
		//create temporary path name
		String strTmpPath;
		strTmpPath = PathName::Path_TempDirectory();
		char szNameBuffer[MAX_PATH] = "";
		GetTempFileName(strTmpPath, item.GetName(), 0, szNameBuffer); //TOFIX Linux port

		//NOTE: this operation will require double progress (unpack + copy/pack)
		//		see COperations::IsDirectCopy
        if( CopyToLocal(item, szNameBuffer) &&
            pVfsDest->CopyFromLocal(szNameBuffer, item))
        {
            return true;
        }
#endif
    }

    return false;
}

bool Vfs_Ftp::CopyFromLocal(const char *szLocalPath, VfsItem &item, INT64 nOffset)
{
	//upload
	RefreshTransferMode(szLocalPath);
	m_ftpCore.SetProgress(m_pProgress);
	return m_ftpCore.UploadFile(szLocalPath, item.GetName(), nOffset);
}

bool Vfs_Ftp::CopyToLocal(const VfsItem &item, const char *szLocalPath, INT64 nOffset)
{
	//download
	RefreshTransferMode(item.GetName());
	m_ftpCore.SetProgress(m_pProgress);
	return m_ftpCore.DownloadFile(item.GetName(), szLocalPath, nOffset);
}

bool Vfs_Ftp::Ftp2FtpCopy(const char *szName, const char * szNewName, Vfs *pDest)
{
	m_ftpCore.SetProgress(m_pProgress);

#ifdef _DEBUG
	//time_t start = time(NULL);
#endif

	CFtpClient *pCoreSrc  = &m_ftpCore;
	CFtpClient *pCoreDest = &((Vfs_Ftp *)(pDest))->m_ftpCore;

	RefreshTransferMode(szName);
	((Vfs_Ftp *)pDest)->RefreshTransferMode(szName);

	bool bRes = pCoreSrc->FxpTransfer(szName, szName, pCoreDest);

#ifdef _DEBUG
	//time_t span = time(NULL) - start;
	//TRACE("Transfer lasted %d seconds!\n", span);
#endif

    return bRes;
}

String Vfs_Ftp::GetPathTitle()
{
	String strRes("ftp://");
	strRes += m_ftpInfo.m_strHost;
	
	//FIX for Windows server paths like "C:\\temp\\" -> ftp:://localhostC:\\temp\\"
	if(m_strCurDir.Length()>0 && m_strCurDir.at(0) != '/')
		strRes += '/';

	strRes += m_strCurDir;
	return strRes;
}

void Vfs_Ftp::RefreshTransferMode(const char * szItem)
{
	switch(m_nFtpMode){
	 case FTP_AUTO_MODE:
		{
			//TOFIX separate module for ascii ftp ext (with defualt list)
			static const char *gc_szAsciiFiles = "ASP;BAS;C;CPP;CSS;DHTML;H;HTM;HTML;MAK;PAS;PHP;PL;SHTML;TXT;";
			String strExtList = gc_szAsciiFiles;
	 		//TOFIX String strExtList = AfxGetApp()->GetProfileString("Ftp", "AsciiFiles", gc_szAsciiFiles);
			strExtList.Lower();
			String strExt = PathName::GetExt(szItem); //TOFIX without dot!!!
			if('.' == strExt.GetAt(0))
				strExt = strExt.Right(strExt.Length()-1);

			strExt += ";";
			strExt.Lower();

			int nPos = strExtList.Find(strExt);
			if(-1 == nPos){
				//TRACE("FTP AutoMode->bin\n");
				//not an ASCII extension
				m_ftpCore.SetTransferType(CFtpClient::ftpTtype_BINARY);
			}
			else if(nPos > 0 && strExtList.GetAt(nPos-1) != ';'){
				//TRACE("FTP AutoMode->bin\n");
				//not an ASCII extension
				m_ftpCore.SetTransferType(CFtpClient::ftpTtype_BINARY);
			}
			else{
				//TRACE("FTP AutoMode->ascii\n");
				//ASCII extension
				m_ftpCore.SetTransferType(CFtpClient::ftpTtype_ASCII);	
			}
		}
		break;
	 case FTP_BIN_MODE:
		 m_ftpCore.SetTransferType(CFtpClient::ftpTtype_BINARY);
		break;
	 case FTP_TEXT_MODE:
		m_ftpCore.SetTransferType(CFtpClient::ftpTtype_ASCII);
		break;
	}
}

void Vfs_Ftp::ShowCtxMenu(VfsSelection &items, int x, int y)
{
/*
	CPopupMenu menu;
	menu.Append(1, _("Dow&nload"));
	menu.Append(2, _("&Delete"));
	menu.Append(3, _("&View raw listing"));
	menu.Append(0, "");
	menu.Append(4, _("&Properties"));
*/

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


//TOFIX bit library (together with binary content lib)
bool IsBitSet(unsigned long dwValue, int nBit)
{
	unsigned long dwMask = 0x00000001;
	if(nBit > 0)
		dwMask = dwMask << nBit;

	return ((dwValue & dwMask) == dwMask);
}

void SetBit(unsigned long &dwValue, int nBit)
{
	unsigned long dwMask = 0x00000001;
	if(nBit > 0)
		dwMask = dwMask << nBit;

	dwValue |= dwMask;
}

void ClearBit(unsigned long &dwValue, int nBit)
{
	unsigned long dwMask = 0x00000001;
	if(nBit > 0)
		dwMask = dwMask << nBit;

	dwValue &= ~(dwMask);
}

void Vfs_Ftp::FtpFileProperties()
{
	//if(m_ftpCore.) //IsUnix

	//array stores access check box state
	//array is arranged backwards (to match access bit index)
	/*
	int nProp[9];

	//fetch selected FTP items
	//ASSERT(m_lstTarget.size() > 0);
	vector<String> lstItems;
	for(UINT i=0; i<m_lstTarget.size(); i++)
		lstItems.push_back(Vfs::GetBaseName(m_lstTarget[i]));

	for(i=0; i<lstItems.size(); i++){
		int nIdx = FindItem(lstItems[i]);
		if(nIdx >= 0)
		{
			DWORD dwAttr = operator[](nIdx).m_dwAttributes;

			if(0 == i)
			{
				//first item copies its attribs into field
				for(int j=0; j<9; j++)
					nProp[j] = IsBitSet(dwAttr, j)? 1:0;
			}
			else
			{
				//next items modify the field states
				for(int j=0; j<9; j++)
				{
					int nCurVal = nProp[j];
					if(nCurVal != 2){
						int nNewVal = IsBitSet(dwAttr, j)? 1:0;
						if(nCurVal != nNewVal)
							nProp[j] = 2;	//multiple values for single field
					}
				}
			}
		}
	}
	
	CFtpUnixPropertyDlg dlg;
	dlg.m_nPE = nProp[0];
	dlg.m_nPW = nProp[1];
	dlg.m_nPR = nProp[2];
	dlg.m_nGE = nProp[3];
	dlg.m_nGW = nProp[4];
	dlg.m_nGR = nProp[5];
	dlg.m_nOE = nProp[6];
	dlg.m_nOW = nProp[7];
	dlg.m_nOR = nProp[8];
	
	if(IDOK == dlg.DoModal())
	{
		//if properties changed set new states
		//(if system supports "SITE CHMOD")
		if(	dlg.m_nPE != nProp[0] ||
			dlg.m_nPW != nProp[1] ||
			dlg.m_nPR != nProp[2] ||
			dlg.m_nGE != nProp[3] ||
			dlg.m_nGW != nProp[4] ||
			dlg.m_nGR != nProp[5] ||
			dlg.m_nOE != nProp[6] ||
			dlg.m_nOW != nProp[7] ||
			dlg.m_nOR != nProp[8] )
		{
			//for each selected item
			for(i=0; i<lstItems.size(); i++)
			{
				int nIdx = FindItem(lstItems[i]);
				if(nIdx >= 0)
				{
					//curent atributes
					DWORD dwCurAttr = operator[](nIdx).m_dwAttributes;

					//calc new attributes
					DWORD dwNewAttr = 0;

					//calculate public execute flag
					if(2 == dlg.m_nPE){
						if(IsBitSet(dwCurAttr, 0))
							SetBit(dwNewAttr, 0);
					}
					else{
						if(1 == dlg.m_nPE)
							SetBit(dwNewAttr, 0);
						else
							ClearBit(dwNewAttr, 0);
					}
					//calculate public write flag
					if(2 == dlg.m_nPW){
						if(IsBitSet(dwCurAttr, 1))
							SetBit(dwNewAttr, 1);
					}
					else{
						if(1 == dlg.m_nPW)
							SetBit(dwNewAttr, 1);
						else
							ClearBit(dwNewAttr, 1);
					}
					//calculate public read flag
					if(2 == dlg.m_nPR){
						if(IsBitSet(dwCurAttr, 2))
							SetBit(dwNewAttr, 2);
					}
					else{
						if(1 == dlg.m_nPR)
							SetBit(dwNewAttr, 2);
						else
							ClearBit(dwNewAttr, 2);
					}
					//calculate group execute flag
					if(2 == dlg.m_nGE){
						if(IsBitSet(dwCurAttr, 3))
							SetBit(dwNewAttr, 3);
					}
					else{
						if(1 == dlg.m_nGE)
							SetBit(dwNewAttr, 3);
						else
							ClearBit(dwNewAttr, 3);
					}
					//calculate group write flag
					if(2 == dlg.m_nGW){
						if(IsBitSet(dwCurAttr, 4))
							SetBit(dwNewAttr, 4);
					}
					else{
						if(1 == dlg.m_nGW)
							SetBit(dwNewAttr, 4);
						else
							ClearBit(dwNewAttr, 4);
					}
					//calculate group read flag
					if(2 == dlg.m_nGR){
						if(IsBitSet(dwCurAttr, 5))
							SetBit(dwNewAttr, 5);
					}
					else{
						if(1 == dlg.m_nGR)
							SetBit(dwNewAttr, 5);
						else
							ClearBit(dwNewAttr, 5);
					}
					//calculate owner execute flag
					if(2 == dlg.m_nOE){
						if(IsBitSet(dwCurAttr, 6))
							SetBit(dwNewAttr, 6);
					}
					else{
						if(1 == dlg.m_nOE)
							SetBit(dwNewAttr, 6);
						else
							ClearBit(dwNewAttr, 6);
					}
					//calculate owner write flag
					if(2 == dlg.m_nOW){
						if(IsBitSet(dwCurAttr, 7))
							SetBit(dwNewAttr, 7);
					}
					else{
						if(1 == dlg.m_nOW)
							SetBit(dwNewAttr, 7);
						else
							ClearBit(dwNewAttr, 7);
					}
					//calculate owner read flag
					if(2 == dlg.m_nOR){
						if(IsBitSet(dwCurAttr, 8))
							SetBit(dwNewAttr, 8);
					}
					else{
						if(1 == dlg.m_nOR)
							SetBit(dwNewAttr, 8);
						else
							ClearBit(dwNewAttr, 8);
					}
					
					//NOTE: could use single CHMOD for all files if no 2 values in check boxes
					//change file properties
					int user, group, rest;
					rest   = dwNewAttr & 7;
					dwNewAttr = dwNewAttr >> 3;
					group  = dwNewAttr &  7;
					dwNewAttr = dwNewAttr >> 3;
					user   = dwNewAttr & 7;

					String strResult;
					strResult.Format("SITE CHMOD %d%d%d %s", user, group, rest, lstItems[i]);

					if(m_ftpCore.SendFtpCmd(strResult))
					{
						CStrList lstReply;
						m_ftpCore.ReadReply(lstReply, m_ftpCore.m_sockCtrl);
						
						//error handling
						if(lstReply.size()>0 && lstReply[0].GetLength()>0)
							if(lstReply[0].GetAt(0) != '2')
							{
								//"200 chmod command successful"
								String strErr(lstReply[0]);
								strErr.Replace("\r\n", NULL);

								String strMsg;
								strMsg.Printf(_("Error when changing item attributes!\n(%s)"), strErr);
								wxMessageBox(strMsg);
								return;	//skip changing other file attributes
							}
					}
					else
						wxMessageBox(_("Failed to send FTP command!"));
				}
			}
		}
	}
	*/
}

void Vfs_Ftp::ViewRawListing()
{
#ifdef _WIN32
	//create temporary path name
	String strTmpPath;
	strTmpPath = PathName::Path_TempDirectory();
	char szNameBuffer[1024] = "";
	//TOFIX add ftp name into file name?
	GetTempFileName(strTmpPath, "List", 0, szNameBuffer);

	FILE *pFile = fopen(szNameBuffer, "wb");
	if(NULL != pFile)
	{
		for(unsigned int i=0; i<m_ftpCore.m_lstDirRaw.size(); i++){
			String strLine(m_ftpCore.m_lstDirRaw[i].c_str());
			fwrite(strLine, strLine.Length(), 1, pFile);
		}

		fclose(pFile);

		//view file + delete when done
		//TOFIX ::SendMessage(g_info.hwndMain, WMU_NEW_LISTER, (WPARAM)szNameBuffer, 1);
	}
	else
		gtkMessageBox(_("Error writing list to file!"));
#endif		
}

void Vfs_Ftp::SetTraceCallback(FTP_TRACE pfTrace, unsigned long dwData)
{
	m_pfnTrace    = pfTrace;
	m_dwTraceData = dwData;

	m_ftpCore.SetTraceCallback(pfTrace, dwData);
}
