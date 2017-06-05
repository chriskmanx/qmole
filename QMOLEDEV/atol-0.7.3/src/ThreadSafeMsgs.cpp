////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for some thread-safe message boxes
////////////////////////////////////////////////////////////////////////////

#include "support.h"
#include "ThreadSafeMsgs.h"
#include "ThreadSafeGui.h"
#include "core/opcodes.h"
#include "OverwriteDlg.h"
#include "DeleteFileDlg.h"
#include "DeleteDirDlg.h"
#include "CopyErrDlg.h"
#include "OpErrorDlg.h"
#include "FileList.h"
#include "GuiInputDlg.h"

extern GtkWidget *atol_main;
extern bool g_bShowDirWarningDialog;
extern bool g_bShowReadOnlyWarningDialog; 
extern ThreadSafeQueue g_gui_pump;
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
int gtkMessageBoxYNC(const char *szText);

class MsgBoxAction : public ThreadSafeAction
{
	void Run();

public:
	String m_strText;
	int    m_nButtons;
	int    m_nResult;
	bool   m_bYNC;
};

void MsgBoxAction::Run()
{
	if(m_bYNC)
		m_nResult = gtkMessageBoxYNC(m_strText);
	else
		m_nResult = gtkMessageBox(m_strText, m_nButtons);
}

class DlgBoxAction : public ThreadSafeAction
{
	void Run();

public:
	Dialog *m_pDialog;
	int     m_nResult;
};

void DlgBoxAction::Run()
{
	m_pDialog->Create();
	m_nResult = m_pDialog->ShowModal();
	m_pDialog->Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

class FileListRefreshAction : public ThreadSafeAction
{
	void Run();

public:
	FileList *m_pList;
};

void FileListRefreshAction::Run()
{
	m_pList->PanelList(true, true);
}

int MsgBox_ThrSafe(String strTitle, int nButtons, bool bYNC)
{
	MsgBoxAction *pAction = new MsgBoxAction;
	pAction->m_bYNC     = bYNC;
	pAction->m_strText	= strTitle;	
	pAction->m_nButtons	= nButtons;
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);	//block this thread until action done
	int nResult = pAction->m_nResult;
	delete pAction;				//free the action object
	return nResult;
}

//display file overwrite request (returns operation flags)
int MsgOverwrite_ThrSafe(String strTitle)
{
	DlgBoxAction *pAction = new DlgBoxAction;
	pAction->m_pDialog    = new OverwriteDlg;	
	((OverwriteDlg *)pAction->m_pDialog)->m_strInfo = strTitle;
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);    //block this thread until action done
	int nResult = pAction->m_nResult;
	delete pAction->m_pDialog;   //free the dialog object
	delete pAction;              //free the action object
	return nResult;
}

//display copy error message (returns operation flags)
int MsgCopyError_ThrSafe(String strTitle)
{
	DlgBoxAction *pAction = new DlgBoxAction;
	pAction->m_pDialog    = new CopyErrDlg;	
	((CopyErrDlg *)pAction->m_pDialog)->m_strInfo = strTitle;
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);    //block this thread until action done
	int nResult = pAction->m_nResult;
	delete pAction->m_pDialog;   //free the dialog object
	delete pAction;              //free the action object
	return nResult;
}

int MsgNameInput_ThrSafe(String strTitle, String &strValue, bool bPassword)
{
	DlgBoxAction *pAction = new DlgBoxAction;
	pAction->m_pDialog    = new GuiInputDlg(bPassword, false);	
	((GuiInputDlg*)pAction->m_pDialog)->SetLabel(strTitle);
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);    //block this thread until action done
	int nResult = pAction->m_nResult;
	strValue = ((GuiInputDlg*)pAction->m_pDialog)->GetInput();

	delete pAction->m_pDialog;   //free the dialog object
	delete pAction;              //free the action object
	return nResult;
}

//display (read only) file delete request (returns operation flags)
int MsgDelete_ThrSafe(String strFile)
{
	//if skipping user warning
	if(!g_bShowReadOnlyWarningDialog)
		return OPF_DELETE;
 
	String strTitle;
	strTitle.Printf(_("File %s is read only! Delete?"), strFile.c_str());

	DlgBoxAction *pAction	= new DlgBoxAction;
	pAction->m_pDialog	= new DeleteFileDlg;	
	((DeleteFileDlg *)pAction->m_pDialog)->m_strInfo = strTitle;
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);	//block this thread until action done
	int nResult = pAction->m_nResult;
	delete pAction->m_pDialog;	//free the dialog object
	delete pAction;			//free the action object
	return nResult;
}

//display directory delete request (returns operation flags)
int MsgDeleteDir_ThrSafe(String strFile)
{
	//if skipping user warning
	if(!g_bShowDirWarningDialog)
		return OPF_DELETE;

	String strTitle;
	strTitle.Printf(_("Directoy %s is not empty! Delete?"), strFile.c_str());

	DlgBoxAction *pAction	= new DlgBoxAction;
	pAction->m_pDialog	= new DeleteDirDlg;	
	((DeleteDirDlg *)pAction->m_pDialog)->m_strInfo = strTitle;
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);	//block this thread until action done
	int nResult = pAction->m_nResult;
	delete pAction->m_pDialog;	//free the dialog object
	delete pAction;			//free the action object
	return nResult;
}

int MsgDeleteError_ThrSafe(String strMsg)
{
	return MsgOperationError_ThrSafe(strMsg);
}

int MsgOperationError_ThrSafe(String strMsg)
{
	DlgBoxAction *pAction = new DlgBoxAction;
	pAction->m_pDialog    = new OpErrorDlg;	
	((OpErrorDlg *)pAction->m_pDialog)->m_strInfo = strMsg;
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);	//block this thread until action done
	int nResult = pAction->m_nResult;
	delete pAction->m_pDialog;	//free the dialog object
	delete pAction;			//free the action object
	return nResult;
}

void Refresh_ThrSafe(long list)
{
	FileListRefreshAction *pAction = new FileListRefreshAction;
	pAction->m_pList = (FileList *)list;	
	
	g_gui_pump.Add(pAction);
	g_gui_pump.Wait(pAction);   //block this thread until action done
	delete pAction;             //free the action object
}
