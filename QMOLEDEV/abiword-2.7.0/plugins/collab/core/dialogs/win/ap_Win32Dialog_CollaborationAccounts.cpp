/* AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 by Ryan Pavlik <abiryan@ryand.net>
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <windows.h>
#include "xap_App.h"
#include "ap_Win32App.h"
#include "xap_Win32App.h"
#include "xap_Frame.h"
#include "xap_Win32FrameImpl.h"
#include "xap_Win32DialogHelper.h"
#include "ut_string_class.h"

#include "ap_Win32Dialog_CollaborationAccounts.h"

BOOL CALLBACK AP_Win32Dialog_CollaborationAccounts::s_dlgProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg)
	{
		case WM_INITDIALOG:
		{
			AP_Win32Dialog_CollaborationAccounts* pThis = (AP_Win32Dialog_CollaborationAccounts *)lParam;
			UT_return_val_if_fail(pThis, false);
			SetWindowLong(hWnd,DWL_USER,lParam);
			return pThis->_onInitDialog(hWnd,wParam,lParam);
		}
		case WM_COMMAND:
		{
			AP_Win32Dialog_CollaborationAccounts* pThis = (AP_Win32Dialog_CollaborationAccounts *)GetWindowLong(hWnd,DWL_USER);
			UT_return_val_if_fail(pThis, false);
			return pThis->_onCommand(hWnd,wParam,lParam);
		}
		case WM_DESTROY:
		{
			UT_DEBUGMSG(("Got WM_DESTROY\n"));
			AP_Win32Dialog_CollaborationAccounts* pThis = (AP_Win32Dialog_CollaborationAccounts *)GetWindowLong(hWnd,DWL_USER);
			UT_return_val_if_fail(pThis, false);
			DELETEP(pThis->m_pWin32Dialog);
			return true;
		}		
		default:
			// Message not processed - Windows should take care of it
			return false;
		}
}

XAP_Dialog * AP_Win32Dialog_CollaborationAccounts::static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id)
{
	return static_cast<XAP_Dialog *>(new AP_Win32Dialog_CollaborationAccounts(pFactory, id));
}
pt2Constructor ap_Dialog_CollaborationAccounts_Constructor = &AP_Win32Dialog_CollaborationAccounts::static_constructor;

AP_Win32Dialog_CollaborationAccounts::AP_Win32Dialog_CollaborationAccounts(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: AP_Dialog_CollaborationAccounts(pDlgFactory, id),
	m_pWin32Dialog(NULL),
	m_hInstance(NULL)
{
	AbiCollabSessionManager * pSessionManager= AbiCollabSessionManager::getManager();
	if (pSessionManager)
	{
		m_hInstance=pSessionManager->getInstance();
	}
}

void AP_Win32Dialog_CollaborationAccounts::runModal(XAP_Frame * pFrame)
{
	UT_return_if_fail(pFrame);
	UT_return_if_fail(m_hInstance);

	// create the dialog
	LPCTSTR lpTemplate = MAKEINTRESOURCE(AP_RID_DIALOG_COLLABORATIONACCOUNTS);
	int result = DialogBoxParam( m_hInstance, lpTemplate,
		static_cast<XAP_Win32FrameImpl*>(pFrame->getFrameImpl())->getTopLevelWindow(),
		(DLGPROC)s_dlgProc, (LPARAM)this );
	switch (result)
	{
		case 0:
			// MSDN: If the function fails because the hWndParent parameter is invalid, the return value is zero.
			break;
		case -1:
			UT_DEBUGMSG(("Win32 error: %d.  lpTemplate: %d, RID:%d\n", GetLastError(), lpTemplate, AP_RID_DIALOG_COLLABORATIONACCOUNTS));
			break;
		default:
			break;
	};
}

/*****************************************************************/

std::map<UT_UTF8String, AccountHandler*> AP_Win32Dialog_CollaborationAccounts::_constructModel()
{
	std::map<UT_UTF8String, AccountHandler*> mModel;
	UT_UTF8String currentEntry="";

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, mModel);
	
	for (UT_sint32 i = 0; i < pManager->getAccounts().getItemCount(); i++)
	{
		AccountHandler* pHandler = pManager->getAccounts().getNthItem(i);
		if (pHandler)
		{
			UT_DEBUGMSG(("Got account: %s of type %s\n", 
					pHandler->getDescription().utf8_str(), 
					pHandler->getDisplayType().utf8_str()
				));
			currentEntry = "";
			currentEntry += pHandler->getDescription().utf8_str();
			currentEntry += " - ";
			currentEntry += pHandler->getDisplayType().utf8_str();
			if (pHandler->isOnline())
			{
				currentEntry += " (Online)";
			}
			mModel[currentEntry]=pHandler;
		}
	}
	
	return mModel;
}

void AP_Win32Dialog_CollaborationAccounts::_setModel(std::map<UT_UTF8String, AccountHandler*> model)
{
	m_mModel = model;
	int index=0;
	std::map<UT_UTF8String, AccountHandler*>::iterator iter=model.begin();
	std::map<UT_UTF8String, AccountHandler*>::iterator end=model.end();
	m_pWin32Dialog->resetContent(AP_RID_DIALOG_COLLABORATIONACCOUNTS_ACCOUNT_LIST);
	m_mIndices.clear();
	
	while (iter!=end)
	{
		m_pWin32Dialog->addItemToList(AP_RID_DIALOG_COLLABORATIONACCOUNTS_ACCOUNT_LIST, AP_Win32App::s_fromUTF8ToWinLocale((iter->first).utf8_str()).c_str());
		m_mIndices[index]=(iter->second);
		iter++;
		index++;
	}
	_updateSelection();
}

void AP_Win32Dialog_CollaborationAccounts::setOnline(AccountHandler* pHandler, bool online)
{
	UT_DEBUGMSG(("AP_Win32Dialog_CollaborationAccounts::eventOnline()\n"));
	UT_return_if_fail(pHandler);
	
	if (!pHandler->isOnline() && online)
	{
		// if we're not online and we're asked to be
		pHandler->connect();
	}
	else if (pHandler->isOnline() && !online)
	{
		// if we are online and don't want to be
		pHandler->disconnect();
	}
	_setModel(_constructModel());
	
}

void AP_Win32Dialog_CollaborationAccounts::signal(const Event& event, const Buddy* pSource)
{
	UT_DEBUGMSG(("AP_Win32Dialog_CollaborationAccounts::signal()\n"));
	switch (event.getClassType())
	{
		case PCT_AccountNewEvent:
		case PCT_AccountOnlineEvent:
		case PCT_AccountOfflineEvent:
			// FIXME: VERY VERY BAD, CHECK WHICH ACCOUNT HAS CHANGED, AND UPDATE THAT
			_setModel(_constructModel());
			break;
		default:
			// we will ignore the rest
			break;
	}
}

//////////////////
// Windows platform functions - required for message loop
//////////////////

BOOL AP_Win32Dialog_CollaborationAccounts::_onInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	// Welcome, let's initialize a dialog!
	// Store handles for easy access
	// hDlg is stored in the DialogHelper - use that!
	
	// Get ourselves a custom DialogHelper
	DELETEP(m_pWin32Dialog);
	m_pWin32Dialog = new XAP_Win32DialogHelper(hWnd);
	
	// Set up dialog initial state
	_setModel(_constructModel());
	
	// we have no selection yet
	_updateSelection();
	
	// Center Window
	m_pWin32Dialog->centerDialog();

	return true;
}

// return true if we process the command, false otherwise
BOOL AP_Win32Dialog_CollaborationAccounts::_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	WORD wNotifyCode = HIWORD(wParam);
	WORD wId = LOWORD(wParam);
	AccountHandler* pHandler;

	switch (wId)
	{
	case AP_RID_DIALOG_COLLABORATIONACCOUNTS_CLOSE_BUTTON:
		m_answer=AP_Dialog_CollaborationAccounts::a_CLOSE;
		EndDialog(hWnd,0);
		return true;
		
	case AP_RID_DIALOG_COLLABORATIONACCOUNTS_CONNECT_BUTTON:
		if (m_bHasSelection)
		{
			pHandler=m_mIndices[m_iSelected];
			// setOnline checks validity of pHandler
			setOnline(pHandler, true);
		}
		return true;

	case AP_RID_DIALOG_COLLABORATIONACCOUNTS_DISCONNECT_BUTTON:
		if (m_bHasSelection)
		{
			pHandler=m_mIndices[m_iSelected];
			// setOnline checks validity of pHandler
			setOnline(pHandler, false);
		}
		return true;

	case AP_RID_DIALOG_COLLABORATIONACCOUNTS_ACCOUNT_LIST:
		switch (wNotifyCode)
		{
		case LBN_DBLCLK:
			// A double-click will toggle online status
			_updateSelection();
			if (m_bHasSelection)
			{
				pHandler=m_mIndices[m_iSelected];
				UT_DEBUGMSG(("An account handler is selected!\n"));
				if (pHandler)
					setOnline(pHandler, !pHandler->isOnline());
			}
			
			// WM_COMMAND message processed
			return true;
		case LBN_SELCHANGE:
			_updateSelection();
			
			// WM_COMMAND message processed
			return true;
		default:
			// unhandled activity on the list.
			return false;
		}
		
	case AP_RID_DIALOG_COLLABORATIONACCOUNTS_ADD_BUTTON:
		// open the Add dialog
		createNewAccount();
		// TODO: only refresh if it actually changed.
		_setModel(_constructModel());
		return true;
	
	case AP_RID_DIALOG_COLLABORATIONACCOUNTS_DELETE_BUTTON:
		// Delete the account
		UT_DEBUGMSG(("Account Dialog - Delete Clicked\n"));
		if (m_bHasSelection)
		{
			pHandler=m_mIndices[m_iSelected];
			UT_DEBUGMSG(("An account handler is selected!\n"));
			if (pHandler)
			{
				// TODO: we should ask for confirmation, as this account handler
				//		 could be in use by serveral AbiCollab Sessions
				UT_DEBUGMSG(("Delete account: %s of type %s\n", 
						pHandler->getDescription().utf8_str(), 
						pHandler->getDisplayType().utf8_str()
					));
				
				AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
				pManager->destroyAccount(pHandler);
				// for now, recreate the whole model; but we should really just delete
				// the iter we got above
				_setModel(_constructModel());
			}
		}
	
		return true;

	case AP_RID_DIALOG_COLLABORATIONACCOUNTS_PROPERTIES_BUTTON:
		// open the Properties dialog.  This does not exist yet, but when it does, we'll be ready.
		UT_DEBUGMSG(("AP_Win32Dialog_CollaborationAccounts::eventProperties()\n"));
		if (m_bHasSelection)
		{
			// TODO: do something like open a dialog.
		
		}
		return true;

	default:
		// WM_COMMAND message NOT processed
		return false;
	}	
}

void AP_Win32Dialog_CollaborationAccounts::_updateSelection()
{
	int selItem = m_pWin32Dialog->getListSelectedIndex(AP_RID_DIALOG_COLLABORATIONACCOUNTS_ACCOUNT_LIST);
	if (selItem != LB_ERR)
	{
		m_bHasSelection = true;
		m_iSelected = selItem;
		// TODO: Uncomment this line when Preferences does something.
		//m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_PROPERTIES_BUTTON, true);
		m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_DELETE_BUTTON, true);
		
		// Choose which of the Connect/Disconnect buttons is available
		AccountHandler* pHandler;
		pHandler=m_mIndices[m_iSelected];
		if (pHandler)
		{
			if (pHandler->isOnline())
			{
				// We're online with this account - allow disconnect.
				m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_CONNECT_BUTTON, false);
				m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_DISCONNECT_BUTTON, true);
			}
			else
			{
				// We're offline - allow connect
				m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_CONNECT_BUTTON, true);
				m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_DISCONNECT_BUTTON, false);
			}
		}
		else
		{
			// pHandler no good - don't allow connect or disconnect
			m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_CONNECT_BUTTON, false);
			m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_DISCONNECT_BUTTON, false);
		}
	}
	else
	{
		m_bHasSelection = false;
		m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_PROPERTIES_BUTTON, false);
		m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_DELETE_BUTTON, false);
		m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_CONNECT_BUTTON, false);
		m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONACCOUNTS_DISCONNECT_BUTTON, false);
	}

}
