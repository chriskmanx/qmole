/* AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 by Ryan Pavlik <abiryan@ryand.net>
 * Copyright (C) 2006,2008 by Marc Maurer <uwog@uwog.net>
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
#include "xap_Win32DialogHelper.h"
#include "ut_string_class.h"
#include <xp/AbiCollabSessionManager.h>
#include <backends/xp/AccountHandler.h>

#include "ap_Win32Dialog_CollaborationAddAccount.h"

BOOL CALLBACK AP_Win32Dialog_CollaborationAddAccount::s_dlgProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg)
	{
		case WM_INITDIALOG:
		{
			AP_Win32Dialog_CollaborationAddAccount* pThis = (AP_Win32Dialog_CollaborationAddAccount *)lParam;
			UT_return_val_if_fail(pThis, false);
			SetWindowLong(hWnd,DWL_USER,lParam);
			return pThis->_onInitDialog(hWnd,wParam,lParam);
		}
		case WM_COMMAND:
		{
			AP_Win32Dialog_CollaborationAddAccount* pThis = (AP_Win32Dialog_CollaborationAddAccount *)GetWindowLong(hWnd,DWL_USER);
			UT_return_val_if_fail(pThis, false);
			return pThis->_onCommand(hWnd,wParam,lParam);
		}
		case WM_DESTROY:
		{
			UT_DEBUGMSG(("Got WM_DESTROY\n"));
			AP_Win32Dialog_CollaborationAddAccount* pThis = (AP_Win32Dialog_CollaborationAddAccount *)GetWindowLong(hWnd,DWL_USER);
			UT_return_val_if_fail(pThis, false);
			DELETEP(pThis->m_pWin32Dialog);
			return true;
		}
		default:
			// Message not processed - Windows should take care of it
			return false;
	}
}

XAP_Dialog * AP_Win32Dialog_CollaborationAddAccount::static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id)
{
	return static_cast<XAP_Dialog *>(new AP_Win32Dialog_CollaborationAddAccount(pFactory, id));
}
pt2Constructor ap_Dialog_CollaborationAddAccount_Constructor = &AP_Win32Dialog_CollaborationAddAccount::static_constructor;

AP_Win32Dialog_CollaborationAddAccount::AP_Win32Dialog_CollaborationAddAccount(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: AP_Dialog_CollaborationAddAccount(pDlgFactory, id),
	m_pWin32Dialog(NULL),
	m_hInstance(NULL),
	m_hOk(NULL),
	m_hDetails(NULL)
{
	AbiCollabSessionManager * pSessionManager = AbiCollabSessionManager::getManager();
	if (pSessionManager)
	{
		m_hInstance=pSessionManager->getInstance();
	}
}


void AP_Win32Dialog_CollaborationAddAccount::runModal(XAP_Frame * pFrame)
{
	UT_return_if_fail(pFrame);
	UT_return_if_fail(m_hInstance);

	// create the dialog
	LPCTSTR lpTemplate = MAKEINTRESOURCE(AP_RID_DIALOG_COLLABORATIONADDACCOUNT);
	int result = DialogBoxParam( m_hInstance, lpTemplate,
		static_cast<XAP_Win32FrameImpl*>(pFrame->getFrameImpl())->getTopLevelWindow(),
		(DLGPROC)s_dlgProc, (LPARAM)this );
	switch (result)
	{
		case 0:
			// MSDN: If the function fails because the hWndParent parameter is invalid, the return value is zero.
			break;
		case -1:
			UT_DEBUGMSG(("Win32 error: %d.  lpTemplate: %d, RID:%d\n", GetLastError(), lpTemplate, AP_RID_DIALOG_COLLABORATIONADDACCOUNT));
			break;
		default:
			break;
	};
}

/*****************************************************************/
BOOL AP_Win32Dialog_CollaborationAddAccount::_onInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	// Welcome, let's initialize a dialog!
	//////
	// Store handles for easy access
	// Dialog handle stored in DialogHelper - use it!
	
	m_hOk = GetDlgItem(hWnd, AP_RID_DIALOG_COLLABORATIONADDACCOUNT_OK_BUTTON);
	UT_ASSERT(m_hOk);
	m_hDetails = GetDlgItem(hWnd, AP_RID_DIALOG_COLLABORATIONADDACCOUNT_DETAILS_BOX);
	UT_ASSERT(m_hDetails);
	
	// Danger Will Robinson:  This will cause the dialog to not work due to asserts in onCommand pThis.
	//SetWindowLong(m_hDetails, GWL_WNDPROC, AP_Win32Dialog_CollaborationAddAccount::s_dlgProc);
	
	//////
	// Get ourselves a custom DialogHelper
	if (m_pWin32Dialog)
	{
		DELETEP(m_pWin32Dialog);
	}
	m_pWin32Dialog = new XAP_Win32DialogHelper(hWnd);
	
	//////
	// Set up dialog initial state
	_populateWindowData();
	
	// Center Window
	m_pWin32Dialog->centerDialog();

	// Return 1 to proceed with dialog initialization
	return 1;
}

// return true if we process the command, false otherwise
BOOL AP_Win32Dialog_CollaborationAddAccount::_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	WORD wNotifyCode = HIWORD(wParam);
	WORD wId = LOWORD(wParam);
	
	AccountHandler* pHandler;
	switch (wId)
	{
	case AP_RID_DIALOG_COLLABORATIONADDACCOUNT_TYPECOMBO:
		// Oh hey, something happened to the typecombo!
		if (wNotifyCode == CBN_SELCHANGE)
		{
			// New account type selected
			UT_DEBUGMSG(("Add Account - New handler type selected\n"));
			pHandler = _getActiveAccountHandler();
			if (pHandler)
			{
				UT_DEBUGMSG(("Changed account handler to type: %s\n", pHandler->getDisplayType().utf8_str()));
				_setAccountHandler(pHandler);
			}
			else
				UT_DEBUGMSG(("No account handler types to select; this makes abicollab kinda pointless...\n"));
			
			return true;
		}
		return false;
		
	case AP_RID_DIALOG_COLLABORATIONADDACCOUNT_OK_BUTTON:
		UT_DEBUGMSG(("Add Account - OK button clicked\n"));
		pHandler = _getActiveAccountHandler();
		// if pHandler is no good, then we tell Windows we didn't handle the button
		UT_return_val_if_fail(pHandler, 1);
		pHandler->storeProperties();
		m_answer=AP_Dialog_CollaborationAddAccount::a_OK;
		EndDialog(hWnd,0);
		return true;

	case AP_RID_DIALOG_COLLABORATIONADDACCOUNT_CANCEL_BUTTON:
		// Close without applying changes
		EndDialog(hWnd,0);
		m_answer=AP_Dialog_CollaborationAddAccount::a_CANCEL;
		return true;

	default:
		AccountHandler* pAccountHandler = _getActiveAccountHandler();
		if (pAccountHandler) //if we have an account handler
		{
			return pAccountHandler->_onCommand(hWnd, wParam, lParam);
		}
		
		// WM_COMMAND message NOT processed
		return false;
	}
}

void AP_Win32Dialog_CollaborationAddAccount::_populateWindowData()
{
	UT_DEBUGMSG(("AP_Win32Dialog_CollaborationAddAccount::_populateWindowData()\n"));
	
	AbiCollabSessionManager* pSessionManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pSessionManager);
	
	UT_return_if_fail(m_pWin32Dialog);
	
	for (UT_uint32 i = 0; i < pSessionManager->getRegisteredAccountHandlers().size(); i++)
	{
		AccountHandlerConstructor pConstructor = pSessionManager->getRegisteredAccountHandlers().getNthItem(i);
		UT_continue_if_fail(pConstructor);

		// TODO: we need to free these somewhere
		AccountHandler* pHandler = pConstructor();
		UT_continue_if_fail(pHandler);

		UT_sint32 index = m_pWin32Dialog->addItemToCombo(AP_RID_DIALOG_COLLABORATIONADDACCOUNT_TYPECOMBO, (LPCSTR) AP_Win32App::s_fromUTF8ToWinLocale(pHandler->getDisplayType().utf8_str()).c_str());
		if (index >= 0)
		{
			UT_DEBUGMSG(("Added handler to index %d\n", index));
			m_vAccountTypeCombo.insert(m_vAccountTypeCombo.begin()+index, pHandler);
		}
		else
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	}
	
	// if we have at least one account handler, then make sure the first one is selected
	if (pSessionManager->getRegisteredAccountHandlers().size() > 0)
	{
		m_pWin32Dialog->selectComboItem(AP_RID_DIALOG_COLLABORATIONADDACCOUNT_TYPECOMBO, 0);
		// Note, this requires that the backends be alright with having their widgets destroyed before being created.
		// TCP does this, but I didn't add such code to any other backends yet.  --RP 22 July 2007
		UT_DEBUGMSG(("Add Account - New handler type selected\n"));
		AccountHandler* pHandler = _getActiveAccountHandler();
		if (pHandler)
		{
			UT_DEBUGMSG(("Changed account handler to type: %s\n", pHandler->getDisplayType().utf8_str()));
			_setAccountHandler(pHandler);
		}
		else
			UT_DEBUGMSG(("No account handler types to select; this makes abicollab kinda pointless...\n"));
			
	}
	else
	{
		// nope, we don't have any account handler :'-( 
		// Add a sample item to show that we can.  Then, disable the box and the ok button
		m_pWin32Dialog->addItemToCombo(AP_RID_DIALOG_COLLABORATIONADDACCOUNT_TYPECOMBO, (LPCSTR) "No Handlers!");
		m_pWin32Dialog->selectComboItem(AP_RID_DIALOG_COLLABORATIONADDACCOUNT_TYPECOMBO, 0);
		m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONADDACCOUNT_TYPECOMBO, false);
		m_pWin32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONADDACCOUNT_OK_BUTTON, false);
	}
}

AccountHandler* AP_Win32Dialog_CollaborationAddAccount::_getActiveAccountHandler()
{
	UT_DEBUGMSG(("AP_Win32Dialog_CollaborationAddAccount::_getActiveAccountHandler()\n"));
	UT_return_val_if_fail(m_pWin32Dialog, NULL);
	
	int index = m_pWin32Dialog->getComboSelectedIndex(AP_RID_DIALOG_COLLABORATIONADDACCOUNT_TYPECOMBO);
	UT_return_val_if_fail(index >= 0 && index < static_cast<UT_sint32>(m_vAccountTypeCombo.size()), NULL);
	
	// check the return value of this function!
	return m_vAccountTypeCombo[index];
}

void AP_Win32Dialog_CollaborationAddAccount::setBackendValidity(bool valid)
{
	EnableWindow(m_hOk, valid);
}

