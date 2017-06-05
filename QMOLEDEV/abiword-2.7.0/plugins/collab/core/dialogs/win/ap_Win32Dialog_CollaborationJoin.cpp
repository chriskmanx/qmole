/* AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 by Ryan Pavlik <abiryan@ryand.net>
 * Copyright (C) 2006, 2007 by Marc Maurer <uwog@uwog.net>
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
#include <backends/xp/Event.h>
#include <backends/xp/AccountEvent.h>
#include <backends/xmpp/xp/XMPPBuddy.h>

#include "ap_Win32Dialog_CollaborationJoin.h"

BOOL CALLBACK AP_Win32Dialog_CollaborationJoin::s_dlgProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	AP_Win32Dialog_CollaborationJoin * pThis;
	switch (msg)
	{
	case WM_INITDIALOG:
		pThis = (AP_Win32Dialog_CollaborationJoin *)lParam;
		UT_return_val_if_fail(pThis, 0);
		SetWindowLong(hWnd,DWL_USER,lParam);
		return pThis->_onInitDialog(hWnd,wParam,lParam);
		
	case WM_COMMAND:
		pThis = (AP_Win32Dialog_CollaborationJoin *)GetWindowLong(hWnd,DWL_USER);
		UT_return_val_if_fail(pThis, 0);
		return pThis->_onCommand(hWnd,wParam,lParam);
		
	case WM_DESTROY:
		pThis = (AP_Win32Dialog_CollaborationJoin *)GetWindowLong(hWnd,DWL_USER);
		if (pThis->p_win32Dialog)
		{
			DELETEP(pThis->p_win32Dialog);
		}
		
		// WM_DESTROY processed
		return 0;
		
	case WM_NOTIFY:
		pThis = (AP_Win32Dialog_CollaborationJoin *)GetWindowLong(hWnd,DWL_USER);
		UT_return_val_if_fail(pThis, 0);
		UT_return_val_if_fail(lParam, 0);
		return pThis->_onNotify(hWnd, wParam, lParam);
		
	default:
		// Windows system should process any other messages
		return false;
	}
}

XAP_Dialog * AP_Win32Dialog_CollaborationJoin::static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id)
{
	return static_cast<XAP_Dialog *>(new AP_Win32Dialog_CollaborationJoin(pFactory, id));
}
pt2Constructor ap_Dialog_CollaborationJoin_Constructor = &AP_Win32Dialog_CollaborationJoin::static_constructor;


AP_Win32Dialog_CollaborationJoin::AP_Win32Dialog_CollaborationJoin(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: AP_Dialog_CollaborationJoin(pDlgFactory, id),
	p_win32Dialog(NULL),
	m_hInstance(NULL)
{
	UT_DEBUGMSG(("AP_Win32Dialog_CollaborationJoin()\n"));
	AbiCollabSessionManager * pSessionManager= AbiCollabSessionManager::getManager();
	if (pSessionManager)
	{
		m_hInstance=pSessionManager->getInstance();
	}
}

void AP_Win32Dialog_CollaborationJoin::runModal(XAP_Frame * pFrame)
{
	UT_return_if_fail(pFrame);
	UT_return_if_fail(m_hInstance);
	
	LPCTSTR lpTemplate = MAKEINTRESOURCE(AP_RID_DIALOG_COLLABORATIONJOIN);
	
	int result = DialogBoxParam( m_hInstance, lpTemplate,
		static_cast<XAP_Win32FrameImpl*>(pFrame->getFrameImpl())->getTopLevelWindow(),
		(DLGPROC)s_dlgProc, (LPARAM)this );
	switch (result) {
	case 0:
		// MSDN: If the function fails because the hWndParent parameter is invalid, the return value is zero.
		break;
	case -1:
		UT_DEBUGMSG(("Win32 error: %d.  lpTemplate: %d, RID:%d\n", GetLastError(), lpTemplate, AP_RID_DIALOG_COLLABORATIONJOIN));
		
		break;
	default:
		break;
		// ok!
	};

}

/*****************************************************************/

BOOL AP_Win32Dialog_CollaborationJoin::_onInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	// Welcome, let's initialize a dialog!
	//////
	// Store handles for easy access
	// Reminder: hDlg is in our DialogHelper
	m_hDocumentTreeview = GetDlgItem(hWnd, AP_RID_DIALOG_COLLABORATIONJOIN_DOCUMENT_TREE);
	
	// Set up common controls
	INITCOMMONCONTROLSEX icc;
	icc.dwSize=sizeof(INITCOMMONCONTROLSEX);
	icc.dwICC=ICC_TREEVIEW_CLASSES;
	
	// If we can't init common controls, bail out
	UT_return_val_if_fail(InitCommonControlsEx(&icc), false);
	
	// Get ourselves a custom DialogHelper
	DELETEP(p_win32Dialog);
	p_win32Dialog = new XAP_Win32DialogHelper(hWnd);
	
	//////
	// Set up dialog initial state
	_refreshAllDocHandlesAsync();
	_setModel();
	_refreshAccounts();
	
	// we have no selection yet
	_updateSelection();
	
	// Center Window
	p_win32Dialog->centerDialog();
	
	// WM_INITDIALOG wants True returned in order to continue processing
	return true;
}

BOOL AP_Win32Dialog_CollaborationJoin::_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	WORD wId = LOWORD(wParam);
	
	switch (wId)
	{
	case AP_RID_DIALOG_COLLABORATIONJOIN_DISCONNECT_BUTTON:
		// join and close
		// Formerly/secretly the OK button
		_setJoin(m_hSelected, false);
		EndDialog(hWnd, 0);
		return true;

	case AP_RID_DIALOG_COLLABORATIONJOIN_CONNECT_BUTTON:
		// join and close
		// Formerly/secretly the OK button
		_setJoin(m_hSelected, true);
		EndDialog(hWnd, 0);
		return true;

	case AP_RID_DIALOG_COLLABORATIONJOIN_CLOSE_BUTTON:
		// Close without necessarily joining
		// formerly/secretly the Cancel button
		EndDialog(hWnd,0);
		return true;

	case AP_RID_DIALOG_COLLABORATIONJOIN_ADDBUDDY_BUTTON:
		// open the Add Buddy dialog
		_eventAddBuddy();
		
		// Would be nice to know if we actually succeeded in adding a buddy
		// to avoid gratuitous refreshes
		
		// have to refresh buddies
		
		// Refresh documents
		_refreshAllDocHandlesAsync();
		_setModel();
		return true;
	
	case AP_RID_DIALOG_COLLABORATIONJOIN_DELETE_BUTTON:
		// TODO: Implement!
		// didn't actually handle this
		return false;

	case AP_RID_DIALOG_COLLABORATIONJOIN_REFRESH_BUTTON:
		// TODO: we really should refresh the buddies here as well, 
		// as they could pop up automatically as well (for example with a 
		// avahi backend)
		_refreshAllDocHandlesAsync();
		_setModel();
		return true;

	default:
		return false;
	}
}

BOOL AP_Win32Dialog_CollaborationJoin::_onNotify(HWND hWnd, WPARAM wParam, LPARAM lParam){
	switch (((LPNMHDR)lParam)->code)
	{
		//case UDN_DELTAPOS:		return pThis->_onDeltaPos((NM_UPDOWN *)lParam);
		//UT_DEBUGMSG(("Notify: Code=0x%x\n", ((LPNMHDR)lParam)->code));
		
		case NM_DBLCLK:
			return false; // need to think this through, just toggling the state sounds wrong to me, UI-wise - MARCM
			
/*			// A double-click will toggle join status
			// TODO: This is probably awful GUI-wise
			_updateSelection();
			// join stuff!
			if (m_bShareSelected)
			{
				// if they double clicked on a shared document
				if (!_setJoin(m_hSelected, true))
				{
					// if setting join to true didn't change join status
					// then set it to false.
					// this minimizes compares required to do the toggle,
					// while leaving the safety logic in setJoin to prevent double-join or disjoins
					_setJoin(m_hSelected, false);
				}
				
				// refresh list after toggle
				_refreshAllDocHandlesAsync();
				_setModel();
			}
			return 1;*/
			
		case TVN_SELCHANGED:
			_updateSelection();
			return 1;
			
		default:
			return 0;
	}
}

void AP_Win32Dialog_CollaborationJoin::_setModel()
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);
	
	const UT_GenericVector<AccountHandler *>& accounts = pManager->getAccounts();
	
	// clear the treeview
	m_mTreeItemHandles.clear();
	DWORD styles = GetWindowLong(m_hDocumentTreeview, GWL_STYLE);
	TreeView_DeleteAllItems(m_hDocumentTreeview);
	SetWindowLong(m_hDocumentTreeview, GWL_STYLE, styles);

	// Loop through accounts
	for (UT_uint32 i = 0; i < accounts.getItemCount(); i++)
	{
		// Loop through buddies in accounts
		for (UT_uint32 j = 0; j < accounts.getNthItem(i)->getBuddies().size(); j++)
		{
			const Buddy* pBuddy = accounts.getNthItem(i)->getBuddies()[j];
			UT_UTF8String buddyDesc = pBuddy->getDescription();
			UT_DEBUGMSG(("Adding buddy (%s) to the treeview\n", buddyDesc.utf8_str()));

			UT_String sBuddyText = AP_Win32App::s_fromUTF8ToWinLocale(buddyDesc.utf8_str());
			TV_INSERTSTRUCT tviBuddy;
			tviBuddy.item.mask = TVIF_TEXT| TVIF_STATE; // text only right now
			tviBuddy.item.stateMask = TVIS_BOLD|TVIS_EXPANDED;
			tviBuddy.hInsertAfter = TVI_LAST;  // only insert at the end			
			tviBuddy.hParent = NULL; // top most level Item
			tviBuddy.item.state = 0;
			tviBuddy.item.pszText = const_cast<char*>(sBuddyText.c_str());
			HTREEITEM htiBuddy = (HTREEITEM)SendMessage(m_hDocumentTreeview, TVM_INSERTITEM,0,(LPARAM)&tviBuddy);
			m_mTreeItemHandles.insert(std::pair<HTREEITEM, ShareListItem>(htiBuddy, ShareListItem(pBuddy, NULL)));
			
			// Loop through documents for each buddy
			for (const DocTreeItem* item = pBuddy->getDocTreeItems(); item; item = item->m_next)
			{
				UT_continue_if_fail(item->m_docHandle);
				UT_UTF8String docDesc = item->m_docHandle->getName();
				UT_DEBUGMSG(("Adding document (%s) to the treeview\n", docDesc.utf8_str()));
				
				UT_String sDocText = AP_Win32App::s_fromUTF8ToWinLocale(docDesc.utf8_str());
				TV_INSERTSTRUCT tviDocument;
				tviDocument.item.mask = TVIF_TEXT| TVIF_STATE; // text only right now
				tviDocument.item.stateMask = TVIS_BOLD|TVIS_EXPANDED;
				tviDocument.hInsertAfter = TVI_LAST;  // only insert at the end			
				tviDocument.hParent = htiBuddy;
				tviDocument.hInsertAfter = TVI_LAST;
				tviDocument.item.pszText = const_cast<char*>(sDocText.c_str());
				// if we are connected to this document, bold it.  Eventually checkboxes would be cooler, that's a TODO
				tviDocument.item.state = pManager->isActive(item->m_docHandle->getSessionId()) ? TVIS_BOLD : TVIS_EXPANDED;
				HTREEITEM htiDoc = (HTREEITEM)SendMessage(m_hDocumentTreeview, TVM_INSERTITEM, 0, (LPARAM)&tviDocument);
				m_mTreeItemHandles.insert(std::pair<HTREEITEM, ShareListItem>(htiDoc, ShareListItem(pBuddy, item->m_docHandle)));
			}
		}
	}
	
	// Now expand all of the buddies
	for (std::map< HTREEITEM, ShareListItem >::const_iterator cit = m_mTreeItemHandles.begin(); cit != m_mTreeItemHandles.end(); cit++)
	{
		UT_continue_if_fail(cit->first);
		if (!cit->second.pDocHandle)
			TreeView_Expand(m_hDocumentTreeview, cit->first, TVE_EXPAND);
	}
	
	_updateSelection();
}

void AP_Win32Dialog_CollaborationJoin::_refreshWindow()
{
	_setModel();
}

void AP_Win32Dialog_CollaborationJoin::_enableBuddyAddition(bool bEnabled)
{
	p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_ADDBUDDY_BUTTON, bEnabled);
}

void AP_Win32Dialog_CollaborationJoin::_updateSelection()
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);	
	
	HTREEITEM hSelItem = TreeView_GetSelection(m_hDocumentTreeview);
	if (hSelItem)
	{
		m_hSelected = hSelItem;

		std::map< HTREEITEM, ShareListItem >::const_iterator cit = m_mTreeItemHandles.find(hSelItem);
		UT_return_if_fail(cit != m_mTreeItemHandles.end());
		if (cit->second.pDocHandle)
		{
			UT_DEBUGMSG(("Document selected\n"));
			bool bIsConnected = pManager->isActive(cit->second.pDocHandle->getSessionId());
			p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_DISCONNECT_BUTTON, bIsConnected );
			p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_CONNECT_BUTTON, !bIsConnected );
			p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_DELETE_BUTTON, false);
		}
		else
		{
			UT_DEBUGMSG(("Buddy selected\n"));
			p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_DISCONNECT_BUTTON, false);
			p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_CONNECT_BUTTON, false);
			p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_DELETE_BUTTON, true);
		}
	}
	else
	{
		p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_DISCONNECT_BUTTON, false);
		p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_CONNECT_BUTTON, false);
		p_win32Dialog->enableControl(AP_RID_DIALOG_COLLABORATIONJOIN_DELETE_BUTTON, false);
	}
}

void AP_Win32Dialog_CollaborationJoin::_setJoin(HTREEITEM hItem, bool joinStatus)
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);

	std::map< HTREEITEM, ShareListItem >::const_iterator cit = m_mTreeItemHandles.find(hItem);
	UT_return_if_fail(cit != m_mTreeItemHandles.end());	

	const Buddy* pBuddy = cit->second.pBuddy;
	UT_return_if_fail(pBuddy);

	DocHandle* pDocHandle = cit->second.pDocHandle;
	UT_return_if_fail(pDocHandle);

	bool currentlyJoined = pManager->isActive(pDocHandle->getSessionId());
	UT_return_if_fail(joinStatus != currentlyJoined);
	
	if (joinStatus)
	{
		UT_DEBUGMSG(("Got a document we can connect to!\n"));
		m_answer = AP_Dialog_CollaborationJoin::a_CONNECT;
		m_pBuddy = pBuddy;
		m_pDocHandle = pDocHandle;
		return;
	}
	
	if (!joinStatus)
	{
		UT_DEBUGMSG(("Got a document we can disconnect from!\n"));
		m_answer = AP_Dialog_CollaborationJoin::a_DISCONNECT;
		m_pBuddy = pBuddy;
		m_pDocHandle = pDocHandle;
		return;
	}
}
