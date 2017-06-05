/* AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 by Ryan Pavlik <abiryan@ryand.net>
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

#include <boost/lexical_cast.hpp>
#include "TCPWin32AccountHandler.h"

#define ABI_RID_DIALOG_COLLABTCP_SERVERENTRY 201
#define ABI_RID_DIALOG_COLLABTCP_PORTENTRY 202
#define ABI_RID_DIALOG_COLLABTCP_SERVERLABEL 203
#define ABI_RID_DIALOG_COLLABTCP_PORTLABEL 204
#define ABI_RID_DIALOG_COLLABTCP_SERVERRADIO 205
#define ABI_RID_DIALOG_COLLABTCP_JOINRADIO 206
#define ABI_RID_DIALOG_COLLABTCP_AUTOCONNECTCHECK 207
#define ABI_RID_DIALOG_COLLABTCP_USESECURECHECK 208

AccountHandlerConstructor TCPAccountHandlerConstructor = &TCPWin32AccountHandler::static_constructor;

AccountHandler * TCPWin32AccountHandler::static_constructor()
{
	return static_cast<AccountHandler *>(new TCPWin32AccountHandler());
}

// return true if we process the command, false otherwise
BOOL TCPWin32AccountHandler::_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	WORD wId = LOWORD(wParam);
	
	bool serve = _isCheckedHwnd(m_hServerRadio);
	AP_Win32Dialog_CollaborationAddAccount * pThis;
	
	switch (wId)
	{
	// Switch on resource ID's - where do they come from for these controls?
	case ABI_RID_DIALOG_COLLABTCP_SERVERRADIO:
	case ABI_RID_DIALOG_COLLABTCP_JOINRADIO:
		// These have the ability to enable/disable input controls
		// By their doing so, they can also enable/disable the "OK" button
		// which is why there is intentionally no break statement following this section.
		if (serve)
		{
			// disable the address entry
			EnableWindow(m_hServerEntry, false);
		}
		else
		{
			// enable the address entry
			EnableWindow(m_hServerEntry, true);
		}
		return true;
		
	case ABI_RID_DIALOG_COLLABTCP_SERVERENTRY:
	case ABI_RID_DIALOG_COLLABTCP_PORTENTRY:
		// These have the ability to enable or disable the "OK" button
		
		pThis = (AP_Win32Dialog_CollaborationAddAccount *)GetWindowLong(m_hParentDlg,DWL_USER);
		
		// note: GetWindowTextLength may have a Unicode caveat, according to MSDN
		// It may return a value higher than the actual length.  This is probably OK for
		// our purposes here.
		if (GetWindowTextLength(m_hPortEntry)>0)
		{
			// well, we have at least something in the port box.
			// TODO: Input validation would be cool, but oh well.
			
			if (serve)
			{
				// All we need is a port.  We are cleared for enabling the OK button.
				pThis->setBackendValidity(true);
			}
			else
			{
				// We are connecting to a server: must have a server listed.
				if (GetWindowTextLength(m_hServerEntry)>0)
				{
					// looks good, enable OK
					pThis->setBackendValidity(true);
				}
				else
				{
					// No server entered.
					pThis->setBackendValidity(false);
				}
			}
		}
		else
		{
			// we have no port - disable the OK button!
			pThis->setBackendValidity(false);
		}
		return true;
	
	default:
		return false;
	}
}


TCPWin32AccountHandler::TCPWin32AccountHandler()
	: TCPAccountHandler(),
	m_pWin32Dialog(NULL),
	m_hInstance(NULL),
	m_hServerEntry(NULL),
	m_hPortEntry(NULL),
	m_hServerRadio(NULL),
	m_hJoinRadio(NULL),
	m_hServerLabel(NULL),
	m_hPortLabel(NULL),
	m_hAutoconnectCheck(NULL),
	m_hUseSecureCheck(NULL),
	m_hParentDlg(NULL)
{
	AbiCollabSessionManager * pSessionManager = AbiCollabSessionManager::getManager();
	if (pSessionManager)
	{
		m_hInstance = pSessionManager->getInstance();
	}
}

void TCPWin32AccountHandler::embedDialogWidgets(void* pEmbeddingParent)
{
	UT_DEBUGMSG(("TCPWin32AccountHandler::embedDialogWidgets()\n"));
	UT_return_if_fail(pEmbeddingParent);

	// our enclosing window must be a HWND
	HWND hBox = reinterpret_cast<HWND>(pEmbeddingParent);
	
	/* Non-Tabbable Labels */
	
	// "Address:"
	m_hServerLabel = CreateWindowEx(WS_EX_NOPARENTNOTIFY, "STATIC", "Address:", SS_LEFT | WS_CHILD | WS_VISIBLE | WS_GROUP,
	15, 57, 51, 15, hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_SERVERLABEL,  m_hInstance, 0);
	UT_return_if_fail(m_hServerLabel);
	
	// "Port:"
	m_hPortLabel = CreateWindowEx(WS_EX_NOPARENTNOTIFY, "STATIC", "Port:", SS_LEFT | WS_CHILD | WS_VISIBLE | WS_GROUP, 
	15, 87, 47, 15, hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_PORTLABEL,  m_hInstance, 0);
	UT_return_if_fail(m_hPortLabel);
	
	/* Radio Button Group */
	// Act As Server radio button
	m_hServerRadio = CreateWindowEx(WS_EX_NOPARENTNOTIFY |  WS_EX_TRANSPARENT, "BUTTON", "Accept Incoming Connections", BS_NOTIFY | BS_AUTORADIOBUTTON | WS_CHILD | WS_VISIBLE |WS_GROUP|WS_TABSTOP,
	15, 15, 200, 15, hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_SERVERRADIO,  m_hInstance, 0);
	UT_return_if_fail(m_hServerRadio);
	
	// Join a Server radio button
	m_hJoinRadio = CreateWindowEx(WS_EX_NOPARENTNOTIFY |  WS_EX_TRANSPARENT, "BUTTON", "Connect to Another Computer", BS_NOTIFY  | BS_AUTORADIOBUTTON | WS_CHILD | WS_VISIBLE,
	15, 35, 200, 15, hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_JOINRADIO,  m_hInstance, 0);
	UT_return_if_fail(m_hJoinRadio);
	
	/* Tabbable */
	// Entry box for IP address of server
	// should be disabled when in server mode, and OK should be disabled when this is empty in join mode
	m_hServerEntry = CreateWindowEx(WS_EX_NOPARENTNOTIFY | WS_EX_CLIENTEDGE, "EDIT", "", ES_LEFT | ES_AUTOHSCROLL | WS_CHILD | WS_BORDER | WS_VISIBLE | WS_TABSTOP | WS_GROUP,
	80, 55, 121, 20, hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_SERVERENTRY,  m_hInstance, 0);
	UT_return_if_fail(m_hServerEntry);
	
	// Port entry box
	// OK should be disabled when this is empty
	m_hPortEntry = CreateWindowEx(WS_EX_NOPARENTNOTIFY | WS_EX_CLIENTEDGE, "EDIT", boost::lexical_cast<std::string>(DEFAULT_TCP_PORT).c_str(), ES_LEFT | WS_CHILD | WS_BORDER | WS_VISIBLE | WS_TABSTOP| WS_GROUP,
	80, 85, 60, 20, hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_PORTENTRY,  m_hInstance, 0);
	UT_return_if_fail(m_hPortEntry);
	
	// Checkbox for future secure support - currently disabled
	m_hUseSecureCheck = CreateWindowEx(WS_EX_NOPARENTNOTIFY, "BUTTON", "Use Secure Connection", BS_CHECKBOX | BS_AUTOCHECKBOX | WS_CHILD | WS_VISIBLE | WS_DISABLED | WS_TABSTOP | WS_GROUP,
	14, 115, 174, 15, hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_USESECURECHECK,  m_hInstance, 0);
	UT_return_if_fail(m_hUseSecureCheck);
	
	// Checkbox for Connect on Startup
	m_hAutoconnectCheck = CreateWindowEx(WS_EX_NOPARENTNOTIFY, "BUTTON", "Connect Automatically", BS_CHECKBOX | BS_AUTOCHECKBOX | WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_GROUP,
	14, 135, 172, 15 , hBox,  (HMENU) ABI_RID_DIALOG_COLLABTCP_AUTOCONNECTCHECK,  m_hInstance, 0);
	UT_return_if_fail(m_hAutoconnectCheck);
	
	// Font setting code borrowed from XAP_Win32Dlg_About
	LOGFONT lf = { 0 };
	strcpy(lf.lfFaceName, "MS Sans Serif");
	lf.lfHeight = 12;
	lf.lfWeight = 0;
	HFONT hfontPrimary = CreateFontIndirect(&lf);
	HWND rgFontReceivers[] =
		{ m_hServerRadio, m_hJoinRadio, m_hServerLabel, m_hPortLabel, m_hServerEntry, m_hPortEntry, m_hAutoconnectCheck, m_hUseSecureCheck};
	for (UT_uint32 iWnd = 0; iWnd < G_N_ELEMENTS(rgFontReceivers); iWnd++)
		SendMessage(rgFontReceivers[iWnd], WM_SETFONT, (WPARAM) hfontPrimary, 0);
	
	// default to serve
	_checkButtonHwnd(m_hServerRadio, true);
	
	// default to autoconnect
	_checkButtonHwnd(m_hAutoconnectCheck, true);
}

#define DESTROY_WINDOW(M) if (M) { UT_ASSERT_HARMLESS(DestroyWindow(M)); M = 0; }

void TCPWin32AccountHandler::removeDialogWidgets(void* pEmbeddingParent)
{
	UT_DEBUGMSG(("TCPAccountHandler::removeDialogWidgets()\n"));
	UT_return_if_fail(pEmbeddingParent);

	// our enclosing window must be a HWND
	HWND hBox = reinterpret_cast<HWND>(pEmbeddingParent);
	
	DESTROY_WINDOW(m_hServerEntry);
	DESTROY_WINDOW(m_hPortEntry);
	DESTROY_WINDOW(m_hServerLabel);
	DESTROY_WINDOW(m_hPortLabel);
	DESTROY_WINDOW(m_hServerRadio);
	DESTROY_WINDOW(m_hJoinRadio);
	DESTROY_WINDOW(m_hAutoconnectCheck);
	DESTROY_WINDOW(m_hUseSecureCheck);
	
	// destroying a window does not repaint the area it owned, so do it here
	RedrawWindow(GetParent(hBox), NULL, NULL, RDW_ERASE | RDW_INVALIDATE | RDW_UPDATENOW);
}

void TCPWin32AccountHandler::storeProperties()
{
	UT_DEBUGMSG(("TCPWin32AccountHandler::storeProperties()\n"));
	char sServer[256];
	memset(sServer, 0, 256);
	
	char sPort[8];
	memset(sPort, 0, 8);
	
	bool serve = _isCheckedHwnd(m_hServerRadio);
	if (!serve)
	{
		// joining someone else's server
		if (m_hServerEntry)
		{
			
			if (_getControlTextHwnd(m_hServerEntry, 256, sServer) != -1)
			{
				sServer[255]=0;
				addProperty("server", sServer);
			}
		}
	}
	
	if (m_hPortEntry)
	{
		if (_getControlTextHwnd(m_hPortEntry, 8, sPort) != -1)
		{
			sPort[7]=0;
			addProperty("port", sPort);
		}
	}
	if (m_hAutoconnectCheck)
	{
		addProperty("autoconnect", _isCheckedHwnd(m_hAutoconnectCheck) ? "true" : "false" );
	}

}

// Win32 Helper Functions
// Since these controls aren't in a dialog resource file, the DialogHelper is no use, nor the pretty parts of the win api
// we must do some raw sendmessage instead.
void TCPWin32AccountHandler::_checkButtonHwnd(HWND hCtrl, bool bChecked)
{
	UT_return_if_fail(hCtrl);
	SendMessage(hCtrl, BM_SETCHECK, bChecked ? BST_CHECKED : BST_UNCHECKED, 0);
}

bool TCPWin32AccountHandler::_isCheckedHwnd(HWND hCtrl)
{
	// note - does not work for 3-state buttons, obviously, since it returns a bool
	UT_return_val_if_fail(hCtrl, false);
	return (SendMessage(hCtrl, BM_GETCHECK, 0, 0) == BST_CHECKED);
}

int TCPWin32AccountHandler::_getControlTextHwnd(HWND hCtrl, int iLen, const char * p_szBuf)
{
	UT_return_val_if_fail(hCtrl, -1);
	return SendMessage(hCtrl, WM_GETTEXT, iLen, (LPARAM) p_szBuf);
}
