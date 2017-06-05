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

#ifndef AP_WIN32DIALOG_COLLABORATIONJOIN_H
#define AP_WIN32DIALOG_COLLABORATIONJOIN_H

#include <map>
#include <ut_string.h>
#include <windows.h>
#include <commctrl.h>

#include "ap_Win32Res_DlgCollaborationJoin.rc2"

#include <xp/ap_Dialog_CollaborationJoin.h>

class XAP_Frame;

struct ShareListItem
{
	ShareListItem(const Buddy* pBuddy_, DocHandle* pDocHandle_) : pBuddy(pBuddy_), pDocHandle(pDocHandle_) {};
	const Buddy* pBuddy;
	DocHandle* pDocHandle;
};

class AP_Win32Dialog_CollaborationJoin : public AP_Dialog_CollaborationJoin
{
public:
	AP_Win32Dialog_CollaborationJoin(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	static XAP_Dialog * 		static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id);

	void						runModal(XAP_Frame * pFrame);
	static BOOL CALLBACK		s_dlgProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	BOOL 						_onInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL 						_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL						_onNotify(HWND hWnd, WPARAM wParam, LPARAM lParam);

private:
	XAP_Win32DialogHelper *	p_win32Dialog;

	void					_setModel();
	void					_refreshWindow();
	void					_enableBuddyAddition(bool bEnabled);
	void					_updateSelection();
	void					_setJoin(HTREEITEM hItem, bool joinStatus);
	
	// Handles
	HINSTANCE 				m_hInstance;
	HWND					m_hDocumentTreeview;
	
	// Internal states
	HTREEITEM				m_hSelected;
	
	// Model
	std::map< HTREEITEM, ShareListItem > m_mTreeItemHandles;
};

#endif /* AP_WIN32DIALOG_COLLABORATIONJOIN_H */
