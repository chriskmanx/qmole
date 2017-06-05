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

#ifndef AP_WIN32DIALOG_COLLABORATIONACCOUNTS_H
#define AP_WIN32DIALOG_COLLABORATIONACCOUNTS_H

#include <map>
#include <ut_string.h>
#include "ap_Win32Dialog_CollaborationAddAccount.h"

#include <xp/ap_Dialog_CollaborationAccounts.h>
#include "ap_Win32Res_DlgCollaborationAccounts.rc2"

class XAP_Frame;

class AP_Win32Dialog_CollaborationAccounts : public AP_Dialog_CollaborationAccounts
{
public:
	AP_Win32Dialog_CollaborationAccounts(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	
	static XAP_Dialog * 	static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id);
	void					runModal(XAP_Frame * pFrame);
	static BOOL CALLBACK	s_dlgProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
	BOOL 					_onInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL 					_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam);

	virtual void			signal(const Event& event, const Buddy* pSource);

private:
	std::map<UT_UTF8String, AccountHandler*> _constructModel();
	void					_setModel(std::map<UT_UTF8String, AccountHandler*>  model);
	void					setOnline(AccountHandler* pHandler, bool online);
	void					_updateSelection();
	
	XAP_Win32DialogHelper *	m_pWin32Dialog;
	HINSTANCE 				m_hInstance;
	
	// Internal states
	BOOL					m_bHasSelection;
	int						m_iSelected;
	std::map<UT_UTF8String, AccountHandler*> m_mModel;
	std::map<int, AccountHandler*> m_mIndices;
	

};

#endif /* AP_WIN32DIALOG_COLLABORATIONACCOUNTS_H */
