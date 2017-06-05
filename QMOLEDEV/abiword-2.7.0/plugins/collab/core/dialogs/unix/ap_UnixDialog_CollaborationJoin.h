/* AbiCollab- Code to enable the modification of remote documents.
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

#ifndef AP_UNIXDIALOG_COLLABORATIONJOIN_H
#define AP_UNIXDIALOG_COLLABORATIONJOIN_H

#include <gtk/gtk.h>
#include <xp/ap_Dialog_CollaborationJoin.h>

class XAP_Frame;

class AP_UnixDialog_CollaborationJoin : public AP_Dialog_CollaborationJoin
{
public:
	AP_UnixDialog_CollaborationJoin(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	static XAP_Dialog * static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id);
	void				runModal(XAP_Frame * pFrame);

	void				eventAddBuddy();
	void				eventRefresh();
	void				eventConnect();
	void				eventDisconnect();
	void				eventSelectionChanged(GtkTreeView *treeview);


	GtkTreeStore*		getModel()
		{ return m_wModel; }

private:
	GtkWidget*	 		_constructWindow(void);
	void				_populateWindowData(void);
	GtkTreeStore*		_constructModel();
	void				_setModel(GtkTreeStore* model);
	void				_refreshWindow();
	void				_enableBuddyAddition(bool bEnabled);

	GtkWidget*			m_wWindowMain;
	GtkWidget*			m_wAddBuddy;
	GtkWidget*			m_wDeleteBuddy;
	GtkWidget*			m_wRefresh;	
	GtkTreeStore*		m_wModel;
	GtkWidget *			m_wBuddyTree;
	GtkWidget*			m_wConnect;
	GtkWidget*			m_wDisconnect;
};

#endif /* AP_UNIXDIALOG_COLLABORATIONJOIN_H */
