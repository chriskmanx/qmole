/* Copyright (C) 2006-2008 by Marc Maurer <uwog@uwog.net>
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

#include "xap_App.h"
#include "ap_UnixApp.h"
#include "xap_UnixApp.h"
#include "xap_Frame.h"
#include "xap_UnixDialogHelper.h"
#include "ut_string_class.h"
#include <xp/AbiCollabSessionManager.h>
#include <account/xp/Event.h>
#include <account/xp/AccountEvent.h>

#include "ap_UnixDialog_CollaborationJoin.h"

enum
{
	DESCRIPTION_COLUMN = 0,
	CONNECTED_COLUMN,
	DOCHANDLE_COLUMN,
	HANDLER_COLUMN,
	BUDDY_COLUMN,
	VISIBLE_COLUMN,
	NUM_COLUMNS
};

static void s_add_buddy_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationJoin * dlg)
{
	dlg->eventAddBuddy();
}

static void s_refresh_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationJoin * dlg)
{
	dlg->eventRefresh();
}

static void s_connect_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationJoin * dlg)
{
	dlg->eventConnect();
}

static void s_disconnect_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationJoin * dlg)
{
	dlg->eventDisconnect();
}

static void s_selection_changed(GtkTreeView *treeview, AP_UnixDialog_CollaborationJoin * dlg)
{
	UT_return_if_fail(treeview && dlg);
	dlg->eventSelectionChanged(treeview);
}

/*
static void joined_toggled (GtkCellRendererToggle *cell,
	      gchar                 *path_str,
	      gpointer               data)
{
	AP_UnixDialog_CollaborationJoin* pDlg = static_cast<AP_UnixDialog_CollaborationJoin*>(data);
	GtkTreeModel *model = GTK_TREE_MODEL(pDlg->getModel());
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);
	GtkTreeIter iter;
	
	gboolean joined;
	gpointer doc_handle;
	gpointer buddy;	

	// get the toggled state
	gtk_tree_model_get_iter (model, &iter, path);
	gtk_tree_model_get (model, &iter, CONNECTED_COLUMN, &joined, -1);
	gtk_tree_model_get (model, &iter, DOCHANDLE_COLUMN, &doc_handle, -1);
	gtk_tree_model_get (model, &iter, BUDDY_COLUMN, &buddy, -1);	

	// toggle the value
	joined = !joined;
	// set the new value
	gtk_tree_store_set (GTK_TREE_STORE (model), &iter, CONNECTED_COLUMN, joined, -1);

	// handle the joining/closing of the selected document
	pDlg->eventJoin(
			static_cast<Buddy*>(buddy), 
			static_cast<DocHandle*>(doc_handle), joined
		);

	// clean up
	gtk_tree_path_free (path);
}
*/

XAP_Dialog * AP_UnixDialog_CollaborationJoin::static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id)
{
	return static_cast<XAP_Dialog *>(new AP_UnixDialog_CollaborationJoin(pFactory, id));
}
pt2Constructor ap_Dialog_CollaborationJoin_Constructor = &AP_UnixDialog_CollaborationJoin::static_constructor;

AP_UnixDialog_CollaborationJoin::AP_UnixDialog_CollaborationJoin(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: AP_Dialog_CollaborationJoin(pDlgFactory, id),
	m_wWindowMain(NULL),
	m_wAddBuddy(NULL),
	m_wDeleteBuddy(NULL),
	m_wModel(NULL),
	m_wBuddyTree(NULL),
	m_wConnect(NULL),
	m_wDisconnect(NULL)
{
}

void AP_UnixDialog_CollaborationJoin::runModal(XAP_Frame * pFrame)
{
	UT_return_if_fail(pFrame);
	
    // Build the dialog's window
	m_wWindowMain = _constructWindow();
	UT_return_if_fail(m_wWindowMain);

	_populateWindowData();

	// refresh the contents, just to be up to date
	eventRefresh();
	
	switch ( abiRunModalDialog ( GTK_DIALOG(m_wWindowMain),
								 pFrame, this, GTK_RESPONSE_CLOSE, false ) )
	{
		case GTK_RESPONSE_YES: // ugly stock response abuse
			m_answer = AP_Dialog_CollaborationJoin::a_CONNECT;
			break;		
		case GTK_RESPONSE_NO: // ugly stock response abuse
			m_answer = AP_Dialog_CollaborationJoin::a_DISCONNECT;
			break;		
		case GTK_RESPONSE_CLOSE:
			m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
			break;
		default:
			m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
			break;
	}

	abiDestroyWidget(m_wWindowMain);
}

/*****************************************************************/
GtkWidget * AP_UnixDialog_CollaborationJoin::_constructWindow(void)
{
	GtkWidget* window;
	//const XAP_StringSet * pSS = XAP_App::getApp()->getStringSet();
	
	// get the path where our UI file is located
	std::string ui_path = static_cast<XAP_UnixApp*>(XAP_App::getApp())->getAbiSuiteAppUIDir() + "/ap_UnixDialog_CollaborationJoin.xml";
	// load the dialog from the UI file
	GtkBuilder* builder = gtk_builder_new();
	gtk_builder_add_from_file(builder, ui_path.c_str(), NULL);
	
	// Update our member variables with the important widgets that 
	// might need to be queried or altered later
	window = GTK_WIDGET(gtk_builder_get_object(builder, "ap_UnixDialog_CollaborationJoin"));
	m_wAddBuddy = GTK_WIDGET(gtk_builder_get_object(builder, "btAddBuddy"));
	m_wDeleteBuddy = GTK_WIDGET(gtk_builder_get_object(builder, "btDeleteBuddy"));
	m_wRefresh = GTK_WIDGET(gtk_builder_get_object(builder, "btRefresh"));
	m_wBuddyTree = GTK_WIDGET(gtk_builder_get_object(builder, "tvBuddies"));
	m_wConnect = GTK_WIDGET(gtk_builder_get_object(builder, "btConnect"));
	m_wDisconnect = GTK_WIDGET(gtk_builder_get_object(builder, "btDisconnect"));
	
	_refreshAccounts();
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	gtk_widget_set_sensitive(m_wAddBuddy, pManager->getAccounts().size() != 0); // TODO: fix this
	gtk_widget_set_sensitive(m_wDeleteBuddy, false); // TODO: implement this
	gtk_widget_set_sensitive(m_wRefresh, true);	
	gtk_widget_set_sensitive(m_wConnect, false);
	gtk_widget_set_sensitive(m_wDisconnect, false);

	// set the dialog title
	// TODO
	
	// localize the strings in our dialog, and set tags for some widgets
	// TODO

	// connect our signals
	g_signal_connect(G_OBJECT(m_wAddBuddy),
							"clicked",
							G_CALLBACK(s_add_buddy_clicked),
							static_cast<gpointer>(this));

	g_signal_connect(G_OBJECT(m_wRefresh),
							"clicked",
							G_CALLBACK(s_refresh_clicked),
							static_cast<gpointer>(this));
	
	g_signal_connect(G_OBJECT(m_wConnect),
							"clicked",
							G_CALLBACK(s_connect_clicked),
							static_cast<gpointer>(this));	

	g_signal_connect(G_OBJECT(m_wDisconnect),
							"clicked",
							G_CALLBACK(s_disconnect_clicked),
							static_cast<gpointer>(this));	
	
	g_signal_connect_after(G_OBJECT(m_wBuddyTree),
							"cursor-changed",
							G_CALLBACK(s_selection_changed),
							static_cast<gpointer>(this));

	g_object_unref(G_OBJECT(builder));
	return window;
}

void AP_UnixDialog_CollaborationJoin::_populateWindowData()
{
	GtkTreeSelection *sel;
	_setModel(_constructModel());
	
	gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (m_wBuddyTree), true);

	// get the current selection
	sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (m_wBuddyTree));
	gtk_tree_selection_set_mode (sel, GTK_SELECTION_BROWSE);
	//gtk_tree_selection_set_select_function (sel, tree_select_filter, NULL, NULL);
	
	
	gint col_offset;
	GtkTreeViewColumn* column;
	GtkCellRenderer *renderer;
	renderer = gtk_cell_renderer_text_new ();
	col_offset = gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (m_wBuddyTree), 
												-1,	"Buddy", 
												renderer, 
												"text", DESCRIPTION_COLUMN,
												(void*)NULL);
	column = gtk_tree_view_get_column (GTK_TREE_VIEW (m_wBuddyTree), col_offset - 1);	
	
	renderer = gtk_cell_renderer_toggle_new ();
	g_object_set (renderer, "xalign", 0.0, NULL);
	//g_signal_connect (renderer, "toggled", G_CALLBACK (joined_toggled), this);
	col_offset = gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (m_wBuddyTree), 
												-1,	"Connected", 
												renderer, 
												"active", CONNECTED_COLUMN,
												"visible", VISIBLE_COLUMN,
												(void*)NULL);
	column = gtk_tree_view_get_column (GTK_TREE_VIEW (m_wBuddyTree), col_offset - 1);												
	gtk_tree_view_column_set_fixed_width (GTK_TREE_VIEW_COLUMN (column), 50);
	gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

	gtk_tree_view_expand_all (GTK_TREE_VIEW (m_wBuddyTree));
	gtk_widget_show_all(m_wBuddyTree);
}

GtkTreeStore* AP_UnixDialog_CollaborationJoin::_constructModel()
{
	GtkTreeIter iter;
	GtkTreeStore* model = gtk_tree_store_new (NUM_COLUMNS, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_POINTER, G_TYPE_UINT, G_TYPE_UINT, G_TYPE_BOOLEAN);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	const std::vector<AccountHandler*>& accounts = pManager->getAccounts();

	for (UT_uint32 i = 0; i < accounts.size(); i++)
	{
		UT_DEBUGMSG(("Getting buddies for account: %s of type %s\n", 
				accounts[i]->getDescription().utf8_str(), 
				accounts[i]->getDisplayType().utf8_str()
			));
			
		// add all buddies belonging to this account
		for (UT_uint32 j = 0; j < accounts[i]->getBuddies().size(); j++)
		{
			BuddyPtr pBuddy = accounts[i]->getBuddies()[j];
			UT_continue_if_fail(pBuddy);

			gtk_tree_store_append (model, &iter, NULL);
			gtk_tree_store_set (model, &iter, 
					DESCRIPTION_COLUMN, pBuddy->getDescription().utf8_str(), 
					CONNECTED_COLUMN, false, /* joined */
					DOCHANDLE_COLUMN, 0, /* dochandle */
					HANDLER_COLUMN, 0, /* account handler */
					BUDDY_COLUMN, 0, /* buddy */
					VISIBLE_COLUMN, false, /* visibility */
					-1);
					
			// add all documents for this buddy
			GtkTreeIter child_iter;
			for (const DocTreeItem* item = pBuddy->getDocTreeItems(); item; item = item->m_next)
			{
				UT_continue_if_fail(item->m_docHandle);
				UT_DEBUGMSG(("DocHandle document name: %s\n", item->m_docHandle->getName().utf8_str()));
				
				// TODO: handle the DocTreeItem type
				gtk_tree_store_append (model, &child_iter, &iter);
				gtk_tree_store_set (model, &child_iter, 
						DESCRIPTION_COLUMN, (item->m_docHandle ? item->m_docHandle->getName().utf8_str() : "null"),
						CONNECTED_COLUMN, pManager->isActive(item->m_docHandle->getSessionId()),
						DOCHANDLE_COLUMN, item->m_docHandle,
						HANDLER_COLUMN, i,
						BUDDY_COLUMN, j,
						VISIBLE_COLUMN, true,
						-1);
			}
		}
	}
	return model;
}

void AP_UnixDialog_CollaborationJoin::_setModel(GtkTreeStore* model)
{
	// TODO: free the old model
	m_wModel = model;
	gtk_tree_view_set_model(GTK_TREE_VIEW (m_wBuddyTree), GTK_TREE_MODEL(m_wModel));
	gtk_tree_view_expand_all (GTK_TREE_VIEW (m_wBuddyTree));
	gtk_widget_show_all(m_wBuddyTree);	
}

void AP_UnixDialog_CollaborationJoin::_refreshWindow()
{
	_setModel(_constructModel());
}

void AP_UnixDialog_CollaborationJoin::_enableBuddyAddition(bool bEnabled)
{
	gtk_widget_set_sensitive(m_wAddBuddy, bEnabled);
}

void AP_UnixDialog_CollaborationJoin::eventAddBuddy()
{
	_eventAddBuddy();
	
	// Update the dialog
	// We should only refresh a view, not reload then entire contents
	_setModel(_constructModel());
}

void AP_UnixDialog_CollaborationJoin::eventRefresh()
{
	// TODO: we really should refresh the buddies here as well, 
	// as they could pop up automatically as well (for example with a 
	// avahi backend)
	_refreshAllDocHandlesAsync();
}

// FIXME: share code with eventDisconnect()
void AP_UnixDialog_CollaborationJoin::eventConnect()
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationJoin::eventConnect()\n"));
	
	GtkTreeSelection * selection;
	GtkTreeIter iter;
	GtkTreeModel * model;	
	
	selection = gtk_tree_view_get_selection( GTK_TREE_VIEW(m_wBuddyTree) );
	if (!selection || !gtk_tree_selection_get_selected (selection, &model, &iter))
	{
		m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
		return;
	}
	
	// see if we selected a row that has a document that we can still join
	
	// get the row data
	gboolean connected;
	gpointer doc_handle;
	guint handler_idx;	
	guint buddy_idx;	

	gtk_tree_model_get (model, &iter, CONNECTED_COLUMN, &connected, -1);
	gtk_tree_model_get (model, &iter, DOCHANDLE_COLUMN, &doc_handle, -1);
	gtk_tree_model_get (model, &iter, HANDLER_COLUMN, &handler_idx, -1);	
	gtk_tree_model_get (model, &iter, BUDDY_COLUMN, &buddy_idx, -1);
	
	if (!doc_handle || connected)
	{
		UT_DEBUGMSG(("Not a document we can join\n"));
		m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
		return;
	}

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	const std::vector<AccountHandler*>& accounts = pManager->getAccounts();
	if (handler_idx >= accounts.size() || buddy_idx >= accounts[handler_idx]->getBuddies().size())
	{
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
		m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
		return;
	}
	
	UT_DEBUGMSG(("Got a document we can connect to!\n"));
	m_answer = AP_Dialog_CollaborationJoin::a_CONNECT;
	m_pBuddy = accounts[handler_idx]->getBuddies()[buddy_idx];
	m_pDocHandle = reinterpret_cast<DocHandle*>(doc_handle);
}

// FIXME: share code with eventConnect()
void AP_UnixDialog_CollaborationJoin::eventDisconnect()
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationJoin::eventDisconnect()\n"));
	
	GtkTreeSelection * selection;
	GtkTreeIter iter;
	GtkTreeModel * model;	
	
	selection = gtk_tree_view_get_selection( GTK_TREE_VIEW(m_wBuddyTree) );
	if (!selection || !gtk_tree_selection_get_selected (selection, &model, &iter))
	{
		m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
		return;
	}
	
	// see if we selected a row that has a document that we can still join
	
	// get the row data
	gboolean connected;
	gpointer doc_handle;
	guint handler_idx;	
	guint buddy_idx;	

	gtk_tree_model_get (model, &iter, CONNECTED_COLUMN, &connected, -1);
	gtk_tree_model_get (model, &iter, DOCHANDLE_COLUMN, &doc_handle, -1);
	gtk_tree_model_get (model, &iter, HANDLER_COLUMN, &handler_idx, -1);	
	gtk_tree_model_get (model, &iter, BUDDY_COLUMN, &buddy_idx, -1);
	
	if (!doc_handle || !connected)
	{
		UT_DEBUGMSG(("Not a document we can disconnect from\n"));
		m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
		return;
	}

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	const std::vector<AccountHandler*>& accounts = pManager->getAccounts();
	if (handler_idx >= accounts.size() || buddy_idx >= accounts[handler_idx]->getBuddies().size())
	{
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
		m_answer = AP_Dialog_CollaborationJoin::a_CLOSE;
		return;
	}
	
	UT_DEBUGMSG(("Got a document we can disconnect from!\n"));
	m_answer = AP_Dialog_CollaborationJoin::a_DISCONNECT;
	m_pBuddy = accounts[handler_idx]->getBuddies()[buddy_idx];
	m_pDocHandle = reinterpret_cast<DocHandle*>(doc_handle);
}

void AP_UnixDialog_CollaborationJoin::eventSelectionChanged(GtkTreeView *treeview)
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationJoin::eventSelectionChanged()\n"));
	
	GtkTreeSelection * selection;
	GtkTreeIter iter;
	GtkTreeModel * model;
	
	selection = gtk_tree_view_get_selection( GTK_TREE_VIEW(treeview) );
	if (!selection || !gtk_tree_selection_get_selected (selection, &model, &iter))
	{
		gtk_widget_set_sensitive(m_wConnect, false);
		gtk_widget_set_sensitive(m_wDisconnect, false);
		return;
	}
	
	// see if we selected a row that has a document that we can still join
	
	// get the row data
	gboolean connected;
	gpointer doc_handle;

	gtk_tree_model_get (model, &iter, CONNECTED_COLUMN, &connected, -1);
	gtk_tree_model_get (model, &iter, DOCHANDLE_COLUMN, &doc_handle, -1);	

	if (!doc_handle)
	{
		UT_DEBUGMSG(("Not a document\n"));
		gtk_widget_set_sensitive(m_wConnect, false);
		gtk_widget_set_sensitive(m_wDisconnect, false);
		return;
	}

	gtk_widget_set_sensitive(m_wConnect, !connected);
	gtk_widget_set_sensitive(m_wDisconnect, connected);
}
