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

#include "xap_App.h"
#include "ap_UnixApp.h"
#include "xap_UnixApp.h"
#include "xap_Frame.h"
#include "xap_UnixDialogHelper.h"
#include "ut_string_class.h"

#include "ap_UnixDialog_CollaborationAccounts.h"

enum
{
	DESC_COLUMN = 0,
	TYPE_COLUMN,
	ONLINE_COLUMN,
	HANDLER_COLUMN
};

static void s_add_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationAccounts * dlg)
{
	dlg->eventAdd();
}

static void s_properties_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationAccounts * dlg)
{
	dlg->eventProperties();
}

static void s_delete_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationAccounts * dlg)
{
	dlg->eventDelete();
}

static void s_account_selected(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationAccounts * dlg)
{
	dlg->eventSelectAccount();
}

static void s_online_toggled (GtkCellRendererToggle * /*cell*/,
	      gchar                 *path_str,
	      gpointer               data)
{
	AP_UnixDialog_CollaborationAccounts* pDlg = static_cast<AP_UnixDialog_CollaborationAccounts*>(data);
	
	GtkTreeModel *model = GTK_TREE_MODEL(pDlg->getModel());
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);
	GtkTreeIter iter;
	
	gboolean online = false;
	gpointer handler = 0;	

	// get the toggled state
	if (gtk_tree_model_get_iter(model, &iter, path))
	{
		gtk_tree_model_get(model, &iter, ONLINE_COLUMN, &online, -1);
		gtk_tree_model_get(model, &iter, HANDLER_COLUMN, &handler, -1);

		// toggle the value
		// NOTE: don't actually toggle the value here: an async event will come in when we are actually connected!
		//online = !online; 
		//gtk_list_store_set (GTK_LIST_STORE (model), &iter, ONLINE_COLUMN, online, -1);

		// handle the joining/closing of the selected document
		pDlg->eventOnline(reinterpret_cast<AccountHandler*>(handler), !online);
	}
	else
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

	// clean up
	gtk_tree_path_free (path);
}

XAP_Dialog * AP_UnixDialog_CollaborationAccounts::static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id)
{
	return static_cast<XAP_Dialog *>(new AP_UnixDialog_CollaborationAccounts(pFactory, id));
}
pt2Constructor ap_Dialog_CollaborationAccounts_Constructor = &AP_UnixDialog_CollaborationAccounts::static_constructor;

AP_UnixDialog_CollaborationAccounts::AP_UnixDialog_CollaborationAccounts(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: AP_Dialog_CollaborationAccounts(pDlgFactory, id),
	m_wWindowMain(NULL),
	m_wAdd(NULL),
	m_wProperties(NULL),
	m_wDelete(NULL),
	m_wRenderer(NULL),
	m_wToggleRenderer(NULL),
	m_wModel(NULL),
	m_wAccountsTree(NULL)
{
}

void AP_UnixDialog_CollaborationAccounts::runModal(XAP_Frame * pFrame)
{
	UT_return_if_fail(pFrame);
	
    // Build the dialog's window
	m_wWindowMain = _constructWindow();
	UT_return_if_fail(m_wWindowMain);

	_populateWindowData();

	switch ( abiRunModalDialog ( GTK_DIALOG(m_wWindowMain),
								 pFrame, this, GTK_RESPONSE_CLOSE, false ) )
	{
		case GTK_RESPONSE_CLOSE:
			m_answer = AP_Dialog_CollaborationAccounts::a_CLOSE;
			break;
		default:
			m_answer = AP_Dialog_CollaborationAccounts::a_CLOSE;
			break;
	}

	abiDestroyWidget(m_wWindowMain);
}

/*****************************************************************/
GtkWidget * AP_UnixDialog_CollaborationAccounts::_constructWindow(void)
{
	GtkWidget* window;
	//const XAP_StringSet * pSS = XAP_App::getApp()->getStringSet();
	
	// get the path where our UI file is located
	std::string ui_path = static_cast<XAP_UnixApp*>(XAP_App::getApp())->getAbiSuiteAppUIDir() + "/ap_UnixDialog_CollaborationAccounts.xml";
	// load the dialog from the UI file
	GtkBuilder* builder = gtk_builder_new();
	gtk_builder_add_from_file(builder, ui_path.c_str(), NULL);
	
	// Update our member variables with the important widgets that 
	// might need to be queried or altered later
	window = GTK_WIDGET(gtk_builder_get_object(builder, "ap_UnixDialog_CollaborationAccounts"));
	m_wAdd = GTK_WIDGET(gtk_builder_get_object(builder, "btAdd"));
	m_wProperties = GTK_WIDGET(gtk_builder_get_object(builder, "btProperties"));
	m_wDelete = GTK_WIDGET(gtk_builder_get_object(builder, "btDelete"));
	m_wAccountsTree = GTK_WIDGET(gtk_builder_get_object(builder, "tvAccounts"));

	// set the dialog title
	// TODO
	
	// localize the strings in our dialog, and set tags for some widgets
	// TODO

	// connect our signals
	g_signal_connect(G_OBJECT(m_wAdd),
							"clicked",
							G_CALLBACK(s_add_clicked),
							static_cast<gpointer>(this));

	g_signal_connect(G_OBJECT(m_wProperties),
							"clicked",
							G_CALLBACK(s_properties_clicked),
							static_cast<gpointer>(this));	
	
	g_signal_connect(G_OBJECT(m_wDelete),
							"clicked",
							G_CALLBACK(s_delete_clicked),
							static_cast<gpointer>(this));	

							
	g_signal_connect(G_OBJECT(m_wAccountsTree),
							"cursor-changed",
							G_CALLBACK(s_account_selected),
							static_cast<gpointer>(this));

	g_object_unref (G_OBJECT(builder));
	
	return window;
}

void AP_UnixDialog_CollaborationAccounts::_populateWindowData()
{
	GtkTreeSelection *sel;
	_setModel(_constructModel());
	
	gtk_tree_view_set_rules_hint (GTK_TREE_VIEW (m_wAccountsTree), true);

	// get the current selection
	sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (m_wAccountsTree));
	gtk_tree_selection_set_mode (sel, GTK_SELECTION_BROWSE);
	
	m_wRenderer = gtk_cell_renderer_text_new ();
	gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (m_wAccountsTree), 
												-1,
												"Account", 
												m_wRenderer, "text", 0, (void*)NULL);
	gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (m_wAccountsTree), 
												-1,
												"Type", 
												m_wRenderer, "text", 1, (void*)NULL);
	
	m_wToggleRenderer = gtk_cell_renderer_toggle_new ();
	g_object_set (m_wToggleRenderer, "xalign", 0.0, NULL);
	g_signal_connect (m_wToggleRenderer, "toggled", G_CALLBACK (s_online_toggled), this);
	gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (m_wAccountsTree), 
												-1,	"Online", 
												m_wToggleRenderer, 
												"active",
												2,
												(void*)NULL);
	
	gtk_tree_view_expand_all (GTK_TREE_VIEW (m_wAccountsTree));
	gtk_widget_show_all(m_wAccountsTree);
}

GtkListStore* AP_UnixDialog_CollaborationAccounts::_constructModel()
{
	GtkTreeIter iter;
	GtkListStore* model = gtk_list_store_new (4, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_POINTER);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	for (UT_uint32 i = 0; i < pManager->getAccounts().size(); i++)
	{
		AccountHandler* pHandler = pManager->getAccounts()[i];
		if (pHandler)
		{
			UT_DEBUGMSG(("Got account: %s of type %s\n", 
					pHandler->getDescription().utf8_str(), 
					pHandler->getDisplayType().utf8_str()
				));
			
			gtk_list_store_append (model, &iter);	
			gtk_list_store_set (model, &iter, 
					DESC_COLUMN, pHandler->getDescription().utf8_str(), 
					TYPE_COLUMN, pHandler->getDisplayType().utf8_str(), 
					ONLINE_COLUMN, pHandler->isOnline(), 
					HANDLER_COLUMN, pHandler,
					-1);
		}
	}
	
	return model;
}

void AP_UnixDialog_CollaborationAccounts::_setModel(GtkListStore* model)
{
	// TODO: free the old model
	m_wModel = model;
	gtk_tree_view_set_model(GTK_TREE_VIEW (m_wAccountsTree), GTK_TREE_MODEL(m_wModel));
	gtk_widget_show_all(m_wAccountsTree);
	eventSelectAccount();
}

void AP_UnixDialog_CollaborationAccounts::eventAdd()
{
	createNewAccount();
	// TODO: only update the dialog when an entry has been added
	_setModel(_constructModel());
}

void AP_UnixDialog_CollaborationAccounts::eventProperties()
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationAccounts::eventProperties()\n"));

	GtkTreeIter iter;
	GtkTreeSelection* selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(m_wAccountsTree));
	
	bool hasSelection = gtk_tree_selection_get_selected (selection, 0, &iter);
	if (hasSelection)
	{
		UT_DEBUGMSG(("An account handler is selected!\n"));
	}
	else
	{
		UT_DEBUGMSG(("No account handler selected!\n"));
	}
}

void AP_UnixDialog_CollaborationAccounts::eventDelete()
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationAccounts::eventDelete()\n"));

	GtkTreeIter iter;
	GtkTreeSelection* selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(m_wAccountsTree));
	
	bool hasSelection = gtk_tree_selection_get_selected (selection, 0, &iter);
	if (hasSelection)
	{
//		gchar * str_desc_data;
//		gchar * str_type_data;
		gpointer* ptr_data;
		AccountHandler* pHandler = 0;

		gtk_tree_model_get (GTK_TREE_MODEL(m_wModel), &iter, 
                          HANDLER_COLUMN, &ptr_data,
                          -1);		
		
		pHandler = reinterpret_cast<AccountHandler*>(ptr_data);
		if (pHandler)
		{
			// TODO: we should ask for confirmation, as this account handler
			//		 could be in use by serveral AbiCollab Sessions
			UT_DEBUGMSG(("Delete account: %s of type %s\n", 
					pHandler->getDescription().utf8_str(), 
					pHandler->getDisplayType().utf8_str()
				));
			
			_deleteAccount(pHandler);
			
			// for now, recreate the whole model; but we should really just delete
			// the iter we got above
			_setModel(_constructModel());
		}
	}
	else
	{
		UT_DEBUGMSG(("No account handler selected!\n"));
	}
}

void AP_UnixDialog_CollaborationAccounts::eventSelectAccount()
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationAccounts::eventSelectAccount()\n"));
	GtkTreeSelection* selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(m_wAccountsTree));
	bool hasSelection = gtk_tree_selection_get_selected (selection, 0, 0);
	gtk_widget_set_sensitive(m_wProperties, /*hasSelection*/ false); // disable this button until we implement it
	gtk_widget_set_sensitive(m_wDelete, hasSelection);
}

void AP_UnixDialog_CollaborationAccounts::eventOnline(AccountHandler* pHandler, bool online)
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationAccounts::eventOnline()\n"));
	UT_return_if_fail(pHandler);
	
	if (online)
	{
		if (!pHandler->isOnline())
			pHandler->connect();
	}
	else
	{
		if (pHandler->isOnline())
			pHandler->disconnect();
	}
}

void AP_UnixDialog_CollaborationAccounts::signal(const Event& event, BuddyPtr /*pSource*/)
{
	UT_DEBUGMSG(("AP_UnixDialog_CollaborationAccounts::signal()\n"));
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
