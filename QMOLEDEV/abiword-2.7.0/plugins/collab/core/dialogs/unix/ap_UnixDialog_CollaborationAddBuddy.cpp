/* Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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

#include "ap_UnixDialog_CollaborationAddBuddy.h"

enum
{
	DESC_COLUMN = 0,
	HANDLER_COLUMN
};

static void s_ok_clicked(GtkWidget * /*wid*/, AP_UnixDialog_CollaborationAddBuddy * dlg)
{
	dlg->event_Ok();
}

XAP_Dialog * AP_UnixDialog_CollaborationAddBuddy::static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id)
{
	return static_cast<XAP_Dialog *>(new AP_UnixDialog_CollaborationAddBuddy(pFactory, id));
}
pt2Constructor ap_Dialog_CollaborationAddBuddy_Constructor = &AP_UnixDialog_CollaborationAddBuddy::static_constructor;

AP_UnixDialog_CollaborationAddBuddy::AP_UnixDialog_CollaborationAddBuddy(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: AP_Dialog_CollaborationAddBuddy(pDlgFactory, id),
	m_wWindowMain(NULL),
	m_wOk(NULL),
	m_pAccount(NULL)
{
}

void AP_UnixDialog_CollaborationAddBuddy::runModal(XAP_Frame * pFrame)
{
	UT_return_if_fail(pFrame);
	
    // Build the dialog's window
	m_wWindowMain = _constructWindow();
	UT_return_if_fail(m_wWindowMain);

	_populateWindowData();

	switch ( abiRunModalDialog ( GTK_DIALOG(m_wWindowMain),
								 pFrame, this, GTK_RESPONSE_CANCEL, false ) )
	{
		case GTK_RESPONSE_CANCEL:
			m_answer = AP_UnixDialog_CollaborationAddBuddy::a_CANCEL;
			break;
		case GTK_RESPONSE_OK:
			m_answer = AP_UnixDialog_CollaborationAddBuddy::a_OK;
			break;			
		default:
			m_answer = AP_UnixDialog_CollaborationAddBuddy::a_CANCEL;
			break;
	}

	abiDestroyWidget(m_wWindowMain);
}

/*****************************************************************/
GtkWidget * AP_UnixDialog_CollaborationAddBuddy::_constructWindow(void)
{
	GtkWidget* window;
	//const XAP_StringSet * pSS = XAP_App::getApp()->getStringSet();
	
	// get the path where our UI file is located
	std::string ui_path = static_cast<XAP_UnixApp*>(XAP_App::getApp())->getAbiSuiteAppUIDir() + "/ap_UnixDialog_CollaborationAddBuddy.xml";
	// load the dialog from the UI file
	GtkBuilder* builder = gtk_builder_new();
	gtk_builder_add_from_file(builder, ui_path.c_str(), NULL);
	
	// Update our member variables with the important widgets that 
	// might need to be queried or altered later
	window = GTK_WIDGET(gtk_builder_get_object(builder, "ap_UnixDialog_CollaborationAddBuddy"));
	m_wOk = GTK_WIDGET(gtk_builder_get_object(builder, "btOK"));
	m_wName = GTK_WIDGET(gtk_builder_get_object(builder, "edName"));
	m_wAccount = GTK_WIDGET(gtk_builder_get_object(builder, "cbAccount"));

	// set the dialog title
	// TODO
	
	// localize the strings in our dialog, and set tags for some widgets
	// TODO

	// connect our signals
	g_signal_connect(G_OBJECT(m_wOk),
							"clicked",
							G_CALLBACK(s_ok_clicked),
							static_cast<gpointer>(this));

	g_object_unref(G_OBJECT(builder));

	return window;
}

void AP_UnixDialog_CollaborationAddBuddy::_populateWindowData()
{
	// populate the account combobox
	GtkListStore* store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_POINTER);
	GtkTreeIter iter;
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();

	for (UT_uint32 i = 0; i < pManager->getAccounts().size(); i++)
	{
		AccountHandler* pHandler = pManager->getAccounts()[i];
		if (pHandler && pHandler->allowsManualBuddies())
		{
			gtk_list_store_append (store, &iter);
			gtk_list_store_set (store, &iter,
						DESC_COLUMN, pHandler->getDescription().utf8_str(),
						HANDLER_COLUMN, pHandler,
						-1);
		}
	}
	m_model = GTK_TREE_MODEL (store);
	gtk_combo_box_set_model(GTK_COMBO_BOX(m_wAccount), m_model);

	// if we have at least one account, then make sure the first one is selected
	if (pManager->getAccounts().size() > 0)
	{
		gtk_combo_box_set_active(GTK_COMBO_BOX(m_wAccount), 0);
	}
	else
	{
		// nope, we don't have any account :'-(
		gtk_combo_box_set_active(GTK_COMBO_BOX(m_wAccount), -1);
	}
}

void AP_UnixDialog_CollaborationAddBuddy::event_Ok()
{
	GtkTreeIter iter;
	if (gtk_combo_box_get_active_iter(GTK_COMBO_BOX(m_wAccount), &iter))
	{
		gpointer handler = 0;
		gtk_tree_model_get(m_model, &iter, HANDLER_COLUMN, &handler, -1);
		
		if (handler)
		{
			m_pAccount = reinterpret_cast<AccountHandler*>(handler);
			_setName(gtk_entry_get_text(GTK_ENTRY(m_wName)));
		}
		else
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	}
	else
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
}

