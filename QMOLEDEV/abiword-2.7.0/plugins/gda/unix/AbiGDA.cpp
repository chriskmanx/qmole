/* -*- Mode: C++; tab-width: 8; c-basic-offset: 8 -*- */

/* AbiGDA.cpp - hook up AbiWord to SQL databases
 *
 * Copyright (C) 2003 by Dom Lachowicz
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

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_gda_register
#define abi_plugin_unregister abipgn_gda_unregister
#define abi_plugin_supports_version abipgn_gda_supports_version
#endif

#include "xap_Module.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "pd_Document.h"
#include "ap_Menu_Id.h"
#include "ev_Menu_Actions.h"
#include "ev_Menu.h"
#include "ev_Menu_Layouts.h"
#include "ev_Menu_Labels.h"
#include "ev_EditMethod.h"
#include "xap_Menu_Layouts.h"
#include "ut_string_class.h"

#include "ie_imp.h"
#include "ie_types.h"
#include "pd_Document.h"
#include "ut_growbuf.h"
#include "ut_stack.h"

#include "xap_UnixFrameImpl.h"
#include "xap_UnixDialogHelper.h"

#include <gtk/gtk.h>
#include <libgda/libgda.h>
#include <libgnomedb/gnome-db-editor.h>
#include <libgnomedb/gnome-db-login.h>
#include <libgnomedb/gnome-db-util.h>

static GdaClient * connection_pool = NULL;

// -----------------------------------------------------------------------
// -----------------------------------------------------------------------

#define X_ReturnNoMemIfError(exp)	UT_return_val_if_fail(exp,UT_IE_NOMEMORY)
#define X_CheckError(v)			UT_return_if_fail(v != UT_OK)

/**
 * This class' purpose is to take (or generate) a GdaDataModel
 * and turn it into an AbiWord table. If we generate a DataModel
 * from a .connection file (i.e. we're importing a new document),
 * we'll open up a new window. If not, we'll act like the data
 * set got pasted in place
 */
class IE_Imp_GDA : public IE_Imp
{
public:

	IE_Imp_GDA (PD_Document * pDocument, GdaDataModel * model = 0)
		: IE_Imp (pDocument), m_model (model), m_row(0), m_col(0)
	{
		m_generatedModel = (m_model == 0);
	}

	virtual ~IE_Imp_GDA ()
	{
		if (m_model) {
			g_object_unref (G_OBJECT (m_model));
		}
	}

	UT_Error importDataModel ()
	{
		return _importDataModel ();
	}

protected:

	virtual UT_Error _loadFile(GsfInput * input)
	{
		// _importDataModel ();
		UT_ASSERT(UT_TODO);
		return UT_ERROR;
	}

private:

	// unimplemented functions

	IE_Imp_GDA();
	IE_Imp_GDA(const IE_Imp_GDA &);
	IE_Imp_GDA& operator=(const IE_Imp_GDA &);

	// private functions

	UT_Error _importDataModel () {

		if (m_generatedModel) {
			// write out a header - add a section and a paragraph to the document
			X_ReturnNoMemIfError(appendStrux(PTX_Section, NULL));
			X_ReturnNoMemIfError(appendStrux(PTX_Block, NULL));
		} else {
			// tell IE_Imp that we're actually pasting stuff
			PT_DocPosition dpos = 0;

			XAP_Frame * pFrame = XAP_App::getApp()->getLastFocussedFrame();
			if (pFrame) {
				FV_View * pView = static_cast<FV_View*>(pFrame->getCurrentView());
				if (pView)
					dpos = pView->getInsPoint ();
			}

			setClipboard(dpos);
		}

		gint col = 0;
		gint row = 0;
		gint fieldcount = 0;
		gint rowcount   = 0;
		
		g_return_val_if_fail (GDA_IS_DATA_MODEL (m_model), false);
		
		fieldcount = gda_data_model_get_n_columns (GDA_DATA_MODEL (m_model));
		rowcount = gda_data_model_get_n_rows (GDA_DATA_MODEL (m_model));
		
		if (rowcount <= 0) {
			g_object_unref (G_OBJECT (m_model));
			return UT_ERROR;
		}

		openTable();

		// draw the table header
		openRow ();
		for (col = 0; col < fieldcount; col++) {
			addCell (gda_data_model_get_column_title (GDA_DATA_MODEL (m_model), col), true);
		}
		closeRow ();		
		
		for (row = 0; row < rowcount; row++) {
			openRow ();
			for (col = 0; col < fieldcount; col++) {
				addCell (gda_data_model_get_value_at (GDA_DATA_MODEL (m_model),
								      col, row));
			}
			closeRow ();
		}
		
		closeTable ();

		if (m_generatedModel) {
			// append a final empty paragraph into the doc
			X_ReturnNoMemIfError(appendStrux(PTX_Block, NULL));	
		}

		return UT_OK;
	}

	void openTable()
	{
		m_row = m_col = 0;
		X_CheckError(appendStrux(PTX_SectionTable,NULL));
	}

	void closeTable()
	{
		m_row = m_col = 0;
		X_CheckError(appendStrux(PTX_EndTable,NULL));	
	}

	void openRow()
	{
		m_col = 0;
	}

	void closeRow()
	{
		m_row++; m_col = 0;
	}

	void addCell(const GdaValue *value)
	{
		gchar * str = gda_value_stringify (const_cast<GdaValue*>(value));	       
		addCell (str);
		g_free (str);
	}       	

	void addCell(const char * value, bool header = false)
	{
		UT_String attach (UT_String_sprintf("top-attach:%d; bot-attach:%d; left-attach:%d; right-attach:%d", 
						    m_row, m_row+1, m_col, m_col+1));

		if (header) {
			attach += "; background-color:808080";
		}
		
		const gchar *table_props[3];
		table_props[0] = "props";
		table_props[1] = attach.c_str();
		table_props[2] = 0;

#if 0
		// appendFmt has bad effects due to not being implemented
		// in the case of pasting stuff...
		const gchar *font_props[3];
		font_props[0] = "props";
		font_props[2] = 0;

		if (header) {
			font_props[1] = "font-weight: bold";
		} else {
			font_props[1] = "font-weight: normal";
		}
#endif

		X_CheckError(appendStrux(PTX_SectionCell,table_props));
		X_CheckError(appendStrux(PTX_Block,NULL));	

		if (value) {
			UT_UCS4String ucs4 (value);
			if (ucs4.length ()) {
				//appendFmt (font_props);
				appendSpan (ucs4.ucs4_str(), ucs4.length());
			}
		}

		m_col++;
		X_CheckError(appendStrux(PTX_EndCell,NULL));
	}

	// private data

	GdaDataModel *m_model;

	int m_row;
	int m_col;

	bool m_generatedModel;
};

// -----------------------------------------------------------------------
// -----------------------------------------------------------------------

static bool
handle_recordset (GdaDataModel *recset, PD_Document * pDoc)
{
	g_return_val_if_fail (GDA_IS_DATA_MODEL (recset), false);
	
	IE_Imp_GDA gda(pDoc, recset);
	return (gda.importDataModel () == UT_OK);
}

static bool
gda_ok_pressed (GtkWidget * login, GtkWidget * txt, PD_Document * pDoc)
{
	UT_UTF8String dsn_name, user_name, password, sql;	
	
	GdaConnection* cnc;
	GdaDataModel*  recset;
	GdaCommand*    cmd;

	dsn_name  = gnome_db_login_get_dsn (GNOME_DB_LOGIN(login));
	user_name = gnome_db_login_get_username (GNOME_DB_LOGIN(login));
	password  = gnome_db_login_get_password (GNOME_DB_LOGIN(login));
	
	char * editable_txt = gnome_db_editor_get_all_text (GNOME_DB_EDITOR (txt));
	sql = editable_txt;
	g_free (editable_txt);
	
	cnc = gda_client_open_connection (connection_pool, dsn_name.utf8_str(), user_name.utf8_str(), password.utf8_str(), GDA_CONNECTION_OPTIONS_READ_ONLY);
	if (!GDA_IS_CONNECTION (cnc))
		return false;
	
	/* execute command */
	cmd = gda_command_new (sql.utf8_str(), GDA_COMMAND_TYPE_SQL, (GdaCommandOptions)0);
	recset = gda_connection_execute_single_command (cnc, cmd, NULL);
	gda_command_free (cmd);
	
	if (recset == NULL || !GDA_IS_DATA_MODEL (recset))
		return false;
	else
		return handle_recordset (recset, pDoc);  
}

//
// GDA_execSQL
// -------------------
//   This is the function that we actually call to invoke the thesaurus.
//   It should be called when the user hits the thesaurus key (shift+f7?)
//   or chooses "thesaurus" from a menu.
//
static bool 
GDA_execSQL(AV_View* v, EV_EditMethodCallData *d)
{
	// Get the current view that the user is in.
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());
	
	GtkWidget * dlg;
	GtkWidget * db_login;
	GtkWidget * text;
	GtkWidget * table;
	GtkWidget * label;

	bool ret = false;

	dlg = abiDialogNew ("gda database", FALSE, "GDA Table Insertion");
	abiAddStockButton(GTK_DIALOG(dlg), GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
	abiAddStockButton(GTK_DIALOG(dlg), GTK_STOCK_OK, GTK_RESPONSE_OK);

	db_login = gnome_db_login_new (NULL);
	text = gnome_db_editor_new ();

	table = gnome_db_new_table_widget (4, 2, FALSE);
	gtk_container_set_border_width (GTK_CONTAINER (table), 12);

	label = gnome_db_new_label_widget (NULL);
	gtk_label_set_markup (GTK_LABEL (label), "<b>Data Source</b>");
	gtk_table_attach (GTK_TABLE (table), label, 0, 2, 0, 1, GTK_FILL, GTK_FILL, 6, 6);
	gtk_table_attach (GTK_TABLE (table), db_login, 1, 2, 1, 2,
			  GTK_EXPAND, GTK_EXPAND, 6, 6);

	label = gnome_db_new_label_widget (NULL);
	gtk_label_set_markup (GTK_LABEL (label), "<b>Command to Execute</b>");
	gtk_table_attach (GTK_TABLE (table), label, 0, 2, 2, 3, GTK_FILL, GTK_FILL, 6, 6);
	gtk_table_attach (GTK_TABLE (table), text, 1, 2, 3, 4,
			  GTK_FILL, GTK_FILL, 2, 2);

	gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dlg)->vbox), table);

	gtk_widget_show_all (dlg);

	switch (abiRunModalDialog (GTK_DIALOG(dlg), pFrame, NULL, GTK_RESPONSE_CANCEL, false))
		{
		case GTK_RESPONSE_OK:
			ret = gda_ok_pressed (db_login, text, pView->getDocument ());
			break;
		}

	abiDestroyWidget (dlg);

	return ret;
}

//
// GDA_viewDataSources
// -------------------
static bool 
GDA_viewDataSources(AV_View* v, EV_EditMethodCallData *d)
{
	char *argv[2];

	/* run gnome-database-properties config tool */
	argv[0] = (char *) "gnome-database-properties";
	argv[1] = NULL;

	return g_spawn_async (NULL, argv, NULL, G_SPAWN_SEARCH_PATH,
			      NULL, NULL, NULL, NULL);
}

const static struct {
	const char    * methodName;
	EV_EditMethod_pFn method;
	const char    * label;
	const char    * description;
	EV_Menu_LayoutFlags flags;		// usually EV_MLF_Normal
} gda_menus [] = {
	{ "GDA_execSQL", GDA_execSQL, "G&DA Database", "Insert database tables as tbles", EV_MLF_Normal },
	{ "GDA_viewDataSources", GDA_viewDataSources, "Data Sources", "Configure and admin data sources", EV_MLF_Normal }
};

static void
GDA_removeFromMenus()
{
	// First we need to get a pointer to the application itself.
	XAP_App *pApp = XAP_App::getApp();       
	
	// now remove crap from the menus
	int frameCount = pApp->getFrameCount();
	XAP_Menu_Factory * pFact = pApp->getMenuFactory();

	// remove the edit method
	EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer() ;
	EV_EditMethod * pEM;
	
	UT_uint32 i;
	for (i = 0; i < G_N_ELEMENTS (gda_menus); i++) {
		pFact->removeMenuItem("Main",NULL,gda_menus[i].label);
		pEM = ev_EditMethod_lookup ( gda_menus[i].methodName ) ;
		pEMC->removeEditMethod ( pEM ) ;
		DELETEP( pEM ) ;
	}

	for(int i = 0; i < frameCount; ++i) {
		// Get the current frame that we're iterating through.
		XAP_Frame* pFrame = pApp->getFrame(i);
		pFrame->rebuildMenus();
	}

}

static void
GDA_addToMenus()
{
	// First we need to get a pointer to the application itself.
	XAP_App *pApp = XAP_App::getApp();

	EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer() ;
	XAP_Menu_Factory * pFact = pApp->getMenuFactory();
	EV_Menu_ActionSet* pActionSet = pApp->getMenuActionSet();

	int frameCount = pApp->getFrameCount();

	UT_uint32 i;

	for (i = 0; i < G_N_ELEMENTS (gda_menus); i++) {
		// Create an EditMethod that will link our method's name with
		// it's callback function.  This is used to link the name to 
		// the callback.
		EV_EditMethod *myEditMethod = new EV_EditMethod(gda_menus[i].methodName,
								gda_menus[i].method,
								0,
								"");
	
		// We have to add our EditMethod to the application's EditMethodList
		// so that the application will know what callback to call when a call
		// to "AiksaurusABI_invoke" is received.
		pEMC->addEditMethod(myEditMethod);
	
		//
		// Put it in the main menu.
		//
		const char *prev = "S&cripts";
		if (i != 0)
			prev = gda_menus[i - 1].label;
		XAP_Menu_Id newID = pFact->addNewMenuAfter("Main",NULL,
							   prev,
							   gda_menus[i].flags);
		pFact->addNewLabel(NULL,newID,gda_menus[i].label, gda_menus[i].description);
	
		// Create the Action that will be called.
		EV_Menu_Action* myAction = new EV_Menu_Action(newID, 0, 1, 0, 0,
							      (const char *) gda_menus[i].methodName,
							      NULL, NULL);
	
		// Now what we need to do is add this particular action to the ActionSet
		// of the application.  This forms the link between our new ID that we 
		// got for this particular frame with the EditMethod that knows how to 
		// call our callback function.  
	
		pActionSet->addAction(myAction);
	}
	
	for(int i = 0; i < frameCount; ++i) {
		// Get the current frame that we're iterating through.
		XAP_Frame* pFrame = pApp->getFrame(i);
		pFrame->rebuildMenus();
	}
}

// -----------------------------------------------------------------------
//
//      Abiword Plugin Interface 
//
// -----------------------------------------------------------------------

ABI_PLUGIN_DECLARE	("AbiGDA")

ABI_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
	/* initialize connection pool if first time */
	if (!GDA_IS_CLIENT (connection_pool)) {
		connection_pool = gda_client_new ();
		if (!connection_pool)
			return 0;
	}
	
	mi->name    = "GDA plugin";
	mi->desc    = "Database support for AbiWord";
	mi->version = ABI_VERSION_STRING;
	mi->author  = "Dom Lachowicz <cinamod@hotmail.com>";
	mi->usage   = "You had better know some SQL...";
	
	// Add the database to AbiWord's menus.
	GDA_addToMenus();
	
	return 1;
}

ABI_FAR_CALL
int abi_plugin_unregister (XAP_ModuleInfo * mi)
{
	mi->name    = 0;
	mi->desc    = 0;
	mi->version = 0;
	mi->author  = 0;
	mi->usage   = 0;
	
	GDA_removeFromMenus () ;
	
	if (GDA_IS_CLIENT (connection_pool)) {
		g_object_unref (G_OBJECT (connection_pool));
		connection_pool = NULL;
	}
	
	return 1;
}

ABI_FAR_CALL
int abi_plugin_supports_version (UT_uint32 major, UT_uint32 minor, UT_uint32 release)
{
	return 1; 
}
