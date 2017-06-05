////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Start dialog for pack operation, defines archive type, name and additional properties
////////////////////////////////////////////////////////////////////////////

#include "PackFilesDlg.h"
#include "support.h"
#include "core/String.h"
#include "core/PluginManager.h"
#include "core/util.h"
#include "core/PathName.h"
#include "core/IniFile.h"
#include <vector>

extern GtkWidget *atol_main;
extern PluginManager g_PlugManager;
static void on_ok_clicked (GtkMenuItem *menuitem, gpointer user_data);
static void on_archive_type_combo (GtkComboBox *widget, gpointer user_data);
const char *get_selected_archiver(GtkComboBox *cbo);
const char *GetIniFile();

PackFilesDlg::PackFilesDlg()
{
	m_bMultiSupport  = false;
	m_bMoveToArchive = false;
}

PackFilesDlg::~PackFilesDlg()
{
	Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void PackFilesDlg::Destroy()
{
	if(m_pDialog)
	{
		//save last used archiver type
		GtkWidget *combobox5 = lookup_widget(m_pDialog, "combobox5");

		IniFile file;
		file.Load(GetIniFile());
		file.SetValue("Pack", "LastArchiver", get_selected_archiver(GTK_COMBO_BOX(combobox5)));
		file.Save();
		
		gtk_widget_destroy(m_pDialog);
		m_pDialog = NULL;
	}
}

void PackFilesDlg::Create()
{
	m_pDialog = create_pack_dialog();

	//set initial properties
	GtkWidget *entry8 = lookup_widget(m_pDialog, "entry8");
	GtkWidget *combobox5 = lookup_widget(m_pDialog, "combobox5");

	gtk_entry_set_text(GTK_ENTRY(entry8), m_strArchive);

	//fill all supported plugin extensions in the combo
	String strExtensions;
	std::vector<String> lstTokenized;

	int nMax = g_PlugManager.GetCount();
	for(int i=0; i<nMax; i++)
	{
		strExtensions = g_PlugManager[i].m_strExtensions;
		Tokenize(strExtensions, lstTokenized, ';');
		for(unsigned int j=0; j<lstTokenized.size(); j++)
		{
			int nCaps = g_PlugManager[i].m_pfnGetArchiverCaps(lstTokenized[j]);

			//see if plugin capabilities support creating new archive of this type
			//if multiple files selected for compression check caps for storing multiple files in single archive
			if(PK_CAPS_NEW & nCaps)
				if(!m_bMultiSupport || (m_bMultiSupport && (PK_CAPS_MULTIPLE & nCaps)))
					gtk_combo_box_append_text(GTK_COMBO_BOX(combobox5), lstTokenized[j].c_str());    //add to combo
		}
	}
	g_signal_connect (combobox5, "changed", G_CALLBACK(on_archive_type_combo), this); 

	//load last used archiver type
	std::string strValue;
	IniFile file;
	file.Load(GetIniFile());
	file.GetValue("Pack", "LastArchiver", strValue, "");

	if(nMax>0)
	{
		//if posssible select last used archiver (read from INI)
		bool bIniArchiverSet = false;
		if(strValue.size()>0)
		{
			//find and select give content
			GtkTreeModel *model = gtk_combo_box_get_model(GTK_COMBO_BOX(combobox5));
			GtkTreeIter iter;

			int nMax = gtk_tree_model_iter_n_children(model, NULL);	//count toplevel nodes
			for(int nPos=0; nPos<nMax; nPos++)
			{
				char szPath[10];
				sprintf(szPath, "%d", nPos);
				GtkTreePath *path1 = gtk_tree_path_new_from_string (szPath);
				if(gtk_tree_model_get_iter(model, &iter, path1))
				{
					gchar *value = NULL;
					gtk_tree_model_get (model, &iter, 0, &value, -1);
					if(0 == strcmp(value, strValue.c_str()))
					{
						gtk_combo_box_set_active(GTK_COMBO_BOX(combobox5), nPos);
						bIniArchiverSet = true;
						break;
					}
					//g_free(value);
				}
			}
		}

		//select 1st archiver in the list
		if(!bIniArchiverSet)
			gtk_combo_box_set_active(GTK_COMBO_BOX(combobox5), 0);
	}
}

GtkWidget *PackFilesDlg::create_pack_dialog ()
{
	GtkWidget *pack_dialog;
	GtkWidget *dialog_vbox6;
	GtkWidget *table11;
	GtkWidget *label19;
	GtkWidget *entry8;
	GtkWidget *combobox5;
	GtkWidget *checkbutton12;
	GtkWidget *checkbutton13;
	GtkWidget *dialog_action_area6;
	GtkWidget *cancelbutton5;
	GtkWidget *okbutton5;
	
	pack_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (pack_dialog), _("Pack files"));
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (pack_dialog), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (pack_dialog), TRUE);
	gtk_window_set_type_hint (GTK_WINDOW (pack_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (pack_dialog), GTK_WINDOW(atol_main));   //set parent
	
	dialog_vbox6 = GTK_DIALOG (pack_dialog)->vbox;
	gtk_widget_show (dialog_vbox6);
	
	table11 = gtk_table_new (3, 3, FALSE);
	gtk_widget_show (table11);
	gtk_box_pack_start (GTK_BOX (dialog_vbox6), table11, TRUE, TRUE, 0);
	
	label19 = gtk_label_new (_("Archive:"));
	gtk_widget_show (label19);
	gtk_table_attach (GTK_TABLE (table11), label19, 0, 1, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label19), 0, 0.5);
	
	entry8 = gtk_entry_new ();
	gtk_widget_show (entry8);
	gtk_entry_set_activates_default (GTK_ENTRY (entry8), TRUE);
	gtk_table_attach (GTK_TABLE (table11), entry8, 1, 2, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	combobox5 = gtk_combo_box_new_text ();
	gtk_widget_show (combobox5);
	gtk_table_attach (GTK_TABLE (table11), combobox5, 2, 3, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (GTK_FILL), 0, 0);
	
	checkbutton12 = gtk_check_button_new_with_mnemonic (_("_Move to archive"));
	gtk_widget_show (checkbutton12);
	gtk_table_attach (GTK_TABLE (table11), checkbutton12, 0, 2, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	checkbutton13 = gtk_check_button_new_with_mnemonic (_("_SFX"));
	gtk_widget_set_sensitive (checkbutton13, FALSE); //TOFIX until implemented
	gtk_widget_show (checkbutton13);
	gtk_table_attach (GTK_TABLE (table11), checkbutton13, 0, 2, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	dialog_action_area6 = GTK_DIALOG (pack_dialog)->action_area;
	gtk_widget_show (dialog_action_area6);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area6), GTK_BUTTONBOX_END);
	
	cancelbutton5 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton5);
	gtk_dialog_add_action_widget (GTK_DIALOG (pack_dialog), cancelbutton5, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton5, GTK_CAN_DEFAULT);
	
	okbutton5 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton5);
	gtk_container_add (GTK_CONTAINER(dialog_action_area6), okbutton5);
	GTK_WIDGET_SET_FLAGS (okbutton5, GTK_CAN_DEFAULT);

	g_signal_connect (okbutton5, "clicked",	G_CALLBACK (on_ok_clicked), this);

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (pack_dialog, pack_dialog, "pack_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (pack_dialog, dialog_vbox6, "dialog_vbox6");
	GLADE_HOOKUP_OBJECT (pack_dialog, table11, "table11");
	GLADE_HOOKUP_OBJECT (pack_dialog, label19, "label19");
	GLADE_HOOKUP_OBJECT (pack_dialog, entry8, "entry8");
	GLADE_HOOKUP_OBJECT (pack_dialog, combobox5, "combobox5");
	GLADE_HOOKUP_OBJECT (pack_dialog, checkbutton12, "checkbutton12");
	GLADE_HOOKUP_OBJECT (pack_dialog, checkbutton13, "checkbutton13");
	GLADE_HOOKUP_OBJECT_NO_REF (pack_dialog, dialog_action_area6, "dialog_action_area6");
	GLADE_HOOKUP_OBJECT (pack_dialog, cancelbutton5, "cancelbutton5");
	GLADE_HOOKUP_OBJECT (pack_dialog, okbutton5, "okbutton5");

	gtk_widget_grab_focus (entry8);
	gtk_widget_grab_default (okbutton5);

	return pack_dialog;
}

void on_archive_type_combo (GtkComboBox *widget, gpointer user_data)
{
	PackFilesDlg *pDlg = (PackFilesDlg *)user_data;
	GtkWidget *combobox5 = lookup_widget(pDlg->m_pDialog, "combobox5");
	GtkWidget *checkbutton12 = lookup_widget(pDlg->m_pDialog, "checkbutton12");
	
    //check archiver caps for moving file to archive
	std::string strExt = get_selected_archiver(GTK_COMBO_BOX(combobox5));

	ArchiverPlugin *pPlugin = g_PlugManager.FindArchiver(strExt.c_str());
	if(NULL != pPlugin)
	{
		int nCaps = pPlugin->m_pfnGetArchiverCaps(strExt.c_str());
		if(PK_CAPS_REAL & nCaps)
			gtk_widget_set_sensitive (checkbutton12, TRUE);
		else
			gtk_widget_set_sensitive (checkbutton12, FALSE);
	}
}

const char *get_selected_archiver(GtkComboBox *cbo)
{
#if GTK_CHECK_VERSION(2,6,0) //new API //TOFIX set proper version//
	return gtk_combo_box_get_active_text(cbo);
#else
	int nPos = gtk_combo_box_get_active(cbo);
	if(nPos >= 0)
	{
		GtkTreeModel *model = gtk_combo_box_get_model(cbo);
		GtkTreeIter iter;
		char szPath[10];
		sprintf(szPath, "%d", nPos);
		GtkTreePath *path1 = gtk_tree_path_new_from_string (szPath);
		if(gtk_tree_model_get_iter(model, &iter, path1))
		{
			gchar *value = NULL;
			gtk_tree_model_get (model, &iter, 0, &value, -1);
			return value;
			//g_free(value);
		}
	}
#endif
}

void on_ok_clicked (GtkMenuItem *menuitem, gpointer user_data)
{
	PackFilesDlg *pDlg = (PackFilesDlg *)user_data;
	pDlg->OnDialogOK();
	pDlg->Destroy();
}

void PackFilesDlg::OnDialogOK()
{
	//refresh archive name
	GtkWidget *entry8    = lookup_widget(m_pDialog, "entry8");
	GtkWidget *combobox5 = lookup_widget(m_pDialog, "combobox5");
	GtkWidget *checkbutton12 = lookup_widget(m_pDialog, "checkbutton12");

	m_strArchive  = gtk_entry_get_text(GTK_ENTRY(entry8));
	m_strArchive += get_selected_archiver(GTK_COMBO_BOX(combobox5));

	m_bMoveToArchive = false;
	if( GTK_WIDGET_SENSITIVE(checkbutton12) &&
	    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(checkbutton12)))
	{
		m_bMoveToArchive = true;
	}

	//make an OK response
	gtk_dialog_response (GTK_DIALOG(m_pDialog),  GTK_RESPONSE_OK);
}

