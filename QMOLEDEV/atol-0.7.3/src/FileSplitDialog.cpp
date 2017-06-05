////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define options for a file split operation
//////////////////////////////////////////////////////////////////////////// 

#include "FileSplitDialog.h"
#include "support.h"
#include <stdlib.h>	//atoi

extern GtkWidget *atol_main;
static GtkWidget* create_file_split_dialog ();
static void on_custom_size_combo (GtkComboBox *widget, gpointer user_data);

static FileSplitDialog::SPLIT_DEF arPredefines[] =
{
    {_("Custom")                   ,         0},
    {_("HD floppy 3,5\" (1,44 MB)"),   1457664},
    {_("Floppy (2,8 MB)")          ,   3019898},
    {_("Iomega Zip (100 MB)")      , 100431872},
    {_("SuperDisk LS-120 (120 MB)"), 125958144},
    {_("Iomega Zip (250 MB)")      , 250331136},
    {_("CD-ROM 74' (650 MB)")      , 660000*1024},
    {_("CD-ROM 80' (700 MB)")      , 720000*1024},
    {_("CD-ROM 90' (800 MB)")      , 810000*1024},
    {_("Jaz disk (1 GB)")          ,1000*1000000}, //1070MB where 1MB = 10^6
    {_("Jaz disk (2 GB)")          ,2000*1000000}  //2002MB where 1MB = 10^6
}; 

FileSplitDialog::FileSplitDialog()
{
	Create();
}

FileSplitDialog::~FileSplitDialog()
{
	Destroy();
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void FileSplitDialog::Create()
{
	m_pDialog = create_file_split_dialog ();

	GtkWidget *combobox6 = lookup_widget(m_pDialog, "combobox6");
	GtkWidget *combobox7 = lookup_widget(m_pDialog, "combobox7");

	int uSize = sizeof(arPredefines)/sizeof(arPredefines[0]); 
	for(int i=0; i<uSize; i++)
		gtk_combo_box_append_text(GTK_COMBO_BOX(combobox6), _(arPredefines[i].szName) );

	gtk_combo_box_append_text(GTK_COMBO_BOX(combobox7), _("byte(s)"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(combobox7), _("kB"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(combobox7), _("MB"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(combobox7), _("GB"));

	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox6), 0);
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox7), 0);

	g_signal_connect (combobox6, "changed", G_CALLBACK (on_custom_size_combo), this);	
}

GtkWidget* create_file_split_dialog ()
{
	GtkWidget *file_split_dialog;
	GtkWidget *dialog_vbox9;
	GtkWidget *table12;
	GtkWidget *combobox6;
	GtkWidget *label21;
	GtkWidget *entry9;
	GtkWidget *combobox7;
	GtkWidget *label22;
	GtkWidget *dialog_action_area9;
	GtkWidget *cancelbutton8;
	GtkWidget *okbutton8;
	
	file_split_dialog = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (file_split_dialog), _("Split file"));
	gtk_window_set_type_hint (GTK_WINDOW (file_split_dialog), GDK_WINDOW_TYPE_HINT_DIALOG);
	gtk_window_set_transient_for(GTK_WINDOW (file_split_dialog), GTK_WINDOW(atol_main));   //set parent

	dialog_vbox9 = GTK_DIALOG (file_split_dialog)->vbox;
	gtk_widget_show (dialog_vbox9);
	
	table12 = gtk_table_new (2, 3, FALSE);
	gtk_widget_show (table12);
	gtk_box_pack_start (GTK_BOX (dialog_vbox9), table12, TRUE, TRUE, 0);
	
	combobox6 = gtk_combo_box_new_text ();
	gtk_widget_show (combobox6);
	gtk_table_attach (GTK_TABLE (table12), combobox6, 1, 3, 0, 1,
		(GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		(GtkAttachOptions) (GTK_FILL), 0, 0);
	
	label21 = gtk_label_new (_("Predefined sizes:"));
	gtk_widget_show (label21);
	gtk_table_attach (GTK_TABLE (table12), label21, 0, 1, 0, 1,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label21), 0, 0.5);
	
	entry9 = gtk_entry_new ();
	gtk_widget_show (entry9);
	gtk_table_attach (GTK_TABLE (table12), entry9, 1, 2, 1, 2,
		(GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	
	combobox7 = gtk_combo_box_new_text ();
	gtk_widget_show (combobox7);
	gtk_table_attach (GTK_TABLE (table12), combobox7, 2, 3, 1, 2,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (GTK_FILL), 0, 0);
	
	label22 = gtk_label_new (_("Custom size:"));
	gtk_widget_show (label22);
	gtk_table_attach (GTK_TABLE (table12), label22, 0, 1, 1, 2,
		(GtkAttachOptions) (GTK_FILL),
		(GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label22), 0, 0.5);
	
	dialog_action_area9 = GTK_DIALOG (file_split_dialog)->action_area;
	gtk_widget_show (dialog_action_area9);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area9), GTK_BUTTONBOX_END);
	
	cancelbutton8 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton8);
	gtk_dialog_add_action_widget (GTK_DIALOG (file_split_dialog), cancelbutton8, GTK_RESPONSE_CANCEL);
	GTK_WIDGET_SET_FLAGS (cancelbutton8, GTK_CAN_DEFAULT);
	
	okbutton8 = gtk_button_new_from_stock ("gtk-ok");
	gtk_widget_show (okbutton8);
	gtk_dialog_add_action_widget (GTK_DIALOG (file_split_dialog), okbutton8, GTK_RESPONSE_OK);
	GTK_WIDGET_SET_FLAGS (okbutton8, GTK_CAN_DEFAULT);
	
	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (file_split_dialog, file_split_dialog, "file_split_dialog");
	GLADE_HOOKUP_OBJECT_NO_REF (file_split_dialog, dialog_vbox9, "dialog_vbox9");
	GLADE_HOOKUP_OBJECT (file_split_dialog, table12, "table12");
	GLADE_HOOKUP_OBJECT (file_split_dialog, combobox6, "combobox6");
	GLADE_HOOKUP_OBJECT (file_split_dialog, label21, "label21");
	GLADE_HOOKUP_OBJECT (file_split_dialog, entry9, "entry9");
	GLADE_HOOKUP_OBJECT (file_split_dialog, combobox7, "combobox7");
	GLADE_HOOKUP_OBJECT (file_split_dialog, label22, "label22");
	GLADE_HOOKUP_OBJECT_NO_REF (file_split_dialog, dialog_action_area9, "dialog_action_area9");
	GLADE_HOOKUP_OBJECT (file_split_dialog, cancelbutton8, "cancelbutton8");
	GLADE_HOOKUP_OBJECT (file_split_dialog, okbutton8, "okbutton8");
	
	return file_split_dialog;
} 

void on_custom_size_combo (GtkComboBox *widget, gpointer user_data)
{
	FileSplitDialog *pDlg = (FileSplitDialog *)user_data;
	GtkWidget *combobox6 = lookup_widget(pDlg->m_pDialog, "combobox6");
	GtkWidget *entry9 = lookup_widget(pDlg->m_pDialog, "entry9");
	GtkWidget *combobox7 = lookup_widget(pDlg->m_pDialog, "combobox7");
	
	int nPos = gtk_combo_box_get_active(GTK_COMBO_BOX(combobox6));
	if(nPos < 1)
	{
		//custom size, enable custom-size controls
		gtk_widget_set_sensitive(entry9, TRUE);
		gtk_widget_set_sensitive(combobox7, TRUE);
	}
	else
	{
		//predefined size, disable custom-size controls
		gtk_widget_set_sensitive(entry9, FALSE);
		gtk_widget_set_sensitive(combobox7, FALSE);

		//TOFIX fill custom-size controls with predefined values (also select proper size cbo item)
		//char szNum[20];
		//sprintf(szNum, "%d", arPredefines[nPos].nSize);
		//gtk_entry_set_text(GTK_ENTRY(entry9), szNum);
	}
}

int FileSplitDialog::GetSelectedSize()
{
	int nSize = 0;

	GtkWidget *combobox6 = lookup_widget(m_pDialog, "combobox6");
	GtkWidget *entry9 = lookup_widget(m_pDialog, "entry9");
	GtkWidget *combobox7 = lookup_widget(m_pDialog, "combobox7");
	
	int nPos = gtk_combo_box_get_active(GTK_COMBO_BOX(combobox6));
	if(nPos < 1)
	{
		//custom size selected
		nSize = atoi(gtk_entry_get_text(GTK_ENTRY(entry9)));

		//recalculate from selected unit to bytes
		int nUnit = gtk_combo_box_get_active(GTK_COMBO_BOX(combobox7));
		if(nUnit > 0)
			for(int i=1; i <= nUnit; i++)
                nSize *= 1024; 
	}
	else
	{
		//predefined size selected
		nSize = arPredefines[nPos].nSize;
	}

	return nSize;
}
