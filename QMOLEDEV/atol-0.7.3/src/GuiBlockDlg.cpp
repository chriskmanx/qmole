////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to block panel while operation is in progress
//////////////////////////////////////////////////////////////////////////// 

#include "GuiBlockDlg.h"
#include "core/String.h"
#include "support.h"
#include "core/debug.h"

#ifndef min
#define min(a,b) ((a)<(b))?(a):(b)
#endif

extern GtkWidget *atol_main;
static GtkWidget* Create_ProgressDlg (GuiBlockDlg *pDlg);
static void on_dialog_init (GtkWidget *button, gpointer user_data);

GuiBlockDlg::GuiBlockDlg()
{
	m_pEvBegin = NULL;
	m_bAbortRequest = false;
}

GuiBlockDlg::~GuiBlockDlg()
{
	if(m_pDialog){
		gtk_grab_remove(m_pDialog);
		gtk_widget_destroy(m_pDialog);
		m_pDialog = NULL;
	}
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void GuiBlockDlg::Create()
{
	m_pDialog = Create_ProgressDlg(this);
	if(m_pDialog)
	{
		g_signal_connect(m_pDialog, "show", G_CALLBACK (on_dialog_init), this);
		gtk_widget_show (m_pDialog);
	}
}

GtkWidget* Create_ProgressDlg (GuiBlockDlg *pDlg)
{
	GtkWidget *ProgressDlg;
	GtkWidget *dialog_vbox1;
	GtkWidget *table1;
	GtkWidget *label5;

	ProgressDlg = gtk_dialog_new ();
	gtk_window_set_title (GTK_WINDOW (ProgressDlg), _("Progress"));
	gtk_window_set_modal (GTK_WINDOW (ProgressDlg), TRUE);
	gtk_window_set_destroy_with_parent (GTK_WINDOW (ProgressDlg), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (ProgressDlg), TRUE);
#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
	//gtk_window_set_skip_taskbar_hint (GTK_WINDOW (ProgressDlg), TRUE);
#endif
	gtk_window_set_type_hint (GTK_WINDOW (ProgressDlg), (GdkWindowTypeHint)(GDK_WINDOW_TYPE_HINT_DIALOG));
	gtk_window_set_transient_for(GTK_WINDOW (ProgressDlg), GTK_WINDOW(atol_main));   //set parent
	gtk_window_set_resizable (GTK_WINDOW (ProgressDlg), FALSE);
	gtk_widget_realize(ProgressDlg);
	gdk_window_set_decorations(ProgressDlg->window, (GdkWMDecoration)(GDK_DECOR_BORDER|GDK_DECOR_TITLE));
	gdk_window_set_functions(ProgressDlg->window, (GdkWMFunction)0);
	gtk_dialog_set_has_separator (GTK_DIALOG(ProgressDlg), FALSE);

#if GTK_CHECK_VERSION(2,4,0) //new API TOFIX set proper version
	#ifndef _WIN32  //TOFIX API is buggy on Win32 (kills modal dialog state)
		//gtk_window_set_keep_above(GTK_WINDOW (ProgressDlg), TRUE);
	#endif
#endif

	dialog_vbox1 = GTK_DIALOG (ProgressDlg)->vbox;
	gtk_widget_show (dialog_vbox1);
	
	table1 = gtk_table_new (5, 2, FALSE);
	gtk_widget_set_size_request(table1, 300, -1);
	gtk_widget_show (table1);
	gtk_box_pack_start (GTK_BOX (dialog_vbox1), table1, TRUE, TRUE, 0);
	gtk_table_set_row_spacings (GTK_TABLE (table1), 5);
	gtk_table_set_col_spacings (GTK_TABLE (table1), 5);

	label5 = gtk_label_new (_("Please wait ..."));
	gtk_widget_show (label5);
	gtk_table_attach (GTK_TABLE (table1), label5, 1, 2, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label5), 0, 0.5);
	
	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (ProgressDlg, ProgressDlg, "ProgressDlg");
	GLADE_HOOKUP_OBJECT_NO_REF (ProgressDlg, dialog_vbox1, "dialog_vbox1");
	GLADE_HOOKUP_OBJECT (ProgressDlg, table1, "table1");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label5, "label5");

	//while in progress block other windows
	gtk_grab_add(ProgressDlg);

	return ProgressDlg;
} 

void GuiBlockDlg::SetTitle(const char *szTitle)
{
	gtk_window_set_title (GTK_WINDOW (m_pDialog), szTitle);
}

void GuiBlockDlg::SetInfoText(const char *szText)
{
	//use some caching to speedup operation (if already set, skip)
	GtkWidget *label5 = lookup_widget(m_pDialog, "label5");
	gtk_label_set_text (GTK_LABEL(label5), szText);
}

void on_dialog_init (GtkWidget *button, gpointer user_data)
{
	GuiBlockDlg *pDlg = (GuiBlockDlg *)user_data;
	if(pDlg)
	{
		TRACE("Progress dialog init: fire event for operation to start ... ");

		//TOFIX do this when ShowModal begins -> which signal to catch?
		//"grab-focus","realize", "show"
		ASSERT(NULL != pDlg->m_pEvBegin);
		if(NULL != pDlg->m_pEvBegin)
			pDlg->m_pEvBegin->Set();

		TRACE("done\n");
	}
}

