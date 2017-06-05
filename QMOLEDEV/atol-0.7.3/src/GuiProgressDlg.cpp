////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to display progress information for file operations
//////////////////////////////////////////////////////////////////////////// 

#include "GuiProgressDlg.h"
#include "core/String.h"
#include "support.h"
#include "core/debug.h"

#ifndef min
#define min(a,b) ((a)<(b))?(a):(b)
#endif

extern GtkWidget *atol_main;
static GtkWidget* Create_ProgressDlg (GuiProgressDlg *pDlg);
static void on_abort_activate (GtkWidget *button, gpointer user_data);
static void on_dialog_init (GtkWidget *button, gpointer user_data);

GuiProgressDlg::GuiProgressDlg()
{
	m_pEvBegin = NULL;
	m_bAbortRequest = false;
}

GuiProgressDlg::~GuiProgressDlg()
{
	if(m_pDialog){
		gtk_grab_remove(m_pDialog);
		gtk_widget_destroy(m_pDialog);
		m_pDialog = NULL;
	}
	gtk_window_present(GTK_WINDOW(atol_main)); //activate main window
}

void GuiProgressDlg::Create()
{
	m_pDialog = Create_ProgressDlg(this);
	if(m_pDialog)
	{
		g_signal_connect(m_pDialog, "show", G_CALLBACK (on_dialog_init), this);
		gtk_widget_show (m_pDialog);
	}
}

GtkWidget* Create_ProgressDlg (GuiProgressDlg *pDlg)
{
	GtkWidget *ProgressDlg;
	GtkWidget *dialog_vbox1;
	GtkWidget *table1;
	GtkWidget *progressbar1;
	GtkWidget *progressbar3;
	GtkWidget *label5;
	GtkWidget *label6;
	GtkWidget *label7;
	GtkWidget *dialog_action_area1;
	GtkWidget *cancelbutton1;
	GtkWidget *label28;
	GtkWidget *label29; 
	GtkWidget *label30;
	GtkWidget *label33;

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

	label30 = gtk_label_new ("");
#if GTK_CHECK_VERSION(2,6,0) //new API TOFIX set proper version	
	gtk_label_set_ellipsize(GTK_LABEL(label30), (PangoEllipsizeMode)PANGO_ELLIPSIZE_MIDDLE);
#endif	
	gtk_widget_show (label30);
	gtk_table_attach (GTK_TABLE (table1), label30, 1, 2, 0, 1, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label30), 0, 0.5);

	label33 = gtk_label_new ("");
#if GTK_CHECK_VERSION(2,6,0) //new API TOFIX set proper version	
	gtk_label_set_ellipsize(GTK_LABEL(label33), (PangoEllipsizeMode)PANGO_ELLIPSIZE_MIDDLE);
#endif	
	gtk_widget_show (label33);
	gtk_table_attach (GTK_TABLE (table1), label33, 1, 2, 1, 2, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label33), 0, 0.5); 
	
	label28 = gtk_label_new (_("From:"));
	gtk_widget_show (label28);
	gtk_table_attach (GTK_TABLE (table1), label28, 0, 1, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label28), 0, 0.5);

	label29 = gtk_label_new (_("To:"));
	gtk_widget_show (label29);
	gtk_table_attach (GTK_TABLE (table1), label29, 0, 1, 1, 2, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label29), 0, 0.5); 

	progressbar1 = gtk_progress_bar_new ();
	gtk_widget_set_size_request(progressbar1, 200, -1);
	gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (progressbar1), GTK_PROGRESS_CONTINUOUS);
	gtk_widget_show (progressbar1);

	gtk_table_attach (GTK_TABLE (table1), progressbar1, 1, 2, 2, 3, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	progressbar3 = gtk_progress_bar_new ();
	gtk_widget_set_size_request(progressbar3, 200, -1);
	gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (progressbar3), GTK_PROGRESS_CONTINUOUS);
	gtk_widget_show (progressbar3);
	gtk_table_attach (GTK_TABLE (table1), progressbar3, 1, 2, 3, 4, (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	
	label5 = gtk_label_new (_("File:"));
	gtk_widget_show (label5);
	gtk_table_attach (GTK_TABLE (table1), label5, 0, 1, 2, 3, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label5), 0, 0.5);
	
	label6 = gtk_label_new (_("Total:"));
	gtk_widget_show (label6);
	gtk_table_attach (GTK_TABLE (table1), label6, 0, 1, 3, 4, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label6), 0, 0.5);
	
	label7 = gtk_label_new ("");
	gtk_widget_show (label7);
	gtk_table_attach (GTK_TABLE (table1), label7, 0, 2, 4, 5, (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment (GTK_MISC (label7), 0, 0.5);
	
	dialog_action_area1 = GTK_DIALOG (ProgressDlg)->action_area;
	gtk_widget_show (dialog_action_area1);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area1), GTK_BUTTONBOX_END);
	
	cancelbutton1 = gtk_button_new_from_stock ("gtk-cancel");
	gtk_widget_show (cancelbutton1);
	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (ProgressDlg)->action_area), cancelbutton1, TRUE, TRUE, 0);
	g_signal_connect (GTK_OBJECT (cancelbutton1), "clicked", G_CALLBACK (on_abort_activate), pDlg);
	GTK_WIDGET_SET_FLAGS (cancelbutton1, GTK_CAN_DEFAULT);
	
	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (ProgressDlg, ProgressDlg, "ProgressDlg");
	GLADE_HOOKUP_OBJECT_NO_REF (ProgressDlg, dialog_vbox1, "dialog_vbox1");
	GLADE_HOOKUP_OBJECT (ProgressDlg, table1, "table1");
	GLADE_HOOKUP_OBJECT (ProgressDlg, progressbar1, "progressbar1");
	GLADE_HOOKUP_OBJECT (ProgressDlg, progressbar3, "progressbar3");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label5, "label5");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label6, "label6");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label7, "label7");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label28, "label28");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label29, "label29");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label30, "label30");
	GLADE_HOOKUP_OBJECT (ProgressDlg, label33, "label33");
	GLADE_HOOKUP_OBJECT_NO_REF (ProgressDlg, dialog_action_area1, "dialog_action_area1");
	GLADE_HOOKUP_OBJECT (ProgressDlg, cancelbutton1, "cancelbutton1");
	
//	gtk_widget_grab_default (cancelbutton1); 
	
	//while in progress block other windows
	gtk_grab_add(ProgressDlg);

	return ProgressDlg;
} 

void GuiProgressDlg::SetTitle(const char *szTitle)
{
	gtk_window_set_title (GTK_WINDOW (m_pDialog), szTitle);
}

void GuiProgressDlg::SetFileProgress(double dPercentage)
{
	GtkWidget *progressbar1 = lookup_widget(m_pDialog, "progressbar1");
	
	ASSERT(0.0 <= dPercentage && dPercentage <= 1.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar1), dPercentage);
	
	char szPercent[10];
	sprintf(szPercent, "%.1f %%", 100*dPercentage);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar1), szPercent);
}

void GuiProgressDlg::SetTotalProgress(double dPercentage)
{
	GtkWidget *progressbar3 = lookup_widget(m_pDialog, "progressbar3");

	ASSERT(0.0 <= dPercentage && dPercentage <= 1.0);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(progressbar3), dPercentage);

	char szPercent[10];
	sprintf(szPercent, "%.1f %%", 100*dPercentage);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(progressbar3), szPercent);
}

void GuiProgressDlg::SetSourceInfo(const char *szText)
{
	//use some caching to speedup operation (if already set, skip)
	GtkWidget *label30 = lookup_widget(m_pDialog, "label30");
	gtk_label_set_text (GTK_LABEL(label30), szText);
}

void GuiProgressDlg::SetDestinationInfo(const char *szText)
{
	//use some caching to speedup operation (if already set, skip)
	GtkWidget *label33 = lookup_widget(m_pDialog, "label33");
	gtk_label_set_text (GTK_LABEL(label33), szText);
}

void GuiProgressDlg::SetStatsInfo(const char *szText)
{
	GtkWidget *label7 = lookup_widget(m_pDialog, "label7");
	gtk_label_set_text (GTK_LABEL(label7), szText);
}	

void on_abort_activate (GtkWidget *button, gpointer user_data)
{
	GuiProgressDlg *pDlg = (GuiProgressDlg *)user_data;
	if(pDlg)
		pDlg->m_bAbortRequest = true;
}

void on_dialog_init (GtkWidget *button, gpointer user_data)
{
	GuiProgressDlg *pDlg = (GuiProgressDlg *)user_data;
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

