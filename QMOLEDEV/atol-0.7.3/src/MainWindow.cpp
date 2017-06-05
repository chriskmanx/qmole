////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: main window implementation
//////////////////////////////////////////////////////////////////////////// 

#include "MainWindow.h"
#include "core/IniFile.h"
#include "callbacks.h"
#include "DualPanel.h"
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "support.h"
#include "core/PluginManager.h"
#include "core/PortableTrayIcon.h"
#include "core/PathName.h"
#include "core/System.h"

#include "../res/refresh.xpm"
#include "../res/copy.xpm"
#include "../res/move.xpm"
#include "../res/new_dir.xpm"
#include "../res/recycle.xpm"
#include "../res/help.xpm"
#include "../res/atol.xpm"
#include "../res/terminal.xpm"
#include "../res/ftp.xpm"
#include "../res/close.xpm"

extern bool g_bNCSelection;
extern bool g_bRefreshPanelsOnFocus;
extern bool g_bShowExtColumn;
float g_fSplitterPercent = 0.5;
PortableTrayIcon g_tray;
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);

static void splitter_pos_changed_cb (GObject *gobject, GParamSpec *arg1, gpointer user_data);
static gboolean window_size_event (GtkWidget *widget, GdkEventConfigure *event);
static gboolean window_state_event (GtkWidget *widget, GdkEventWindowState *event);
static void create_menu (GtkWidget *atol_main, GtkWidget *menubar1, GtkAccelGroup *accel_group);
static gint cmdline_keyboard_handler(GtkWidget *widget, GdkEventKey *event, gpointer data);

const char *GetIniFile();
extern GtkWidget *atol_main;
extern bool g_bMinimizeToTray;
extern DualPanel g_dp;
extern String g_strEditor;
extern String g_strTerminal;
extern PluginManager g_PlugManager;
extern MainWindow g_wnd; 
extern bool g_bShowDirWarningDialog;
extern bool g_bShowReadOnlyWarningDialog; 
GdkColor g_labelTextColor       = { 0, 0xFFFF, 0xFFFF, 0xFFFF };
GdkColor g_labelBackgroundColor = { 0, 0, 0, 0xAFFF };

MainWindow::MainWindow()
{
	m_pWidget = NULL;
	m_pHistoryPrev = NULL;
	m_pHistoryNext = NULL;
	m_pConnectionClose = NULL;
	m_pTransferMode = NULL;
	m_pCmdLineWidget = NULL;
}

MainWindow::~MainWindow()
{
}

void MainWindow::Create(const char *szLeftPath, const char *szRightPath)
{
	//read pre-create options
	IniFile file;
	file.Load(GetIniFile());
	file.GetValue("Display", "ShowExtColumn", g_bShowExtColumn, true);

	m_pWidget = create_atol_main();
	OnCreate(szLeftPath, szRightPath);
}

void MainWindow::OnCreate(const char *szLeftPath, const char *szRightPath)
{
	//check startup options
	IniFile file;
	file.Load(GetIniFile());

	atol_main = m_pWidget;
		
	//set custom GUI colors
	std::string strColor, strColor1, strColor2, strColor3;
	file.GetValue("Display", "TextColor", strColor, "");
	file.GetValue("Display", "BackgroundColor", strColor1, "");
	file.GetValue("Display", "SelectionTextColor", strColor2, "");
	file.GetValue("Display", "SelectionBackgroundColor", strColor3, "");
	if(strColor.size() > 0)
	{
		GdkColor color;
		gdk_color_parse (strColor.c_str(), &color);
		gtk_widget_modify_text(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_NORMAL, &color);
		gtk_widget_modify_text(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_NORMAL, &color);
	}
	if(strColor1.size() > 0)
	{
		GdkColor color;
		gdk_color_parse (strColor1.c_str(), &color);
		gtk_widget_modify_base(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_NORMAL, &color);
		gtk_widget_modify_base(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_NORMAL, &color);
	}
	if(strColor2.size() > 0)
	{
		gdk_color_parse (strColor2.c_str(), &g_labelTextColor);
		gtk_widget_modify_text(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelTextColor);
		gtk_widget_modify_text(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelTextColor);
	}
	if(strColor3.size() > 0)
	{
		gdk_color_parse (strColor3.c_str(), &g_labelBackgroundColor);
		gtk_widget_modify_base(GTK_WIDGET(g_dp.GetRightFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelBackgroundColor);
		gtk_widget_modify_base(GTK_WIDGET(g_dp.GetLeftFileList().m_pWidget), GTK_STATE_SELECTED, &g_labelBackgroundColor);
	}

	g_signal_connect (atol_main, "focus-in-event", G_CALLBACK (on_mainwindow_focus), NULL); 
	g_signal_connect (atol_main, "destroy", G_CALLBACK (on_quit1_activate), NULL); 
	gtk_widget_show (atol_main);
	
	bool bMaximize;
	if(file.GetValue("Startup", "Maximize", bMaximize, true))
		if(bMaximize)
			gtk_window_maximize(GTK_WINDOW(atol_main));

	//restore "toolbar visible" option
	bool bShowToolbar;
	file.GetValue("Display", "ShowToolBar", bShowToolbar, 1);
	if(bShowToolbar)
		set_show_toolbar(true);

	//restore "command line visible" option
	bool bShowCmdLine;
	file.GetValue("Display", "ShowCommandLine", bShowCmdLine, 1);
	if(bShowCmdLine)	// default is true, change only if false
		set_show_command_line(true);	//TOFIX temporary

	//restore "trace view visible" option
	bool bShowTraceView;
	file.GetValue("Display", "ShowTraceView", bShowTraceView, 1);
	if(bShowTraceView)	// default is true, change only if false
		set_show_trace_view(true);

	//restore "status bar visible" option
	bool bShowStatBar;
	file.GetValue("Display", "ShowStatusBar", bShowStatBar, 1);
	if(bShowStatBar)	// default is true, change only if false
		set_show_status_bar(true);

	//restore "show hidden files" option
	bool bShowHidden;
	file.GetValue("Display", "ShowHiddenFiles", bShowHidden, 0);
	if(!bShowHidden)
		set_show_hidden(false);

	//load global option
	file.GetValue("Display", "MinimizeToTray", g_bMinimizeToTray);
	file.GetValue("Operation", "NC_Selection", g_bNCSelection);
	
	//set starting panel directories
	int nValue = 0;
	file.GetValue("Startup", "RestorePaths", nValue, 0);
	if(nValue > 0 || NULL != szLeftPath)
	{
		std::string strValue;
		
		if(NULL != szLeftPath)
			strValue = szLeftPath;
		else
			file.GetValue("LeftPanel", "Path", strValue, "");

		if(strValue.size()>0)
		{
			g_dp.GetLeftFileList().m_ctrl.m_lstHistory.Clear();
			if(!g_dp.GetLeftFileList().SetDirectory(strValue.c_str(), true, true))  //silent
			{
				g_dp.GetLeftFileList().SetDirectory(PathName::GetDefaultStartDir());

				String strMsg;
				strMsg.Printf(_("Failed to set directory: %s!"), strValue.c_str());
				gtkMessageBox(strMsg);
			}
		}
		else
			g_dp.GetLeftFileList().SetDirectory(PathName::GetDefaultStartDir());
	}
	
	if(nValue > 0 || NULL != szRightPath)
	{
		std::string strValue;

		if(NULL != szRightPath)
			strValue = szRightPath;
		else
			file.GetValue("RightPanel", "Path", strValue, "");
 
		if(strValue.size()>0)
		{
			g_dp.GetRightFileList().m_ctrl.m_lstHistory.Clear();
			if(!g_dp.GetRightFileList().SetDirectory(strValue.c_str(), true, true))  //silent
			{
				g_dp.GetRightFileList().SetDirectory(PathName::GetDefaultStartDir());

				String strMsg;
				strMsg.Printf(_("Failed to set directory: %s!"), strValue.c_str());
				gtkMessageBox(strMsg);
			}
		}
		else
			g_dp.GetRightFileList().SetDirectory(PathName::GetDefaultStartDir());
	}
	else
	{
		g_dp.GetLeftFileList().SetDirectory(PathName::GetDefaultStartDir());
		g_dp.GetRightFileList().SetDirectory(PathName::GetDefaultStartDir());
	}

	//read "show directory delete warning" option
	file.GetValue("Operation", "DeleteDirWarning", g_bShowDirWarningDialog, true);

	//read "show read-only file delete warning" option
	file.GetValue("Operation", "ReadOnlyWarning", g_bShowReadOnlyWarningDialog, true);
  
	//refresh Vfs::LOCAL panels on windowfocus setting
	file.GetValue("Operation", "RefreshPanels", g_bRefreshPanelsOnFocus, true);

	//restore sort columns for file panels
	int nCol;
	bool bAsc;
	file.GetValue("LeftPanel", "SortCol", nCol);
	file.GetValue("LeftPanel", "SortAsc", bAsc, true);
	g_dp.GetLeftFileList().SetSort(nCol, bAsc);
	file.GetValue("RightPanel", "SortCol", nCol);
	file.GetValue("RightPanel", "SortAsc", bAsc, true);
	g_dp.GetRightFileList().SetSort(nCol, bAsc);

	//set panel column widths
	String strKey;
	int nWidth = 0;
	for(int i=0; i<5; i++)
	{
		strKey.Printf("WidthCol%d", i+1);

		file.GetValue("LeftPanel", strKey, nWidth);
		if(nWidth > 0)
			gtk_tree_view_column_set_fixed_width(g_dp.GetLeftFileList().cols[i], nWidth);
		file.GetValue("RightPanel", strKey, nWidth);
		if(nWidth > 0)
			gtk_tree_view_column_set_fixed_width(g_dp.GetRightFileList().cols[i], nWidth);
	}

	//read custom file editor
	std::string strValue;
	file.GetValue("Operation", "FileEditor", strValue, "");
	g_strEditor = strValue.c_str();
	if(g_strEditor.IsEmpty())
	{
#ifdef _WIN32
		g_strEditor = "notepad.exe";
#else
		g_strEditor = "gedit";
#endif
	}

#ifndef _WIN32
	std::string strValue1;
	file.GetValue("Operation", "Terminal", strValue1, "");
	g_strTerminal = strValue1.c_str();
	if(g_strTerminal.IsEmpty())
	{
		g_strTerminal = "/usr/bin/gnome-terminal";
	}
#endif

}

GtkWidget* MainWindow::create_atol_main ()
{
	GtkWidget *atol_main;
	GtkWidget *vbox3;
	GtkWidget *menubar1;
	GtkWidget *toolbar1;
	GtkWidget *hpaned1;
	GtkWidget *vbox5;
	GtkWidget *vbox4;
	GtkWidget *statusbar1;
	GtkWidget *tracetab;
	GtkAccelGroup *accel_group;

	accel_group = gtk_accel_group_new ();
	
	//prepare version string
	String strApp(APP_NAME_STR);
	strApp += " v.";
	strApp += APP_VER_STR;

	atol_main = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (atol_main), strApp);
	gtk_window_set_default_size (GTK_WINDOW (atol_main), 600, 400); 
	
	vbox3 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox3);
	gtk_container_add (GTK_CONTAINER (atol_main), vbox3);
	
	menubar1 = gtk_menu_bar_new ();
	gtk_widget_show (menubar1);
	gtk_box_pack_start (GTK_BOX (vbox3), menubar1, FALSE, FALSE, 0);
	
	create_menu (atol_main, menubar1, accel_group);
	
	// create toolbar (initially hidden)
	toolbar1 = create_toolbar();
	gtk_box_pack_start (GTK_BOX (vbox3), toolbar1, FALSE, FALSE, 0);
	
	hpaned1 = gtk_hpaned_new ();
	gtk_widget_show (hpaned1);
	gtk_box_pack_start (GTK_BOX (vbox3), hpaned1, TRUE, TRUE, 0);
	gtk_paned_set_position (GTK_PANED(hpaned1), 300);
	
	//create file panels
	g_dp.CreatePanels();
	
	vbox5 = g_dp.GetLeftPanel().m_pWidget;
	gtk_widget_show (vbox5);
	gtk_paned_pack1 (GTK_PANED (hpaned1), vbox5, FALSE, TRUE);

	vbox4 = g_dp.GetRightPanel().m_pWidget;
	gtk_widget_show (vbox4);
	gtk_paned_pack2 (GTK_PANED (hpaned1), vbox4, TRUE, TRUE);

	//create (initially hidden trace bar)
	tracetab = gtk_notebook_new ();
	//gtk_widget_show (tracetab);
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(tracetab), (GtkPositionType)GTK_POS_BOTTOM);
	gtk_box_pack_start (GTK_BOX (vbox3), tracetab, FALSE, TRUE, 0);

	//TOFIX add trace views for local panes ?
	/*
	GtkWidget *scrolledwindow18;
	scrolledwindow18 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow18);
	GTK_WIDGET_UNSET_FLAGS(scrolledwindow18, GTK_CAN_FOCUS);
	gtk_container_add (GTK_CONTAINER (tracetab), scrolledwindow18);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow18), GTK_SHADOW_IN);

	GtkWidget *traceview = gtk_text_view_new ();
	gtk_widget_set_size_request(traceview, -1, 50);
	gtk_widget_set_sensitive(traceview, FALSE);
	GTK_WIDGET_UNSET_FLAGS(traceview, GTK_CAN_FOCUS);
	gtk_widget_show (traceview);
	gtk_container_add (GTK_CONTAINER (scrolledwindow18), traceview);

	GtkWidget *label35 = gtk_label_new (_("Local"));
	GTK_WIDGET_UNSET_FLAGS(label35, GTK_CAN_FOCUS);
	gtk_widget_show (label35);
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (tracetab), gtk_notebook_get_nth_page (GTK_NOTEBOOK (tracetab), 0), label35);
	*/

	//create command line entry with history
	GtkWidget *cmdlinecbo = gtk_combo_box_entry_new_text (); //initially invisible (set by ini value)
#if GTK_CHECK_VERSION(2,6,0)
	gtk_combo_box_set_focus_on_click(GTK_COMBO_BOX(cmdlinecbo), FALSE);
#endif
	gtk_box_pack_start (GTK_BOX (vbox3), cmdlinecbo, FALSE, FALSE, 0);
	g_signal_connect (GTK_ENTRY (GTK_BIN (cmdlinecbo)->child), "key_press_event", G_CALLBACK(cmdline_keyboard_handler), this);
	m_pCmdLineWidget = cmdlinecbo;

	statusbar1 = gtk_statusbar_new (); //initially invisible (set by ini value)
	gtk_box_pack_start (GTK_BOX (vbox3), statusbar1, FALSE, FALSE, 0);
	
	gtk_widget_add_events(hpaned1, GDK_EXPOSURE_MASK);
	g_signal_connect ( hpaned1, "notify::position", G_CALLBACK (splitter_pos_changed_cb), hpaned1);
	g_signal_connect ( atol_main, "configure_event", G_CALLBACK (window_size_event),  NULL); 
	g_signal_connect ( atol_main, "window_state_event", G_CALLBACK (window_state_event),  NULL); 

	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (atol_main, atol_main, "atol_main");
	GLADE_HOOKUP_OBJECT (atol_main, vbox3, "vbox3");
	GLADE_HOOKUP_OBJECT (atol_main, menubar1, "menubar1");
	GLADE_HOOKUP_OBJECT (atol_main, toolbar1, "toolbar1");
	GLADE_HOOKUP_OBJECT (atol_main, hpaned1, "hpaned1");
	GLADE_HOOKUP_OBJECT (atol_main, vbox5, "vbox5");
	GLADE_HOOKUP_OBJECT (atol_main, vbox4, "vbox4");
	GLADE_HOOKUP_OBJECT (atol_main, statusbar1, "statusbar1");
	GLADE_HOOKUP_OBJECT (atol_main, cmdlinecbo, "cmdlinecbo");
//	GLADE_HOOKUP_OBJECT (atol_main, traceview, "traceview");
	GLADE_HOOKUP_OBJECT (atol_main, tracetab, "tracetab");
	
	gtk_window_add_accel_group (GTK_WINDOW (atol_main), accel_group);

	g_dp.InitVfs(); //initial setup

	//load archiver plugins
	//TOFIX separate method ?
#ifdef _WIN32
	String strPluginDir = PathName::GetParentDirPath(System::GetAppPath().c_str()).c_str();
	strPluginDir += "plugins";
#else
	String strPluginDir = PLUGIN_DIR;	// fixed predefined directory (see config.h)
#endif
	g_PlugManager.LoadPlugins(strPluginDir);

#ifdef _WIN32
#else
	//set window/application icon
	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&atol_xpm);
	gtk_window_set_icon(GTK_WINDOW(atol_main), pixbuf);
	g_object_unref(pixbuf);
#endif

	g_dp.GetLeftPanel().Activate(); //activate left panel

	return atol_main;
}

GtkWidget* MainWindow::create_toolbar()
{
	GtkWidget *toolitem1;
	GtkWidget *button3; 
	GtkWidget *ftpcombo; 
	GtkIconSize tmp_toolbar_icon_size;

	GtkWidget *toolbar1 = gtk_toolbar_new (); //initialy invisible (set by .ini value)
	gtk_toolbar_set_style (GTK_TOOLBAR (toolbar1), GTK_TOOLBAR_ICONS);
	gtk_toolbar_set_icon_size (GTK_TOOLBAR (toolbar1), GTK_ICON_SIZE_SMALL_TOOLBAR); 
	tmp_toolbar_icon_size = gtk_toolbar_get_icon_size (GTK_TOOLBAR (toolbar1));
		
	gtk_toolbar_set_tooltips (GTK_TOOLBAR (toolbar1), TRUE );
	GtkTooltips *tips = gtk_tooltips_new ();

	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&refresh_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	//g_object_unref(G_OBJECT(pixbuf));
	//GTK_WIDGET_SET_FLAGS (button3, GTK_NO_WINDOW);
	gtk_widget_show (button3);
	g_object_unref(G_OBJECT(pixbuf));
	
	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Refresh"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Refresh"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_refresh_activate), NULL);
	//g_object_set (G_OBJECT(toolitem1), "can-focus", FALSE, NULL);

	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&copy_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);

	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Copy"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Copy"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_copy1_activate), NULL);


	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&move_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);

	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Move"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Move"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_move1_activate), NULL);


	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&new_dir_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);
	
	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Make Directory"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Make Directory"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_mkdir1_activate), NULL);


	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&recycle_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);

	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Delete"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Delete"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_delete1_activate), NULL);


	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&terminal_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);

	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Terminal"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Open Terminal"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_open_terminal_activate), NULL);

	//append separator
	toolitem1 = (GtkWidget*) gtk_separator_tool_item_new();
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);


	button3 = gtk_image_new_from_stock (GTK_STOCK_GO_BACK, tmp_toolbar_icon_size);
	gtk_widget_show (button3);

#if GTK_CHECK_VERSION(2,6,0) //minimal version for new widget
	toolitem1 = (GtkWidget*) gtk_menu_tool_button_new (button3, _("Back"));
#else	
	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Back"));
#endif
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Back"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_history_back), 0);
	gtk_widget_set_sensitive(toolitem1, FALSE);
	//GLADE_HOOKUP_OBJECT (toolbar1, button3, "tbr_undo");

	m_pHistoryPrev = toolitem1;

	button3 = gtk_image_new_from_stock (GTK_STOCK_GO_FORWARD, tmp_toolbar_icon_size);
	gtk_widget_show (button3);

#if GTK_CHECK_VERSION(2,6,0) //minimal version for new widget	
	toolitem1 = (GtkWidget*) gtk_menu_tool_button_new (button3, _("Forward"));
#else	
	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Forward"));
#endif
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Forward"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_history_forward), 0);
	gtk_widget_set_sensitive(toolitem1, FALSE);
	//GLADE_HOOKUP_OBJECT (toolbar1, button3, "tbr_redo");

	m_pHistoryNext = toolitem1;

	//append space
	toolitem1 = (GtkWidget*) gtk_separator_tool_item_new();
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);


	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&ftp_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);

	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Remote connection"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Remote connection"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_remote_connection_activate), NULL);


	// initially invisible button
	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&close_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);

	// create button to close remote connection 
	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Close connection"));
	//gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Close connection"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_connection_close_activate), NULL);

	m_pConnectionClose = toolitem1; // store pointer

	//create combo to select FTP transfer type
	toolitem1 = (GtkWidget*) gtk_tool_item_new ();
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);

	ftpcombo = gtk_combo_box_new_text();
	//gtk_widget_show (ftpcombo);
	gtk_container_add (GTK_CONTAINER (toolitem1), ftpcombo);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Transfer mode"), NULL);
	g_signal_connect (GTK_OBJECT (ftpcombo), "changed", G_CALLBACK(on_transfer_type_combo_activate), NULL);
	
	gtk_combo_box_append_text(GTK_COMBO_BOX(ftpcombo), _("Automatic"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(ftpcombo), _("Text mode"));
	gtk_combo_box_append_text(GTK_COMBO_BOX(ftpcombo), _("Binary mode"));
	gtk_combo_box_set_active(GTK_COMBO_BOX(ftpcombo), 0);

	m_pTransferMode = ftpcombo;	//store pointer

	pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&help_xpm);
	button3 = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (button3);

	toolitem1 = (GtkWidget*) gtk_tool_button_new (button3, _("Help"));
	gtk_widget_show (toolitem1);
	gtk_container_add (GTK_CONTAINER (toolbar1), toolitem1);
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(toolitem1), tips, _("Help"), NULL);
	g_signal_connect (GTK_OBJECT (toolitem1), "clicked", G_CALLBACK (on_about1_activate), NULL);

	//g_object_unref(G_OBJECT(tips)); //TOFIX first assign reference to toolbar?
	return toolbar1;
}

void create_menu (GtkWidget *atol_main, GtkWidget *menubar1, GtkAccelGroup *accel_group)
{
	GtkWidget *menuitem4;
	GtkWidget *menu4;
	GtkWidget *view1;
	GtkWidget *newfile1;
	GtkWidget *showhidden1;
	GtkWidget *edit1;
	GtkWidget *copy1;
	GtkWidget *move1;
	GtkWidget *mkdir1;
	GtkWidget *rename1;
	GtkWidget *pack1;
	GtkWidget *unpack1;
	GtkWidget *split1;
	GtkWidget *merge1;
	GtkWidget *options1;
	GtkWidget *separatormenuitem1;
	GtkWidget *separatormenuitem2;
	GtkWidget *quit1;
	GtkWidget *menuitem5;
	GtkWidget *menu5;
	GtkWidget *selection;
	GtkWidget *homedir1;
	GtkWidget *menu_sel;
	GtkWidget *filter;
	GtkWidget *select_all;
	GtkWidget *select_none;
	GtkWidget *select_invert;
	GtkWidget *select_select;
	GtkWidget *select_deselect;
	GtkWidget *delete1;
	GtkWidget *search1;
	GtkWidget *encdec1;
	GtkWidget *terminal1;
	GtkWidget *hash1;
	GtkWidget *refresh1;
	GtkWidget *menuitem6;
	GtkWidget *menutool1;
	GtkWidget *menu6;
	GtkWidget *swap1;
	GtkWidget *equal1;
	GtkWidget *compare1;
	GtkWidget *menuitem7;
	GtkWidget *menu7;
	GtkWidget *about1;
	GtkWidget *icon;
	GtkWidget *menucmd1;
	GtkWidget *statbar1;
	GtkWidget *remote1;
	GtkWidget *menutrace1;

	menuitem4 = gtk_menu_item_new_with_mnemonic (_("_File"));
	gtk_widget_show (menuitem4);
	gtk_container_add (GTK_CONTAINER (menubar1), menuitem4);
	
	menu4 = gtk_menu_new ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem4), menu4);

	newfile1 = gtk_menu_item_new_with_mnemonic (_("_Create File"));
	gtk_widget_show (newfile1);
	gtk_widget_add_accelerator (newfile1, "activate", accel_group, GDK_F4, (GdkModifierType)GDK_SHIFT_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), newfile1);
	
	view1 = gtk_menu_item_new_with_mnemonic (_("_View"));
	gtk_widget_show (view1);
	gtk_widget_add_accelerator (view1, "activate", accel_group, GDK_F3, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), view1);
	
	edit1 = gtk_menu_item_new_with_mnemonic (_("_Edit"));
	gtk_widget_show (edit1);
	gtk_widget_add_accelerator (edit1, "activate", accel_group, GDK_F4, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), edit1);
	
	separatormenuitem1 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem1);
	gtk_container_add (GTK_CONTAINER (menu4), separatormenuitem1);
	gtk_widget_set_sensitive (separatormenuitem1, FALSE);

	copy1 = gtk_image_menu_item_new_with_mnemonic (_("_Copy"));
	gtk_widget_show (copy1);
	gtk_widget_add_accelerator (copy1, "activate", accel_group, GDK_F5, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), copy1);
	icon = gtk_image_new_from_stock (GTK_STOCK_COPY, GTK_ICON_SIZE_MENU);
	gtk_widget_show (icon);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (copy1), icon);

	move1 = gtk_menu_item_new_with_mnemonic (_("_Move"));
	gtk_widget_show (move1);
	gtk_widget_add_accelerator (move1, "activate", accel_group, GDK_F6, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), move1);
	
	mkdir1 = gtk_menu_item_new_with_mnemonic (_("_Make Directory"));
	gtk_widget_show (mkdir1);
	gtk_widget_add_accelerator (mkdir1, "activate", accel_group, GDK_F7, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), mkdir1);
	
	delete1 = gtk_image_menu_item_new_with_mnemonic (_("_Delete"));
	gtk_widget_show (delete1);
	//TOFIX this one seems illegal, how to display "Delete" as an accelerator?
	//gtk_widget_add_accelerator (delete1, "activate", accel_group, GDK_DELETE, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), delete1);
	icon = gtk_image_new_from_stock (GTK_STOCK_DELETE, GTK_ICON_SIZE_MENU);
	gtk_widget_show (icon);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (delete1), icon);

	rename1 = gtk_menu_item_new_with_mnemonic (_("_Rename"));
	gtk_widget_show (rename1);
	gtk_widget_add_accelerator (rename1, "activate", accel_group, GDK_F6, (GdkModifierType)GDK_SHIFT_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), rename1);

	separatormenuitem1 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem1);
	gtk_container_add (GTK_CONTAINER (menu4), separatormenuitem1);
	gtk_widget_set_sensitive (separatormenuitem1, FALSE);

	pack1 = gtk_menu_item_new_with_mnemonic (_("_Pack files"));
	gtk_widget_show (pack1);
	gtk_widget_add_accelerator (pack1, "activate", accel_group, '5', (GdkModifierType)GDK_MOD1_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), pack1);

	unpack1 = gtk_menu_item_new_with_mnemonic (_("_Unpack files"));
	gtk_widget_show (unpack1);
	gtk_widget_add_accelerator (unpack1, "activate", accel_group, GDK_F9, (GdkModifierType)GDK_SHIFT_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), unpack1);
	
	separatormenuitem2 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem2);
	gtk_container_add (GTK_CONTAINER (menu4), separatormenuitem2);
	gtk_widget_set_sensitive (separatormenuitem2, FALSE);

	quit1 = gtk_image_menu_item_new_with_mnemonic (_("_Quit"));
	gtk_widget_show (quit1);
	gtk_widget_add_accelerator (quit1, "activate", accel_group, 'Q', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu4), quit1);
	icon = gtk_image_new_from_stock (GTK_STOCK_QUIT, GTK_ICON_SIZE_MENU);
	gtk_widget_show (icon);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (quit1), icon);

	menuitem6 = gtk_menu_item_new_with_mnemonic (_("_View"));
	gtk_widget_show (menuitem6);
	gtk_container_add (GTK_CONTAINER (menubar1), menuitem6);
	
	menu6 = gtk_menu_new ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem6), menu6);

	refresh1 = gtk_image_menu_item_new_with_mnemonic (_("_Refresh"));
	gtk_widget_show (refresh1);
	gtk_widget_add_accelerator (refresh1, "activate", accel_group, GDK_F2, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu6), refresh1);
	icon = gtk_image_new_from_stock (GTK_STOCK_REFRESH, GTK_ICON_SIZE_MENU);
	gtk_widget_show (icon);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (refresh1), icon);

	swap1 = gtk_menu_item_new_with_mnemonic (_("_Swap panels"));
	gtk_widget_show (swap1);
	gtk_widget_add_accelerator (swap1, "activate", accel_group, 'U', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu6), swap1);

	equal1 = gtk_menu_item_new_with_mnemonic (_("_Equal panels"));
	gtk_widget_show (equal1);
	gtk_widget_add_accelerator (equal1, "activate", accel_group, 'E', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu6), equal1);

	homedir1 = gtk_menu_item_new_with_mnemonic (_("_Home dir"));
	gtk_widget_show (homedir1);
	gtk_widget_add_accelerator (homedir1, "activate", accel_group, GDK_Home, (GdkModifierType)GDK_SHIFT_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu6), homedir1);

	separatormenuitem1 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem1);
	gtk_container_add (GTK_CONTAINER (menu6), separatormenuitem1);
	gtk_widget_set_sensitive (separatormenuitem1, FALSE);
	
	menutool1 = gtk_check_menu_item_new_with_label (_("Tool Bar"));
	gtk_check_menu_item_set_show_toggle(GTK_CHECK_MENU_ITEM(menutool1), TRUE);	//show check mark
	gtk_widget_show (menutool1);
	gtk_container_add (GTK_CONTAINER (menu6), menutool1);

	menucmd1 = gtk_check_menu_item_new_with_label (_("Command Line"));
	gtk_check_menu_item_set_show_toggle(GTK_CHECK_MENU_ITEM(menucmd1), TRUE);	//show check mark
	gtk_widget_show (menucmd1);
	//gtk_widget_set_sensitive(menucmd1, FALSE); //TOFIX temprary
	gtk_container_add (GTK_CONTAINER (menu6), menucmd1);
	
	menutrace1 = gtk_check_menu_item_new_with_label (_("Trace View"));
	gtk_check_menu_item_set_show_toggle(GTK_CHECK_MENU_ITEM(menutrace1), TRUE);	//show check mark
	gtk_widget_show (menutrace1);
	gtk_container_add (GTK_CONTAINER (menu6), menutrace1);

	statbar1= gtk_check_menu_item_new_with_label (_("Status Bar"));
	gtk_check_menu_item_set_show_toggle(GTK_CHECK_MENU_ITEM(statbar1), TRUE);	//show check mark
	gtk_widget_show (statbar1);
	gtk_container_add (GTK_CONTAINER (menu6), statbar1);

	showhidden1 = gtk_check_menu_item_new_with_label (_("Show hidden files"));
	gtk_check_menu_item_set_show_toggle(GTK_CHECK_MENU_ITEM(showhidden1), TRUE);	//show check mark
	gtk_check_menu_item_set_state(GTK_CHECK_MENU_ITEM(showhidden1),true);
	gtk_widget_show (showhidden1);
	gtk_container_add (GTK_CONTAINER (menu6), showhidden1);

	filter = gtk_menu_item_new_with_mnemonic (_("_Filter"));
	gtk_widget_add_accelerator (filter, "activate", accel_group, 'D', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_widget_show (filter);
	gtk_container_add (GTK_CONTAINER (menu6), filter);

	selection = gtk_menu_item_new_with_mnemonic (_("_Selection"));
	gtk_widget_show (selection);
	gtk_container_add (GTK_CONTAINER (menu6), selection);

	menu_sel = gtk_menu_new ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (selection), menu_sel);

	select_all = gtk_menu_item_new_with_mnemonic (_("_All"));
	gtk_widget_show (select_all);
	gtk_container_add (GTK_CONTAINER (menu_sel), select_all);

	select_none = gtk_menu_item_new_with_mnemonic (_("_None"));
	gtk_widget_show (select_none);
	gtk_container_add (GTK_CONTAINER (menu_sel), select_none);

	select_invert = gtk_menu_item_new_with_mnemonic (_("_Invert"));
	gtk_widget_show (select_invert);
	gtk_container_add (GTK_CONTAINER (menu_sel), select_invert);

	select_select = gtk_menu_item_new_with_mnemonic (_("_Select"));
	gtk_widget_show (select_select);
	//gtk_widget_add_accelerator (rename1, "activate", accel_group, '+', (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu_sel), select_select);

	select_deselect = gtk_menu_item_new_with_mnemonic (_("_Deselect"));
	gtk_widget_show (select_deselect);
	//gtk_widget_add_accelerator (rename1, "activate", accel_group, '-', (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu_sel), select_deselect);

	menuitem5 = gtk_menu_item_new_with_mnemonic (_("_Advanced"));
	gtk_widget_show (menuitem5);
	gtk_container_add (GTK_CONTAINER (menubar1), menuitem5);
	
	menu5 = gtk_menu_new ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem5), menu5);
	
	search1 = gtk_menu_item_new_with_mnemonic (_("_File Search"));
	gtk_widget_show (search1);
	gtk_widget_add_accelerator (search1, "activate", accel_group, 'F', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), search1);
	
	compare1 = gtk_menu_item_new_with_mnemonic (_("_Compare directories"));
	gtk_widget_show (compare1);
	gtk_widget_add_accelerator (compare1, "activate", accel_group, GDK_F2, (GdkModifierType)GDK_SHIFT_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), compare1);
	
	terminal1 = gtk_menu_item_new_with_mnemonic (_("_Open Terminal"));
	gtk_widget_show (terminal1);
	gtk_widget_add_accelerator (terminal1, "activate", accel_group, GDK_F3, (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), terminal1);

	hash1 = gtk_menu_item_new_with_mnemonic (_("Calculate _Hash"));
	gtk_widget_show (hash1);
	gtk_widget_add_accelerator (hash1, "activate", accel_group, 'H', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), hash1);

	split1 = gtk_menu_item_new_with_mnemonic (_("Split file"));
	gtk_widget_show (split1);
	gtk_widget_add_accelerator (split1, "activate", accel_group, 'S', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), split1);

	merge1 = gtk_menu_item_new_with_mnemonic (_("Merge files"));
	gtk_widget_show (merge1);
	gtk_widget_add_accelerator (merge1, "activate", accel_group, 'M', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), merge1);

	encdec1 = gtk_menu_item_new_with_mnemonic (_("Encrypt/decrypt files"));
	gtk_widget_show (encdec1);
	gtk_widget_add_accelerator (encdec1, "activate", accel_group, GDK_F9, (GdkModifierType)0, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), encdec1);

	remote1 = gtk_menu_item_new_with_mnemonic (_("Remote connection"));
	gtk_widget_show (remote1);
	gtk_widget_add_accelerator (remote1, "activate", accel_group, 'R', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), remote1);

	separatormenuitem1 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem1);
	gtk_container_add (GTK_CONTAINER (menu5), separatormenuitem1);
	gtk_widget_set_sensitive (separatormenuitem1, FALSE);
	
	options1 = gtk_image_menu_item_new_with_mnemonic (_("_Options"));
	gtk_widget_show (options1);
	//gtk_widget_set_sensitive(options1, FALSE);	//TOFIX until implemented
	gtk_widget_add_accelerator (options1, "activate", accel_group, 'O', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu5), options1);
	icon = gtk_image_new_from_stock (GTK_STOCK_PREFERENCES, GTK_ICON_SIZE_MENU);
	gtk_widget_show (icon);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (options1), icon);

	menuitem7 = gtk_menu_item_new_with_mnemonic (_("_Help"));
	gtk_widget_show (menuitem7);
	gtk_container_add (GTK_CONTAINER (menubar1), menuitem7);
	
	menu7 = gtk_menu_new ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem7), menu7);
	
	about1 = gtk_image_menu_item_new_with_mnemonic (_("_About"));
	gtk_widget_show (about1);
	gtk_container_add (GTK_CONTAINER (menu7), about1);
	icon = gtk_image_new_from_stock (GTK_STOCK_HELP, GTK_ICON_SIZE_MENU);
	gtk_widget_show (icon);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (about1), icon);

	g_signal_connect ( refresh1,"activate", G_CALLBACK (on_refresh_activate), NULL);
	g_signal_connect ( newfile1, "activate", G_CALLBACK (on_view_create_file_activate), NULL);
	g_signal_connect ( edit1, "activate", G_CALLBACK (on_edit1_activate), NULL);
	g_signal_connect ( copy1, "activate", G_CALLBACK (on_copy1_activate), NULL);
	g_signal_connect ( move1, "activate", G_CALLBACK (on_move1_activate), NULL);
	g_signal_connect ( delete1, "activate", G_CALLBACK (on_delete1_activate), NULL);
	g_signal_connect ( mkdir1, "activate", G_CALLBACK (on_mkdir1_activate), NULL);
	g_signal_connect ( rename1, "activate", G_CALLBACK (on_rename1_activate), NULL);
	g_signal_connect ( quit1, "activate", G_CALLBACK (on_quit1_activate), NULL);
	g_signal_connect ( about1, "activate", G_CALLBACK (on_about1_activate), NULL);
	g_signal_connect ( menutool1,"activate", G_CALLBACK (on_show_toolbar_activate), NULL);
	g_signal_connect ( statbar1,"activate", G_CALLBACK (on_show_status_bar_activate), NULL);
	g_signal_connect ( terminal1,"activate", G_CALLBACK (on_open_terminal_activate), NULL);
	g_signal_connect ( showhidden1,"activate", G_CALLBACK (on_show_hidden_activate), NULL);
	g_signal_connect ( filter, "activate", G_CALLBACK (on_filter_activate), NULL);
	g_signal_connect ( select_all, "activate", G_CALLBACK (on_select_all_activate), NULL);
	g_signal_connect ( select_none, "activate", G_CALLBACK (on_select_none_activate), NULL);
	g_signal_connect ( select_invert,"activate", G_CALLBACK (on_select_invert_activate), NULL);
	g_signal_connect ( select_select,"activate", G_CALLBACK (on_select_select_activate), NULL);
	g_signal_connect ( select_deselect,"activate", G_CALLBACK (on_select_deselect_activate), NULL);
	g_signal_connect ( swap1,"activate", G_CALLBACK (on_swap_panels), NULL);
	g_signal_connect ( equal1,"activate", G_CALLBACK (on_equal_panels), NULL);
	g_signal_connect ( compare1,"activate", G_CALLBACK (on_compare_panels), NULL);
	g_signal_connect ( options1,"activate", G_CALLBACK (on_options_activate), NULL);
	g_signal_connect ( search1,"activate", G_CALLBACK (on_file_search_activate), NULL);
	g_signal_connect ( pack1,"activate", G_CALLBACK (on_file_pack_activate), NULL);
	g_signal_connect ( unpack1,"activate", G_CALLBACK (on_file_unpack_activate), NULL);
	g_signal_connect ( hash1,"activate", G_CALLBACK (on_file_hash_activate), NULL);
	g_signal_connect ( split1,"activate", G_CALLBACK (on_file_split_activate), NULL);
	g_signal_connect ( merge1,"activate", G_CALLBACK (on_file_merge_activate), NULL);
	g_signal_connect ( encdec1,"activate", G_CALLBACK (on_file_encrypt_decrypt_activate), NULL);
	g_signal_connect ( remote1,"activate", G_CALLBACK (on_remote_connection_activate), NULL);
	g_signal_connect ( view1,"activate", G_CALLBACK (on_file_view_activate), NULL);
	g_signal_connect ( menucmd1,"activate", G_CALLBACK (on_show_command_line_activate), NULL);
	g_signal_connect ( menutrace1,"activate", G_CALLBACK (on_show_trace_view_activate), NULL);
	g_signal_connect ( homedir1,"activate", G_CALLBACK (on_go_user_home_directory), NULL);

	GLADE_HOOKUP_OBJECT (atol_main, menuitem4, "menuitem4");
	GLADE_HOOKUP_OBJECT (atol_main, menu4, "menu4");
	GLADE_HOOKUP_OBJECT (atol_main, separatormenuitem1, "separatormenuitem1");
	GLADE_HOOKUP_OBJECT (atol_main, quit1, "quit1");
	GLADE_HOOKUP_OBJECT (atol_main, menuitem5, "menuitem5");
	GLADE_HOOKUP_OBJECT (atol_main, menu5, "menu5");
	GLADE_HOOKUP_OBJECT (atol_main, refresh1, "refresh1");
	GLADE_HOOKUP_OBJECT (atol_main, copy1, "copy1");
	GLADE_HOOKUP_OBJECT (atol_main, delete1, "delete1");
	GLADE_HOOKUP_OBJECT (atol_main, menuitem6, "menuitem6");
	GLADE_HOOKUP_OBJECT (atol_main, menutool1, "menutool1");
	GLADE_HOOKUP_OBJECT (atol_main, menu6, "menu6");
	GLADE_HOOKUP_OBJECT (atol_main, menuitem7, "menuitem7");
	GLADE_HOOKUP_OBJECT (atol_main, menu7, "menu7");
	GLADE_HOOKUP_OBJECT (atol_main, about1, "about1");
	GLADE_HOOKUP_OBJECT (atol_main, menucmd1, "menucmd1");
	GLADE_HOOKUP_OBJECT (atol_main, menutrace1, "menutrace1");
	GLADE_HOOKUP_OBJECT (atol_main, statbar1, "statbar1");
	GLADE_HOOKUP_OBJECT (atol_main, showhidden1, "showhidden1");
	GLADE_HOOKUP_OBJECT (atol_main, terminal1, "terminal1");
	GLADE_HOOKUP_OBJECT (atol_main, pack1, "pack1");
	GLADE_HOOKUP_OBJECT (atol_main, unpack1, "unpack1");
	GLADE_HOOKUP_OBJECT (atol_main, hash1, "hash1");
	GLADE_HOOKUP_OBJECT (atol_main, split1, "split1");
	GLADE_HOOKUP_OBJECT (atol_main, merge1, "merge1");
	GLADE_HOOKUP_OBJECT (atol_main, encdec1, "encdec1");
	GLADE_HOOKUP_OBJECT (atol_main, remote1, "remote1");
	GLADE_HOOKUP_OBJECT (atol_main, homedir1, "homedir1");
}

static void splitter_pos_changed_cb (GObject * gobject, GParamSpec * arg1, gpointer user_data)
{
	//refresh splitter percent value
	gint max = 0;
	gdk_window_get_size(GTK_WIDGET(gobject)->window, &max, NULL);
	gint cur = gtk_paned_get_position (GTK_PANED (gobject));
	g_fSplitterPercent = (float)cur / max;
}

gboolean window_size_event (GtkWidget *widget, GdkEventConfigure *event)
{
	static gint ox=-1, oy=-1;
	
	if (event->type == GDK_CONFIGURE) 
	{
		if (event->width != ox || event->height != oy)
		{	
			//on resize restore splitter percent
			GtkWidget *splitter = lookup_widget(atol_main, "hpaned1");
			gint max = 0;
			gint cur = 0;
			
			gdk_window_get_size(splitter->window, &max, NULL);
			cur = (gint)(g_fSplitterPercent * max);

			gtk_paned_set_position(GTK_PANED(splitter), cur);

			//remember new window size
			ox = event->width;
			oy = event->height;
		}
    }
	return FALSE;
}

gboolean window_state_event (GtkWidget *widget, GdkEventWindowState *event)
{
	if( event->changed_mask & GDK_WINDOW_STATE_ICONIFIED	 &&
		event->new_window_state & GDK_WINDOW_STATE_ICONIFIED )
	{
		//on window minimized, show tray icon
		if(g_bMinimizeToTray)
		{
			g_tray.Show();
			g_tray.SetTooltip("Atol");
			gtk_widget_hide(atol_main);
		}
	}

	return FALSE;
}

gint cmdline_keyboard_handler(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	//catch enter key press
	if( GDK_KEY_PRESS == event->type    &&
		'\r' == event->hardware_keycode )
	{
		const gchar* szText = gtk_entry_get_text (GTK_ENTRY(widget));
		if( g_dp.GetActiveFileList().m_ctrl.m_pVfs->Execute(szText) )
		{
			const int nMaxComboEntries = 12;

			MainWindow *pWnd = (MainWindow *)data;
			GtkWidget *cmdlinecbo = lookup_widget(pWnd->m_pWidget, "cmdlinecbo");

			//add command into the combo box (history)
			gtk_combo_box_insert_text(GTK_COMBO_BOX(cmdlinecbo), 0, szText);

			//calculate number of items in the combo
			GtkTreeModel *model = gtk_combo_box_get_model(GTK_COMBO_BOX(cmdlinecbo));
			int nCount = gtk_tree_model_iter_n_children(model, NULL);

			//if new string already exists in the combo, remove the older duplicate
			for(int i=1; i<nCount; i++)
			{
				GtkTreeIter  iter;
				if(!gtk_tree_model_iter_nth_child  (model, &iter, NULL, i))
					break;

				//get combo box entry text
				GValue value = {0,};
				gtk_tree_model_get_value(model, &iter, 0, &value);
				const gchar* szText1 = g_value_get_string (&value);

				if( NULL != szText1 &&
					0 == strcmp(szText1, szText))
				{
					gtk_combo_box_remove_text(GTK_COMBO_BOX(cmdlinecbo), i);
					nCount --;
					break;
				}
			}


			//do we need to remove one item too many
			if(nCount > nMaxComboEntries)
				gtk_combo_box_remove_text(GTK_COMBO_BOX(cmdlinecbo), nCount-1);
		}

		//clear text from command line
		gtk_entry_set_text(GTK_ENTRY(widget), "");
	}

	return FALSE;
}

void MainWindow::AddToCommandLine(const char *szText)
{
	GtkWidget *cmdlinecbo = lookup_widget(m_pWidget, "cmdlinecbo");
	GtkEntry  *entry = GTK_ENTRY (GTK_BIN (cmdlinecbo)->child);

	String strEntry;
	strEntry = gtk_entry_get_text (entry);
	strEntry += szText;
	gtk_entry_set_text(entry, strEntry.c_str());

	gtk_widget_grab_focus (GTK_WIDGET(entry));
}

