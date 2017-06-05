////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Contains mostly callback handler functions
//////////////////////////////////////////////////////////////////////////// 

#include <gtk/gtk.h>

#if _MSC_VER > 1000
  #pragma warning(disable:4786)
#endif
#ifdef _WIN32
  #include <io.h>
  #define access _access
#else
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <unistd.h>
#endif
#include <stdlib.h>

#include "callbacks.h"
#include "support.h"
#include "core/opcodes.h"
#include "core/System.h"
#include "core/GuiLanguage.h"
#include "core/String.h"
#include "core/IniFile.h"
#include "core/util.h"
#include "DualPanel.h"
#include "core/VfsManager.h"
#include "OptionsDialog.h"
#include "FileSearchDlg.h"
#include "PackFilesDlg.h"
#include "core/PluginManager.h"
#include "core/PathName.h"
#include "HashTypeDialog.h"
#include "HashResultDialog.h"
#include "FileMergeDialog.h"
#include "FileSplitDialog.h"
#include "EncryptionDialog.h"
#include "viewer/FileViewer.h"
#include "core/VfsArchive.h"
#include "MainWindow.h"
#include "SiteManagerDlg.h"
#include "core/ConnectionInfoList.h"
#include "core/VfsRemote.h"
#include "core/VfsFtp.h"


int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
const char *GetIniFile();
extern GtkWidget *atol_main;
extern DualPanel g_dp;
extern VfsManager g_VfsManager;
extern GuiLanguage g_lang;
extern PluginManager g_PlugManager;
extern OpManager  g_objOpManager;
extern MainWindow g_wnd;
extern bool g_bShowDirWarningDialog;
extern bool g_bShowReadOnlyWarningDialog;
bool g_bRefreshPanelsOnFocus = true;
void show_hidden(bool bShow);
void DoRemoteConnect(const char *szSiteTitle);
void DoRemoteConnect(CNodeInfo &info);

void on_refresh_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	// F2 -> refresh list
	FileList &list = g_dp.GetActiveFileList();
	
	g_dp.GetActiveFileList().StopNotifications(list.m_ctrl.m_pVfs, list.m_pOtherList->m_ctrl.m_pVfs);
	g_dp.GetActiveFileList().PanelList(true, true);
	g_dp.GetActiveFileList().StartNotifications(list.m_ctrl.m_pVfs, list.m_pOtherList->m_ctrl.m_pVfs);
}

void on_edit1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	// F4 -> edit file
	g_dp.GetActiveFileList().EditFile();
}

void on_copy1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	// F5 -> file copy
	g_dp.GetActiveFileList().Copy();
}

void on_move1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	// F6 -> move list
	g_dp.GetActiveFileList().Move();
}

void on_delete1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	// Del -> delete list
	g_dp.GetActiveFileList().Delete();
}

void on_mkdir1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	// F7 -> make directory
	g_dp.GetActiveFileList().MkDir();
}

void on_rename1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	//Shift + F6 -> inplace rename item
	g_dp.GetActiveFileList().Rename();
}

void on_quit1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	//save some options
	IniFile file;
	file.Load(GetIniFile());
	file.SetValue("Display", "ShowToolBar", get_show_toolbar());
	file.SetValue("Display", "ShowStatusBar", get_show_status_bar());
	file.SetValue("Display", "ShowHiddenFiles", get_show_hidden());
	file.SetValue("Display", "ShowCommandLine", get_show_command_line());
	file.SetValue("Display", "ShowTraceView", get_show_trace_view());

	file.SetValue("Operation", "DeleteDirWarning", g_bShowDirWarningDialog);
	file.SetValue("Operation", "ReadOnlyWarning", g_bShowReadOnlyWarningDialog);

	//store last path only for Local VFS
	if(g_dp.GetLeftFileList().m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
		file.SetValue("LeftPanel", "Path", g_dp.GetLeftFileList().m_ctrl.m_pVfs->GetDir());
	if(g_dp.GetRightFileList().m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
		file.SetValue("RightPanel", "Path", g_dp.GetRightFileList().m_ctrl.m_pVfs->GetDir());

	//remember sorting
	file.SetValue("LeftPanel", "SortCol",  g_dp.GetLeftFileList().m_ctrl.m_nSortColumn);
	file.SetValue("LeftPanel", "SortAsc",  g_dp.GetLeftFileList().m_ctrl.m_bSortAscending);
	file.SetValue("RightPanel", "SortCol", g_dp.GetRightFileList().m_ctrl.m_nSortColumn);
	file.SetValue("RightPanel", "SortAsc", g_dp.GetRightFileList().m_ctrl.m_bSortAscending);

	//remember panel column widths
	String strKey;
	int nWidth = 0;
	for(int i=0; i<5; i++)
	{
		strKey.Printf("WidthCol%d", i+1);

		nWidth = gtk_tree_view_column_get_width(g_dp.GetLeftFileList().cols[i]);
		file.SetValue("LeftPanel", strKey, nWidth);
			
		nWidth = gtk_tree_view_column_get_width(g_dp.GetRightFileList().cols[i]);
		file.SetValue("RightPanel", strKey, nWidth);
	}

	file.Save();

	//clear Vfs stacks
	g_VfsManager.ClearVfsList();

	gtk_main_quit();
}

void on_about1_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *dialog;
	GtkWidget *label1, *label, *okay_button, *table;

	/* Create the widgets */
	dialog = gtk_dialog_new();
	gtk_window_set_title(GTK_WINDOW(dialog), _("About Atol"));
	gtk_dialog_set_has_separator(GTK_DIALOG(dialog), false);

	table = gtk_table_new(2, 1, true);
	gtk_container_add (GTK_CONTAINER (GTK_DIALOG(dialog)->vbox), table);

	//prepare version string
	String strMsg(APP_NAME_STR);
	strMsg += " v.";
	strMsg += APP_VER_STR;

	label1= gtk_label_new (strMsg);
	gtk_misc_set_alignment(GTK_MISC(label1), 0, 0.5);
	gtk_table_attach(GTK_TABLE(table), label1, 0, 1, 0, 1, (GtkAttachOptions)(GTK_EXPAND|GTK_SHRINK|GTK_FILL), (GtkAttachOptions)0, 0, 3);

	//TOFIX put in eventbox to handle click
	label = gtk_label_new ("<span foreground=\"blue\"underline=\"single\">http://atol.sourceforge.net</span>");
	gtk_label_set_use_markup(GTK_LABEL(label), true);
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 1, 2, (GtkAttachOptions)(GTK_EXPAND|GTK_SHRINK|GTK_FILL), (GtkAttachOptions)0, 0, 3);

	okay_button = gtk_button_new_with_label("OK");
   
	/* Ensure that the dialog box is destroyed when the user clicks ok. */
	/*
	gtk_signal_connect_object (GTK_OBJECT (okay_button), "clicked",
                              GTK_SIGNAL_FUNC (gtk_widget_destroy), dialog);
	gtk_container_add (GTK_CONTAINER (GTK_DIALOG(dialog)->action_area),
                      okay_button);
	*/

	/* Add the label, and show everything we've added to the dialog. */
	gtk_widget_show_all (dialog);

	/*
	//append locale string
	strMsg += "\n\n(locale: ";
	strMsg += GuiLanguage::QueryLocale();
	strMsg += ")";
	*/
}

void on_show_toolbar_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *menutool1 = lookup_widget(atol_main, "menutool1");
	GtkWidget *toolbar1 = lookup_widget(atol_main, "toolbar1");

	if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menutool1)))
		gtk_widget_show(toolbar1);
	else
		gtk_widget_hide(toolbar1);
}

void refresh_toolbar_menu(bool bActivate)
{
	GtkWidget *menutool1 = lookup_widget(atol_main, "menutool1");
	if(bActivate)
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menutool1), TRUE);
	else
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menutool1), FALSE);
}

void set_show_toolbar(bool bShow)
{
	bool bIsVisible = get_show_toolbar();

	if((bShow && !bIsVisible) || (!bShow && bIsVisible))
		refresh_toolbar_menu(bShow); //this will trigger the signal
}

bool get_show_toolbar()
{
	GtkWidget *toolbar1 = lookup_widget(atol_main, "toolbar1");
	return GTK_WIDGET_VISIBLE(toolbar1);
}

void on_show_status_bar_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *menu = lookup_widget(atol_main, "statbar1");
	GtkWidget *statusbar1 = lookup_widget(atol_main, "statusbar1");

	if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menu)))
		gtk_widget_show(statusbar1);
	else
		gtk_widget_hide(statusbar1);
}

void refresh_statbar_menu (bool bActivate)
{
	GtkWidget *menutool1 = lookup_widget(atol_main, "statbar1");
	if(bActivate)
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menutool1), TRUE);
	else
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menutool1), FALSE);
}

void set_show_status_bar(bool bShow)
{
	bool bIsVisible = get_show_status_bar();

	if((bShow && !bIsVisible) || (!bShow && bIsVisible))
		refresh_statbar_menu(bShow); //this will trigger the signal
}

bool get_show_status_bar()
{
	GtkWidget *toolbar1 = lookup_widget(atol_main, "statusbar1");
	return GTK_WIDGET_VISIBLE(toolbar1);
}

void refresh_command_line_menu (bool bActivate)
{
	GtkWidget *menucmd1 = lookup_widget(atol_main, "menucmd1");
	if(bActivate)
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menucmd1), TRUE);
	else
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menucmd1), FALSE);
}

void set_show_command_line(bool bShow)
{
	bool bIsVisible = get_show_command_line();

	if((bShow && !bIsVisible) || (!bShow && bIsVisible))
		refresh_command_line_menu(bShow); //this will trigger the signal
}

bool get_show_command_line()
{
	GtkWidget *toolbar1 = lookup_widget(atol_main, "cmdlinecbo");
	return GTK_WIDGET_VISIBLE(toolbar1);
}

void on_show_command_line_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *menu = lookup_widget(atol_main, "menucmd1");
	GtkWidget *cmdlinecbo = lookup_widget(atol_main, "cmdlinecbo");

	if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menu)))
		gtk_widget_show(cmdlinecbo);
	else
		gtk_widget_hide(cmdlinecbo);
}

void refresh_trace_view_menu (bool bActivate)
{
	GtkWidget *menutrace1 = lookup_widget(atol_main, "menutrace1");
	if(bActivate)
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menutrace1), TRUE);
	else
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menutrace1), FALSE);
}

void set_show_trace_view(bool bShow)
{
	bool bIsVisible = get_show_trace_view();

	if((bShow && !bIsVisible) || (!bShow && bIsVisible))
		refresh_trace_view_menu(bShow); //this will trigger the signal
}

bool get_show_trace_view()
{
	GtkWidget *tracetab = lookup_widget(atol_main, "tracetab");
	return GTK_WIDGET_VISIBLE(tracetab);
}

void on_show_trace_view_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *menu = lookup_widget(atol_main, "menutrace1");
	GtkWidget *tracetab = lookup_widget(atol_main, "tracetab");

	if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menu)))
		gtk_widget_show(tracetab);
	else
		gtk_widget_hide(tracetab);
}

void refresh_hidden_menu (bool bActivate)
{
	GtkWidget *showhidden1 = lookup_widget(atol_main, "showhidden1");
	if(bActivate)
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(showhidden1), TRUE);
	else
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(showhidden1), FALSE);
}

void on_show_hidden_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *showhidden1 = lookup_widget(atol_main, "showhidden1");
	if(gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(showhidden1)))
		show_hidden(true);
	else
		show_hidden(false);
}

void set_show_hidden(bool bShow)
{
	//if needed refresh menu status, that will trigger the action itself
	bool bIsVisible = get_show_hidden();
	if((bShow && !bIsVisible) || (!bShow && bIsVisible))
		refresh_hidden_menu(bShow); //this will trigger the signal
}

void show_hidden(bool bShow)
{
	//"hide" command affects both file panels
#ifdef _WIN32
	unsigned short nAttrShow1 = g_dp.GetLeftPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().GetAttrShow(); //"must have" attributes
	unsigned short nAttrShow2 = g_dp.GetRightPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().GetAttrShow(); //"must have" attributes

	unsigned short nAttrHide1 = g_dp.GetLeftPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().GetAttrHide(); //"must not have" attributes
	unsigned short nAttrHide2 = g_dp.GetRightPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().GetAttrHide(); //"must not have" attributes

	if(bShow){
		nAttrHide1 &= ~(ATTR_HIDDEN); //remove flag to hide hidden files (not hidden anymore)
		nAttrHide2 &= ~(ATTR_HIDDEN); //remove flag to hide hidden files (not hidden anymore)
	}
	else{
		nAttrHide1 |= ATTR_HIDDEN; //hide hidden files
		nAttrHide2 |= ATTR_HIDDEN; //hide hidden files
	}

	g_dp.GetLeftPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().SetAttrGroup(nAttrShow1, nAttrHide1);
	g_dp.GetRightPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().SetAttrGroup(nAttrShow2, nAttrHide2);
#else
	static const String strWild(".*");
	if(bShow){
		g_dp.GetLeftPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().RemoveFromNameGroup(strWild, false);
		g_dp.GetRightPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().RemoveFromNameGroup(strWild, false);
	}
	else{
		g_dp.GetLeftPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().AddToNameGroup(strWild, false);
		g_dp.GetRightPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().AddToNameGroup(strWild, false);
	}
#endif

	//refilter the lists (no need to relist the VFS)
	g_dp.GetLeftPanel().m_wndFileList.RefilterList();	
	g_dp.GetRightPanel().m_wndFileList.RefilterList();
}

bool get_show_hidden()
{
#ifdef _WIN32
    //hidden files under Windows OS are files having hidden attribute flag set
    bool bHideLeft  = (ATTR_HIDDEN == (ATTR_HIDDEN & g_dp.GetLeftPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().GetAttrHide()));
    bool bHideRight = (ATTR_HIDDEN == (ATTR_HIDDEN & g_dp.GetRightPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().GetAttrHide()));
#else
    //"hidden" files under Linux are considered files whose name start with "."
    static const String strWild(".*");
    bool bHideLeft  = (0 <= g_dp.GetLeftPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().FindNameWildcard(strWild, false));
    bool bHideRight = (0 <= g_dp.GetRightPanel().m_wndFileList.m_ctrl.m_lstItems.GetFilter().FindNameWildcard(strWild, false));
#endif

    bool bFilesHidden = bHideLeft && bHideRight;
    return !bFilesHidden;
}

void on_open_terminal_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	String strDir;
	strDir = g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetDir();

	OpenCommandPrompt(strDir, NULL);
}

void on_select_all_activate	(GtkMenuItem *menuitem, gpointer user_data)
{
	g_dp.GetActiveFileList().SelectAll();
}

void on_select_none_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	g_dp.GetActiveFileList().SelectNone();
}

void on_select_invert_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	g_dp.GetActiveFileList().SelectInvert();
}

void on_select_select_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	g_dp.GetActiveFileList().SelectAdd();
}

void on_select_deselect_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	g_dp.GetActiveFileList().SelectRemove();
}

void on_filter_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	g_dp.GetActiveFileList().SetFilter();
}

void update_history_buttons()
{
	if(NULL == atol_main)
		return;	//main window not yet created

	GtkWidget *toolbar1 = lookup_widget(atol_main, "toolbar1");
	GtkToolItem *back = gtk_toolbar_get_nth_item (GTK_TOOLBAR(toolbar1), 7);
	GtkToolItem *forward = gtk_toolbar_get_nth_item (GTK_TOOLBAR(toolbar1), 8);

	BrowseHistoryList &list = g_dp.GetActiveFileList().m_ctrl.m_lstHistory;
	
	if(list.CanMovePrev())
		gtk_widget_set_sensitive(GTK_WIDGET(back), TRUE);
	else
		gtk_widget_set_sensitive(GTK_WIDGET(back), FALSE);

	if(list.CanMoveNext())
		gtk_widget_set_sensitive(GTK_WIDGET(forward), TRUE);
	else
		gtk_widget_set_sensitive(GTK_WIDGET(forward), FALSE);

	//create drop menu for each history button
	//TOFIX write handler for the menu that will be similar as on_history_back, ...
#if GTK_CHECK_VERSION(2,6,0) //minimal version for tool button
	GtkWidget *menu = NULL;
	GtkWidget *menu_item;
	unsigned int nCount = list.m_lstBackward.size();
	if(nCount > 0)
	{
		menu = gtk_menu_new();
		for(int i=nCount-1; i>=0; --i){
			menu_item = gtk_menu_item_new_with_label(list.m_lstBackward[i].c_str());
			g_signal_connect(menu_item, "activate",	G_CALLBACK (on_history_back), (gpointer)(nCount-i));
			gtk_menu_append(menu, menu_item);
			gtk_widget_show (menu_item);
		}
		gtk_widget_show (menu);
	}
	gtk_menu_tool_button_set_menu(GTK_MENU_TOOL_BUTTON(g_wnd.m_pHistoryPrev), menu);

	menu = NULL;
	nCount = list.m_lstForward.size();
	if(nCount > 0)
	{
		menu = gtk_menu_new();
		for(int i=nCount-1; i>=0; --i){
			menu_item = gtk_menu_item_new_with_label(list.m_lstForward[i].c_str());
			g_signal_connect(menu_item, "activate",	G_CALLBACK (on_history_forward), (gpointer)(nCount-i));
			gtk_menu_append(menu, menu_item);
			gtk_widget_show (menu_item);
		}
		gtk_widget_show (menu);
	}
	gtk_menu_tool_button_set_menu(GTK_MENU_TOOL_BUTTON(g_wnd.m_pHistoryNext), menu);
#endif	

}

void on_history_back (GtkMenuItem *menuitem, gpointer user_data)
{
	long nCount = (long)user_data;
	if(nCount < 1)
		nCount = 1;
	TRACE("History move back cnt=%d\n", nCount);

	BrowseHistoryList &list = g_dp.GetActiveFileList().m_ctrl.m_lstHistory;
	if(list.CanMovePrev()){
		list.Move(nCount, true);
		String strPath = list.m_strCurrent.c_str();
		g_VfsManager.VfsStackClean(&g_dp.GetActiveFileList().m_ctrl); //drop Vfs stack to ensure we are within VfsLocal
		g_dp.GetActiveFileList().SetDirectory(strPath, false);
		update_history_buttons();
	}
}

void on_history_forward (GtkMenuItem *menuitem, gpointer user_data)
{
	long nCount = (long)user_data;
	if(nCount < 1)
		nCount = 1;
	TRACE("History move forward cnt=%d\n", nCount);

	BrowseHistoryList &list = g_dp.GetActiveFileList().m_ctrl.m_lstHistory;
	if(list.CanMoveNext()){
		list.Move(nCount, false);
		String strPath = list.m_strCurrent.c_str();
		g_VfsManager.VfsStackClean(&g_dp.GetActiveFileList().m_ctrl); //drop Vfs stack to ensure we are within VfsLocal
		g_dp.GetActiveFileList().SetDirectory(strPath, false);
		update_history_buttons();
	}
}

void on_swap_panels (GtkMenuItem *menuitem, gpointer user_data)
{
	g_dp.SwapPanels();
}

void on_equal_panels (GtkMenuItem *menuitem, gpointer user_data)
{
	//TOFIX will need rewrite when start using multiple Vfs
	//TOFIX clean Vfs stack and add same Vfs type as in other panel
	//TOFIX speedup process (do not list again)
	g_VfsManager.VfsStackClean(&g_dp.GetInactiveFileList().m_ctrl);
	String strDir = g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetDir();
	g_dp.GetInactiveFileList().SetDirectory(strDir); 
}

void on_compare_panels (GtkMenuItem *menuitem, gpointer user_data)
{
	//TOFIX ensure quick search mode is switched off
	//wxBusyCursor wait;

	//select all FILES, unselects other
	int nFiles1 = g_dp.GetLeftFileList().SelectFilesOnly();
	int nFiles2 = g_dp.GetRightFileList().SelectFilesOnly();
	int nCommonCount = 0;

	// remote VFS causes us to skip compare by date (server date can be differnet)
	bool bAnyRemote = false;
	if( g_dp.GetLeftFileList().m_ctrl.m_pVfs->GetType() == Vfs::FTP    || 
	    g_dp.GetLeftFileList().m_ctrl.m_pVfs->GetType() == Vfs::SFTP    ||
	    g_dp.GetRightFileList().m_ctrl.m_pVfs->GetType() == Vfs::FTP    ||
	    g_dp.GetRightFileList().m_ctrl.m_pVfs->GetType() == Vfs::SFTP)
		bAnyRemote = true;

	//
	int nMax = g_dp.GetLeftFileList().GetListing().GetCount();
	for(int i=0; i<nMax; i++)
	{
		//only files are compared
		if(!g_dp.GetLeftFileList().GetListing().GetAt(i).IsDir())
		{
			String strItem = g_dp.GetLeftFileList().GetListing().GetAt(i).GetName();
			int nPos = g_dp.GetRightFileList().GetListing().FindItem(strItem);
			if(nPos >= 0)
			{
				//item found, compare size, date?
				INT64 nSize1 = g_dp.GetLeftFileList().GetListing().GetAt(i).m_nSize;
				INT64 nSize2 = g_dp.GetRightFileList().GetListing().GetAt(nPos).m_nSize;

				if(nSize1 == nSize2)
				{
					if(!bAnyRemote)
					{
						//if both VFS are local compare additionaly by date + attributes
						String strDate1 = g_dp.GetLeftFileList().GetListing().GetAt(i).GetDate();
						String strDate2 = g_dp.GetRightFileList().GetListing().GetAt(nPos).GetDate();
						if(strDate1 == strDate2)
						{
							int dwAttr1 = g_dp.GetLeftFileList().GetListing().GetAt(i).m_nAttrib;
							int dwAttr2 = g_dp.GetRightFileList().GetListing().GetAt(nPos).m_nAttrib;

							if(dwAttr1 == dwAttr2){
								g_dp.GetLeftFileList().DeselectItem(i);
								g_dp.GetRightFileList().DeselectItem(nPos);
								nCommonCount ++;
							}
						}
					}
					else
					{
						g_dp.GetLeftFileList().DeselectItem(i);
						g_dp.GetRightFileList().DeselectItem(nPos);
						nCommonCount ++;
					}
				}
			}
		}
	}

	//message box if the two directories look identical
	if(nFiles1 == nFiles2 && nFiles1 == nCommonCount)
		gtkMessageBox(_("The two directories look identical!"));
}

void on_options_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	OptionsDialog dlg;
	dlg.ShowModal();
}

void on_file_search_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	FileSearchDlg dlg;
	dlg.m_strInitialDir  = g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetDir();
	dlg.Create();
	dlg.ShowModal();
}

void on_file_pack_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	g_dp.GetActiveFileList().GetSelection(objSel, true, true); 

	//last checks
	int nCount = objSel.GetTotalCount();
	if(0 == nCount){
		gtkMessageBox(_("No selection!"));
		return;
	}
	if(0 == g_PlugManager.CountPackCapableFormats(nCount > 1)){
		gtkMessageBox(_("No packer plugins available!"));
		return;
	} 

	//calculate default name for destination archive path 
	String strArchive;
	strArchive = g_dp.GetInactiveFileList().m_ctrl.m_pVfs->GetDir();
	PathName::EnsureTerminated(strArchive);

	if(1 == nCount)
	{    
		//file name => archive name
		strArchive += objSel.m_lstRootItems[0].GetTitle();
	}
	else
	{
		//directory name => archive name
		String strDir = g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetDir();
		PathName::EnsureNotTerminated(strDir);
		strDir = PathName::GetBaseName(strDir);
		if(!strDir.IsEmpty() && -1 == strDir.Find(":")) //parent name is not a root "D:"
			strArchive += strDir;
		else
			strArchive += "archive";
	}

	g_dp.GetInactiveFileList().m_ctrl.m_pVfs->FixPath(strArchive);

	PackFilesDlg dlg;
	dlg.m_bMultiSupport = (nCount > 1);
	dlg.m_strArchive = strArchive; 
	dlg.Create();
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;
	dlg.Destroy();

	// perform an actual operation
	Vfs *pVfsSrc = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	Vfs *pVfsDst = g_dp.GetInactiveFileList().m_ctrl.m_pVfs;

	//STEP 2: start copy operation with progress (op executing in separate thread)
	g_objOpManager.m_strParam1 = dlg.m_strArchive;
	g_objOpManager.m_nParam1   = dlg.m_bMoveToArchive;
	g_objOpManager.StartOperation(OP_PACK, pVfsSrc, pVfsDst, objSel);

	//TOFIX:move to manager + check to refresh active if needed
	g_dp.GetInactiveFileList().PanelList();
	g_dp.GetActiveFileList().PanelList(); 
}

void on_file_unpack_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	if(0 == g_PlugManager.GetCount()){
		gtkMessageBox(_("No plugins available!"));
		return;
	}

	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	g_dp.GetActiveFileList().GetSelection(objSel, true, true);

	//last checks
	int nCount = objSel.GetTotalCount();
	if(0 == nCount){
		gtkMessageBox(_("No selection!"));
		return;
	}

	if(GTK_RESPONSE_YES != gtkMessageBox(_("Are you sure to unpack selected files?"), GTK_BUTTONS_YES_NO))
		return;

	Vfs *pVfsSrc = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	Vfs *pVfsDst = g_dp.GetInactiveFileList().m_ctrl.m_pVfs;

	//STEP 2: start operation with progress (op executing in separate thread)
	g_objOpManager.StartOperation(OP_UNPACK, pVfsSrc, pVfsDst, objSel);

	//TOFIX:move to manager + check to refresh active if needed
	g_dp.GetInactiveFileList().PanelList();
	g_dp.GetActiveFileList().PanelList(); 
}

void on_file_hash_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	if(g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetType() != Vfs::LOCAL){
		gtkMessageBox(_("Supported only for local files!"));
		return;	
	}

	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	g_dp.GetActiveFileList().GetSelection (objSel, true, true);
	objSel.RemoveRootDirs();	// only files can be target

	//last checks
	int nCount = objSel.GetTotalCount ();
	if (0 == nCount){
		gtkMessageBox(_("No files selected!"));
		return;
	}

	HashTypeDialog dlg;
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;
	dlg.Destroy();

	Vfs *pVfsSrc = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	Vfs *pVfsDst = g_dp.GetInactiveFileList().m_ctrl.m_pVfs;
	
	//pass data into the manager???
	g_objOpManager.m_nParam1    = dlg.m_nHashType;
	
	//STEP 2: start operation with progress (op executing in separate thread)
	g_objOpManager.StartOperation(OP_HASH, pVfsSrc, pVfsDst, objSel);

	//TRACE("Operation done!\n");
			
	//now display the results
	HashResultDialog dlg1;
	//FIX now using global data
	//dlg1.m_pData = &m_lstHashResults;
	//dlg1.m_nAlgorithm = m_nHashType;
	dlg1.ShowModal ();
}

void on_file_split_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	if(g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetType() != Vfs::LOCAL){
		gtkMessageBox(_("Supported only for local files!"));
		return;	
	}

	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	g_dp.GetActiveFileList().GetSelection (objSel, true, true);
	objSel.RemoveRootDirs();	// only files can be target
	
	//last checks
	int nCount = objSel.GetTotalCount ();
	if (0 == nCount){
		gtkMessageBox(_("No files selected!"));
		return;
	}

	FileSplitDialog dlg;
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;
	
	//pass data into the manager
	g_objOpManager.m_nParam1 = dlg.GetSelectedSize();
	
	dlg.Destroy();

	//STEP 2: start operation with progress (op executing in separate thread)
	Vfs *pVfsSrc = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	Vfs *pVfsDst = g_dp.GetInactiveFileList().m_ctrl.m_pVfs;
	g_objOpManager.StartOperation(OP_SPLIT, pVfsSrc, pVfsDst, objSel);

	//TOFIX:move to manager + check to refresh active if needed
	g_dp.GetInactiveFileList().PanelList();
	g_dp.GetActiveFileList().PanelList(); 
}

void on_file_merge_activate	(GtkMenuItem *menuitem, gpointer user_data)
{
	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	g_dp.GetActiveFileList().GetSelection (objSel, true, true);
	
	//last checks
	int nCount = objSel.GetTotalCount ();
	if (0 == nCount){
		gtkMessageBox(_("No files selected!"));
		return;
	}

	FileMergeDialog dlg;

	//add the file names into the list
	int i;
	for (i = 0; i < nCount; i++) 
		dlg.AddFile(objSel.m_lstRootItems[i].GetName().c_str()); 

	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	//update merge file list 
	//(files were removed or reordered in the merge dialog)
	VfsSelection objSel1;
	for (i = 0; i < dlg.GetFileCount(); i++) {
		VfsItem *vItem;
		vItem = objSel1.Insert(dlg.GetFileName(i));
		VfsSelectionItem vSelItem(*vItem);
		objSel1.m_lstRootItems[i] = vSelItem;
	}
	//pass name of output file into the manager
	g_objOpManager.m_strParam1	= dlg.GetDestFile();
	dlg.Destroy();
	
	//STEP 2: start operation with progress (op executing in separate thread)
	Vfs *pVfsSrc = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	Vfs *pVfsDst = g_dp.GetInactiveFileList().m_ctrl.m_pVfs;
	g_objOpManager.StartOperation(OP_MERGE, pVfsSrc, pVfsDst, objSel1);

	//TOFIX:move to manager + check to refresh active if needed
	g_dp.GetInactiveFileList().PanelList();
	g_dp.GetActiveFileList().PanelList(); 
}

void on_file_encrypt_decrypt_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	g_dp.GetActiveFileList().GetSelection (objSel, true, true);
	objSel.RemoveRootDirs();  //only files are being encrypted/decrypted

	//last checks
	int nCount = objSel.GetRootCount();
	if(0 == nCount){
		gtkMessageBox(_("No files selected!"));
		return;
	}

	//check first file extension (if .enc file select decrpyt button)
	bool bEncFile = false;
	String strExt = objSel.m_lstRootItems[0].GetExt();
	if(0 == strExt.CmpNoCase(".enc"))
		bEncFile = true;

	EncryptionDialog dlg;
	dlg.m_bEncrypt = !bEncFile;	//for encrypted file focus "decrypt" button

	dlg.ShowModal();
	if(!dlg.m_bSelected)
		return;

	//pass data into the manager
	g_objOpManager.m_strParam1	= dlg.m_strPassword;
	g_objOpManager.m_nParam1	= dlg.m_bDeleteOriginal;

	Vfs *pVfsSrc = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	Vfs *pVfsDst = g_dp.GetInactiveFileList().m_ctrl.m_pVfs;

	if(dlg.m_bEncrypt)
	{
		//STEP 2: start operation with progress (op executing in separate thread)
		g_objOpManager.StartOperation(OP_ENCRYPT, pVfsSrc, pVfsDst, objSel);
	}
	else
	{
		//STEP 2: start operation with progress (op executing in separate thread)
		g_objOpManager.StartOperation(OP_DECRYPT, pVfsSrc, pVfsDst, objSel);
	}

	//TOFIX:move to manager + check to refresh active if needed
	g_dp.GetInactiveFileList().PanelList();
	g_dp.GetActiveFileList().PanelList(); 
}

// copies the specified item from the specified archive to temp dir
// if this file already exists the user is asked for overwriting
// returns the full path to the newly created/overwritten file or an empty string otherwise
String CreateTempFile(Vfs *pVfsSrc, const VfsItem& item)
{
	if(pVfsSrc->GetType() == Vfs::ARCHIVE) 
	{
		//check archiver plugin capabilities
		String strExt = ((Vfs_Archive *)pVfsSrc)->GetArchivePath();
		strExt = PathName::GetExt(strExt);
		ArchiverPlugin *pPlugin = g_PlugManager.FindArchiver(strExt);
		if(pPlugin)
		{
			int nCaps = pPlugin->m_pfnGetArchiverCaps(strExt);
			if(0 == (nCaps & PK_CAPS_REAL))
			{
				//archive does not contain real file contents, just file names
				gtkMessageBox(_("Cannot view files in virtual archive!"));
				return "";
			}
		}
	}

	//
	// support to view file from the archive 
	// (unpack it into the temp directory before viewing)
	//

	String strFile = PathName::Path_TempDirectory();
	PathName::EnsureTerminated(strFile);
	strFile += item.GetName();
	pVfsSrc->FixPath(strFile);

	//overwrite checking
	if(0 == access(strFile, 0)){
		String strMsg;
		strMsg.Printf(_("File %s already exists in system temp directory. Overwrite?"),
			System::FileNameToUTF8(item.GetName()).c_str());
		if(GTK_RESPONSE_YES != gtkMessageBox(strMsg, GTK_BUTTONS_YES_NO))
			return "";

		//delete destination file so copy won't fail
		if (!System::Remove(strFile.c_str())){
			String strMsg(_("Failed to overwrite the file!"));
			gtkMessageBox(strMsg);
			return "";
		}
	}

	//TOFIX progress dialog for unpacking ??
	pVfsSrc->m_pProgress = NULL;
	if(!pVfsSrc->CopyToLocal(item, strFile)){
		String strMsg;
		strMsg = _("Failed to unpack the file!");
		gtkMessageBox(strMsg);
		return "";
	}

	return strFile;
}

void on_file_view_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	//view file on local Vfs or on archive Vfs
	Vfs *pVfsSrc = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	int nType = pVfsSrc->GetType();
	
	if( nType != Vfs::LOCAL   && 
	    nType != Vfs::ARCHIVE )
	{
		gtkMessageBox(_("Can view only local files!"));
		return;
	}

	//STEP 1: create list of items selected in currently active file panel
	int nTargetIdx = g_dp.GetActiveFileList().GetItemFocus();
	if(nTargetIdx < 0)
	{
		VfsSelection sel;
		g_dp.GetActiveFileList().GetSelection(sel, true);
		sel.RemoveRootDirs();  	//only files can be viewed
		if(sel.GetRootCount() > 0)
			nTargetIdx = g_dp.GetActiveFileList().m_ctrl.m_lstItems.FindItem(sel.m_lstRootItems[0].GetName());
	}
	
	//last checks
	if( nTargetIdx < 0 ||
	    g_dp.GetActiveFileList().m_ctrl.m_lstItems.GetAt(nTargetIdx).IsDir())
	{
		gtkMessageBox(_("Please select a file to view!"));
		return;
	}

	//get first selected file
	bool bDeleteAfterView = false;
	String strFile;
	VfsItem item = g_dp.GetActiveFileList().m_ctrl.m_lstItems.GetAt(nTargetIdx);
	
	if( nType == Vfs::LOCAL )
	{
		strFile = pVfsSrc->GetDir();
		PathName::EnsureTerminated(strFile);
		strFile += item.GetName();
		pVfsSrc->FixPath(strFile);

		if(item.IsLink())
		{
	#ifdef _WIN32
			return;	//TOFIX
	#else
			String strTarget = System::GetLinkTarget(strFile.c_str()).c_str();
			TRACE("View: LinkTarget=%s\n", strTarget.c_str());

			//convert target to absolute path if needed
			String strDst;
			if(strTarget.Length()>0 && strTarget.GetAt(0) !='/'){
				strDst = pVfsSrc->GetDir();
				PathName::EnsureTerminated(strDst);
			}
			strDst += strTarget;
			pVfsSrc->FixPath(strDst);
			TRACE("View: LinkTarget2=%s\n", strDst.c_str());

			//check if target is a file
			struct stat64 st;
			if (lstat64(strDst, &st) == -1)
				return;

			if (S_ISDIR(st.st_mode) || S_ISLNK(st.st_mode))
				return;	//target is dir or another link
			else
				strFile = strDst; // target is normal file
	#endif
		}
	}
	else if( nType == Vfs::ARCHIVE )
	{
		strFile = CreateTempFile((Vfs_Archive *)pVfsSrc, item);
		bDeleteAfterView = true;	//temporary file - should be deleted after viewing
	}

	Lister_ViewFile(strFile.c_str(), VIEW_AUTO, bDeleteAfterView);
}

void on_remote_connection_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	CNodeInfo objSite;
	int nRes = GTK_RESPONSE_CANCEL;

	{
		SiteManagerDlg dlg;
		nRes = dlg.ShowModal();
		if(nRes == GTK_RESPONSE_OK)
			objSite = dlg.m_objCurSite;
	}

	if(nRes == GTK_RESPONSE_OK)
		DoRemoteConnect(objSite);
}

void DoRemoteConnect(CNodeInfo &info)
{
	//check maximal count of remote connections (sockets)
	if(g_VfsManager.RemoteVfsCount()>=10)//TOFIX define max. or setting
	{
		gtkMessageBox(_("You have reached maximum number of allowed remote connections!\nClose one of them and then try again!"));
		return;
	}

	//
	// create new edit in the trace view tab
	//
	GtkWidget *tracetab = lookup_widget(atol_main, "tracetab");
	GtkWidget *scrolledwindow18;
	scrolledwindow18 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow18);
	gtk_container_add (GTK_CONTAINER (tracetab), scrolledwindow18);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow18), GTK_SHADOW_IN);

	GtkWidget *traceview = gtk_text_view_new ();
	gtk_widget_set_size_request(traceview, -1, 50);
	gtk_widget_set_sensitive(traceview, FALSE);
	gtk_widget_show (traceview);
	gtk_container_add (GTK_CONTAINER (scrolledwindow18), traceview);

	GtkWidget *label35 = gtk_label_new (info.m_strTitle.c_str());
	gtk_widget_show (label35);
	
	int nPages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(tracetab));
	gtk_notebook_set_tab_label (GTK_NOTEBOOK (tracetab), gtk_notebook_get_nth_page (GTK_NOTEBOOK(tracetab), nPages-1), label35);

	//ensure trace view is visible and new page is visible
	gtk_widget_show (tracetab); 
	gtk_notebook_set_current_page(GTK_NOTEBOOK(tracetab), nPages-1);

	Vfs *pVfsOld = g_dp.GetActivePanel().m_wndFileList.m_ctrl.m_pVfs;
	Vfs *pNewVFS = g_VfsManager.RemoteVfsAlloc(info);
	
	//store trace view pointer
	((Vfs_Remote*)pNewVFS)->m_pWndTrace = traceview;

	g_VfsManager.VfsRemotePush(&(g_dp.GetActivePanel().m_wndFileList.m_ctrl), pNewVFS);
	//TOFIX pView->m_wndList.REfreshFilter
	//GetActivePanel()->m_pFileList->m_pVfs->SetFilter(GetActiveList()->m_filter);
	
	//do connect operation as separate thread with progress in this thread
	VfsSelection objSel;
	g_objOpManager.StartOperation(OP_CONNECT, pNewVFS, NULL, objSel);
	//FIX: on failure do not close ftp window/trace tab, this way we can inspect possible errors
	
	//store path history from old VFS
	if(NULL != pVfsOld && pVfsOld->GetType() == Vfs::LOCAL)
		g_dp.GetActivePanel().m_wndFileList.m_ctrl.m_lstHistory.Push(pVfsOld->GetDir());
	
	//add to remote connected VFSs pool
	g_VfsManager.RemoteVfsAdd(pNewVFS);
	
	//refresh toolbar button
	refresh_connection_gui();
	
	g_dp.GetActiveFileList().PanelList();
}

void DoRemoteConnect(const char *szSiteTitle)
{
	//find site by title
	CConnectionInfoList lstSites;
	lstSites.LoadDefault();
	
	int nIdx = lstSites.Find(szSiteTitle);
	if(nIdx <= 0){
		String strMsg;
		strMsg.Printf(_("Failed to find remote site: %s!"), szSiteTitle);
		gtkMessageBox(strMsg);
		return;
	}
	
	DoRemoteConnect(lstSites[nIdx]);
}

void on_connection_close_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	Vfs *pVfs = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	if( pVfs->GetType() == Vfs::FTP ||
		pVfs->GetType() == Vfs::SFTP)
	{
		GtkWidget *tracetab = lookup_widget(atol_main, "tracetab");

		//find trace view page number
		GtkWidget *textview = (GtkWidget *)((Vfs_Remote *)pVfs)->m_pWndTrace;
		int nViewIdx = -1;
		int nMax = 	gtk_notebook_get_n_pages(GTK_NOTEBOOK(tracetab));
		for(int i=0; i<nMax; i++)
		{
			GtkWidget *scroll  = gtk_notebook_get_nth_page(GTK_NOTEBOOK(tracetab), i);
			GtkWidget *tabview = gtk_bin_get_child(GTK_BIN(scroll));

			if(textview == tabview)
			{
				nViewIdx = i;
				break;
			}
		}

		//delete remote Vfs
		pVfs->Close();
		g_VfsManager.RemoteVfsRemove(pVfs);
		g_VfsManager.VfsStackKillTop(&(g_dp.GetActiveFileList().m_ctrl));

		g_dp.GetActiveFileList().PanelList();

		//delete trace view page
		if(nViewIdx >= 0)
			gtk_notebook_remove_page (GTK_NOTEBOOK(tracetab), nViewIdx);

		//ensure trace view is invisible when no tabs present
		nMax = gtk_notebook_get_n_pages(GTK_NOTEBOOK(tracetab));
		if(0 == nMax)
			gtk_widget_hide(tracetab);

		refresh_connection_gui();
	}
}

void refresh_connection_gui()
{
	int nType  = g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetType();
	int nCount = g_VfsManager.RemoteVfsCount();

	if( nType == Vfs::FTP || nType == Vfs::SFTP )
	{
		gtk_widget_show(g_wnd.m_pConnectionClose);
		gtk_widget_set_sensitive(g_wnd.m_pConnectionClose, TRUE);

		if( nType == Vfs::FTP ){
			gtk_widget_show(g_wnd.m_pTransferMode);
			gtk_widget_set_sensitive(g_wnd.m_pTransferMode, TRUE);

			//refresh state of "transfer mode" combo
			g_signal_handlers_block_by_func(g_wnd.m_pTransferMode, (void *)on_transfer_type_combo_activate, 0);
			int nMode = ((Vfs_Ftp *)g_dp.GetActiveFileList().m_ctrl.m_pVfs)->m_nFtpMode;
			if(FTP_AUTO_MODE == nMode)
				gtk_combo_box_set_active(GTK_COMBO_BOX(g_wnd.m_pTransferMode), 0);
			else if(FTP_TEXT_MODE == nMode)
				gtk_combo_box_set_active(GTK_COMBO_BOX(g_wnd.m_pTransferMode), 1);
			else
				gtk_combo_box_set_active(GTK_COMBO_BOX(g_wnd.m_pTransferMode), 2);
			g_signal_handlers_unblock_by_func(g_wnd.m_pTransferMode, (void *)on_transfer_type_combo_activate, 0);
		}
		else{
			gtk_widget_set_sensitive(g_wnd.m_pTransferMode, FALSE);
		}
	}
	else
	{
		//in no remote connections, hide the related toolbar widgets
		//else, just disable them
		if(0 == nCount){
			gtk_widget_hide(g_wnd.m_pConnectionClose);
			gtk_widget_hide(g_wnd.m_pTransferMode);
		}
		else{
			gtk_widget_set_sensitive(g_wnd.m_pConnectionClose, FALSE);
			gtk_widget_set_sensitive(g_wnd.m_pTransferMode, FALSE);
		}
	}
}

void RemoteTrace(const char *szMsg, int nType, unsigned long nData)
{
	Vfs_Remote *pVfs = (Vfs_Remote *)nData;
	if(pVfs)
	{
		String strMsg(szMsg);
		strMsg.Replace("\r\n", "\n");
	
		//append the text to the bottom of the text view
		GtkWidget *textview = (GtkWidget *)pVfs->m_pWndTrace;
		GtkTextBuffer* buffer1 = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
		gtk_text_buffer_insert_at_cursor(buffer1, strMsg, strMsg.Length());

		//scroll text to the bottom
		gint nLines = gtk_text_buffer_get_line_count(buffer1);
		GtkTextIter iter;
		gtk_text_buffer_get_iter_at_line (buffer1, &iter, nLines-1);
		GtkTextMark*mark = gtk_text_buffer_create_mark(buffer1, NULL, &iter, TRUE);
		gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(textview), mark, 0.0, TRUE, 0.5, 0.5);
		gtk_text_buffer_delete_mark(buffer1, mark);
	} 
}

void on_transfer_type_combo_activate(GtkComboBox *widget, gpointer user_data)
{
	if(NULL == atol_main)
		return;	// on startup

	GtkWidget *ftpcombo = g_wnd.m_pTransferMode;
	int nType  = g_dp.GetActiveFileList().m_ctrl.m_pVfs->GetType();
	if(nType == Vfs::FTP)
	{
		int nActive = gtk_combo_box_get_active(GTK_COMBO_BOX(ftpcombo));

		// change tranfer type for the current conenction
		Vfs_Ftp *pVfs = (Vfs_Ftp *)g_dp.GetActiveFileList().m_ctrl.m_pVfs;
		if(0 == nActive)
			pVfs->m_nFtpMode = FTP_AUTO_MODE;
		else if(1 == nActive)
			pVfs->m_nFtpMode = FTP_TEXT_MODE;
		else
			pVfs->m_nFtpMode = FTP_BIN_MODE;			
	}
}

void on_view_create_file_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	//check for supported Vfs types
	Vfs *pVfsMain  = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
	Vfs *pVfsOther = g_dp.GetInactiveFileList().m_ctrl.m_pVfs;

	int nType  = pVfsMain->GetType();
	if( nType != Vfs::LOCAL )
	{
		gtkMessageBox(_("Can create only local files!"));
		return;
	}

	g_dp.GetActiveFileList().StopNotifications(pVfsMain, pVfsOther);

	//generate unique temp file
	String strPath = pVfsMain->GetDir();
	char szNameBuffer[1024] = "";
#ifdef _WIN32	//TOFIX define GetTempFileName for Linux
	GetTempFileName(strPath, "NewFile", 0, szNameBuffer);
#else
	sprintf(szNameBuffer, "%s/NewFileXXXXXX", strPath.c_str());
	mkstemp(szNameBuffer);
	//create the file
	FILE *pOut = fopen(szNameBuffer, "w");
	if(pOut) fclose(pOut);
#endif
	String strName = PathName::GetBaseName(szNameBuffer);

	//refresh the Vfs list and trigger the inline rename in the panel
	g_dp.GetActiveFileList().PanelList();
	
	int nPos = g_dp.GetActiveFileList().m_ctrl.m_lstItems.FindItem(strName);
	if(nPos >= 0)
	{
		g_dp.GetActiveFileList().SelectNone();
		g_dp.GetActiveFileList().SetItemFocus(nPos);
		g_dp.GetActiveFileList().Rename();
	}

	g_dp.GetActiveFileList().StartNotifications(pVfsMain, pVfsOther);

	//refresh destination panel if showing the same path
	//(in case user canceled the rename, so no new notification
	// will trigger the refresh to show newly created file)
	if(pVfsMain->IsEqualLocation(pVfsOther))
		g_dp.GetInactiveFileList().PanelList();
}

void on_go_user_home_directory (GtkMenuItem *menuitem, gpointer user_data)
{
	String strPath = System::GetHomeDir().c_str();
	g_VfsManager.VfsStackClean(&g_dp.GetActiveFileList().m_ctrl); //drop Vfs stack to ensure we are within VfsLocal
	g_dp.GetActiveFileList().SetDirectory(strPath);
}

bool on_mainwindow_focus(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	if(g_bRefreshPanelsOnFocus) {
		Vfs *pVfsActive  = g_dp.GetActiveFileList().m_ctrl.m_pVfs;
		Vfs *pVfsInactive= g_dp.GetInactiveFileList().m_ctrl.m_pVfs;
		if(pVfsActive->GetType() == Vfs::LOCAL) {
			g_dp.GetActiveFileList().PanelList(true);
		}
		if(pVfsInactive->GetType() == Vfs::LOCAL) {
			g_dp.GetInactiveFileList().PanelList(true);
		}
	}

	gtk_widget_grab_focus(GTK_WIDGET(widget));
	return FALSE;
}
