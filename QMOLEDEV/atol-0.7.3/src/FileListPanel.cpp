////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Window for single file list panel (file list + path info + drive button menu)
//////////////////////////////////////////////////////////////////////////// 

#include <gdk/gdkkeysyms.h>

#include "FileListPanel.h"
#include "DualPanel.h"
#include "GuiInputDlg.h"
#include "BookmarkEditDlg.h"
#include "support.h"
#include "core/BrowseBookmarkList.h"
#include "core/String.h"
#include "core/System.h"
#include "core/VfsManager.h"
#include "core/VfsFtp.h"
#include "core/VfsSftp.h"
#include "core/VfsNet.h"
#include "core/VfsArchive.h"
#include "core/PathName.h"
#include "../res/drive.xpm"

#ifdef _WIN32
#include "../res/drv_cdrom.xpm"
#include "../res/drv_floppy.xpm"
#include "../res/drv_fixed.xpm"
#include "../res/drv_network.xpm"
#include "../res/drv_network_drive.xpm"
#include "../res/drv_remote_connection.xpm"
#include "../res/drv_removable.xpm"

#pragma comment(lib, "mpr")	//automatically link this library (WNetGetConnection)
#endif
 
extern DualPanel g_dp;
extern GtkWidget *atol_main;
extern VfsManager g_VfsManager;
extern GdkColor g_labelTextColor;
extern GdkColor g_labelBackgroundColor;

int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
void update_history_buttons();
void refresh_connection_gui();

static void do_drive_menu (GdkEventButton *event, gpointer user_data);
static void on_drive_btn_activate (GtkMenuItem *menuitem, gpointer user_data);
static void on_drive_selected (GtkMenuItem *menuitem, gpointer user_data);
static void on_add_dir_selected (GtkMenuItem *menuitem, gpointer user_data);
static void on_bookmark_selected (GtkMenuItem *menuitem, gpointer user_data);
static gboolean on_label_clicked (GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_addressbar (GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_addressbar_focus_out (GtkWidget *widget, GdkEventFocus *event, gpointer user_data);
static gboolean on_addressbar_key_press (GtkWidget *widget, GdkEventKey *event, gpointer user_data);
static gboolean on_bookmark_menu (GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_focus_event (GtkWidget *widget, GtkDirectionType arg1, gpointer user_data);
static gboolean on_focus_in_event (GtkWidget *widget, GdkEventFocus *event, gpointer user_data);
static void on_grab_focus_event (GtkWidget *widget, gpointer user_data);
static void on_configure_selected (GtkMenuItem *menuitem, gpointer user_data);
void drive_menu_position_func (GtkMenu *menu, gint *x, gint *y, gboolean *push_in, gpointer user_data);
void bookmark_menu_position_func (GtkMenu *menu, gint *x, gint *y, gboolean *push_in, gpointer user_data);

FileListPanel::FileListPanel()
{
	m_pWidget = NULL;
	m_bMenuIgnoreMousePos = false;
	m_bActive = false;
}

FileListPanel::~FileListPanel()
{
}

void FileListPanel::Create()
{
	GtkWidget *vbox5;
	GtkWidget *hbox, *hbox1;
	GtkWidget *evbox;
	GtkWidget *button;
	GtkWidget *image;
	GtkWidget *label1;
	GtkWidget *scrolledwindow4;
	GtkWidget *label3;
	GtkWidget *treeview2;

	vbox5 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox5);

	hbox = gtk_hbox_new(FALSE, 2);
	gtk_widget_show (hbox);
	gtk_box_pack_start(GTK_BOX(vbox5), hbox, FALSE, FALSE, 0);

	button = gtk_button_new ();
	gtk_container_set_border_width (GTK_CONTAINER (button), 0);
	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drive_xpm);
	image = gtk_image_new_from_pixbuf(pixbuf);
	gtk_widget_show (image);
	gtk_container_add (GTK_CONTAINER (button), image);
	gtk_widget_set_usize (GTK_WIDGET (button), 20, 20);
	gtk_button_set_relief (GTK_BUTTON (button), GTK_RELIEF_NONE); 
	GTK_WIDGET_UNSET_FLAGS(button, GTK_CAN_FOCUS);

	gtk_widget_show (button);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, TRUE, 0);
	g_signal_connect (GTK_OBJECT (button), "clicked", G_CALLBACK (on_drive_btn_activate), this);
 
	m_pEntry = gtk_entry_new ();
	gtk_entry_set_has_frame(GTK_ENTRY(m_pEntry), false);
	gtk_box_pack_start(GTK_BOX(hbox), m_pEntry, TRUE, TRUE, 0);
	g_signal_connect(m_pEntry, "focus-out-event", G_CALLBACK(on_addressbar_focus_out), this);
	g_signal_connect(m_pEntry, "key-press-event", G_CALLBACK(on_addressbar_key_press), this);

	//event box is needed for colored labels
	evbox = gtk_event_box_new ();
	gtk_widget_show (evbox);
	gtk_box_pack_start(GTK_BOX(hbox), evbox, TRUE, TRUE, 0);
	
	hbox1 = gtk_hbox_new(FALSE, 0);
	gtk_widget_show (hbox1);
	gtk_container_add(GTK_CONTAINER(evbox), hbox1);

	// create dummy label to ident panel path by some space to the right (3 pixels)
	GtkWidget *label0 = gtk_label_new ("");
	gtk_widget_set_usize(GTK_WIDGET(label0), 3, -1);
	gtk_widget_show(label0);
	gtk_box_pack_start(GTK_BOX(hbox1), label0, FALSE, FALSE, 0);


	label1 = gtk_label_new ("");
	gtk_widget_show (label1);
	gtk_misc_set_alignment(GTK_MISC(label1), 0.01f, 0.5f);  // horizontal: almost left aligned, vertical: centered
	gtk_box_pack_start(GTK_BOX(hbox1), label1, TRUE, TRUE, 0);
#if GTK_CHECK_VERSION(2,6,0) //new API TOFIX set proper version
	gtk_label_set_ellipsize(GTK_LABEL(label1), (PangoEllipsizeMode)PANGO_ELLIPSIZE_MIDDLE);
#endif 

	scrolledwindow4 = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrolledwindow4);
	gtk_box_pack_start (GTK_BOX (vbox5), scrolledwindow4, TRUE, TRUE, 0);

	//create file panel
	m_wndFileList.Create();
	m_wndFileList.m_pScrollWidget = scrolledwindow4;
	treeview2 = m_wndFileList.m_pWidget;
	gtk_container_add (GTK_CONTAINER (scrolledwindow4), treeview2);

	label3 = gtk_label_new ("");
	gtk_widget_show (label3);
	gtk_box_pack_start (GTK_BOX (vbox5), label3, FALSE, TRUE, 0);
	gtk_misc_set_alignment(GTK_MISC(label3), 0.05f, 0.5f);  // horizontal: almost left aligned, vertical: centered
	gtk_widget_set_size_request (GTK_WIDGET (label3), 0, -1); 

	g_signal_connect (treeview2, "focus",          G_CALLBACK (on_focus_event), this);
	g_signal_connect (treeview2, "focus-in-event", G_CALLBACK (on_focus_in_event), this);
	g_signal_connect (treeview2, "grab-focus",     G_CALLBACK (on_grab_focus_event), this);
	g_signal_connect (evbox,     "button-release-event", G_CALLBACK (on_label_clicked), this);

	m_wndFileList.m_pPathWidget = label1;
	m_wndFileList.m_pInfoWidget = label3;

	m_pWidget = vbox5;
	m_pPathLabel = label1;
	m_pEventBox = evbox;
	m_pDriveButton = button;

	ShowActive(false);
}

bool FileListPanel::IsActive()
{
	return m_bActive;
}

void FileListPanel::Activate()
{
	gtk_widget_grab_focus(m_wndFileList.m_pWidget);
	update_history_buttons();
	refresh_connection_gui();
}

void FileListPanel::ShowActive(bool bActive)
{
	m_bActive = bActive;

	if(bActive)
	{
		//use user selected text/bkg color to indicate active panel
		gtk_widget_modify_fg(GTK_WIDGET(m_pPathLabel), GTK_STATE_NORMAL, &g_labelTextColor);
		gtk_widget_modify_bg (GTK_WIDGET (m_pEventBox), GTK_STATE_NORMAL, &g_labelBackgroundColor);
	}
	else
	{
		GdkColor gray = { 0, 0xAFFF, 0xAFFF, 0xAFFF };
		GdkColor black = { 0, 0, 0, 0 };
		gtk_widget_modify_fg(GTK_WIDGET(m_pPathLabel), GTK_STATE_NORMAL, &black);
		gtk_widget_modify_bg (GTK_WIDGET (m_pEventBox), GTK_STATE_NORMAL, &gray);
	}
}

void FileListPanel::DropDriveMenu()
{
	do_drive_menu (NULL, this);
}

void FileListPanel::DropBookmarkMenu()
{
	m_bMenuIgnoreMousePos = true;
	on_bookmark_menu(NULL, NULL, this);
	m_bMenuIgnoreMousePos = false;
}

void on_drive_btn_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	do_drive_menu (NULL, user_data);
}

static void do_drive_menu (GdkEventButton *event, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;

	//quit quick search mode for both panels before the operation
	pPanel->m_wndFileList.QuickSearchExit();
	pPanel->m_wndFileList.m_pOtherList->QuickSearchExit();

	GtkWidget *menu;
	GtkWidget *menu_item;
	int button, event_time;
	
	menu = gtk_menu_new ();

	//fill menu with the list of partitions
	pPanel->m_lstPartitions.clear();
	System::GetPartitionList(pPanel->m_lstPartitions);

	unsigned int i;
	for(i=0; i<pPanel->m_lstPartitions.size(); i++)
	{
		// add menu item
#ifdef _WIN32
		//add volume name next to the path
		char szVolumeName[1024] = "";
		
		//skip query for floppy, since it generates scraping sound when no floppy inside
		UINT nType = GetDriveType(pPanel->m_lstPartitions[i].c_str());
		if( DRIVE_NO_ROOT_DIR != nType &&
		    !(0 == i && DRIVE_REMOVABLE == nType)) //assume floppy is the first drive
		{
			#if 0
			//this code makes menu too slow (waited for menu some 10 seconds)
			//when menu is being displayed for the first time
			GetVolumeInformation(pPanel->m_lstPartitions[i].c_str(),
		                     szVolumeName, sizeof(szVolumeName), NULL, NULL, NULL, NULL, 0);
			#endif
		}

		//invent "volume name" if not existing (for better info)
		if( DRIVE_CDROM == nType && !*szVolumeName )
			strcpy(szVolumeName, "CD");
		
		std::string strTitle("_");
		strTitle += pPanel->m_lstPartitions[i];

		//append volume name
		if(strlen(szVolumeName) > 0)
		{
			strTitle += "\t[";
			strTitle += szVolumeName;
			strTitle += "]";
		}

		// create an image
		GdkPixbuf *pixbuf = NULL;

		switch(nType)
		{
		case DRIVE_REMOVABLE:
			if(0 == i)	//assume floppy is the first drive
				pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drv_floppy_xpm);
			else
				pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drv_removable_xpm);
			break;

		case DRIVE_CDROM:
			pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drv_cdrom_xpm);
			break;

		case DRIVE_REMOTE:
		{
			char tmp[512];
			DWORD l = sizeof(tmp);
			String strDrive(pPanel->m_lstPartitions[i].c_str());
	
			// get UNC path for this drive and add additional information to menu
			if(WNetGetConnection(strDrive.Left(2), tmp, &l) == NO_ERROR)
			{
				String strTmp(tmp + 2);
				strTitle += "  [";
				strTitle += PathName::GetBaseName(tmp);
				strTitle += _(" on \"");
				strTitle += strTmp.Left(strTmp.Find("\\"));
				strTitle += "\"]";
			}

			pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drv_network_drive_xpm);
			break;
		}
		
		case DRIVE_FIXED:
			pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drv_fixed_xpm);
			break;
		}

		//convert to UTF-8
		gsize bytesWritten;
		gchar *szStrUtf8 = g_locale_to_utf8 (strTitle.c_str(), -1, NULL, &bytesWritten, NULL);
		menu_item = gtk_image_menu_item_new_with_mnemonic(szStrUtf8);
		g_free(szStrUtf8);

		if(pixbuf)
		{
			GtkWidget *image = gtk_image_new_from_pixbuf(pixbuf);
			g_object_unref(G_OBJECT(pixbuf));
			gtk_widget_show (image);
			gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), image);
		} 
#else
		menu_item = gtk_menu_item_new_with_label(pPanel->m_lstPartitions[i].c_str());
#endif
		g_signal_connect(menu_item, "activate",	G_CALLBACK (on_drive_selected), user_data);
		gtk_menu_append(menu, menu_item);
		gtk_widget_show (menu_item);  // Show the widget 
	}

	//add separator
	menu_item = gtk_separator_menu_item_new ();
	gtk_widget_show (menu_item);
	gtk_container_add (GTK_CONTAINER (menu), menu_item);
	gtk_widget_set_sensitive (menu_item, FALSE);

	//add network browse support
#ifdef _WIN32
	menu_item = gtk_image_menu_item_new_with_mnemonic(_("Network"));
	GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drv_network_xpm);
	GtkWidget *image = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(G_OBJECT(pixbuf));
	gtk_widget_show (image);
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), image);
#else 
	menu_item = gtk_menu_item_new_with_mnemonic(_("Network"));
#endif
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_drive_selected), user_data);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 
#ifndef _WIN32
	gtk_widget_set_sensitive (menu_item, FALSE);
#endif

	//TOFIX menu.AppendSeparator(); 

	//append FTP/SFTP sessions
	for(i=0; i<g_VfsManager.RemoteVfsCount(); i++)
	{
		String strItem;
		Vfs *pVfs = g_VfsManager.m_lstVfsRemote[i];
		//TOFIX create Vfs_Remote base class!!!
		if(Vfs::FTP == pVfs->GetType())
			strItem.Printf("%d: %s", i, ((Vfs_Ftp *)pVfs)->m_ftpInfo.m_strHost.c_str());
		else if(Vfs::SFTP == pVfs->GetType())
			strItem.Printf("%d: %s", i, ((Vfs_Sftp *)pVfs)->m_szHost.c_str());

		//add mnemonic for first 10 items (digits from 0-9)
		if(i < 10){
			String strTmp(strItem);
			strItem  = "_";
			strItem += strTmp;
		}

	#ifdef _WIN32
		menu_item = gtk_image_menu_item_new_with_mnemonic(strItem);
		GdkPixbuf *pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&drv_remote_connection_xpm);
		GtkWidget *image = gtk_image_new_from_pixbuf(pixbuf);
		g_object_unref(G_OBJECT(pixbuf));
		gtk_widget_show (image);
		gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), image);
	#else 
		menu_item = gtk_menu_item_new_with_mnemonic(strItem);
	#endif
		g_signal_connect(menu_item, "activate",	G_CALLBACK (on_drive_selected), user_data);
		gtk_menu_append(menu, menu_item);
		gtk_widget_show (menu_item);  // Show the widget 

		//check if this Vfs is already used and must be disabled in the menu
		if( pVfs == g_dp.GetActiveFileList().m_ctrl.m_pVfs ||
			pVfs == g_dp.GetInactiveFileList().m_ctrl.m_pVfs)
		{
			gtk_widget_set_sensitive(menu_item, FALSE);
		}
	}

	if (event)
		event_time = event->time;
	else
		event_time = gtk_get_current_event_time ();
	button = 0;	//FIX: allow mouse button to trigger the submenu
	
	gtk_menu_popup (GTK_MENU (menu), NULL, NULL, drive_menu_position_func, user_data, button, event_time);
}

void drive_menu_position_func (GtkMenu *menu, gint *x, gint *y, gboolean *push_in, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;

	gint dest_x, dest_y;
	gtk_widget_translate_coordinates (pPanel->m_pDriveButton, 
                                          atol_main,
                                          21, 0, &dest_x, &dest_y);
	TRACE("Coordinates %d,%d\n", dest_x, dest_y);

	gint root_x, root_y;
	gtk_window_get_position (GTK_WINDOW(atol_main), &root_x, &root_y);
	TRACE("Root coordinates %d,%d\n", root_x, root_y);

	// fill output values to define popup menu position
	*push_in = TRUE;
	*x = root_x + dest_x;
	*y = root_y + dest_y + 20; //TOFIX offset for window manager decoration
}

void on_drive_selected (GtkMenuItem *menuitem, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;
	if(pPanel)
	{
		//calculate child index within the menu
		GtkMenu *menu = (GtkMenu *)((GtkWidget *)menuitem)->parent;
		GList *list = gtk_container_get_children (GTK_CONTAINER(menu));

		int nIdx = -1;
		int nMax = g_list_length(list);
		for(int i=0; i<nMax; i++)
		{
			if(menuitem == g_list_nth_data(list, i))
			{
				nIdx = i;
				break;
			}
		}
		g_list_free(list);

		if(nIdx >= 0)
		{
			int nLocalItemsCount = pPanel->m_lstPartitions.size();
			
			if(nIdx < nLocalItemsCount)
			{
				g_VfsManager.VfsStackClean(&(pPanel->m_wndFileList.m_ctrl));

				String strPath = pPanel->m_lstPartitions[nIdx].c_str();

				//for local drive fetch the latest used work dir
				String strPathPrev = g_VfsManager.DriveHistoryGet(strPath);

				if(pPanel->m_wndFileList.m_ctrl.m_pVfs->SetDir(strPathPrev.c_str()))	
					pPanel->m_wndFileList.PanelList();
				else if(pPanel->m_wndFileList.m_ctrl.m_pVfs->SetDir(strPath.c_str()))	// set drive root (if setting latest dir failed)
					pPanel->m_wndFileList.PanelList();
				else
				{
					String strMsg;
					strMsg.Printf(_("Failed to set: %s"), strPath.c_str());
					gtkMessageBox(strMsg);
				}
			}
			else if(nIdx == nLocalItemsCount+1)
			{
				//network
				Vfs_Net *pVfs = new Vfs_Net;
				g_VfsManager.VfsStackPush(&(pPanel->m_wndFileList.m_ctrl), pVfs);
				pPanel->m_wndFileList.SetDirectory("/"); 
			}
			else if(nIdx > nLocalItemsCount+1)
			{
				//just activate remote Vfs
				int nRelIdx = nIdx - nLocalItemsCount - 2;
				Vfs *pVfs = g_VfsManager.m_lstVfsRemote[nRelIdx];
				g_VfsManager.VfsRemotePush(&(pPanel->m_wndFileList.m_ctrl), pVfs); //TOFIX
				pPanel->m_wndFileList.PanelList();
			}

			pPanel->Activate();
		}
	}
}

gboolean on_focus_in_event (GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;
	if(pPanel)
	{
		pPanel->ShowActive(true);
		g_dp.GetOtherPanel(*pPanel).ShowActive(false);
		update_history_buttons();
		refresh_connection_gui();
	}
	return FALSE;
}

gboolean on_focus_event (GtkWidget *widget, GtkDirectionType arg1, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;
	if(pPanel)
	{
		// this code prohibits having two focus points on a single widget
		// (header + list/tree widget parts)
		static bool bRecurse = false;
		if(!bRecurse)
		{
			bRecurse = true;
			//on first focus, pass focus point forward from header to main tree part 
			gtk_widget_child_focus (pPanel->m_wndFileList.m_pWidget, (GtkDirectionType)GTK_DIR_DOWN);
			bRecurse = false;
		}

		pPanel->ShowActive(true);
		g_dp.GetOtherPanel(*pPanel).ShowActive(false);
		update_history_buttons();
		refresh_connection_gui();
	}
	return FALSE;
}

void on_grab_focus_event (GtkWidget *widget, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;
	if(pPanel)
	{
		pPanel->ShowActive(true);
		g_dp.GetOtherPanel(*pPanel).ShowActive(false);
		update_history_buttons();
		refresh_connection_gui();
	}
}

gboolean on_label_clicked (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	if(event->button == 1)
		return on_addressbar(widget, event, user_data);
	else
		return on_bookmark_menu(widget, event, user_data);
}

gboolean on_addressbar (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel*)user_data;
	FileList *pFileList = &pPanel->m_wndFileList;

	gtk_widget_grab_focus(pFileList->m_pWidget);
	gtk_widget_hide(pPanel->m_pEventBox);
	gtk_widget_show(pPanel->m_pEntry);

	String strPath = g_VfsManager.GetPathTitle(&pFileList->m_ctrl);
	gtk_entry_set_text(GTK_ENTRY(pPanel->m_pEntry), strPath.c_str());
	gtk_widget_grab_focus(pPanel->m_pEntry);
	return true;
}

static gboolean on_addressbar_key_press (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	//process only key press events here
	if(event->type != GDK_KEY_PRESS)
		return FALSE;	

	FileListPanel *pPanel = (FileListPanel*)user_data;
	FileList *pFileList = &pPanel->m_wndFileList;

	if(event->keyval == GDK_Escape || event->keyval == GDK_Tab)
	{
		// set focus to file list. This automatically hides the entry widget.
		gtk_widget_grab_focus(pFileList->m_pWidget);
		return true;
	}
	else if(event->keyval == GDK_Return)
	{
		String strPath(gtk_entry_get_text(GTK_ENTRY(pPanel->m_pEntry)));
		strPath = System::FileNameFromUTF8(strPath).c_str();

		if(pFileList->m_ctrl.m_pVfs->GetType() == Vfs::ARCHIVE)
		{
			Vfs_Archive *pVfs = (Vfs_Archive *)pFileList->m_ctrl.m_pVfs;
			String strArchivePath(pVfs->GetArchivePath());

			if(strArchivePath == strPath.Left(strArchivePath.Length()))
				strPath = strPath.Mid(strArchivePath.Length());
			else
				g_VfsManager.VfsStackClean(&(pFileList->m_ctrl));
		}

		pFileList->SetDirectory(strPath.c_str());

		// set focus to file list. This automatically hides the entry widget.
		gtk_widget_grab_focus(pFileList->m_pWidget);
		return true;
	}

	return false;
}

static gboolean on_addressbar_focus_out (GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel*)user_data;

	// program crashes if we focus out (= hiding the widget) while a cursor
	// is blinking!?! Avoid this by selecting text
	String strPath(gtk_entry_get_text(GTK_ENTRY(pPanel->m_pEntry)));
	if(strPath.length() <= 0)
	gtk_entry_set_text(GTK_ENTRY(pPanel->m_pEntry), " ");
	gtk_editable_select_region(GTK_EDITABLE(pPanel->m_pEntry), 0, -1);

	gtk_widget_hide(pPanel->m_pEntry);
	gtk_widget_show(pPanel->m_pEventBox);
	return true;
}

gboolean on_bookmark_menu (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	GtkWidget *menu;
	GtkWidget *menu_item;
	int button, event_time;
	
	menu = gtk_menu_new ();

	BrowseBookmarkList books;
	books.Load();

	int nBookCount = books.GetCount();
	for(int i=0; i<nBookCount; i++)
	{
		// add menu item
		menu_item = gtk_menu_item_new_with_label(books.GetBookTitle(i));
		g_signal_connect(menu_item, "activate",	G_CALLBACK (on_bookmark_selected), user_data);
		gtk_menu_append(menu, menu_item);
		gtk_widget_show (menu_item);  // Show the widget 

		//TOFIX checkmark the item
        	//if(strCurDir == books.GetBookPath(i))
        	//    menu.Check(i+1, true);
	}

	if(nBookCount>0)
	{
		//add separator
		menu_item = gtk_separator_menu_item_new ();
		gtk_widget_show (menu_item);
		gtk_container_add (GTK_CONTAINER (menu), menu_item);
		gtk_widget_set_sensitive (menu_item, FALSE);
	}

	// add menu item
	menu_item = gtk_menu_item_new_with_label(_("Add current directory"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_add_dir_selected), user_data);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 

	menu_item = gtk_menu_item_new_with_label(_("Configure"));
	g_signal_connect(menu_item, "activate",	G_CALLBACK (on_configure_selected), user_data);
	gtk_menu_append(menu, menu_item);
	gtk_widget_show (menu_item);  // Show the widget 

	if (event)
		event_time = event->time;
	else
		event_time = gtk_get_current_event_time ();
	button = 0;	//FIX: allow mouse button to trigger the submenu

	FileListPanel *pPanel = (FileListPanel *)user_data;
	if(pPanel->m_bMenuIgnoreMousePos)
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, bookmark_menu_position_func, user_data, button, event_time);
	else
		gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, button, event_time);

	return TRUE;
}

void bookmark_menu_position_func (GtkMenu *menu, gint *x, gint *y, gboolean *push_in, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;

	//TOFIX get path label width
	gint dest_x, dest_y;
	gtk_widget_translate_coordinates (pPanel->m_pPathLabel, 
                                          atol_main,
                                          100, 21, &dest_x, &dest_y);
	TRACE("Coordinates %d,%d\n", dest_x, dest_y);

	gint root_x, root_y;
	gtk_window_get_position (GTK_WINDOW(atol_main), &root_x, &root_y);
	TRACE("Root coordinates %d,%d\n", root_x, root_y);

	// fill output values to define popup menu position
	*push_in = TRUE;
	*x = root_x + dest_x;
	*y = root_y + dest_y + 20; //TOFIX offset for window manager decoration
}

void on_add_dir_selected (GtkMenuItem *menuitem, gpointer user_data)
{
	FileListPanel *pPanel = (FileListPanel *)user_data;
	String strCurDir = pPanel->m_wndFileList.m_ctrl.m_pVfs->GetDir();

	//get password
	GuiInputDlg dlg;
	dlg.SetLabel(_("Set entry title"));
	dlg.SetInput(strCurDir,true);
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	//save new entry into the bookmark list
	BrowseBookmarkList books;
	books.Load();
	
	if(books.Insert(dlg.GetInput(), strCurDir))
		books.Save();
	else
		gtkMessageBox(_("Failed to add bookmark!"));
}

void on_configure_selected (GtkMenuItem *menuitem, gpointer user_data)
{
	//show bookmark editor dialog
	BookmarkEditDlg dlg;
	dlg.ShowModal();
}

void on_bookmark_selected (GtkMenuItem *menuitem, gpointer user_data)
{
	//execute the bookmark
	FileListPanel *pPanel = (FileListPanel *)user_data;
	if(pPanel)
	{
		const gchar *s = gtk_label_get_text (GTK_LABEL (GTK_BIN (GTK_MENU_ITEM (menuitem))->child));
		if(s)
		{
			BrowseBookmarkList books;
			books.Load();

			int nIdx = books.FindBookByTitle(s);
			if(nIdx >= 0)
			{
				String strPath = books.GetBookPath(nIdx);

				if(pPanel->m_wndFileList.m_ctrl.m_pVfs->SetDir(strPath))
					pPanel->m_wndFileList.PanelList();
				else
				{
					String strMsg;
					strMsg.Printf(_("Failed to set: %s"), strPath.c_str());
					gtkMessageBox(strMsg);
				}
			}
		}
	}
}

