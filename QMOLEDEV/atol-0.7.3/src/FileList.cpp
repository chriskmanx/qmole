////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file list widget using GtkTreeView
//////////////////////////////////////////////////////////////////////////// 

#include "FileList.h"
#include "core/PathName.h"
#include "core/System.h"
#include "core/opcodes.h"
#include "support.h"
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "../res/blank.xpm"
#include "../res/folder.xpm"
#include "../res/up_dir.xpm"
#include "../res/archive.xpm"
#include "../res/lock.xpm"
#include "GuiInputDlg.h"
#include "core/util.h"
#include "core/FilterDesc.h"
#include "core/VfsManager.h"
#include "core/VfsArchive.h"
#include "core/PluginManager.h"
#include "core/debug.h"
#include "DeleteStartDlg.h"
#include "FileListPanel.h"
#include "DualPanel.h"
#include "callbacks.h"
#include "core/VfsLocal.h"
#include "ExecutionThread.h"
#include "MainWindow.h"

#ifdef _WIN32
 #include "shlobj.h"
#endif 

extern String CreateTempFile(Vfs *pVfsSrc, const VfsItem& item);
extern VfsManager g_VfsManager;
extern OpManager  g_objOpManager;
extern String g_strEditor;

extern MainWindow g_wnd; 
extern PluginManager g_PlugManager;
extern DualPanel g_dp;
bool g_bShowExtColumn = true;

gint treeview_keyboard_handler(GtkWidget *widget, GdkEventKey *event, gpointer data);
void on_header_clicked(GtkTreeViewColumn *treeviewcolumn, gpointer user_data);
void on_item_executed (GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col, gpointer  userdata);
void on_selection_changed (GtkTreeSelection *treeselection, gpointer user_data);
gboolean on_ctx_menu_activate (GtkWidget *widget, GdkEventButton *event, gpointer user_data);
gint on_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data);
gint on_popup_menu(GtkWidget *widget, gpointer data);

void FileListDataFunc (GtkTreeViewColumn *tree_column, GtkCellRenderer *cell, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data);
void FileListIconFunc (GtkTreeViewColumn *tree_column, GtkCellRenderer *cell, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data);

void cell_edited_callback(GtkCellRendererText *cell,  gchar *path_string, gchar *new_text, gpointer user_data);
void cell_edited_start_callback (GtkCellRenderer *renderer, GtkCellEditable *editable, gchar *path, gpointer user_data);
void cell_edited_cancel_callback (GtkCellRenderer *renderer, gpointer user_data);

void update_history_buttons();
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);

bool g_bNCSelection = true;

gboolean DirChangeEvent_CumulateTimer(gpointer data);
bool CheckRefreshList(FileList *pList);

static const GtkTargetEntry targets[] = {
	{ "text/uri-list", 0, 0 }
};

FileList::FileList()
{
	m_pWidget = NULL;
	m_pPathWidget = NULL;
	m_pInfoWidget = NULL;
	m_pOtherList = NULL;
	m_pScrollWidget = NULL;
	m_nEditedIdx = -1;
	m_bQuickSearchMode = false;
	m_nChangeCumulateTimer = -1;
	m_nLastChangeEventMs = 0;
	m_nLastRefreshEventMs = 0;
	m_bDnDEnabled = false;
	m_bSkipSpaceInfo = false;

	for(int i=0; i<5; i++)
		cols[i] = NULL;
}

void FileList::InitVfs()
{
	m_ctrl.m_lstItems.SetSort(VfsListing::ST_NAME);

	//register change notification
	((Vfs_Local *)(m_ctrl.m_pVfs))->RegisterChangeNotify(this);
}

void FileList::RefreshSortIndicator()
{
	SetSortIndicator(m_ctrl.m_nSortColumn, m_ctrl.m_bSortAscending);
}

void FileList::SetSortIndicator(int nCol, bool bAsc)
{
	//remove previous indicator
	if(m_ctrl.m_nSortColumn >= 0)
		gtk_tree_view_column_set_sort_indicator (cols[m_ctrl.m_nSortColumn], FALSE);

	if(nCol >= 0 && nCol < 5)
	{
		gtk_tree_view_column_set_sort_indicator (cols[nCol], TRUE);
		gtk_tree_view_column_set_sort_order(cols[nCol], (bAsc)? GTK_SORT_ASCENDING:GTK_SORT_DESCENDING);
	}
}

void FileList::SetSort(int nCol, bool bAsc)
{
	//remember selection before sorting
	VfsSelection objSel;
	GetSelection(objSel);

	//display sort indicator
	SetSortIndicator(nCol, bAsc);
	
	m_ctrl.m_nSortColumn = nCol;
	m_ctrl.m_bSortAscending = bAsc;
	
	//actually sort the data
	int iSortCol = m_ctrl.m_nSortColumn + 1;
	if(!g_bShowExtColumn && iSortCol > 1)
		iSortCol++;

	m_ctrl.m_lstItems.SetSort((VfsListing::_SORT_COL)(iSortCol), m_ctrl.m_bSortAscending);

	//restore selection after sorting
	SelectNone();
	SetSelection(objSel);

	Redraw();
}

void on_drag_data_get(GtkWidget *widget,
	GdkDragContext *drag_context,
	GtkSelectionData *data,
	guint info,
	guint time,
	gpointer user_data)
{
	int i;
	FileList *pFileList = (FileList*)user_data;

	//get current path
	String strPathTemplate(pFileList->m_ctrl.m_pVfs->GetDir());
	PathName::EnsureTerminated(strPathTemplate);
	pFileList->m_ctrl.m_pVfs->FixPath(strPathTemplate); 

	VfsSelection objSel;
	pFileList->GetSelection(objSel, true);

	//create an uri-list for all selected files
	int nCnt = objSel.GetRootCount();
	gchar **uris = g_new(gchar *, nCnt + 1);
	if(!uris)
		return;
	
	//put the selected files to the list
	for(i = 0; i < nCnt; i++)
	{
		String strPath(strPathTemplate);
		strPath += objSel.m_lstRootItems[i].GetName();
		uris[i] = g_filename_to_uri (strPath.c_str(), NULL, NULL);
	}
	uris[i] = NULL;

	gtk_selection_data_set_uris(data, uris);
	g_strfreev(uris);
}

void on_drag_data_received(GtkWidget *widget,
	GdkDragContext *drag_context,
	gint x,
	gint y,
	GtkSelectionData *data,
	guint info,
	guint time,
	gpointer user_data)
{
	FileList *pFileList = (FileList*)user_data;
	Vfs *pVfsDst = pFileList->m_ctrl.m_pVfs;
	GtkTreePath *path = NULL;
	bool bSubDir = false;
	String strDropitemName;

	//step 1: Get target path
	String strDestTemplate = pFileList->m_ctrl.m_pVfs->GetDir();
	PathName::EnsureTerminated(strDestTemplate);

	//was the drop to an item of the list?
	if (gtk_tree_view_get_path_at_pos((GtkTreeView *)widget, x, y - 24, &path, NULL, NULL, NULL))
	{
		int nIdxClicked = gtk_tree_path_get_indices(path)[0];
		VfsItem &dropitem = pFileList->m_ctrl.m_lstItems.GetAt(nIdxClicked);
		strDropitemName = strDestTemplate;
		strDropitemName += dropitem.GetName();
		pVfsDst->FixPath(strDropitemName); 

		//is the item the drop occured a directory?
		if(dropitem.IsDir() || dropitem.IsDots())
		{
			//append this directory to strDestTemplate
			bSubDir = true;
			strDestTemplate += dropitem.GetName();
			PathName::EnsureTerminated(strDestTemplate);
		}
	}

	if(!bSubDir)
	{
		//return if source and target widget are the same
		if(gtk_drag_get_source_widget(drag_context) == widget)
			return;
	}

	//step 2: perform DnD action
	pVfsDst->FixPath(strDestTemplate); 
	String strOldDstDir(pVfsDst->GetDir());
	pVfsDst->SetDir(strDestTemplate);

	gchar **uris = g_uri_list_extract_uris((gchar *)data->data);
	if(!uris)
		return;

	pFileList->StopNotifications(pFileList->m_ctrl.m_pVfs, pFileList->m_pOtherList->m_ctrl.m_pVfs); //stop notifications during operation

	for(int i = 0; uris[i]; i++)
	{
		gchar *pFilename = g_filename_from_uri(uris[i], NULL, NULL);

		if(pFilename)
		{
			Vfs_Local VfsSrc;
			bool bAbort;
			VfsListing vfslisting;

			//cancel if a directory was dropped on itself
			if(!i && !strDropitemName.Cmp(pFilename))
			{
				g_free(pFilename);
				g_strfreev(uris);
				pVfsDst->SetDir(strOldDstDir);
				pFileList->StartNotifications(pFileList->m_ctrl.m_pVfs, pFileList->m_pOtherList->m_ctrl.m_pVfs);
				return;
			}

			//create a VfsListing for getting a VfsItem of pFilename
			VfsSrc.SetDir(PathName::GetParentDirPath(pFilename).c_str());
			VfsSrc.CachedListDir(vfslisting, bAbort, true);
			int iIdx = vfslisting.FindItem(PathName::GetBaseName(pFilename));
			if(iIdx >= 0)
			{
				//use the VfsItem to create VfsSelection
				VfsSelection objSel;
				objSel.m_lstRootItems.push_back(vfslisting.GetAt(iIdx));

				String strDest(strDestTemplate);
				strDest += PathName::GetBaseName(pFilename);
				pVfsDst->FixPath(strDest); 

				//start copy/move operation with progress (op executing in separate thread)
				g_objOpManager.m_strParam1 = strDest;
				if(GDK_ACTION_COPY == drag_context->action)
					g_objOpManager.StartOperation(OP_COPY, &VfsSrc, pVfsDst, objSel);
				else if(GDK_ACTION_MOVE == drag_context->action)
					g_objOpManager.StartOperation(OP_MOVE, &VfsSrc, pVfsDst, objSel);
#ifdef _WIN32
				//handle drag from explorer
				else if(GDK_DRAG_PROTO_WIN32_DROPFILES == drag_context->protocol)
					g_objOpManager.StartOperation(OP_COPY, &VfsSrc, pVfsDst, objSel);
#endif
			}
			g_free(pFilename);
		}
	}

	g_strfreev(uris);

	// restore old vfs dir
	pVfsDst->SetDir(strOldDstDir);

	//refresh panels
	pFileList->m_pOtherList->PanelList(true, true);
	pFileList->PanelList(true, true);

	pFileList->StartNotifications(pFileList->m_ctrl.m_pVfs, pFileList->m_pOtherList->m_ctrl.m_pVfs); //restart notifications after operation
}

gboolean on_drag_motion(GtkWidget *widget,
	GdkDragContext *drag_context,
	gint x,
	gint y,
	guint time,
	gpointer user_data)
{
	//do a copy per default, move on shift-drag
	gdk_drag_status(drag_context, drag_context->suggested_action == GDK_ACTION_COPY
		? GDK_ACTION_COPY : GDK_ACTION_MOVE, time);
	return false;
}

#ifdef _WIN32
gboolean on_drag_end(GtkWidget *widget,	GdkDragContext *drag_context, gpointer user_data)
{
	DROPFILES dfObj;
	HWND hWnd;
	FileList *pFileList = (FileList*)user_data;

	// skip everything, if drop was on atol window
	if(drag_context->dest_window || drag_context->action)
		return false;

	if(!GetCursorPos(&dfObj.pt))
		return false; //error

	hWnd = WindowFromPoint(dfObj.pt);

	//while(GetParent(hWnd))
	//	hWnd = GetParent(hWnd);

	if(!hWnd)
		return false; //error

	dfObj.pFiles = sizeof(DROPFILES);
	dfObj.fNC  = TRUE;
	dfObj.fWide  = FALSE; //TOFIX unicode version?

	String strDir(pFileList->m_ctrl.m_pVfs->GetDir().c_str());
	PathName::EnsureTerminated(strDir);
	pFileList->m_ctrl.m_pVfs->FixPath(strDir);

	VfsSelection objSel;
	pFileList->GetSelection(objSel, true, true);
	
	//calc size of buffer to store path names (double null terminated)
	int nItems = objSel.GetTotalCount();
	int nPathLen = 1 + nItems * (strDir.Length() + 1);
	int i;
	for(i = 0; i < nItems; i++)
		nPathLen += objSel.m_lstRootItems[i].GetName().length();

	//total size
	int nGblLen  = sizeof(DROPFILES) + nPathLen;//lots of nulls and multibyte_char

	//allocate memory buffer
	HGLOBAL hGbl = GlobalAlloc(GMEM_ZEROINIT|GMEM_MOVEABLE|GMEM_DDESHARE,nGblLen);
	char* sData = (char*)::GlobalLock(hGbl);

	//initialize memory buffer
	ZeroMemory(sData, nGblLen);
	memcpy(sData, &dfObj, sizeof(DROPFILES));
	char* sWStr = sData + sizeof(DROPFILES);
	for(i = 0; i < nItems; i++)
	{
		std::string strPath(strDir);
		strPath += objSel.m_lstRootItems[i].GetName().c_str();
		int nLen = strPath.size();
		memcpy(sWStr, strPath.c_str(), nLen);
		sWStr += nLen + 1;
	}

	::GlobalUnlock(hGbl);

	PostMessageA(hWnd, WM_DROPFILES, (WPARAM)hGbl, 0L);
	return false;
}
#endif 

void FileList::Create()
{
	gchar *header[] = {
		_("Name"),
		_("Ext"),
		_("Size"),
		_("Date"),
		_("Attr")
	};

	// create file list
	GtkListStore *store = gtk_list_store_new (2, GDK_TYPE_PIXBUF, G_TYPE_STRING);
  	GtkWidget *treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	g_object_unref (G_OBJECT (store));  //tree now holds reference

	gtk_widget_show (treeview);
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview), TRUE);
	gtk_tree_view_set_headers_clickable (GTK_TREE_VIEW(treeview), TRUE);
	gtk_tree_view_set_enable_search(GTK_TREE_VIEW(treeview),FALSE);
	g_signal_connect (treeview, "row-activated", G_CALLBACK (on_item_executed), this);
	g_signal_connect (treeview, "key_press_event", G_CALLBACK (treeview_keyboard_handler), this);
	g_signal_connect (treeview, "button-press-event", G_CALLBACK (on_button_press), this);
	g_signal_connect (treeview, "popup-menu", G_CALLBACK (on_popup_menu), this);

	//add icon column
	GtkCellRenderer *renderer, *renderer1;

	//pack two renderers in the single column
	GtkTreeViewColumn *col = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(col, header[0]);

	renderer = gtk_cell_renderer_pixbuf_new();
	gtk_tree_view_column_pack_start(col, renderer, FALSE);
	gtk_tree_view_column_set_attributes(col, renderer, "pixbuf", 0, NULL);
	gtk_tree_view_column_set_cell_data_func(col, renderer, FileListIconFunc, this,  NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(col, renderer, TRUE);
	gtk_tree_view_column_set_attributes(col, renderer, "text", 1, NULL);
	gtk_tree_view_column_set_cell_data_func(col, renderer, FileListDataFunc, this,  NULL);
	g_signal_connect(renderer, "edited", (GCallback) cell_edited_callback, this);
	g_signal_connect(renderer, "editing-started", (GCallback) cell_edited_start_callback, this);
	g_signal_connect(renderer, "editing-canceled", (GCallback) cell_edited_cancel_callback, this);

	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), col);
	
	int nPos = 1;

	//additional columns
	if(g_bShowExtColumn)
		gtk_tree_view_insert_column_with_data_func(GTK_TREE_VIEW (treeview), nPos, header[1],	renderer, FileListDataFunc, this,  NULL);
	nPos++;

	//"Size" column has right aligned data - we must use separate renderer
	renderer1 = gtk_cell_renderer_text_new();
	g_object_set (G_OBJECT (renderer1), "xalign", 1.0, NULL);
	
	gtk_tree_view_insert_column_with_data_func(GTK_TREE_VIEW (treeview), nPos++, header[2],	renderer1, FileListDataFunc, this,  NULL);
 
	//more columns
	gtk_tree_view_insert_column_with_data_func(GTK_TREE_VIEW (treeview), nPos++, header[3],	renderer, FileListDataFunc, this,  NULL);
	gtk_tree_view_insert_column_with_data_func(GTK_TREE_VIEW (treeview), nPos++, header[4],	renderer, FileListDataFunc, this,  NULL);

	//initialize columns
	int i=0; 
	for(i=0; i<5; i++)
	{
		cols[i] = gtk_tree_view_get_column(GTK_TREE_VIEW (treeview), i);
		if(NULL != cols[i]){
			gtk_tree_view_column_set_clickable(cols[i], TRUE);	//allow column header clicks
			gtk_tree_view_column_set_sizing (cols[i], GTK_TREE_VIEW_COLUMN_FIXED);
			gtk_tree_view_column_set_resizable(cols[i], TRUE);
			g_signal_connect (cols[i], "clicked", G_CALLBACK (on_header_clicked), this);

		#ifdef _WIN32	//until fixed to work on Linux too
			// Give the header labels a smaller height:
			// Create a vbox containing an empty dummy label and the really used label.
			// Setting a negative spacing results in the real label being drawn higher as usual.
			// Then cut the button by using gtk_widget_set_size_request
			GtkWidget *vbox = gtk_vbox_new (FALSE, -15);
			gtk_widget_show (vbox);

			GtkWidget *label = gtk_label_new("");
			gtk_widget_show(label);
			gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, TRUE, 0);

			if(i>=1 && !g_bShowExtColumn)
				label = gtk_label_new(header[i+1]);
			else
				label = gtk_label_new(header[i]);

			gtk_widget_show(label);
			gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, TRUE, 0);

			g_object_set(G_OBJECT(cols[i]), "widget", vbox, (gchar *)NULL);

			GtkWidget *widget;
			g_object_get(G_OBJECT(cols[i]), "widget", &widget, NULL);
			widget = gtk_widget_get_parent(widget);
			widget = gtk_widget_get_parent(widget);
			gtk_widget_set_size_request(widget, -1, 10);

			// do not show focus rectangle on buttons
			widget = gtk_widget_get_parent(widget);
			gtk_button_set_focus_on_click(GTK_BUTTON(widget), false);

		#endif
		}
	}

	//default column sizes
	gtk_tree_view_column_set_fixed_width(cols[0], 180);
	gtk_tree_view_column_set_fixed_width(cols[1],  50);
	gtk_tree_view_column_set_fixed_width(cols[2],  90);
	gtk_tree_view_column_set_fixed_width(cols[3],  90);
	gtk_tree_view_column_set_fixed_width(cols[4],  40);

	//right align "size" column header title
	gtk_tree_view_column_set_alignment(cols[g_bShowExtColumn ? 2 : 1], 1.0);

	//allow multiple selection
	GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

	//register selection change signal
	g_signal_connect (selection, "changed", G_CALLBACK (on_selection_changed), this);
	
	//right click - context menu 	
	g_signal_connect (treeview, "button-release-event", G_CALLBACK (on_ctx_menu_activate), this);

	m_pWidget = treeview;
	
	//
	// set up drag & drop
	//
	gtk_drag_dest_set(GTK_WIDGET(treeview),
		GTK_DEST_DEFAULT_ALL,
		targets,
		G_N_ELEMENTS (targets),
		GdkDragAction(GDK_ACTION_COPY|GDK_ACTION_MOVE));

	g_signal_connect(G_OBJECT(treeview), "drag_data_get",     G_CALLBACK(on_drag_data_get), this);
	g_signal_connect(G_OBJECT(treeview), "drag_data_received",G_CALLBACK(on_drag_data_received), this);
	g_signal_connect(G_OBJECT(treeview), "drag_motion",       G_CALLBACK(on_drag_motion), this);
#ifdef _WIN32
	g_signal_connect(G_OBJECT(treeview), "drag_end",          G_CALLBACK(on_drag_end), this);
#endif 
}

void FileList::Copy()
{
	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	GetSelection(objSel, true, true);
	
	//last checks
	if(0 == objSel.GetTotalCount()){
		gtkMessageBox(_("No selection!"));
		return;
	}

	Vfs *pVfsSrc = m_ctrl.m_pVfs;
	Vfs *pVfsDst = m_pOtherList->m_ctrl.m_pVfs;

	 //create name to be displayed in the copy dialog
	String strDestTemplate = pVfsDst->GetDir();
	PathName::EnsureTerminated(strDestTemplate);
	if(1 == objSel.GetRootCount()){
		//single root item, copy its full path name
		strDestTemplate += objSel.m_lstRootItems[0].GetName();
	}
	else{
		//multiple root items to be copied, use common directory path
	}
	pVfsDst->FixPath(strDestTemplate); 

	//operation start dialog
	GuiInputDlg dlg;
	dlg.SetLabel(_("Copy file(s)"));
#ifdef _WIN32
	dlg.SetInput(System::FileNameToUTF8(strDestTemplate).c_str(), true);
#else
	dlg.SetInput(strDestTemplate, true);
#endif
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	StopNotifications(pVfsSrc, pVfsDst); //stop notifications during operation

	//STEP 2: start copy operation with progress (op executing in separate thread)
	String strTarget = System::FileNameFromUTF8(dlg.GetInput()).c_str();
	g_objOpManager.m_strParam1 = strTarget;
	dlg.Destroy();
	g_objOpManager.StartOperation(OP_COPY, pVfsSrc, pVfsDst, objSel);

	//refresh panels (source panel only if necessary)
	//gdk_window_freeze_updates(g_wnd.m_pWidget->window);
	m_pOtherList->PanelList();
		
	//try to focus first copied item in the destination panel
	String strItem;
	if(1 == objSel.m_lstRootItems.size())
		strItem = PathName::GetBaseName(strTarget);
	else
		strItem = objSel.m_lstRootItems[0].GetName();
	int nIdx = m_pOtherList->m_ctrl.m_lstItems.FindItem(strItem);
	if(nIdx < 0)
		nIdx = 0;
	m_pOtherList->SetItemFocus(nIdx);

	if(g_objOpManager.m_bResult1 || m_ctrl.m_pVfs->IsEqualLocation(m_pOtherList->m_ctrl.m_pVfs))
		PanelList(true, true);

	//gdk_window_thaw_updates(g_wnd.m_pWidget->window);

	StartNotifications(pVfsSrc, pVfsDst); //restart notifications after operation
}

void FileList::Rename()
{
	GtkTreeView *treeview = GTK_TREE_VIEW(m_pWidget);

	//edit focused item or first selected one
	int nTargetIdx = GetItemFocus();
	if(nTargetIdx < 0)
	{
		VfsSelection sel;
		GetSelection(sel, true);
		if(sel.GetRootCount() > 0)
			nTargetIdx = m_ctrl.m_lstItems.FindItem(sel.m_lstRootItems[0].GetName());
	}

	if(nTargetIdx >= 0)
	{
		//make sure that the target is not ".." item (can not rename that)
		if(m_ctrl.m_lstItems.GetAt(nTargetIdx).IsDots())
			return;

		//quit quick search mode for both panels before the operation
		QuickSearchExit();
		m_pOtherList->QuickSearchExit();

		//from index to path
		char szLevel[50];
		sprintf(szLevel, "%d", nTargetIdx);
		GtkTreePath* path1 = gtk_tree_path_new_from_string(szLevel);

		//trigger cell editing (edit close is handled elsewhere)
		GtkTreeViewColumn *column1 = gtk_tree_view_get_column(treeview, 0);
		GList *list1 = gtk_tree_view_column_get_cell_renderers(column1);
		GtkCellRendererText *renderer = (GtkCellRendererText *)g_list_nth_data(list1, 1);
		
		g_object_set(renderer, "editable", TRUE, NULL);	//makes tree cells editable
		gtk_tree_view_set_cursor(treeview, path1, column1, TRUE);	//trigger edit
		g_object_set(renderer, "editable", FALSE, NULL);//makes tree cells non-editable
		
		g_list_free(list1);
		gtk_tree_path_free(path1);
	}
}

void FileList::Move()
{
	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	GetSelection(objSel, true, true);

	//last checks
	if(0 == objSel.GetTotalCount()){
		gtkMessageBox(_("No selection!"));
		return;
	}

	Vfs *pVfsSrc  = m_ctrl.m_pVfs;
	Vfs *pVfsDst  = m_pOtherList->m_ctrl.m_pVfs;

	//create name to be displayed in the copy dialog
	String strDestTemplate = pVfsDst->GetDir();
	PathName::EnsureTerminated(strDestTemplate);
	if(1 == objSel.m_lstRootItems.size()){
		//single root item, copy its full path name
		strDestTemplate += objSel.m_lstRootItems[0].GetName();
	}
	else{
		//multiple root items to be copied, use common directory path
	}
	pVfsDst->FixPath(strDestTemplate);

	//operation start dialog
	GuiInputDlg dlg;
	dlg.SetLabel(_("Move file(s)"));
#ifdef _WIN32
	dlg.SetInput(System::FileNameToUTF8(strDestTemplate).c_str(), true);
#else
	dlg.SetInput(strDestTemplate, true);
#endif
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	StopNotifications(pVfsSrc, pVfsDst); //stop notifications during operation

	//STEP 2: start move operation with progress (op executing in separate thread)
	String strTarget = System::FileNameFromUTF8(dlg.GetInput()).c_str();
	g_objOpManager.m_strParam1 = strTarget;
	dlg.Destroy();
	g_objOpManager.StartOperation(OP_MOVE, pVfsSrc, pVfsDst, objSel);

	//refresh panels
	m_pOtherList->PanelList();

	//try to focus first copied item in the destination panel
	String strItem;
	if(1 == objSel.m_lstRootItems.size())
		strItem = PathName::GetBaseName(strTarget);
	else
		strItem = objSel.m_lstRootItems[0].GetName();
	int nIdx = m_pOtherList->m_ctrl.m_lstItems.FindItem(strItem);
	if(nIdx < 0)
		nIdx = 0;
	m_pOtherList->SetItemFocus(nIdx);
	if(!m_pOtherList->IsItemVisible(nIdx))
		m_pOtherList->SetTopItem(nIdx);

	PanelList(true, true);
	
	StartNotifications(pVfsSrc, pVfsDst); //restart notifications after operation
}

void FileList::MkDir()
{
	GuiInputDlg dlg;
	dlg.SetLabel(_("Insert directory name"));
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	String strName = dlg.GetInput();
	if(strName.Length() < 1){
		gtkMessageBox(_("Invalid directory name!"));
		return;
	}

	Vfs *pVfsSrc = m_ctrl.m_pVfs;

	StopNotifications(pVfsSrc); //stop notifications during operation

	pVfsSrc->MkDir(strName);
	//TOFIX g_objOpManager.StartOperation(OP_MKDIR, pVfsSrc, pVfsDst, objSel);

	//refresh panels (other only if necessary)
	PanelList(false, true);
	if(m_ctrl.m_pVfs->IsEqualLocation(m_pOtherList->m_ctrl.m_pVfs))
		m_pOtherList->PanelList(true, true);

	//focus new directory in the file panel list
	int nPos = m_ctrl.m_lstItems.FindItem(strName);
	if(nPos >= 0){
		SelectNone();
		SetItemFocus(nPos);
	}

	StartNotifications(pVfsSrc); //restart notifications after operation
}

void FileList::Delete(bool bRecycleBin)
{
	//STEP 1: create list of items selected in currently active file panel
	VfsSelection objSel;
	GetSelection(objSel, true, true);

	//last checks
	if(0 == objSel.GetTotalCount()){
        	gtkMessageBox(_("No selection!"));
        	return;
	}

	TRACE("Delete required RecycleBin = %d\n", bRecycleBin);
	
	Vfs *pVfsSrc = m_ctrl.m_pVfs;
	Vfs *pVfsDst = m_pOtherList->m_ctrl.m_pVfs;

	// filter some cases where trash is not an option 
	if(pVfsSrc->GetType() != Vfs::LOCAL)
		bRecycleBin = false;	// only available for local Vfs
	else
	{
	#ifdef _WIN32
		UINT uiDriveType = GetDriveType(pVfsSrc->GetDir().Left(2));
		if(DRIVE_REMOVABLE == uiDriveType || DRIVE_REMOTE == uiDriveType)
			bRecycleBin = false;	// network and removable drives do not support recycle on Windows
 	#endif
	}
 
	//calculate list of all selected items
	String strList;
	int nCnt = objSel.GetRootCount();
	for(int i=0; i<nCnt; i++)
	{
	#ifdef _WIN32
		strList += System::FileNameToUTF8(objSel.m_lstRootItems[i].GetName()).c_str();
	#else
		strList += objSel.m_lstRootItems[i].GetName();
	#endif
		if(i < (nCnt-1))
			strList += '\n';
	}

	//ask for confirmation (display list of selected items)
	{
		DeleteStartDlg dlg(bRecycleBin);
		dlg.SetList(strList);
		if(GTK_RESPONSE_OK != dlg.ShowModal())
			return;
	}

	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	StopNotifications(pVfsSrc, pVfsDst); //stop notifications during operation

	g_objOpManager.m_nParam1 = bRecycleBin;	//pass additional params

	//STEP 2: start delete operation with progress (op executing in separate thread)
	g_objOpManager.StartOperation(OP_DELETE, pVfsSrc, pVfsDst, objSel);

	//refresh panels (other only if necessary)
	PanelList(true, true);
	if(m_ctrl.m_pVfs->IsEqualLocation(m_pOtherList->m_ctrl.m_pVfs))
		m_pOtherList->PanelList(true, true);

	StartNotifications(pVfsSrc, pVfsDst); //restart notifications after operation
}

void FileList::EditFile()
{
	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	int nTargetIdx = GetItemFocus();
	if(nTargetIdx < 0)
	{
		VfsSelection sel;
		GetSelection(sel, true);
		if(sel.GetRootCount() > 0)
			nTargetIdx = m_ctrl.m_lstItems.FindItem(sel.m_lstRootItems[0].GetName());
	}

	if(nTargetIdx >= 0)
	{
		VfsItem item(m_ctrl.m_lstItems.GetAt(nTargetIdx));
		if(item.IsDir())
			return;	//dirs can not be edited

		if(m_ctrl.m_pVfs->GetType() != Vfs::LOCAL)
		{
			//code to edit an non-local VFS entry (unpacked to temp file)
			String strTempFile = CreateTempFile(m_ctrl.m_pVfs, item);
			if(strTempFile.Length() <= 0)
				return;

			ExecutionThread *pThread = new ExecutionThread(this, strTempFile, g_strEditor);
			pThread->Run();
			return;
		}

		//edit focused/first selected item
		std::string arg(item.GetName());

		// quote string if it contains the spaces
	#ifdef _WIN32
		if(arg.find(' ') != std::string::npos)
		{
			arg.insert(0, "\"");
			arg += "\"";
		}
	#endif

		//TOFIX allow other editor -> Ini setting
		System::Execute(g_strEditor, arg, m_ctrl.m_pVfs->GetDir());
	}
}

void FileListDataFunc (GtkTreeViewColumn *tree_column, GtkCellRenderer *cell, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data)
{
	FileList *pObj = (FileList *)data;

	//calculate row number from iterator
	GtkTreePath *path = gtk_tree_model_get_path(tree_model, iter);
	int nIdx = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	if(nIdx >= pObj->m_ctrl.m_lstItems.GetCount())
		return;
	
	//set text data
	int nColIdx = pObj->GetColumnIndex(tree_column);

	if(!g_bShowExtColumn)
		if(nColIdx > 0)
			nColIdx ++;	// convert to coodrdinates as when "exe" would be used

	#ifdef _WIN32
	std::string strData = System::FileNameToUTF8(pObj->m_ctrl.m_lstItems.GetColumnText(nIdx, (VfsListing::_SORT_COL)(nColIdx+1)).c_str());
	#else
	std::string strData = pObj->m_ctrl.m_lstItems.GetColumnText(nIdx, (VfsListing::_SORT_COL)(nColIdx+1)).c_str();
	#endif

	if(!g_bShowExtColumn)
	{
		if(nColIdx == 0)
		{
			//show name with extension
			if(!pObj->m_ctrl.m_lstItems.GetAt(nIdx).IsDir())
				strData = System::FileNameToUTF8(pObj->m_ctrl.m_lstItems.GetAt(nIdx).GetName());
		}
	}

	const char *szTxt = g_strdup(strData.c_str());
	g_object_set (cell, "text", szTxt, NULL);
	g_free ((char *)szTxt);

	//TOFIX colorize rows/text based on dir/not dir?
}

void FileListIconFunc (GtkTreeViewColumn *tree_column, GtkCellRenderer *cell, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data)
{
	FileList *pObj = (FileList *)data;

	//calculate row number from iterator
	GtkTreePath *path = gtk_tree_model_get_path(tree_model, iter);
	int nIdx = gtk_tree_path_get_indices(path)[0];
	gtk_tree_path_free(path);

	if(nIdx >= pObj->m_ctrl.m_lstItems.GetCount())
		return;

	//TOFIX move to thread, here only return already calculated index or something ?
	//TOFIX add support for mime icons?
	//calculate icon
	GdkPixbuf *pixbuf = NULL;
	if(pObj->m_ctrl.m_lstItems.GetAt(nIdx).IsDots())
		pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&up_dir_xpm);
	else if(pObj->m_ctrl.m_lstItems.GetAt(nIdx).IsDir())
		pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&folder_xpm);
	else
	{
		//calculate file item icon
		String strExt = pObj->m_ctrl.m_lstItems.GetAt(nIdx).GetExt();
		if(0 == strExt.Cmp(".enc"))
			pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&lock_xpm);
		else if(NULL != g_PlugManager.FindArchiver(strExt))
			pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&archive_xpm);
		else
			pixbuf = gdk_pixbuf_new_from_xpm_data ((const char **)&blank_xpm);
	}

	g_object_set (cell, "pixbuf", pixbuf, NULL);
	g_object_unref (G_OBJECT (pixbuf));  //cell now holds reference
}

gint treeview_keyboard_handler(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	//process only key press events here
	if( event->type != GDK_KEY_PRESS )
		return FALSE;	

	FileList *pObj = (FileList *)data;
	guint32 cLetter = gdk_keyval_to_unicode (event->keyval);

	if( cLetter == 'Q' )
	{
		if( event->state & GDK_SHIFT_MASK )
		{
			//Shift+Q enters in quick search mode
			if (!pObj->m_bQuickSearchMode)
			{
				//start quick search mode
				pObj->m_bQuickSearchMode = true;
        
				pObj->m_strQuickPtrn = "";
				pObj->UpdateInfoBar();
				return TRUE; 
			}
		}
	}
	else if( event->keyval == GDK_Escape )
	{
		//quit quick search mode if inside one
		if(pObj->QuickSearchExit())    
			return TRUE;    //eat event 

		//TOFIX else clear command line
	}
	else if( event->keyval == GDK_space )
	{
		pObj->OnCalcDirSize();
		return TRUE;	
	}
	else if( event->keyval == GDK_Return )
	{
		if( event->state & GDK_CONTROL_MASK )
		{
			// Ctrl + Enter -> send selected file to command line
			int nTargetIdx = pObj->GetItemFocus();
			if(nTargetIdx < 0)
			{
				VfsSelection sel;
				pObj->GetSelection(sel, true);
				if(sel.GetRootCount() > 0)
					nTargetIdx = pObj->m_ctrl.m_lstItems.FindItem(sel.m_lstRootItems[0].GetName());
			}
			if(nTargetIdx >= 0)
			{
				g_wnd.AddToCommandLine(pObj->m_ctrl.m_lstItems.GetAt(nTargetIdx).GetName().c_str());
			}
		}
		else
		{
			on_item_executed (GTK_TREE_VIEW(widget), NULL, NULL, data);	//TOFIX
		}
		return TRUE;
	}
	else if( event->keyval == GDK_Page_Up || event->keyval == GDK_Left )
	{
		if( event->state & GDK_CONTROL_MASK  || event->keyval == GDK_Left )
		{
			//Ctrl + PgUp (or Left_Arrow) -> go to the parent directory
			pObj->GoUpDir();
			return TRUE;    //eat event
		}
		else if ( g_bNCSelection )
		{
			VfsSelection sel;
			pObj->GetSelection(sel);

			//scroll one page up
			g_signal_emit_by_name(pObj->m_pScrollWidget, "scroll-child", (GtkScrollType)GTK_SCROLL_PAGE_UP, FALSE, NULL);

			//set focus to the first visible item
			int nIdxStart = pObj->GetTopItem();
			if(nIdxStart >= 0)
				pObj->SetItemFocus(nIdxStart);

			pObj->SetSelection(sel);

			return TRUE;    //eat event
		}
	}
	else if( event->keyval == GDK_Page_Down  || event->keyval == GDK_Right )
	{
		if( event->state & GDK_CONTROL_MASK  || event->keyval == GDK_Right )
		{
			//Ctrl + PgDown (or Right_Arrow) -> go into dir or archive 
			//(but never execute it if not supported)
			pObj->GoIntoFile();
			return TRUE;    //eat event
		}
		else if ( g_bNCSelection )
		{
			VfsSelection sel;
			pObj->GetSelection(sel);

			//get top index
			int nTopIdx = pObj->GetTopItem();

			//scroll one page down
			g_signal_emit_by_name(pObj->m_pScrollWidget, "scroll-child", (GtkScrollType)GTK_SCROLL_PAGE_DOWN, FALSE, NULL);

			//set focus to the first visible item or last item if no scrolling occured
			int nTopIdx2 = pObj->GetTopItem();

			if(nTopIdx == nTopIdx2)
				pObj->SetItemFocus(pObj->m_ctrl.m_lstItems.GetCount()-1);	//focus last one
			else
				pObj->SetItemFocus(nTopIdx2);

			pObj->SetSelection(sel);

			return TRUE;    //eat event
		}
	}
	else if( event->keyval == GDK_Home )
	{
		if( event->state & GDK_MOD1_MASK )
		{
			//Alt+Home - go to the user's home directory
			//quit quick search mode for both panels before the operation
			pObj->GoHomeDir();
			return TRUE;    //eat event
		}
		if( event->state & GDK_CONTROL_MASK )
		{
			//Ctrl + Home -> go to the root directory
			//quit quick search mode for both panels before the operation
			pObj->QuickSearchExit();
			pObj->m_pOtherList->QuickSearchExit();

			if(pObj->m_ctrl.GoRootDir())
			{ 
				pObj->PanelList();

				//remember working directory
				if(pObj->m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
				{
					g_VfsManager.DriveHistoryAdd(pObj->m_ctrl.m_pVfs->GetDir());
					pObj->m_ctrl.m_lstHistory.Push(pObj->m_ctrl.m_pVfs->GetDir().c_str());
					update_history_buttons();
				}
			}
			return TRUE;    //eat event
		}
		else if ( g_bNCSelection )
		{
			VfsSelection sel;
			pObj->GetSelection(sel);

			//scroll to start
			g_signal_emit_by_name(pObj->m_pScrollWidget, "scroll-child", (GtkScrollType)GTK_SCROLL_START, FALSE, NULL);

			//set focus to the first item
			if(pObj->m_ctrl.m_lstItems.GetCount() > 0)
				pObj->SetItemFocus(0);

			pObj->SetSelection(sel);

			return TRUE;    //eat event
		}
	}
	else if( event->keyval == GDK_End )
	{
		if ( g_bNCSelection )
		{
			VfsSelection sel;
			pObj->GetSelection(sel);

			//scroll to end
			g_signal_emit_by_name(pObj->m_pScrollWidget, "scroll-child", (GtkScrollType)GTK_SCROLL_END, FALSE, NULL);

			//set focus to the last item
			int nIdx = pObj->m_ctrl.m_lstItems.GetCount() - 1;
			if(nIdx >= 0)
				pObj->SetItemFocus(nIdx);

			pObj->SetSelection(sel);

			return TRUE;    //eat event
		}
	}
	else if( cLetter == 'B' )
	{
		if( event->state & GDK_SHIFT_MASK )
		{
			//Shift+B -> drop bookmark menu on active panel
			g_dp.GetActivePanel().DropBookmarkMenu();
			return TRUE;    //eat event
		}
	}
	else if( event->keyval == '1' )
	{
		if( event->state & GDK_MOD1_MASK )
		{
			//Alt+1 -> drop left drive menu
			pObj->DropDriveMenu(true);
			return TRUE;    //eat event
		}
	}
	else if( event->keyval == '2' )
	{
		if( event->state & GDK_MOD1_MASK )
		{
			//Alt+2 -> drop right drive menu
			pObj->DropDriveMenu(false);
			return TRUE;    //eat event
		}
	}
	else if( event->keyval == GDK_F4 )
	{
		//F4 -> edit file
		pObj->EditFile();
		return TRUE;    //eat event
	}
	else if( event->keyval == GDK_F6 )
	{
		if( event->state & GDK_SHIFT_MASK )
		{
			// Shift + F6 -> inplace rename
			pObj->Rename();
			return TRUE;    //eat event
		}
		else
		{
			// F6 -> move
			pObj->Move();
			return TRUE;    //eat event
		}
	}
	else if( event->keyval == GDK_F7 )
	{
		// F7 -> make directory
		pObj->MkDir();
		return TRUE;    //eat event
	}	
	else if( event->keyval == GDK_Delete ||
		     event->keyval == GDK_F8 )
	{
		if( event->state & GDK_SHIFT_MASK )
		{
			// Shift + Del -> delete 
			// Shift + F8 -> delete 
			pObj->Delete();
			return TRUE;    //eat event
		}
		else
		{
			// F8 -> move to recycle bin
			// Del -> move to recycle bin
			pObj->Delete(true);
			return TRUE;    //eat event
		}
	}
	else if(event->keyval == GDK_Down)
	{
		if(g_bNCSelection)
		{
			int nIdx = pObj->GetItemFocus();

			VfsSelection sel;
			pObj->GetSelection(sel);

			if(pObj->m_bQuickSearchMode)
			{
				//restrict moving to matching items only
				int nPos = pObj->m_ctrl.m_lstItems.FindPattern(pObj->m_strQuickPtrn + "*", nIdx+1, true);
				if(nPos >= 0)
					pObj->SetItemFocus(nPos);
				//else
				//	wxBell();
			}
			else
			{
				if(nIdx+1 < pObj->m_ctrl.m_lstItems.GetCount()){
					pObj->SetItemFocus(nIdx+1);
				}
			}

			//restore selection
			//TOFIX reduce flicker
			pObj->SetSelection(sel);

			//TOFIX handle quick search mode
			return TRUE;    //eat event
		}

		//handle quick search in normal selection mode
		if(pObj->m_bQuickSearchMode)
		{
			//restrict moving to matching items only
			int nFocus = pObj->GetItemFocus();
			int nPos = pObj->m_ctrl.m_lstItems.FindPattern(pObj->m_strQuickPtrn + "*", nFocus+1, true);
			if(nPos >= 0)
				pObj->SetItemFocus(nPos);
			//else
			//	wxBell();
			return TRUE;    //eat event
		}
	}
	else if(event->keyval == GDK_Up)
	{
		if(g_bNCSelection)
		{
			int nIdx = pObj->GetItemFocus();

			VfsSelection sel;
			pObj->GetSelection(sel);

			if(pObj->m_bQuickSearchMode)
	   		{
				//restrict moving to matching items only
				int nPos = pObj->m_ctrl.m_lstItems.FindPattern(pObj->m_strQuickPtrn + "*", nIdx-1, false);
				if(nPos >= 0)
					pObj->SetItemFocus(nPos);
				//else
				//    wxBell();
				return TRUE;    //eat event
			}
			else
			{
				if(nIdx > 0)
					pObj->SetItemFocus(nIdx-1);
			}

			//restore selection
			//TOFIX reduce flicker
			pObj->SetSelection(sel);

			//TOFIX handle quick search mode

			return TRUE;    //eat event
		}

		//handle quick search in normal selection mode
		if(pObj->m_bQuickSearchMode)
	   	{
			//restrict moving to matching items only
			int nFocus = pObj->GetItemFocus();
			int nPos = pObj->m_ctrl.m_lstItems.FindPattern(pObj->m_strQuickPtrn + "*", nFocus-1, false);
			if(nPos >= 0)
				pObj->SetItemFocus(nPos);
			//else
			//    wxBell();
			return TRUE;    //eat event
		}
	}
	else if(event->keyval == GDK_BackSpace)
	{
		if(pObj->m_bQuickSearchMode)
	   	{
			//remove last pattern character
			int nLen = pObj->m_strQuickPtrn.Length();
			if(nLen > 0)
			{
				pObj->m_strQuickPtrn = pObj->m_strQuickPtrn.Left(nLen-1);
				int nPos = pObj->m_ctrl.m_lstItems.FindPattern(pObj->m_strQuickPtrn + "*", 0, true);
				if(nPos >= 0)
					pObj->SetItemFocus(nPos);
				//else
				//	wxBell();

				//exit quick search mode if last character deleted
				if(nLen <= 1)
					pObj->QuickSearchExit();
			}
			//else
			//	wxBell();
			return TRUE;    //eat event
		}
	}
	else if( cLetter == '+' )
	{
		if( !pObj->m_bQuickSearchMode &&
		    0 == (event->state & GDK_SHIFT_MASK) &&
		    0 == (event->state & GDK_CONTROL_MASK) )
		{
			on_select_select_activate(NULL, 0);
			return TRUE;    //eat event			
		}
	}
	else if( cLetter == '-' )
	{
		if( !pObj->m_bQuickSearchMode &&
		    0 == (event->state & GDK_SHIFT_MASK) &&
		    0 == (event->state & GDK_CONTROL_MASK) )
		{
			on_select_deselect_activate(NULL, 0);
			return TRUE;    //eat event			
		}
	}
	else if( event->keyval == 'x' && event->state & GDK_CONTROL_MASK )
	{
		//Ctrl + X -> cut to clipboard
		VfsSelection sel;
		pObj->GetSelection(sel, true);
		pObj->m_ctrl.m_pVfs->ClipboardCut(sel); 
		return TRUE;	 //eat event
	}
	else if( event->keyval == 'c' && event->state & GDK_CONTROL_MASK )
	{
		//Ctrl + C -> copy to clipboard
		VfsSelection sel;
		pObj->GetSelection(sel, true);
		pObj->m_ctrl.m_pVfs->ClipboardCopy(sel); 
		return TRUE;	 //eat event
	}
	else if( event->keyval == 'v' && event->state & GDK_CONTROL_MASK )
	{
		//Ctrl + V -> paste from clipboard
		pObj->m_ctrl.m_pVfs->ClipboardPaste(); 
		return TRUE;	 //eat event
	}
	else if( event->keyval == GDK_F12 )
	{
		VfsSelection sel;
		pObj->GetSelection(sel, true);
		pObj->m_ctrl.m_pVfs->ShowProperties(sel);
		return TRUE;	 //eat event
	} 
	else if(g_bNCSelection && event->keyval == GDK_Insert)
	{
		//NC selection mode
		//select focused item, move focus one place down
		int nIdx = pObj->GetItemFocus();
		if(nIdx >= 0)
		{
			VfsSelection sel;
			pObj->GetSelection(sel);

			if(nIdx+1 < pObj->m_ctrl.m_lstItems.GetCount())
				pObj->SetItemFocus(nIdx+1);

			//restore selection
			//TOFIX reduce flicker
			pObj->SetSelection(sel);

			//change item selection state
			if(pObj->IsItemSelected(nIdx))
				pObj->DeselectItem(nIdx);
			else
				pObj->SelectItem(nIdx);

			return TRUE;	//eat event
		}
	}
	else if(event->keyval == GDK_Tab) {
		//handle tab key press to move focus from one panel to the next
		gtk_widget_grab_focus(pObj->m_pOtherList->m_pWidget);
		return TRUE;	//eat event
	}
	else if( cLetter == 'p' )
	{
		if( event->state & GDK_CONTROL_MASK )
		{
			//Ctrl+P -> send current path to command line
			g_wnd.AddToCommandLine(pObj->m_ctrl.m_pVfs->GetDir().c_str());
			return TRUE;    //eat event
		}
	}

	//quick search mode processing
	if(0 == (event->state & GDK_CONTROL_MASK)) //Ctrl not allowed
	{
		if(0 == cLetter)
			return FALSE;

		pObj->m_bQuickSearchMode = true; //NOTE: comment this to disable automatic quick search mode
		if(pObj->m_bQuickSearchMode)
		{
			if('/' == cLetter && pObj->m_strQuickPtrn.Length()==0)
			{
				//special char '/' triggers search for file extension
				pObj->m_strQuickPtrn = "*."; 
			}
			else
			{
				//pObj->m_strQuickPtrn += cLetter;
				gchar *szData = g_filename_from_utf8((const gchar*)&cLetter, 1, NULL, NULL, NULL);
				pObj->m_strQuickPtrn += szData;
				g_free(szData);
			}

			TRACE("Quick search pattern: %s\n", pObj->m_strQuickPtrn.c_str());

			//quick search restarts from 0 since user could be above matching items
			int nPos = pObj->m_ctrl.m_lstItems.FindPattern(pObj->m_strQuickPtrn + "*", 0, true);
			if(nPos >= 0)
				pObj->SetItemFocus(nPos);
			else{
				//wxBell();
				pObj->m_strQuickPtrn = pObj->m_strQuickPtrn.Left(pObj->m_strQuickPtrn.Length()-1);
			}
			pObj->UpdateInfoBar();
			return TRUE;	//eat event
		} 	
	}	
	return FALSE;
}

bool FileList::GoUpDir()
{
	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	StopNotifications(m_ctrl.m_pVfs); //stop notifications during operation
	//m_ctrl.m_pVfs->Lock();

	if(m_ctrl.m_pVfs->IsRootDir())
	{
		TRACE("OnUpDir, isroot=1\n");

		String strRestore;
		if(m_ctrl.m_pVfs->GetType() == Vfs::ARCHIVE){
			strRestore = ((Vfs_Archive *)m_ctrl.m_pVfs)->GetArchivePath();    //remember dir before changes
			strRestore = PathName::GetBaseName(strRestore);
		}

		//we are inside root of current Vfs, go up in the Vfs stack?
		if(g_VfsManager.CheckForUpVfs(&m_ctrl, m_ctrl.m_pVfs))
		{
			TRACE("OnUpDir, detected up Vfs\n");

			PanelList();

			//select archive file in the list
			if(!strRestore.IsEmpty()){
				int nPos = m_ctrl.m_lstItems.FindItem(strRestore);
				if(nPos >= 0){
					SetItemFocus(nPos);
					TRACE("1: Select item %s\n", strRestore.c_str());
				}
			}

			//remember working directory
			if(m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
			{
				g_VfsManager.DriveHistoryAdd(m_ctrl.m_pVfs->GetDir());
				m_ctrl.m_lstHistory.Push(m_ctrl.m_pVfs->GetDir().c_str());
				update_history_buttons(); 
			}

			//m_ctrl.m_pVfs->Unlock();
			StartNotifications(m_ctrl.m_pVfs); //restart notifications after operation
			return true;	//climb one VFS up
		}
		else{
			//m_ctrl.m_pVfs->Unlock();
			StartNotifications(m_ctrl.m_pVfs); //restart notifications after operation
			TRACE("OnUpDir, root dir already set\n");
			return false;	//root dir
		}
	} 

	//climb one directory up, until first success
	//(to handle the case when the browsed directory was deleted inside another program)
	bool bSuccess = false;
	String strPath = m_ctrl.m_pVfs->GetDir();
	String strRestore = PathName::GetBaseName(strPath);
	while(!bSuccess && !m_ctrl.m_pVfs->IsRootDir())
	{
		TRACE("OnUpDir, go up dir!\n");
		//NOTE: must use UpDir instead of path calculation because of VfsNet
		bSuccess = m_ctrl.m_pVfs->UpDir();
	}

	//select directory name that we just moved from one level up
	if(bSuccess)
	{
		TRACE("OnUpDir, success - relist panel!\n");

		PanelList(false, false, false); 

		int nPos = m_ctrl.m_lstItems.FindItem(strRestore);
		if(nPos >= 0){
			SetItemFocus(nPos);
			TRACE("1: Select item: %s\n", strRestore.c_str());
		}
		else{
			SetItemFocus(0);
			TRACE("1: No selected item: %s, path: %s\n", strRestore.c_str(), strPath.c_str());
		}

		if(m_ctrl.m_pVfs->GetType()==Vfs::LOCAL){
			//remember history 
			m_ctrl.m_lstHistory.Push(m_ctrl.m_pVfs->GetDir().c_str());
			update_history_buttons();
		}
	}
	else{
		TRACE("OnUpDir, failed - set default drive!\n");

		//in case of failure (on Win32 entire drive can go away) switch to default dir
		g_VfsManager.VfsStackClean(&m_ctrl); // ensure no additional Vfs stacked
		SetDirectory(PathName::GetDefaultStartDir());
	}

	//remember working directory
	if(m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
		g_VfsManager.DriveHistoryAdd(m_ctrl.m_pVfs->GetDir());

	//restart notifications
	//m_ctrl.m_pVfs->Unlock();
	StartNotifications(m_ctrl.m_pVfs); //restart notifications after operation
	TRACE("OnUpDir, end!\n");
	return true;
}

void FileList::GoHomeDir()
{
	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	g_VfsManager.VfsStackClean(&m_ctrl); // ensure no additional Vfs stacked
	SetDirectory(System::GetHomeDir().c_str());
}

void FileList::GoIntoFile()
{
	QuickSearchExit();    //ensure quick search exited 
	m_pOtherList->QuickSearchExit();

	int nTargetIdx = GetItemFocus();
	if(nTargetIdx < 0)
	{
		VfsSelection sel;
		GetSelection(sel, true);
		if(sel.GetRootCount() > 0)
			nTargetIdx = m_ctrl.m_lstItems.FindItem(sel.m_lstRootItems[0].GetName());
	}
	
	VfsItem& item = m_ctrl.m_lstItems.GetAt(nTargetIdx);
	String strName = item.GetName();
	TRACE("Open item %d [%s]\n", nTargetIdx, strName.c_str());

	// calculate next path
	String strPath = m_ctrl.m_pVfs->GetDir();

	// dereference link
	VfsItem target;
	if(item.IsLink())
	{
		target = m_ctrl.m_pVfs->GetLinkTarget(item);
		if(target.GetName().Length() <= 0)
		{
			gtkMessageBox(_("Link can not be dereferenced!"));
			return;
		}
	}

	if(item.IsDots())
	{
		// Do nothing there is Ctrl+PgUp for this
	}
	else if(item.IsDir())
	{
		// browse some sub-directory
		PathName::EnsureTerminated(strPath);
		strPath += strName.c_str();
		m_ctrl.m_pVfs->FixPath(strPath);

		TRACE("Dir [%s]\n", strPath.c_str());
		SetDirectory(strPath);
	}
	else if(item.IsLink() && target.IsDir())
	{
		TRACE("Dir [%s]\n", target.GetPath().c_str());
		SetDirectory(target.GetPath());
	}
	else
	{
		// check for archive browsing (virtual subdirectory)
		int nRes = g_VfsManager.CheckForSubVfs(&m_ctrl, m_ctrl.m_pVfs, item, true);
		if (nRes > 0)
		{
			PanelList();
			return;
		}
		// else do nothing since we do not want to execute anything
	}
}

void FileList::OnItemDblClick(int nIdx)
{
	QuickSearchExit();    //ensure quick search exited 
	m_pOtherList->QuickSearchExit();

	VfsItem& item = m_ctrl.m_lstItems.GetAt(nIdx);
	String strName = item.GetName();
	TRACE("Exec item %d [%s]\n", nIdx, strName.c_str());

	// calculate next path
	String strPath = m_ctrl.m_pVfs->GetDir();

	// dereference link
	VfsItem target;
	if(item.IsLink())
	{
		target = m_ctrl.m_pVfs->GetLinkTarget(item);
		if(target.GetName().Length() <= 0)
		{
			gtkMessageBox(_("Link can not be dereferenced!"));
			return;
		}
	}

	if(item.IsDots())
	{
		// browse ".." directory
		GoUpDir();
	}
	else if(item.IsLink() && target.IsDir())
	{
		TRACE("Dir [%s]\n", target.GetPath().c_str());
		SetDirectory(target.GetPath());
	}
	else if(item.IsDir())
	{
		// browse some sub-directory
		PathName::EnsureTerminated(strPath);
		strPath += strName.c_str();
		m_ctrl.m_pVfs->FixPath(strPath);

		TRACE("Dir [%s]\n", strPath.c_str());
		SetDirectory(strPath);
	}
	else
	{
		// check for archive browsing (virtual subdirectory)
		int nRes = g_VfsManager.CheckForSubVfs(&m_ctrl, m_ctrl.m_pVfs, item);
		if (nRes > 0)
		{
			PanelList();
			return;
		}
		else if (nRes < 0)
		{
			// open file
			if (item.IsLink())
				m_ctrl.m_pVfs->Execute(target.GetPath().c_str(), false);
			else 
			{
#ifdef _WIN32
				if(m_ctrl.m_pVfs->GetType() == Vfs::ARCHIVE)
				{
					VfsItem item;
					item.SetName(strName.c_str());

					//code to edit an archive entry (unpacked to temp file)
					String strTempFile = CreateTempFile((Vfs_Archive *)m_ctrl.m_pVfs, item);
					if(strTempFile.Length() > 0)
					{
						ExecutionThread *pThread = new ExecutionThread(this, strTempFile);
						pThread->Run();
					}
				}
				else
#endif
					m_ctrl.m_pVfs->Execute(strName.c_str());
		   }
		}
	}
}

void on_item_executed (GtkTreeView *treeview, GtkTreePath *path, GtkTreeViewColumn *col, gpointer userdata)
{
	FileList *pObj = (FileList *)userdata;
	
	int nTargetIdx = pObj->GetItemFocus();
	if(nTargetIdx < 0)
	{
		VfsSelection sel;
		pObj->GetSelection(sel);
		if(sel.GetRootCount() > 0)
			nTargetIdx = pObj->m_ctrl.m_lstItems.FindItem(sel.m_lstRootItems[0].GetName());
	}

	//execute focused/first selected item
	if(nTargetIdx >= 0)
		pObj->OnItemDblClick(nTargetIdx);	//handle as double click
}

void FileList::Redraw()
{
	gdk_window_clear_area (m_pWidget->window, 
                        m_pWidget->allocation.x,
                        m_pWidget->allocation.y,
                        m_pWidget->allocation.width,
                        m_pWidget->allocation.height);
	gtk_widget_queue_draw (m_pWidget);
}

bool FileList::SetDirectory(const char *szDir, bool bAddToHistory, bool bSilent)
{
	StopNotifications(m_ctrl.m_pVfs); //stop notifications during operation
	//m_ctrl.m_pVfs->Lock();

	String strDir(szDir);

	while(!m_ctrl.m_pVfs->SetDir(strDir.c_str()))
 	{
		//if failed to set dir, try to set its parent dir (recursively)
		// - implemented for all Vfs except Net (Net path is PIDL based)
		//TOFIX test if can Net_Vfs use this
		if(PathName::IsRootDir(strDir) || m_ctrl.m_pVfs->GetType() == Vfs::NET)
 		{
			if(!bSilent)
			{
				String strMsg;
				strMsg.Printf(_("Failed to set directory: %s!"), szDir);
				gtkMessageBox(strMsg);
			}
			//m_ctrl.m_pVfs->Unlock();
			StartNotifications(m_ctrl.m_pVfs); //restart notifications after operation
			return false;
		}

		strDir = PathName::GetParentDirPath(strDir.c_str());
		PathName::EnsureNotTerminated(strDir); 
	}

	PanelList(); 
	
	if(bAddToHistory)
	{
		if(m_ctrl.m_pVfs->GetType()==Vfs::LOCAL){
			m_ctrl.m_lstHistory.Push(szDir);    //remember history 
			update_history_buttons();
		}
	}

	//remember working directory
	if(m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
		g_VfsManager.DriveHistoryAdd(m_ctrl.m_pVfs->GetDir());

	//m_ctrl.m_pVfs->Unlock();
	StartNotifications(m_ctrl.m_pVfs); //restart notifications after operation
	return true;
}

static int GetRowHeight(GtkTreeView *pTreeView)
{
	static int iHeight = -1;

	if(iHeight < 0)
	{
		GdkRectangle rect;
		GtkTreePath *path = gtk_tree_path_new_from_indices(0, -1);
		gtk_tree_view_get_cell_area(pTreeView, path, NULL, &rect);
		gtk_tree_path_free(path);
		iHeight = rect.y;
	}
	return iHeight;
}

void FileList::PanelList(bool bRestoreSelection, bool bForceRefresh, bool bSetFocus)
{
	int nScrollPos = -1;
	String strScrollName;
	int iScrollOffset = 0;

	//remember initial selection
	VfsSelection objSel;
	if(bRestoreSelection)
	{
		GetSelection(objSel);

		// find the item, that shall "stay" where it is
		// first choice is focused item
		nScrollPos = GetItemFocus();

		// if not found, use first one of the selection
		if(nScrollPos < 0 && objSel.GetTotalCount() > 0)
			nScrollPos = m_ctrl.m_lstItems.FindItem(objSel.m_lstRootItems[0].GetName());
		
		// get name of this item
		if(nScrollPos >= 0)
			strScrollName = m_ctrl.m_lstItems.GetAt(nScrollPos).GetName(); // get name of this item

		// get scroll offset
		GdkRectangle rect;
		gtk_tree_view_get_visible_rect((GtkTreeView *)m_pWidget, &rect);
		iScrollOffset = rect.y;
	}
	//m_ctrl.m_pVfs->Lock();

	//TOFIX list in a thread?
	bool bAbort = false;    //TOFIX allow aborting 
	bool bOk = m_ctrl.m_pVfs->CachedListDir(m_ctrl.m_lstItems, bAbort, bForceRefresh);
	//m_ctrl.m_pVfs->Unlock();
	if(!bOk)
	{
		m_ctrl.m_lstItems.Clear();
		m_bSkipSpaceInfo = true;
		RefilterList();
		m_bSkipSpaceInfo = false;
		gtkMessageBox(_("Current path is not accessible!"));
		if(m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
			SetDirectory(PathName::GetDefaultStartDir());	
		return;
	}

	TRACE("PanelList, done!\n");

	UpdatePathBar();

	TRACE("PanelList, path bar updated!\n");

	RefilterList();

	TRACE("PanelList, list refiltered!\n");

	//refresh sort indicator (GTK seems to forget it on relist)
	RefreshSortIndicator();

	if(!bOk)
		return;		//listing error TOFIX

	//refresh selection
	if(SetSelection(objSel) > 0)
	{
		int nNewScrollPos = m_ctrl.m_lstItems.FindItem(strScrollName);
		if(nNewScrollPos >= 0)
		{
			// the active item still exists. Keep its relative position
			int iNewY = iScrollOffset + (nNewScrollPos - nScrollPos) * GetRowHeight((GtkTreeView *)m_pWidget);

			TRACE("FileList: scroll to pos %d\n", nNewScrollPos);
			// possibly a GTK bug: Need to call gtk_tree_view_scroll_to_point twice
			gdk_window_freeze_updates(m_pWidget->window); //TOFIX does not work
			gtk_tree_view_scroll_to_point((GtkTreeView *)m_pWidget, -1, iNewY - 1);
			gtk_tree_view_scroll_to_point((GtkTreeView *)m_pWidget, -1, iNewY);
			gdk_window_thaw_updates(m_pWidget->window);
		}
	}
	else
	{
		//try to preserve focus index to be as close as before
		if(nScrollPos >= 0){
			int nCount = m_ctrl.m_lstItems.GetCount();
			if(nCount > 0){
				if(nScrollPos >= nCount)
					nScrollPos = nCount-1;
			}
			else
				nScrollPos = 0;
		}
		else
			nScrollPos = 0;

		if(bSetFocus)
			SetItemFocus(nScrollPos);
		
		// keep old scroll position
		TRACE("FileList: scroll to pos index: %d\n", nScrollPos);
		SetTopItem(nScrollPos);

		//TOFIX: Move this to a better place ?
		//allow DnD only for LOCAL Vfs type
		if(m_ctrl.m_pVfs->GetType() == Vfs::LOCAL)
		{
			if(!m_bDnDEnabled)
			{
				gtk_drag_source_set(GTK_WIDGET(m_pWidget),
					GDK_BUTTON1_MASK, 
					targets, 
					G_N_ELEMENTS (targets),
				    GdkDragAction(GDK_ACTION_COPY|GDK_ACTION_MOVE));
				m_bDnDEnabled = true;
			}
		}
		else
		{
			if(m_bDnDEnabled)
			{
				gtk_drag_source_unset(GTK_WIDGET(m_pWidget));
				m_bDnDEnabled = false;
			}
		}
	}

	TRACE("PanelList, end!\n");
}

void FileList::UpdatePathBar()
{
	if(m_pPathWidget && m_ctrl.m_pVfs)
	{
		String strPath = g_VfsManager.GetPathTitle(&m_ctrl);
		gtk_label_set_text(GTK_LABEL(m_pPathWidget), strPath);
	}
}

void FileList::RefilterList()
{
	m_ctrl.m_lstItems.Sort();	//this will refilter the list
	RefreshList();
	SetSortIndicator(m_ctrl.m_nSortColumn, m_ctrl.m_bSortAscending);	//refresh sort indicator
}

void FileList::RefreshList()
{
	int nCnt = m_ctrl.m_lstItems.GetCount();

	//TOFIX use virtual list ()
	GtkListStore *liststore = (GtkListStore *)gtk_tree_view_get_model(GTK_TREE_VIEW(m_pWidget));

	gtk_list_store_clear(liststore);

	//detach from screen to speedup insertions
	//g_object_ref(liststore); /* Make sure the model stays with us after the tree view unrefs it */
	//gtk_tree_view_set_model(GTK_TREE_VIEW(m_pWidget), NULL); /* Detach model from view */

	GtkTreeIter iter;
	for(int i=0; i<nCnt; i++)
		gtk_list_store_append(liststore, &iter);

	//gtk_tree_view_set_model(GTK_TREE_VIEW(m_pWidget), GTK_TREE_MODEL(liststore)); /* Re-attach model to view */
	//g_object_unref(liststore);

	UpdateInfoBar();
}

void on_header_clicked (GtkTreeViewColumn *treeviewcolumn, gpointer user_data)
{
	FileList *pObj = (FileList *)user_data;

	//calculate sort parameters
	int  nCol = pObj->GetColumnIndex(treeviewcolumn);
	bool bAsc = true;
	if(pObj->m_ctrl.m_nSortColumn == nCol)		//same column clicked twice
		bAsc = !pObj->m_ctrl.m_bSortAscending;	//just invert previous asc. state

	pObj->SetSort(nCol, bAsc);
}

int FileList::GetColumnIndex(GtkTreeViewColumn *column)
{
	for(int i=0; i<6; i++)
		if(cols[i] == column)
			return i;

	return -1;
}

bool FileList::IsItemSelected(int nIdx)
{
	bool bSelected = false;
	
	//fill selection object with selected items
	//get iterator to selected node
	GtkWidget *treeview = m_pWidget;
	GtkTreeModel *model = gtk_tree_view_get_model((GtkTreeView *)treeview);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)treeview);

	//multiple selection list
	GList* list = gtk_tree_selection_get_selected_rows(treesel, &model);

	int ncount = g_list_length(list);
	for(int i=0; i<ncount; i++)
	{
		GtkTreePath *path = (GtkTreePath *)g_list_nth(list, i)->data;
		int nItem = gtk_tree_path_get_indices(path)[0];	//get index from path

		if(nItem == nIdx)
		{
			bSelected = true;
			break;
		}
	}

	//cleanup list
	g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
	g_list_free (list);

	return bSelected;
}

void FileList::GetSelection(VfsSelection &sel, bool bIgnoreDots, bool bAddFocusOnEmpty)
{
	//remove old content
	sel.Clear();

	//fill selection object with selected items
	//get iterator to selected node
	GtkWidget *treeview = m_pWidget;
	GtkTreeModel *model = gtk_tree_view_get_model((GtkTreeView *)treeview);
	GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)treeview);

	//multiple selection list
	GList* list = gtk_tree_selection_get_selected_rows(treesel, &model);

	int ncount = g_list_length(list);
	for(int i=0; i<ncount; i++)
	{
		GtkTreePath *path = (GtkTreePath *)g_list_nth(list, i)->data;
		int nIdx = gtk_tree_path_get_indices(path)[0];	//get index from path

		//filter ".." items
		if(!bIgnoreDots || !m_ctrl.m_lstItems.GetAt(nIdx).IsDots())
			sel.m_lstRootItems.push_back(m_ctrl.m_lstItems.GetAt(nIdx));
	}

	//cleanup list
	g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
	g_list_free (list);

	//if nothing selected, store focused item
	if(sel.m_lstRootItems.size() == 0 && bAddFocusOnEmpty)
	{
		int nIdx = GetItemFocus();
		if(nIdx >= 0)
			if(!bIgnoreDots || !m_ctrl.m_lstItems.GetAt(nIdx).IsDots())
				sel.m_lstRootItems.push_back(m_ctrl.m_lstItems.GetAt(nIdx));
	}
}

int FileList::SetSelection(VfsSelection &sel)
{
	gdk_window_freeze_updates(m_pWidget->window); //TOFIX does not work

	int nCnt = 0;
	int nMax = sel.GetRootCount();
	for(int i=0; i<nMax; i++){
		int nPos = m_ctrl.m_lstItems.FindItem(sel.m_lstRootItems[i].GetName());
		if(nPos >= 0){
			SelectItem(nPos);
			nCnt ++;
		}
	}

	gdk_window_thaw_updates(m_pWidget->window);

	return nCnt;
}

void cell_edited_start_callback (GtkCellRenderer *renderer, GtkCellEditable *editable, gchar *path, gpointer user_data)
{
	FileList *pList = (FileList *)user_data;
	if(pList)
	{
		int nIdx = atoi(path); //index from path string
		
		pList->m_nEditedIdx = nIdx;	//store edited index

		//set entry text
		GtkEntry *entry = (GtkEntry *)editable;
		gtk_entry_set_text(entry, System::FileNameToUTF8(pList->m_ctrl.m_lstItems.GetAt(nIdx).GetName().c_str()).c_str());
	}
}

void cell_edited_cancel_callback (GtkCellRenderer *renderer, gpointer user_data)
{
	FileList *pList = (FileList *)user_data;
	if(pList)
		pList->m_nEditedIdx = -1;
}

void cell_edited_callback(GtkCellRendererText *cell, gchar *path_string, gchar *new_text, gpointer user_data)
{
	//get document node index from GUI tree iterator
	GtkTreePath* path1 = gtk_tree_path_new_from_string(path_string);
	
	//index from path string
	int nIdx = atoi(path_string);

	FileList *pList = (FileList *)user_data;
	if(pList)
	{
		//prepare full item name (old and new)
		String strOld = pList->m_ctrl.m_lstItems.GetAt(nIdx).GetName();
		String strDir = pList->m_ctrl.m_pVfs->GetDir();
		PathName::EnsureTerminated(strDir, '/');

		String strOldPath = strDir;
		strOldPath += strOld;
		String strNewPath = strDir;
		strNewPath += System::FileNameFromUTF8(new_text).c_str();

		Vfs *pVfsSrc = pList->m_ctrl.m_pVfs;

		pList->StopNotifications(pVfsSrc); //stop notifications during operation

		if(pVfsSrc->Rename(strOldPath, strNewPath))
		{
			//First rename item in old list, to restore selection after refresh
			pList->m_ctrl.m_lstItems.GetAt(nIdx).SetName(System::FileNameFromUTF8(new_text).c_str());
			//Then we recache file list. We need this for remote vfs, but not a problem to do it in local
			pList->PanelList(true, true);
		}
		else
		{
			//error report
			String strMsg;
			strMsg.Printf(_("Failed to rename %s"), (const char *)strOld);
			gtkMessageBox(strMsg);
		}

		pList->StartNotifications(pVfsSrc); //restart notifications after operation
	}
	
	pList->m_nEditedIdx = -1;
	gtk_tree_path_free(path1);
}

void FileList::UpdateInfoBar()
{
	if(m_pInfoWidget)
	{
		String strInfo;

		if(m_bQuickSearchMode)
		{
			//TOFIX bold font!
			//set red font
			GdkColor red = { 0, 0xFFFF, 0, 0 };
			gtk_widget_modify_fg(GTK_WIDGET(m_pInfoWidget), GTK_STATE_NORMAL, &red);

			strInfo.Printf(_("Search: %s"), m_strQuickPtrn.c_str());
		} 
		else
		{
			//set black font
			GdkColor black = { 0, 0, 0, 0 };
			gtk_widget_modify_fg(GTK_WIDGET(m_pInfoWidget), GTK_STATE_NORMAL, &black);

			VfsSelection sel;
			GetSelection(sel, true);

			int nSelCount   = sel.GetTotalCount(); 
			int nTotalCount = m_ctrl.m_lstItems.GetCount();
			if(!m_ctrl.m_pVfs->IsRootDir())
				nTotalCount --;	// do not count ".." in the total files

			//format selected size
			INT64  nSelSize = sel.GetTotalSize();
			String strSelSize = FormatSizeUnits(nSelSize); 

			//calculate and format total size
			INT64  nTotSize = 0;
			for(int i=0; i<m_ctrl.m_lstItems.GetCount(); i++)
			{
				if(!m_ctrl.m_lstItems.GetAt(i).IsDir())
					nTotSize += m_ctrl.m_lstItems.GetAt(i).m_nSize;
			}
			String strTotSize = FormatSizeUnits(nTotSize); 

			strInfo.Printf(_("%d/%d file(s) (%s/%s) selected"), nSelCount, nTotalCount, (const char *)strSelSize, (const char *)strTotSize);
			//_("%d/%d file (%s/%s) selected")
			//_("%d/%d files (%s/%s) selected")
			//strInfo.Printf(ngettext("%d/%d file (%s/%s) selected", "%d/%d files (%s/%s) selected", nTotalCount), 
			//	nSelCount, nTotalCount, (const char *)strSelSize, (const char *)strTotSize);
 
			if(!m_bSkipSpaceInfo)
			{
				INT64 nFree  = m_ctrl.m_pVfs->GetDriveFreeSpace();
				INT64 nTotal = m_ctrl.m_pVfs->GetDriveSize();
				
				if(nTotal >= 0)
				{
					String strFreeDrive = FormatSizeUnits(nFree); 
					String strTotDrive = FormatSizeUnits(nTotal); 

					String strSpace;
					strSpace.Printf(_("(Free %s of %s)"), (const char *)strFreeDrive, (const char *)strTotDrive);

					strInfo += " ";
					strInfo += strSpace;
				}
			}

			//TOFIX draw this part only in red color!??
			if(!m_ctrl.m_lstItems.GetFilter().IsEmpty())
			{
				String strFilter;
				strFilter.Printf(_(" Filter: %s"), m_ctrl.m_lstItems.GetFilter().GetDescription().c_str());
				strInfo += strFilter;
			} 
		}

		gtk_label_set_text(GTK_LABEL(m_pInfoWidget), strInfo);
	}
}

void  on_selection_changed (GtkTreeSelection *treeselection, gpointer user_data)
{
	FileList *pList = (FileList *)user_data;
	pList->UpdateInfoBar();
}

void FileList::SelectAll()
{
	GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)m_pWidget);
	gtk_tree_selection_select_all (treesel);
}

void FileList::SelectNone()
{
	GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)m_pWidget);
	gtk_tree_selection_unselect_all (treesel);
}

void FileList::SelectInvert()
{
	GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)m_pWidget);

	for(int i=0; i<m_ctrl.m_lstItems.GetCount(); i++)
	{
		char szPath[20];
		sprintf(szPath, "%d", i);
		GtkTreePath *path = gtk_tree_path_new_from_string (szPath);
		
		if(gtk_tree_selection_path_is_selected (treesel, path))
			gtk_tree_selection_unselect_path (treesel, path);
		else
			gtk_tree_selection_select_path (treesel, path);
		
		gtk_tree_path_free (path);
	}
}

int FileList::SelectFilesOnly (bool bUnselectOther)
{
	if(bUnselectOther)
		SelectNone();

	gdk_window_freeze_updates(m_pWidget->window);

	int nFileCnt = 0;
	for(int i=0; i<m_ctrl.m_lstItems.GetCount(); i++)
	{
		if(!m_ctrl.m_lstItems.GetAt(i).IsDir())
		{
			nFileCnt ++;
			SelectItem(i);
		}
	}

	gdk_window_thaw_updates(m_pWidget->window);

	return nFileCnt; 
}
	
void FileList::SelectItem(int nIdx)
{
	GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)m_pWidget);

	char szPath[20];
	sprintf(szPath, "%d", nIdx);
	GtkTreePath *path = gtk_tree_path_new_from_string (szPath);
	
	gtk_tree_selection_select_path (treesel, path);
	gtk_tree_path_free (path);
}

void FileList::DeselectItem(int nIdx)
{
	GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)m_pWidget);

	char szPath[20];
	sprintf(szPath, "%d", nIdx);
	GtkTreePath *path = gtk_tree_path_new_from_string (szPath);
	
	gtk_tree_selection_unselect_path (treesel, path);
	gtk_tree_path_free (path);
}

void FileList::SelectAdd()
{
	GuiInputDlg dlg;
	dlg.SetLabel(_("Select files"));
	//TOFIX remove last setting
#ifdef _WIN32	
	dlg.SetInput("*.*",true);
#else
	dlg.SetInput("*",true);
#endif
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	for(int i=0; i<m_ctrl.m_lstItems.GetCount(); i++)
	{
		//TOFIX implement full filtering here (not only by name)
		FilterDesc filter;
		filter.SetNameGroup(dlg.GetInput(), "");

		//if item matches pattern then select it
		for(int i=0; i<m_ctrl.m_lstItems.GetCount(); i++)
			if(filter.Match(m_ctrl.m_lstItems.GetAt(i)))
				SelectItem(i);
	}
}

void FileList::SelectRemove()
{
	GuiInputDlg dlg;
	dlg.SetLabel(_("Deselect files"));
	//TOFIX remember last setting
#ifdef _WIN32	
	dlg.SetInput("*.*",true);
#else
	dlg.SetInput("*",true);
#endif
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	for(int i=0; i<m_ctrl.m_lstItems.GetCount(); i++)
	{
		//TOFIX implement full filtering here (not only by name)
        FilterDesc filter;
        filter.SetNameGroup(dlg.GetInput(), "");

        //if item matches pattern then deselect it
		for(int i=0; i<m_ctrl.m_lstItems.GetCount(); i++)
			if(filter.Match(m_ctrl.m_lstItems.GetAt(i)))
				DeselectItem(i);
	}
}

void FileList::SetFilter()
{
	GuiInputDlg dlg;
	dlg.SetLabel(_("Filter list"));
	//TOFIX remember last setting
#ifdef _WIN32	
	dlg.SetInput("*.*",true);
#else
	dlg.SetInput("*",true);
#endif
	if(GTK_RESPONSE_OK != dlg.ShowModal())
		return;

	//TOFIX use full filtering here
	FilterDesc filter;
	filter.SetNameGroup(dlg.GetInput(), "");
	filter.AddGroupFlags(FILTER_SkipDirMatch);    //do not remove dirs
	m_ctrl.m_lstItems.SetFilter(filter);

	RefilterList();
}

void FileList::SetListing(VfsListing &list)
{
	m_ctrl.m_lstItems = list;

	UpdatePathBar();
	RefreshList();
}

void FileList::SetItemFocus(int nIdx)
{
	char szPath[20];
	sprintf(szPath, "%d", nIdx);
	GtkTreePath *path = gtk_tree_path_new_from_string (szPath);

	//is item selected
	bool bSelected = IsItemSelected(nIdx);

	//this API also selects the item
	gtk_tree_view_set_cursor(GTK_TREE_VIEW(m_pWidget), path, NULL, FALSE);
	gtk_tree_path_free (path);

	//restore selection state
	if(!bSelected) DeselectItem(nIdx);
}

int FileList::GetItemFocus()
{
	GtkTreePath *path = NULL;
	gtk_tree_view_get_cursor(GTK_TREE_VIEW(m_pWidget), &path, NULL);

	int nIdx = -1;
	if(NULL != path)
	{
		nIdx = gtk_tree_path_get_indices(path)[0];
		gtk_tree_path_free(path);
	}
	return nIdx;
}

//TOFIX move outside of this class?, separate thread, abortable
void FileList::OnCalcDirSize()
{
	//TOFIX check if file selected!!!
	//TOFIX escape press can abort the operation
	//wxBusyCursor wait;
	int nPos = GetItemFocus();
	if(nPos < 0)
		return;

	VfsItem item = m_ctrl.m_lstItems.GetAt(nPos);
	if(!m_ctrl.m_lstItems.GetAt(nPos).IsDir() ||
	    m_ctrl.m_lstItems.GetAt(nPos).IsDots())
		return;    //not a valid directory

	VfsSelectionItem itemSel(item);
	VfsSelection sel;
	sel.m_lstRootItems.push_back(itemSel);

	bool bAbort = false;
	m_ctrl.m_pVfs->ExpandSelection(sel, bAbort);

	//store result back into the item
	m_ctrl.m_lstItems.GetAt(nPos).m_nSize = sel.GetTotalSize();

	//TOFIX redraw item
	DeselectItem(nPos);
	SelectItem(nPos);
}

bool FileList::QuickSearchExit()
{
	//exit quick search
	if(m_bQuickSearchMode){
		m_bQuickSearchMode = false;
		m_strQuickPtrn = "";
		UpdateInfoBar();
		return true;
	}
	return false;
} 

void FileList::DropDriveMenu(bool bLeft)
{
	FileListPanel &panel = bLeft? g_dp.GetLeftPanel() : g_dp.GetRightPanel();
	panel.DropDriveMenu();
}

gboolean on_ctx_menu_activate (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	FileList *pObj = (FileList *)user_data;
	if(event->button == 3)
	{
		pObj->ContextMenu(widget, (gint)event->x, (gint)event->y + 25);
		return TRUE;
	}
	return FALSE;
}

void FileList::ContextMenu(GtkWidget *widget, int x, int y)
{
	//grab selected items
	VfsSelection sel;
	GetSelection(sel, true);

	//quit quick search mode for both panels before the operation
	QuickSearchExit();
	m_pOtherList->QuickSearchExit();

	//show context menu for these items 
	m_ctrl.m_pVfs->ShowCtxMenu(sel, x, y);
}

gint on_button_press(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
	gint nRet = FALSE;

	if(g_bNCSelection)
	{
		//activate clicked panel
		FileList *pObj = (FileList *)data;
		FileListPanel &panel = g_dp.GetPanelFromList(*pObj);
		panel.Activate();

		//allow handling left btn double-clicking
		if (1 == event->button)
		{
			//TOFIX better header height handling
			if((int)event->y < 20)	//header click
				return FALSE;	//keep processing (fixes column resizing)

			GtkTreePath *path = NULL;
			if (gtk_tree_view_get_path_at_pos((GtkTreeView *)widget, (gint)event->x, (gint)event->y, &path, NULL, NULL, NULL))
			{
				int nIdxClicked = gtk_tree_path_get_indices(path)[0]; //get index from path

				if(event->type == GDK_BUTTON_PRESS)
				{
					VfsSelection sel;
					pObj->GetSelection(sel);
					pObj->SetItemFocus(nIdxClicked);
					pObj->SetSelection(sel);	//restore previous selection
				}
				else if(event->type == GDK_2BUTTON_PRESS)
				{
					pObj->OnItemDblClick(nIdxClicked);
				}
				if (path)
					gtk_tree_path_free(path);
			}
		}
		return FALSE;	//do not eat event (or else dnd will stop working)
	}

	if (3 == event->button)
	{
		//handle/eat right click event over an existing selection
		//to prevent destruction of the selection 
		GtkTreePath *path = NULL;

		if (gtk_tree_view_get_path_at_pos((GtkTreeView *)widget, (gint)event->x, (gint)event->y, &path, NULL, NULL, NULL))
		{
			int nIdxClicked = gtk_tree_path_get_indices(path)[0]; //get index from path

			GtkTreeModel *model = gtk_tree_view_get_model((GtkTreeView *)widget);
			GtkTreeSelection* treesel = gtk_tree_view_get_selection ((GtkTreeView *)widget);

			//multiple selection list
			GList* list = gtk_tree_selection_get_selected_rows(treesel, &model);

			int ncount = g_list_length(list);
			for(int i=0; i<ncount; i++)
			{
				GtkTreePath *path = (GtkTreePath *)g_list_nth(list, i)->data;
				if (gtk_tree_path_get_indices(path)[0] == nIdxClicked)
				{
					nRet = TRUE; // eat event

					//activate clicked panel
					FileList *pObj = (FileList *)data;
					FileListPanel &panel = g_dp.GetPanelFromList(*pObj);
					panel.Activate();
				}
			}
		}

		if (path)
			gtk_tree_path_free(path);
	}

	return nRet;
}

//this handler enables keyboard-only access to the context menu
// -> use shortcut: Shift + F10
gint on_popup_menu(GtkWidget *widget, gpointer data)
{
	FileList *pObj = (FileList *)data;
	pObj->ContextMenu(widget, 0, 25);
	return TRUE;
}

void FileList::StartNotifications(Vfs *pVfsSrc, Vfs *pVfsDst)
{
	//restart notifications after operation
	if(pVfsSrc && pVfsSrc->GetType() == Vfs::LOCAL)
		((Vfs_Local *)pVfsSrc)->StartNotifications();
	if(pVfsDst && pVfsDst->GetType() == Vfs::LOCAL)
		((Vfs_Local *)pVfsDst)->StartNotifications();

	//restart notification cumulation timer
#ifdef _WIN32	//currently no support for Linux change notifier
	if(pVfsSrc && pVfsSrc->GetType() == Vfs::LOCAL)
	{
		if(m_nChangeCumulateTimer < 0)
		{
			TRACE("FileList: Create cumulation timer\n");
			//get current time
			UINT64 nCurTimeMs;
			StopWatch::GetCurTime(nCurTimeMs);
			nCurTimeMs = nCurTimeMs / 10000;	//convert to Ms

			m_nLastRefreshEventMs = nCurTimeMs;
			m_nChangeCumulateTimer = g_timeout_add (2000, DirChangeEvent_CumulateTimer, (void *)this); 
		}
	}
#endif
}

void FileList::StopNotifications(Vfs *pVfsSrc, Vfs *pVfsDst)
{
	//stop notifications during operation
	if(pVfsSrc && pVfsSrc->GetType() == Vfs::LOCAL)
		((Vfs_Local *)pVfsSrc)->StopNotifications();
	if(pVfsDst && pVfsDst->GetType() == Vfs::LOCAL)
		((Vfs_Local *)pVfsDst)->StopNotifications();

	//kill the timer
	if(m_nChangeCumulateTimer >= 0){
		g_source_remove (m_nChangeCumulateTimer);
		m_nChangeCumulateTimer = -1;
	}
}

gboolean DirChangeEvent_CumulateTimer(gpointer data)
{
	//TRACE("Change Cumulation timer\n");
	FileList *pList = (FileList *)data;
	CheckRefreshList(pList);
	return TRUE;
}

bool CheckRefreshList(FileList *pList)
{
	if(pList->m_nLastRefreshEventMs < pList->m_nLastChangeEventMs)
	{
		//get current time
		UINT64 nCurTimeMs;
		StopWatch::GetCurTime(nCurTimeMs);
		nCurTimeMs = nCurTimeMs / 10000;	//convert to Ms

		UINT64 nDiff1 = nCurTimeMs - pList->m_nLastChangeEventMs;
		UINT64 nDiff2 = nCurTimeMs - pList->m_nLastRefreshEventMs;

		TRACE("CheckRefreshList - check times\n");

		//if changes come too slow, or enough time passed since the start of cumulation
		if(nDiff1 > 2000 || nDiff2 > 7000)
		{
			TRACE("CheckRefreshList - time to refresh\n");

			//kill the timer
			//if(pList->m_nChangeCumulateTimer >= 0){
			//	g_source_remove (pList->m_nChangeCumulateTimer);
			//	pList->m_nChangeCumulateTimer = -1;
			//}

			TRACE("CheckRefreshList - timer killed\n");

			//refresh panel list in thread-safe manner
			//Refresh_ThrSafe((long)pList);
			pList->PanelList(true, true);

			pList->m_nLastRefreshEventMs = nCurTimeMs;
			TRACE("CheckRefreshList - time to exit\n");
			return true;
		}
	}

	return false;
}

bool FileList::IsItemVisible(int nIdx)
{
	GdkRectangle rect;
	gtk_tree_view_get_visible_rect((GtkTreeView *)m_pWidget, &rect);
	int nHeightTotal  = rect.y;
	int nHeightPerRow = GetRowHeight((GtkTreeView *)m_pWidget);
	
	int nItemsPerPage = nHeightTotal / nHeightPerRow;
	int nTopItemIdx	  = GetTopItem();

	if(nIdx < nTopItemIdx || nIdx >= nTopItemIdx + nItemsPerPage)
		return false;

	return true;
}

void FileList::SetTopItem(int nIdx)
{
	TRACE("FileList: scroll to pos index: %d\n", nIdx);
//	g_signal_emit_by_name(m_pScrollWidget, "scroll-child", (GtkScrollType)GTK_SCROLL_JUMP, nIdx, NULL);

	//from index to path
	char szLevel[50];
	sprintf(szLevel, "%d", nIdx);
	GtkTreePath* path1 = gtk_tree_path_new_from_string(szLevel);

	gtk_tree_view_scroll_to_cell    ((GtkTreeView *)m_pWidget,
                                             path1,
                                             NULL,
                                             false,
                                             0.0,
                                             0.0);

	gtk_tree_path_free(path1);
}

int FileList::GetTopItem()
{
	int nTopIdx = -1;
	GtkTreePath *path;
	if(gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW(m_pWidget), 1, 1, &path, NULL, NULL, NULL))
	{
		//path to index
		gchar *strPath = gtk_tree_path_to_string(path);
		nTopIdx = atoi(strPath);
		g_free(strPath);
		gtk_tree_path_free (path);
	}

	return nTopIdx;
}
