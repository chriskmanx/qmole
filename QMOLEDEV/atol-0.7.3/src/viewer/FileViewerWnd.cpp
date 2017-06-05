////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Main window for file viewer, implements file drawing
//////////////////////////////////////////////////////////////////////////// 

#include "FileViewerWnd.h"
#include "../support.h"
#include <gdk/gdkkeysyms.h>
#include "../core/String.h"
#include "../core/debug.h"

#ifndef min
	#define min(x,y) (((x)<(y))?(x):(y))
#endif
#ifndef max
	#define max(x,y) (((x)>(y))?(x):(y))
#endif

#define VIEWER_FONT_SIZE	12	
#define VIEWER_LINE_HEIGHT	(VIEWER_FONT_SIZE)*5/4

void Viewer_OnDestroy(FileViewerWnd *pObj);
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);
static gboolean expose_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data);
static gint viewer_keyboard_handler (GtkWidget *widget, GdkEventKey *event, gpointer data);
static gboolean on_window_delete_event (GtkWidget *widget, GdkEvent *event, gpointer data);
static void on_file_open2_activate (GtkMenuItem *menuitem, gpointer user_data);
static void on_quit2_activate (GtkMenuItem *menuitem, gpointer user_data);
static void on_text_view_activate (GtkMenuItem *menuitem, gpointer user_data);
static void on_bin_view_activate (GtkMenuItem *menuitem, gpointer user_data);
static void on_hex_view_activate (GtkMenuItem *menuitem, gpointer user_data);
static gboolean refresh_timer(gpointer data);
static gboolean on_scroll_event(GtkWidget *widget, GdkEvent *event, gpointer user_data);

FileViewerWnd::FileViewerWnd()
{
	m_pWidget = NULL;
	m_nTopLine = 0;
	m_nNumLinesPerPage = -1;
	m_nTotalLines = -1;
	m_nLeftOffset = 0;
	m_bDeleteOnClose = false;
	m_nTimer = -1;
}

FileViewerWnd::~FileViewerWnd()
{
}

void FileViewerWnd::Create()
{
	m_data.SetDrawMode(DRAW_TEXT); 	//TOFIX force BIN mode for now
	m_pWidget = create_viewer_window();
}

void FileViewerWnd::create_menu_bar(GtkWidget *viewer_window, GtkWidget *parent, GtkAccelGroup *accel_group)
{
	GtkWidget *menubar2;
	GtkWidget *menuitem8;
	GtkWidget *menu8;
	GtkWidget *open2;
	GtkWidget *separatormenuitem2;
	GtkWidget *quit2;
	GtkWidget *menuitem9;
	GtkWidget *menu9;
	GtkWidget *copy1;
	GtkWidget *select_all;
	GtkWidget *menuitem10;
	GtkWidget *menu10;
	GtkWidget *text_mode;
	GtkWidget *bin_mode;
	GtkWidget *hex_mode;
	GtkWidget *wrap;
	
	menubar2 = gtk_menu_bar_new ();
	gtk_widget_show (menubar2);
	gtk_box_pack_start (GTK_BOX (parent), menubar2, FALSE, FALSE, 0);
	
	menuitem8 = gtk_menu_item_new_with_mnemonic (_("_File"));
	gtk_widget_show (menuitem8);
	gtk_container_add (GTK_CONTAINER (menubar2), menuitem8);
	
	menu8 = gtk_menu_new ();
	ASSERT(NULL != menu8);
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem8), menu8);
	
	open2 = gtk_image_menu_item_new_from_stock ("gtk-open", accel_group);
	gtk_widget_show (open2);
	gtk_container_add (GTK_CONTAINER (menu8), open2);
	
	separatormenuitem2 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem2);
	gtk_container_add (GTK_CONTAINER (menu8), separatormenuitem2);
	gtk_widget_set_sensitive (separatormenuitem2, FALSE);
	
	quit2 = gtk_image_menu_item_new_from_stock ("gtk-quit", accel_group);
	gtk_widget_show (quit2);
	gtk_container_add (GTK_CONTAINER (menu8), quit2);

	menuitem9 = gtk_menu_item_new_with_mnemonic (_("_Edit"));
	gtk_widget_show (menuitem9);
	gtk_container_add (GTK_CONTAINER (menubar2), menuitem9);
	
	menu9 = gtk_menu_new ();
	ASSERT(NULL != menu9);
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem9), menu9);
	
	copy1 = gtk_image_menu_item_new_from_stock ("gtk-copy", accel_group);
	gtk_widget_show (copy1);
	gtk_widget_set_sensitive (copy1, FALSE);
	gtk_container_add (GTK_CONTAINER (menu9), copy1);

	select_all = gtk_menu_item_new_with_mnemonic (_("Select All"));
	gtk_widget_show (select_all);
	gtk_widget_set_sensitive (select_all, FALSE);
	gtk_container_add (GTK_CONTAINER (menu9), select_all);

	menuitem10 = gtk_menu_item_new_with_mnemonic (_("View"));
	gtk_widget_show (menuitem10);
	gtk_container_add (GTK_CONTAINER (menubar2), menuitem10);
	
	menu10 = gtk_menu_new ();
	ASSERT(NULL != menu10);
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem10), menu10);
	
	text_mode = gtk_menu_item_new_with_mnemonic (_("Text mode"));
	gtk_widget_show (text_mode);
	gtk_widget_add_accelerator (text_mode, "activate", accel_group, '1', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu10), text_mode);

	bin_mode = gtk_menu_item_new_with_mnemonic (_("Binary mode"));
	gtk_widget_show (bin_mode);
	gtk_widget_add_accelerator (bin_mode, "activate", accel_group, '2', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu10), bin_mode);

	hex_mode = gtk_menu_item_new_with_mnemonic (_("Hex mode"));
	gtk_widget_show (hex_mode);
	gtk_widget_add_accelerator (hex_mode, "activate", accel_group, '3', (GdkModifierType)GDK_CONTROL_MASK, (GtkAccelFlags)(GTK_ACCEL_VISIBLE));
	gtk_container_add (GTK_CONTAINER (menu10), hex_mode);

	separatormenuitem2 = gtk_separator_menu_item_new ();
	gtk_widget_show (separatormenuitem2);
	gtk_container_add (GTK_CONTAINER (menu10), separatormenuitem2);
	gtk_widget_set_sensitive (separatormenuitem2, FALSE);

	wrap = gtk_menu_item_new_with_mnemonic (_("Wrap text"));
	gtk_widget_show (wrap);
	gtk_widget_set_sensitive (wrap, FALSE);
	gtk_container_add (GTK_CONTAINER (menu10), wrap);

	g_signal_connect (open2, "activate", G_CALLBACK (on_file_open2_activate), this);
	g_signal_connect (quit2, "activate", G_CALLBACK (on_quit2_activate), this);
	g_signal_connect (text_mode, "activate", G_CALLBACK (on_text_view_activate), this);
	g_signal_connect (bin_mode, "activate", G_CALLBACK (on_bin_view_activate), this);
	g_signal_connect (hex_mode, "activate", G_CALLBACK (on_hex_view_activate), this);

	//TOFIX	
	//g_signal_connect (copy1, "activate", G_CALLBACK (on_copy1_activate), NULL);

	GLADE_HOOKUP_OBJECT (viewer_window, menubar2, "menubar2");
	GLADE_HOOKUP_OBJECT (viewer_window, menuitem8, "menuitem8");
	GLADE_HOOKUP_OBJECT (viewer_window, menu8, "menu8");
	GLADE_HOOKUP_OBJECT (viewer_window, open2, "open2");
	GLADE_HOOKUP_OBJECT (viewer_window, separatormenuitem2, "separatormenuitem2");
	GLADE_HOOKUP_OBJECT (viewer_window, quit2, "quit2");
	GLADE_HOOKUP_OBJECT (viewer_window, menuitem9, "menuitem9");
	GLADE_HOOKUP_OBJECT (viewer_window, menu9, "menu9");
	GLADE_HOOKUP_OBJECT (viewer_window, copy1, "copy1");
	GLADE_HOOKUP_OBJECT (viewer_window, menuitem10, "menuitem10");
	GLADE_HOOKUP_OBJECT (viewer_window, menu10, "menu10");
	GLADE_HOOKUP_OBJECT (viewer_window, text_mode, "text_mode");
	GLADE_HOOKUP_OBJECT (viewer_window, bin_mode, "bin_mode");
	GLADE_HOOKUP_OBJECT (viewer_window, hex_mode, "hex_mode");
}

GtkWidget *FileViewerWnd::create_viewer_window()
{
	GtkWidget *viewer_window;
	GtkWidget *vbox15;
	GtkWidget *scrollwin;
	GtkWidget *viewport1;
	GtkWidget *drawingarea1;
	GtkWidget *statusbar2;
	GtkAccelGroup *accel_group;
	
	accel_group = gtk_accel_group_new ();
	
	viewer_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (viewer_window), _("File Viewer"));
	gtk_widget_set_size_request (viewer_window, 600, 400);
	gtk_widget_show (viewer_window);

	vbox15 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox15);
	gtk_container_add (GTK_CONTAINER (viewer_window), vbox15);

	create_menu_bar(viewer_window, vbox15, accel_group);

	scrollwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_widget_show (scrollwin);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrollwin), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrollwin), GTK_SHADOW_NONE);
	gtk_box_pack_start (GTK_BOX (vbox15), scrollwin, TRUE, TRUE, 0);

	viewport1 = gtk_viewport_new (NULL, NULL);
	gtk_widget_show (viewport1);
	gtk_container_add (GTK_CONTAINER (scrollwin), viewport1); 

	drawingarea1 = gtk_drawing_area_new ();
	gtk_widget_show (drawingarea1);
	gtk_container_add (GTK_CONTAINER (viewport1), drawingarea1);

	//set white background
	GdkColor white = { 0, 0xFFFF, 0xFFFF, 0xFFFF };
	gtk_widget_modify_bg (drawingarea1, GTK_STATE_NORMAL, &white);

	//set custom (fixed width) font
#ifdef _WIN32
	PangoFontDescription *pfd = pango_font_description_new();
	pango_font_description_set_family (pfd, "monospace");
	pango_font_description_set_weight (pfd, (PangoWeight)PANGO_WEIGHT_NORMAL);
//	pango_font_description_set_size (pfd, VIEWER_FONT_SIZE);
#else
	//PangoFontDescription *pfd = pango_font_description_from_string(/*"Luxi Mono 12"*/"Fixed 8");
	PangoContext *context = gtk_widget_get_pango_context (drawingarea1);
	PangoFontDescription *pdef = pango_context_get_font_description(context);
	PangoFontDescription *pfd = pango_font_description_copy(pdef);	//copy from default font

//	pango_font_description_set_size (pfd, VIEWER_FONT_SIZE);
#endif
	gtk_widget_modify_font (drawingarea1, pfd);
	TRACE("Pango font: Family: %s, size: %d, weight: %d\n", pango_font_description_get_family(pfd), pango_font_description_get_size(pfd), pango_font_description_get_weight(pfd));
	pango_font_description_free(pfd);

	statusbar2 = gtk_statusbar_new ();
	gtk_widget_show (statusbar2);
	gtk_box_pack_start (GTK_BOX (vbox15), statusbar2, FALSE, FALSE, 0);
	
	gtk_widget_add_events(drawingarea1, GDK_SCROLL_MASK|GDK_BUTTON_PRESS_MASK);

	g_signal_connect (G_OBJECT (drawingarea1), "expose_event", G_CALLBACK (expose_event_callback), this);
	g_signal_connect (G_OBJECT (viewer_window), "key_press_event", G_CALLBACK (viewer_keyboard_handler), this);
	g_signal_connect (G_OBJECT (viewer_window), "delete_event", G_CALLBACK (on_window_delete_event), this);
	g_signal_connect (G_OBJECT (drawingarea1), "scroll-event", G_CALLBACK (on_scroll_event), this);
	
	/* Store pointers to all widgets, for use by lookup_widget(). */
	GLADE_HOOKUP_OBJECT_NO_REF (viewer_window, viewer_window, "viewer_window");
	GLADE_HOOKUP_OBJECT (viewer_window, vbox15, "vbox15");
	GLADE_HOOKUP_OBJECT (viewer_window, drawingarea1, "drawingarea1");
	GLADE_HOOKUP_OBJECT (viewer_window, statusbar2, "statusbar2");
	
	gtk_window_add_accel_group (GTK_WINDOW (viewer_window), accel_group);
	
	return viewer_window;
}

gboolean expose_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
	FileViewerWnd *pWnd = (FileViewerWnd *)data;
	if(!pWnd->m_data.IsOpen())
		return TRUE;	//nothing to draw, exit

	//draw some text
	PangoContext *context = gtk_widget_get_pango_context(widget);
	PangoLayout *layout = pango_layout_new (context);

	//TOFIX cache most of these values (refresh only when needed)
	pWnd->m_nLineHeight = VIEWER_LINE_HEIGHT;
	pWnd->m_nNumLinesPerPage = widget->allocation.height / pWnd->m_nLineHeight;  //TOFIX no need to refresh so often
	pWnd->m_nTotalLines = pWnd->m_data.GetLineCount();
	int nMax = min(pWnd->m_nTopLine + pWnd->m_nNumLinesPerPage, pWnd->m_nTotalLines);  //TOFIX

	TRACE("Viewer: total lines = %d, lines per page = %d\n", pWnd->m_nTotalLines, pWnd->m_nNumLinesPerPage);

	char *pszLine = NULL;
	int  nLineSize;

	gsize bytes_read, bytes_written;
	gchar *szStrUtf8;

	//draw text line by line 
	for(int i=pWnd->m_nTopLine; i<nMax; i++)
	{
		bool bRes = pWnd->m_data.GetLineFormated(i, (LPBYTE &)pszLine, nLineSize);

		//TOFIX temporary
		//replace \r \n \t chars with some other char (preferable non-printable)
		//because we don't want pango to break line for us or expand the tab
		//when not in text mode
		if(pWnd->m_data.IsBinMode() || pWnd->m_data.IsHexMode())
		{
			char *pszLetter;
			for(int i=0; i<nLineSize; i++){
				pszLetter = pszLine + i;
				if(*pszLetter == '\t' || *pszLetter == '\r' || *pszLetter == '\n')
					*pszLetter = ' ';  //TOFIX find some non-prinatable char? 
			}
		}

		szStrUtf8 = g_locale_to_utf8(pszLine, nLineSize, &bytes_read, &bytes_written, NULL); 

		if(!bRes)
			TRACE("Viewer error reading line into buffer\n");

		pango_layout_set_text(layout, szStrUtf8, -1);
		gdk_draw_layout (widget->window, widget->style->fg_gc[GTK_WIDGET_STATE (widget)], -pWnd->m_nLeftOffset, (i-pWnd->m_nTopLine)*pWnd->m_nLineHeight, layout);
		g_free(szStrUtf8);
	}

	g_object_unref(layout);

	return TRUE;
}

gint viewer_keyboard_handler(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	if( event->keyval == GDK_Escape )  //Esc closes the viewer
	{
		gtk_widget_destroy(widget);
		on_window_delete_event(widget, NULL, data);
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_Down)	//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		if(pWnd->m_nTopLine < pWnd->m_nTotalLines - 1){
			pWnd->m_nTopLine ++;
			pWnd->Invalidate();
		}
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_Up)		//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		if(pWnd->m_nTopLine > 0){
			pWnd->m_nTopLine --;
			pWnd->Invalidate();
		}
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_Page_Up)		//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		if(pWnd->m_nTopLine > 0){
			pWnd->m_nTopLine -= pWnd->m_nNumLinesPerPage;
			if(pWnd->m_nTopLine < 0)
				pWnd->m_nTopLine = 0;
			pWnd->Invalidate();
		}
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_Page_Down)		//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		if(pWnd->m_nTopLine < (pWnd->m_nTotalLines - pWnd->m_nNumLinesPerPage)){
			pWnd->m_nTopLine += pWnd->m_nNumLinesPerPage;
			pWnd->Invalidate();
		}
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_Home)		//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		if(pWnd->m_nTopLine > 0){
			pWnd->m_nTopLine = 0;
			pWnd->Invalidate();
		}
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_End)		//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		int nTop = max(0, pWnd->m_nTotalLines-pWnd->m_nNumLinesPerPage+1);
		pWnd->m_nTopLine = nTop;
		pWnd->Invalidate();
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_Left)		//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		if(pWnd->m_nLeftOffset > 0){
			pWnd->m_nLeftOffset --;
			pWnd->Invalidate();
		}
		return TRUE;    //eat event (handled here)
	}
	else if(event->keyval == GDK_Right)		//scrolling
	{
		FileViewerWnd *pWnd = (FileViewerWnd *)data;
		pWnd->m_nLeftOffset ++;	//TOFIX where is the limit
		pWnd->Invalidate();
		return TRUE;    //eat event (handled here)
	}

    return FALSE;
}

//on main window destruction
gboolean on_window_delete_event(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	//remove yourself from the list
	FileViewerWnd *pWnd = (FileViewerWnd *)data;
	Viewer_OnDestroy(pWnd);
	return FALSE;	//allows GTK to destroy window
}

const char *FileViewerWnd::GetFileName()
{
	return m_data.m_strFilePath.c_str();
}

void FileViewerWnd::FileLoad(const char *szPath)
{
	//load the file into the viewer
	if(m_data.Open(szPath))
	{
		//set window title
		String strTitle = _("File Viewer");
		strTitle += " - [";
		strTitle += szPath;
		strTitle += "]";

		gtk_window_set_title (GTK_WINDOW (m_pWidget), strTitle);

		m_nTopLine = 0;
		m_nLeftOffset = 0;
		Invalidate();

		TRACE("Viewer: file loaded\n");

		if(m_data.GetViewMode() == DRAW_TEXT)
		{
			//start refresh timer, while another thread is scanning for line ends
			TRACE("Viewer: Create refresh timer for text mode\n");
			m_nTimer = g_timeout_add (100, refresh_timer, this); 
		}
		else
		{
			TRACE("Viewer: Not a text mode, mode =%d\n", m_data.GetViewMode());
		}
	}
	else
		gtkMessageBox(_("Failed to open file!"));
}
	
void FileViewerWnd::FileClose()
{
	m_data.Close();
	//TOFIX other cleanups (data, ...) ???
}

void FileViewerWnd::Invalidate()
{
	GtkWidget *da1 = lookup_widget(m_pWidget, "drawingarea1");
	gtk_widget_queue_draw_area (da1, 0, 0, da1->allocation.width, da1->allocation.height);
}

void on_file_open2_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	GtkWidget *dialog;
	dialog = gtk_file_chooser_dialog_new(_("Open File"), NULL, 
			GTK_FILE_CHOOSER_ACTION_OPEN, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, 
			GTK_STOCK_OPEN, GTK_RESPONSE_OK, NULL);

	if(GTK_RESPONSE_OK == gtk_dialog_run(GTK_DIALOG(dialog)))
	{
		const char *szFile = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		FileViewerWnd *pWnd = (FileViewerWnd *)user_data;
		pWnd->FileLoad(szFile);
	}
	gtk_widget_destroy(dialog);
}

void on_quit2_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	FileViewerWnd *pWnd = (FileViewerWnd *)user_data;
	gtk_widget_destroy(pWnd->m_pWidget);
	on_window_delete_event(pWnd->m_pWidget, NULL, user_data);
}

void on_text_view_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	FileViewerWnd *pWnd = (FileViewerWnd *)user_data;
	pWnd->m_data.SetDrawMode(DRAW_TEXT);
	pWnd->Invalidate();
}

void on_bin_view_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	FileViewerWnd *pWnd = (FileViewerWnd *)user_data;
	pWnd->m_data.SetDrawMode(DRAW_BIN);
	pWnd->Invalidate();
}

void on_hex_view_activate (GtkMenuItem *menuitem, gpointer user_data)
{
	FileViewerWnd *pWnd = (FileViewerWnd *)user_data;
	pWnd->m_data.SetDrawMode(DRAW_HEX);
	pWnd->Invalidate();
}

gboolean refresh_timer(gpointer data)
{
	TRACE("viewer refresh timer\n");

	FileViewerWnd *pWnd = (FileViewerWnd *)data;
	if(!pWnd->m_data.IsTextMode()){
		TRACE("closing viewer refresh timer - not a text mode\n");
		return FALSE;
	}

	if(pWnd->m_nTotalLines != pWnd->m_data.GetLineCount()){
		//line count changed, refresh the display
		TRACE("Requesting viewer refresh\n");
		pWnd->Invalidate();
	}

	//TOFIX add IsTerminated() to thread class
	if( NULL == pWnd->m_data.m_pThread ||
	   (NULL != pWnd->m_data.m_pThread && ((Thread *)pWnd->m_data.m_pThread)->m_bDone))
	{
		TRACE("closing viewer refresh timer - thread terminated\n");
		return FALSE;
	}

	return TRUE;
}

gboolean on_scroll_event(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
	FileViewerWnd *pWnd = (FileViewerWnd *)user_data;
	
	if(GDK_SCROLL == event->type)
	{
		if (event->scroll.direction == GDK_SCROLL_UP)	// scroll wheel up
		{
			if(0 != (GDK_CONTROL_MASK & event->scroll.state))
			{
				//change font size
				GtkWidget *draw = lookup_widget(pWnd->m_pWidget, "drawingarea1");

				PangoContext *context = gtk_widget_get_pango_context (draw);
				PangoFontDescription *desc2 = pango_context_get_font_description(context);
				int nFontSize = pango_font_description_get_size (desc2);
				if(nFontSize > 4)
				{
					nFontSize --;

					PangoFontDescription *desc3 = pango_font_description_copy(desc2);
				#if GTK_CHECK_VERSION(2,6,0) //minimal version for pango_font_description_set_absolute_size	
					pango_font_description_set_absolute_size(desc3, nFontSize);
				#else	
					pango_font_description_set_size (desc3, nFontSize);	//TOFIX convert from pixels to fractions?
				#endif	 
					gtk_widget_modify_font(draw, desc3);
					pango_font_description_free(desc3);
					TRACE("Node title bar: Font size=%d\n", nFontSize);

					pWnd->Invalidate();
				}
			}
			else
			{
				//mouse wheel scrolling
				if(pWnd->m_nTopLine > 0){
					pWnd->m_nTopLine --;
					pWnd->Invalidate();
					return TRUE;
				}
			}
		}
		else if (event->scroll.direction == GDK_SCROLL_DOWN) // scroll wheel down
		{
			if(0 != (GDK_CONTROL_MASK & event->scroll.state))
			{
				//change font size
				GtkWidget *draw = lookup_widget(pWnd->m_pWidget, "drawingarea1");

				PangoContext *context = gtk_widget_get_pango_context (draw);
				PangoFontDescription *desc2 = pango_context_get_font_description(context);
				int nFontSize = pango_font_description_get_size (desc2);
				if(nFontSize < 35)
				{
					nFontSize ++;

					PangoFontDescription *desc3 = pango_font_description_copy(desc2);
				#if GTK_CHECK_VERSION(2,6,0) //minimal version for pango_font_description_set_absolute_size	
					pango_font_description_set_absolute_size(desc3, nFontSize);
				#else	
					pango_font_description_set_size (desc3, nFontSize);	//TOFIX convert from pixels to fractions?
				#endif	 
					gtk_widget_modify_font(draw, desc3);
					pango_font_description_free(desc3);
					TRACE("Node title bar: Font size=%d\n", nFontSize);
				}
			}
			else
			{
				//mouse wheel scrolling
				pWnd->m_nTopLine ++;	//TOFIX where is the maximum!!!???
				pWnd->Invalidate();
				return TRUE;
			}
		}
	}
	return FALSE;
}

