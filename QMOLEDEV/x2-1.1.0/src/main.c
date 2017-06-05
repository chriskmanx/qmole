#include <glib.h>
#include <glib/gstdio.h>
#include <gtk/gtk.h>
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <gtksourceview/gtksourceiter.h>
#include <stdlib.h>
#ifdef G_OS_UNIX 
#include <vte/vte.h>
#endif
#include <gdk/gdkkeysyms.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include "data.h"
#include "func_proto.h"
#include "callbacks.c"
#include "prefs.c"
#include "templates.c"

int main( int argc, char *argv[ ] ) 
{
	// Init gtk
	gtk_init( &argc, &argv );
	sets = g_malloc( sizeof( struct settings ) );
	load_prefs( );
	GError *error = NULL;
	
	// Create accel group
	GtkAccelGroup *Agr = gtk_accel_group_new( );

	// Define widgets
	window = gtk_window_new( GTK_WINDOW_TOPLEVEL );
	GtkWidget *box = gtk_vbox_new( FALSE, 0 );
	GtkWidget *Toolbar = gtk_toolbar_new( );
	GtkWidget *Notebook = gtk_notebook_new( );
	gtk_drag_dest_set( Notebook, GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_HIGHLIGHT, targets, n_targets, GDK_ACTION_COPY );

	// Set glib stuff, name, icon etc
	gchar *loc1 = "/usr/share/pixmaps/x2/x2.png";
	gchar *loc2 = "/usr/local/share/pixmaps/x2/x2.png";

	g_set_application_name( "X2" );
	GdkPixbuf *pixbuf;

	if( g_file_test( loc1, G_FILE_TEST_EXISTS ) )
		pixbuf = gdk_pixbuf_new_from_file( loc1, &error );

	else
		pixbuf = gdk_pixbuf_new_from_file( loc2, &error );

	gtk_window_set_icon( GTK_WINDOW( window ), pixbuf );

	// Tool items and tooltips
	GtkToolItem *tb_new = gtk_tool_button_new_from_stock( GTK_STOCK_NEW );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_new ), "Creates a new document" );
	GtkToolItem *tb_open = gtk_tool_button_new_from_stock( GTK_STOCK_OPEN );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_open ), "Open an existing document" );
	GtkToolItem *tb_save = gtk_tool_button_new_from_stock( GTK_STOCK_SAVE );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_save ), "Save current document" );
	GtkToolItem *tb_save_as = gtk_tool_button_new_from_stock( GTK_STOCK_SAVE_AS );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_save_as ), "Save current document in a specific location" );
	GtkToolItem *tb_undo = gtk_tool_button_new_from_stock( GTK_STOCK_UNDO );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_undo ), "Undo last operation" );
	GtkToolItem *tb_redo = gtk_tool_button_new_from_stock( GTK_STOCK_REDO );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_redo ), "Redo last operation" );
	GtkToolItem *tb_cut = gtk_tool_button_new_from_stock( GTK_STOCK_CUT );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_cut ), "Cut text" );
	GtkToolItem *tb_copy = gtk_tool_button_new_from_stock( GTK_STOCK_COPY );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_copy ), "Copy text" );
	GtkToolItem *tb_paste = gtk_tool_button_new_from_stock( GTK_STOCK_PASTE );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_paste ), "Paste text" );
	GtkToolItem *tb_find = gtk_tool_button_new_from_stock( GTK_STOCK_FIND );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_find ), "Open the search tool" );
	GtkToolItem *tb_replace = gtk_tool_button_new_from_stock( GTK_STOCK_FIND_AND_REPLACE );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_replace ), "Open the search and replace tool" );
	GtkToolItem *tb_jump = gtk_tool_button_new_from_stock( GTK_STOCK_JUMP_TO );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_jump ), "Open the jump to line tool" );
	GtkToolItem *Sep = gtk_separator_tool_item_new( );
	GtkToolItem *tb_prefs = gtk_tool_button_new_from_stock( GTK_STOCK_PREFERENCES );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_prefs ), "Preferences" );

	// Actually add the tool items
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_new, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), gtk_separator_tool_item_new( ), -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_open, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_save, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_save_as, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), gtk_separator_tool_item_new( ), -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_undo, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_redo, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), gtk_separator_tool_item_new( ), -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_cut, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_copy, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_paste, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), gtk_separator_tool_item_new( ), -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_find, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_replace, -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_jump, -1 );

	#ifdef G_OS_UNIX
	GtkToolItem *tb_cmd = gtk_tool_button_new_from_stock( GTK_STOCK_EXECUTE );
	gtk_widget_set_tooltip_text( GTK_WIDGET( tb_cmd ), "Show the terminal" );

	if( g_strrstr( sets->terminal, ":true" ) )
	{
		gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), gtk_separator_tool_item_new( ), -1 );
		gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_cmd, -1 );
		g_signal_connect( tb_cmd, "clicked", G_CALLBACK( Cmd ), Notebook );
		gtk_widget_add_accelerator( GTK_WIDGET( tb_cmd ), "clicked", Agr, GDK_T, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	}
	#endif
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), GTK_TOOL_ITEM( Sep ), -1 );
	gtk_toolbar_insert( GTK_TOOLBAR( Toolbar ), tb_prefs, -1 );

	// Create prefs_data object
	struct prefs_data *pd = g_malloc( sizeof( struct prefs_data ) );
	struct prefs *pref = g_malloc( sizeof( struct prefs ) ); 
	pd->Notebook = Notebook;
	pd->tb_cmd = tb_cmd;
	pd->pref = pref;

	// Connect signals
	g_signal_connect( window, "delete-event", G_CALLBACK ( destroy ), Notebook );
	g_signal_connect( tb_new, "clicked", G_CALLBACK( New ), Notebook );
	g_signal_connect( tb_open, "clicked", G_CALLBACK( Open ), Notebook );
	g_signal_connect( tb_save, "clicked", G_CALLBACK( Save ), Notebook );
	g_signal_connect( tb_save_as, "clicked", G_CALLBACK( SaveAs ), Notebook );
	g_signal_connect( tb_undo, "clicked", G_CALLBACK( Undo ), Notebook );
	g_signal_connect( tb_redo, "clicked", G_CALLBACK( Redo ), Notebook );
	g_signal_connect( tb_cut, "clicked", G_CALLBACK( Cut ), Notebook );
	g_signal_connect( tb_copy, "clicked", G_CALLBACK( Copy ), Notebook );
	g_signal_connect( tb_paste, "clicked", G_CALLBACK( Paste ), Notebook );
	g_signal_connect( tb_find, "clicked", G_CALLBACK( Find ), Notebook );
	g_signal_connect( tb_replace, "clicked", G_CALLBACK( Replace ), Notebook );
	g_signal_connect( tb_prefs, "clicked", G_CALLBACK( Prefs ), pd );
	g_signal_connect( tb_jump, "clicked", G_CALLBACK( Jump ), Notebook );
	g_signal_connect( Notebook, "drag-data-received", G_CALLBACK( drag_data_received_handler ), Notebook );
	g_signal_connect_after( Notebook, "switch-page", G_CALLBACK( Set_Title ), NULL );
	g_signal_connect_after( window, "key-press-event", G_CALLBACK( Catch_Keypress ), Notebook );

	// Layout window
	gtk_box_pack_start( GTK_BOX( box ), Toolbar, FALSE, TRUE, 0 );
	gtk_box_pack_start( GTK_BOX( box ), Notebook, TRUE, TRUE, 0 );

	// Set widget properties
	gtk_notebook_set_tab_pos( GTK_NOTEBOOK( Notebook ), GTK_POS_BOTTOM );
	gtk_notebook_set_scrollable( GTK_NOTEBOOK( Notebook ), TRUE );
	gtk_tool_item_set_expand( GTK_TOOL_ITEM( Sep ), TRUE );
	gtk_separator_tool_item_set_draw( GTK_SEPARATOR_TOOL_ITEM( Sep ), FALSE );
	gtk_container_add( GTK_CONTAINER( window ), box );
	gtk_window_set_default_size( GTK_WINDOW( window ), 600, 600 );
	gtk_notebook_set_show_border( GTK_NOTEBOOK( Notebook ), TRUE );

	// Keybindings
	gtk_widget_add_accelerator( GTK_WIDGET( tb_new ), "clicked", Agr, GDK_N, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	gtk_widget_add_accelerator( GTK_WIDGET( tb_open ), "clicked", Agr, GDK_O, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	gtk_widget_add_accelerator( GTK_WIDGET( tb_save ), "clicked", Agr, GDK_S, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	gtk_widget_add_accelerator( GTK_WIDGET( tb_save_as ), "clicked", Agr, GDK_W, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	gtk_widget_add_accelerator( GTK_WIDGET( tb_find ), "clicked", Agr, GDK_F, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	gtk_widget_add_accelerator( GTK_WIDGET( tb_replace ), "clicked", Agr, GDK_R, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	gtk_widget_add_accelerator( GTK_WIDGET( tb_prefs ), "clicked", Agr, GDK_comma, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );
	gtk_widget_add_accelerator( GTK_WIDGET( tb_jump ), "clicked", Agr, GDK_J, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE );

	// Show/hide widgets
	gtk_widget_show_all( window );
	gtk_window_add_accel_group( GTK_WINDOW( window ), Agr );

	if( argc == 1 ) // If no arguments were passed
		gtk_window_set_focus( GTK_WINDOW( window ), New( NULL, Notebook ) );

	else // Check to see if any files were specified on the cmd line
	{	
		gint i;

		for( i = 1; i < argc; i++ )
		{
			if( g_file_test( argv[ i ], G_FILE_TEST_EXISTS ) )
				gtk_window_set_focus( GTK_WINDOW( window ), open_with( argv[ i ], Notebook ) );

			else
			{
				gchar *templates = g_build_filename( g_get_home_dir( ), "/.config/X2/templates/", NULL );
				GFile *path = g_file_new_for_path( templates );

				if( !g_file_test( templates, G_FILE_TEST_EXISTS ) )
					g_file_make_directory( path, NULL, NULL );

				gchar **split = g_strsplit( g_path_get_basename( argv[ i ] ), ".", 2 );
				gchar *template = g_strconcat( g_get_user_config_dir( ), "/X2/templates/template.", split[ 1 ], NULL );
				GFile *source = g_file_new_for_path( template );
				GFile *dest = g_file_new_for_path( argv[ i ] );

				if( g_file_test( template, G_FILE_TEST_EXISTS ) )
					g_file_copy( source, dest, G_FILE_COPY_BACKUP, NULL, ( GFileProgressCallback ) NULL, NULL, NULL );

				else
					g_file_create( dest, G_FILE_CREATE_NONE, NULL, NULL );

				gtk_window_set_focus( GTK_WINDOW( window ), open_with( argv[ i ], Notebook ) );
			}

		}

		if( gtk_notebook_get_n_pages( GTK_NOTEBOOK( Notebook ) ) == 0 )
			gtk_window_set_focus( GTK_WINDOW( window ), New( NULL, Notebook ) );
	}

	gtk_main( );

	return( 0 );
}