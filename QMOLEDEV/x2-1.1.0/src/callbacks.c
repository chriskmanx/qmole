static GtkWidget *New( GtkWidget *widget, GtkWidget *notebook )
{
	// Create node instance
	struct node *doc = g_malloc( sizeof( struct node ) );
	doc->Box = gtk_vbox_new( FALSE, 0 );
	doc->Name = "Untitled";
	doc->Location = "None";
	doc->Scroller = gtk_scrolled_window_new( NULL, NULL );
	#ifdef G_OS_UNIX
	doc->Buffer = gtk_source_buffer_new( NULL );
	doc->Doc = gtk_source_view_new_with_buffer( GTK_SOURCE_BUFFER( doc->Buffer ) );
	doc->Term = vte_terminal_new( );
	doc->TermScroll = gtk_scrolled_window_new( NULL, NULL );
	#endif	
	#ifndef G_OS_UNIX
	doc->Buffer = gtk_text_buffer_new( NULL );
	doc->Doc = gtk_text_view_new_with_buffer( GTK_TEXT_BUFFER( doc->Buffer ) );
	#endif
	doc->Label = gtk_label_new( doc->Name );
	doc->TabBox = gtk_hbox_new( FALSE, 0 );
	doc->Image = gtk_image_new_from_stock( GTK_STOCK_CLOSE, GTK_ICON_SIZE_SMALL_TOOLBAR );
	doc->Button = gtk_button_new( );
	doc->SearchBox = gtk_hbox_new( FALSE, 0 );
	doc->Search1 = gtk_entry_new( );
	doc->ReplaceBox = gtk_vbox_new( FALSE, 0 );
	doc->Search2 = gtk_entry_new( );
	doc->Replace2 = gtk_entry_new( );
	doc->Searchlbl1 = gtk_label_new( "Search: " );
	doc->Searchlbl2 = gtk_label_new( "Search: " );
	doc->Replacelbl = gtk_label_new( "Replace: " );
	doc->Paned = gtk_vpaned_new( );
	doc->Frame = gtk_frame_new( NULL );
	doc->JumpLbl = gtk_label_new( "Jump to line: " );
	doc->JumpSpin = gtk_spin_button_new_with_range( 1, 1, 1 );
	doc->JumpBox = gtk_hbox_new( FALSE, 0 );
	doc->templatebox = gtk_hbox_new( FALSE, 0 );
	doc->templatecmb = gtk_combo_box_text_new( );
	doc->templatebtn_img = gtk_image_new_from_stock( GTK_STOCK_APPLY, GTK_ICON_SIZE_SMALL_TOOLBAR );
	doc->templatebtn = gtk_button_new( );
	doc->templatelbl = gtk_label_new( "Select a template: " );

	gtk_button_set_image( GTK_BUTTON( doc->templatebtn ), doc->templatebtn_img );
	gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", doc->Name, NULL ) );

	// Pango-font stuff
	PangoFontDescription *PFD = pango_font_description_from_string( sets->font );
	gtk_widget_modify_font( doc->Doc, PFD );

	// Create stuff instance
	struct str_data *tmp = g_malloc( sizeof( struct str_data ) );
	tmp->Widget = GTK_WIDGET( doc->Paned );
	tmp->Notebook = notebook;
	
	// Entry widgets icons
	gtk_entry_set_icon_from_stock( GTK_ENTRY( doc->Search1 ), GTK_ENTRY_ICON_SECONDARY, GTK_STOCK_FIND );
	gtk_entry_set_icon_from_stock( GTK_ENTRY( doc->Search2 ), GTK_ENTRY_ICON_SECONDARY, GTK_STOCK_FIND );
	gtk_entry_set_icon_from_stock( GTK_ENTRY( doc->Replace2 ), GTK_ENTRY_ICON_SECONDARY, GTK_STOCK_FIND_AND_REPLACE );
	gtk_entry_set_icon_from_stock( GTK_ENTRY( doc->JumpSpin ), GTK_ENTRY_ICON_SECONDARY, GTK_STOCK_JUMP_TO );
	gtk_entry_set_width_chars( GTK_ENTRY( doc->JumpSpin ), 5 );
	
	// Set button properties
	gtk_button_set_image( GTK_BUTTON( doc->Button ), doc->Image );
	gtk_widget_set_tooltip_text( GTK_WIDGET( doc->Button ), "Close document" );

	// Extra boxes
	GtkWidget *hb1 = gtk_hbox_new( FALSE, 0 );
	GtkWidget *hb2 = gtk_hbox_new( FALSE, 0 );

	// Add/pack widgets
	gtk_container_add( GTK_CONTAINER( doc->Scroller ), doc->Doc );
	gtk_container_add( GTK_CONTAINER( doc->Frame ), doc->Scroller );
	
	gtk_box_pack_end( GTK_BOX( doc->SearchBox ), doc->Search1, FALSE, FALSE, 0 );
	gtk_box_pack_end( GTK_BOX( doc->SearchBox ), doc->Searchlbl1, FALSE, FALSE, 0 );
	
	gtk_box_pack_end( GTK_BOX( hb2 ), doc->Replace2, FALSE, FALSE, 0 );
	gtk_box_pack_end( GTK_BOX( hb2 ), doc->Replacelbl, FALSE, FALSE, 0 );
	gtk_box_pack_end( GTK_BOX( hb1 ), doc->Search2, FALSE, FALSE, 0 );
	gtk_box_pack_end( GTK_BOX( hb1 ), doc->Searchlbl2, FALSE, FALSE, 0 );	
	
	gtk_box_pack_start( GTK_BOX( doc->ReplaceBox ), hb1, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( doc->ReplaceBox ), hb2, FALSE, FALSE, 0 );

	gtk_box_pack_start( GTK_BOX( doc->TabBox ), doc->Label, FALSE, TRUE, 0 );
	gtk_box_pack_start( GTK_BOX( doc->TabBox ), doc->Button, FALSE, TRUE, 0 );
	
	gtk_box_pack_end( GTK_BOX( doc->JumpBox ), doc->JumpSpin, FALSE, FALSE, 0 );
	gtk_box_pack_end( GTK_BOX( doc->JumpBox ), doc->JumpLbl, FALSE, FALSE, 0 );

	gtk_box_pack_end( GTK_BOX( doc->templatebox ), doc->templatebtn, FALSE, FALSE, 0 );
	gtk_box_pack_end( GTK_BOX( doc->templatebox ), doc->templatecmb, FALSE, FALSE, 0 );
	gtk_box_pack_end( GTK_BOX( doc->templatebox ), doc->templatelbl, FALSE, FALSE, 0 );
	
	gtk_box_pack_start( GTK_BOX( doc->Box ), doc->Frame, TRUE, TRUE, 0 );
	gtk_box_pack_start( GTK_BOX( doc->Box ), doc->SearchBox, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( doc->Box ), doc->ReplaceBox, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( doc->Box ), doc->JumpBox, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( doc->Box ), doc->templatebox, FALSE, FALSE, 0 );

	// Set properties
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW( doc->Scroller ), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC );
	gtk_widget_queue_draw( GTK_WIDGET( notebook ) );
	gtk_notebook_append_page( GTK_NOTEBOOK( notebook ), doc->Paned, doc->TabBox );
	gtk_scrolled_window_add_with_viewport( GTK_SCROLLED_WINDOW( doc->TermScroll ), doc->Term );
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW( doc->TermScroll ), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC );	
	gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( doc->Buffer ), FALSE );
	gtk_drag_dest_set( doc->Doc, GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_HIGHLIGHT, targets, n_targets, GDK_ACTION_COPY );

	// Experimental sourceview stuff
	if( g_strrstr( sets->highlightline, ":true" ) )
		gtk_source_view_set_highlight_current_line( GTK_SOURCE_VIEW( doc->Doc ), TRUE );
	
	// Sourceview prefs
	if( g_strrstr( sets->lines, ":true" ) )
		gtk_source_view_set_show_line_numbers( GTK_SOURCE_VIEW( doc->Doc ), TRUE );

	// Paned stuff
	gtk_paned_add1( GTK_PANED( doc->Paned ), doc->Box );
	
	// Vte stuff
	#ifdef G_OS_UNIX
	vte_terminal_set_background_transparent( VTE_TERMINAL( doc->Term ), TRUE );
	vte_terminal_set_size( VTE_TERMINAL( doc->Term ), 79, 2 );
	vte_terminal_set_scroll_on_output( VTE_TERMINAL( doc->Term ), TRUE );
	vte_terminal_set_scroll_on_keystroke( VTE_TERMINAL( doc->Term ), TRUE );
	gtk_paned_add2( GTK_PANED( doc->Paned ), doc->TermScroll );
	long size = pathconf( ".", _PC_PATH_MAX );
	char *buf, *ptr;

        if ( ( buf = ( char * ) g_malloc( ( size_t ) size ) ) != NULL ) 
		ptr = getcwd( buf, ( size_t ) size );

        vte_terminal_fork_command( VTE_TERMINAL( doc->Term ), NULL, NULL, NULL, ptr, TRUE, TRUE, TRUE );
	//vte_terminal_fork_command_full( VTE_TERMINAL( doc->Term ), VTE_PTY_DEFAULT, ptr, NULL, NULL, NULL, NULL, NULL, NULL, NULL );
	#endif
	
	gtk_paned_set_position( GTK_PANED( doc->Paned ), 450 );
	gtk_notebook_set_tab_reorderable( GTK_NOTEBOOK( notebook ), doc->Paned, TRUE );
	
	struct search_data *find1 = g_malloc( sizeof( struct search_data ) );
	find1->tmp = doc;
	find1->textmark = "search1";
	find1->Notebook = notebook;
	find1->Search = doc->Search1;

	struct search_data *find2 = g_malloc( sizeof( struct search_data ) );
	find2->tmp = doc;
	find2->textmark = "search2";
	find2->Notebook = notebook;
	find2->Search = doc->Search2;
	
	// Entry icon signals
	g_signal_connect( doc->Search1, "icon-press", G_CALLBACK( S1 ), find1 );
	g_signal_connect( doc->Search1, "activate", G_CALLBACK( S2 ), find1 );
	g_signal_connect( doc->Search2, "icon-press", G_CALLBACK( S1 ), find2 );
	g_signal_connect( doc->Search2, "activate", G_CALLBACK( S2 ), find2 );
	g_signal_connect( doc->Replace2, "icon-press", G_CALLBACK( C1 ), find2 );
	g_signal_connect( doc->Replace2, "activate", G_CALLBACK( C2 ), find2 );
	g_signal_connect( doc->JumpSpin, "icon-press", G_CALLBACK( G1 ), doc );
	g_signal_connect( doc->JumpSpin, "activate", G_CALLBACK( G2 ), doc );
	g_signal_connect( doc->Button, "clicked", G_CALLBACK( Close ), tmp );
	g_signal_connect( doc->Buffer, "changed", G_CALLBACK( BufferChanged ), doc );
	g_signal_connect( doc->Doc, "drag-data-received", G_CALLBACK( drag_data_received_handler ), notebook );
	g_signal_connect( doc->templatebtn, "clicked", G_CALLBACK( LoadTemplate ), notebook );

	// Show/hide widgets
	gtk_widget_show_all( doc->Paned );
	gtk_widget_show_all( doc->TabBox );
	gtk_widget_hide( doc->SearchBox );
	gtk_widget_hide( doc->ReplaceBox );
	#ifdef G_OS_UNIX
	gtk_widget_hide( doc->TermScroll );
	#endif
	gtk_widget_hide( doc->JumpBox );
	gtk_widget_hide( doc->templatebox );

	// Set new page
	gtk_notebook_set_current_page( GTK_NOTEBOOK( notebook ), -1 );
	g_object_set_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document", doc );

	return( doc->Doc );
}

static void Open( GtkWidget *widget, GtkWidget *notebook )
{
	GtkWidget *OpenDialog = gtk_file_chooser_dialog_new( "Open file",
		   NULL, 
		   GTK_FILE_CHOOSER_ACTION_OPEN, 
		   GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		   GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		   NULL );

	// Add file filter support so X2 can't open video files etc 
	const gchar *f1 = "text/plain";
	GtkFileFilter *filter = gtk_file_filter_new( );
	gtk_file_filter_set_name( GTK_FILE_FILTER( filter ), "Text files" );
	gtk_file_filter_add_mime_type( GTK_FILE_FILTER( filter ), f1 );
	gtk_file_chooser_add_filter( GTK_FILE_CHOOSER( OpenDialog ), filter );

	const gchar *home_dir = g_get_home_dir( );
	gtk_file_chooser_set_current_folder( GTK_FILE_CHOOSER( OpenDialog ), home_dir );

	if( gtk_dialog_run( GTK_DIALOG( OpenDialog ) ) == GTK_RESPONSE_ACCEPT )
	{
		struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

		if( gtk_text_buffer_get_modified( GTK_TEXT_BUFFER( tmp->Buffer ) ) )
		{
			New( NULL, notebook ); 
			tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
		}	
		
		gchar *contents;

		tmp->Name = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( OpenDialog ) );
		g_file_get_contents( tmp->Name, &contents, NULL, NULL );
		gtk_text_buffer_set_text( GTK_TEXT_BUFFER( tmp->Buffer ), contents, -1 );
		gtk_label_set_text( GTK_LABEL( tmp->Label ), g_strconcat( g_path_get_basename( tmp->Name ), " ", NULL ) );
		gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), FALSE );
		g_free( contents );
		
		highlight_code( tmp );

		if( tmp )	
			gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", tmp->Name, NULL ) );
	}
	gtk_widget_destroy( OpenDialog );
}

static void SaveAs( GtkWidget *widget, GtkWidget *notebook )
{
	GtkWidget *SaveAsDialog = gtk_file_chooser_dialog_new( "Save file as...",
		   NULL, 
		   GTK_FILE_CHOOSER_ACTION_SAVE, 
		   GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		   GTK_STOCK_SAVE_AS, GTK_RESPONSE_ACCEPT,
		   NULL );

	const gchar *home_dir = g_get_home_dir( );
	gtk_file_chooser_set_current_folder( GTK_FILE_CHOOSER( SaveAsDialog ), home_dir );

	if( gtk_dialog_run( GTK_DIALOG( SaveAsDialog ) ) == GTK_RESPONSE_ACCEPT )
	{
		struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
		tmp->Name = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( SaveAsDialog ) );

		write_file( tmp );
		gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), FALSE );
		gtk_label_set_text( GTK_LABEL( tmp->Label ), g_path_get_basename( tmp->Name ) );

		highlight_code( tmp );

		if( tmp )	
			gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", tmp->Name, NULL ) );
	}

	gtk_widget_destroy( SaveAsDialog );
}

static void Save( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	gchar *name = "Untitled";

	if( tmp->Name == name )
		SaveAs( widget, notebook );
	
	else
		write_file( tmp );

	gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), FALSE );
	gtk_label_set_text( GTK_LABEL( tmp->Label ), g_path_get_basename( tmp->Name ) );
	
	highlight_code( tmp );

	if( tmp )	
		gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", tmp->Name, NULL ) );
}

static void Undo( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( gtk_source_buffer_can_undo( GTK_SOURCE_BUFFER( tmp->Buffer ) ) )
		gtk_source_buffer_undo( GTK_SOURCE_BUFFER( tmp->Buffer ) );
}

static void Redo( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( gtk_source_buffer_can_redo( GTK_SOURCE_BUFFER( tmp->Buffer ) ) )
		gtk_source_buffer_redo( GTK_SOURCE_BUFFER( tmp->Buffer ) );
}

static void Cut( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( gtk_text_buffer_get_has_selection( GTK_TEXT_BUFFER( tmp->Buffer ) ) )
	{
		GtkClipboard *clipboard = gtk_clipboard_get( GDK_SELECTION_CLIPBOARD );
		gtk_text_buffer_cut_clipboard( GTK_TEXT_BUFFER( tmp->Buffer ), clipboard, TRUE );
	}
}

static void Copy( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( gtk_text_buffer_get_has_selection( GTK_TEXT_BUFFER( tmp->Buffer ) ) )
	{
		GtkClipboard *clipboard = gtk_clipboard_get( GDK_SELECTION_CLIPBOARD );
		gtk_text_buffer_copy_clipboard( GTK_TEXT_BUFFER( tmp->Buffer ), clipboard );
	}
}

static void Paste( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	GtkClipboard *clipboard = gtk_clipboard_get( GDK_SELECTION_CLIPBOARD );
	gtk_text_buffer_paste_clipboard( GTK_TEXT_BUFFER( tmp->Buffer ), clipboard, NULL, TRUE );
}

static void Find( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	
	if( gtk_widget_get_visible( tmp->SearchBox ) )
		gtk_widget_hide( tmp->SearchBox );
	else
	{
		gtk_widget_show_all( tmp->SearchBox );
		gtk_widget_hide( tmp->ReplaceBox );
		gtk_widget_hide( tmp->JumpBox );
		gtk_widget_hide( tmp->templatebox );
		gtk_widget_grab_focus( tmp->Search1 );
	}
}

static void Replace( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	
	if( gtk_widget_get_visible( tmp->ReplaceBox ) )
		gtk_widget_hide( tmp->ReplaceBox );
	else
	{
		gtk_widget_show_all( tmp->ReplaceBox );
		gtk_widget_hide( tmp->SearchBox );
		gtk_widget_hide( tmp->JumpBox );
		gtk_widget_hide( tmp->templatebox );
		gtk_widget_grab_focus( tmp->Search2 );
	}
}

static void Jump( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( gtk_widget_get_visible( tmp->JumpBox ) )
		gtk_widget_hide( tmp->JumpBox );
	else
	{
		gtk_widget_show_all( tmp->JumpBox );
		gtk_widget_hide( tmp->ReplaceBox );
		gtk_widget_hide( tmp->SearchBox );
		gtk_widget_hide( tmp->templatebox );
		gtk_widget_grab_focus( tmp->JumpSpin );
	}
}

static void Cmd( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	
	if( gtk_widget_get_visible( tmp->TermScroll ) )
		gtk_widget_hide( tmp->TermScroll );
	else
		gtk_widget_show_all( tmp->TermScroll );
}

static void Enable_Programming_Check_Buttons( GtkWidget *widget, struct prefs *pref )
{
	if( !gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->Programming_Check ) ) )
	{
		gtk_widget_set_sensitive( pref->Highlight_Check, FALSE );
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->Highlight_Check ), FALSE );
		gtk_widget_set_sensitive( pref->Lines_Check, FALSE );
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->Lines_Check ), FALSE );
		gtk_widget_set_sensitive( pref->Terminal_Check, FALSE );
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->Terminal_Check ), FALSE );
		gtk_widget_set_sensitive( pref->HighlightLine, FALSE );
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->HighlightLine ), FALSE );
	}

	else
	{
		gtk_widget_set_sensitive( pref->Highlight_Check, TRUE );
		gtk_widget_set_sensitive( pref->Lines_Check, TRUE );
		gtk_widget_set_sensitive( pref->Terminal_Check, TRUE );
		gtk_widget_set_sensitive( pref->HighlightLine, TRUE );
	}
} 

static void S1( GtkEntry *entry, GtkEntryIconPosition icon_pos, GdkEvent *event, struct search_data *sd ) { Search( sd ); }
static void S2( GtkEntry *entry, struct search_data *sd ) { Search( sd ); }

static void Search( struct search_data *sd )
{
	struct node *doc = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( sd->Notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( sd->Notebook ) ) ) ), "document" );
	const gchar *search = gtk_entry_get_text( GTK_ENTRY( sd->Search ) );

	GtkTextIter mstart, mend, iter, start, end;
  	GtkTextBuffer *buffer = gtk_text_view_get_buffer( GTK_TEXT_VIEW( doc->Doc ) );

	GtkSourceSearchFlags search_flags = GTK_SOURCE_SEARCH_VISIBLE_ONLY | GTK_SOURCE_SEARCH_TEXT_ONLY;

	if( !g_strrstr( sets->find_sensitive, ":true" ) )
		search_flags = search_flags | GTK_SOURCE_SEARCH_CASE_INSENSITIVE;
	
	GtkTextMark *last_pos = gtk_text_buffer_get_mark( buffer, sd->textmark );
	gtk_text_buffer_get_bounds( buffer, &start, &end );
	
	if( !last_pos )
	{
		gtk_text_buffer_create_mark( buffer, sd->textmark, &start, FALSE );
  		last_pos = gtk_text_buffer_get_mark( buffer, sd->textmark );
	}

  	gtk_text_buffer_get_iter_at_mark( buffer, &iter, last_pos );

	if( !g_strrstr( sets->find_in_words, ":true" ) )
	{
		gchar *stmp = g_strconcat( " ", search, " ", NULL );
		search = stmp;
	}

	if( gtk_source_iter_forward_search( &iter, search, search_flags, &mstart, &mend, NULL ) )
	{
		gtk_text_buffer_create_mark( buffer, sd->textmark, &mend, FALSE );
		gtk_text_view_scroll_mark_onscreen( GTK_TEXT_VIEW( doc->Doc ), last_pos );
		gtk_text_buffer_select_range( buffer, &mstart, &mend );
	}

	else
		gtk_text_buffer_delete_mark( GTK_TEXT_BUFFER( buffer ), last_pos ); 
}

static void C1( GtkEntry *entry, GtkEntryIconPosition icon_pos, GdkEvent *event, struct search_data *sd ) { Change( sd ); }
static void C2( GtkEntry *entry, struct search_data *sd ) { Change( sd ); }

static void Change( struct search_data *tmp )
{
	struct node *doc = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( tmp->Notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( tmp->Notebook ) ) ) ), "document" );
	const gchar *replace_text = gtk_entry_get_text( GTK_ENTRY( doc->Replace2 ) );
	GtkTextIter start, end, iter;
	GtkTextMark *last_pos = gtk_text_buffer_get_mark( GTK_TEXT_BUFFER( doc->Buffer ), tmp->textmark );
	
	if( last_pos )
	{
		gtk_text_buffer_get_selection_bounds( GTK_TEXT_BUFFER( doc->Buffer ), &start, &end );
		gtk_text_buffer_create_mark( GTK_TEXT_BUFFER( doc->Buffer ), "paste", &start, FALSE );
		gtk_text_buffer_delete( GTK_TEXT_BUFFER( doc->Buffer ), &start, &end );
		gtk_text_buffer_get_iter_at_mark( GTK_TEXT_BUFFER( doc->Buffer ), &iter, last_pos );
		gtk_text_buffer_insert( GTK_TEXT_BUFFER( doc->Buffer ), &iter, replace_text, -1 );
	}
}

static void G1( GtkEntry *entry, GtkEntryIconPosition icon_pos, GdkEvent *event, struct node *tmp ) { go_to_line( tmp ); }
static void G2( GtkEntry *entry, struct node *tmp ) { go_to_line( tmp ); }

static void go_to_line( struct node *tmp )
{
	gint line = atoi( gtk_entry_get_text( GTK_ENTRY( tmp->JumpSpin ) ) );
		
	if( line )
	{
		GtkTextIter iter;
		gtk_text_buffer_get_iter_at_line( GTK_TEXT_BUFFER( tmp->Buffer ), &iter, line -1 );
		gtk_text_view_scroll_to_iter( GTK_TEXT_VIEW( tmp->Doc ), &iter, 0.0, TRUE, 0, 0 );
		gtk_text_buffer_place_cursor( GTK_TEXT_BUFFER( tmp->Buffer ), &iter );
	}
}

static void Close( GtkWidget *widget, struct str_data *tmp )
{	
	gint page_num = gtk_notebook_page_num( GTK_NOTEBOOK( tmp->Notebook ), tmp->Widget );
	struct node *doc = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( tmp->Notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( tmp->Notebook ) ) ) ), "document" );
	
	if( gtk_text_buffer_get_modified( GTK_TEXT_BUFFER( doc->Buffer ) ) )
	{
		gtk_notebook_set_current_page( GTK_NOTEBOOK( tmp->Notebook ), page_num );
		GtkWidget *warning = gtk_dialog_new_with_buttons(
		"Save file?",
                 NULL,
                 GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                 GTK_STOCK_YES,
                 GTK_RESPONSE_YES,
                 GTK_STOCK_NO,
                 GTK_RESPONSE_NO,
                 NULL );

		gint result = gtk_dialog_run( GTK_DIALOG( warning ) );
			
		if( result == GTK_RESPONSE_YES )
			Save( NULL, tmp->Notebook );

		gtk_widget_destroy( warning );
	}
	
	gtk_notebook_remove_page( GTK_NOTEBOOK( tmp->Notebook ), page_num );

	if( gtk_notebook_get_n_pages( GTK_NOTEBOOK( tmp->Notebook ) ) == 0 )
		gtk_main_quit( );

	g_free( tmp );
}

static void BufferChanged( GtkWidget *widget, struct node *tmp )
{
	if( !gtk_text_buffer_get_modified( GTK_TEXT_BUFFER( tmp->Buffer ) ) )
	{
		const gchar *text = gtk_label_get_text( GTK_LABEL( tmp->Label ) );
		const gchar *new_text = g_strconcat( text, "* ", NULL );
		gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), TRUE );
		gtk_label_set_text( GTK_LABEL( tmp->Label ), new_text );
	}

	// Create the spin button adjustment, then get the number of lines followed be re-setting the upper limit
	GtkAdjustment *adjust = gtk_spin_button_get_adjustment( GTK_SPIN_BUTTON( tmp->JumpSpin ) );
	gint lines = gtk_text_buffer_get_line_count( GTK_TEXT_BUFFER( tmp->Buffer ) );
	gtk_adjustment_set_upper( adjust, lines );
}

static gboolean destroy( GtkWidget *widget, GdkEvent *event, GtkWidget *notebook ) 
{ 
	gint pages = gtk_notebook_get_n_pages( GTK_NOTEBOOK( notebook ) ) -1;
	gint i = 0;
	GtkWidget *label = gtk_label_new( "There are unsaved files.\nAre you sure you want to quit?" );
	gboolean saved = TRUE;

	gtk_notebook_set_current_page( GTK_NOTEBOOK( notebook ), i );
	GtkWidget *warning = gtk_dialog_new_with_buttons(
	"Really quit?",
        NULL,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_STOCK_YES, GTK_RESPONSE_YES,
        GTK_STOCK_NO, GTK_RESPONSE_NO,
        NULL );
	gtk_container_add( GTK_CONTAINER( GTK_DIALOG( warning )->vbox), label );
	gtk_widget_show( GTK_WIDGET( label ) );
	gtk_button_box_set_layout( GTK_DIALOG( warning )->action_area, GTK_BUTTONBOX_EDGE );

	for( i = 0; i <= pages; i++ )
	{
		struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

		if( gtk_text_buffer_get_modified( GTK_TEXT_BUFFER( tmp->Buffer ) ) )
			saved = FALSE;

		gtk_notebook_set_current_page( GTK_NOTEBOOK( notebook ), i+1 );
	}

	if( !saved )
	{
		gint result = gtk_dialog_run( GTK_DIALOG( warning ) );
			
		switch( result )
		{
			case GTK_RESPONSE_YES:
				gtk_widget_destroy( warning );
				gtk_main_quit( );
				return( FALSE );

			case GTK_RESPONSE_NO:
				gtk_widget_destroy( warning );
				return( TRUE );
		}

	}

	gtk_widget_destroy( warning );
	gtk_main_quit( );
	return( FALSE );
}

static void drag_data_received_handler( GtkWidget *widget, GdkDragContext *context, gint x, gint y, GtkSelectionData *selection_data, guint target_type, guint time, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	if( gtk_text_buffer_get_char_count( GTK_TEXT_BUFFER( tmp->Buffer ) ) )
	{
		New( NULL, notebook ); 
		tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	}

	gchar *file = selection_data->data;
	g_strchomp( file );
	gchar **s = g_strsplit( file, "file://", 2 );
	gchar *t = s[ 1 ];
	gchar **u = g_strsplit( t, "%20", 0 );
	gchar *fn = "";

	if( g_strrstr( s[ 1 ], "%20" ) )
	{
		gint i = 0;
		for( ; ; )
		{
			if( u[ i ] )
				fn = g_strconcat( fn, " ", u[ i ], NULL );
			else
				break;
			i++;
		}

		g_strstrip( fn );
		tmp->Name = fn;		
	}

	else
		tmp->Name = s[ 1 ];
	
	gchar *contents;

	g_file_get_contents( tmp->Name, &contents, NULL, NULL );
	gtk_text_buffer_set_text( GTK_TEXT_BUFFER( tmp->Buffer ), contents, -1 );
	gtk_label_set_text( GTK_LABEL( tmp->Label ), g_strconcat( g_path_get_basename( tmp->Name ), " ", NULL ) );
	gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), FALSE );

	if( tmp )	
		gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", tmp->Name, NULL ) );

	// Syntax highlighting
	highlight_code( tmp );
	g_free( contents );
}

static void write_file( struct node *tmp )
{
	GtkTextIter StartIter, EndIter;
	gtk_text_buffer_get_bounds( GTK_TEXT_BUFFER( tmp->Buffer ), &StartIter, &EndIter );

	int r = access( tmp->Name, R_OK );
	int w = access( tmp->Name, W_OK );
	int x = access( tmp->Name, X_OK );

	g_file_set_contents( tmp->Name, gtk_text_buffer_get_text( GTK_TEXT_BUFFER( tmp->Buffer ), &StartIter, &EndIter, TRUE ), -1, NULL );

	if( r == 0 && w == 0 && x == 0 )
		chmod( tmp->Name, S_IRWXU );	

}

static void Set_Title( GtkWidget *notebook, gpointer page, guint page_num, gpointer user_data )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( tmp )	
		gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", tmp->Name, NULL ) );
}

static GtkWidget *open_with( gchar *fn, GtkWidget *notebook )
{
	New( NULL, notebook );
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	tmp->Name = fn;	
	gchar *contents;

	g_file_get_contents( tmp->Name, &contents, NULL, NULL );
	gtk_text_buffer_set_text( GTK_TEXT_BUFFER( tmp->Buffer ), contents, -1 );
	gtk_label_set_text( GTK_LABEL( tmp->Label ), g_strconcat( g_path_get_basename( tmp->Name ), " ", NULL ) );
	gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), FALSE );

	gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", tmp->Name, NULL ) );

	// Syntax highlighting
	highlight_code( tmp );
	g_free( contents );

	return( tmp->Doc );
}

static GtkWidget *open_from( gchar *fn, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( gtk_text_buffer_get_modified( GTK_TEXT_BUFFER( tmp->Buffer ) ) )
	{
		New( NULL, notebook ); 
		tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	}	

	gchar *contents;

	g_file_get_contents( fn, &contents, NULL, NULL );
	gtk_text_buffer_set_text( GTK_TEXT_BUFFER( tmp->Buffer ), contents, -1 );
	gtk_label_set_text( GTK_LABEL( tmp->Label ), g_strconcat( g_path_get_basename( tmp->Name ), "* ", NULL ) );
	gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), FALSE );
	gtk_window_set_title( GTK_WINDOW( window ), g_strconcat( "X2: ", tmp->Name, NULL ) );

	// Syntax highlighting
	highlight_code( tmp );
	g_free( contents );
	gtk_text_buffer_set_modified( GTK_TEXT_BUFFER( tmp->Buffer ), TRUE );

	return( tmp->Doc );
}

static void highlight_code( struct node *tmp )
{
	if( g_strrstr( sets->highlighting, ":true" ) )
	{
		GtkSourceLanguage *lang;
		GtkSourceLanguageManager *lm = gtk_source_language_manager_new( );
		lang = gtk_source_language_manager_guess_language( lm, tmp->Name, NULL );
		gtk_source_buffer_set_language( tmp->Buffer, lang );
	}
}

static void Catch_Keypress( GtkWidget *widget, GdkEvent *event, GtkWidget *notebook )
{
	GdkEventKey *gek = ( GdkEventKey * ) event;

	if( ( gek->keyval == GDK_N ) && ( gek->state & GDK_CONTROL_MASK ) )
		Template( widget, notebook );

	else if( ( gek->keyval == GDK_S ) && ( gek->state & GDK_CONTROL_MASK ) )
		SaveAs( widget, notebook );

	else if( ( gek->keyval == GDK_W ) && ( gek->state & GDK_CONTROL_MASK ) )
		SaveTemplate( widget, notebook );
}