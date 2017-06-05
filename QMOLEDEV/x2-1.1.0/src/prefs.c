static void Prefs( GtkWidget *widget, struct prefs_data *pd )
{
	if( prefs_win_open )
		return;
	
	// Lock on!
	prefs_win_open = TRUE;

	// Variables
	struct prefs *pref = pd->pref;

	// Create widgets
	// Boxes
	pref->Prefs_Win = gtk_window_new( GTK_WINDOW_TOPLEVEL );
	pref->Box = gtk_vbox_new( FALSE, 0 );
	pref->General_Box = gtk_vbox_new( FALSE, 0 );
	pref->Appearence_Box = gtk_vbox_new( FALSE, 0 );
	pref->BtnBox = gtk_hbutton_box_new( );
	pref->Programming_Box = gtk_vbox_new( FALSE, 0 );
	pref->UIBox = gtk_vbox_new( FALSE, 0 );
	pref->Search_Box = gtk_vbox_new( FALSE, 0 );
	pref->FindBox = gtk_vbox_new( FALSE, 0 );

	// Frames
	pref->FontFrame = gtk_frame_new( "Font" );
	pref->ProgrammingFrame = gtk_frame_new( "Programming Features" );
	pref->FindFrame = gtk_frame_new( "Search Preferences" );

	// Labels
	pref->GeneralLbl = gtk_label_new( "General" );
	pref->AppearenceLbl = gtk_label_new( "Appearance" );
	pref->LicenceLbl = gtk_label_new( "Licence" );
	pref->SearchLbl = gtk_label_new( "Search" );

	// Check buttons, replace with Toggle buttons
	pref->Programming_Check = gtk_check_button_new_with_label( "Enable programming features" );
	pref->Highlight_Check = gtk_check_button_new_with_label( "Enable Syntax Highlighting" );
	pref->Lines_Check = gtk_check_button_new_with_label( "Enable Line Numbers" );	
	pref->Terminal_Check = gtk_check_button_new_with_label( "Enable Terminal" );
	pref->FindInWords = gtk_check_button_new_with_label( "Search inside words" );
	pref->FindSensitive = gtk_check_button_new_with_label( "Search case sensitive" );
	pref->HighlightLine = gtk_check_button_new_with_label( "Highlight current line" );

	// Buttons
	pref->Apply = gtk_button_new_from_stock( GTK_STOCK_APPLY );
	pref->Cancel = gtk_button_new_from_stock( GTK_STOCK_CANCEL );

	// Text widgets
	pref->Licence_Buffer = gtk_text_buffer_new( NULL );
	pref->Licence = gtk_text_view_new_with_buffer( GTK_TEXT_BUFFER( pref->Licence_Buffer ) );
	pref->Scroller = gtk_scrolled_window_new( NULL, NULL );

	// And the rest
	pref->Font = gtk_font_button_new( );	
	pref->Notes = gtk_notebook_new( );
	
	// Set widget properties
	gtk_window_set_title( GTK_WINDOW( pref->Prefs_Win ), "X2: Preferences" );

	// General page
	gtk_notebook_append_page( GTK_NOTEBOOK( pref->Notes ), pref->General_Box, pref->GeneralLbl );

	// Appearence page
	gtk_notebook_append_page( GTK_NOTEBOOK( pref->Notes ), pref->Appearence_Box, pref->AppearenceLbl );

	// Search page
	gtk_notebook_append_page( GTK_NOTEBOOK( pref->Notes ), pref->Search_Box, pref->SearchLbl );

	// Licence/info page
	gtk_notebook_append_page( GTK_NOTEBOOK( pref->Notes ), pref->Scroller, pref->LicenceLbl );

	
	gtk_button_box_set_layout( GTK_BUTTON_BOX( pref->BtnBox ), GTK_BUTTONBOX_END );
	gtk_font_button_set_title( GTK_FONT_BUTTON( pref->Font ), "Choose font" );

	// Load prefs
	if( g_strrstr( sets->prog_features, ":true" ) )
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->Programming_Check ), TRUE );
	else
		Enable_Programming_Check_Buttons( NULL, pref );

	if( g_strrstr( sets->highlighting, ":true" ) )
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->Highlight_Check ), TRUE );

	if( g_strrstr( sets->lines, ":true" ) )
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->Lines_Check ), TRUE );

	if( g_strrstr( sets->terminal, ":true" ) )
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->Terminal_Check ), TRUE );

	if( g_strrstr( sets->find_in_words, ":true" ) )
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->FindInWords ), TRUE );

	if( g_strrstr( sets->find_sensitive, ":true" ) )
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->FindSensitive ), TRUE );

	if( g_strrstr( sets->highlightline, ":true" ) )
		gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( pref->HighlightLine ), TRUE );


	gtk_font_button_set_font_name( GTK_FONT_BUTTON( pref->Font ), sets->font );

	// Add and pack widgets
	gtk_container_add( GTK_CONTAINER( pref->Prefs_Win ), GTK_WIDGET( pref->Box ) );
	gtk_container_add( GTK_CONTAINER( pref->Scroller ), GTK_WIDGET( pref->Licence ) );
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW( pref->Scroller ), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC );	
	gtk_box_pack_start( GTK_BOX( pref->Box ), pref->Notes, TRUE, TRUE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->Box ), pref->BtnBox, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->BtnBox ), pref->Cancel, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->BtnBox ), pref->Apply, FALSE, FALSE, 0 );

	gtk_box_pack_start( GTK_BOX( pref->General_Box ), pref->ProgrammingFrame, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->Search_Box ), pref->FindFrame, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->Appearence_Box ), pref->FontFrame, FALSE, FALSE, 0 );
	gtk_container_add( GTK_CONTAINER( pref->FontFrame ), GTK_WIDGET( pref->Font ) );
	gtk_container_add( GTK_CONTAINER( pref->ProgrammingFrame ), GTK_WIDGET( pref->Programming_Box ) );
	gtk_container_add( GTK_CONTAINER( pref->FindFrame ), GTK_WIDGET( pref->FindBox ) );
	gtk_box_pack_start( GTK_BOX( pref->Programming_Box ), pref->Programming_Check, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->Programming_Box ), pref->Highlight_Check, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->Programming_Box ), pref->Lines_Check, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->Programming_Box ), pref->Terminal_Check, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->Programming_Box ), pref->HighlightLine, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->FindBox ), pref->FindInWords, FALSE, FALSE, 0 );
	gtk_box_pack_start( GTK_BOX( pref->FindBox ), pref->FindSensitive, FALSE, FALSE, 0 );
	
	// Connect signals
	g_signal_connect( pref->Prefs_Win, "delete-event", G_CALLBACK( close_prefs ), pref );
	g_signal_connect( pref->Apply, "clicked", G_CALLBACK( save_prefs ), pd );
	g_signal_connect( pref->Cancel, "clicked", G_CALLBACK( close_prefs ), pref );
	g_signal_connect( pref->Programming_Check, "toggled", G_CALLBACK( Enable_Programming_Check_Buttons ), pref );

	// Show licence
	GtkTextIter End;
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "Copyright (c) 2011, Rock Computing\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "All rights reserved.\n\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "Redistribution and use in source and binary forms, with or without\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "modification, are permitted provided that the following conditions are met:\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "\n\t* Redistributions of source code must retain the above copyright\n\tnotice, this list of conditions and the following disclaimer.\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "\n\t* Redistributions in binary form must reproduce the above copyright\n\tnotice, this list of conditions and the following disclaimer in the\n\tdocumentation and/or other materials provided with the distribution.\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "\n\t* Neither the name Rock Computing nor the names of its contributors\n\tmay be used to endorse or promote products derived from this\n\tsoftware without specific prior written permission.\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "\n\nTHIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "DISCLAIMED. IN NO EVENT SHALL ROCK COMPUTING BE LIABLE FOR ANY\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n", -1 );
	gtk_text_buffer_get_end_iter( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End );
	gtk_text_buffer_insert( GTK_TEXT_BUFFER( pref->Licence_Buffer ), &End, "SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n", -1 );
	gtk_text_view_set_editable( GTK_TEXT_VIEW( pref->Licence ), FALSE );

	// Show widgets
	gtk_widget_show( pref->Prefs_Win );
	gtk_widget_show_all( pref->Box );
}


static void close_prefs( GtkWidget *widget, struct prefs *pref )
{
	if( gtk_widget_get_visible( GTK_WIDGET( pref->Prefs_Win ) ) )
		gtk_widget_destroy( GTK_WIDGET( pref->Prefs_Win ) );

	// Lock off!
	prefs_win_open = FALSE;
}

static void save_prefs( GtkWidget *widget, struct prefs_data *pd )
{
	struct prefs *pref = pd->pref;
	GError *error = NULL;
	gchar *base = g_build_filename( g_get_home_dir( ), "/.config/X2/", NULL );
	gchar *fn = g_build_filename( base, "X2.conf", NULL );

	// Get preference values
	gchar *programming_pref = "programmingfeatures:false\n";
	gchar *highlighting_pref = "syntaxhighlighting:false\n";	
	gchar *linenumbers_pref = "linenumbers:false\n";
	gchar *terminal_pref = "terminal:false\n";
	gchar *searchwords_pref = "searchinwords:false\n";
	gchar *searchcasesensitive_pref = "casesensitive:false\n";
	gchar *highlightline = "highlightline:false\n";
	const gchar *fontnamesize = gtk_font_button_get_font_name( GTK_FONT_BUTTON( pref->Font ) );

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->Programming_Check ) ) )
		programming_pref = "programmingfeatures:true\n";

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->Highlight_Check ) ) )
		highlighting_pref = "syntaxhighlighting:true\n";

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->Lines_Check ) ) )
		linenumbers_pref = "linenumbers:true\n";

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->Terminal_Check ) ) )
		terminal_pref = "terminal:true\n";

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->FindInWords ) ) )
		searchwords_pref = "searchinwords:true\n";

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->FindSensitive ) ) )
		searchcasesensitive_pref = "casesensitive:true\n";

	if( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( pref->HighlightLine ) ) )
		highlightline = "highlightline:true\n";

	gsize bytes;
	GIOChannel *write = g_io_channel_new_file( fn, "w", &error );
	g_io_channel_write_chars( write, programming_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, highlighting_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, linenumbers_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, terminal_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, searchwords_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, searchcasesensitive_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, highlightline, -1, &bytes, NULL );
	g_io_channel_write_chars( write, fontnamesize, -1, &bytes, NULL );
	g_io_channel_close( write );

	g_free( error );
	g_free( write );
	reload_prefs( pd );
	gtk_widget_destroy( GTK_WIDGET( pref->Prefs_Win ) );

	// Lock off!
	prefs_win_open = FALSE;
}

static void set_default_prefs( )
{
	GError *error = NULL;
	gchar *base = g_build_filename( g_get_home_dir( ), "/.config/X2/", NULL );
	gchar *fn = g_build_filename( base, "X2.conf", NULL );
	gsize bytes;
	gchar *programming_pref = "programmingfeatures:false\n";
	gchar *highlighting_pref = "syntaxhighlighting:false\n";	
	gchar *linenumbers_pref = "linenumbers:false\n";
	gchar *terminal_pref = "terminal:false\n";
	gchar *searchwords_pref = "searchinwords:false\n";
	gchar *searchcasesensitive_pref = "casesensitive:false\n";
	gchar *highlightline = "highlightline:false\n";
	const gchar *fontnamesize = "Sans 10\n";

	GIOChannel *write = g_io_channel_new_file( fn, "w", &error );

	g_io_channel_write_chars( write, programming_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, highlighting_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, linenumbers_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, terminal_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, searchwords_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, searchcasesensitive_pref, -1, &bytes, NULL );
	g_io_channel_write_chars( write, highlightline, -1, &bytes, NULL );
	g_io_channel_write_chars( write, fontnamesize, -1, &bytes, NULL );
	
	g_io_channel_close( write );
	g_free( write );
	/*g_free( error );*/
}

static void reload_prefs( struct prefs_data *pd )
{
	// Load prefs
	load_prefs( );
	// Iterate through each open doc and apply the new preference
	gint curr_page = gtk_notebook_get_current_page( GTK_NOTEBOOK( pd->Notebook ) );
	gint i;
		
	// Deal with terminal now, since it would just waste cpu cycles dealing with it in a for loop
	if( g_strrstr( sets->terminal, ":false" ) )
		gtk_widget_hide( GTK_WIDGET( pd->tb_cmd ) );

	else
		gtk_widget_show( GTK_WIDGET( pd->tb_cmd ) );

	// Apply settings to each document
	for( i = 0; i < gtk_notebook_get_n_pages( GTK_NOTEBOOK( pd->Notebook ) ); i++ )
	{
		// Switch to the next page, first page if first iteration of loop
		gtk_notebook_set_current_page( GTK_NOTEBOOK( pd->Notebook ), i );
		struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( pd->Notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( pd->Notebook ) ) ) ), "document" );

		if( g_strrstr( sets->highlighting, ":true" ) )
		{
			GtkSourceLanguage *lang;
			GtkSourceLanguageManager *lm = gtk_source_language_manager_new( );
			lang = gtk_source_language_manager_guess_language( lm, tmp->Name, NULL );
			gtk_source_buffer_set_language( tmp->Buffer, lang );
		}

		else
			gtk_source_buffer_set_language( tmp->Buffer, NULL );


		if( g_strrstr( sets->highlightline, ":true" ) )
			gtk_source_view_set_highlight_current_line( GTK_SOURCE_VIEW( tmp->Doc ), TRUE );

		else
			gtk_source_view_set_highlight_current_line( GTK_SOURCE_VIEW( tmp->Doc ), FALSE );
	
		if( g_strrstr( sets->lines, ":true" ) )
			gtk_source_view_set_show_line_numbers( GTK_SOURCE_VIEW( tmp->Doc ), TRUE );

		else
			gtk_source_view_set_show_line_numbers( GTK_SOURCE_VIEW( tmp->Doc ), FALSE );
	
		// Font prefs
		PangoFontDescription *PFD = pango_font_description_from_string( sets->font );
		gtk_widget_modify_font( tmp->Doc, PFD );
	}

	gtk_notebook_set_current_page( GTK_NOTEBOOK( pd->Notebook ), curr_page );
}

struct settings *load_prefs( )
{
	gchar *base = g_build_filename( g_get_home_dir( ), "/.config/X2/", NULL );
	gchar *fn = g_build_filename( base, "X2.conf", NULL );

	if( !g_file_test( fn, G_FILE_TEST_EXISTS ) )
		g_file_make_directory_with_parents( g_file_new_for_path( base ), FALSE, NULL );

	if( !g_file_test( fn, G_FILE_TEST_EXISTS ) )
		set_default_prefs( );

	gchar *line;
	GIOChannel *read;
	read = g_io_channel_new_file( fn, "r", NULL );

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->prog_features = line;

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->highlighting = line;

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->lines = line;

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->terminal = line;

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->find_in_words = line;

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->find_sensitive = line;

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->highlightline = line;

	g_io_channel_read_line( read, &line, NULL, NULL, NULL );
	sets->font = line;

	g_io_channel_close( read );
	g_free( read );

	return( sets );
}
