static void Template( GtkWidget *widget, GtkWidget *notebook )
{
	gchar *templates = g_build_filename( g_get_home_dir( ), "/.config/X2/templates/", NULL );
	GDir *d = g_dir_open( templates, 0, NULL );
	const gchar *dir = g_dir_read_name( d );	

	if( dir == NULL )
		return;

	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	Populate_CMB( tmp->templatecmb );

	if( gtk_widget_get_visible( tmp->templatebox ) )
		gtk_widget_hide( tmp->templatebox );
	else
	{
		gtk_widget_show_all( tmp->templatebox );
		gtk_widget_hide( tmp->JumpBox );
		gtk_widget_hide( tmp->ReplaceBox );
		gtk_widget_hide( tmp->SearchBox );
		gtk_widget_grab_focus( tmp->templatecmb );
		gtk_combo_box_set_active( GTK_COMBO_BOX( tmp->templatecmb ), 0 );
	}
}

static void LoadTemplate( GtkWidget *widget, GtkWidget *notebook )
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );
	gchar *template = g_build_filename( g_get_home_dir( ), "/.config/X2/templates/", gtk_combo_box_text_get_active_text( GTK_COMBO_BOX_TEXT( tmp->templatecmb ) ), NULL );
	open_from( template, notebook );
	gtk_widget_hide( tmp->templatebox );
}

static void SaveTemplate( GtkWidget *widget, GtkWidget *notebook )	
{
	struct node *tmp = g_object_get_data( G_OBJECT( gtk_notebook_get_nth_page( GTK_NOTEBOOK( notebook ), gtk_notebook_get_current_page( GTK_NOTEBOOK( notebook ) ) ) ), "document" );

	if( !g_strrstr( g_path_get_basename( tmp->Name ), "." ) )
		return;

	// Split file name up
	gchar **s = g_strsplit( g_path_get_basename( tmp->Name ), ".", 2 );
	gchar *t = s[ 1 ];
	gchar *fn = g_strconcat( "template.", t, NULL );
	gchar *template = g_build_filename( g_get_home_dir( ), "/.config/X2/templates/", fn, NULL );
	g_print( "%s\n", template );

	GtkTextIter StartIter, EndIter;
	gtk_text_buffer_get_bounds( GTK_TEXT_BUFFER( tmp->Buffer ), &StartIter, &EndIter );
	g_file_set_contents( template, gtk_text_buffer_get_text( GTK_TEXT_BUFFER( tmp->Buffer ), &StartIter, &EndIter, TRUE ), -1, NULL );
}

static void Populate_CMB( GtkWidget *cmb )
{
	gchar *templates = g_build_filename( g_get_home_dir( ), "/.config/X2/templates/", NULL );
	GDir *d = g_dir_open( templates, 0, NULL );

	// Following code found here: http://www.gtkforums.com/viewtopic.php?t=3425
	GtkTreeModel *store = gtk_combo_box_get_model( GTK_COMBO_BOX( cmb ) );
	gtk_list_store_clear( GTK_LIST_STORE( store ) );

	for( ; ; )
	{
		const gchar *tmp = g_dir_read_name( d );	

		if( tmp != NULL )
			gtk_combo_box_text_append_text( GTK_COMBO_BOX_TEXT( cmb ), tmp );

		else
			break;
	}
}