////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Contains mostly callback handler functions
////////////////////////////////////////////////////////////////////////////

#ifndef CALLBACKS_H__
#define CALLBACKS_H__

#include <gtk/gtk.h>

void on_refresh_activate (GtkMenuItem *menuitem, gpointer user_data);

void on_edit1_activate	 (GtkMenuItem *menuitem, gpointer user_data);

void on_view_create_file_activate (GtkMenuItem *menuitem, gpointer user_data);

void on_copy1_activate	 (GtkMenuItem *menuitem, gpointer user_data);

void on_move1_activate   (GtkMenuItem *menuitem, gpointer user_data);

void on_mkdir1_activate  (GtkMenuItem *menuitem, gpointer user_data);

void on_rename1_activate (GtkMenuItem *menuitem, gpointer user_data);

void on_delete1_activate (GtkMenuItem *menuitem, gpointer user_data);

void on_quit1_activate	 (GtkMenuItem *menuitem, gpointer user_data);

void on_about1_activate  (GtkMenuItem *menuitem, gpointer user_data);

void on_go_user_home_directory (GtkMenuItem *menuitem, gpointer user_data);

void on_open_terminal_activate (GtkMenuItem *menuitem, gpointer user_data);

void set_show_toolbar(bool bShow);
bool get_show_toolbar();
void on_show_toolbar_activate  (GtkMenuItem *menuitem, gpointer user_data);

void set_show_status_bar(bool bShow);
bool get_show_status_bar();
void on_show_status_bar_activate (GtkMenuItem *menuitem, gpointer user_data);

void set_show_command_line(bool bShow);
bool get_show_command_line();
void on_show_command_line_activate (GtkMenuItem *menuitem, gpointer user_data);

void set_show_trace_view(bool bShow);
bool get_show_trace_view();
void on_show_trace_view_activate (GtkMenuItem *menuitem, gpointer user_data);

void set_show_hidden(bool bShow);
bool get_show_hidden();
void on_show_hidden_activate (GtkMenuItem *menuitem, gpointer user_data);

void on_select_all_activate	 (GtkMenuItem *menuitem, gpointer user_data);;
void on_select_none_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_select_invert_activate	 (GtkMenuItem *menuitem, gpointer user_data);
void on_select_select_activate	 (GtkMenuItem *menuitem, gpointer user_data);
void on_select_deselect_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_filter_activate	(GtkMenuItem *menuitem, gpointer user_data);

void on_history_back (GtkMenuItem *menuitem, gpointer user_data);
void on_history_forward	 (GtkMenuItem *menuitem, gpointer user_data);
void update_history_buttons();

void on_swap_panels	(GtkMenuItem *menuitem, gpointer user_data);
void on_equal_panels (GtkMenuItem *menuitem, gpointer user_data);
void on_compare_panels	 (GtkMenuItem *menuitem, gpointer user_data);
void on_options_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_file_search_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_file_pack_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_file_unpack_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_file_hash_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_file_split_activate	(GtkMenuItem *menuitem, gpointer user_data);
void on_file_merge_activate	(GtkMenuItem *menuitem, gpointer user_data);
void on_file_encrypt_decrypt_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_file_view_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_remote_connection_activate (GtkMenuItem *menuitem, gpointer user_data);
void on_connection_close_activate (GtkMenuItem *menuitem, gpointer user_data);
void refresh_connection_gui();

void on_transfer_type_combo_activate(GtkComboBox *widget, gpointer user_data);
bool on_mainwindow_focus(GtkWidget *widget, GdkEventFocus *event, gpointer user_data);



#endif //CALLBACKS_H__

