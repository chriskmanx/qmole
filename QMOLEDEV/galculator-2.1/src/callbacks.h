/*
 *  callbacks.h
 *	part of galculator
 *  	(c) 2002-2013 Simon Flöry (simon.floery@rechenraum.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <gtk/gtk.h>

/* G_MODULE_EXPORT is needed for win32. on UNIX systems its empty. */

G_MODULE_EXPORT void on_number_button_clicked (GtkToggleButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_operation_button_clicked (GtkToggleButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_function_button_clicked (GtkToggleButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_constant_button_clicked (GtkToggleButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_tbutton_fmod_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_gfunc_button_clicked (GtkToggleButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_main_window_destroy (GtkWidget* widget,
					gpointer user_data);
G_MODULE_EXPORT void on_dec_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_hex_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_oct_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_bin_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_deg_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_rad_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_grad_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_ordinary_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_rpn_toggled (GtkMenuItem *menuitem,
					gpointer user_data);					
G_MODULE_EXPORT void on_form_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_about_activate (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_preferences1_activate (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_result_font_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_result_color_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_mod_font_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_act_mod_color_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_inact_mod_color_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bkg_color_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_button_font_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_close_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_color_ok_button_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_color_cancel_button_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_font_ok_button_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_font_cancel_button_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_cancel_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_color_cancel_button_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_font_cancel_button_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_show_menubar1_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_custom_button_font_toggled (GtkToggleButton *togglebutton,
					gpointer user_data);
G_MODULE_EXPORT gboolean on_button_event(GtkWidget *widget, 
					GdkEvent *event, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_vis_number_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_vis_angle_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_vis_notation_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_vis_arith_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_vis_bracket_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_show_menu_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_rem_display_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_button_width_changed (GtkSpinButton *spinbutton,
					GtkScrollType arg1,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_button_height_changed (GtkSpinButton *spinbutton,
					GtkScrollType arg1,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_hex_bits_value_changed (GtkSpinButton *spinbutton, 
					GtkScrollType arg1, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_hex_signed_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);				
G_MODULE_EXPORT void on_prefs_oct_bits_value_changed (GtkSpinButton *spinbutton, 
					GtkScrollType arg1, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_oct_signed_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_bits_value_changed (GtkSpinButton *spinbutton, 
					GtkScrollType arg1, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_signed_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_dec_sep_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_hex_sep_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_oct_sep_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_sep_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_sep_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_dec_sep_length_value_changed (GtkSpinButton *spinbutton,
					GtkScrollType arg1,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_hex_sep_length_value_changed (GtkSpinButton *spinbutton,
					GtkScrollType arg1,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_oct_sep_length_value_changed (GtkSpinButton *spinbutton,
					GtkScrollType arg1,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_dec_sep_char_changed (GtkEditable *editable,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_hex_sep_char_changed (GtkEditable *editable,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_oct_sep_char_changed (GtkEditable *editable,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_sep_char_changed (GtkEditable *editable,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_menu_dec_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_menu_hex_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_menu_oct_activate (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_menu_bin_activate (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void const_list_selection_changed_cb (GtkTreeSelection *selection, 
					gpointer data);
G_MODULE_EXPORT void on_togglebutton_released (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_display_control_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_logical_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_copy_activate (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_basic_mode_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_cut_activate (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_scientific_mode_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_paste_activate (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_functions_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_standard_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_main_window_check_resize (GtkContainer *container,
					gpointer user_data);
G_MODULE_EXPORT void on_finite_stack_size_clicked (GtkRadioButton *rb,
					gpointer user_data);
G_MODULE_EXPORT void on_infinite_stack_size_clicked (GtkRadioButton *rb,
					gpointer user_data);
G_MODULE_EXPORT gboolean on_button_press_event (GtkWidget *widget,
					GdkEventButton *event,
					gpointer user_data);
G_MODULE_EXPORT void on_formula_entry_activate (GtkEntry *entry,
					gpointer user_data);
G_MODULE_EXPORT void on_formula_entry_changed (GtkEditable *editable, 
					gpointer user_data);
G_MODULE_EXPORT void on_user_function_button_clicked (GtkToggleButton *button,
					gpointer user_data);
G_MODULE_EXPORT void user_function_list_selection_changed_cb (GtkTreeSelection *selection, 
					gpointer data);
G_MODULE_EXPORT void on_paper_mode_toggled (GtkMenuItem *menuitem,
					gpointer user_data);
G_MODULE_EXPORT void on_paper_entry_activate (GtkWidget *activated_widget, 
					gpointer user_data);
G_MODULE_EXPORT gboolean paper_tree_view_selection_changed_cb (GtkWidget *widget,
				    GdkEventButton *event,
				    gpointer user_data);
G_MODULE_EXPORT gboolean on_button_can_activate_accel (GtkWidget *widget, 
					guint signal_id, 
					gpointer user_data);
G_MODULE_EXPORT gboolean on_menuitem_can_activate_accel (GtkWidget *widget, 
					guint signal_id, 
					gpointer user_data);
G_MODULE_EXPORT void on_mplus_button_clicked (GtkToggleButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_mr_button_clicked (GtkToggleButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_ms_button_clicked (GtkToggleButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_ufadd_clicked (GtkButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_ufupdate_clicked (GtkButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_ufdelete_clicked (GtkButton *button,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_ufclear_clicked (GtkButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_cadd_clicked (GtkButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_cupdate_clicked (GtkButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_cdelete_clicked (GtkButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_cclear_clicked (GtkButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_sep_length_value_changed (GtkSpinButton *spinbutton,
					GtkScrollType arg1,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_fixed_toggled (GtkToggleButton *togglebutton, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bin_length_value_changed (GtkSpinButton *spinbutton,
					GtkScrollType arg1,
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_button_font_set (GtkFontButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_mod_font_set (GtkFontButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_result_font_set (GtkFontButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_result_color_set (GtkColorButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_stack_font_set (GtkFontButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_stack_color_set (GtkColorButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_act_mod_color_set (GtkColorButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_inact_mod_color_set (GtkColorButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_bkg_color_set (GtkColorButton *button, 
					gpointer user_data);
G_MODULE_EXPORT void on_prefs_number_combo_changed(GtkComboBox *widget,
					gpointer user_data);
