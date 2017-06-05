/*
 *  callbacks.c - functions to handle GUI events.
 *    part of galculator
 *      (c) 2002-2013 Simon Flöry (simon.floery@rechenraum.com)
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
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "calc_basic.h"
#include "galculator.h"
#include "math_functions.h"
#include "general_functions.h"
#include "display.h"
#include "config_file.h"
#include "callbacks.h"
#include "ui.h"
#include "flex_parser.h"

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

/* File */

void
on_main_window_destroy               (GtkWidget*         widget,
                                        gpointer         user_data)
{
    char     **stack;
    
    /* remember some things */
    if (prefs.mode != BASIC_MODE) {
        /* save number and angle mode only in scientific mode. */
        prefs.def_number = current_status.number;
        prefs.def_angle = current_status.angle;
    }
    /* if we have the classic view display we remember the display values */
    if (prefs.mode != PAPER_MODE) {
        if (prefs.rem_valuex) g_free (prefs.rem_valuex);
        prefs.rem_valuex = display_result_get();
        if (current_status.notation == CS_RPN) {
            if (prefs.rem_valuey) g_free (prefs.rem_valuey);
            if (prefs.rem_valuez) g_free (prefs.rem_valuez);
            if (prefs.rem_valuet) g_free (prefs.rem_valuet);
            /* we only save the visible stack */
            stack = display_stack_get_yzt();
            prefs.rem_valuey = g_strdup(stack[0]);
            g_free (stack[0]);
            prefs.rem_valuez = g_strdup(stack[1]);
            g_free (stack[1]);
            prefs.rem_valuet = g_strdup(stack[2]);
            g_free (stack[2]);
            g_free (stack);
        }
        prefs.def_notation = current_status.notation;
    }

    g_object_unref(main_window_xml);
    main_window_xml = NULL;

    gtk_main_quit();
}

/* Help */

void
on_about_activate                     (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    GtkWidget *about_dialog = ui_about_dialog_create();
    gtk_dialog_run (GTK_DIALOG(about_dialog));
}

/* this callback is called if a button for entering a number is clicked. There are two
 * cases: either starting a new number or appending a digit to the existing number.
 * The decimal point leads to some specialities.
 */

void
on_number_button_clicked               (GtkToggleButton  *button,
                                        gpointer         user_data)
{    
    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    if (current_status.notation == CS_FORMULA) {
        ui_formula_entry_insert (gtk_button_get_label ((GtkButton *)button));
    } else {
        rpn_stack_lift();
        display_result_add_digit (*(gtk_button_get_label ((GtkButton *)button)), current_status.number);
    }
    return;
}

/* this callback is called if a button for doing one of the arithmetic operations plus, minus, 
 * multiply, divide or power is clicked. it is mainly an interface to the calc_basic code.
 */

void
on_operation_button_clicked(GtkToggleButton *button, gpointer user_data)
{
    s_cb_token        current_token;
    G_REAL            return_value, *stack;
    GtkWidget        *tbutton;
    
    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    
    current_token.operation = GPOINTER_TO_INT(g_object_get_data(G_OBJECT (button), "operation"));
    /* do inverse left shift is a right shift */
    if ((current_token.operation == '<') && (BIT (current_status.fmod, CS_FMOD_FLAG_INV) == 1)) {
		tbutton = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_inv"));
		gtk_toggle_button_set_active ((GtkToggleButton *) tbutton, FALSE);
        current_token.operation = '>';
    }
    
    if (current_status.notation == CS_FORMULA) {
        if (strcmp (gtk_buildable_get_name(GTK_BUILDABLE(button)), "button_enter") == 0)
            ui_formula_entry_activate();
        /* as long as we don't support string operation ids, we take
         * operation char. take this later on:
         * else ui_formula_entry_insert (
         *    g_object_get_data (G_OBJECT (button), "display_string"));
         */
        else {
			if (current_token.operation == '<')
				ui_formula_entry_insert("<<");
			else if (current_token.operation == '>')
				ui_formula_entry_insert(">>");
			else {
				char text[2];
				text[0] = current_token.operation;
				text[1] = '\0';
				ui_formula_entry_insert(text);
			}
        }
        return;
    }
    
    /* current number, get it from the display! */
    current_token.number = display_result_get_double (current_status.number);
    current_token.func = NULL;
    
    /* notation specific interface code */
    
    if (current_status.notation == CS_PAN) {
        /* '(' doesn't pay respect to allow_arith_op but sets it: a+((((((b-...
         * ')' pays respect to allow_arith_op but doesn't set it: ...+a)))))-...
         * '=' pays respect to allow_arith_op but doesn't set it: ...+a=
         *     (in order to continue with the result on the display)
         * all other operator pay respect and set allow_arith_op.
         *
         * in general, a closing bracket is only useful if there were opening
         *    brackets.
         */
        if (((current_token.operation == '(') || current_status.allow_arith_op) && \
            ((current_token.operation != ')') || (display_module_bracket_label_update (GET) > 0))) {
            return_value = alg_add_token (&main_alg, current_token);
            display_result_set_double (return_value, current_status.number);
            display_module_arith_label_update (current_token.operation);
            
            /* setting of allow_arith_op. the missing breaks are wanted */
            switch (current_token.operation) {
                case '=':
                    display_module_bracket_label_update (RESET);
                    break;
                case ')':
                    display_module_bracket_label_update (ONE_LESS);
                    break;
                case '(':
                    display_module_bracket_label_update (ONE_MORE);
                default:
                    current_status.allow_arith_op=FALSE;
            }
        }
    } else if (current_status.notation == CS_RPN) {
        switch (current_token.operation) {
        case '=':
            rpn_stack_push (current_token.number);
            stack = rpn_stack_get (RPN_FINITE_STACK);
            display_stack_set_yzt_double (stack, current_status.number);
            free (stack);
            /* ENT is a stack lift disabling button */
            current_status.rpn_stack_lift_enabled = FALSE;
            /* display line isn't cleared! */
            break;
        default:
            display_result_set_double (rpn_stack_operation (current_token), current_status.number);
            stack = rpn_stack_get (RPN_FINITE_STACK);
            display_stack_set_yzt_double (stack, current_status.number);
            free (stack);
            /* all other operations are stack lift enabling */
            current_status.rpn_stack_lift_enabled = TRUE;
        }
    } else error_message ("on_operation_button_clicked: unknown status");

    current_status.calc_entry_start_new = TRUE;
    return;
}

/* this callback is called if a button for a function manipulating the current 
 * entry directly is clicked. the array function_list knows the relation between 
 * button label and function to call.
 */

void
on_function_button_clicked             (GtkToggleButton    *button,
                                        gpointer user_data)
{
    G_REAL        (*func[4])(G_REAL);
    char         **display_name;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    if (current_status.notation == CS_FORMULA) {
        display_name = (char **) g_object_get_data (G_OBJECT (button), "display_names");
        ui_formula_entry_insert (display_name[current_status.fmod]);
        if (current_status.fmod != 0) ui_relax_fmod_buttons();
        return;
    }
    memcpy (func, g_object_get_data (G_OBJECT (button), "func"), sizeof (func));
    if (!*func) error_message ("This button has no function associated with");
    display_result_set_double (
        func[current_status.fmod](display_result_get_double(current_status.number)), current_status.number);
    current_status.calc_entry_start_new = TRUE;    
    if (current_status.notation == CS_RPN) 
        current_status.rpn_stack_lift_enabled = TRUE;
    if (current_status.fmod != 0) ui_relax_fmod_buttons();
}

/* tbutton_fmod - these are function modifiers such as INV (inverse) 
 *    and HYP (hyperbolic).
 */

void
on_tbutton_fmod_clicked                (GtkButton       *button,
                                        gpointer         user_data)
{
    if (strcmp (gtk_button_get_label (button), "inv") == 0)
        current_status.fmod ^= 1 << CS_FMOD_FLAG_INV;
    else if (strcmp (gtk_button_get_label (button), "hyp") == 0)
        current_status.fmod ^= 1 << CS_FMOD_FLAG_HYP;
    else error_message ("unknown function modifier (INV/HYP)");
}

void
on_gfunc_button_clicked                (GtkToggleButton       *button,
                                        gpointer         user_data)
{
    void    (*func)();
    char     *display_string;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    if (current_status.notation == CS_FORMULA) {
        display_string = g_object_get_data (G_OBJECT (button), "display_string");
        if (display_string != NULL) {
            ui_formula_entry_insert (display_string);
            return;
        }
    }
    if (strcmp(gtk_buildable_get_name(GTK_BUILDABLE(button)), "button_ee") == 0) 
        rpn_stack_lift();
    func = g_object_get_data (G_OBJECT (button), "func");
    if (func != NULL) func(button);
    else error_message ("This button has no general function associated with");
}

/*
 * MENU
 */

void
on_dec_toggled                       (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (!gtk_check_menu_item_get_active((GtkCheckMenuItem *)menuitem)) return;
    change_option (CS_DEC, DISPLAY_OPT_NUMBER);
}


void
on_hex_toggled (GtkMenuItem     *menuitem, gpointer         user_data)
{
    if (!gtk_check_menu_item_get_active((GtkCheckMenuItem *)menuitem)) return;
    change_option (CS_HEX, DISPLAY_OPT_NUMBER);
}


void
on_oct_toggled                       (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (!gtk_check_menu_item_get_active((GtkCheckMenuItem *)menuitem)) return;
    change_option (CS_OCT, DISPLAY_OPT_NUMBER);
}


void
on_bin_toggled                       (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (!gtk_check_menu_item_get_active((GtkCheckMenuItem *)menuitem)) return;
    change_option (CS_BIN, DISPLAY_OPT_NUMBER);
}


void
on_deg_toggled                       (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (!gtk_check_menu_item_get_active((GtkCheckMenuItem *)menuitem)) return;
    change_option (CS_DEG, DISPLAY_OPT_ANGLE);
}


void
on_rad_toggled                       (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (!gtk_check_menu_item_get_active((GtkCheckMenuItem *)menuitem)) return;
    change_option (CS_RAD, DISPLAY_OPT_ANGLE);
}


void
on_grad_toggled                      (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (!gtk_check_menu_item_get_active((GtkCheckMenuItem *)menuitem)) return;
    change_option (CS_GRAD, DISPLAY_OPT_ANGLE);
}

void
on_ordinary_toggled                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menuitem)) == FALSE) return;
    change_option (CS_PAN, DISPLAY_OPT_NOTATION);
    set_widget_visibility (view_xml, "formula_entry_hbox", FALSE);
    rpn_free();
    all_clear();
    ui_button_set_pan();
    display_stack_remove();
    update_dispctrl();
    /* pixel above/below display result line */
    display_update_tags ();
}

void
on_rpn_toggled                       (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menuitem)) == FALSE) return;
    change_option (CS_RPN, DISPLAY_OPT_NOTATION);
    
    set_widget_visibility (view_xml, "formula_entry_hbox", FALSE);
    alg_free(main_alg);
    all_clear();
    ui_button_set_rpn();
    /* stack is created by all_clear */
    update_dispctrl();
    /* pixel above/below display result line */
    display_update_tags ();
}

/* toggle formula entry */
void 
on_form_toggled             (GtkMenuItem     *menuitem,
                    gpointer         user_data)
{
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menuitem)) == FALSE) return;
    change_option (CS_FORMULA, DISPLAY_OPT_NOTATION);
    all_clear();
    ui_button_set_pan();
    update_dispctrl();
    set_widget_visibility (view_xml, "formula_entry_hbox", TRUE);
}

void
on_display_control_toggled (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    if (prefs.mode == PAPER_MODE) return;
    prefs.vis_dispctrl = 
        gtk_check_menu_item_get_active((GtkCheckMenuItem *) menuitem);
    set_widget_visibility (dispctrl_xml, "table_dispctrl", 
        prefs.vis_dispctrl);
}

void 
on_logical_toggled (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    if (prefs.mode == BASIC_MODE) return;
    if (prefs.mode == PAPER_MODE) return;

    prefs.vis_logic = 
        gtk_check_menu_item_get_active((GtkCheckMenuItem *) menuitem);
    set_widget_visibility (button_box_xml, "table_bin_buttons",
        prefs.vis_logic);
}

void
on_functions_toggled (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    if (prefs.mode == BASIC_MODE) return;
    if (prefs.mode == PAPER_MODE) return;
    prefs.vis_funcs = 
        gtk_check_menu_item_get_active((GtkCheckMenuItem *) menuitem);
    set_widget_visibility (button_box_xml, "table_func_buttons",
        prefs.vis_funcs);
}

void
on_standard_toggled (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    if (prefs.mode == BASIC_MODE) return;
    if (prefs.mode == PAPER_MODE) return;
    prefs.vis_standard = 
        gtk_check_menu_item_get_active((GtkCheckMenuItem *) menuitem);
    set_widget_visibility (button_box_xml, "table_standard_buttons",
        prefs.vis_standard);
}

void
on_basic_mode_toggled (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    GtkWidget    *menu_item;
    
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menuitem)) == FALSE) return;

	double display_value = 0;
    if (prefs.mode == SCIENTIFIC_MODE) {
        /* remember number and angle. notation is active in basic mode */
        prefs.def_number = current_status.number;
        prefs.def_angle = current_status.angle;
        display_value = display_result_get_double(current_status.number);
    }
    prefs.mode = BASIC_MODE;
    
    ui_paper_view_destroy ();
    ui_classic_view_create ();
    ui_main_window_buttons_destroy ();
    ui_main_window_buttons_create (prefs.mode);
    update_dispctrl();
    
    display_update_modules();
    
    /* In basic mode:
     *    - number base is always decimal.
     *    - ignore angle, as there are no angle operations in basic mode.
     *    - notation is fully functional.
     */    
    display_module_number_activate (CS_DEC);
    display_module_notation_activate (current_status.notation);

    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "display_control"));
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), prefs.vis_dispctrl);

    update_active_buttons (current_status.number, current_status.notation);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "functions"));
    gtk_widget_set_sensitive (menu_item, FALSE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "logical"));
    gtk_widget_set_sensitive (menu_item, FALSE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "standard"));
    gtk_widget_set_sensitive (menu_item, FALSE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "base_units"));
    gtk_widget_set_sensitive (menu_item, FALSE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "angle_units"));
    gtk_widget_set_sensitive (menu_item, FALSE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "buttons1"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "notation"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    
    set_window_size_minimal();
    
    display_result_set_double(display_value, current_status.number);
}

void
on_scientific_mode_toggled (GtkMenuItem *menuitem,
                gpointer user_data)
{
    GtkWidget    *menu_item;
    
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menuitem)) == FALSE) return;

	double display_value = 0;
    if (prefs.mode == BASIC_MODE)
		display_value = display_result_get_double(current_status.number);
		
    prefs.mode = SCIENTIFIC_MODE;

    ui_paper_view_destroy ();
    ui_classic_view_create ();
    ui_main_window_buttons_destroy ();
    ui_main_window_buttons_create (prefs.mode);
    
    display_update_modules();
    display_module_number_activate (prefs.def_number);
    display_module_angle_activate (prefs.def_angle);
    display_module_notation_activate (current_status.notation);

    update_active_buttons (current_status.number, current_status.notation);
    update_dispctrl();
    
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "functions"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu_item), prefs.vis_funcs);
    
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "display_control"));
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu_item), prefs.vis_dispctrl);
    
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "logical"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu_item), prefs.vis_logic);
    
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "standard"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu_item), prefs.vis_standard);
    
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "base_units"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "angle_units"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "buttons1"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "notation"));
    gtk_widget_set_sensitive (menu_item, TRUE);
    
    set_window_size_minimal();
    
    display_result_set_double(display_value, current_status.number);
}

void
on_paper_mode_toggled (GtkMenuItem *menuitem,
                gpointer user_data)
{
    GtkWidget    *menu_item;
    
    if (gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(menuitem)) == FALSE) return;

    prefs.mode = PAPER_MODE;

    ui_classic_view_destroy();
    ui_paper_view_create ();
    
    /* this is to get the radio menu items right. display* naming is 
     * misleading though ...
     */
    display_module_number_activate (prefs.def_number);
    display_module_angle_activate (prefs.def_angle);
    
    /* update menus */
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "buttons1"));
    gtk_widget_set_sensitive (menu_item, FALSE);
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "notation"));
    gtk_widget_set_sensitive (menu_item, FALSE);
    
    set_window_size_minimal();
}    

void
on_cut_activate (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    if (prefs.mode == PAPER_MODE) return;
    gtk_clipboard_set_text (gtk_clipboard_get (GDK_SELECTION_CLIPBOARD), 
        display_result_get(), -1);
    clear ();
}

void
on_paste_activate (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    GtkWidget    *formula_entry;
    char        *cp_text;
    
    if (prefs.mode == PAPER_MODE) return;
    cp_text = gtk_clipboard_wait_for_text (gtk_clipboard_get (GDK_SELECTION_CLIPBOARD));
    if (cp_text) {
        if ((formula_entry = formula_entry_is_active_no_toplevel_check ()) != NULL) {
            gtk_editable_paste_clipboard((GtkEditable *)formula_entry);
        }
        else display_result_feed (cp_text, current_status.number);
        g_free (cp_text);
    }
}

void
on_copy_activate (GtkMenuItem     *menuitem,
            gpointer         user_data)
{
    if (prefs.mode == PAPER_MODE) return;
    gtk_clipboard_set_text (gtk_clipboard_get (GDK_SELECTION_CLIPBOARD), 
        display_result_get(), -1);
}

/*
 * Preferences
 */

void user_function_list_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeModel     *model;
    char         *string;
    GtkWidget    *entry;
    GtkTreeIter     current_list_iter;
    
        if (gtk_tree_selection_get_selected (selection, &model, &current_list_iter))
        {
                gtk_tree_model_get (model, &current_list_iter, UFUNC_NAME_COLUMN, &string, -1);
        entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufname_entry"));
        gtk_entry_set_text ((GtkEntry *) entry, string);
                g_free (string);
        gtk_tree_model_get (model, &current_list_iter, UFUNC_VARIABLE_COLUMN, &string, -1);
        entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufvar_entry"));
        gtk_entry_set_text ((GtkEntry *) entry, string);
                g_free (string);
        gtk_tree_model_get (model, &current_list_iter, UFUNC_EXPRESSION_COLUMN, &string, -1);
        entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufexpr_entry"));
        gtk_entry_set_text ((GtkEntry *) entry, string);
                g_free (string);
        }
}

void const_list_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeModel    *model;
    char            *string;
    GtkWidget       *entry;
    GtkTreeIter     current_list_iter;
    
        if (gtk_tree_selection_get_selected (selection, &model, &current_list_iter))
        {
                gtk_tree_model_get (model, &current_list_iter, CONST_NAME_COLUMN, &string, -1);
        entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cname_entry"));
        gtk_entry_set_text ((GtkEntry *) entry, string);
                g_free (string);
        gtk_tree_model_get (model, &current_list_iter, CONST_VALUE_COLUMN, &string, -1);
        entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cvalue_entry"));
        gtk_entry_set_text ((GtkEntry *) entry, string);
                g_free (string);
        gtk_tree_model_get (model, &current_list_iter, CONST_DESC_COLUMN, &string, -1);
        entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cdesc_entry"));
        gtk_entry_set_text ((GtkEntry *) entry, string);
                g_free (string);
        }
}

void
on_preferences1_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    ui_pref_dialog_create();
}

void
on_prefs_result_font_set(GtkFontButton *button, gpointer user_data)
{
    const char    *font_name;
    
    font_name = gtk_font_button_get_font_name(button);
    if (prefs.result_font != NULL) g_free (prefs.result_font);
    prefs.result_font = g_strdup(font_name);
    display_update_tags();
}

void
on_prefs_result_color_set(GtkColorButton *button, gpointer user_data)
{
    if (prefs.result_color != NULL) g_free (prefs.result_color);

    GdkColor color;
    gtk_color_button_get_color (button, &color);
    prefs.result_color = convert_gdk_color_to_string(color);

    display_update_tags();
}

void
on_prefs_stack_font_set(GtkFontButton *button, gpointer user_data)
{
    const char    *font_name;
    
    font_name = gtk_font_button_get_font_name(button);
    if (prefs.stack_font != NULL) g_free (prefs.stack_font);
    prefs.stack_font = g_strdup(font_name);
    display_update_tags();
}

void
on_prefs_stack_color_set(GtkColorButton *button, gpointer user_data)
{
	if (prefs.stack_color != NULL) g_free (prefs.stack_color);
	
    GdkColor color;
    gtk_color_button_get_color (button, &color);
    prefs.stack_color = convert_gdk_color_to_string(color);
    
    display_update_tags();
}

void
on_prefs_mod_font_set(GtkFontButton *button, gpointer user_data)
{
    const char    *font_name;
    
    font_name = gtk_font_button_get_font_name(button);
    if (prefs.mod_font != NULL) g_free (prefs.mod_font);
    prefs.mod_font = g_strdup(font_name);
    display_update_tags();
}

void
on_prefs_act_mod_color_set(GtkColorButton *button, gpointer user_data)
{
    if (prefs.act_mod_color != NULL) g_free (prefs.act_mod_color);
    
    GdkColor color;
    gtk_color_button_get_color (button, &color);
    prefs.act_mod_color = convert_gdk_color_to_string(color);
    
    display_update_tags();
}

void
on_prefs_inact_mod_color_set(GtkColorButton *button, gpointer user_data)
{
	if (prefs.inact_mod_color != NULL) g_free (prefs.inact_mod_color);
    
    GdkColor color;
    gtk_color_button_get_color (button, &color);
    prefs.inact_mod_color = convert_gdk_color_to_string(color);

    display_update_tags();
}

void
on_prefs_bkg_color_set(GtkColorButton *button, gpointer user_data)
{
	if (prefs.bkg_color != NULL) g_free (prefs.bkg_color);
	
    GdkColor color;
    gtk_color_button_get_color (button, &color);
    prefs.bkg_color = convert_gdk_color_to_string(color);

    display_set_bkg_color (prefs.bkg_color);
}

void
on_prefs_button_font_set(GtkFontButton *button, gpointer user_data)
{
    const char    *font_name;
    char        *button_font;
    
    font_name = gtk_font_button_get_font_name(button);
    if (prefs.button_font != NULL) g_free (prefs.button_font);
    prefs.button_font = g_strdup(font_name);
    if (prefs.custom_button_font == TRUE) button_font = g_strdup (prefs.button_font);
    else button_font = g_strdup ("");
    set_all_buttons_font (button_font);    
    g_free (button_font);
}

void
on_prefs_close_clicked                (GtkButton       *button,
                                        gpointer         user_data)
{
    gtk_widget_destroy (gtk_widget_get_toplevel((GtkWidget *)button));
}

void
on_show_menubar1_toggled              (GtkMenuItem     *menuitem,
                                        gpointer         user_data)
{
    GtkWidget    *menu_item;
    prefs.show_menu = gtk_check_menu_item_get_active ((GtkCheckMenuItem *) menuitem);
#ifdef WITH_HILDON
    set_widget_visibility (main_window_xml, "main_menu", prefs.show_menu);
#else
     set_widget_visibility (main_window_xml, "menubar", prefs.show_menu);
#endif

    /* in case this cb is called by the right button mouse click menu */
    menu_item = GTK_WIDGET(gtk_builder_get_object (main_window_xml, "show_menubar1"));
    g_signal_handlers_block_by_func(menuitem, on_show_menubar1_toggled, user_data);
    gtk_check_menu_item_set_active ((GtkCheckMenuItem *) menu_item, prefs.show_menu);
    g_signal_handlers_unblock_by_func(menuitem, on_show_menubar1_toggled, user_data);
}

void
on_prefs_custom_button_font_toggled    (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
    GtkWidget    *w;
    char        *button_font;
    
    prefs.custom_button_font = gtk_toggle_button_get_active (togglebutton);
    
    w = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_button_font_label"));
    gtk_widget_set_sensitive (w, prefs.custom_button_font);
    
    w = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_button_font"));
    gtk_widget_set_sensitive (w, prefs.custom_button_font);
    
    if (prefs.custom_button_font == TRUE) button_font = g_strdup (prefs.button_font);
    else button_font = g_strdup ("");
    set_all_buttons_font (button_font);
    g_free (button_font);
}

void on_prefs_vis_number_toggled (GtkToggleButton *togglebutton, 
                gpointer user_data)
{
    prefs.vis_number = gtk_toggle_button_get_active (togglebutton);
    display_update_modules ();
}
                                        
void on_prefs_vis_angle_toggled (GtkToggleButton *togglebutton, 
                gpointer user_data)
{
    prefs.vis_angle = gtk_toggle_button_get_active (togglebutton);
    display_update_modules ();
}

void on_prefs_vis_notation_toggled (GtkToggleButton *togglebutton, 
                gpointer user_data)
{
    prefs.vis_notation = gtk_toggle_button_get_active (togglebutton);
    display_update_modules ();
}

void on_prefs_vis_arith_toggled (GtkToggleButton *togglebutton, 
                gpointer user_data)
{
    prefs.vis_arith = gtk_toggle_button_get_active (togglebutton);
    display_update_modules ();
}
    
void on_prefs_vis_bracket_toggled (GtkToggleButton *togglebutton, 
                gpointer user_data)
{
    prefs.vis_bracket = gtk_toggle_button_get_active (togglebutton);
    display_update_modules ();
}

void on_prefs_show_menu_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    GtkCheckMenuItem        *show_menubar_item;
    
    prefs.show_menu = gtk_toggle_button_get_active (togglebutton);
#ifdef WITH_HILDON
    set_widget_visibility (main_window_xml, "main_menu", prefs.show_menu);
#else
     set_widget_visibility (main_window_xml, "menubar", prefs.show_menu);
#endif
    show_menubar_item = (GtkCheckMenuItem *) gtk_builder_get_object (main_window_xml, "show_menubar1");
    gtk_check_menu_item_set_active (show_menubar_item, prefs.show_menu);
}

void on_prefs_rem_display_toggled (GtkToggleButton *togglebutton, 
                gpointer user_data)
{
    prefs.rem_display = gtk_toggle_button_get_active (togglebutton);
    /* only is important when leaving galculator */
}

void on_prefs_button_width_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    int new_button_width = (int) gtk_spin_button_get_value (spinbutton);
    
    if (new_button_width != prefs.button_width) {
        prefs.button_width = new_button_width;
        set_all_buttons_size (prefs.button_width, prefs.button_height);
    }
}

void on_prefs_button_height_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{    
    int new_button_height = (int) gtk_spin_button_get_value (spinbutton);
    if (new_button_height != prefs.button_height) {
        prefs.button_height = new_button_height;
        set_all_buttons_size (prefs.button_width, prefs.button_height);
    }
}

/*
 * USER FUNCTIONS
 */

void user_functions_menu_handler (GtkMenuItem *menuitem, gpointer user_data)
{
    int             index;
    s_flex_parser_result    result;
    
    index = GPOINTER_TO_INT(user_data);
    result = compute_user_function (
        user_function[index].expression, user_function[index].variable,
        display_result_get());
    if (!result.error) {
        display_result_set_double (result.value, current_status.number);
        current_status.calc_entry_start_new = TRUE;    
        if (current_status.notation == CS_RPN) 
            current_status.rpn_stack_lift_enabled = TRUE;
    } else fprintf (stderr, "[%s] User function %s(%s)=%s returned with an error.\
Please check the expression string.\n", PROG_NAME, user_function[index].name, 
user_function[index].variable, user_function[index].expression); 
}

void
on_user_function_button_clicked (GtkToggleButton       *button,
                                        gpointer         user_data)
{
    GtkWidget    *menu;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    menu = ui_user_functions_menu_create(user_function, (GCallback)user_functions_menu_handler);
    gtk_menu_popup ((GtkMenu *)menu, NULL, NULL, (GtkMenuPositionFunc) position_menu, 
        button, 0, 0);
}

/*
 * CONSTANTS
 */

void constants_menu_handler (GtkMenuItem *menuitem, gpointer user_data)
{
    char        *const_value;
    
    /* push current display value */
    current_status.rpn_stack_lift_enabled = TRUE;
    rpn_stack_lift();
    const_value = user_data;
    display_result_set (const_value, TRUE, string2double(const_value, current_status.number));
    current_status.rpn_stack_lift_enabled = TRUE;
    current_status.calc_entry_start_new = TRUE;
}


void
on_constant_button_clicked (GtkToggleButton       *button,
                                        gpointer         user_data)
{
    GtkWidget        *menu;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    menu = ui_constants_menu_create(constant, (GCallback)constants_menu_handler);
    gtk_menu_popup ((GtkMenu *)menu, NULL, NULL, (GtkMenuPositionFunc) position_menu, 
        button, 0, 0);
}

/*
 * MEMORY
 */

void ms_menu_handler (GtkMenuItem *menuitem, gpointer user_data)
{
    GtkWidget    *button;
    int        index;
    
    index = GPOINTER_TO_INT(user_data);
    if (index >= memory.len) {
        index = memory.len;
        memory.data = (G_REAL *) realloc (memory.data, (index + 1) * sizeof(G_REAL));
        memory.len++;
    }
    memory.data[index] = display_result_get_double(current_status.number);
    
    /* at startup, mr and m+ button are disabled as there is nothing
     * to show. now, as there is sth, enable them. see also 
     * ui.c::ui_main_window_buttons_create
     */
    button = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_MR"));
    gtk_widget_set_sensitive (button, TRUE);
    button = GTK_WIDGET(gtk_builder_get_object (button_box_xml, "button_Mplus"));
    gtk_widget_set_sensitive (button, TRUE);
    
    current_status.calc_entry_start_new = TRUE;
}

void on_ms_button_clicked (GtkToggleButton *button, gpointer user_data)
{
    GtkWidget    *menu;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    menu = ui_memory_menu_create (memory, (GCallback)ms_menu_handler, _("save here"));
    gtk_menu_popup ((GtkMenu *)menu, NULL, NULL, (GtkMenuPositionFunc) position_menu, 
        button, 0, 0);
}

void mr_menu_handler (GtkMenuItem *menuitem, gpointer user_data)
{
    int        index;

    /* current display value on stack */
    current_status.rpn_stack_lift_enabled = TRUE;
    rpn_stack_lift();
    index = GPOINTER_TO_INT(user_data);
    display_result_set_double(memory.data[index], current_status.number);
    current_status.rpn_stack_lift_enabled = TRUE;
    current_status.calc_entry_start_new = TRUE;
}

void on_mr_button_clicked (GtkToggleButton *button, gpointer user_data)
{
    GtkWidget    *menu;;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    menu = ui_memory_menu_create(memory, (GCallback)mr_menu_handler, NULL);
    gtk_menu_popup ((GtkMenu *)menu, NULL, NULL, (GtkMenuPositionFunc) position_menu,
        button, 0, 0);

}

void mplus_menu_handler (GtkMenuItem *menuitem, gpointer user_data)
{
    int        index;
    
    index = GPOINTER_TO_INT(user_data);
    memory.data[index] += display_result_get_double(current_status.number);
}

void on_mplus_button_clicked (GtkToggleButton *button, gpointer user_data)
{
    GtkWidget    *menu;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    menu = ui_memory_menu_create(memory, (GCallback)mplus_menu_handler, NULL);
    gtk_menu_popup ((GtkMenu *)menu, NULL, NULL, (GtkMenuPositionFunc) position_menu,
        button, 0, 0);

}

void mc_menu_handler (GtkMenuItem *menuitem, gpointer user_data)
{
    int        index, counter;
    
    index = GPOINTER_TO_INT(user_data);
    if (index >= memory.len) {
        if (memory.len > 0) free (memory.data);
        memory.data = NULL;
        memory.len = 0;
    } else {
        for (counter = index; counter < (memory.len - 1); counter++) 
            memory.data[counter] = memory.data[counter + 1];
        memory.len--;
        memory.data = (G_REAL *) realloc (memory.data, memory.len * sizeof(G_REAL));
    }
}

void
on_mc_button_clicked             (GtkToggleButton       *button,
                gpointer         user_data)
{
    GtkWidget    *menu;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    menu = ui_memory_menu_create(memory, (GCallback)mc_menu_handler, "clear all");
    gtk_menu_popup ((GtkMenu *)menu, NULL, NULL, (GtkMenuPositionFunc) position_menu,
        button, 0, 0);

}

void mx_menu_handler (GtkMenuItem *menuitem, gpointer user_data)
{
    int        index, temp;
    
    index = GPOINTER_TO_INT(user_data);
    temp = memory.data[index];
    memory.data[index] = display_result_get_double(current_status.number);
    display_result_set_double (temp, current_status.number);
}

void
on_mx_button_clicked             (GtkToggleButton       *button,
                gpointer         user_data)
{
    GtkWidget    *menu;

    if (gtk_toggle_button_get_active(button) == FALSE) return;
    button_activation (button);
    menu = ui_memory_menu_create(memory, (GCallback)mx_menu_handler, NULL);
    gtk_menu_popup ((GtkMenu *)menu, NULL, NULL, (GtkMenuPositionFunc) position_menu,
        button, 0, 0);

}

void on_prefs_ufclear_clicked (GtkButton *button, gpointer user_data)
{
    GtkWidget    *entry;
    
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufname_entry"));
    gtk_entry_set_text ((GtkEntry *) entry, "");
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufvar_entry"));
    gtk_entry_set_text ((GtkEntry *) entry, "");
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufexpr_entry"));
    gtk_entry_set_text ((GtkEntry *) entry, "");
}

void on_prefs_ufadd_clicked (GtkButton *button, gpointer user_data)
{
    GtkWidget        *entry;
    GtkTreeIter           iter;
    int            nr_user_functions;
    char             *name, *value, *desc;
    
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufname_entry"));
    name = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufvar_entry"));
    value = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufexpr_entry"));
    desc = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    
    if ((strlen(name) == 0) || (strlen(value) == 0) || (strlen(desc) == 0)) {
        g_free (name);
        g_free (value);
        g_free (desc);
        return;
    }
        
    nr_user_functions = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(prefs_user_function_store), NULL);
    user_function = (s_user_function *) realloc (user_function, (nr_user_functions + 2) * sizeof(s_user_function));
    user_function[nr_user_functions + 1].name = NULL;
    
    user_function[nr_user_functions].name = name;
    user_function[nr_user_functions].variable = value;
    user_function[nr_user_functions].expression = desc;
    
    gtk_list_store_append (prefs_user_function_store, &iter);    
    gtk_list_store_set (prefs_user_function_store, &iter, 
        UFUNC_NAME_COLUMN, user_function[nr_user_functions].name, 
        UFUNC_VARIABLE_COLUMN, user_function[nr_user_functions].variable, 
        UFUNC_EXPRESSION_COLUMN, user_function[nr_user_functions].expression, 
        -1);
}

void on_prefs_ufdelete_clicked (GtkButton *button, gpointer user_data)
{
    GtkTreePath        *path;
    int            index, counter, nr_user_functions;
    GtkTreeIter        current_list_iter;
        
    if (!gtk_tree_selection_get_selected (gtk_tree_view_get_selection(
        (GtkTreeView *)gtk_builder_get_object (prefs_xml, "user_function_treeview")),
        NULL, &current_list_iter)) return;
    
    nr_user_functions = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(prefs_user_function_store), NULL);
    path = gtk_tree_model_get_path (GTK_TREE_MODEL (prefs_user_function_store), 
        &current_list_iter);
    index = *(gtk_tree_path_get_indices (path));

    gtk_list_store_remove (prefs_user_function_store, &current_list_iter);
    on_prefs_ufclear_clicked (NULL, NULL);

    for (counter = index; counter < (nr_user_functions - 1); counter++)
        memcpy (&user_function[counter], &user_function[counter+1], sizeof(s_user_function));
    
    nr_user_functions--;
    user_function = (s_user_function *) realloc (user_function, (nr_user_functions + 1) * sizeof(s_user_function));
    
    user_function[nr_user_functions].name = NULL;
}

void on_prefs_ufupdate_clicked (GtkButton *button, gpointer user_data)
{
    GtkWidget    *entry;
    GtkTreePath    *path;
    int        index;
    GtkTreeIter     current_list_iter;
    
    if (!gtk_tree_selection_get_selected (gtk_tree_view_get_selection(
        (GtkTreeView *)gtk_builder_get_object (prefs_xml, "user_function_treeview")),
        NULL, &current_list_iter)) return;
    
    path = gtk_tree_model_get_path (GTK_TREE_MODEL (prefs_user_function_store), 
        &current_list_iter);
    index = *(gtk_tree_path_get_indices (path));
    
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufname_entry"));
    user_function[index].name = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufvar_entry"));
    user_function[index].variable = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_ufexpr_entry"));
    user_function[index].expression = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    
    gtk_list_store_set (prefs_user_function_store, &current_list_iter, 
        UFUNC_NAME_COLUMN, user_function[index].name, 
        UFUNC_VARIABLE_COLUMN, user_function[index].variable, 
        UFUNC_EXPRESSION_COLUMN, user_function[index].expression, 
        -1);
}

void on_prefs_cclear_clicked (GtkButton *button, gpointer user_data)
{
    GtkWidget    *entry;
    
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cname_entry"));
    gtk_entry_set_text ((GtkEntry *) entry, "");
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cvalue_entry"));
    gtk_entry_set_text ((GtkEntry *) entry, "");
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cdesc_entry"));
    gtk_entry_set_text ((GtkEntry *) entry, "");
}

void on_prefs_cadd_clicked (GtkButton *button, gpointer user_data)
{
    GtkWidget        *entry;
    GtkTreeIter           iter;
    int            nr_consts;
    char             *name, *value, *desc;
    
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cname_entry"));
    name = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cvalue_entry"));
    value = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cdesc_entry"));
    desc = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    
    if ((strlen(name) == 0) || (strlen(value) == 0) || (strlen(desc) == 0)) {
        g_free (name);
        g_free (value);
        g_free (desc);
        return;
    }
        
    nr_consts = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(prefs_constant_store), NULL);
    constant = (s_constant *) realloc (constant, (nr_consts + 2) * sizeof(s_constant));
    constant[nr_consts + 1].name = NULL;
    
    constant[nr_consts].name = name;
    constant[nr_consts].value = value;
    constant[nr_consts].desc = desc;
    
    gtk_list_store_append (prefs_constant_store, &iter);    
    gtk_list_store_set (prefs_constant_store, &iter, 
        CONST_NAME_COLUMN, constant[nr_consts].name, 
        CONST_VALUE_COLUMN, constant[nr_consts].value, 
        CONST_DESC_COLUMN, constant[nr_consts].desc, 
        -1);
}

void on_prefs_cdelete_clicked (GtkButton *button, gpointer user_data)
{
    GtkTreePath        *path;
    int            index, counter, nr_consts;
    GtkTreeIter        current_list_iter;
        
    if (!gtk_tree_selection_get_selected (gtk_tree_view_get_selection(
        (GtkTreeView *)gtk_builder_get_object (prefs_xml, "constant_treeview")),
        NULL, &current_list_iter)) return;
    
    nr_consts = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(prefs_constant_store), NULL);
    path = gtk_tree_model_get_path (GTK_TREE_MODEL (prefs_constant_store), &current_list_iter);
    index = *(gtk_tree_path_get_indices (path));

    gtk_list_store_remove (prefs_constant_store, &current_list_iter);
    on_prefs_cclear_clicked (NULL, NULL);

    for (counter = index; counter < (nr_consts - 1); counter++)
        memcpy (&constant[counter], &constant[counter+1], sizeof(s_constant));
    
    nr_consts--;
    constant = (s_constant *) realloc (constant, (nr_consts + 1) * sizeof(s_constant));
    
    constant[nr_consts].name = NULL;
}

void on_prefs_cupdate_clicked (GtkButton *button, gpointer user_data)
{
    GtkWidget    *entry;
    GtkTreePath    *path;
    int        index;
    GtkTreeIter     current_list_iter;
    
    if (!gtk_tree_selection_get_selected (gtk_tree_view_get_selection(
        (GtkTreeView *)gtk_builder_get_object (prefs_xml, "constant_treeview")),
        NULL, &current_list_iter)) return;
    
    path = gtk_tree_model_get_path (GTK_TREE_MODEL (prefs_constant_store), 
        &current_list_iter);
    index = *(gtk_tree_path_get_indices (path));
    
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cname_entry"));
    constant[index].name = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cvalue_entry"));
    constant[index].value = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    entry = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_cdesc_entry"));
    constant[index].desc = g_strdup (gtk_entry_get_text ((GtkEntry *) entry));
    
    gtk_list_store_set (prefs_constant_store, &current_list_iter, 
        CONST_NAME_COLUMN, constant[index].name, 
        CONST_VALUE_COLUMN, constant[index].value, 
        CONST_DESC_COLUMN, constant[index].desc, 
        -1);
}

void on_prefs_hex_bits_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{    
    prefs.hex_bits = (int) gtk_spin_button_get_value (spinbutton);
}

void on_prefs_hex_signed_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    prefs.hex_signed = gtk_toggle_button_get_active (togglebutton);
}

void on_prefs_oct_bits_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    prefs.oct_bits = (int) gtk_spin_button_get_value (spinbutton);
}

void on_prefs_oct_signed_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    prefs.oct_signed = gtk_toggle_button_get_active (togglebutton);
}

void on_prefs_bin_bits_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    prefs.bin_bits = (int) gtk_spin_button_get_value (spinbutton);
}


void on_prefs_bin_signed_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    prefs.bin_signed = gtk_toggle_button_get_active (togglebutton);
}

void on_prefs_bin_fixed_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    GtkWidget    *w;
    
    prefs.bin_fixed = gtk_toggle_button_get_active (togglebutton);
    w = GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_bin_length"));
    gtk_widget_set_sensitive (w, prefs.bin_fixed);
}

void on_prefs_bin_length_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    prefs.bin_length = (int) gtk_spin_button_get_value (spinbutton);
}


void on_prefs_number_combo_changed(GtkComboBox *widget, gpointer user_data)
{
	GtkTreeIter iter;
	if (!gtk_combo_box_get_active_iter(widget, &iter)) {
		fprintf (stderr, _("[%s] on_prefs_number_combo_changed failed to retrieve iter. %s\n"), PROG_NAME, BUG_REPORT);
		return;
	}
	
	int selID;
	gtk_tree_model_get(gtk_combo_box_get_model(widget), &iter, 1, &selID, -1);

    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_dec")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_hex")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_oct")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_bin")));
    
    if (selID == 0) gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_dec")));
    else if (selID == 1) gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_hex")));
    else if (selID == 2) gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_oct")));
    else if (selID == 3) gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_bin")));
}

void on_prefs_menu_dec_activate (GtkMenuItem *menuitem, gpointer user_data)
{
    gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_dec")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_hex")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_oct")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_bin")));
}
            
void on_prefs_menu_hex_activate (GtkMenuItem *menuitem, gpointer user_data)
{
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_dec")));
    gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_hex")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_oct")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_bin")));
}
            
void on_prefs_menu_oct_activate (GtkMenuItem *menuitem, gpointer user_data)
{
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_dec")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_hex")));
    gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_oct")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_bin")));
}
            
void on_prefs_menu_bin_activate (GtkMenuItem *menuitem, gpointer user_data)
{
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_dec")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_hex")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_oct")));
    gtk_widget_show (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_vbox_bin")));
}

void on_prefs_dec_sep_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    prefs.dec_sep = gtk_toggle_button_get_active (togglebutton);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_dec_sep_char_label")), prefs.dec_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_dec_sep_char")), prefs.dec_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_dec_sep_length_label")), prefs.dec_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_dec_sep_length")), prefs.dec_sep);
    display_result_getset();
}

void on_prefs_hex_sep_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    prefs.hex_sep = gtk_toggle_button_get_active (togglebutton);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_hex_sep_char_label")), prefs.hex_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_hex_sep_char")), prefs.hex_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_hex_sep_length_label")), prefs.hex_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_hex_sep_length")), prefs.hex_sep);
    display_result_getset();
}

void on_prefs_oct_sep_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    prefs.oct_sep = gtk_toggle_button_get_active (togglebutton);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_oct_sep_char_label")), prefs.oct_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_oct_sep_char")), prefs.oct_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_oct_sep_length_label")), prefs.oct_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_oct_sep_length")), prefs.oct_sep);
    display_result_getset();
}

void on_prefs_bin_sep_toggled (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    prefs.bin_sep = gtk_toggle_button_get_active (togglebutton);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_bin_sep_char_label")), prefs.bin_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_bin_sep_char")), prefs.bin_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_bin_sep_length_label")), prefs.bin_sep);
    gtk_widget_set_sensitive (GTK_WIDGET(gtk_builder_get_object (prefs_xml, "prefs_bin_sep_length")), prefs.bin_sep);
    display_result_getset();
}

void on_prefs_dec_sep_length_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    prefs.dec_sep_length = (int) gtk_spin_button_get_value (spinbutton);
    display_result_getset();
}

void on_prefs_hex_sep_length_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    prefs.hex_sep_length = (int) gtk_spin_button_get_value (spinbutton);
    display_result_getset();
}

void on_prefs_oct_sep_length_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    prefs.oct_sep_length = (int) gtk_spin_button_get_value (spinbutton);
    display_result_getset();
}

void on_prefs_bin_sep_length_value_changed (GtkSpinButton *spinbutton,
                    GtkScrollType arg1,
                    gpointer user_data)
{
    prefs.bin_sep_length = (int) gtk_spin_button_get_value (spinbutton);
    display_result_getset();
}

void on_prefs_dec_sep_char_changed (GtkEditable *editable,
                                            gpointer user_data)
{
    prefs_sep_char_changed (editable, prefs.dec_sep_char, CS_DEC);
}

void on_prefs_hex_sep_char_changed (GtkEditable *editable,
                                            gpointer user_data)
{
    prefs_sep_char_changed (editable, prefs.hex_sep_char, CS_HEX);
}

void on_prefs_oct_sep_char_changed (GtkEditable *editable,
                                            gpointer user_data)
{
    prefs_sep_char_changed (editable, prefs.oct_sep_char, CS_OCT);
}

void on_prefs_bin_sep_char_changed (GtkEditable *editable,
                                            gpointer user_data)
{
    prefs_sep_char_changed (editable, prefs.bin_sep_char, CS_BIN);
}

void on_togglebutton_released (GtkToggleButton *togglebutton, 
                    gpointer user_data)
{
    gtk_toggle_button_set_active (togglebutton, FALSE);
}

/* this function was the signal handler for 'check-resize' of main_window.
 * I see no reason to have it further. so i unlinked it. the code is here
 * if we need it some later time.
 */
void on_main_window_check_resize (GtkContainer *container,
                                            gpointer user_data)
{
    fprintf (stderr, _("[%s] on_main_window_check_resize should not get called. %s\n"), PROG_NAME, BUG_REPORT);
    
    static gboolean        itsme=FALSE;
    
    /* only in classic views this function may take effect */
    if (prefs.mode == PAPER_MODE) return;
    /* is there a nicer way to to this? */
    if (itsme) {
        itsme = FALSE;
        return;
    }
    gtk_window_resize ((GtkWindow *)gtk_widget_get_toplevel((GtkWidget *)container), 1, 1);
    itsme = TRUE;
}

void on_finite_stack_size_clicked (GtkRadioButton *rb, gpointer user_data)
{
    prefs.stack_size = RPN_FINITE_STACK;
    rpn_stack_set_size (prefs.stack_size);
}

void on_infinite_stack_size_clicked (GtkRadioButton *rb, gpointer user_data)
{
    prefs.stack_size = RPN_INFINITE_STACK;
    rpn_stack_set_size (prefs.stack_size);
}

gboolean on_button_press_event (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    GtkWidget    *menu;
    
    if (event->button != 3) return FALSE;
    menu = ui_right_mouse_menu_create ();
    gtk_menu_popup ((GtkMenu *) menu, NULL, NULL, NULL, NULL, 3, event->time);
    return FALSE;
}

void on_formula_entry_activate (GtkEntry *entry, gpointer user_data)
{
    s_flex_parser_result     result;
    
    result = flex_parser(gtk_entry_get_text(entry));
    ui_formula_entry_state (result.error);
    if (!result.error) display_result_set_double (result.value, current_status.number);
}

void on_formula_entry_changed (GtkEditable *editable, gpointer user_data)
{
    ui_formula_entry_state(FALSE);
}

void on_paper_entry_activate (GtkWidget *activated_widget, gpointer user_data)
{
    s_flex_parser_result     result;
    GtkEntry                *entry;
    GtkTreeView                *tree_view;
    GtkListStore            *paper_store;
    GtkTreeIter               iter;
    char                    *escaped_input_string, *result_string, *markup_result_string;
    GtkTreePath*             last_row_path;
    
    if (!GTK_IS_ENTRY(activated_widget))
        entry = GTK_ENTRY(gtk_builder_get_object (view_xml, "paper_entry"));
    else
        entry = GTK_ENTRY(activated_widget);
    
    if (strcmp(gtk_entry_get_text(entry), "") == 0) return;
    /* result.error result.value */
    result = flex_parser(gtk_entry_get_text(entry));
    
    /* add to tree view */
    tree_view = GTK_TREE_VIEW(gtk_builder_get_object (view_xml, "paper_treeview"));
    paper_store = GTK_LIST_STORE(gtk_tree_view_get_model(tree_view));
    gtk_list_store_append (paper_store, &iter);
    escaped_input_string = g_markup_escape_text(gtk_entry_get_text(entry), -1);
    gtk_list_store_set (paper_store, &iter, 0, escaped_input_string, 1, 0.0, 2, NULL, -1);
    g_free(escaped_input_string);
    
    gtk_list_store_append (paper_store, &iter);
    result_string = get_display_number_string(result.value, current_status.number);
    markup_result_string = g_markup_printf_escaped ("<b>%s</b>", result_string);
    
    g_free(result_string);
    if (result.error)
        gtk_list_store_set (paper_store, &iter, 0, "Syntax Error", 1, 1.0, 2, "red", -1);
    else
        gtk_list_store_set (paper_store, &iter, 0, markup_result_string, 1, 1.0, 2, NULL, -1);
    g_free(markup_result_string);
    
    /* scroll to last row */
    last_row_path = gtk_tree_model_get_path (gtk_tree_view_get_model(tree_view), &iter);
    gtk_tree_view_scroll_to_cell(tree_view, last_row_path, NULL, FALSE, 0., 0.);
    
    /* clear entry */
    gtk_entry_set_text (entry, "");
}

gboolean paper_tree_view_selection_changed_cb (GtkWidget *widget,
                                            GdkEventButton *event,
                                            gpointer user_data)
{
    GtkTreeModel         *model;
    char             *string, *stripped_string;
    GtkWidget        *entry;
    GtkTreeIter         current_list_iter;
    int            position;
    GtkTreeSelection    *select;
    
    if ((event->type == GDK_2BUTTON_PRESS) && (event->button == 1)) {
        select = gtk_tree_view_get_selection (GTK_TREE_VIEW (widget));
        if (gtk_tree_selection_get_selected (select, &model, &current_list_iter)) {
            gtk_tree_model_get (model, &current_list_iter, 0, &string, -1);
            stripped_string = g_strdup(string);
            pango_parse_markup (string, -1, 0, NULL, &stripped_string, NULL, NULL);
            g_free (string);
            entry = GTK_WIDGET(gtk_builder_get_object (view_xml, "paper_entry"));
            position = gtk_editable_get_position (GTK_EDITABLE(entry));
            gtk_editable_insert_text (GTK_EDITABLE (entry), stripped_string, strlen(stripped_string), &position);
            /* set position after currently inserted text */
            gtk_editable_set_position (GTK_EDITABLE(entry), position);
            g_free (stripped_string);
        }
    }
    /* return FALSE to let other process this signal, too */
    return FALSE;
}

#if !GTK_CHECK_VERSION(2, 18, 0)

#define gtk_widget_get_sensitive(widget)    GTK_WIDGET_SENSITIVE(widget)

#endif

gboolean on_button_can_activate_accel (GtkWidget *widget, guint signal_id, gpointer user_data)
{
    if (!gtk_widget_get_sensitive(widget)) return FALSE;
    if (strcmp("clicked", g_signal_name(signal_id)) == 0) return TRUE;
    return FALSE;
}

gboolean on_menuitem_can_activate_accel (GtkWidget *widget, guint signal_id, gpointer user_data)
{
    if (!gtk_widget_get_sensitive(widget)) return FALSE;
    if (strcmp("activate", g_signal_name(signal_id)) == 0) return TRUE;
    return FALSE;
}

/* This callback is connected to the "Event" event of the main window. Every
 * event in gtk triggers three events: this general "Event", the more specific
 * event and the general "Event-after". If we return TRUE, processing of the
 * event stops. For GTK2 we use the key_snooper installed in main.c.
 */
gboolean on_button_event(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
#if GTK_CHECK_VERSION(3, 0, 0)
    /* do all cheap checks first before calling expensive formula_entry_is_active */
    if ((current_status.notation == CS_FORMULA) && (event->type == GDK_KEY_PRESS)) {
        GdkEventKey *key_event = (GdkEventKey *) event;
        /* try to rule out some obvious key presses */
        if (key_event->state & GDK_SUPER_MASK ||
             key_event->state & GDK_HYPER_MASK ||
             key_event->state & GDK_META_MASK ||
             key_event->state & GDK_CONTROL_MASK) return FALSE;
        GtkWidget *formula_entry = formula_entry_is_active(widget);
        if (formula_entry) {
            gtk_widget_event (formula_entry, event);
            return TRUE;
        }
    }
#endif
    return FALSE;
}

/* END */
