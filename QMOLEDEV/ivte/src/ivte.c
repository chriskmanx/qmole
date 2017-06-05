/* Forked from sakura 2.0.1, http://www.pleyades.net/david/sakura.php
 * Copyright (C) 2006-2008  David Gómez <david@pleyades.net>
 * Copyright (C) 2008-2012  Wen-Yen Chuang <caleb AT calno DOT com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <ctype.h>
#include <dlfcn.h>
#include <glib.h>
#ifndef G_CONST_RETURN
#define G_CONST_RETURN const
#endif
#include <gtk/gtk.h>
#if GTK_CHECK_VERSION(2,90,7)
#include <gdk/gdkkeysyms-compat.h>
#endif
#include <gdk/gdkkeysyms.h>
#include <gdk/gdkx.h>
#include <libintl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <vte/vte.h>
#include <vte/vteaccess.h>

#ifndef VTE_CHECK_VERSION
#define VTE_CHECK_VERSION(x,y,z) FALSE
#endif

#ifndef VTE_ERASE_TTY
#define VTE_ERASE_TTY VTE_ERASE_AUTO
#endif

#define AUTO            VTE_ERASE_AUTO
#define BACKSPACE       VTE_ERASE_ASCII_BACKSPACE
#define DELETE          VTE_ERASE_ASCII_DELETE
#define DELETE_SEQUENCE VTE_ERASE_DELETE_SEQUENCE
#define ERASE_TTY       VTE_ERASE_TTY

#define BLOCK     VTE_CURSOR_SHAPE_BLOCK
#define IBEAM     VTE_CURSOR_SHAPE_IBEAM
#define UNDERLINE VTE_CURSOR_SHAPE_UNDERLINE

#define LINUX           1
#define RXVT            2
#define TANGO           3
#define VTE_FIXED       4
#define XTERM           5
#define ZENBURN_DARK    6
#define ZENBURN         7
#define SOLARIZED_DARK  8
#define SOLARIZED_LIGHT 9

#define LEFT   0
#define RIGHT  1
#define TOP    2
#define BOTTOM 3
#define OFF_L  4
#define OFF_R  5

#define ALT(x) (((event->state & GDK_MOD1_MASK) == GDK_MOD1_MASK) && (event->keyval == (x)))
#define ALT_SHIFT(x) (((event->state & (GDK_MOD1_MASK | GDK_SHIFT_MASK)) == (GDK_MOD1_MASK | GDK_SHIFT_MASK)) && (event->keyval == (x)))
#define CTRL(x) (((event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) && (event->keyval == (x)))
#define CTRL_ALT(x) (((event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK)) == (GDK_CONTROL_MASK | GDK_MOD1_MASK)) && (event->keyval == (x)))
#define CTRL_ALT_SHIFT(x) (((event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK | GDK_SHIFT_MASK)) == (GDK_CONTROL_MASK | GDK_MOD1_MASK | GDK_SHIFT_MASK)) && (event->keyval == (x)))
#define CTRL_SHIFT(x) (((event->state & (GDK_CONTROL_MASK | GDK_SHIFT_MASK)) == (GDK_CONTROL_MASK | GDK_SHIFT_MASK)) && (event->keyval == (x)))
#define SHIFT(x) (((event->state & GDK_SHIFT_MASK) == GDK_SHIFT_MASK) && (event->keyval == (x)))

#include "custom.h"
#include "ivte.h"

#if GTK_CHECK_VERSION(3,3,4) && !defined(USE_GTK_GRID) && defined(GTK_DISABLE_DEPRECATED)
#define USE_GTK_GRID
#endif

#ifdef RULE_THEM_ALL
#ifndef GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
#define GTK_STYLE_PROVIDER_PRIORITY_APPLICATION 600
#endif
typedef struct _GdkColormap GdkColormap;
typedef struct _GdkDrawable GdkDrawable;
typedef struct _GtkCssProvider GtkCssProvider;
typedef struct _GtkFontSelectionDialog GtkFontSelectionDialog;
typedef struct _GtkScrollable GtkScrollable;
typedef struct _GtkStyleProvider GtkStyleProvider;
#undef GTK_BOX
#define GTK_BOX (GtkBox*)
#undef GTK_BUTTON
#define GTK_BUTTON (GtkButton*)
#undef GTK_COLOR_SELECTION
#define GTK_COLOR_SELECTION (GtkColorSelection*)
#undef GTK_COLOR_SELECTION_DIALOG
#define GTK_COLOR_SELECTION_DIALOG (GtkColorSelectionDialog*)
#undef GTK_CONTAINER
#define GTK_CONTAINER (GtkContainer*)
#undef GTK_DIALOG
#define GTK_DIALOG (GtkDialog*)
#undef GTK_ENTRY
#define GTK_ENTRY (GtkEntry*)
#undef GTK_FONT_SELECTION_DIALOG
#define GTK_FONT_SELECTION_DIALOG (GtkFontSelectionDialog*)
#undef GTK_IMAGE_MENU_ITEM
#define GTK_IMAGE_MENU_ITEM (GtkImageMenuItem*)
#undef GTK_LABEL
#define GTK_LABEL (GtkLabel*)
#undef GTK_MENU
#define GTK_MENU (GtkMenu*)
#undef GTK_MENU_ITEM
#define GTK_MENU_ITEM (GtkMenuItem*)
#undef GTK_MENU_SHELL
#define GTK_MENU_SHELL (GtkMenuShell*)
#undef GTK_NOTEBOOK
#define GTK_NOTEBOOK (GtkNotebook*)
#undef GTK_RANGE
#define GTK_RANGE (GtkRange*)
#undef GTK_SCROLLABLE
#define GTK_SCROLLABLE (GtkScrollable*)
#undef GTK_STATUSBAR
#define GTK_STATUSBAR (GtkStatusbar*)
#undef GTK_STYLE_PROVIDER
#define GTK_STYLE_PROVIDER (GtkStyleProvider*)
#undef GTK_WIDGET
#define GTK_WIDGET (GtkWidget*)
#undef GTK_WINDOW
#define GTK_WINDOW (GtkWindow*)
#undef USE_GTK_GRID
#undef VTE_TERMINAL
#define VTE_TERMINAL (VteTerminal*)
#define gdk_color_parse (*p_gdk_color_parse)
#define gdk_device_warp (*p_gdk_device_warp)
#define gdk_disable_multidevice (*p_gdk_disable_multidevice)
#define gdk_display_get_default (*p_gdk_display_get_default)
#define gdk_display_get_default_screen (*p_gdk_display_get_default_screen)
#define gdk_display_warp_pointer (*p_gdk_display_warp_pointer)
#define gdk_screen_get_rgba_colormap (*p_gdk_screen_get_rgba_colormap)
#define gdk_screen_get_rgba_visual (*p_gdk_screen_get_rgba_visual)
#define gdk_window_get_display (*p_gdk_window_get_display)
#define gdk_x11_drawable_get_xid (*p_gdk_x11_drawable_get_xid)
#define gdk_x11_window_get_xid (*p_gdk_x11_window_get_xid)
#define gtk_box_pack_start (*p_gtk_box_pack_start)
#define gtk_button_new (*p_gtk_button_new)
#define gtk_button_set_focus_on_click (*p_gtk_button_set_focus_on_click)
#define gtk_button_set_image (*p_gtk_button_set_image)
#define gtk_button_set_relief (*p_gtk_button_set_relief)
#define gtk_clipboard_get (*p_gtk_clipboard_get)
#define gtk_clipboard_get_for_display (*p_gtk_clipboard_get_for_display)
#define gtk_clipboard_set_text (*p_gtk_clipboard_set_text)
#define gtk_clipboard_wait_is_text_available (*p_gtk_clipboard_wait_is_text_available)
#define gtk_color_selection_dialog_get_color_selection (*p_gtk_color_selection_dialog_get_color_selection)
#define gtk_color_selection_dialog_new (*p_gtk_color_selection_dialog_new)
#define gtk_color_selection_get_current_color (*p_gtk_color_selection_get_current_color)
#define gtk_color_selection_set_current_color (*p_gtk_color_selection_set_current_color)
#define gtk_container_add (*p_gtk_container_add)
#define gtk_container_child_set (*p_gtk_container_child_set)
#define gtk_css_provider_load_from_data (*p_gtk_css_provider_load_from_data)
#define gtk_css_provider_new (*p_gtk_css_provider_new)
#define gtk_dialog_get_content_area (*p_gtk_dialog_get_content_area)
#define gtk_dialog_new_with_buttons (*p_gtk_dialog_new_with_buttons)
#define gtk_dialog_run (*p_gtk_dialog_run)
#define gtk_dialog_set_default_response (*p_gtk_dialog_set_default_response)
#define gtk_entry_get_text (*p_gtk_entry_get_text)
#define gtk_entry_new (*p_gtk_entry_new)
#define gtk_entry_set_activates_default (*p_gtk_entry_set_activates_default)
#define gtk_entry_set_text (*p_gtk_entry_set_text)
#define gtk_font_selection_dialog_get_font_name (*p_gtk_font_selection_dialog_get_font_name)
#define gtk_font_selection_dialog_new (*p_gtk_font_selection_dialog_new)
#define gtk_font_selection_dialog_set_font_name (*p_gtk_font_selection_dialog_set_font_name)
#define gtk_hbox_new (*p_gtk_hbox_new)
#define gtk_hscale_new_with_range (*p_gtk_hscale_new_with_range)
#define gtk_image_menu_item_new_from_stock (*p_gtk_image_menu_item_new_from_stock)
#define gtk_image_menu_item_new_with_label (*p_gtk_image_menu_item_new_with_label)
#define gtk_image_menu_item_new_with_mnemonic (*p_gtk_image_menu_item_new_with_mnemonic)
#define gtk_image_menu_item_set_image (*p_gtk_image_menu_item_set_image)
#define gtk_image_new_from_stock (*p_gtk_image_new_from_stock)
#define gtk_init (*p_gtk_init)
#define gtk_label_get_text (*p_gtk_label_get_text)
#define gtk_label_new (*p_gtk_label_new)
#define gtk_main (*p_gtk_main)
#define gtk_main_quit (*p_gtk_main_quit)
#define gtk_menu_item_new_with_label (*p_gtk_menu_item_new_with_label)
#define gtk_menu_item_new_with_mnemonic (*p_gtk_menu_item_new_with_mnemonic)
#define gtk_menu_item_set_submenu (*p_gtk_menu_item_set_submenu)
#define gtk_menu_new (*p_gtk_menu_new)
#define gtk_menu_popup (*p_gtk_menu_popup)
#define gtk_menu_shell_append (*p_gtk_menu_shell_append)
#define gtk_menu_shell_prepend (*p_gtk_menu_shell_prepend)
#define gtk_notebook_append_page (*p_gtk_notebook_append_page)
#define gtk_notebook_get_current_page (*p_gtk_notebook_get_current_page)
#define gtk_notebook_get_n_pages (*p_gtk_notebook_get_n_pages)
#define gtk_notebook_get_nth_page (*p_gtk_notebook_get_nth_page)
#define gtk_notebook_get_show_tabs (*p_gtk_notebook_get_show_tabs)
#define gtk_notebook_get_tab_label (*p_gtk_notebook_get_tab_label)
#define gtk_notebook_new (*p_gtk_notebook_new)
#define gtk_notebook_popup_enable (*p_gtk_notebook_popup_enable)
#define gtk_notebook_prepend_page (*p_gtk_notebook_prepend_page)
#define gtk_notebook_remove_page (*p_gtk_notebook_remove_page)
#define gtk_notebook_set_current_page (*p_gtk_notebook_set_current_page)
#define gtk_notebook_set_scrollable (*p_gtk_notebook_set_scrollable)
#define gtk_notebook_set_show_border (*p_gtk_notebook_set_show_border)
#define gtk_notebook_set_show_tabs (*p_gtk_notebook_set_show_tabs)
#define gtk_notebook_set_tab_border (*p_gtk_notebook_set_tab_border)
#define gtk_notebook_set_tab_hborder (*p_gtk_notebook_set_tab_hborder)
#define gtk_notebook_set_tab_label (*p_gtk_notebook_set_tab_label)
#define gtk_notebook_set_tab_label_text (*p_gtk_notebook_set_tab_label_text)
#define gtk_notebook_set_tab_pos (*p_gtk_notebook_set_tab_pos)
#define gtk_notebook_set_tab_reorderable (*p_gtk_notebook_set_tab_reorderable)
#define gtk_notebook_set_tab_vborder (*p_gtk_notebook_set_tab_vborder)
#define gtk_range_get_value (*p_gtk_range_get_value)
#define gtk_range_set_value (*p_gtk_range_set_value)
#define gtk_rc_parse_string (*p_gtk_rc_parse_string)
#define gtk_scale_new_with_range (*p_gtk_scale_new_with_range)
#define gtk_scrollable_get_vadjustment (*p_gtk_scrollable_get_vadjustment)
#define gtk_scrollbar_new (*p_gtk_scrollbar_new)
#define gtk_separator_menu_item_new (*p_gtk_separator_menu_item_new)
#define gtk_settings_get_default (*p_gtk_settings_get_default)
#define gtk_statusbar_new (*p_gtk_statusbar_new)
#define gtk_statusbar_push (*p_gtk_statusbar_push)
#define gtk_style_context_add_provider_for_screen (*p_gtk_style_context_add_provider_for_screen)
#define gtk_test_widget_click (*p_gtk_test_widget_click)
#define gtk_test_widget_send_key (*p_gtk_test_widget_send_key)
#define gtk_vbox_new (*p_gtk_vbox_new)
#define gtk_vscrollbar_new (*p_gtk_vscrollbar_new)
#define gtk_widget_destroy (*p_gtk_widget_destroy)
#define gtk_widget_get_screen (*p_gtk_widget_get_screen)
#define gtk_widget_get_window (*p_gtk_widget_get_window)
#define gtk_widget_hide (*p_gtk_widget_hide)
#define gtk_widget_realize (*p_gtk_widget_realize)
#define gtk_widget_set_can_focus (*p_gtk_widget_set_can_focus)
#define gtk_widget_set_colormap (*p_gtk_widget_set_colormap)
#define gtk_widget_set_hexpand (*p_gtk_widget_set_hexpand)
#define gtk_widget_set_sensitive (*p_gtk_widget_set_sensitive)
#define gtk_widget_set_size_request (*p_gtk_widget_set_size_request)
#define gtk_widget_set_vexpand (*p_gtk_widget_set_vexpand)
#define gtk_widget_set_visual (*p_gtk_widget_set_visual)
#define gtk_widget_show (*p_gtk_widget_show)
#define gtk_widget_show_all (*p_gtk_widget_show_all)
#define gtk_widget_style_get (*p_gtk_widget_style_get)
#define gtk_window_maximize (*p_gtk_window_maximize)
#define gtk_window_new (*p_gtk_window_new)
#define gtk_window_parse_geometry (*p_gtk_window_parse_geometry)
#define gtk_window_resize (*p_gtk_window_resize)
#define gtk_window_resize_grip_is_visible (*p_gtk_window_resize_grip_is_visible)
#define gtk_window_set_decorated (*p_gtk_window_set_decorated)
#define gtk_window_set_focus (*p_gtk_window_set_focus)
#define gtk_window_set_has_resize_grip (*p_gtk_window_set_has_resize_grip)
#define gtk_window_set_icon_from_file (*p_gtk_window_set_icon_from_file)
#define gtk_window_set_keep_above (*p_gtk_window_set_keep_above)
#define gtk_window_set_keep_below (*p_gtk_window_set_keep_below)
#define gtk_window_set_skip_pager_hint (*p_gtk_window_set_skip_pager_hint)
#define gtk_window_set_skip_taskbar_hint (*p_gtk_window_set_skip_taskbar_hint)
#define gtk_window_set_title (*p_gtk_window_set_title)
#define gtk_window_set_type_hint (*p_gtk_window_set_type_hint)
#define gtk_window_set_urgency_hint (*p_gtk_window_set_urgency_hint)
#define gtk_window_set_wmclass (*p_gtk_window_set_wmclass)
#define gtk_window_unmaximize (*p_gtk_window_unmaximize)
#define vte_terminal_accessible_new (*p_vte_terminal_accessible_new)
#define vte_terminal_copy_clipboard (*p_vte_terminal_copy_clipboard)
#define vte_terminal_fork_command (*p_vte_terminal_fork_command)
#define vte_terminal_fork_command_full (*p_vte_terminal_fork_command_full)
#define vte_terminal_get_adjustment (*p_vte_terminal_get_adjustment)
#define vte_terminal_get_char_height (*p_vte_terminal_get_char_height)
#define vte_terminal_get_char_width (*p_vte_terminal_get_char_width)
#define vte_terminal_get_encoding (*p_vte_terminal_get_encoding)
#define vte_terminal_get_has_selection (*p_vte_terminal_get_has_selection)
#define vte_terminal_get_window_title (*p_vte_terminal_get_window_title)
#define vte_terminal_im_append_menuitems (*p_vte_terminal_im_append_menuitems)
#define vte_terminal_match_add_gregex (*p_vte_terminal_match_add_gregex)
#define vte_terminal_match_check (*p_vte_terminal_match_check)
#define vte_terminal_new (*p_vte_terminal_new)
#define vte_terminal_paste_clipboard (*p_vte_terminal_paste_clipboard)
#define vte_terminal_reset (*p_vte_terminal_reset)
#define vte_terminal_search_find_next (*p_vte_terminal_search_find_next)
#define vte_terminal_search_find_previous (*p_vte_terminal_search_find_previous)
#define vte_terminal_search_set_gregex (*p_vte_terminal_search_set_gregex)
#define vte_terminal_search_set_wrap_around (*p_vte_terminal_search_set_wrap_around)
#define vte_terminal_select_all (*p_vte_terminal_select_all)
#define vte_terminal_set_allow_bold (*p_vte_terminal_set_allow_bold)
#define vte_terminal_set_audible_bell (*p_vte_terminal_set_audible_bell)
#define vte_terminal_set_background_image_file (*p_vte_terminal_set_background_image_file)
#define vte_terminal_set_background_saturation (*p_vte_terminal_set_background_saturation)
#define vte_terminal_set_background_tint_color (*p_vte_terminal_set_background_tint_color)
#define vte_terminal_set_background_transparent (*p_vte_terminal_set_background_transparent)
#define vte_terminal_set_backspace_binding (*p_vte_terminal_set_backspace_binding)
#define vte_terminal_set_color_background (*p_vte_terminal_set_color_background)
#define vte_terminal_set_color_bold (*p_vte_terminal_set_color_bold)
#define vte_terminal_set_color_cursor (*p_vte_terminal_set_color_cursor)
#define vte_terminal_set_color_dim (*p_vte_terminal_set_color_dim)
#define vte_terminal_set_color_foreground (*p_vte_terminal_set_color_foreground)
#define vte_terminal_set_color_highlight (*p_vte_terminal_set_color_highlight)
#define vte_terminal_set_colors (*p_vte_terminal_set_colors)
#define vte_terminal_set_cursor_blink_mode (*p_vte_terminal_set_cursor_blink_mode)
#define vte_terminal_set_cursor_shape (*p_vte_terminal_set_cursor_shape)
#define vte_terminal_set_default_colors (*p_vte_terminal_set_default_colors)
#define vte_terminal_set_delete_binding (*p_vte_terminal_set_delete_binding)
#define vte_terminal_set_emulation (*p_vte_terminal_set_emulation)
#define vte_terminal_set_encoding (*p_vte_terminal_set_encoding)
#define vte_terminal_set_font_from_string (*p_vte_terminal_set_font_from_string)
#define vte_terminal_set_font_from_string_full (*p_vte_terminal_set_font_from_string_full)
#define vte_terminal_set_mouse_autohide (*p_vte_terminal_set_mouse_autohide)
#define vte_terminal_set_opacity (*p_vte_terminal_set_opacity)
#define vte_terminal_set_scroll_background (*p_vte_terminal_set_scroll_background)
#define vte_terminal_set_scroll_on_keystroke (*p_vte_terminal_set_scroll_on_keystroke)
#define vte_terminal_set_scroll_on_output (*p_vte_terminal_set_scroll_on_output)
#define vte_terminal_set_scrollback_lines (*p_vte_terminal_set_scrollback_lines)
#define vte_terminal_set_size (*p_vte_terminal_set_size)
#define vte_terminal_set_visible_bell (*p_vte_terminal_set_visible_bell)
#define vte_terminal_set_word_chars (*p_vte_terminal_set_word_chars)
#endif

#if !GTK_CHECK_VERSION(2,13,0)
#undef HOTKEY_MIMIC_SCROLL_UP
#undef HOTKEY_MIMIC_SCROLL_DOWN
#undef HOTKEY_SCROLL_ONE_PAGE_UP
#undef HOTKEY_SCROLL_ONE_PAGE_DOWN
#undef ONLY_ONE_MENU_ITEM
#endif

#if !GTK_CHECK_VERSION(2,13,4)
#define gtk_color_selection_dialog_get_color_selection(x) (x)->colorsel
#define gtk_dialog_get_content_area(x) (x)->vbox
#endif

#if !GTK_CHECK_VERSION(2,17,5)
#define gtk_widget_set_can_focus(x,y);
#endif

#if !defined(RULE_THEM_ALL) && !GTK_CHECK_VERSION(2,90,0)
#define gtk_scrollbar_new(x,y) gtk_vscrollbar_new(y)
#define gtk_scale_new_with_range(w,x,y,z) gtk_hscale_new_with_range(x,y,z)
#endif

#ifndef GTK_FONT_SELECTION_DIALOG
#define GTK_FONT_SELECTION_DIALOG
#endif

#if !defined(RULE_THEM_ALL) && GTK_CHECK_VERSION(2,91,0)
#define gtk_widget_set_colormap gtk_widget_set_visual
#define gdk_screen_get_rgba_colormap gdk_screen_get_rgba_visual
#endif

#if GTK_CHECK_VERSION(2,91,1)
#undef GTK_HAS_RESIZE_GRIP
#define GTK_HAS_RESIZE_GRIP
#endif

#if !defined(RULE_THEM_ALL) && !defined(GTK_HAS_RESIZE_GRIP)
#define gtk_window_set_has_resize_grip(x,y)
#endif

#if !defined(RULE_THEM_ALL) && !GTK_CHECK_VERSION(2,91,1)
#define gtk_widget_set_hexpand(x,y);
#define gtk_widget_set_vexpand(x,y);
#endif

#if !defined(RULE_THEM_ALL) && !GTK_CHECK_VERSION(2,91,2)
#undef GTK_SCROLLABLE
#define GTK_SCROLLABLE VTE_TERMINAL
#define gtk_scrollable_get_vadjustment vte_terminal_get_adjustment
#endif

#if GTK_CHECK_VERSION(2,91,2) && defined(USE_GTK_GRID)
#undef GTK_BOX
#define GTK_BOX GTK_GRID
#define gtk_hbox_new(x,y) gtk_grid_new()
#define gtk_vbox_new(x,y) gtk_grid_new()
#define gtk_box_pack_start(v,w,x,y,z) gtk_container_add(GTK_CONTAINER(v),w)
#endif

#if !defined(RULE_THEM_ALL) && GTK_CHECK_VERSION(2,91,6)
#define gdk_x11_drawable_get_xid gdk_x11_window_get_xid
#endif

#define IVTE_GET_WINDOW gtk_widget_get_window
#ifdef RULE_THEM_ALL
#undef IVTE_GET_WINDOW
#define IVTE_GET_WINDOW (GdkDrawable*)gtk_widget_get_window
#endif

#if !defined(RULE_THEM_ALL) && !GTK_CHECK_VERSION(2,99,0)
#define gdk_device_warp(w,x,y,z) gdk_display_warp_pointer(display,x,y,z)
#endif

#if !defined(RULE_THEM_ALL) && GTK_CHECK_VERSION(3,1,6) && !defined(USE_GTK_GRID) && defined(GTK_DISABLE_DEPRECATED)
#define gtk_hbox_new (GtkWidget*)gtk_hbox_new
#define gtk_vbox_new (GtkWidget*)gtk_vbox_new
#endif

#if !defined(RULE_THEM_ALL) && GTK_CHECK_VERSION(3,1,90)
#define gtk_font_selection_dialog_new(x) gtk_font_chooser_dialog_new(x,NULL)
#define gtk_font_selection_dialog_get_font_name gtk_font_chooser_get_font
#define gtk_font_selection_dialog_set_font_name gtk_font_chooser_set_font
#undef GTK_FONT_SELECTION_DIALOG
#define GTK_FONT_SELECTION_DIALOG GTK_FONT_CHOOSER
#endif

#if !defined(RULE_THEM_ALL) && GTK_CHECK_VERSION(3,3,16)
#undef GTK_COLOR_SELECTION
#define GTK_COLOR_SELECTION GTK_COLOR_CHOOSER
#define gtk_color_selection_dialog_get_color_selection GTK_COLOR_CHOOSER
#define gtk_color_selection_dialog_new(x) gtk_color_chooser_dialog_new(x,GTK_WINDOW(main_window))
#define gtk_color_selection_get_current_rgba gtk_color_chooser_get_rgba
#define gtk_color_selection_set_current_rgba gtk_color_chooser_set_rgba
#endif

#if !VTE_CHECK_VERSION(0,25,1)
#undef HOTKEY_SEARCH_STRING
#undef HOTKEY_SEARCH_PREVIOUS
#undef HOTKEY_SEARCH_NEXT
#endif

#ifndef INNER_BORDER_H
#define INNER_BORDER_H 2
#endif

#ifndef INNER_BORDER_W
#define INNER_BORDER_W 2
#endif

#ifndef VTE_FUNNY
#define VTE_FUNNY TRUE
#endif

#if !VTE_FUNNY || !VTE_CHECK_VERSION(0,27,1) || !GTK_CHECK_VERSION(2,91,1)
#undef VTE_FUNNY
#endif

#if defined(COLOR_STYLE) && (COLOR_STYLE == VTE_FIXED)
#undef COLOR_STYLE
#endif

#ifndef VTE_FORK_CMD_OLD
#define VTE_FORK_CMD_OLD TRUE
#endif

#if VTE_CHECK_VERSION(0,25,1) && defined(VTE_DISABLE_DEPRECATED)
#undef VTE_FORK_CMD_OLD
#define VTE_FORK_CMD_OLD FALSE
#endif

#ifndef USE_ACCESSIBLE
#define USE_ACCESSIBLE FALSE
#if defined(RULE_THEM_ALL) || VTE_CHECK_VERSION(0,29,0)
#undef USE_ACCESSIBLE
#define USE_ACCESSIBLE TRUE
#endif
#endif

#define VTE_WINDOW_RESIZE(x,y,z) gtk_window_resize(x,y,z)
#if defined(RULE_THEM_ALL) || (VTE_CHECK_VERSION(0,27,1) && GTK_CHECK_VERSION(2,91,1))
#ifndef VTE_FUNNY
#undef VTE_WINDOW_RESIZE
#ifdef RULE_THEM_ALL
#define VTE_WINDOW_RESIZE(x,y,z) if (with_gtk == 2) gtk_window_resize(x,y,z)
#endif
#ifndef RULE_THEM_ALL
#define VTE_WINDOW_RESIZE(x,y,z)
#endif
#endif
#ifndef VTE_COLUMNS
#define VTE_COLUMNS 80
#endif
#ifndef VTE_ROWS
#define VTE_ROWS 24
#endif
#endif

#ifndef VTE_COLUMNS
#define VTE_COLUMNS 0
#endif

#ifndef VTE_ROWS
#define VTE_ROWS 0
#endif

#define GET_CURRENT_TAB(x) term = (struct terminal*)g_object_get_data(G_OBJECT(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), (x))), "current_tab")

#ifndef DEFAULT_COMMAND
#define DEFAULT_COMMAND g_getenv("SHELL")
#endif

#ifndef RECORD_LASTLOG
#define RECORD_LASTLOG TRUE
#endif
#define IVTE_PTY_NO_LASTLOG 0
#if !RECORD_LASTLOG
#undef IVTE_PTY_NO_LASTLOG
#define IVTE_PTY_NO_LASTLOG VTE_PTY_NO_LASTLOG
#endif

#ifndef RECORD_UTMP
#define RECORD_UTMP TRUE
#endif
#define IVTE_PTY_NO_UTMP 0
#if !RECORD_UTMP
#undef IVTE_PTY_NO_UTMP
#define IVTE_PTY_NO_UTMP VTE_PTY_NO_UTMP
#endif

#ifndef RECORD_WTMP
#define RECORD_WTMP TRUE
#endif
#define IVTE_PTY_NO_WTMP 0
#if !RECORD_WTMP
#undef IVTE_PTY_NO_WTMP
#define IVTE_PTY_NO_WTMP VTE_PTY_NO_WTMP
#endif

#define IVTE_PTY_NO_HELPER 0
#if !RECORD_LASTLOG && !RECORD_UTMP && !RECORD_WTMP
#undef IVTE_PTY_NO_HELPER
#define IVTE_PTY_NO_HELPER VTE_PTY_NO_HELPER
#endif

#ifndef LABEL_DEFAULT_ENCODING
#define LABEL_DEFAULT_ENCODING "_Default Encoding"
#endif

#ifndef LABEL_DIALOG_BACKGROUND_TINT
#define LABEL_DIALOG_BACKGROUND_TINT "_Background tint color"
#endif

#ifndef LABEL_DIALOG_CLOSE
#define LABEL_DIALOG_CLOSE "Do you really want to close it?"
#endif

#ifndef LABEL_DIALOG_SEARCH
#define LABEL_DIALOG_SEARCH "Find"
#endif

#ifndef LABEL_MENU_SATURATION
#define LABEL_MENU_SATURATION "_Adjust saturation"
#endif

#ifndef LABEL_MENU_TOGGLE_ANTI_ALIAS
#define LABEL_MENU_TOGGLE_ANTI_ALIAS "_Toggle anti-alias"
#endif

#ifndef LABEL_MENU_TOGGLE_BG
#define LABEL_MENU_TOGGLE_BG "_Toggle background"
#endif

#ifndef LABEL_MENU_TOGGLE_DECORATED
#define LABEL_MENU_TOGGLE_DECORATED "_Toggle window decorated"
#endif

#ifndef LABEL_MENU_TOGGLE_FULLSCREEN
#define LABEL_MENU_TOGGLE_FULLSCREEN "_Toggle fullscreen"
#endif

#ifndef LABEL_MENU_TOGGLE_HOTKEYS
#define LABEL_MENU_TOGGLE_HOTKEYS "_Toggle hotkeys locking"
#endif

#ifndef LABEL_MENU_TOGGLE_ON_TOP
#define LABEL_MENU_TOGGLE_ON_TOP "_Toggle always on top"
#endif

#ifndef LABEL_MENU_TOGGLE_SCROLLBAR
#define LABEL_MENU_TOGGLE_SCROLLBAR "_Toggle scrollbar"
#endif

#ifndef LABEL_MENU_TOGGLE_STATUS_BAR
#define LABEL_MENU_TOGGLE_STATUS_BAR "_Toggle status bar"
#endif

#ifndef LABEL_MENU_TOGGLE_TABBAR
#define LABEL_MENU_TOGGLE_TABBAR "_Toggle tabbar"
#endif

#ifndef LABEL_SUBMENU_ENCODING
#define LABEL_SUBMENU_ENCODING "_Character Encoding"
#endif

#ifndef LABEL_SUBMENU_IME
#define LABEL_SUBMENU_IME "_Input Methods"
#endif

#define MATCH_HTTP_DATA "((f|F)|(h|H)(t|T))(t|T)(p|P)(s|S)?://(([^|.< \t\r\n\\\"]*([.][^|< \t\r\n\\\"])?[^|.< \t\r\n\\\"]*)*[^< \t\r\n,;|\\\"]*[^|.< \t\r\n\\\"])?/*"
#define MATCH_FILE_DATA "(f|F)(i|I)(l|L)(e|E):///(([^|.< \t\r\n\\\"]*([.][^|< \t\r\n\\\"])?[^|.< \t\r\n\\\"]*)*[^< \t\r\n,;|\\\"]*[^|.< \t\r\n\\\"])?/*"
#define MATCH_MAIL_DATA "(m|M)(a|A)(i|I)(l|L)(t|T)(o|O):(([^|.< \t\r\n\\\"]*([.][^|< \t\r\n\\\"])?[^|.< \t\r\n\\\"]*)*@[^< \t\r\n,;|\\\"]*[^|.< \t\r\n\\\"])?/*"

#ifndef CLOSE_DIALOG
#define CLOSE_DIALOG FALSE
#endif

#if CLOSE_DIALOG
#define DEL_TAB del_tab
#define DEL_TAB_DECL del_tab
#endif
#if !CLOSE_DIALOG
#define DEL_TAB(x,y) del_tab()
#define DEL_TAB_DECL(x,y) del_tab(void)
#endif

#ifndef TAB
#define TAB 0
#endif

#if !TAB
#undef COMMAND_TAB_NUMBERS
#undef SHOW_WINDOW_BORDER
#undef TAB_BORDER
#undef TAB_BORDER_VERTICAL
#undef TAB_BORDER_HORIZONTAL
#undef TAB_CLOSE_BUTTON
#undef TAB_EXPANDED_WIDTH
#undef TAB_LABEL
#undef TAB_LABEL_DYNAMIC
#undef TAB_LABEL_CUSTOM
#undef TAB_NEW_PATH_EQUAL_OLD
#undef TAB_REORDERABLE
#undef TAB_SHOW_INFO_AT_TITLE
#undef TABBAR
#undef TABBAR_PLACE
#undef TABBAR_AUTOHIDE
#undef TABBAR_SCROLLABLE
#undef TABBAR_MENU_SELECT_TAB
#define SHOW_WINDOW_BORDER FALSE
#define TABBAR FALSE
#endif

#ifndef SHOW_WINDOW_ICON
#define SHOW_WINDOW_ICON 0
#endif
#ifndef COMMAND_EXEC_PROGRAM
#define COMMAND_EXEC_PROGRAM 0
#endif
#ifndef COMMAND_SHOW_OPTIONS
#define COMMAND_SHOW_OPTIONS 0
#endif
#ifndef COMMAND_SHOW_VERSION
#define COMMAND_SHOW_VERSION 0
#endif
#ifndef CLOSE_SAFELY
#define CLOSE_SAFELY 0
#endif
#ifndef EXPORT_WINDOWID
#define EXPORT_WINDOWID 0
#endif
#ifndef TAB_NEW_PATH_EQUAL_OLD
#define TAB_NEW_PATH_EQUAL_OLD 0
#endif
#ifndef MATCH_STRING_HTTP
#define MATCH_STRING_HTTP 0
#endif
#ifndef MATCH_STRING_MAIL
#define MATCH_STRING_MAIL 0
#endif
#ifndef MATCH_STRING_FILE
#define MATCH_STRING_FILE 0
#endif
#ifndef TAB_NEW_TAB_AT_TAB_ONE
#define TAB_NEW_TAB_AT_TAB_ONE 0
#endif
#ifndef TAB_LABEL_DYNAMIC
#define TAB_LABEL_DYNAMIC 0
#endif
#ifndef MOUSE_CTRL_SATURATION
#define MOUSE_CTRL_SATURATION 0
#endif
#ifndef TABBAR_AUTOHIDE
#define TABBAR_AUTOHIDE 0
#endif
#ifndef TAB_EXPANDED_WIDTH
#define TAB_EXPANDED_WIDTH 0
#endif
#ifndef TAB_SHOW_INFO_AT_TITLE
#define TAB_SHOW_INFO_AT_TITLE 0
#endif
#ifndef WINDOW_TITLE_DYNAMIC
#define WINDOW_TITLE_DYNAMIC 0
#endif
#ifndef TAB_CLOSE_BUTTON
#define TAB_CLOSE_BUTTON 0
#endif
#ifndef TABBAR_SCROLLABLE
#define TABBAR_SCROLLABLE 0
#endif
#ifndef TABBAR_MENU_SELECT_TAB
#define TABBAR_MENU_SELECT_TAB 0
#endif
#ifndef BELL_URGENT
#define BELL_URGENT 0
#endif
#ifndef TAB_REORDERABLE
#define TAB_REORDERABLE 0
#endif
#ifndef COMMAND_TAB_NUMBERS
#define COMMAND_TAB_NUMBERS 0
#endif
#ifndef COMMAND_FULLSCREEN
#define COMMAND_FULLSCREEN 0
#endif
#ifndef COMMAND_GEOMETRY
#define COMMAND_GEOMETRY 0
#endif
#ifndef COMMAND_DOCK_MODE
#define COMMAND_DOCK_MODE 0
#endif
#ifndef COMMAND_AT_ROOT_WINDOW
#define COMMAND_AT_ROOT_WINDOW 0
#endif
#ifndef COMMAND_LOGIN_SHELL
#define COMMAND_LOGIN_SHELL 0
#endif
#ifndef BACKGROUND_OPACITY
#define BACKGROUND_OPACITY 0
#endif
#ifndef COMMAND_SET_TITLE
#define COMMAND_SET_TITLE 0
#endif
#ifndef PROGRAM_WM_CLASS
#define PROGRAM_WM_CLASS 0
#endif
#ifndef BUTTON_ORDER_BY_RCFILE
#define BUTTON_ORDER_BY_RCFILE 0
#endif
#ifndef COMMAND_SHOW_HELP
#define COMMAND_SHOW_HELP 0
#endif
#ifndef COMMAND_FONT
#define COMMAND_FONT 0
#endif
#ifndef COMMAND_COLOR_FG
#define COMMAND_COLOR_FG 0
#endif
#ifndef COMMAND_COLOR_BG
#define COMMAND_COLOR_BG 0
#endif
#ifndef COMMAND_SATURATION
#define COMMAND_SATURATION 0
#endif

#define GET_VTE_CHILD_PID NULL
#if VTE_FORK_CMD_OLD
#undef GET_VTE_CHILD_PID
#define GET_VTE_CHILD_PID
#endif

#if TAB_NEW_PATH_EQUAL_OLD || CLOSE_DIALOG || CLOSE_SAFELY
#undef GET_VTE_CHILD_PID
#define GET_VTE_CHILD_PID &(term->pid)
#if VTE_FORK_CMD_OLD
#undef GET_VTE_CHILD_PID
#define GET_VTE_CHILD_PID term->pid =
#endif
#endif

#if TAB_NEW_PATH_EQUAL_OLD
#ifndef DEFAULT_DIRECTORY
#define DEFAULT_DIRECTORY g_get_current_dir()
#endif
#define VTE_DEFAULT_DIRECTORY default_directory
char *default_directory;
#endif

#if !TAB_NEW_PATH_EQUAL_OLD
#ifndef DEFAULT_DIRECTORY
#define DEFAULT_DIRECTORY NULL
#endif
#define VTE_DEFAULT_DIRECTORY DEFAULT_DIRECTORY
#endif

#ifndef HOTKEY_HAS_DEFINE
#undef HOTKEY
#endif

#ifndef HOTKEY
#define HOTKEY 0
#endif

#if !HOTKEY
#undef MENU_TOGGLE_HOTKEYS
#undef HOTKEY_COLOR_BACKGROUND
#undef HOTKEY_FONT_BIGGER
#undef HOTKEY_FONT_SMALLER
#undef HOTKEY_FONT_DEFAULT_SIZE
#undef HOTKEY_FONT_SELECT
#undef HOTKEY_HAS_DEFINE
#undef HOTKEY_MIMIC_SCROLL_UP
#undef HOTKEY_MIMIC_SCROLL_DOWN
#undef HOTKEY_SATURATION_DIALOG
#undef HOTKEY_SATURATION_MORE
#undef HOTKEY_SATURATION_LESS
#undef HOTKEY_SEARCH_STRING
#undef HOTKEY_SEARCH_PREVIOUS
#undef HOTKEY_SEARCH_NEXT
#undef HOTKEY_TOGGLE_ANTI_ALIAS
#undef HOTKEY_TOGGLE_DECORATED
#undef HOTKEY_TOGGLE_FULLSCREEN
#undef HOTKEY_TOGGLE_HOTKEYS
#undef HOTKEY_TOGGLE_ON_TOP
#undef HOTKEY_TOGGLE_SCROLLBAR
#undef HOTKEY_TOGGLE_STATUS_BAR
#undef HOTKEY_TOGGLE_BACKGROUND
#endif

#if defined(HOTKEY_TAB_GO_TO_NUMBER) && !defined(CTRL_NUMBER_GO_TO_TAB_NUMBER)
#define CTRL_NUMBER_GO_TO_TAB_NUMBER HOTKEY_TAB_GO_TO_NUMBER
#endif

#if !TAB || !HOTKEY
#undef HOTKEY_TAB_ADD
#undef HOTKEY_TAB_REMOVE
#undef HOTKEY_TAB_PREVIOUS
#undef HOTKEY_TAB_NEXT
#undef HOTKEY_TAB_FIRST
#undef HOTKEY_TAB_LAST
#undef ALT_NUMBER_GO_TO_TAB_NUMBER
#undef CTRL_NUMBER_GO_TO_TAB_NUMBER
#undef HOTKEY_TAB_EDIT_LABEL
#undef HOTKEY_TOGGLE_TABBAR
#endif

#ifndef ALT_NUMBER_GO_TO_TAB_NUMBER
#define ALT_NUMBER_GO_TO_TAB_NUMBER 0
#endif
#ifndef CTRL_NUMBER_GO_TO_TAB_NUMBER
#define CTRL_NUMBER_GO_TO_TAB_NUMBER 0
#endif

#if BUTTON_ORDER_BY_RCFILE
bool button_order = FALSE;
#endif

#ifndef MENU
#define MENU 0
#endif

#if !MENU
#undef MENU_ENCODING_LIST
#undef MENU_MATCH_STRING_EXEC
#undef MENU_CUSTOM
#endif

#ifndef MENU_CUSTOM_SIZE
#undef MENU_CUSTOM
#endif

#ifndef MENU_ENCODING_LIST_SIZE
#undef MENU_ENCODING_LIST
#endif

#ifndef TOGGLE_BG_ORDER_SIZE
#undef TOGGLE_BG_ORDER
#endif

#ifndef MENU_CUSTOM
#undef MENU_COPY
#undef MENU_PASTE
#undef MENU_SELECT_ALL
#undef MENU_COLOR_BACKGROUND
#undef MENU_OPEN_NEW_WINDOW
#undef MENU_QUIT
#undef MENU_FONT_BIGGER
#undef MENU_FONT_SMALLER
#undef MENU_FONT_DEFAULT_SIZE
#undef MENU_RESET_TERMINAL
#undef MENU_RESET_AND_CLEAR
#undef MENU_FONT_SELECT
#undef MENU_SEPARATOR
#undef SUBMENU_ENCODING_LIST
#undef SUBMENU_INPUT_METHOD
#undef MENU_TOGGLE_BACKGROUND
#undef MENU_TOGGLE_HOTKEYS
#undef MENU_TOGGLE_ON_TOP
#undef MENU_TOGGLE_SCROLLBAR
#undef MENU_TOGGLE_STATUS_BAR
#undef MENU_TOGGLE_DECORATED
#undef MENU_TOGGLE_FULLSCREEN
#undef MENU_TOGGLE_ANTI_ALIAS
#undef MENU_CHANGE_SATURATION
#undef ONLY_ONE_MENU_ITEM
#endif

#ifndef STATUS_BAR
#define STATUS_BAR 0
#endif
#ifndef SCROLLBAR
#define SCROLLBAR OFF_R
#endif

#if !TAB || !defined(MENU_CUSTOM)
#undef MENU_TAB_ADD
#undef MENU_TAB_REMOVE
#undef MENU_TAB_EDIT_LABEL
#undef MENU_TOGGLE_TABBAR
#endif

#if defined(HOTKEY_TOGGLE_ON_TOP) || defined(MENU_TOGGLE_ON_TOP)
#ifndef PROGRAM_ALWAYS_ON_TOP
#define PROGRAM_ALWAYS_ON_TOP FALSE
#endif
#endif

#if defined(HOTKEY_SATURATION_DIALOG) || defined(MENU_CHANGE_SATURATION)
GtkWidget *adjustment;
#endif

#if defined(HOTKEY_TOGGLE_HOTKEYS) || defined(MENU_TOGGLE_HOTKEYS)
bool hotkey_status = TRUE;
#endif

#ifdef MENU_ENCODING_LIST
char *encoding[] = { MENU_ENCODING_LIST };
#endif

#if !defined(MENU_ENCODING_LIST) && MENU
GtkWidget *encoding_item;
#endif

#if defined(TOGGLE_BG_ORDER_SIZE) && (TOGGLE_BG_ORDER_SIZE == 1)
#undef HOTKEY_TOGGLE_BACKGROUND
#undef MENU_TOGGLE_BACKGROUND
#endif

#if defined(HOTKEY_TOGGLE_BACKGROUND) || defined(MENU_TOGGLE_BACKGROUND)
#ifndef BACKGROUND_IMAGE
#define BACKGROUND_IMAGE ".config/ivte/background.png"
#endif
#ifndef TOGGLE_BG_ORDER
#define TOGGLE_BG_ORDER "Image", "Transparent", "No background", "Opacity"
#undef TOGGLE_BG_ORDER_SIZE
#define TOGGLE_BG_ORDER_SIZE 4
#define TOGGLE_BG_OPACITY
#define TOGGLE_BG_IMAGE
#define TOGGLE_BG_TRANSPARENT
#define TOGGLE_BG_NO_BACKGROUND
#endif
const char *background_order[] = { TOGGLE_BG_ORDER };
unsigned short background_status = 0;
#endif

#ifdef BACKGROUND_IMAGE
char imgstr[sizeof(BACKGROUND_IMAGE) + 64];
#endif

#ifdef PROGRAM_ICON
char iconstr[sizeof(PROGRAM_ICON) + 64];
#endif

#if defined(BACKGROUND_IMAGE) || (defined(BACKGROUND_TRANSPARENT) && BACKGROUND_TRANSPARENT) || defined(TOGGLE_BG_TRANSPARENT)
#define BACKGROUND_EXIST
#endif

#if defined(HOTKEY_COLOR_BACKGROUND) || defined(MENU_COLOR_BACKGROUND)
#ifndef BACKGROUND_TINT_COLOR
#define BACKGROUND_TINT_COLOR "black"
#endif
#endif

#if defined(BACKGROUND_TINT_COLOR) && defined(BACKGROUND_EXIST)
GdkColor color_tint;
#endif

#if defined(HOTKEY_TOGGLE_SCROLLBAR) || defined(MENU_TOGGLE_SCROLLBAR)
#ifndef SCROLLBAR
#define SCROLLBAR RIGHT
#endif
bool scrollbar_status = (SCROLLBAR < 3);
#endif

#if !defined(HOTKEY_TOGGLE_SCROLLBAR) && !defined(MENU_TOGGLE_SCROLLBAR) && (SCROLLBAR > 3)
#undef SCROLLBAR
#endif

#ifdef SCROLLBAR
#define VTE_HBOX term->hbox
#endif
#ifndef SCROLLBAR
#define VTE_HBOX term->vte
#endif

#if defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
bool status_bar_status = STATUS_BAR;
bool status_bar_resize_grip = FALSE;
#endif

#if defined(HOTKEY_TOGGLE_DECORATED) || defined(MENU_TOGGLE_DECORATED)
#ifdef SHOW_WINDOW_DECORATED
bool window_decorated_status = SHOW_WINDOW_DECORATED;
#endif
#ifndef SHOW_WINDOW_DECORATED
bool window_decorated_status = TRUE;
#endif
#endif

#if defined(HOTKEY_TOGGLE_FULLSCREEN) || defined(MENU_TOGGLE_FULLSCREEN) || defined(VTE_FUNNY)
bool window_fullscreen_status = FALSE;
#endif

#if VTE_CHECK_VERSION(0,19,1) && defined(VTE_DISABLE_DEPRECATED)
#undef FONT_ANTI_ALIAS
#undef HOTKEY_TOGGLE_ANTI_ALIAS
#undef MENU_TOGGLE_ANTI_ALIAS
#endif

#if defined(HOTKEY_TOGGLE_ANTI_ALIAS) || defined(MENU_TOGGLE_ANTI_ALIAS)
#if defined(FONT_ANTI_ALIAS) && !FONT_ANTI_ALIAS
unsigned short antialias_status = VTE_ANTI_ALIAS_FORCE_DISABLE;
#endif
#if !defined(FONT_ANTI_ALIAS) || FONT_ANTI_ALIAS
unsigned short antialias_status = VTE_ANTI_ALIAS_FORCE_ENABLE;
#endif
#endif

#define ONLY_ONE_MENU_HANDLE
#ifdef ONLY_ONE_MENU_ITEM
#undef ONLY_ONE_MENU_HANDLE
#define ONLY_ONE_MENU_HANDLE || (menu_item_success == 1)
#endif

#ifdef MENU_CUSTOM
const char *menu_custom[] = { MENU_CUSTOM };
unsigned short menu_item_success = 0;
#endif

#if TABBAR_AUTOHIDE && !defined(TABBAR)
#define TABBAR TRUE
#endif

#if defined(HOTKEY_TOGGLE_TABBAR) || defined(MENU_TOGGLE_TABBAR)
#ifdef TABBAR
bool tabbar_status = TABBAR;
#endif
#ifndef TABBAR
bool tabbar_status = TRUE;
#endif
#define VTE_TABBAR tabbar_status
#endif

#if !defined(HOTKEY_TOGGLE_TABBAR) && !defined(MENU_TOGGLE_TABBAR)
#define VTE_TABBAR TABBAR
#endif

#if defined(MENU_MATCH_STRING_EXEC) || defined(MATCH_STRING_L) || defined(MATCH_STRING_M)
#if !MATCH_STRING_HTTP && !MATCH_STRING_MAIL && !MATCH_STRING_FILE && !defined(MATCH_STRING)
#undef MATCH_STRING_HTTP
#define MATCH_STRING_HTTP TRUE
#endif
#endif

#ifdef FONT_ANTI_ALIAS
#define VTE_ANTI_ALIAS VTE_ANTI_ALIAS_FORCE_ENABLE
#if !FONT_ANTI_ALIAS
#undef VTE_ANTI_ALIAS
#define VTE_ANTI_ALIAS VTE_ANTI_ALIAS_FORCE_DISABLE
#endif
#endif

#ifndef FONT_ANTI_ALIAS
#define VTE_ANTI_ALIAS VTE_ANTI_ALIAS_USE_DEFAULT
#endif

#define IVTE_SET_FONT(x,y,z) vte_terminal_set_font_from_string(x,y)
#if defined(FONT_ANTI_ALIAS) || defined(MENU_TOGGLE_ANTI_ALIAS) || defined(HOTKEY_TOGGLE_ANTI_ALIAS)
#undef IVTE_SET_FONT
#define IVTE_SET_FONT vte_terminal_set_font_from_string_full
#endif

#ifndef PROGRAM_NAME
#define PROGRAM_NAME "ivte"
#define UPPER_PROGRAM_NAME "IVte"
#endif

#if PROGRAM_WM_CLASS
char *wm_class_name = PROGRAM_NAME;
char *wm_class_class = UPPER_PROGRAM_NAME;
#endif

#define VTE_REGEX 0
#if defined(SEARCH_CASE_SENSITIVE) && !SEARCH_CASE_SENSITIVE
#undef VTE_REGEX
#define VTE_REGEX G_REGEX_CASELESS
#endif

#define VTE_PROGRAM_NAME PROGRAM_NAME
#if COMMAND_SET_TITLE
#undef VTE_PROGRAM_NAME
#define VTE_PROGRAM_NAME program_name
char *program_name = PROGRAM_NAME;
#endif

#if COMMAND_FONT
char *command_font = NULL;
#endif

#if COMMAND_COLOR_BG
char *command_color_bg = NULL;
#endif

#if COMMAND_COLOR_FG
char *command_color_fg = NULL;
#endif

#if COMMAND_GEOMETRY
char *command_geometry = NULL;
#endif

GtkWidget *main_window;
GtkWidget *notebook;

#define VTE_LABEL NULL
#if defined(TAB_LABEL) || defined(TAB_LABEL_CUSTOM)
#undef VTE_LABEL
#define VTE_LABEL label
#endif

#if defined(GTK3_CSS_USE_BOX) && GTK_CHECK_VERSION(2,91,2) && defined(USE_GTK_GRID)
#if CLOSE_DIALOG || TAB_CLOSE_BUTTON || defined(SCROLLBAR) || STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
#warning "You are using GtkGrid but your GTK3_CSS defined GtkHBox or GtkVBox."
#endif
#endif

#if defined(DEF_TAB_LABEL) || TAB_CLOSE_BUTTON
#ifndef TAB_LABEL
#define TAB_LABEL "Page %u"
#endif
#endif

#if TAB_CLOSE_BUTTON
#undef VTE_LABEL
#define VTE_LABEL term->label
#ifndef GTK3_CSS
#ifdef USE_GTK_GRID
#define GTK3_CSS "GtkNotebook GtkGrid GtkButton { -GtkWidget-focus-line-width: 0; } GtkNotebook GtkButton { border-width: 0; padding: 0; -GtkButton-inner-border: 0; }"
#endif
#ifndef USE_GTK_GRID
#define GTK3_CSS "GtkNotebook GtkHBox GtkButton { -GtkWidget-focus-line-width: 0; } GtkNotebook GtkButton { border-width: 0; padding: 0; -GtkButton-inner-border: 0; }"
#endif
#endif
#endif

#ifdef TAB_LABEL_CUSTOM
#undef TAB_LABEL
const char *label_style_custom[] = { TAB_LABEL_CUSTOM };
const int label_style_size = sizeof(label_style_custom) / sizeof(label_style_custom[0]);
#endif

#ifdef COLOR_STYLE
const GdkColor color_style[16] = {
#if COLOR_STYLE == LINUX
  { 0, 0x0000, 0x0000, 0x0000 },
  { 0, 0xa8a8, 0x0000, 0x0000 },
  { 0, 0x0000, 0xa8a8, 0x0000 },
  { 0, 0xa8a8, 0x5757, 0x0000 },
  { 0, 0x0000, 0x0000, 0xa8a8 },
  { 0, 0xa8a8, 0x0000, 0xa8a8 },
  { 0, 0x0000, 0xa8a8, 0xa8a8 },
  { 0, 0xa8a8, 0xa8a8, 0xa8a8 },
  { 0, 0x5757, 0x5757, 0x5757 },
  { 0, 0xffff, 0x5757, 0x5757 },
  { 0, 0x5757, 0xffff, 0x5757 },
  { 0, 0xffff, 0xffff, 0x5757 },
  { 0, 0x5757, 0x5757, 0xffff },
  { 0, 0xffff, 0x5757, 0xffff },
  { 0, 0x5757, 0xffff, 0xffff },
  { 0, 0xffff, 0xffff, 0xffff }
#endif
#if COLOR_STYLE == RXVT
  { 0, 0x0000, 0x0000, 0x0000 },
  { 0, 0xcdcd, 0x0000, 0x0000 },
  { 0, 0x0000, 0xcdcd, 0x0000 },
  { 0, 0xcdcd, 0xcdcd, 0x0000 },
  { 0, 0x0000, 0x0000, 0xcdcd },
  { 0, 0xcdcd, 0x0000, 0xcdcd },
  { 0, 0x0000, 0xcdcd, 0xcdcd },
  { 0, 0xfafa, 0xebeb, 0xd7d7 },
  { 0, 0x4040, 0x4040, 0x4040 },
  { 0, 0xffff, 0x0000, 0x0000 },
  { 0, 0x0000, 0xffff, 0x0000 },
  { 0, 0xffff, 0xffff, 0x0000 },
  { 0, 0x0000, 0x0000, 0xffff },
  { 0, 0xffff, 0x0000, 0xffff },
  { 0, 0x0000, 0xffff, 0xffff },
  { 0, 0xffff, 0xffff, 0xffff }
#endif
#if COLOR_STYLE == TANGO
  { 0, 0x2e2e, 0x3434, 0x3636 },
  { 0, 0xcccc, 0x0000, 0x0000 },
  { 0, 0x4e4e, 0x9a9a, 0x0606 },
  { 0, 0xc4c4, 0xa0a0, 0x0000 },
  { 0, 0x3434, 0x6565, 0xa4a4 },
  { 0, 0x7575, 0x5050, 0x7b7b },
  { 0, 0x0606, 0x9898, 0x9a9a },
  { 0, 0xd3d3, 0xd7d7, 0xcfcf },
  { 0, 0x5555, 0x5757, 0x5353 },
  { 0, 0xefef, 0x2929, 0x2929 },
  { 0, 0x8a8a, 0xe2e2, 0x3434 },
  { 0, 0xfcfc, 0xe9e9, 0x4f4f },
  { 0, 0x7272, 0x9f9f, 0xcfcf },
  { 0, 0xadad, 0x7f7f, 0xa8a8 },
  { 0, 0x3434, 0xe2e2, 0xe2e2 },
  { 0, 0xeeee, 0xeeee, 0xecec }
#endif
#if COLOR_STYLE == XTERM
  { 0, 0x0000, 0x0000, 0x0000 },
  { 0, 0xcdcd, 0x0000, 0x0000 },
  { 0, 0x0000, 0xcdcd, 0x0000 },
  { 0, 0xcdcd, 0xcdcd, 0x0000 },
  { 0, 0x0000, 0x0000, 0xeeee },
  { 0, 0xcdcd, 0x0000, 0xcdcd },
  { 0, 0x0000, 0xcdcd, 0xcdcd },
  { 0, 0xe5e5, 0xe5e5, 0xe5e5 },
  { 0, 0x7f7f, 0x7f7f, 0x7f7f },
  { 0, 0xffff, 0x0000, 0x0000 },
  { 0, 0x0000, 0xffff, 0x0000 },
  { 0, 0xffff, 0xffff, 0x0000 },
  { 0, 0x5c5c, 0x5c5c, 0xffff },
  { 0, 0xffff, 0x0000, 0xffff },
  { 0, 0x0000, 0xffff, 0xffff },
  { 0, 0xffff, 0xffff, 0xffff }
#endif
#if COLOR_STYLE == ZENBURN_DARK
  { 0, 0x0000, 0x0000, 0x0000 },
  { 0, 0x9e9e, 0x1818, 0x2828 },
  { 0, 0xaeae, 0xcece, 0x9292 },
  { 0, 0x9696, 0x8a8a, 0x3838 },
  { 0, 0x4141, 0x4141, 0x7171 },
  { 0, 0x9696, 0x3c3c, 0x5959 },
  { 0, 0x4141, 0x8181, 0x7979 },
  { 0, 0xbebe, 0xbebe, 0xbebe },
  { 0, 0x6666, 0x6666, 0x6666 },
  { 0, 0xcfcf, 0x6161, 0x7171 },
  { 0, 0xc5c5, 0xf7f7, 0x7979 },
  { 0, 0xffff, 0xf7f7, 0x9696 },
  { 0, 0x4141, 0x8686, 0xbebe },
  { 0, 0xcfcf, 0x9e9e, 0xbebe },
  { 0, 0x7171, 0xbebe, 0xbebe },
  { 0, 0xffff, 0xffff, 0xffff }
#endif
#if COLOR_STYLE == ZENBURN
  { 0, 0x3f3f, 0x3f3f, 0x3f3f },
  { 0, 0x7070, 0x5050, 0x5050 },
  { 0, 0x6060, 0xb4b4, 0x8a8a },
  { 0, 0xdfdf, 0xafaf, 0x8f8f },
  { 0, 0x5050, 0x6060, 0x7070 },
  { 0, 0xdcdc, 0x8c8c, 0xc3c3 },
  { 0, 0x8c8c, 0xd0d0, 0xd3d3 },
  { 0, 0xdcdc, 0xdcdc, 0xcccc },
  { 0, 0x7070, 0x9090, 0x8080 },
  { 0, 0xdcdc, 0xa3a3, 0xa3a3 },
  { 0, 0xc3c3, 0xbfbf, 0x9f9f },
  { 0, 0xf0f0, 0xdfdf, 0xafaf },
  { 0, 0x9494, 0xbfbf, 0xf3f3 },
  { 0, 0xecec, 0x9393, 0xd3d3 },
  { 0, 0x9393, 0xe0e0, 0xe3e3 },
  { 0, 0xffff, 0xffff, 0xffff }
#endif
#if COLOR_STYLE == SOLARIZED_DARK
  { 0, 0x0707, 0x3636, 0x4242 },
  { 0, 0xdcdc, 0x3232, 0x2f2f },
  { 0, 0x8585, 0x9999, 0x0000 },
  { 0, 0xb5b5, 0x8989, 0x0000 },
  { 0, 0x2626, 0x8b8b, 0xd2d2 },
  { 0, 0xd3d3, 0x3636, 0x8282 },
  { 0, 0x2a2a, 0xa1a1, 0x9898 },
  { 0, 0xeeee, 0xe8e8, 0xd5d5 },
  { 0, 0x0000, 0x2b2b, 0x3636 },
  { 0, 0xcbcb, 0x4b4b, 0x1616 },
  { 0, 0x5858, 0x6e6e, 0x7575 },
  { 0, 0x6565, 0x7b7b, 0x8383 },
  { 0, 0x8383, 0x9494, 0x9696 },
  { 0, 0x6c6c, 0x7171, 0xc4c4 },
  { 0, 0x9393, 0xa1a1, 0xa1a1 },
  { 0, 0xfdfd, 0xf6f6, 0xe3e3 }
#endif
#if COLOR_STYLE == SOLARIZED_LIGHT
  { 0, 0xeeee, 0xe8e8, 0xd5d5 },
  { 0, 0xdcdc, 0x3232, 0x2f2f },
  { 0, 0x8585, 0x9999, 0x0000 },
  { 0, 0xb5b5, 0x8989, 0x0000 },
  { 0, 0x2626, 0x8b8b, 0xd2d2 },
  { 0, 0xd3d3, 0x3636, 0x8282 },
  { 0, 0x2a2a, 0xa1a1, 0x9898 },
  { 0, 0x0707, 0x3636, 0x4242 },
  { 0, 0xfdfd, 0xf6f6, 0xe3e3 },
  { 0, 0xcbcb, 0x4b4b, 0x1616 },
  { 0, 0x9393, 0xa1a1, 0xa1a1 },
  { 0, 0x8383, 0x9494, 0x9696 },
  { 0, 0x6565, 0x7b7b, 0x8383 },
  { 0, 0x6c6c, 0x7171, 0xc4c4 },
  { 0, 0x5858, 0x6e6e, 0x7575 },
  { 0, 0x0000, 0x2b2b, 0x3636 }
#endif
};
#endif

#if defined(FONT_ANTI_ALIAS) || defined(MENU_FONT_BIGGER) || defined(MENU_FONT_SMALLER) || defined(MENU_FONT_SELECT) || defined(MENU_TOGGLE_ANTI_ALIAS) || defined(HOTKEY_TOGGLE_ANTI_ALIAS) || defined(HOTKEY_FONT_BIGGER) || defined(HOTKEY_FONT_SMALLER) || defined(HOTKEY_FONT_SELECT) || COMMAND_FONT
#ifndef FONT
#define FONT "Monospace 10"
#endif
#endif

#if !defined(MENU_FONT_BIGGER) && !defined(MENU_FONT_SMALLER) && !defined(MENU_FONT_SELECT) && !defined(HOTKEY_FONT_BIGGER) && !defined(HOTKEY_FONT_SMALLER) && !defined(HOTKEY_FONT_SELECT)
#undef MENU_FONT_DEFAULT_SIZE
#undef HOTKEY_FONT_DEFAULT_SIZE
#endif

#ifdef FONT
char font_name[125];
char font_str[128];
unsigned int font_size;
#if defined(HOTKEY_FONT_DEFAULT_SIZE) || defined(MENU_FONT_DEFAULT_SIZE)
unsigned int font_size_default;
#endif
#endif

#if COMMAND_EXEC_PROGRAM
#define VTE_DEFAULT_COMMAND default_command
char default_command[256];
char **default_argv = NULL;
#endif

#if !COMMAND_EXEC_PROGRAM
#define VTE_DEFAULT_COMMAND DEFAULT_COMMAND
#define default_argv NULL
#endif

#if COMMAND_LOGIN_SHELL && VTE_FORK_CMD_OLD
char *login_shell[] = { "-", NULL };
#endif

#if COMMAND_LOGIN_SHELL || (!VTE_FORK_CMD_OLD && COMMAND_EXEC_PROGRAM)
unsigned short login_shell_flag = 0;
#endif

#if defined(HOTKEY_SATURATION_MORE) || defined(HOTKEY_SATURATION_LESS) || MOUSE_CTRL_SATURATION || BACKGROUND_OPACITY || defined(HOTKEY_SATURATION_DIALOG) || defined(MENU_CHANGE_SATURATION) || defined(HOTKEY_TOGGLE_BACKGROUND) || defined(MENU_TOGGLE_BACKGROUND) || COMMAND_SATURATION
#ifndef BACKGROUND_SATURATION
#define BACKGROUND_SATURATION 0.4
#endif
#endif

#ifdef PROGRAM_ALWAYS_ON_TOP
bool always_on_top = PROGRAM_ALWAYS_ON_TOP;
#endif

#ifdef BACKGROUND_SATURATION
double saturation_level = BACKGROUND_SATURATION;
#endif

#if COMMAND_AT_ROOT_WINDOW
bool at_root_window = FALSE;
#endif

#if MENU
GtkWidget *menu;
#endif

#ifdef MENU_COPY
GtkWidget *menu_copy;
#endif

#ifdef MENU_PASTE
GtkWidget *menu_paste;
#endif

#ifdef MENU_FONT_DEFAULT_SIZE
GtkWidget *menu_zoom_100;
#endif

#ifdef MENU_MATCH_STRING_EXEC
GtkWidget *match_open;
GtkWidget *match_copy;
GtkWidget *match_item;
#endif

#if defined(MENU_MATCH_STRING_EXEC) || defined(MATCH_STRING_L) || defined(MATCH_STRING_M)
char *matched_url = NULL;
#endif

#if STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
GtkWidget *statusbar;
GtkWidget *vbox;
#endif

#if defined(HOTKEY_SEARCH_STRING) || defined(HOTKEY_SEARCH_PREVIOUS) || defined(HOTKEY_SEARCH_NEXT)
  GtkWidget *global_search_string;
#endif

struct terminal {
  GtkWidget *vte;
#if CLOSE_DIALOG || CLOSE_SAFELY || TAB_NEW_PATH_EQUAL_OLD
  int pid;
#endif
#if TAB_CLOSE_BUTTON
  GtkWidget *button;
  GtkWidget *label;
  GtkWidget *label_edit;
#endif
#ifdef SCROLLBAR
  GtkWidget *hbox;
  GtkWidget *scrollbar;
#endif
#if defined(HOTKEY_TAB_EDIT_LABEL) || defined(MENU_TAB_EDIT_LABEL)
  bool label_exist;
#endif
#if defined(HOTKEY_SEARCH_STRING) || defined(HOTKEY_SEARCH_PREVIOUS) || defined(HOTKEY_SEARCH_NEXT)
  GtkWidget *search_string;
  bool global_string;
#endif
};

struct terminal *term;

#ifdef RULE_THEM_ALL
#define GET_GTK_TWO (p_hdl_gtk = dlopen("libgtk-x11-2.0.so.0", RTLD_LAZY|RTLD_LOCAL))
#define GET_GTK_THREE (p_hdl_gtk = dlopen("libgtk-3.so.0", RTLD_LAZY|RTLD_LOCAL))
#define GET_VTE_TWO (p_hdl_vte = dlopen("libvte.so.9", RTLD_LAZY|RTLD_LOCAL))
#define GET_VTE_THREE (p_hdl_vte = dlopen("libvte2_90.so.9", RTLD_LAZY|RTLD_LOCAL))
gboolean (*p_gdk_color_parse)(const gchar *spec, GdkColor *color);
void (*p_gdk_device_warp)(GdkDevice *device, GdkScreen *screen, gint x, gint y);
void (*p_gdk_disable_multidevice)(void);
GdkDisplay* (*p_gdk_display_get_default)(void);
GdkScreen* (*p_gdk_display_get_default_screen)(GdkDisplay *display);
void (*p_gdk_display_warp_pointer)(GdkDisplay *display, GdkScreen *screen, gint x, gint y);
GdkColormap* (*p_gdk_screen_get_rgba_colormap)(GdkScreen *screen);
GdkVisual* (*p_gdk_screen_get_rgba_visual)(GdkScreen *screen);
GdkDisplay* (*p_gdk_window_get_display)(GdkWindow *window);
XID (*p_gdk_x11_drawable_get_xid)(GdkDrawable *drawable);
Window (*p_gdk_x11_window_get_xid)(GdkWindow *window);
void (*p_gtk_box_pack_start)(GtkBox *box, GtkWidget *child, gboolean expand, gboolean fill, guint padding);
GtkWidget* (*p_gtk_button_new)(void);
void (*p_gtk_button_set_focus_on_click)(GtkButton *button, gboolean focus_on_click);
void (*p_gtk_button_set_image)(GtkButton *button, GtkWidget *image);
void (*p_gtk_button_set_relief)(GtkButton *button, GtkReliefStyle newstyle);
GtkClipboard* (*p_gtk_clipboard_get)(GdkAtom selection);
GtkClipboard* (*p_gtk_clipboard_get_for_display)(GdkDisplay *display, GdkAtom selection);
void (*p_gtk_clipboard_set_text)(GtkClipboard *clipboard, const gchar *text, gint len);
gboolean (*p_gtk_clipboard_wait_is_text_available)(GtkClipboard *clipboard);
GtkWidget* (*p_gtk_color_selection_dialog_get_color_selection)(GtkColorSelectionDialog *colorsel);
GtkWidget* (*p_gtk_color_selection_dialog_new)(const gchar *title);
void (*p_gtk_color_selection_get_current_color)(GtkColorSelection *colorsel, GdkColor *color);
void (*p_gtk_color_selection_set_current_color)(GtkColorSelection *colorsel, const GdkColor *color);
void (*p_gtk_container_add)(GtkContainer *container, GtkWidget *widget);
void (*p_gtk_container_child_set)(GtkContainer *container, GtkWidget *child, const gchar *first_prop_name, ...);
gboolean (*p_gtk_css_provider_load_from_data)(GtkCssProvider *css_provider, const gchar *data, gssize length, GError **error);
GtkCssProvider* (*p_gtk_css_provider_new)(void);
GtkWidget* (*p_gtk_dialog_get_content_area)(GtkDialog *dialog);
GtkWidget* (*p_gtk_dialog_new_with_buttons)(const gchar *title, GtkWindow *parent, GtkDialogFlags flags, const gchar *first_button_text, ...);
gint (*p_gtk_dialog_run)(GtkDialog *dialog);
void (*p_gtk_dialog_set_default_response)(GtkDialog *dialog, gint response_id);
const gchar* (*p_gtk_entry_get_text)(GtkEntry *entry);
GtkWidget* (*p_gtk_entry_new)(void);
void (*p_gtk_entry_set_activates_default)(GtkEntry *entry, gboolean setting);
void (*p_gtk_entry_set_text)(GtkEntry *entry, const gchar *text);
gchar* (*p_gtk_font_selection_dialog_get_font_name)(GtkFontSelectionDialog *fsd);
GtkWidget* (*p_gtk_font_selection_dialog_new)(const gchar *title);
gboolean (*p_gtk_font_selection_dialog_set_font_name)(GtkFontSelectionDialog *fsd, const gchar *fontname);
GtkWidget* (*p_gtk_hbox_new)(gboolean homogeneous, gint spacing);
GtkWidget* (*p_gtk_hscale_new_with_range)(gdouble min, gdouble max, gdouble step);
GtkWidget* (*p_gtk_image_menu_item_new_from_stock)(const gchar *stock_id, GtkAccelGroup *accel_group);
GtkWidget* (*p_gtk_image_menu_item_new_with_label)(const gchar *label);
GtkWidget* (*p_gtk_image_menu_item_new_with_mnemonic)(const gchar *label);
void (*p_gtk_image_menu_item_set_image)(GtkImageMenuItem *image_menu_item, GtkWidget *image);
GtkWidget* (*p_gtk_image_new_from_stock)(const gchar *stock_id, GtkIconSize size);
void (*p_gtk_init)(int *argc, char ***argv);
const gchar* (*p_gtk_label_get_text)(GtkLabel *label);
GtkWidget* (*p_gtk_label_new)(const gchar *str);
void (*p_gtk_main)(void);
void (*p_gtk_main_quit)(void);
GtkWidget* (*p_gtk_menu_item_new_with_label)(const gchar *label);
GtkWidget* (*p_gtk_menu_item_new_with_mnemonic)(const gchar *label);
void (*p_gtk_menu_item_set_submenu)(GtkMenuItem *menu_item, GtkWidget *submenu);
GtkWidget* (*p_gtk_menu_new)(void);
void (*p_gtk_menu_popup)(GtkMenu *menu, GtkWidget *parent_menu_shell, GtkWidget *parent_menu_item, GtkMenuPositionFunc func, gpointer data, guint button, guint32 activate_time);
void (*p_gtk_menu_shell_append)(GtkMenuShell *menu_shell, GtkWidget *child);
void (*p_gtk_menu_shell_prepend)(GtkMenuShell *menu_shell, GtkWidget *child);
gint (*p_gtk_notebook_append_page)(GtkNotebook *notebook, GtkWidget *child, GtkWidget *tab_label);
gint (*p_gtk_notebook_get_current_page)(GtkNotebook *notebook);
gint (*p_gtk_notebook_get_n_pages)(GtkNotebook *notebook);
GtkWidget* (*p_gtk_notebook_get_nth_page)(GtkNotebook *notebook, gint page_num);
gboolean (*p_gtk_notebook_get_show_tabs)(GtkNotebook *notebook);
GtkWidget* (*p_gtk_notebook_get_tab_label)(GtkNotebook *notebook, GtkWidget *child);
GtkWidget* (*p_gtk_notebook_new)(void);
void (*p_gtk_notebook_popup_enable)(GtkNotebook *notebook);
gint (*p_gtk_notebook_prepend_page)(GtkNotebook *notebook, GtkWidget *child, GtkWidget *tab_label);
void (*p_gtk_notebook_remove_page)(GtkNotebook *notebook, gint page_num);
void (*p_gtk_notebook_set_current_page)(GtkNotebook *notebook, gint page_num);
void (*p_gtk_notebook_set_scrollable)(GtkNotebook *notebook, gboolean scrollable);
void (*p_gtk_notebook_set_show_border)(GtkNotebook *notebook, gboolean show_border);
void (*p_gtk_notebook_set_show_tabs)(GtkNotebook *notebook, gboolean show_tabs);
void (*p_gtk_notebook_set_tab_border)(GtkNotebook *notebook, guint border_width);
void (*p_gtk_notebook_set_tab_hborder)(GtkNotebook *notebook, guint tab_hborder);
void (*p_gtk_notebook_set_tab_label)(GtkNotebook *notebook, GtkWidget *child, GtkWidget *tab_label);
void (*p_gtk_notebook_set_tab_label_text)(GtkNotebook *notebook, GtkWidget *child, const gchar *tab_text);
void (*p_gtk_notebook_set_tab_pos)(GtkNotebook *notebook, GtkPositionType pos);
void (*p_gtk_notebook_set_tab_reorderable)(GtkNotebook *notebook, GtkWidget *child, gboolean reorderable);
void (*p_gtk_notebook_set_tab_vborder)(GtkNotebook *notebook, guint tab_vborder);
gdouble (*p_gtk_range_get_value)(GtkRange *range);
void (*p_gtk_range_set_value)(GtkRange *range, gdouble value);
void (*p_gtk_rc_parse_string)(const gchar *rc_string);
GtkWidget* (*p_gtk_scale_new_with_range)(GtkOrientation orientation, gdouble min, gdouble max, gdouble step);
GtkAdjustment* (*p_gtk_scrollable_get_vadjustment)(GtkScrollable *scrollable);
GtkWidget* (*p_gtk_scrollbar_new)(GtkOrientation orientation, GtkAdjustment *adjustment);
GtkWidget* (*p_gtk_separator_menu_item_new)(void);
GtkSettings* (*p_gtk_settings_get_default)(void);
GtkWidget* (*p_gtk_statusbar_new)(void);
guint (*p_gtk_statusbar_push)(GtkStatusbar *statusbar, guint context_id, const gchar *text);
void (*p_gtk_style_context_add_provider_for_screen)(GdkScreen *screen, GtkStyleProvider *provider, guint priority);
gboolean (*p_gtk_test_widget_click)(GtkWidget *widget, guint button, GdkModifierType modifiers);
gboolean (*p_gtk_test_widget_send_key)(GtkWidget *widget, guint keyval, GdkModifierType modifiers);
GtkWidget* (*p_gtk_vbox_new)(gboolean homogeneous, gint spacing);
GtkWidget* (*p_gtk_vscrollbar_new)(GtkAdjustment *adjustment);
void (*p_gtk_widget_destroy)(GtkWidget *widget);
GdkScreen* (*p_gtk_widget_get_screen)(GtkWidget *widget);
GdkWindow* (*p_gtk_widget_get_window)(GtkWidget *widget);
void (*p_gtk_widget_hide)(GtkWidget *widget);
void (*p_gtk_widget_realize)(GtkWidget *widget);
void (*p_gtk_widget_set_can_focus)(GtkWidget *widget, gboolean can_focus);
void (*p_gtk_widget_set_colormap)(GtkWidget *widget, GdkColormap *colormap);
void (*p_gtk_widget_set_hexpand)(GtkWidget *widget, gboolean expand);
void (*p_gtk_widget_set_sensitive)(GtkWidget *widget, gboolean sensitive);
void (*p_gtk_widget_set_size_request)(GtkWidget *widget, gint width, gint height);
void (*p_gtk_widget_set_vexpand)(GtkWidget *widget, gboolean expand);
void (*p_gtk_widget_set_visual)(GtkWidget *widget, GdkVisual *visual);
void (*p_gtk_widget_show)(GtkWidget *widget);
void (*p_gtk_widget_show_all)(GtkWidget *widget);
void (*p_gtk_widget_style_get)(GtkWidget *widget, const gchar *first_property_name, ...);
void (*p_gtk_window_maximize)(GtkWindow *window);
GtkWidget* (*p_gtk_window_new)(GtkWindowType type);
gboolean (*p_gtk_window_parse_geometry)(GtkWindow *window, const gchar *geometry);
void (*p_gtk_window_resize)(GtkWindow *window, gint width, gint height);
gboolean (*p_gtk_window_resize_grip_is_visible)(GtkWindow *window);
void (*p_gtk_window_set_decorated)(GtkWindow *window, gboolean setting);
void (*p_gtk_window_set_focus)(GtkWindow *window, GtkWidget *focus);
void (*p_gtk_window_set_has_resize_grip)(GtkWindow *window, gboolean value);
gboolean (*p_gtk_window_set_icon_from_file)(GtkWindow *window, const gchar *filename, GError **err);
void (*p_gtk_window_set_keep_above)(GtkWindow *window, gboolean setting);
void (*p_gtk_window_set_keep_below)(GtkWindow *window, gboolean setting);
void (*p_gtk_window_set_skip_pager_hint)(GtkWindow *window, gboolean setting);
void (*p_gtk_window_set_skip_taskbar_hint)(GtkWindow *window, gboolean setting);
void (*p_gtk_window_set_title)(GtkWindow *window, const gchar *title);
void (*p_gtk_window_set_type_hint)(GtkWindow *window, GdkWindowTypeHint hint);
void (*p_gtk_window_set_urgency_hint)(GtkWindow *window, gboolean setting);
void (*p_gtk_window_set_wmclass)(GtkWindow *window, const gchar *wmclass_name, const gchar *wmclass_class);
void (*p_gtk_window_unmaximize)(GtkWindow *window);
AtkObject* (*p_vte_terminal_accessible_new)(VteTerminal *terminal);
void (*p_vte_terminal_copy_clipboard)(VteTerminal *terminal);
pid_t (*p_vte_terminal_fork_command)(VteTerminal *terminal, const char *command, char **argv, char **envv, const char *working_directory, gboolean lastlog, gboolean utmp, gboolean wtmp);
gboolean (*p_vte_terminal_fork_command_full)(VteTerminal *terminal, VtePtyFlags pty_flags, const char *working_directory, char **argv, char **envv, GSpawnFlags spawn_flags, GSpawnChildSetupFunc child_setup, gpointer child_setup_data, GPid *child_pid, GError **error);
GtkAdjustment* (*p_vte_terminal_get_adjustment)(VteTerminal *terminal);
glong (*p_vte_terminal_get_char_height)(VteTerminal *terminal);
glong (*p_vte_terminal_get_char_width)(VteTerminal *terminal);
const char* (*p_vte_terminal_get_encoding)(VteTerminal *terminal);
gboolean (*p_vte_terminal_get_has_selection)(VteTerminal *terminal);
const char* (*p_vte_terminal_get_window_title)(VteTerminal *terminal);
void (*p_vte_terminal_im_append_menuitems)(VteTerminal *terminal, GtkMenuShell *menushell);
int (*p_vte_terminal_match_add_gregex)(VteTerminal *terminal, GRegex *regex, GRegexMatchFlags flags);
char* (*p_vte_terminal_match_check)(VteTerminal *terminal, glong column, glong row, int *tag);
GtkWidget* (*p_vte_terminal_new)(void);
void (*p_vte_terminal_paste_clipboard)(VteTerminal *terminal);
void (*p_vte_terminal_reset)(VteTerminal *terminal, gboolean clear_tabstops, gboolean clear_history);
gboolean (*p_vte_terminal_search_find_next)(VteTerminal *terminal);
gboolean (*p_vte_terminal_search_find_previous)(VteTerminal *terminal);
void (*p_vte_terminal_search_set_gregex)(VteTerminal *terminal, GRegex *regex);
void (*p_vte_terminal_search_set_wrap_around)(VteTerminal *terminal, gboolean wrap_around);
void (*p_vte_terminal_select_all)(VteTerminal *terminal);
void (*p_vte_terminal_set_allow_bold)(VteTerminal *terminal, gboolean allow_bold);
void (*p_vte_terminal_set_audible_bell)(VteTerminal *terminal, gboolean is_audible);
void (*p_vte_terminal_set_background_image_file)(VteTerminal *terminal, const char *path);
void (*p_vte_terminal_set_background_saturation)(VteTerminal *terminal, double saturation);
void (*p_vte_terminal_set_background_tint_color)(VteTerminal *terminal, const GdkColor *color);
void (*p_vte_terminal_set_background_transparent)(VteTerminal *terminal, gboolean transparent);
void (*p_vte_terminal_set_backspace_binding)(VteTerminal *terminal, VteTerminalEraseBinding binding);
void (*p_vte_terminal_set_color_background)(VteTerminal *terminal, const GdkColor *background);
void (*p_vte_terminal_set_color_bold)(VteTerminal *terminal, const GdkColor *bold);
void (*p_vte_terminal_set_color_cursor)(VteTerminal *terminal, const GdkColor *cursor_background);
void (*p_vte_terminal_set_color_dim)(VteTerminal *terminal, const GdkColor *dim);
void (*p_vte_terminal_set_color_foreground)(VteTerminal *terminal, const GdkColor *foreground);
void (*p_vte_terminal_set_color_highlight)(VteTerminal *terminal, const GdkColor *highlight_background);
void (*p_vte_terminal_set_colors)(VteTerminal *terminal, const GdkColor *foreground, const GdkColor *background, const GdkColor *palette, glong palette_size);
void (*p_vte_terminal_set_cursor_blink_mode)(VteTerminal *terminal, VteTerminalCursorBlinkMode mode);
void (*p_vte_terminal_set_cursor_shape)(VteTerminal *terminal, VteTerminalCursorShape shape);
void (*p_vte_terminal_set_default_colors)(VteTerminal *terminal);
void (*p_vte_terminal_set_delete_binding)(VteTerminal *terminal, VteTerminalEraseBinding binding);
void (*p_vte_terminal_set_emulation)(VteTerminal *terminal, const char *emulation);
void (*p_vte_terminal_set_encoding)(VteTerminal *terminal, const char *codeset);
void (*p_vte_terminal_set_font_from_string)(VteTerminal *terminal, const char *name);
void (*p_vte_terminal_set_font_from_string_full)(VteTerminal *terminal, const char *name, VteTerminalAntiAlias antialias);
void (*p_vte_terminal_set_mouse_autohide)(VteTerminal *terminal, gboolean setting);
void (*p_vte_terminal_set_opacity)(VteTerminal *terminal, guint16 opacity);
void (*p_vte_terminal_set_scroll_background)(VteTerminal *terminal, gboolean scroll);
void (*p_vte_terminal_set_scroll_on_keystroke)(VteTerminal *terminal, gboolean scroll);
void (*p_vte_terminal_set_scroll_on_output)(VteTerminal *terminal, gboolean scroll);
void (*p_vte_terminal_set_scrollback_lines)(VteTerminal *terminal, glong lines);
void (*p_vte_terminal_set_size)(VteTerminal *terminal, glong columns, glong rows);
void (*p_vte_terminal_set_visible_bell)(VteTerminal *terminal, gboolean is_visible);
void (*p_vte_terminal_set_word_chars)(VteTerminal *terminal, const char *spec);
unsigned short with_gtk = 0;
void *p_hdl_gtk = NULL;
void *p_hdl_vte = NULL;
bool has_resize_grip = 1;
#endif

#if CLOSE_DIALOG
static GtkWidget* make_close_dialog(void)
{
  GtkWidget *dialog;
  GtkWidget *dialog_hbox;
#if BUTTON_ORDER_BY_RCFILE
  if (button_order)
    dialog = gtk_dialog_new_with_buttons(LABEL_DIALOG_CLOSE, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, NULL);
  else
#endif
    dialog = gtk_dialog_new_with_buttons(LABEL_DIALOG_CLOSE, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE, NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
  dialog_hbox = gtk_hbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(gtk_dialog_get_content_area(GTK_DIALOG(dialog))), dialog_hbox);
  gtk_box_pack_start(GTK_BOX(dialog_hbox), gtk_image_new_from_stock(GTK_STOCK_DIALOG_QUESTION, GTK_ICON_SIZE_DIALOG), FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(dialog_hbox), gtk_label_new(LABEL_DIALOG_CLOSE), TRUE, FALSE, 0);
  return dialog;
}
#endif

static void DEL_TAB_DECL(GtkWidget *widget, bool do_close_dialog)
{
  int index = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
#if CLOSE_DIALOG
  GET_CURRENT_TAB(index);
#if CLOSE_SAFELY
  bool user_want_to_close = FALSE;
#endif
  if (do_close_dialog) {
    /* Known to work on FreeBSD *with* linprocfs mounted */
    char *stat = NULL;
    char **stats = NULL;
    gsize length;
    if (g_file_get_contents(g_strdup_printf("/proc/%d/stat", term->pid), &stat, &length, NULL)) {
      stats = g_strsplit_set(stat, " ", 8);
      if (atoi(stats[7]) != term->pid) {
        GtkWidget *dialog = make_close_dialog();
        gtk_widget_show_all(dialog);
        if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_CLOSE) {
          gtk_widget_destroy(dialog);
#if CLOSE_SAFELY
          user_want_to_close = TRUE;
#endif
        } else {
          gtk_widget_destroy(dialog);
          return;
        }
      }
    }
  }
#endif

#if CLOSE_SAFELY
  GET_CURRENT_TAB(index);
#if CLOSE_DIALOG
  if (user_want_to_close || !do_close_dialog)
    kill(term->pid, SIGKILL);
  else
#endif
  {
    /* Known to work on FreeBSD *with* linprocfs mounted */
    char *stat = NULL;
    char **stats = NULL;
    gsize length;
    if (g_file_get_contents(g_strdup_printf("/proc/%d/stat", term->pid), &stat, &length, NULL)) {
      stats = g_strsplit_set(stat, " ", 8);
      if (atoi(stats[7]) != term->pid)
        return;
      else
        kill(term->pid, SIGKILL);
    }
  }
#endif

  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  g_free(term);
  (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) < 2) ? gtk_main_quit() : gtk_notebook_remove_page(GTK_NOTEBOOK(notebook), index);
  gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), index);

#if TABBAR_AUTOHIDE
  if (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) == 1
#if defined(HOTKEY_TOGGLE_TABBAR) || defined(MENU_TOGGLE_TABBAR)
      && tabbar_status
#endif
     ) {
    gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), FALSE);
#ifdef VTE_FUNNY
    if (!window_fullscreen_status)
#endif
    {
      VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
    }
  }
#endif
}

#if TAB_CLOSE_BUTTON
static void button_clicked(GtkWidget *widget, void *data)
{
  int index = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  int i = 0;
  int killed = 0;
  for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
    GET_CURRENT_TAB(i);
    if (data == term->button) {
      gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), i);
      DEL_TAB(NULL, CLOSE_DIALOG);
      killed = i;
    }
  }
  gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), (killed < index) ? index - 1 : index);
}
#endif

#if TAB_CLOSE_BUTTON
static void tab_close_button(GtkWidget *tab_label)
{
  term->label = gtk_hbox_new(FALSE, 0);
  term->button = gtk_button_new();
  gtk_button_set_image(GTK_BUTTON(term->button), gtk_image_new_from_stock(GTK_STOCK_CLOSE, GTK_ICON_SIZE_MENU));
  gtk_button_set_relief(GTK_BUTTON(term->button), GTK_RELIEF_NONE);
  gtk_button_set_focus_on_click(GTK_BUTTON(term->button), FALSE);
  term->label_edit = tab_label;
#if GTK_CHECK_VERSION(2,91,2) && defined(USE_GTK_GRID) && TAB_EXPANDED_WIDTH
  gtk_widget_set_hexpand(term->label_edit, TRUE);
#endif
  gtk_box_pack_start(GTK_BOX(term->label), term->label_edit, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(term->label), term->button, FALSE, FALSE, 0);
  gtk_widget_show_all(term->label);
  g_signal_connect(term->button, "clicked", G_CALLBACK(button_clicked), term->button);
}
#endif

#if WINDOW_TITLE_DYNAMIC || TAB_LABEL_DYNAMIC
static void do_title_changed(void)
{
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
#if TAB_LABEL_DYNAMIC
#if TAB_CLOSE_BUTTON
  tab_close_button(gtk_label_new(vte_terminal_get_window_title(VTE_TERMINAL(term->vte))));
  gtk_notebook_set_tab_label(GTK_NOTEBOOK(notebook), VTE_HBOX, term->label);
#endif
#if !TAB_CLOSE_BUTTON
  gtk_notebook_set_tab_label_text(GTK_NOTEBOOK(notebook), VTE_HBOX, vte_terminal_get_window_title(VTE_TERMINAL(term->vte)));
#endif
#endif
#if WINDOW_TITLE_DYNAMIC
  gtk_window_set_title(GTK_WINDOW(main_window), vte_terminal_get_window_title(VTE_TERMINAL(term->vte)));
#endif
}
#endif

#if BELL_URGENT
static void do_beep(void)
{
  gtk_window_set_urgency_hint(GTK_WINDOW(main_window), TRUE);
}
#endif

#if MENU || defined(MATCH_STRING_L) || defined(MATCH_STRING_M)
static bool menu_popup(GtkWidget *widget, GdkEventButton *event)
{
#if BELL_URGENT
  gtk_window_set_urgency_hint(GTK_WINDOW(main_window), FALSE);
#endif
#if defined(ONLY_ONE_MENU_ITEM) || defined(MENU_PASTE)
  GdkDisplay *display = gdk_window_get_display(event->window);
#endif

#if defined(MENU_MATCH_STRING_EXEC) || defined(MATCH_STRING_L) || defined(MATCH_STRING_M)
  int tag = -1;
#endif

#ifdef MATCH_STRING_L
  if (event->button == 1) {
    GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
    matched_url = vte_terminal_match_check(VTE_TERMINAL(term->vte), event->x / vte_terminal_get_char_width(VTE_TERMINAL(term->vte)), event->y / vte_terminal_get_char_height(VTE_TERMINAL(term->vte)), &tag);
    if (matched_url != NULL) {
      char new_window_str[256];
      if (event->button == 1)
        g_snprintf(new_window_str, sizeof(new_window_str), "%s '%s' &", MATCH_STRING_L, matched_url);
      system(new_window_str);
      matched_url = NULL;
      return TRUE;
    }
  }
#endif

#ifdef MATCH_STRING_M
  if (event->button == 2) {
    GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
    matched_url = vte_terminal_match_check(VTE_TERMINAL(term->vte), event->x / vte_terminal_get_char_width(VTE_TERMINAL(term->vte)), event->y / vte_terminal_get_char_height(VTE_TERMINAL(term->vte)), &tag);
    if (matched_url != NULL) {
      char new_window_str[256];
      if (event->button == 2)
        g_snprintf(new_window_str, sizeof(new_window_str), "%s '%s' &", MATCH_STRING_M, matched_url);
      system(new_window_str);
      matched_url = NULL;
      return TRUE;
    }
  }
#endif

#if MENU
  if (event->button == 3) {
    GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
#ifdef MENU_COPY
    if (vte_terminal_get_has_selection(VTE_TERMINAL(term->vte))ONLY_ONE_MENU_HANDLE)
      gtk_widget_set_sensitive(menu_copy, TRUE);
    else
      gtk_widget_set_sensitive(menu_copy, FALSE);
#endif
#ifdef MENU_PASTE
    if (gtk_clipboard_wait_is_text_available(gtk_clipboard_get_for_display(display, GDK_SELECTION_CLIPBOARD))ONLY_ONE_MENU_HANDLE)
      gtk_widget_set_sensitive(menu_paste, TRUE);
    else
      gtk_widget_set_sensitive(menu_paste, FALSE);
#endif
#ifdef MENU_FONT_DEFAULT_SIZE
    if ((font_size != font_size_default)ONLY_ONE_MENU_HANDLE)
      gtk_widget_set_sensitive(menu_zoom_100, TRUE);
    else
      gtk_widget_set_sensitive(menu_zoom_100, FALSE);
#endif
#ifdef MENU_MATCH_STRING_EXEC
      matched_url = vte_terminal_match_check(VTE_TERMINAL(term->vte), event->x / vte_terminal_get_char_width(VTE_TERMINAL(term->vte)), event->y / vte_terminal_get_char_height(VTE_TERMINAL(term->vte)), &tag);
      if (matched_url != NULL) {
        gtk_widget_show_all(menu);
        gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, event->button, event->time);
      } else {
        gtk_widget_hide(match_open);
        gtk_widget_hide(match_copy);
        if (match_item)
          gtk_widget_hide(match_item);
#endif
#ifdef MENU_CUSTOM
        if (menu_item_success)
#endif
          gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, event->button, event->time);
#ifdef ONLY_ONE_MENU_ITEM
        if (menu_item_success == 1) {
          gtk_test_widget_send_key(menu, GDK_Down, 0);
          gtk_test_widget_send_key(menu, GDK_Return, 0);
#ifdef RULE_THEM_ALL
          if (with_gtk == 2)
            gdk_display_warp_pointer(display, gdk_display_get_default_screen(display), event->x_root, event->y_root);
          else
#endif
            gdk_device_warp(event->device, gdk_display_get_default_screen(display), event->x_root, event->y_root);
        }
#endif
#ifdef MENU_MATCH_STRING_EXEC
      }
#endif
      return TRUE;
  }
#endif /* MENU */
  return FALSE;
}
#endif

#if defined(HOTKEY_SATURATION_MORE) || defined(HOTKEY_SATURATION_LESS) || MOUSE_CTRL_SATURATION || defined(HOTKEY_SATURATION_DIALOG) || defined(MENU_CHANGE_SATURATION)
static void do_saturation_routine(void)
{
  vte_terminal_set_background_saturation(VTE_TERMINAL(term->vte), saturation_level);
#if BACKGROUND_OPACITY
  vte_terminal_set_opacity(VTE_TERMINAL(term->vte), (1 - saturation_level) * 65535);
#endif
#if (defined(HOTKEY_TOGGLE_BACKGROUND) || defined(MENU_TOGGLE_BACKGROUND)) && defined(TOGGLE_BG_OPACITY)
  if (!strncmp(background_order[background_status], "Opacity", 8))
    vte_terminal_set_opacity(VTE_TERMINAL(term->vte), (1 - saturation_level) * 65535);
#endif
}
#endif

#if defined(HOTKEY_SATURATION_MORE) || defined(HOTKEY_SATURATION_LESS) || MOUSE_CTRL_SATURATION || defined(HOTKEY_SATURATION_DIALOG) || defined(MENU_CHANGE_SATURATION)
static void saturation_routine(void)
{
  int i = 0;
  for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
    GET_CURRENT_TAB(i);
    do_saturation_routine();
  }
}
#endif

#if MOUSE_CTRL_SATURATION
static bool scroll_event(GtkWidget *widget, GdkEventScroll *event)
{
#if HOTKEY
  if ((event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) {
    if (event->direction == GDK_SCROLL_UP) {
      saturation_level += 0.025;
      if (saturation_level > 1)
        saturation_level = 1;
      saturation_routine();
      return TRUE;
    }
    if (event->direction == GDK_SCROLL_DOWN) {
      saturation_level -= 0.025;
      if (saturation_level < 0)
        saturation_level = 0;
      saturation_routine();
      return TRUE;
    }
  }
#endif
  if (event->direction == GDK_SCROLL_RIGHT) {
    saturation_level += 0.025;
    if (saturation_level > 1)
      saturation_level = 1;
    saturation_routine();
    return TRUE;
  }
  if (event->direction == GDK_SCROLL_LEFT) {
    saturation_level -= 0.025;
    if (saturation_level < 0)
      saturation_level = 0;
    saturation_routine();
    return TRUE;
  }
  return FALSE;
}
#endif

#if defined(HOTKEY_TOGGLE_SCROLLBAR) || defined(MENU_TOGGLE_SCROLLBAR)
static void hide_scrollbar(void)
{
    int i = 0;
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
      gtk_widget_hide(term->scrollbar);
    }
}
#endif

static void add_tab(void)
{
#if defined(TAB_LABEL) || defined(TAB_LABEL_CUSTOM)
  GtkWidget *label;
#endif
#if defined(HOTKEY_TAB_EDIT_LABEL) || defined(MENU_TAB_EDIT_LABEL)
  bool label_exist;
#endif
#if USE_ACCESSIBLE && defined(RULE_THEM_ALL)
  bool use_accessible;
#endif
#if !VTE_FORK_CMD_OLD
  char **ivte_argv;
#endif
  int index;

  term = g_malloc(sizeof(struct terminal));
#if defined(HOTKEY_SEARCH_STRING) || defined(HOTKEY_SEARCH_PREVIOUS) || defined(HOTKEY_SEARCH_NEXT)
  term->search_string = gtk_entry_new();
  term->global_string = TRUE;
#endif

#if defined(HOTKEY_TAB_EDIT_LABEL) || defined(MENU_TAB_EDIT_LABEL)
  label_exist = FALSE;
#endif

#ifdef TAB_LABEL
  label = gtk_label_new(g_strdup_printf(gettext(TAB_LABEL), (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) + 1)));
#if defined(HOTKEY_TAB_EDIT_LABEL) || defined(MENU_TAB_EDIT_LABEL)
  label_exist = TRUE;
#endif
#endif

#ifdef TAB_LABEL_CUSTOM
  if (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) < label_style_size) {
    label = gtk_label_new(g_strdup_printf("%s", label_style_custom[gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook))]));
#if defined(HOTKEY_TAB_EDIT_LABEL) || defined(MENU_TAB_EDIT_LABEL)
    label_exist = TRUE;
#endif
  } else
    label = NULL;
#endif

#if TAB_CLOSE_BUTTON
  tab_close_button(label);
#endif

  term->vte = vte_terminal_new();
#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
#endif
  {
    gtk_widget_set_hexpand(term->vte, TRUE);
    gtk_widget_set_vexpand(term->vte, TRUE);
  }

#ifdef SCROLLBAR
  term->hbox = gtk_hbox_new(FALSE, 0);
#ifdef RULE_THEM_ALL
  if (with_gtk == 2)
    term->scrollbar = gtk_vscrollbar_new(vte_terminal_get_adjustment(VTE_TERMINAL(term->vte)));
  else
#endif
    term->scrollbar = gtk_scrollbar_new(GTK_ORIENTATION_VERTICAL, gtk_scrollable_get_vadjustment(GTK_SCROLLABLE(term->vte)));
#if !(SCROLLBAR & 1)
  gtk_box_pack_start(GTK_BOX(term->hbox), term->scrollbar, FALSE, FALSE, 0);
#endif
  gtk_box_pack_start(GTK_BOX(term->hbox), term->vte, TRUE, TRUE, 0);
#if (SCROLLBAR & 1)
  gtk_box_pack_start(GTK_BOX(term->hbox), term->scrollbar, FALSE, FALSE, 0);
#endif
#endif

#if TAB_NEW_PATH_EQUAL_OLD
  if (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 0)
    default_directory = g_file_read_link(g_strdup_printf("/proc/%d/cwd", ((struct terminal*)g_object_get_data(G_OBJECT(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), (gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook))))), "current_tab"))->pid), NULL);
  else
    default_directory = DEFAULT_DIRECTORY;
#endif

#if !VTE_FORK_CMD_OLD
  ivte_argv = NULL;
#if COMMAND_EXEC_PROGRAM
  if (login_shell_flag & 2)
    ivte_argv = default_argv;
  else
#endif
    g_shell_parse_argv(VTE_DEFAULT_COMMAND, NULL, &ivte_argv, NULL);
#if COMMAND_LOGIN_SHELL
  if (login_shell_flag == 1)
    ivte_argv[1] = "-";
#endif
  vte_terminal_fork_command_full(VTE_TERMINAL(term->vte), IVTE_PTY_NO_LASTLOG | IVTE_PTY_NO_UTMP | IVTE_PTY_NO_WTMP | IVTE_PTY_NO_HELPER, VTE_DEFAULT_DIRECTORY, ivte_argv, NULL, G_SPAWN_CHILD_INHERITS_STDIN | G_SPAWN_SEARCH_PATH | G_SPAWN_FILE_AND_ARGV_ZERO, NULL, NULL, GET_VTE_CHILD_PID, NULL);
#endif

#if VTE_FORK_CMD_OLD
#if COMMAND_LOGIN_SHELL
  if (login_shell_flag == 1)
    GET_VTE_CHILD_PID vte_terminal_fork_command(VTE_TERMINAL(term->vte), VTE_DEFAULT_COMMAND, login_shell, NULL, VTE_DEFAULT_DIRECTORY, RECORD_LASTLOG, RECORD_UTMP, RECORD_WTMP);
  else
#endif
    GET_VTE_CHILD_PID vte_terminal_fork_command(VTE_TERMINAL(term->vte), VTE_DEFAULT_COMMAND, default_argv, NULL, VTE_DEFAULT_DIRECTORY, RECORD_LASTLOG, RECORD_UTMP, RECORD_WTMP);
#endif

#if USE_ACCESSIBLE
#ifdef RULE_THEM_ALL
  use_accessible = ((GTK_MAJOR_VERSION == 2) || VTE_CHECK_VERSION(0,29,0));
  if (g_getenv("USE_ACCESSIBLE"))
    use_accessible = strncmp(g_getenv("USE_ACCESSIBLE"), "0", 2);
  if ((with_gtk == 3) && use_accessible)
#endif
    vte_terminal_accessible_new(VTE_TERMINAL(term->vte));
#endif

#if VTE_CHECK_VERSION(0,17,1)
#ifdef MATCH_STRING
  vte_terminal_match_add_gregex(VTE_TERMINAL(term->vte), g_regex_new(MATCH_STRING, 0, 0, NULL), 0);
#endif
#if MATCH_STRING_HTTP
  vte_terminal_match_add_gregex(VTE_TERMINAL(term->vte), g_regex_new(MATCH_HTTP_DATA, 0, 0, NULL), 0);
#endif
#if MATCH_STRING_MAIL
  vte_terminal_match_add_gregex(VTE_TERMINAL(term->vte), g_regex_new(MATCH_MAIL_DATA, 0, 0, NULL), 0);
#endif
#if MATCH_STRING_FILE
  vte_terminal_match_add_gregex(VTE_TERMINAL(term->vte), g_regex_new(MATCH_FILE_DATA, 0, 0, NULL), 0);
#endif
#endif

#ifdef DEFAULT_EMULATION_TYPE
  vte_terminal_set_emulation(VTE_TERMINAL(term->vte), DEFAULT_EMULATION_TYPE);
#endif

#ifdef FONT_ENABLE_BOLD_TEXT
  vte_terminal_set_allow_bold(VTE_TERMINAL(term->vte), FONT_ENABLE_BOLD_TEXT);
#endif

#ifdef BELL_AUDIBLE
  vte_terminal_set_audible_bell(VTE_TERMINAL(term->vte), BELL_AUDIBLE);
#endif

#ifdef BELL_VISIBLE
  vte_terminal_set_visible_bell(VTE_TERMINAL(term->vte), BELL_VISIBLE);
#endif

#ifdef BACKGROUND_IMAGE
  vte_terminal_set_background_image_file(VTE_TERMINAL(term->vte), imgstr);
#endif

#if BACKGROUND_OPACITY
  vte_terminal_set_opacity(VTE_TERMINAL(term->vte), (1 - saturation_level) * 65535);
#endif

#if defined(BACKGROUND_TINT_COLOR) && defined(BACKGROUND_EXIST)
  vte_terminal_set_background_tint_color(VTE_TERMINAL(term->vte), &color_tint);
#endif

#if (defined(BACKGROUND_SATURATION) || COMMAND_SATURATION) && defined(BACKGROUND_EXIST)
  vte_terminal_set_background_saturation(VTE_TERMINAL(term->vte), saturation_level);
#endif

#ifdef BACKGROUND_TRANSPARENT
  vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), BACKGROUND_TRANSPARENT);
#endif

#ifdef BACKSPACE_KEY
  vte_terminal_set_backspace_binding(VTE_TERMINAL(term->vte), BACKSPACE_KEY);
#endif

#ifdef DELETE_KEY
  vte_terminal_set_delete_binding(VTE_TERMINAL(term->vte), DELETE_KEY);
#endif

#ifdef COLOR_STYLE
  vte_terminal_set_colors(VTE_TERMINAL(term->vte), NULL, NULL, color_style, 16);
#endif

#if defined(COLOR_BACKGROUND) || defined(COLOR_TEXT_BOLD) || defined(CURSOR_COLOR) || defined(COLOR_TEXT_DIM) || defined(COLOR_FOREGROUND) || defined(COLOR_TEXT_HIGHLIGHTED) || COMMAND_COLOR_FG || COMMAND_COLOR_BG
#ifndef COLOR_STYLE
  vte_terminal_set_default_colors(VTE_TERMINAL(term->vte));
#endif
#endif

#if defined(COLOR_BACKGROUND) || COMMAND_COLOR_BG
  GdkColor color_bg;
#endif
#ifdef COLOR_BACKGROUND
  gdk_color_parse(COLOR_BACKGROUND, &color_bg);
#endif
#if COMMAND_COLOR_BG
  gdk_color_parse(command_color_bg, &color_bg);
  if (command_color_bg)
#endif
#if defined(COLOR_BACKGROUND) || COMMAND_COLOR_BG
    vte_terminal_set_color_background(VTE_TERMINAL(term->vte), &color_bg);
#endif

#ifdef COLOR_TEXT_BOLD
  GdkColor color_bold;
  gdk_color_parse(COLOR_TEXT_BOLD, &color_bold);
  vte_terminal_set_color_bold(VTE_TERMINAL(term->vte), &color_bold);
#endif

#ifdef CURSOR_COLOR
  GdkColor color_cursor;
  gdk_color_parse(CURSOR_COLOR, &color_cursor);
  vte_terminal_set_color_cursor(VTE_TERMINAL(term->vte), &color_cursor);
#endif

#ifdef COLOR_TEXT_DIM
  GdkColor color_dim;
  gdk_color_parse(COLOR_TEXT_DIM, &color_dim);
  vte_terminal_set_color_dim(VTE_TERMINAL(term->vte), &color_dim);
#endif

#if defined(COLOR_FOREGROUND) || COMMAND_COLOR_FG
  GdkColor color_fg;
#endif
#ifdef COLOR_FOREGROUND
  gdk_color_parse(COLOR_FOREGROUND, &color_fg);
#endif
#if COMMAND_COLOR_FG
  gdk_color_parse(command_color_fg, &color_fg);
  if (command_color_fg)
#endif
#if defined(COLOR_FOREGROUND) || COMMAND_COLOR_FG
    vte_terminal_set_color_foreground(VTE_TERMINAL(term->vte), &color_fg);
#endif

#ifdef COLOR_TEXT_HIGHLIGHTED
  GdkColor color_highlight;
  gdk_color_parse(COLOR_TEXT_HIGHLIGHTED, &color_highlight);
  vte_terminal_set_color_highlight(VTE_TERMINAL(term->vte), &color_highlight);
#endif

#if defined(CURSOR_BLINKS) && VTE_CHECK_VERSION(0,17,1)
#if CURSOR_BLINKS
  vte_terminal_set_cursor_blink_mode(VTE_TERMINAL(term->vte), VTE_CURSOR_BLINK_ON);
#endif
#if !CURSOR_BLINKS
  vte_terminal_set_cursor_blink_mode(VTE_TERMINAL(term->vte), VTE_CURSOR_BLINK_OFF);
#endif
#endif

#if defined(CURSOR_SHAPE) && VTE_CHECK_VERSION(0,19,1)
  vte_terminal_set_cursor_shape(VTE_TERMINAL(term->vte), CURSOR_SHAPE);
#endif

#ifdef DEFAULT_ENCODING
  vte_terminal_set_encoding(VTE_TERMINAL(term->vte), DEFAULT_ENCODING);
#endif

#ifdef FONT
#if defined(HOTKEY_TOGGLE_ANTI_ALIAS) || defined(MENU_TOGGLE_ANTI_ALIAS)
  IVTE_SET_FONT(VTE_TERMINAL(term->vte), font_str, antialias_status);
#endif
#if !defined(HOTKEY_TOGGLE_ANTI_ALIAS) && !defined(MENU_TOGGLE_ANTI_ALIAS)
  IVTE_SET_FONT(VTE_TERMINAL(term->vte), font_str, VTE_ANTI_ALIAS);
#endif
#endif

#ifdef MOUSE_CURSOR_AUTOHIDE
  vte_terminal_set_mouse_autohide(VTE_TERMINAL(term->vte), MOUSE_CURSOR_AUTOHIDE);
#endif

#ifdef BACKGROUND_SCROLLABLE
  vte_terminal_set_scroll_background(VTE_TERMINAL(term->vte), BACKGROUND_SCROLLABLE);
#endif

#ifdef SCROLL_ON_KEYSTROKE
  vte_terminal_set_scroll_on_keystroke(VTE_TERMINAL(term->vte), SCROLL_ON_KEYSTROKE);
#endif

#ifdef SCROLL_ON_OUTPUT
  vte_terminal_set_scroll_on_output(VTE_TERMINAL(term->vte), SCROLL_ON_OUTPUT);
#endif

#ifdef SCROLL_LINES
  vte_terminal_set_scrollback_lines(VTE_TERMINAL(term->vte), SCROLL_LINES);
#endif

#if defined(RULE_THEM_ALL) || !VTE_CHECK_VERSION(0,27,1) || !GTK_CHECK_VERSION(2,91,1)
#if VTE_COLUMNS && VTE_ROWS
#ifdef RULE_THEM_ALL
  if (with_gtk == 2)
#endif
    vte_terminal_set_size(VTE_TERMINAL(term->vte), VTE_COLUMNS, VTE_ROWS);
#endif
#endif

#ifdef WORD_CHARS
  vte_terminal_set_word_chars(VTE_TERMINAL(term->vte), WORD_CHARS);
#endif

  gtk_widget_show_all(VTE_HBOX);

#if TAB_NEW_TAB_AT_TAB_ONE
  index = gtk_notebook_prepend_page(GTK_NOTEBOOK(notebook), VTE_HBOX, VTE_LABEL);
#endif
#if !TAB_NEW_TAB_AT_TAB_ONE
  index = gtk_notebook_append_page(GTK_NOTEBOOK(notebook), VTE_HBOX, VTE_LABEL);
#endif
  g_object_set_data(G_OBJECT(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), index)), "current_tab", term);

  g_signal_connect(term->vte, "child-exited", G_CALLBACK(del_tab), (bool*)CLOSE_DIALOG);

#if WINDOW_TITLE_DYNAMIC || TAB_LABEL_DYNAMIC
  g_signal_connect(term->vte, "window-title-changed", do_title_changed, NULL);
#endif

#if BELL_URGENT
  g_signal_connect(term->vte, "beep", do_beep, NULL);
#endif

#if MENU || defined(MATCH_STRING_L) || defined(MATCH_STRING_M)
  g_signal_connect(term->vte, "button-press-event", G_CALLBACK(menu_popup), NULL);
#endif

#if MOUSE_CTRL_SATURATION
  g_signal_connect(term->vte, "scroll-event", G_CALLBACK(scroll_event), NULL);
#endif

#if TABBAR_AUTOHIDE
  gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) == 1) ? FALSE : VTE_TABBAR);
#ifndef VTE_FUNNY
  VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
#endif
  gtk_widget_set_can_focus(notebook, FALSE);
#endif

#if defined(HOTKEY_TOGGLE_BACKGROUND) || defined(MENU_TOGGLE_BACKGROUND)
#ifdef TOGGLE_BG_TRANSPARENT
  if (!strncmp(background_order[background_status], "Transparent", 12))
    vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), TRUE);
#endif
#ifdef TOGGLE_BG_IMAGE
  if (!strncmp(background_order[background_status], "Image", 6)) {
    vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), FALSE);
    vte_terminal_set_background_image_file(VTE_TERMINAL(term->vte), imgstr);
  }
#endif
#ifdef TOGGLE_BG_NO_BACKGROUND
  if (!strncmp(background_order[background_status], "No background", 14)) {
    vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), FALSE);
    vte_terminal_set_background_image_file(VTE_TERMINAL(term->vte), "/dev/null");
#if defined(TOGGLE_BG_OPACITY) || BACKGROUND_OPACITY
    vte_terminal_set_opacity(VTE_TERMINAL(term->vte), 65535);
#endif
  }
#endif
#ifdef TOGGLE_BG_OPACITY
  if (!strncmp(background_order[background_status], "Opacity", 8)) {
    vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), FALSE);
    vte_terminal_set_background_image_file(VTE_TERMINAL(term->vte), "/dev/null");
    vte_terminal_set_opacity(VTE_TERMINAL(term->vte), (1 - saturation_level) * 65535);
  }
#endif
#endif

#if VTE_CHECK_VERSION(0,27,1) && GTK_CHECK_VERSION(2,91,1) && defined(VTE_FUNNY)
#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
#endif
    gtk_widget_set_size_request(term->vte, VTE_COLUMNS * vte_terminal_get_char_width(VTE_TERMINAL(term->vte)) + INNER_BORDER_W, VTE_ROWS * vte_terminal_get_char_height(VTE_TERMINAL(term->vte)) + INNER_BORDER_H);
#endif

  gtk_widget_show_all(notebook);

#if defined(HOTKEY_TOGGLE_SCROLLBAR) || defined(MENU_TOGGLE_SCROLLBAR)
  if (!scrollbar_status) {
    hide_scrollbar();
#ifndef VTE_FUNNY
    VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
#endif
  }
#endif

  gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), index);

#if TAB_EXPANDED_WIDTH
  gtk_container_child_set(GTK_CONTAINER(notebook), VTE_HBOX, "tab-expand", TRUE, NULL);
#endif
#if TAB_REORDERABLE && GTK_CHECK_VERSION(2,9,0)
  gtk_notebook_set_tab_reorderable(GTK_NOTEBOOK(notebook), VTE_HBOX, TRUE);
#endif

  gtk_window_set_focus(GTK_WINDOW(main_window), term->vte);

#if defined(HOTKEY_TAB_EDIT_LABEL) || defined(MENU_TAB_EDIT_LABEL)
  term->label_exist = label_exist;
#endif
}

#if defined(HOTKEY_SATURATION_DIALOG) || defined(MENU_CHANGE_SATURATION)
static void do_change_saturation(void)
{
  saturation_level = gtk_range_get_value(GTK_RANGE(adjustment));
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  do_saturation_routine();
}
#endif

#if BELL_URGENT
static bool focus_in_event(void)
{
  gtk_window_set_urgency_hint(GTK_WINDOW(main_window), FALSE);
  return FALSE;
}
#endif

#if defined(HOTKEY_TOGGLE_ON_TOP) || defined(MENU_TOGGLE_ON_TOP)
static void do_always_on_top(void)
{
  always_on_top ^= 1;
  gtk_window_set_keep_above(GTK_WINDOW(main_window), always_on_top);
#if COMMAND_AT_ROOT_WINDOW
  if (at_root_window && !always_on_top) {
    gtk_window_set_keep_below(GTK_WINDOW(main_window), TRUE);
    gtk_window_set_decorated(GTK_WINDOW(main_window), FALSE);
    gtk_window_set_skip_taskbar_hint(GTK_WINDOW(main_window), TRUE);
    gtk_window_set_skip_pager_hint(GTK_WINDOW(main_window), TRUE);
  }
#endif
}
#endif

#if defined(HOTKEY_SEARCH_STRING) || defined(HOTKEY_SEARCH_PREVIOUS) || defined(HOTKEY_SEARCH_NEXT)
static void do_edit_search(void)
{
  GtkWidget *entry = gtk_entry_new();
  GtkWidget *dialog;
#if BUTTON_ORDER_BY_RCFILE
  if (button_order)
    dialog = gtk_dialog_new_with_buttons(LABEL_DIALOG_SEARCH, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_OK, GTK_RESPONSE_OK, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, NULL);
  else
#endif
    dialog = gtk_dialog_new_with_buttons(LABEL_DIALOG_SEARCH, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
  if (strlen(gtk_entry_get_text(GTK_ENTRY(term->search_string))))
    gtk_entry_set_text(GTK_ENTRY(entry), gtk_entry_get_text(GTK_ENTRY(term->search_string)));
  else {
    if (term->global_string)
      gtk_entry_set_text(GTK_ENTRY(entry), gtk_entry_get_text(GTK_ENTRY(global_search_string)));
  }
  gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
  gtk_container_add(GTK_CONTAINER(gtk_dialog_get_content_area(GTK_DIALOG(dialog))), entry);
  gtk_widget_show_all(dialog);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK) {
    term->global_string = FALSE;
    if (strlen(gtk_entry_get_text(GTK_ENTRY(entry))))
      gtk_entry_set_text(GTK_ENTRY(global_search_string), gtk_entry_get_text(GTK_ENTRY(entry)));
    gtk_entry_set_text(GTK_ENTRY(term->search_string), gtk_entry_get_text(GTK_ENTRY(entry)));
    vte_terminal_search_set_gregex(VTE_TERMINAL(term->vte), g_regex_new(gtk_entry_get_text(GTK_ENTRY(term->search_string)), VTE_REGEX, 0, NULL));
    vte_terminal_search_set_wrap_around(VTE_TERMINAL(term->vte), TRUE);
  }
  gtk_widget_destroy(dialog);
}
#endif

#if defined(HOTKEY_TAB_EDIT_LABEL) || defined(MENU_TAB_EDIT_LABEL)
static void do_edit_label(void)
{
  char *label_name = "";
  int index = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  GET_CURRENT_TAB(index);
  if (term->label_exist) {
#if TAB_CLOSE_BUTTON
    label_name = (char*)gtk_label_get_text(GTK_LABEL(term->label_edit));
#endif
#if !TAB_CLOSE_BUTTON
    label_name = (char*)gtk_label_get_text(GTK_LABEL(gtk_notebook_get_tab_label(GTK_NOTEBOOK(notebook), gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), index))));
#endif
  }
  GtkWidget *entry = gtk_entry_new();
  GtkWidget *dialog;
#if BUTTON_ORDER_BY_RCFILE
  if (button_order)
    dialog = gtk_dialog_new_with_buttons(label_name, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_OK, GTK_RESPONSE_OK, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, NULL);
  else
#endif
    dialog = gtk_dialog_new_with_buttons(label_name, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
  gtk_entry_set_text(GTK_ENTRY(entry), label_name);
  gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
  gtk_container_add(GTK_CONTAINER(gtk_dialog_get_content_area(GTK_DIALOG(dialog))), entry);
  gtk_widget_show_all(dialog);
  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK) {
#if TAB_CLOSE_BUTTON
    tab_close_button(gtk_label_new(gtk_entry_get_text(GTK_ENTRY(entry))));
    gtk_notebook_set_tab_label(GTK_NOTEBOOK(notebook), VTE_HBOX, term->label);
#endif
#if !TAB_CLOSE_BUTTON
    gtk_notebook_set_tab_label_text(GTK_NOTEBOOK(notebook), VTE_HBOX, gtk_entry_get_text(GTK_ENTRY(entry)));
#endif
    term->label_exist = TRUE;
  }
  gtk_widget_destroy(dialog);
}
#endif

#if defined(HOTKEY_SATURATION_DIALOG) || defined(MENU_CHANGE_SATURATION)
static void do_menu_saturation(void)
{
  double saturation_level_old = saturation_level;
  GtkWidget *dialog;
#if BUTTON_ORDER_BY_RCFILE
  if (button_order)
    dialog = gtk_dialog_new_with_buttons(LABEL_MENU_SATURATION, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_OK, GTK_RESPONSE_OK, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, NULL);
  else
#endif
    dialog = gtk_dialog_new_with_buttons(LABEL_MENU_SATURATION, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
#ifdef RULE_THEM_ALL
  if (with_gtk == 2)
    adjustment = gtk_hscale_new_with_range(0.0, 1.0, 0.01);
  else
#endif
    adjustment = gtk_scale_new_with_range(GTK_ORIENTATION_HORIZONTAL, 0.0, 1.0, 0.01);
  gtk_range_set_value(GTK_RANGE(adjustment), saturation_level_old);
  g_signal_connect_after(adjustment, "change-value", do_change_saturation, NULL);
  gtk_container_add(GTK_CONTAINER(gtk_dialog_get_content_area(GTK_DIALOG(dialog))), adjustment);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
  gtk_widget_show_all(dialog);
  saturation_level = (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK) ? gtk_range_get_value(GTK_RANGE(adjustment)) : saturation_level_old;
  gtk_widget_destroy(dialog);
  saturation_routine();
}
#endif

#if defined(HOTKEY_COLOR_BACKGROUND) || defined(MENU_COLOR_BACKGROUND)
static void do_menu_tint_color(void)
{
  GtkWidget *color_tint_dialog = gtk_color_selection_dialog_new(LABEL_DIALOG_BACKGROUND_TINT);
#if defined(RULE_THEM_ALL) || !GTK_CHECK_VERSION(3,3,8)
  gtk_color_selection_set_current_color(GTK_COLOR_SELECTION(gtk_color_selection_dialog_get_color_selection(GTK_COLOR_SELECTION_DIALOG(color_tint_dialog))), &color_tint);
#endif
#if !defined(RULE_THEM_ALL) && GTK_CHECK_VERSION(3,3,8)
  GdkRGBA rgba_tint;
  gdk_rgba_parse(&rgba_tint, gdk_color_to_string(&color_tint));
  gtk_color_selection_set_current_rgba(GTK_COLOR_SELECTION(gtk_color_selection_dialog_get_color_selection(GTK_COLOR_SELECTION_DIALOG(color_tint_dialog))), &rgba_tint);
#endif
  if (GTK_RESPONSE_OK == gtk_dialog_run(GTK_DIALOG(color_tint_dialog))) {
#if defined(RULE_THEM_ALL) || !GTK_CHECK_VERSION(3,3,8)
    gtk_color_selection_get_current_color(GTK_COLOR_SELECTION(gtk_color_selection_dialog_get_color_selection(GTK_COLOR_SELECTION_DIALOG(color_tint_dialog))), &color_tint);
#endif
#if !defined(RULE_THEM_ALL) && GTK_CHECK_VERSION(3,3,8)
    gtk_color_selection_get_current_rgba(GTK_COLOR_SELECTION(gtk_color_selection_dialog_get_color_selection(GTK_COLOR_SELECTION_DIALOG(color_tint_dialog))), &rgba_tint);
    color_tint.red = rgba_tint.red * 65535;
    color_tint.green = rgba_tint.green * 65535;
    color_tint.blue = rgba_tint.blue * 65535;
#endif
    int i = 0;
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
      vte_terminal_set_background_tint_color(VTE_TERMINAL(term->vte), &color_tint);
    }
  }
  gtk_widget_destroy(GTK_WIDGET(color_tint_dialog));
}
#endif

#ifdef FONT
static void calculate_font(void)
{
    int len = strlen(font_name) - 1;
    if (len < 1) {
      g_snprintf(font_name, sizeof(font_name), FONT);
      len = strlen(font_name) - 1;
    }
    font_size = atoi(strrchr(font_name, ' '));
    if (font_size < 1)
      font_size = 1;
    while (len > 0 && isdigit(font_name[len]))
      font_name[len--] = 0;
    while (len > 0 && font_name[len] == ' ')
      font_name[len--] = 0;
}
#endif

#if defined(HOTKEY_FONT_DEFAULT_SIZE) || defined(MENU_FONT_DEFAULT_SIZE) || defined(HOTKEY_FONT_BIGGER) || defined(MENU_FONT_BIGGER) || defined(HOTKEY_FONT_SMALLER) || defined(MENU_FONT_SMALLER) || defined(HOTKEY_FONT_SELECT) || defined(MENU_FONT_SELECT)
static void do_zoom_routine(void)
{
  g_snprintf(font_str, sizeof(font_str), "%s %d", font_name, font_size);
  int i = 0;
  for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
    GET_CURRENT_TAB(i);
    vte_terminal_set_font_from_string(VTE_TERMINAL(term->vte), font_str);
#ifdef VTE_FUNNY
#ifdef RULE_THEM_ALL
    if (with_gtk == 3)
#endif
      gtk_widget_set_size_request(term->vte, VTE_COLUMNS * vte_terminal_get_char_width(VTE_TERMINAL(term->vte)) + INNER_BORDER_W, VTE_ROWS * vte_terminal_get_char_height(VTE_TERMINAL(term->vte)) + INNER_BORDER_H);
#endif
  }
#ifndef VTE_FUNNY
  VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
#endif
}
#endif

#if defined(HOTKEY_FONT_SELECT) || defined(MENU_FONT_SELECT)
static void do_select_font(void)
{
  GtkWidget *font_dialog = gtk_font_selection_dialog_new(font_str);
  gtk_font_selection_dialog_set_font_name(GTK_FONT_SELECTION_DIALOG(font_dialog), font_str);
  if (GTK_RESPONSE_OK == gtk_dialog_run(GTK_DIALOG(font_dialog))) {
    g_snprintf(font_name, sizeof(font_name), "%s", gtk_font_selection_dialog_get_font_name(GTK_FONT_SELECTION_DIALOG(font_dialog)));
    calculate_font();
    do_zoom_routine();
  }
  gtk_widget_destroy(font_dialog);
}
#endif

#if defined(HOTKEY_TOGGLE_ANTI_ALIAS) || defined(MENU_TOGGLE_ANTI_ALIAS)
static void do_toggle_antialias(void)
{
  antialias_status++;
  if (antialias_status > VTE_ANTI_ALIAS_FORCE_DISABLE)
    antialias_status = VTE_ANTI_ALIAS_FORCE_ENABLE;
  int i = 0;
  for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
    GET_CURRENT_TAB(i);
    vte_terminal_set_font_from_string_full(VTE_TERMINAL(term->vte), font_str, antialias_status);
  }
}
#endif

#if defined(HOTKEY_TOGGLE_BACKGROUND) || defined(MENU_TOGGLE_BACKGROUND)
static void do_toggle_bg(void)
{
  int i = 0;
  background_status++;
  if (background_status >= TOGGLE_BG_ORDER_SIZE)
    background_status = 0;
#ifdef TOGGLE_BG_TRANSPARENT
  if (!strncmp(background_order[background_status], "Transparent", 12)) {
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
      vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), TRUE);
    }
  }
#endif
#ifdef TOGGLE_BG_IMAGE
  if (!strncmp(background_order[background_status], "Image", 6)) {
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
      vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), FALSE);
      vte_terminal_set_background_image_file(VTE_TERMINAL(term->vte), imgstr);
    }
  }
#endif
#ifdef TOGGLE_BG_NO_BACKGROUND
  if (!strncmp(background_order[background_status], "No background", 14)) {
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
      vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), FALSE);
      vte_terminal_set_background_image_file(VTE_TERMINAL(term->vte), "/dev/null");
#if defined(TOGGLE_BG_OPACITY) || BACKGROUND_OPACITY
      vte_terminal_set_opacity(VTE_TERMINAL(term->vte), 65535);
#endif
    }
  }
#endif
#ifdef TOGGLE_BG_OPACITY
  if (!strncmp(background_order[background_status], "Opacity", 8)) {
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
      vte_terminal_set_background_transparent(VTE_TERMINAL(term->vte), FALSE);
      vte_terminal_set_background_image_file(VTE_TERMINAL(term->vte), "/dev/null");
      vte_terminal_set_opacity(VTE_TERMINAL(term->vte), (1 - saturation_level) * 65535);
    }
  }
#endif
}
#endif

#if defined(HOTKEY_TOGGLE_DECORATED) || defined(MENU_TOGGLE_DECORATED)
static void do_toggle_decorated(void)
{
  window_decorated_status ^= 1;
  gtk_window_set_decorated(GTK_WINDOW(main_window), window_decorated_status);
  int index = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  GET_CURRENT_TAB(index);
  gtk_widget_hide(main_window);
  gtk_widget_show_all(main_window);
#if defined(HOTKEY_TOGGLE_SCROLLBAR) || defined(MENU_TOGGLE_SCROLLBAR)
  if (!scrollbar_status)
    hide_scrollbar();
#endif
#if defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
  if (status_bar_status) {
#ifdef RULE_THEM_ALL
    if (has_resize_grip)
#endif
    {
      gtk_window_set_has_resize_grip(GTK_WINDOW(main_window), status_bar_resize_grip);
    }
  } else {
    gtk_widget_hide(statusbar);
#ifdef RULE_THEM_ALL
    if (has_resize_grip)
#endif
    {
      gtk_window_set_has_resize_grip(GTK_WINDOW(main_window), FALSE);
    }
  }
#endif
#ifndef VTE_FUNNY
  VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
#endif
  gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), index);
  gtk_window_set_focus(GTK_WINDOW(main_window), term->vte);
}
#endif

#if defined(HOTKEY_TOGGLE_SCROLLBAR) || defined(MENU_TOGGLE_SCROLLBAR)
static void do_toggle_scrollbar(void)
{
  scrollbar_status ^= 1;
  if (scrollbar_status) {
    int i = 0;
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
      gtk_widget_show(term->scrollbar);
    }
  } else
    hide_scrollbar();
#ifndef VTE_FUNNY
  VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
#endif
}
#endif

#if defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
static void do_toggle_status_bar(void)
{
  status_bar_status ^= 1;
  if (status_bar_status) {
    gtk_widget_show(statusbar);
#ifdef RULE_THEM_ALL
    if (has_resize_grip)
#endif
    {
      gtk_window_set_has_resize_grip(GTK_WINDOW(main_window), status_bar_resize_grip);
    }
  } else {
    gtk_widget_hide(statusbar);
#ifdef RULE_THEM_ALL
    if (has_resize_grip)
#endif
    {
      gtk_window_set_has_resize_grip(GTK_WINDOW(main_window), FALSE);
    }
  }
#ifndef VTE_FUNNY
  VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
#endif
}
#endif

#if defined(HOTKEY_TOGGLE_TABBAR) || defined(MENU_TOGGLE_TABBAR)
static void do_toggle_tabbar(void)
{
#if TABBAR_AUTOHIDE
  tabbar_status = gtk_notebook_get_show_tabs(GTK_NOTEBOOK(notebook));
#endif
  tabbar_status ^= 1;
  gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), tabbar_status);
#ifndef VTE_FUNNY
  VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
#endif
  gtk_widget_set_can_focus(notebook, FALSE);
}
#endif

#if defined(HOTKEY_FONT_DEFAULT_SIZE) || defined(MENU_FONT_DEFAULT_SIZE)
static void do_zoom_100(void)
{
  font_size = font_size_default;
  do_zoom_routine();
}
#endif

#if defined(HOTKEY_FONT_BIGGER) || defined(MENU_FONT_BIGGER)
static void do_zoom_in(void)
{
  font_size++;
  do_zoom_routine();
}
#endif

#if defined(HOTKEY_FONT_SMALLER) || defined(MENU_FONT_SMALLER)
static void do_zoom_out(void)
{
  if (font_size > 1) {
    font_size--;
    do_zoom_routine();
  }
}
#endif

#ifdef HOTKEY_HAS_DEFINE
static bool key_press_event(GtkWidget *widget, GdkEventKey *event)
{
#if BELL_URGENT
  gtk_window_set_urgency_hint(GTK_WINDOW(main_window), FALSE);
#endif

#ifdef HOTKEY_TOGGLE_HOTKEYS
    if (HOTKEY_TOGGLE_HOTKEYS) {
      hotkey_status ^= 1;
      return TRUE;
    }
#endif

#if defined(HOTKEY_TOGGLE_HOTKEYS) || defined(MENU_TOGGLE_HOTKEYS)
    if (hotkey_status)
#endif
    {
#ifdef HOTKEY_COLOR_BACKGROUND
      if (HOTKEY_COLOR_BACKGROUND) {
        do_menu_tint_color();
        return TRUE;
      }
#endif

#ifdef HOTKEY_SATURATION_DIALOG
      if (HOTKEY_SATURATION_DIALOG) {
        do_menu_saturation();
        return TRUE;
      }
#endif

#ifdef HOTKEY_TOGGLE_ANTI_ALIAS
      if (HOTKEY_TOGGLE_ANTI_ALIAS) {
        do_toggle_antialias();
        return TRUE;
      }
#endif

#ifdef HOTKEY_TOGGLE_DECORATED
      if (HOTKEY_TOGGLE_DECORATED) {
        do_toggle_decorated();
        return TRUE;
      }
#endif

#ifdef HOTKEY_TOGGLE_FULLSCREEN
      if (HOTKEY_TOGGLE_FULLSCREEN) {
        window_fullscreen_status ^= 1;
        window_fullscreen_status ? gtk_window_maximize(GTK_WINDOW(main_window)) : gtk_window_unmaximize(GTK_WINDOW(main_window));
        return TRUE;
      }
#endif

#ifdef HOTKEY_TOGGLE_TABBAR
      if (HOTKEY_TOGGLE_TABBAR) {
        do_toggle_tabbar();
        return TRUE;
      }
#endif

#ifdef HOTKEY_TOGGLE_STATUS_BAR
      if (HOTKEY_TOGGLE_STATUS_BAR) {
        do_toggle_status_bar();
        return TRUE;
      }
#endif

#if defined(HOTKEY_TOGGLE_SCROLLBAR) && defined(SCROLLBAR)
      if (HOTKEY_TOGGLE_SCROLLBAR) {
        do_toggle_scrollbar();
        return TRUE;
      }
#endif

#ifdef HOTKEY_OPEN_NEW_WINDOW
      if (HOTKEY_OPEN_NEW_WINDOW) {
#if TAB_NEW_PATH_EQUAL_OLD
        char new_window_str[512];
        default_directory = g_file_read_link(g_strdup_printf("/proc/%d/cwd", ((struct terminal*)g_object_get_data(G_OBJECT(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), (gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook))))), "current_tab"))->pid), NULL);
        g_snprintf(new_window_str, sizeof(new_window_str), "cd '%s' ; %s &", default_directory, PROGRAM_NAME);
        system(new_window_str);
#endif
#if !TAB_NEW_PATH_EQUAL_OLD
        system(PROGRAM_NAME " &");
#endif
        return TRUE;
      }
#endif

#ifdef HOTKEY_TOGGLE_ON_TOP
      if (HOTKEY_TOGGLE_ON_TOP) {
        do_always_on_top();
        return TRUE;
      }
#endif

#ifdef HOTKEY_SEARCH_STRING
      if (HOTKEY_SEARCH_STRING) {

        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        do_edit_search();
        vte_terminal_search_find_previous(VTE_TERMINAL(term->vte));

        return TRUE;
      }
#endif

#ifdef HOTKEY_SEARCH_PREVIOUS
      if (HOTKEY_SEARCH_PREVIOUS) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        if (strlen(gtk_entry_get_text(GTK_ENTRY(term->search_string))))
          vte_terminal_search_find_previous(VTE_TERMINAL(term->vte));
        else {
          if (term->global_string && strlen(gtk_entry_get_text(GTK_ENTRY(global_search_string)))) {
            term->global_string = FALSE;
            gtk_entry_set_text(GTK_ENTRY(term->search_string), gtk_entry_get_text(GTK_ENTRY(global_search_string)));
            vte_terminal_search_set_gregex(VTE_TERMINAL(term->vte), g_regex_new(gtk_entry_get_text(GTK_ENTRY(term->search_string)), VTE_REGEX, 0, NULL));
            vte_terminal_search_set_wrap_around(VTE_TERMINAL(term->vte), TRUE);
          } else
            do_edit_search();
          vte_terminal_search_find_previous(VTE_TERMINAL(term->vte));
        }
        return TRUE;
      }
#endif

#ifdef HOTKEY_SEARCH_NEXT
      if (HOTKEY_SEARCH_NEXT) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        if (strlen(gtk_entry_get_text(GTK_ENTRY(term->search_string))))
          vte_terminal_search_find_next(VTE_TERMINAL(term->vte));
        else {
          if (term->global_string && strlen(gtk_entry_get_text(GTK_ENTRY(global_search_string)))) {
            term->global_string = FALSE;
            gtk_entry_set_text(GTK_ENTRY(term->search_string), gtk_entry_get_text(GTK_ENTRY(global_search_string)));
            vte_terminal_search_set_gregex(VTE_TERMINAL(term->vte), g_regex_new(gtk_entry_get_text(GTK_ENTRY(term->search_string)), VTE_REGEX, 0, NULL));
            vte_terminal_search_set_wrap_around(VTE_TERMINAL(term->vte), TRUE);
          } else
            do_edit_search();
          vte_terminal_search_find_next(VTE_TERMINAL(term->vte));
        }
        return TRUE;
      }
#endif

#ifdef HOTKEY_SELECT_ALL
      if (HOTKEY_SELECT_ALL) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        vte_terminal_select_all(VTE_TERMINAL(term->vte));
        return TRUE;
      }
#endif

#ifdef HOTKEY_EDIT_ENCODING
      if (HOTKEY_EDIT_ENCODING) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        const char *encoding_name = vte_terminal_get_encoding(VTE_TERMINAL(term->vte));
        GtkWidget *encoding_entry = gtk_entry_new();
        GtkWidget *dialog;
#if BUTTON_ORDER_BY_RCFILE
        if (button_order)
          dialog = gtk_dialog_new_with_buttons(encoding_name, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_OK, GTK_RESPONSE_OK, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, NULL);
        else
#endif
          dialog = gtk_dialog_new_with_buttons(encoding_name, GTK_WINDOW(main_window), GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
        gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
        gtk_entry_set_text(GTK_ENTRY(encoding_entry), encoding_name);
        gtk_entry_set_activates_default(GTK_ENTRY(encoding_entry), TRUE);
        gtk_container_add(GTK_CONTAINER(gtk_dialog_get_content_area(GTK_DIALOG(dialog))), encoding_entry);
        gtk_widget_show_all(dialog);
        if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK) {
          vte_terminal_set_encoding(VTE_TERMINAL(term->vte), gtk_entry_get_text(GTK_ENTRY(encoding_entry)));
#if STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
          gtk_statusbar_push(GTK_STATUSBAR(statusbar), 0, vte_terminal_get_encoding(VTE_TERMINAL(term->vte)));
#endif
        }
        gtk_widget_destroy(dialog);
        return TRUE;
      }
#endif

#ifdef HOTKEY_TAB_EDIT_LABEL
      if (HOTKEY_TAB_EDIT_LABEL) {

        //do_edit_label();

	// CK START

  if (true) {
    GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
#ifdef MENU_COPY
    if (vte_terminal_get_has_selection(VTE_TERMINAL(term->vte))ONLY_ONE_MENU_HANDLE)
      gtk_widget_set_sensitive(menu_copy, TRUE);
    else
      gtk_widget_set_sensitive(menu_copy, FALSE);
#endif
#ifdef MENU_PASTE
    //GdkDisplay *display = gdk_window_get_display(event->window);
    if (gtk_clipboard_wait_is_text_available(gtk_clipboard_get_for_display(gdk_window_get_display(event->window), GDK_SELECTION_CLIPBOARD))ONLY_ONE_MENU_HANDLE)
      gtk_widget_set_sensitive(menu_paste, TRUE);
    else
      gtk_widget_set_sensitive(menu_paste, FALSE);
#endif
#ifdef MENU_FONT_DEFAULT_SIZE
    if ((font_size != font_size_default)ONLY_ONE_MENU_HANDLE)
      gtk_widget_set_sensitive(menu_zoom_100, TRUE);
    else
      gtk_widget_set_sensitive(menu_zoom_100, FALSE);
#endif
#ifdef MENU_CUSTOM
        if (menu_item_success)
#endif
          gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, 3, event->time);
#ifdef ONLY_ONE_MENU_ITEM
        if (menu_item_success == 1) {
          gtk_test_widget_send_key(menu, GDK_Down, 0);
          gtk_test_widget_send_key(menu, GDK_Return, 0);
#ifdef RULE_THEM_ALL
          if (with_gtk == 2)
            gdk_display_warp_pointer(display, gdk_display_get_default_screen(display), event->x_root, event->y_root);
          else
#endif
            gdk_device_warp(event->device, gdk_display_get_default_screen(display), event->x_root, event->y_root);
        }
#endif
 
  }




	// CK END
        return TRUE;
      }
#endif

#if ALT_NUMBER_GO_TO_TAB_NUMBER || CTRL_NUMBER_GO_TO_TAB_NUMBER
      bool mask_is_pressed = 0;
#endif
#if ALT_NUMBER_GO_TO_TAB_NUMBER
      if ((event->state & GDK_MOD1_MASK) == GDK_MOD1_MASK)
        mask_is_pressed = 1;
#endif
#if CTRL_NUMBER_GO_TO_TAB_NUMBER
      if ((event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK)
        mask_is_pressed = 1;
#endif
#if ALT_NUMBER_GO_TO_TAB_NUMBER || CTRL_NUMBER_GO_TO_TAB_NUMBER
      if (mask_is_pressed) {
        if (event->keyval == GDK_1) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 0);
          return TRUE;
        }
        if ((event->keyval == GDK_2) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 1)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 1);
          return TRUE;
        }
        if ((event->keyval == GDK_3) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 2)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 2);
          return TRUE;
        }
        if ((event->keyval == GDK_4) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 3)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 3);
          return TRUE;
        }
        if ((event->keyval == GDK_5) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 4)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 4);
          return TRUE;
        }
        if ((event->keyval == GDK_6) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 5)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 5);
          return TRUE;
        }
        if ((event->keyval == GDK_7) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 6)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 6);
          return TRUE;
        }
        if ((event->keyval == GDK_8) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 7)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 7);
          return TRUE;
        }
        if ((event->keyval == GDK_9) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 8)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 8);
          return TRUE;
        }
        if ((event->keyval == GDK_0) && (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 9)) {
          gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 9);
          return TRUE;
        }
      }
#endif

#ifdef HOTKEY_TOGGLE_BACKGROUND
      if (HOTKEY_TOGGLE_BACKGROUND) {
        do_toggle_bg();
        return TRUE;
      }
#endif

#ifdef HOTKEY_SATURATION_MORE
      if (HOTKEY_SATURATION_MORE) {
        saturation_level += 0.1;
        if (saturation_level > 1)
          saturation_level = 1;
        saturation_routine();
        return TRUE;
      }
#endif

#ifdef HOTKEY_SATURATION_LESS
      if (HOTKEY_SATURATION_LESS) {
        saturation_level -= 0.1;
        if (saturation_level < 0)
          saturation_level = 0;
        saturation_routine();
        return TRUE;
      }
#endif

#ifdef HOTKEY_RESET_TERMINAL
      if (HOTKEY_RESET_TERMINAL) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        vte_terminal_reset(VTE_TERMINAL(term->vte), TRUE, FALSE);
        return TRUE;
      }
#endif

#ifdef HOTKEY_RESET_AND_CLEAR
      if (HOTKEY_RESET_AND_CLEAR) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        vte_terminal_reset(VTE_TERMINAL(term->vte), TRUE, TRUE);
        return TRUE;
      }
#endif

#ifdef HOTKEY_COPY
      if (HOTKEY_COPY) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        vte_terminal_copy_clipboard(VTE_TERMINAL(term->vte));
        return TRUE;
      }
#endif

#ifdef HOTKEY_PASTE
      if (HOTKEY_PASTE) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        vte_terminal_paste_clipboard(VTE_TERMINAL(term->vte));
        return TRUE;
      }
#endif

#ifdef HOTKEY_FONT_BIGGER
      if (HOTKEY_FONT_BIGGER) {
        do_zoom_in();
        return TRUE;
      }
#endif

#ifdef HOTKEY_FONT_SMALLER
      if (HOTKEY_FONT_SMALLER) {
        do_zoom_out();
        return TRUE;
      }
#endif

#ifdef HOTKEY_FONT_DEFAULT_SIZE
      if (HOTKEY_FONT_DEFAULT_SIZE) {
        do_zoom_100();
        return TRUE;
      }
#endif

#ifdef HOTKEY_FONT_SELECT
      if (HOTKEY_FONT_SELECT) {
        do_select_font();
        return TRUE;
      }
#endif

#ifdef HOTKEY_TAB_FIRST
      if (HOTKEY_TAB_FIRST) {
        gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 0);
        return TRUE;
      }
#endif

#ifdef HOTKEY_TAB_LAST
      if (HOTKEY_TAB_LAST) {
        gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) - 1);
        return TRUE;
      }
#endif

#ifdef HOTKEY_SCROLL_ONE_PAGE_UP
      if (((event->state & GDK_SHIFT_MASK) == GDK_SHIFT_MASK) && (event->keyval == GDK_Page_Up))
        return FALSE;
      if (HOTKEY_SCROLL_ONE_PAGE_UP) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        gtk_test_widget_send_key(term->vte, GDK_Page_Up, GDK_SHIFT_MASK);
        return TRUE;
      }
#endif

#ifdef HOTKEY_SCROLL_ONE_PAGE_DOWN
      if (((event->state & GDK_SHIFT_MASK) == GDK_SHIFT_MASK) && (event->keyval == GDK_Page_Down))
        return FALSE;
      if (HOTKEY_SCROLL_ONE_PAGE_DOWN) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        gtk_test_widget_send_key(term->vte, GDK_Page_Down, GDK_SHIFT_MASK);
        return TRUE;
      }
#endif

#ifdef HOTKEY_MIMIC_SCROLL_UP
      if (HOTKEY_MIMIC_SCROLL_UP) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        gtk_test_widget_click(term->vte, 4, 0);
        return TRUE;
      }
#endif

#ifdef HOTKEY_MIMIC_SCROLL_DOWN
      if (HOTKEY_MIMIC_SCROLL_DOWN) {
        GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
        gtk_test_widget_click(term->vte, 5, 0);
        return TRUE;
      }
#endif

#ifdef HOTKEY_TAB_PREVIOUS
      if (HOTKEY_TAB_PREVIOUS) {
        int index = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
        gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), index ? index - 1 : gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) - 1);
        return TRUE;
      }
#endif

#ifdef HOTKEY_TAB_NEXT
      if (HOTKEY_TAB_NEXT) {
        int index = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
        gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), (index == (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) - 1)) ? 0 : index + 1);
        return TRUE;
      }
#endif

#ifdef HOTKEY_TAB_ADD
      if (HOTKEY_TAB_ADD) {
	fprintf(stderr,"Adding a tab\n");
        add_tab();
        return TRUE;
      }
#endif

#ifdef HOTKEY_TAB_REMOVE
      if (HOTKEY_TAB_REMOVE) {
        DEL_TAB(NULL, CLOSE_DIALOG);
        return TRUE;
      }
#endif
    }
  return FALSE;
}
#endif /* HOTKEY_HAS_DEFINE */

#if CLOSE_DIALOG
static bool delete_event(void) {
  if (gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) > 1) {
    GtkWidget *dialog = make_close_dialog();
    gtk_widget_show_all(dialog);
    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_CLOSE) {
      gtk_widget_destroy(dialog);
#if !CLOSE_SAFELY
      gtk_main_quit();
#endif
    } else {
      gtk_widget_destroy(dialog);
      return TRUE;
    }
  }
#endif
#if !CLOSE_DIALOG
static void delete_event(void) {
#endif
  int i = 0;
  for (i = gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i > 0 ; i--) {
    gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), i - 1);
    DEL_TAB(NULL, 0);
  }
#if CLOSE_DIALOG
  return FALSE;
#endif
}

#ifdef MENU_RESET_AND_CLEAR
static void do_clear(void)
{
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  vte_terminal_reset(VTE_TERMINAL(term->vte), TRUE, TRUE);
}
#endif

#ifdef MENU_COPY
static void do_copy(void)
{
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  vte_terminal_copy_clipboard(VTE_TERMINAL(term->vte));
}
#endif

#ifdef MENU_MATCH_STRING_EXEC
static void do_match_copy(void)
{
  gtk_clipboard_set_text(gtk_clipboard_get(GDK_SELECTION_CLIPBOARD), matched_url, -1);
  matched_url = NULL;
}
#endif

#ifdef MENU_MATCH_STRING_EXEC
static void do_match_open(void)
{
  char new_window_str[256];
  g_snprintf(new_window_str, sizeof(new_window_str), "%s '%s' &", MENU_MATCH_STRING_EXEC, matched_url);
  system(new_window_str);
  matched_url = NULL;
}
#endif

#ifdef MENU_OPEN_NEW_WINDOW
static void do_new_window(void)
{
  system(PROGRAM_NAME " &");
}
#endif

#ifdef MENU_PASTE
static void do_paste(void)
{
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  vte_terminal_paste_clipboard(VTE_TERMINAL(term->vte));
}
#endif

#ifdef MENU_RESET_TERMINAL
static void do_reset(void)
{
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  vte_terminal_reset(VTE_TERMINAL(term->vte), TRUE, FALSE);
}
#endif

#ifdef MENU_SELECT_ALL
static void do_select_all(void)
{
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  vte_terminal_select_all(VTE_TERMINAL(term->vte));
}
#endif

#ifdef MENU_TOGGLE_FULLSCREEN
static void do_toggle_fullscreen(void)
{
  window_fullscreen_status ^= 1;
  window_fullscreen_status ? gtk_window_maximize(GTK_WINDOW(main_window)) : gtk_window_unmaximize(GTK_WINDOW(main_window));
}
#endif

#ifdef MENU_TOGGLE_HOTKEYS
static void do_toggle_hotkeys(void)
{
  hotkey_status ^= 1;
}
#endif

#if MENU
static void set_encoding(GtkWidget *widget, char *data)
{
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  vte_terminal_set_encoding(VTE_TERMINAL(term->vte), data);
#if STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
  gtk_statusbar_push(GTK_STATUSBAR(statusbar), 0, data);
#endif
}
#endif

static void switch_page(void)
{
#if STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR) || WINDOW_TITLE_DYNAMIC
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
#endif
#if STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
  gtk_statusbar_push(GTK_STATUSBAR(statusbar), 0, vte_terminal_get_encoding(VTE_TERMINAL(term->vte)));
#endif
#if TAB_SHOW_INFO_AT_TITLE
  int index = gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook));
  if (index == 1)
    gtk_window_set_title(GTK_WINDOW(main_window), VTE_PROGRAM_NAME);
  else {
    char tabtitle[64];
    g_snprintf(tabtitle, sizeof(tabtitle), "%s - tab %d of %d", VTE_PROGRAM_NAME, gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)) + 1, index);
    gtk_window_set_title(GTK_WINDOW(main_window), tabtitle);
  }
#endif
#if WINDOW_TITLE_DYNAMIC
  gtk_window_set_title(GTK_WINDOW(main_window), vte_terminal_get_window_title(VTE_TERMINAL(term->vte)));
#endif
}

#if defined(HOTKEY_TOGGLE_FULLSCREEN) || defined(MENU_TOGGLE_FULLSCREEN) || defined(VTE_FUNNY)
static void window_state_event(GtkWidget *widget, GdkEventWindowState *event)
{
  window_fullscreen_status = (event->new_window_state & GDK_WINDOW_STATE_MAXIMIZED) ? 1 : 0;
}
#endif

int main(int argc, char **argv)
{
#if COMMAND_DOCK_MODE
bool at_dock_mode = FALSE;
#endif

#if COMMAND_EXEC_PROGRAM || COMMAND_TAB_NUMBERS || defined(MENU_ENCODING_LIST)
  int i = 0;
#endif
#if COMMAND_AT_ROOT_WINDOW || COMMAND_DOCK_MODE || COMMAND_FULLSCREEN || COMMAND_LOGIN_SHELL || PROGRAM_WM_CLASS || COMMAND_SET_TITLE || COMMAND_FONT || COMMAND_COLOR_FG || COMMAND_COLOR_BG || COMMAND_SATURATION || COMMAND_GEOMETRY || COMMAND_SHOW_HELP || COMMAND_SHOW_OPTIONS || COMMAND_SHOW_VERSION || COMMAND_TAB_NUMBERS || defined(RULE_THEM_ALL) || defined(MENU_CUSTOM)
  int j = 0;
#endif

#if COMMAND_EXEC_PROGRAM
#if !VTE_FORK_CMD_OLD
  bool change_command = 0;
#endif
  g_snprintf(default_command, sizeof(default_command), "%s", DEFAULT_COMMAND);
  if (argc > 2) {
    for (i = 0 ; i < argc ; i++) {
      if (!strncmp(argv[i], "-e", 3)) {
        if (i < argc - 1) {
          g_snprintf(default_command, sizeof(default_command), "%s", argv[i + 1]);
#if COMMAND_LOGIN_SHELL || !VTE_FORK_CMD_OLD
          login_shell_flag = 2;
#endif
        }
        if (++i != argc)
#if VTE_FORK_CMD_OLD
          default_argv = &(argv[i]);
#endif
#if !VTE_FORK_CMD_OLD
          default_argv = &(argv[i - 1]);
        change_command = 1;
#endif
        break;
      }
    }
  }
#endif

#if PROGRAM_WM_CLASS
  if (g_getenv("RESOURCE_NAME"))
    wm_class_name = (char*)g_getenv("RESOURCE_NAME");
#endif

#if COMMAND_TAB_NUMBERS
  i = 1;
#endif

#if COMMAND_AT_ROOT_WINDOW || COMMAND_DOCK_MODE || COMMAND_LOGIN_SHELL || PROGRAM_WM_CLASS || COMMAND_SET_TITLE || COMMAND_FONT || COMMAND_COLOR_FG || COMMAND_COLOR_BG || COMMAND_SATURATION || COMMAND_GEOMETRY || COMMAND_SHOW_HELP || COMMAND_SHOW_OPTIONS || COMMAND_SHOW_VERSION || COMMAND_TAB_NUMBERS || defined(RULE_THEM_ALL)
  j = 1;
  while ((j < argc) && strncmp(argv[j], "-e", 3)) {
#if PROGRAM_WM_CLASS
    if (argc > (j + 1) && !strncmp(argv[j], "--name", 7))
      wm_class_name = argv[j + 1];
    if (argc > (j + 1) && !strncmp(argv[j], "--class", 8))
      wm_class_class = argv[j + 1];
#endif

#if COMMAND_SET_TITLE
    if (argc > (j + 1) && (!strncmp(argv[j], "-title", 7) || !strncmp(argv[j], "-T", 3)))
      program_name = argv[j + 1];
#endif

#if COMMAND_FONT
    if (argc > (j + 1) && !strncmp(argv[j], "-fn", 4))
      command_font = argv[j + 1];
#endif

#if COMMAND_COLOR_FG
    if (argc > (j + 1) && !strncmp(argv[j], "-fg", 4))
      command_color_fg = argv[j + 1];
#endif

#if COMMAND_COLOR_BG
    if (argc > (j + 1) && !strncmp(argv[j], "-bg", 4))
      command_color_bg = argv[j + 1];
#endif

#if COMMAND_SATURATION
    if (argc > (j + 1) && !strncmp(argv[j], "-sa", 4))
      saturation_level = strtod(argv[j + 1], NULL);
#endif

#if COMMAND_GEOMETRY
    if (argc > (j + 1) && !strncmp(argv[j], "-g", 3))
      command_geometry = argv[j + 1];
#endif

#if COMMAND_SHOW_VERSION
    if (!strncmp(argv[j], "-v", 3)) {
      printf("%s, version %s\n", PROGRAM_NAME, PROGRAM_VERSION);
      return FALSE;
    }
#endif

#if COMMAND_SHOW_OPTIONS
    if (!strncmp(argv[j], "-o", 3)) {
      system("showvte");
      return FALSE;
    }
#endif

#if COMMAND_SHOW_HELP
    if (!strncmp(argv[j], "-h", 3)) {
      printf("%s, version %s\n\nUsage:\n\t%s [options]\n\nOption%s:\n", PROGRAM_NAME, PROGRAM_VERSION, PROGRAM_NAME,
#if COMMAND_AT_ROOT_WINDOW || COMMAND_DOCK_MODE || COMMAND_EXEC_PROGRAM || COMMAND_FULLSCREEN || COMMAND_LOGIN_SHELL || COMMAND_SET_TITLE || COMMAND_FONT || COMMAND_COLOR_FG || COMMAND_COLOR_BG || COMMAND_SATURATION || COMMAND_GEOMETRY || COMMAND_SHOW_OPTIONS || COMMAND_SHOW_VERSION || COMMAND_TAB_NUMBERS
             "s");
#endif
#if !COMMAND_AT_ROOT_WINDOW && !COMMAND_DOCK_MODE && !COMMAND_EXEC_PROGRAM && !COMMAND_FULLSCREEN && !COMMAND_LOGIN_SHELL && !COMMAND_SET_TITLE && !COMMAND_FONT && !COMMAND_COLOR_FG && !COMMAND_COLOR_BG && !COMMAND_SATURATION && !COMMAND_GEOMETRY && !COMMAND_SHOW_OPTIONS && !COMMAND_SHOW_VERSION && !COMMAND_TAB_NUMBERS
             "");
#endif
#if COMMAND_DOCK_MODE
      printf("\t-d                    \tstart %s as a dock\n", PROGRAM_NAME);
#endif
#if COMMAND_EXEC_PROGRAM
      printf("\t-e [program] [options]\tspecify the program to be run in %s\n", PROGRAM_NAME);
#endif
#if COMMAND_FULLSCREEN
      printf("\t-f                    \tstart %s in fullscreen mode\n", PROGRAM_NAME);
#endif
#if COMMAND_FONT
      printf("\t-fn \"[font] [size]\"   \tspecify font and font size\n");
#endif
#if COMMAND_COLOR_FG
      printf("\t-fg [color]           \tspecify foreground color\n");
#endif
#if COMMAND_COLOR_BG
      printf("\t-bg [color]           \tspecify background color\n");
#endif
#if COMMAND_SATURATION
      printf("\t-sa [saturation]      \tspecify saturation level in [0, 1]\n");
#endif
#if COMMAND_GEOMETRY
      printf("\t-g +X+Y               \tspecify geometry\n");
#endif
      printf("\t-h                    \tshow this help\n");
#if COMMAND_LOGIN_SHELL
      printf("\t-ls                   \tuse login shell\n");
#endif
#if COMMAND_SHOW_OPTIONS
      printf("\t-o                    \tshow build-time configuration\n");
#endif
#if COMMAND_AT_ROOT_WINDOW
      printf("\t-r                    \tmake %s run in root window\n", PROGRAM_NAME);
#endif
#if COMMAND_SET_TITLE
      printf("\t-T [string]           \tspecify program title\n\t-title [string]       \tspecify program title\n");
#endif
#if COMMAND_SHOW_VERSION
      printf("\t-v                    \tshow version\n");
#endif
#if COMMAND_TAB_NUMBERS
      printf("\t-2 to -9              \tspecify the initial tab numbers\n");
#endif
      printf("\nGTK+ Options:\n");
#ifdef RULE_THEM_ALL
      printf("\t--2                   \tspecify GTK+ 2.x as GUI\n\t--3                   \tspecify GTK+ 3.x as GUI\n");
#endif
      printf("\t--class [string]      \tspecify WM_CLASS class\n\t--name [string]       \tspecify WM_CLASS name\n");
#ifdef BACKGROUND_IMAGE
      printf("\nBackground image:\n\t$HOME/%s\n", BACKGROUND_IMAGE);
#endif
      printf("\n");
      return FALSE;
    }
#endif

#ifdef RULE_THEM_ALL
    if (!strncmp(argv[j], "--2", 4))
      with_gtk = 2;
    if (!strncmp(argv[j], "--3", 4))
      with_gtk = 3;
#endif

#if COMMAND_LOGIN_SHELL
    if (!strncmp(argv[j], "-ls", 4))
      login_shell_flag |= 1;
#endif

#if COMMAND_AT_ROOT_WINDOW
    if (!strncmp(argv[j], "-r", 3))
      at_root_window = 1;
#endif

#if COMMAND_DOCK_MODE
    if (!strncmp(argv[j], "-d", 3))
      at_dock_mode = 1;
#endif

#if COMMAND_TAB_NUMBERS
    if (!strncmp(argv[j], "-2", 3))
      i = 2;
    if (!strncmp(argv[j], "-3", 3))
      i = 3;
    if (!strncmp(argv[j], "-4", 3))
      i = 4;
    if (!strncmp(argv[j], "-5", 3))
      i = 5;
    if (!strncmp(argv[j], "-6", 3))
      i = 6;
    if (!strncmp(argv[j], "-7", 3))
      i = 7;
    if (!strncmp(argv[j], "-8", 3))
      i = 8;
    if (!strncmp(argv[j], "-9", 3))
      i = 9;
#endif

    j++;
  }
#endif /* COMMAND_AT_ROOT_WINDOW || COMMAND_DOCK_MODE || COMMAND_LOGIN_SHELL || PROGRAM_WM_CLASS || COMMAND_SET_TITLE || COMMAND_FONT || COMMAND_COLOR_FG || COMMAND_COLOR_BG || COMMAND_SATURATION || COMMAND_GEOMETRY || COMMAND_SHOW_HELP || COMMAND_SHOW_OPTIONS || COMMAND_SHOW_VERSION || COMMAND_TAB_NUMBERS || defined(RULE_THEM_ALL) */

#ifdef BACKGROUND_IMAGE
  g_snprintf(imgstr, sizeof(imgstr), "%s/%s", g_getenv("HOME"), BACKGROUND_IMAGE);
#endif

#ifdef PROGRAM_ICON
  g_snprintf(iconstr, sizeof(iconstr), "%s/%s", g_getenv("HOME"), PROGRAM_ICON);
#endif

#ifdef FONT
#if COMMAND_FONT
  if (command_font)
    g_snprintf(font_name, sizeof(font_name), "%s", command_font);
  else
#endif
    g_snprintf(font_name, sizeof(font_name), "%s", FONT);
  calculate_font();
  g_snprintf(font_str, sizeof(font_str), "%s %d", font_name, font_size);
#if defined(HOTKEY_FONT_DEFAULT_SIZE) || defined(MENU_FONT_DEFAULT_SIZE)
  font_size_default = font_size;
#endif
#endif

#ifdef RULE_THEM_ALL
  if (with_gtk == 2) {
    if (!GET_VTE_TWO || !GET_GTK_TWO)
      with_gtk = 4;
  }

  if (with_gtk == 3) {
    if (!GET_VTE_THREE || !GET_GTK_THREE)
      with_gtk = 4;
  }

  if (with_gtk == 0) {
    with_gtk = 4;
#if (GTK_MAJOR_VERSION == 2)
    if (GET_VTE_TWO && GET_GTK_TWO)
      with_gtk = 2;
    else {
      if (GET_VTE_THREE && GET_GTK_THREE)
        with_gtk = 3;
    }
#endif
#if (GTK_MAJOR_VERSION == 3)
    if (GET_VTE_THREE && GET_GTK_THREE)
      with_gtk = 3;
    else {
      if (GET_VTE_TWO && GET_GTK_TWO)
        with_gtk = 2;
    }
#endif
  }

  if (with_gtk == 4) {
    printf("Failed to load VTE/GTK+ runtime.\n");
    return -1;
  }

  *(void **)(&p_gdk_color_parse) = dlsym(p_hdl_gtk, "gdk_color_parse");
  *(void **)(&p_gdk_display_get_default) = dlsym(p_hdl_gtk, "gdk_display_get_default");
  *(void **)(&p_gdk_display_get_default_screen) = dlsym(p_hdl_gtk, "gdk_display_get_default_screen");
  *(void **)(&p_gdk_window_get_display) = dlsym(p_hdl_gtk, "gdk_window_get_display");
  *(void **)(&p_gtk_box_pack_start) = dlsym(p_hdl_gtk, "gtk_box_pack_start");
  *(void **)(&p_gtk_button_new) = dlsym(p_hdl_gtk, "gtk_button_new");
  *(void **)(&p_gtk_button_set_focus_on_click) = dlsym(p_hdl_gtk, "gtk_button_set_focus_on_click");
  *(void **)(&p_gtk_button_set_image) = dlsym(p_hdl_gtk, "gtk_button_set_image");
  *(void **)(&p_gtk_button_set_relief) = dlsym(p_hdl_gtk, "gtk_button_set_relief");
  *(void **)(&p_gtk_clipboard_get) = dlsym(p_hdl_gtk, "gtk_clipboard_get");
  *(void **)(&p_gtk_clipboard_get_for_display) = dlsym(p_hdl_gtk, "gtk_clipboard_get_for_display");
  *(void **)(&p_gtk_clipboard_set_text) = dlsym(p_hdl_gtk, "gtk_clipboard_set_text");
  *(void **)(&p_gtk_clipboard_wait_is_text_available) = dlsym(p_hdl_gtk, "gtk_clipboard_wait_is_text_available");
  *(void **)(&p_gtk_color_selection_dialog_get_color_selection) = dlsym(p_hdl_gtk, "gtk_color_selection_dialog_get_color_selection");
  *(void **)(&p_gtk_color_selection_dialog_new) = dlsym(p_hdl_gtk, "gtk_color_selection_dialog_new");
  *(void **)(&p_gtk_color_selection_get_current_color) = dlsym(p_hdl_gtk, "gtk_color_selection_get_current_color");
  *(void **)(&p_gtk_color_selection_set_current_color) = dlsym(p_hdl_gtk, "gtk_color_selection_set_current_color");
  *(void **)(&p_gtk_container_add) = dlsym(p_hdl_gtk, "gtk_container_add");
  *(void **)(&p_gtk_container_child_set) = dlsym(p_hdl_gtk, "gtk_container_child_set");
  *(void **)(&p_gtk_dialog_get_content_area) = dlsym(p_hdl_gtk, "gtk_dialog_get_content_area");
  *(void **)(&p_gtk_dialog_new_with_buttons) = dlsym(p_hdl_gtk, "gtk_dialog_new_with_buttons");
  *(void **)(&p_gtk_dialog_run) = dlsym(p_hdl_gtk, "gtk_dialog_run");
  *(void **)(&p_gtk_dialog_set_default_response) = dlsym(p_hdl_gtk, "gtk_dialog_set_default_response");
  *(void **)(&p_gtk_entry_get_text) = dlsym(p_hdl_gtk, "gtk_entry_get_text");
  *(void **)(&p_gtk_entry_new) = dlsym(p_hdl_gtk, "gtk_entry_new");
  *(void **)(&p_gtk_entry_set_activates_default) = dlsym(p_hdl_gtk, "gtk_entry_set_activates_default");
  *(void **)(&p_gtk_entry_set_text) = dlsym(p_hdl_gtk, "gtk_entry_set_text");
  *(void **)(&p_gtk_font_selection_dialog_get_font_name) = dlsym(p_hdl_gtk, "gtk_font_selection_dialog_get_font_name");
  *(void **)(&p_gtk_font_selection_dialog_new) = dlsym(p_hdl_gtk, "gtk_font_selection_dialog_new");
  *(void **)(&p_gtk_font_selection_dialog_set_font_name) = dlsym(p_hdl_gtk, "gtk_font_selection_dialog_set_font_name");
  *(void **)(&p_gtk_hbox_new) = dlsym(p_hdl_gtk, "gtk_hbox_new");
  *(void **)(&p_gtk_image_menu_item_new_from_stock) = dlsym(p_hdl_gtk, "gtk_image_menu_item_new_from_stock");
  *(void **)(&p_gtk_image_menu_item_new_with_label) = dlsym(p_hdl_gtk, "gtk_image_menu_item_new_with_label");
  *(void **)(&p_gtk_image_menu_item_new_with_mnemonic) = dlsym(p_hdl_gtk, "gtk_image_menu_item_new_with_mnemonic");
  *(void **)(&p_gtk_image_menu_item_set_image) = dlsym(p_hdl_gtk, "gtk_image_menu_item_set_image");
  *(void **)(&p_gtk_image_new_from_stock) = dlsym(p_hdl_gtk, "gtk_image_new_from_stock");
  *(void **)(&p_gtk_init) = dlsym(p_hdl_gtk, "gtk_init");
  *(void **)(&p_gtk_label_get_text) = dlsym(p_hdl_gtk, "gtk_label_get_text");
  *(void **)(&p_gtk_label_new) = dlsym(p_hdl_gtk, "gtk_label_new");
  *(void **)(&p_gtk_main) = dlsym(p_hdl_gtk, "gtk_main");
  *(void **)(&p_gtk_main_quit) = dlsym(p_hdl_gtk, "gtk_main_quit");
  *(void **)(&p_gtk_menu_item_new_with_label) = dlsym(p_hdl_gtk, "gtk_menu_item_new_with_label");
  *(void **)(&p_gtk_menu_item_new_with_mnemonic) = dlsym(p_hdl_gtk, "gtk_menu_item_new_with_mnemonic");
  *(void **)(&p_gtk_menu_item_set_submenu) = dlsym(p_hdl_gtk, "gtk_menu_item_set_submenu");
  *(void **)(&p_gtk_menu_new) = dlsym(p_hdl_gtk, "gtk_menu_new");
  *(void **)(&p_gtk_menu_popup) = dlsym(p_hdl_gtk, "gtk_menu_popup");
  *(void **)(&p_gtk_menu_shell_append) = dlsym(p_hdl_gtk, "gtk_menu_shell_append");
  *(void **)(&p_gtk_menu_shell_prepend) = dlsym(p_hdl_gtk, "gtk_menu_shell_prepend");
  *(void **)(&p_gtk_notebook_append_page) = dlsym(p_hdl_gtk, "gtk_notebook_append_page");
  *(void **)(&p_gtk_notebook_get_current_page) = dlsym(p_hdl_gtk, "gtk_notebook_get_current_page");
  *(void **)(&p_gtk_notebook_get_n_pages) = dlsym(p_hdl_gtk, "gtk_notebook_get_n_pages");
  *(void **)(&p_gtk_notebook_get_nth_page) = dlsym(p_hdl_gtk, "gtk_notebook_get_nth_page");
  *(void **)(&p_gtk_notebook_get_show_tabs) = dlsym(p_hdl_gtk, "gtk_notebook_get_show_tabs");
  *(void **)(&p_gtk_notebook_get_tab_label) = dlsym(p_hdl_gtk, "gtk_notebook_get_tab_label");
  *(void **)(&p_gtk_notebook_new) = dlsym(p_hdl_gtk, "gtk_notebook_new");
  *(void **)(&p_gtk_notebook_popup_enable) = dlsym(p_hdl_gtk, "gtk_notebook_popup_enable");
  *(void **)(&p_gtk_notebook_prepend_page) = dlsym(p_hdl_gtk, "gtk_notebook_prepend_page");
  *(void **)(&p_gtk_notebook_remove_page) = dlsym(p_hdl_gtk, "gtk_notebook_remove_page");
  *(void **)(&p_gtk_notebook_set_current_page) = dlsym(p_hdl_gtk, "gtk_notebook_set_current_page");
  *(void **)(&p_gtk_notebook_set_scrollable) = dlsym(p_hdl_gtk, "gtk_notebook_set_scrollable");
  *(void **)(&p_gtk_notebook_set_show_border) = dlsym(p_hdl_gtk, "gtk_notebook_set_show_border");
  *(void **)(&p_gtk_notebook_set_show_tabs) = dlsym(p_hdl_gtk, "gtk_notebook_set_show_tabs");
  *(void **)(&p_gtk_notebook_set_tab_label) = dlsym(p_hdl_gtk, "gtk_notebook_set_tab_label");
  *(void **)(&p_gtk_notebook_set_tab_label_text) = dlsym(p_hdl_gtk, "gtk_notebook_set_tab_label_text");
  *(void **)(&p_gtk_notebook_set_tab_pos) = dlsym(p_hdl_gtk, "gtk_notebook_set_tab_pos");
  *(void **)(&p_gtk_notebook_set_tab_reorderable) = dlsym(p_hdl_gtk, "gtk_notebook_set_tab_reorderable");
  *(void **)(&p_gtk_range_get_value) = dlsym(p_hdl_gtk, "gtk_range_get_value");
  *(void **)(&p_gtk_range_set_value) = dlsym(p_hdl_gtk, "gtk_range_set_value");
  *(void **)(&p_gtk_separator_menu_item_new) = dlsym(p_hdl_gtk, "gtk_separator_menu_item_new");
  *(void **)(&p_gtk_settings_get_default) = dlsym(p_hdl_gtk, "gtk_settings_get_default");
  *(void **)(&p_gtk_statusbar_new) = dlsym(p_hdl_gtk, "gtk_statusbar_new");
  *(void **)(&p_gtk_statusbar_push) = dlsym(p_hdl_gtk, "gtk_statusbar_push");
  *(void **)(&p_gtk_test_widget_click) = dlsym(p_hdl_gtk, "gtk_test_widget_click");
  *(void **)(&p_gtk_test_widget_send_key) = dlsym(p_hdl_gtk, "gtk_test_widget_send_key");
  *(void **)(&p_gtk_vbox_new) = dlsym(p_hdl_gtk, "gtk_vbox_new");
  *(void **)(&p_gtk_widget_destroy) = dlsym(p_hdl_gtk, "gtk_widget_destroy");
  *(void **)(&p_gtk_widget_get_screen) = dlsym(p_hdl_gtk, "gtk_widget_get_screen");
  *(void **)(&p_gtk_widget_get_window) = dlsym(p_hdl_gtk, "gtk_widget_get_window");
  *(void **)(&p_gtk_widget_hide) = dlsym(p_hdl_gtk, "gtk_widget_hide");
  *(void **)(&p_gtk_widget_realize) = dlsym(p_hdl_gtk, "gtk_widget_realize");
  *(void **)(&p_gtk_widget_set_can_focus) = dlsym(p_hdl_gtk, "gtk_widget_set_can_focus");
  *(void **)(&p_gtk_widget_set_sensitive) = dlsym(p_hdl_gtk, "gtk_widget_set_sensitive");
  *(void **)(&p_gtk_widget_show) = dlsym(p_hdl_gtk, "gtk_widget_show");
  *(void **)(&p_gtk_widget_show_all) = dlsym(p_hdl_gtk, "gtk_widget_show_all");
  *(void **)(&p_gtk_window_maximize) = dlsym(p_hdl_gtk, "gtk_window_maximize");
  *(void **)(&p_gtk_window_new) = dlsym(p_hdl_gtk, "gtk_window_new");
  *(void **)(&p_gtk_window_parse_geometry) = dlsym(p_hdl_gtk, "gtk_window_parse_geometry");
  *(void **)(&p_gtk_window_resize) = dlsym(p_hdl_gtk, "gtk_window_resize");
  *(void **)(&p_gtk_window_resize_grip_is_visible) = dlsym(p_hdl_gtk, "gtk_window_resize_grip_is_visible");
  if (dlerror() != NULL)
    has_resize_grip = 0;
  *(void **)(&p_gtk_window_set_has_resize_grip) = dlsym(p_hdl_gtk, "gtk_window_set_has_resize_grip");
  if (dlerror() != NULL)
    has_resize_grip = 0;
  *(void **)(&p_gtk_window_set_decorated) = dlsym(p_hdl_gtk, "gtk_window_set_decorated");
  *(void **)(&p_gtk_window_set_focus) = dlsym(p_hdl_gtk, "gtk_window_set_focus");
  *(void **)(&p_gtk_window_set_icon_from_file) = dlsym(p_hdl_gtk, "gtk_window_set_icon_from_file");
  *(void **)(&p_gtk_window_set_keep_above) = dlsym(p_hdl_gtk, "gtk_window_set_keep_above");
  *(void **)(&p_gtk_window_set_keep_below) = dlsym(p_hdl_gtk, "gtk_window_set_keep_below");
  *(void **)(&p_gtk_window_set_skip_pager_hint) = dlsym(p_hdl_gtk, "gtk_window_set_skip_pager_hint");
  *(void **)(&p_gtk_window_set_skip_taskbar_hint) = dlsym(p_hdl_gtk, "gtk_window_set_skip_taskbar_hint");
  *(void **)(&p_gtk_window_set_title) = dlsym(p_hdl_gtk, "gtk_window_set_title");
  *(void **)(&p_gtk_window_set_type_hint) = dlsym(p_hdl_gtk, "gtk_window_set_type_hint");
  *(void **)(&p_gtk_window_set_urgency_hint) = dlsym(p_hdl_gtk, "gtk_window_set_urgency_hint");
  *(void **)(&p_gtk_window_set_wmclass) = dlsym(p_hdl_gtk, "gtk_window_set_wmclass");
  *(void **)(&p_gtk_window_unmaximize) = dlsym(p_hdl_gtk, "gtk_window_unmaximize");
  *(void **)(&p_vte_terminal_copy_clipboard) = dlsym(p_hdl_vte, "vte_terminal_copy_clipboard");
  *(void **)(&p_vte_terminal_fork_command) = dlsym(p_hdl_vte, "vte_terminal_fork_command");
  *(void **)(&p_vte_terminal_fork_command_full) = dlsym(p_hdl_vte, "vte_terminal_fork_command_full");
  *(void **)(&p_vte_terminal_get_char_height) = dlsym(p_hdl_vte, "vte_terminal_get_char_height");
  *(void **)(&p_vte_terminal_get_char_width) = dlsym(p_hdl_vte, "vte_terminal_get_char_width");
  *(void **)(&p_vte_terminal_get_encoding) = dlsym(p_hdl_vte, "vte_terminal_get_encoding");
  *(void **)(&p_vte_terminal_get_has_selection) = dlsym(p_hdl_vte, "vte_terminal_get_has_selection");
  *(void **)(&p_vte_terminal_get_window_title) = dlsym(p_hdl_vte, "vte_terminal_get_window_title");
  *(void **)(&p_vte_terminal_im_append_menuitems) = dlsym(p_hdl_vte, "vte_terminal_im_append_menuitems");
  *(void **)(&p_vte_terminal_match_add_gregex) = dlsym(p_hdl_vte, "vte_terminal_match_add_gregex");
  *(void **)(&p_vte_terminal_match_check) = dlsym(p_hdl_vte, "vte_terminal_match_check");
  *(void **)(&p_vte_terminal_new) = dlsym(p_hdl_vte, "vte_terminal_new");
  *(void **)(&p_vte_terminal_paste_clipboard) = dlsym(p_hdl_vte, "vte_terminal_paste_clipboard");
  *(void **)(&p_vte_terminal_reset) = dlsym(p_hdl_vte, "vte_terminal_reset");
  *(void **)(&p_vte_terminal_search_find_next) = dlsym(p_hdl_vte, "vte_terminal_search_find_next");
  *(void **)(&p_vte_terminal_search_find_previous) = dlsym(p_hdl_vte, "vte_terminal_search_find_previous");
  *(void **)(&p_vte_terminal_search_set_gregex) = dlsym(p_hdl_vte, "vte_terminal_search_set_gregex");
  *(void **)(&p_vte_terminal_search_set_wrap_around) = dlsym(p_hdl_vte, "vte_terminal_search_set_wrap_around");
  *(void **)(&p_vte_terminal_select_all) = dlsym(p_hdl_vte, "vte_terminal_select_all");
  *(void **)(&p_vte_terminal_set_allow_bold) = dlsym(p_hdl_vte, "vte_terminal_set_allow_bold");
  *(void **)(&p_vte_terminal_set_audible_bell) = dlsym(p_hdl_vte, "vte_terminal_set_audible_bell");
  *(void **)(&p_vte_terminal_set_background_image_file) = dlsym(p_hdl_vte, "vte_terminal_set_background_image_file");
  *(void **)(&p_vte_terminal_set_background_saturation) = dlsym(p_hdl_vte, "vte_terminal_set_background_saturation");
  *(void **)(&p_vte_terminal_set_background_tint_color) = dlsym(p_hdl_vte, "vte_terminal_set_background_tint_color");
  *(void **)(&p_vte_terminal_set_background_transparent) = dlsym(p_hdl_vte, "vte_terminal_set_background_transparent");
  *(void **)(&p_vte_terminal_set_backspace_binding) = dlsym(p_hdl_vte, "vte_terminal_set_backspace_binding");
  *(void **)(&p_vte_terminal_set_color_background) = dlsym(p_hdl_vte, "vte_terminal_set_color_background");
  *(void **)(&p_vte_terminal_set_color_bold) = dlsym(p_hdl_vte, "vte_terminal_set_color_bold");
  *(void **)(&p_vte_terminal_set_color_cursor) = dlsym(p_hdl_vte, "vte_terminal_set_color_cursor");
  *(void **)(&p_vte_terminal_set_color_dim) = dlsym(p_hdl_vte, "vte_terminal_set_color_dim");
  *(void **)(&p_vte_terminal_set_color_foreground) = dlsym(p_hdl_vte, "vte_terminal_set_color_foreground");
  *(void **)(&p_vte_terminal_set_color_highlight) = dlsym(p_hdl_vte, "vte_terminal_set_color_highlight");
  *(void **)(&p_vte_terminal_set_colors) = dlsym(p_hdl_vte, "vte_terminal_set_colors");
  *(void **)(&p_vte_terminal_set_cursor_blink_mode) = dlsym(p_hdl_vte, "vte_terminal_set_cursor_blink_mode");
  *(void **)(&p_vte_terminal_set_cursor_shape) = dlsym(p_hdl_vte, "vte_terminal_set_cursor_shape");
  *(void **)(&p_vte_terminal_set_default_colors) = dlsym(p_hdl_vte, "vte_terminal_set_default_colors");
  *(void **)(&p_vte_terminal_set_delete_binding) = dlsym(p_hdl_vte, "vte_terminal_set_delete_binding");
  *(void **)(&p_vte_terminal_set_emulation) = dlsym(p_hdl_vte, "vte_terminal_set_emulation");
  *(void **)(&p_vte_terminal_set_encoding) = dlsym(p_hdl_vte, "vte_terminal_set_encoding");
  *(void **)(&p_vte_terminal_set_font_from_string) = dlsym(p_hdl_vte, "vte_terminal_set_font_from_string");
  *(void **)(&p_vte_terminal_set_font_from_string_full) = dlsym(p_hdl_vte, "vte_terminal_set_font_from_string_full");
  *(void **)(&p_vte_terminal_set_mouse_autohide) = dlsym(p_hdl_vte, "vte_terminal_set_mouse_autohide");
  *(void **)(&p_vte_terminal_set_opacity) = dlsym(p_hdl_vte, "vte_terminal_set_opacity");
  *(void **)(&p_vte_terminal_set_scroll_background) = dlsym(p_hdl_vte, "vte_terminal_set_scroll_background");
  *(void **)(&p_vte_terminal_set_scroll_on_keystroke) = dlsym(p_hdl_vte, "vte_terminal_set_scroll_on_keystroke");
  *(void **)(&p_vte_terminal_set_scroll_on_output) = dlsym(p_hdl_vte, "vte_terminal_set_scroll_on_output");
  *(void **)(&p_vte_terminal_set_scrollback_lines) = dlsym(p_hdl_vte, "vte_terminal_set_scrollback_lines");
  *(void **)(&p_vte_terminal_set_visible_bell) = dlsym(p_hdl_vte, "vte_terminal_set_visible_bell");
  *(void **)(&p_vte_terminal_set_word_chars) = dlsym(p_hdl_vte, "vte_terminal_set_word_chars");

  if (with_gtk == 2) {
    *(void **)(&p_gdk_display_warp_pointer) = dlsym(p_hdl_gtk, "gdk_display_warp_pointer");
    *(void **)(&p_gdk_screen_get_rgba_colormap) = dlsym(p_hdl_gtk, "gdk_screen_get_rgba_colormap");
    *(void **)(&p_gdk_x11_drawable_get_xid) = dlsym(p_hdl_gtk, "gdk_x11_drawable_get_xid");
    *(void **)(&p_gtk_hscale_new_with_range) = dlsym(p_hdl_gtk, "gtk_hscale_new_with_range");
    *(void **)(&p_gtk_notebook_set_tab_border) = dlsym(p_hdl_gtk, "gtk_notebook_set_tab_border");
    *(void **)(&p_gtk_notebook_set_tab_hborder) = dlsym(p_hdl_gtk, "gtk_notebook_set_tab_hborder");
    *(void **)(&p_gtk_notebook_set_tab_vborder) = dlsym(p_hdl_gtk, "gtk_notebook_set_tab_vborder");
    *(void **)(&p_gtk_rc_parse_string) = dlsym(p_hdl_gtk, "gtk_rc_parse_string");
    *(void **)(&p_gtk_vscrollbar_new) = dlsym(p_hdl_gtk, "gtk_vscrollbar_new");
    *(void **)(&p_gtk_widget_set_colormap) = dlsym(p_hdl_gtk, "gtk_widget_set_colormap");
    *(void **)(&p_vte_terminal_get_adjustment) = dlsym(p_hdl_vte, "vte_terminal_get_adjustment");
    *(void **)(&p_vte_terminal_set_size) = dlsym(p_hdl_vte, "vte_terminal_set_size");
  }

  if (with_gtk == 3) {
    *(void **)(&p_gdk_device_warp) = dlsym(p_hdl_gtk, "gdk_device_warp");
    *(void **)(&p_gdk_disable_multidevice) = dlsym(p_hdl_gtk, "gdk_disable_multidevice");
    *(void **)(&p_gdk_screen_get_rgba_visual) = dlsym(p_hdl_gtk, "gdk_screen_get_rgba_visual");
    *(void **)(&p_gdk_x11_window_get_xid) = dlsym(p_hdl_gtk, "gdk_x11_window_get_xid");
    *(void **)(&p_gtk_css_provider_load_from_data) = dlsym(p_hdl_gtk, "gtk_css_provider_load_from_data");
    *(void **)(&p_gtk_css_provider_new) = dlsym(p_hdl_gtk, "gtk_css_provider_new");
    *(void **)(&p_gtk_scale_new_with_range) = dlsym(p_hdl_gtk, "gtk_scale_new_with_range");
    *(void **)(&p_gtk_scrollable_get_vadjustment) = dlsym(p_hdl_gtk, "gtk_scrollable_get_vadjustment");
    *(void **)(&p_gtk_scrollbar_new) = dlsym(p_hdl_gtk, "gtk_scrollbar_new");
    *(void **)(&p_gtk_style_context_add_provider_for_screen) = dlsym(p_hdl_gtk, "gtk_style_context_add_provider_for_screen");
    *(void **)(&p_gtk_widget_set_hexpand) = dlsym(p_hdl_gtk, "gtk_widget_set_hexpand");
    *(void **)(&p_gtk_widget_set_size_request) = dlsym(p_hdl_gtk, "gtk_widget_set_size_request");
    *(void **)(&p_gtk_widget_set_vexpand) = dlsym(p_hdl_gtk, "gtk_widget_set_vexpand");
    *(void **)(&p_gtk_widget_set_visual) = dlsym(p_hdl_gtk, "gtk_widget_set_visual");
    *(void **)(&p_gtk_widget_style_get) = dlsym(p_hdl_gtk, "gtk_widget_style_get");
    *(void **)(&p_vte_terminal_accessible_new) = dlsym(p_hdl_vte, "vte_terminal_accessible_new");
  }
#endif

#if defined(RULE_THEM_ALL) || GTK_CHECK_VERSION(2,91,7)
#if defined(HOTKEY_MIMIC_SCROLL_UP) || defined(HOTKEY_MIMIC_SCROLL_DOWN) || defined(ONLY_ONE_MENU_ITEM)
#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
#endif
    gdk_disable_multidevice();
#endif
#endif

  gtk_init(&argc, &argv);

#if TAB_CLOSE_BUTTON
#ifdef RULE_THEM_ALL
  if (with_gtk == 2)
#endif
#if defined(RULE_THEM_ALL) || !GTK_CHECK_VERSION(2,91,6)
    gtk_rc_parse_string("style \"ivte\" { GtkButton::default-border = { 0, 0, 0, 0 } GtkButton::default-outside-border = { 0, 0, 0, 0 } GtkButton::inner-border = { 0, 0, 0, 0 } GtkWidget::focus-line-width = 0 xthickness = 0 ythickness = 0 } widget_class \"*.GtkNotebook.GtkHBox.GtkButton\" style \"ivte\"");
#endif
#endif

#if defined(GTK3_CSS) && (defined(RULE_THEM_ALL) || GTK_CHECK_VERSION(2,91,6))
#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
#endif
  {
    GtkCssProvider *provider = gtk_css_provider_new();
    gtk_css_provider_load_from_data(provider, GTK3_CSS, -1, NULL);
    gtk_style_context_add_provider_for_screen(gdk_display_get_default_screen(gdk_display_get_default()), GTK_STYLE_PROVIDER(provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    g_object_unref(provider);
  }
#endif

#ifdef TAB_LABEL
#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
    textdomain("gtk30");
  else
    textdomain("gtk20");
#endif
#ifndef RULE_THEM_ALL
#if GTK_CHECK_VERSION(2,90,0)
  textdomain("gtk30");
#endif
#if !GTK_CHECK_VERSION(2,90,0)
  textdomain("gtk20");
#endif
#endif
#endif

#if defined(HOTKEY_SEARCH_STRING) || defined(HOTKEY_SEARCH_PREVIOUS) || defined(HOTKEY_SEARCH_NEXT)
  global_search_string = gtk_entry_new();
#endif

#if BUTTON_ORDER_BY_RCFILE
  g_object_get(gtk_settings_get_default(), "gtk-alternative-button-order", &button_order, NULL);
#endif

#if defined(BACKGROUND_TINT_COLOR) && defined(BACKGROUND_EXIST)
  gdk_color_parse(BACKGROUND_TINT_COLOR, &color_tint);
#endif

  main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

#ifdef RULE_THEM_ALL
  if (has_resize_grip)
#endif
#if defined(RULE_THEM_ALL) || defined(GTK_HAS_RESIZE_GRIP)
  {
#if defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
    status_bar_resize_grip = gtk_window_resize_grip_is_visible(GTK_WINDOW(main_window));
    if (!status_bar_status)
      gtk_window_set_has_resize_grip(GTK_WINDOW(main_window), FALSE);
#endif
#if !STATUS_BAR && !defined(HOTKEY_TOGGLE_STATUS_BAR) && !defined(MENU_TOGGLE_STATUS_BAR)
    gtk_window_set_has_resize_grip(GTK_WINDOW(main_window), FALSE);
#endif
  }
#endif

#if PROGRAM_WM_CLASS
  gtk_window_set_wmclass(GTK_WINDOW(main_window), wm_class_name, wm_class_class);
#endif

#ifdef SHOW_WINDOW_DECORATED
  gtk_window_set_decorated(GTK_WINDOW(main_window), SHOW_WINDOW_DECORATED);
#endif

#if COMMAND_SET_TITLE
  gtk_window_set_title(GTK_WINDOW(main_window), VTE_PROGRAM_NAME);
#endif

#ifdef PROGRAM_ICON
  if (!gtk_window_set_icon_from_file(GTK_WINDOW(main_window), iconstr, NULL))
#endif
#if defined(PROGRAM_ICON) || SHOW_WINDOW_ICON
    gtk_window_set_icon_from_file(GTK_WINDOW(main_window), ICON_DIR"/ivte.png", NULL);
#endif

#if STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
  vbox = gtk_vbox_new(FALSE, 0);
#if GTK_CHECK_VERSION(2,91,2) && defined(USE_GTK_GRID)
  gtk_orientable_set_orientation(GTK_ORIENTABLE(vbox), GTK_ORIENTATION_VERTICAL);
#endif
  gtk_container_add(GTK_CONTAINER(main_window), vbox);
#endif

  notebook = gtk_notebook_new();

#ifdef SHOW_WINDOW_BORDER
  gtk_notebook_set_show_border(GTK_NOTEBOOK(notebook), SHOW_WINDOW_BORDER);
#endif

#ifdef RULE_THEM_ALL
  if (with_gtk == 2)
#endif
#if defined(RULE_THEM_ALL) || !GTK_CHECK_VERSION(2,90,0)
  {
#ifdef TAB_BORDER
    gtk_notebook_set_tab_border(GTK_NOTEBOOK(notebook), TAB_BORDER);
#endif
#ifdef TAB_BORDER_VERTICAL
    gtk_notebook_set_tab_hborder(GTK_NOTEBOOK(notebook), TAB_BORDER_VERTICAL);
#endif
#ifdef TAB_BORDER_HORIZONTAL
    gtk_notebook_set_tab_vborder(GTK_NOTEBOOK(notebook), TAB_BORDER_HORIZONTAL);
#endif
  }
#endif

#ifdef TABBAR
  gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), VTE_TABBAR);
#endif

#if TABBAR_SCROLLABLE
  gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook), TABBAR_SCROLLABLE);
#endif
#if TABBAR_MENU_SELECT_TAB
  gtk_notebook_popup_enable(GTK_NOTEBOOK(notebook));
#endif
#ifdef TABBAR_PLACE
  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), TABBAR_PLACE);
#endif

  gtk_widget_set_can_focus(notebook, FALSE);

#if defined(TOGGLE_BG_OPACITY) || BACKGROUND_OPACITY
#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
    gtk_widget_set_visual(main_window, gdk_screen_get_rgba_visual(gtk_widget_get_screen(main_window)));
  else
#endif
    gtk_widget_set_colormap(main_window, gdk_screen_get_rgba_colormap(gtk_widget_get_screen(main_window)));
#endif

#if COMMAND_EXEC_PROGRAM && !VTE_FORK_CMD_OLD
  if (change_command)
    default_argv[0] = default_command;
#endif

#if EXPORT_WINDOWID
  gtk_widget_realize(main_window);
  char windowid[16];
#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
    g_snprintf(windowid, 16, "%d", (int)gdk_x11_window_get_xid(gtk_widget_get_window(main_window)));
  else
#endif
    g_snprintf(windowid, 16, "%d", (int)gdk_x11_drawable_get_xid(IVTE_GET_WINDOW(main_window)));
  setenv("WINDOWID", windowid, TRUE);
#endif

  add_tab();

#if COMMAND_EXEC_PROGRAM
  g_snprintf(default_command, sizeof(default_command), "%s", DEFAULT_COMMAND);
  default_argv = NULL;
#if COMMAND_LOGIN_SHELL || !VTE_FORK_CMD_OLD
  login_shell_flag = (login_shell_flag & 1) ? 1 : 0;
#endif
#endif

#if COMMAND_TAB_NUMBERS
  while (i > 1) {
    add_tab();
    i--;
  }
  GET_CURRENT_TAB(0);
  gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), 0);
  gtk_window_set_focus(GTK_WINDOW(main_window), term->vte);
#endif

#if STATUS_BAR || defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
  gtk_box_pack_start(GTK_BOX(vbox), notebook, TRUE, TRUE, 0);
  statusbar = gtk_statusbar_new();
  GET_CURRENT_TAB(gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook)));
  gtk_statusbar_push(GTK_STATUSBAR(statusbar), 0, vte_terminal_get_encoding(VTE_TERMINAL(term->vte)));
  gtk_box_pack_start(GTK_BOX(vbox), statusbar, FALSE, TRUE, 0);
#endif
#if !STATUS_BAR && !defined(HOTKEY_TOGGLE_STATUS_BAR) && !defined(MENU_TOGGLE_STATUS_BAR)
  gtk_container_add(GTK_CONTAINER(main_window), notebook);
#endif

  g_signal_connect(main_window, "delete_event", G_CALLBACK(delete_event), NULL);

#ifdef HOTKEY_HAS_DEFINE
  g_signal_connect(main_window, "key-press-event", G_CALLBACK(key_press_event), NULL);
#endif

#if BELL_URGENT
  g_signal_connect(main_window, "focus-in-event", G_CALLBACK(focus_in_event), NULL);
#endif

#if defined(HOTKEY_TOGGLE_FULLSCREEN) || defined(MENU_TOGGLE_FULLSCREEN) || defined(VTE_FUNNY)
  g_signal_connect(main_window, "window_state_event", G_CALLBACK(window_state_event), NULL);
#endif

  g_signal_connect_after(notebook, "switch-page", switch_page, NULL);

#if TAB_REORDERABLE && GTK_CHECK_VERSION(2,9,0)
  g_signal_connect(notebook, "page-reordered", switch_page, NULL);
#endif

#if COMMAND_AT_ROOT_WINDOW
  if (at_root_window) {
    gtk_window_set_keep_below(GTK_WINDOW(main_window), TRUE);
#if defined(HOTKEY_TOGGLE_DECORATED) || defined(MENU_TOGGLE_DECORATED)
    window_decorated_status = 0;
#endif
    gtk_window_set_decorated(GTK_WINDOW(main_window), FALSE);
    gtk_window_set_skip_taskbar_hint(GTK_WINDOW(main_window), TRUE);
    gtk_window_set_skip_pager_hint(GTK_WINDOW(main_window), TRUE);
  }
#endif

#ifdef PROGRAM_ALWAYS_ON_TOP
#if COMMAND_AT_ROOT_WINDOW
  if (!at_root_window)
#endif
    gtk_window_set_keep_above(GTK_WINDOW(main_window), always_on_top);
#endif

#if COMMAND_DOCK_MODE
  if (at_dock_mode)
    gtk_window_set_type_hint(GTK_WINDOW(main_window), GDK_WINDOW_TYPE_HINT_DOCK);
#endif

#if COMMAND_GEOMETRY
  if (command_geometry)
    gtk_window_parse_geometry(GTK_WINDOW(main_window), command_geometry);
#endif

#ifdef RULE_THEM_ALL
  if (with_gtk == 3)
#endif
  {
#ifdef VTE_FUNNY
#if COMMAND_TAB_NUMBERS
    for (i = 0 ; i < gtk_notebook_get_n_pages(GTK_NOTEBOOK(notebook)) ; i++) {
      GET_CURRENT_TAB(i);
#endif
      gtk_widget_set_size_request(term->vte, VTE_COLUMNS * vte_terminal_get_char_width(VTE_TERMINAL(term->vte)) + INNER_BORDER_W, VTE_ROWS * vte_terminal_get_char_height(VTE_TERMINAL(term->vte)) + INNER_BORDER_H);
#if COMMAND_TAB_NUMBERS
    }
#endif
#endif
  }

#if (INNER_BORDER_H < 2) || (INNER_BORDER_W < 2)
  gtk_window_resize(GTK_WINDOW(main_window), 1, 1);
#endif

  gtk_widget_show_all(main_window);

#if defined(HOTKEY_TOGGLE_STATUS_BAR) || defined(MENU_TOGGLE_STATUS_BAR)
  if (!status_bar_status) {
    gtk_widget_hide(statusbar);
    VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
  }
#endif

#if defined(HOTKEY_TOGGLE_SCROLLBAR) || defined(MENU_TOGGLE_SCROLLBAR)
  if (!scrollbar_status) {
    hide_scrollbar();
    VTE_WINDOW_RESIZE(GTK_WINDOW(main_window), 1, 1);
  }
#endif

#if COMMAND_FULLSCREEN
  j = 1;
  while ((j < argc) && strncmp(argv[j], "-e", 3)) {
    if (!strncmp(argv[j], "-f", 3))
      gtk_window_maximize(GTK_WINDOW(main_window));
    j++;
  }
#endif

#if MENU
  menu = gtk_menu_new();
#endif

#ifdef MENU_CUSTOM
  for (j = 0 ; j < MENU_CUSTOM_SIZE ; j++)
#endif
  {
#ifdef MENU_COPY
    if (!strncmp(menu_custom[j], "Copy", 5)) {
      menu_copy = gtk_image_menu_item_new_from_stock(GTK_STOCK_COPY, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_copy);
      g_signal_connect(menu_copy, "activate", do_copy, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_PASTE
    if (!strncmp(menu_custom[j], "Paste", 6)) {
      menu_paste = gtk_image_menu_item_new_from_stock(GTK_STOCK_PASTE, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_paste);
      g_signal_connect(menu_paste, "activate", do_paste, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_SELECT_ALL
    if (!strncmp(menu_custom[j], "Select all", 11)) {
      GtkWidget *menu_select_all = gtk_image_menu_item_new_from_stock(GTK_STOCK_SELECT_ALL, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_select_all);
      g_signal_connect(menu_select_all, "activate", do_select_all, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TAB_ADD
    if (!strncmp(menu_custom[j], "Add tab", 8)) {
      GtkWidget *menu_add_tab = gtk_image_menu_item_new_from_stock(GTK_STOCK_ADD, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_add_tab);
      g_signal_connect(menu_add_tab, "activate", add_tab, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TAB_REMOVE
    if (!strncmp(menu_custom[j], "Remove tab", 11)) {
      GtkWidget *menu_close_tab = gtk_image_menu_item_new_from_stock(GTK_STOCK_REMOVE, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_close_tab);
      g_signal_connect(menu_close_tab, "activate", G_CALLBACK(del_tab), (bool*)CLOSE_DIALOG);
      menu_item_success++;
    }
#endif

#ifdef MENU_TAB_EDIT_LABEL
    if (!strncmp(menu_custom[j], "Edit label", 11)) {
      GtkWidget *menu_edit_label = gtk_image_menu_item_new_from_stock(GTK_STOCK_EDIT, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_edit_label);
      g_signal_connect(menu_edit_label, "activate", do_edit_label, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_OPEN_NEW_WINDOW
    if (!strncmp(menu_custom[j], "New window", 11)) {
      GtkWidget *menu_new_window = gtk_image_menu_item_new_from_stock(GTK_STOCK_NEW, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_new_window);
      g_signal_connect(menu_new_window, "activate", do_new_window, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_QUIT
    if (!strncmp(menu_custom[j], "Quit", 5)) {
      GtkWidget *menu_quit = gtk_image_menu_item_new_from_stock(GTK_STOCK_QUIT, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_quit);
      g_signal_connect(menu_quit, "activate", G_CALLBACK(delete_event), NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_FONT_BIGGER
    if (!strncmp(menu_custom[j], "Zoom in", 8)) {
      GtkWidget *menu_zoom_in = gtk_image_menu_item_new_from_stock(GTK_STOCK_ZOOM_IN, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_zoom_in);
      g_signal_connect(menu_zoom_in, "activate", do_zoom_in, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_FONT_SMALLER
    if (!strncmp(menu_custom[j], "Zoom out", 9)) {
      GtkWidget *menu_zoom_out = gtk_image_menu_item_new_from_stock(GTK_STOCK_ZOOM_OUT, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_zoom_out);
      g_signal_connect(menu_zoom_out, "activate", do_zoom_out, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_FONT_DEFAULT_SIZE
    if (!strncmp(menu_custom[j], "Zoom default", 13)) {
      menu_zoom_100 = gtk_image_menu_item_new_from_stock(GTK_STOCK_ZOOM_100, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_zoom_100);
      g_signal_connect(menu_zoom_100, "activate", do_zoom_100, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_RESET_TERMINAL
    if (!strncmp(menu_custom[j], "Reset terminal", 15)) {
      GtkWidget *menu_reset = gtk_image_menu_item_new_from_stock(GTK_STOCK_REFRESH, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_reset);
      g_signal_connect(menu_reset, "activate", do_reset, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_RESET_AND_CLEAR
    if (!strncmp(menu_custom[j], "Reset and clear", 16)) {
      GtkWidget *menu_clear = gtk_image_menu_item_new_from_stock(GTK_STOCK_CLEAR, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_clear);
      g_signal_connect(menu_clear, "activate", do_clear, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_FONT_SELECT
    if (!strncmp(menu_custom[j], "Select font", 12)) {
      GtkWidget *menu_font = gtk_image_menu_item_new_from_stock(GTK_STOCK_SELECT_FONT, NULL);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_font);
      g_signal_connect(menu_font, "activate", do_select_font, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_ON_TOP
    if (!strncmp(menu_custom[j], "Toggle always on top", 21)) {
      GtkWidget *menu_toggle_on_top = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_ON_TOP);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_on_top), gtk_image_new_from_stock(GTK_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_on_top);
      g_signal_connect(menu_toggle_on_top, "activate", do_always_on_top, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_BACKGROUND
    if (!strncmp(menu_custom[j], "Toggle background", 18)) {
      GtkWidget *menu_toggle_bg = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_BG);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_bg), gtk_image_new_from_stock(GTK_STOCK_DND_MULTIPLE, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_bg);
      g_signal_connect(menu_toggle_bg, "activate", do_toggle_bg, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_HOTKEYS
    if (!strncmp(menu_custom[j], "Toggle hotkeys locking", 23)) {
      GtkWidget *menu_toggle_hotkeys = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_HOTKEYS);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_hotkeys), gtk_image_new_from_stock(GTK_STOCK_DIALOG_AUTHENTICATION, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_hotkeys);
      g_signal_connect(menu_toggle_hotkeys, "activate", do_toggle_hotkeys, NULL);
      menu_item_success++;
    }
#endif

#if defined(MENU_TOGGLE_SCROLLBAR) && defined(SCROLLBAR)
    if (!strncmp(menu_custom[j], "Toggle scrollbar", 17)) {
      GtkWidget *menu_toggle_scrollbar = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_SCROLLBAR);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_scrollbar), gtk_image_new_from_stock(GTK_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_scrollbar);
      g_signal_connect(menu_toggle_scrollbar, "activate", do_toggle_scrollbar, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_STATUS_BAR
    if (!strncmp(menu_custom[j], "Toggle status bar", 18)) {
      GtkWidget *menu_toggle_status_bar = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_STATUS_BAR);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_status_bar), gtk_image_new_from_stock(GTK_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_status_bar);
      g_signal_connect(menu_toggle_status_bar, "activate", do_toggle_status_bar, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_TABBAR
    if (!strncmp(menu_custom[j], "Toggle tabbar", 14)) {
      GtkWidget *menu_toggle_tabbar = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_TABBAR);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_tabbar), gtk_image_new_from_stock(GTK_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_tabbar);
      g_signal_connect(menu_toggle_tabbar, "activate", do_toggle_tabbar, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_DECORATED
    if (!strncmp(menu_custom[j], "Toggle window decorated", 24)) {
      GtkWidget *menu_toggle_decorated = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_DECORATED);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_decorated), gtk_image_new_from_stock(GTK_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_decorated);
      g_signal_connect(menu_toggle_decorated, "activate", do_toggle_decorated, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_FULLSCREEN
    if (!strncmp(menu_custom[j], "Toggle fullscreen", 18)) {
      GtkWidget *menu_toggle_fullscreen = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_FULLSCREEN);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_fullscreen), gtk_image_new_from_stock(GTK_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_fullscreen);
      g_signal_connect(menu_toggle_fullscreen, "activate", do_toggle_fullscreen, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_TOGGLE_ANTI_ALIAS
    if (!strncmp(menu_custom[j], "Toggle anti-alias", 18)) {
      GtkWidget *menu_toggle_antialias = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_TOGGLE_ANTI_ALIAS);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_toggle_antialias), gtk_image_new_from_stock(GTK_STOCK_LEAVE_FULLSCREEN, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_toggle_antialias);
      g_signal_connect(menu_toggle_antialias, "activate", do_toggle_antialias, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_CHANGE_SATURATION
    if (!strncmp(menu_custom[j], "Adjust saturation", 18)) {
      GtkWidget *menu_change_saturation = gtk_image_menu_item_new_with_mnemonic(LABEL_MENU_SATURATION);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_change_saturation), gtk_image_new_from_stock(GTK_STOCK_DIALOG_INFO, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_change_saturation);
      g_signal_connect(menu_change_saturation, "activate", do_menu_saturation, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_COLOR_BACKGROUND
    if (!strncmp(menu_custom[j], "Background tint", 16)) {
      GtkWidget *menu_change_tint = gtk_image_menu_item_new_with_mnemonic(LABEL_DIALOG_BACKGROUND_TINT);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menu_change_tint), gtk_image_new_from_stock(GTK_STOCK_SELECT_COLOR, GTK_ICON_SIZE_MENU));
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), menu_change_tint);
      g_signal_connect(menu_change_tint, "activate", do_menu_tint_color, NULL);
      menu_item_success++;
    }
#endif

#ifdef MENU_SEPARATOR
    if (!strncmp(menu_custom[j], "Separator", 10))
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), gtk_separator_menu_item_new());
#endif

#ifdef MENU_CUSTOM
    if (!strncmp(menu_custom[j], "Encoding list", 14))
#endif
#ifdef MENU_ENCODING_LIST
    {
      GtkWidget *encoding_item[MENU_ENCODING_LIST_SIZE];
      for (i = 0 ; i < MENU_ENCODING_LIST_SIZE ; i++) {
#ifdef MENU_DEFAULT_ENCODING
        if (!strncmp(encoding[i], "Default Encoding", 17)) {
          encoding[i] = (char*)vte_terminal_get_encoding(VTE_TERMINAL(term->vte));
#ifndef MENU_CUSTOM
          encoding_item[i] = gtk_menu_item_new_with_mnemonic(LABEL_DEFAULT_ENCODING);
#endif
#ifdef MENU_CUSTOM
          encoding_item[i] = gtk_image_menu_item_new_with_mnemonic(LABEL_DEFAULT_ENCODING);
#endif
        } else
#endif
#ifndef MENU_CUSTOM
          encoding_item[i] = gtk_menu_item_new_with_label(encoding[i]);
#endif
#ifdef MENU_CUSTOM
          encoding_item[i] = gtk_image_menu_item_new_with_label(encoding[i]);
        gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(encoding_item[i]), gtk_image_new_from_stock(GTK_STOCK_CONVERT, GTK_ICON_SIZE_MENU));
        menu_item_success++;
#endif
        gtk_menu_shell_append(GTK_MENU_SHELL(menu), encoding_item[i]);
        g_signal_connect(encoding_item[i], "activate", G_CALLBACK(set_encoding), encoding[i]);
      }
    }
#endif /* MENU_ENCODING_LIST */
#if !defined(MENU_ENCODING_LIST) && MENU
    {
#ifndef MENU_CUSTOM
      encoding_item = gtk_menu_item_new_with_label("UTF-8");
#endif
#ifdef MENU_CUSTOM
      encoding_item = gtk_image_menu_item_new_with_label("UTF-8");
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(encoding_item), gtk_image_new_from_stock(GTK_STOCK_CONVERT, GTK_ICON_SIZE_MENU));
      menu_item_success++;
#endif
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), encoding_item);
      g_signal_connect(encoding_item, "activate", G_CALLBACK(set_encoding), "UTF-8");
    }
#endif

#ifdef MENU_CUSTOM
    if (!strncmp(menu_custom[j], "Input method", 13))
#endif
#if MENU
    {
#if COMMAND_TAB_NUMBERS
      GET_CURRENT_TAB(0);
#endif
      vte_terminal_im_append_menuitems(VTE_TERMINAL(term->vte), GTK_MENU_SHELL(menu));
#ifdef MENU_CUSTOM
      menu_item_success++;
#endif
    }
#endif

#ifdef SUBMENU_ENCODING_LIST
    if (!strncmp(menu_custom[j], "Submenu encoding list", 22)) {
      GtkWidget *subitem_enc = gtk_image_menu_item_new_with_mnemonic(LABEL_SUBMENU_ENCODING);
      GtkWidget *submenu_enc = gtk_menu_new();
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), subitem_enc);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(subitem_enc), gtk_image_new_from_stock(GTK_STOCK_CONVERT, GTK_ICON_SIZE_MENU));
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(subitem_enc), submenu_enc);
#ifdef MENU_ENCODING_LIST
      GtkWidget *encoding_sub[MENU_ENCODING_LIST_SIZE];
      for (i = 0 ; i < MENU_ENCODING_LIST_SIZE ; i++) {
#ifdef MENU_DEFAULT_ENCODING
        if (!strncmp(encoding[i], "Default Encoding", 17)) {
          encoding[i] = (char*)vte_terminal_get_encoding(VTE_TERMINAL(term->vte));
          encoding_sub[i] = gtk_menu_item_new_with_mnemonic(LABEL_DEFAULT_ENCODING);
        } else
#endif
          encoding_sub[i] = gtk_menu_item_new_with_label(encoding[i]);
        gtk_menu_shell_append(GTK_MENU_SHELL(submenu_enc), encoding_sub[i]);
        g_signal_connect(encoding_sub[i], "activate", G_CALLBACK(set_encoding), encoding[i]);
      }
#endif
#ifndef MENU_ENCODING_LIST
      encoding_item = gtk_menu_item_new_with_label("UTF-8");
      gtk_menu_shell_append(GTK_MENU_SHELL(submenu_enc), encoding_item);
      g_signal_connect(encoding_item, "activate", G_CALLBACK(set_encoding), "UTF-8");
#endif
      menu_item_success++;
    }
#endif

#ifdef SUBMENU_INPUT_METHOD
    if (!strncmp(menu_custom[j], "Submenu input method", 21)) {
      GtkWidget *subitem_ime = gtk_image_menu_item_new_with_mnemonic(LABEL_SUBMENU_IME);
      GtkWidget *submenu_ime = gtk_menu_new();
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), subitem_ime);
      gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(subitem_ime), gtk_image_new_from_stock(GTK_STOCK_INFO, GTK_ICON_SIZE_MENU));
      gtk_menu_item_set_submenu(GTK_MENU_ITEM(subitem_ime), submenu_ime);
#if COMMAND_TAB_NUMBERS
      GET_CURRENT_TAB(0);
#endif
      vte_terminal_im_append_menuitems(VTE_TERMINAL(term->vte), GTK_MENU_SHELL(submenu_ime));
      menu_item_success++;
    }
#endif
  }

#if MENU
#ifdef MENU_MATCH_STRING_EXEC
#ifdef MENU_CUSTOM
  if (menu_item_success > 0)
#endif
  {
    match_item = gtk_separator_menu_item_new();
    gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), match_item);
  }
  match_copy = gtk_image_menu_item_new_from_stock(GTK_STOCK_COPY, NULL);
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), match_copy);
  g_signal_connect(match_copy, "activate", do_match_copy, NULL);
  match_open = gtk_image_menu_item_new_from_stock(GTK_STOCK_OPEN, NULL);
  gtk_menu_shell_prepend(GTK_MENU_SHELL(menu), match_open);
  g_signal_connect(match_open, "activate", do_match_open, NULL);
#endif
  gtk_widget_show_all(menu);
#endif

  gtk_main();
  return FALSE;
}
