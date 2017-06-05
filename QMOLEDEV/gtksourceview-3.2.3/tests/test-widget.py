#!/usr/bin/env python
# -*- Mode: Python; py-indent-offset: 4 -*-
# vim: tabstop=4 shiftwidth=4 expandtab

import os, os.path
import sys

import gi
from gi.repository import Gio, GObject, Pango, Gtk, GdkPixbuf, Gdk, GtkSource

ui_description = """
<ui>
  <menubar name=\"MainMenu\">
    <menu action=\"FileMenu\">
      <menuitem action=\"Open\"/>
      <menuitem action=\"Print\"/>
      <menuitem action=\"Find\"/>
      <menuitem action=\"Replace\"/>
      <separator/>
      <menuitem action=\"Quit\"/>
    </menu>
    <menu action=\"ViewMenu\">
      <menuitem action=\"NewView\"/>
      <separator/>
      <menuitem action=\"HlSyntax\"/>
      <menuitem action=\"HlBracket\"/>
      <menuitem action=\"ShowNumbers\"/>
      <menuitem action=\"ShowMarks\"/>
      <menuitem action=\"ShowMargin\"/>
      <menuitem action=\"HlLine\"/>
      <menuitem action=\"DrawSpaces\"/>
      <menuitem action=\"WrapLines\"/>
      <separator/>
      <menuitem action=\"AutoIndent\"/>
      <menuitem action=\"InsertSpaces\"/>
      <separator/>
      <menu action=\"TabWidth\">
        <menuitem action=\"TabWidth4\"/>
        <menuitem action=\"TabWidth6\"/>
        <menuitem action=\"TabWidth8\"/>
        <menuitem action=\"TabWidth10\"/>
        <menuitem action=\"TabWidth12\"/>
      </menu>
      <menu action=\"IndentWidth\">
        <menuitem action=\"IndentWidthUnset\"/>
        <menuitem action=\"IndentWidth4\"/>
        <menuitem action=\"IndentWidth6\"/>
        <menuitem action=\"IndentWidth8\"/>
        <menuitem action=\"IndentWidth10\"/>
        <menuitem action=\"IndentWidth12\"/>
      </menu>
      <separator/>
      <menu action=\"SmartHomeEnd\">
        <menuitem action=\"SmartHomeEndDisabled\"/>
        <menuitem action=\"SmartHomeEndBefore\"/>
        <menuitem action=\"SmartHomeEndAfter\"/>
        <menuitem action=\"SmartHomeEndAlways\"/>
      </menu>
      <separator/>
      <menuitem action=\"ForwardString\"/>
      <menuitem action=\"BackwardString\"/>
    </menu>
    <menu action=\"HelpMenu\">
      <menuitem action=\"About\"/>
    </menu>
  </menubar>
</ui>
"""

class AboutDialog(Gtk.AboutDialog):

    def __init__(self, parent):
        Gtk.AboutDialog.__init__(self)
        self.set_name('GtkSourceView Test')
        self.set_copyright('Copyright (c) 2010 Ignacio Casal Quinteiro')
        self.set_website_label('http://projects.gnome.org/gtksourceview/')
        self.set_authors(['Ignacio Casal Quinteiro', 'Paolo Borelli'])
        self.set_transient_for(parent)
        self.connect("response", lambda d, r: d.destroy())

class SearchDialog(Gtk.Dialog):

    def __init__(self, parent, replace, what, replacement):
        if replace:
            title = "Replace"
        else:
            title = "Find"

        Gtk.Dialog.__init__(self, title, parent, Gtk.DialogFlags.MODAL,
                            (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL,
                             Gtk.STOCK_OK, Gtk.ResponseType.OK))

        self._search_widget = Gtk.Entry()
        if what:
            self._search_widget.set_text(what)
        self._search_widget.set_activates_default(True)
        self._search_widget.show()
        print self.get_content_area()
        self.get_content_area().pack_start(self._search_widget, True, True, 0)

        if replace:
            self._replace_widget = Gtk.Entry()
            if replacement:
                self._replace_widget.set_text(replacement)
            self._replace_widget.set_activates_default(True)
            self._replace_widget.show()
            self.get_content_area().pack_start(self._replace_widget, True, True, 0)

        self._case_sensitive = Gtk.CheckButton.new_with_label("Case sensitive")
        self._case_sensitive.show()
        self.get_content_area().pack_start(self._case_sensitive, False, False, 0)

    def run_search(self):
        while True:
            if self.run() != Gtk.ResponseType.OK:
                self.hide()
                return False

            if self._search_widget.get_text() != "":
                break

        self.hide()
        return True

    def is_case_sensitive(self):
        return self._case_sensitive.get_active()

    def get_search_text(self):
        return self._search_widget.get_text()

    def get_replace_text(self):
        return self._replace_widget.get_text()

class Window(Gtk.Window):

    def __init__(self):
        Gtk.Window.__init__(self)

        self.MARK_TYPE_1 = "one"
        self.MARK_TYPE_2 = "two"

        self.set_title('GtkSourceView Demo')
        self.set_icon_name('text-editor')
        self.set_default_size(500, 500)
        self.connect_after('destroy', _quit)

        self._vbox = Gtk.VBox()
        self.add(self._vbox)

        sw = Gtk.ScrolledWindow(hadjustment=None,
                                vadjustment=None)
        sw.set_shadow_type(Gtk.ShadowType.IN)

        self._buf = GtkSource.Buffer()
        self._view = GtkSource.View.new_with_buffer(self._buf)
        self.insert_menu()

        mgr = GtkSource.StyleSchemeManager.get_default()
        style_scheme = mgr.get_scheme('classic')
        if style_scheme:
            self._buf.set_style_scheme(style_scheme)

        self._vbox.pack_start(sw, True, True, 0)
        sw.add(self._view)

        self._pos_label = Gtk.Label()
        self._vbox.pack_end(self._pos_label, False, False, 0)

        self.add_source_mark_pixbufs()

        self._buf.connect("mark-set", self.move_cursor_cb, None)
        self._view.connect("line-mark-activated", self.line_mark_activated, None)
        self._buf.connect("bracket-matched", self.bracket_matched, None);

    def insert_menu(self):
        action_group = Gtk.ActionGroup("GtkSourceViewActions")
        action_group.add_actions([("FileMenu", None, "_File", None, None, None),
                                  ("Open", Gtk.STOCK_OPEN, "_Open", "<control>O",
                                   "Open a file", self.open_file_cb),
                                  ("Print", Gtk.STOCK_PRINT, "_Print", "<control>P",
                                   "Print the current file", self.print_file_cb),
                                  ("Find", Gtk.STOCK_FIND, "_Find", "<control>F",
                                   "Find", self.find_cb),
                                  ("Replace", Gtk.STOCK_FIND_AND_REPLACE, "Search and _Replace", "<control>R",
                                   "Search and replace", self.replace_cb),
                                  ("Quit", Gtk.STOCK_QUIT, "_Quit", "<control>Q",
                                   "Exit the application", self.quit_cb),
                                  ("ViewMenu", None, "_View", None, None, None),
                                  ("NewView", Gtk.STOCK_NEW, "_New View", None,
                                   "Create a new view of the file", self.new_view_cb),
                                  ("TabWidth", None, "_Tab Width", None, None, None),
                                  ("IndentWidth", None, "I_ndent Width", None, None, None),
                                  ("SmartHomeEnd", None, "_Smart Home/End", None, None, None),
                                  ("ForwardString", None, "_Forward to string toggle", "<control>S",
                                   "Forward to the start or end of the next string", self.forward_string_cb),
                                  ("BackwardString", None, "_Backward to string toggle", "<control><shift>S",
                                   "Backward to the start or end of the next string", self.backward_string_cb),
                                  ("HelpMenu", None, "_Help", None, None, None),
                                  ("About", Gtk.STOCK_ABOUT, "_About...", None,
                                   "About GtkSourceView Test Widget", self.about_cb)])

        action_group.add_toggle_actions([("HlSyntax", None, "Highlight _Syntax", None,
                                          "Toggle syntax highlighting", self.hl_syntax_toggled_cb),
                                         ("HlBracket", None, "Highlight Matching _Bracket", None,
                                          "Toggle highlighting of matching bracket", self.hl_bracket_toggled_cb),
                                         ("ShowNumbers", None, "Show _Line Numbers", None,
                                          "Toggle visibility of line numbers in the left margin", self.numbers_toggled_cb),
                                         ("ShowMarks", None, "Show Line _Marks", None,
                                          "Toggle visibility of marks in the left margin", self.marks_toggled_cb),
                                         ("ShowMargin", None, "Show Right M_argin", None,
                                          "Toggle visibility of right margin indicator", self.margin_toggled_cb),
                                         ("HlLine", None, "_Highlight Current Line", None,
                                          "Toggle highlighting of current line", self.hl_line_toggled_cb),
                                         ("DrawSpaces", None, "_Draw Spaces", None,
                                          "Draw Spaces", self.draw_spaces_toggled_cb),
                                         ("WrapLines", None, "_Wrap Lines", None,
                                          "Toggle line wrapping", self.wrap_lines_toggled_cb),
                                         ("AutoIndent", None, "Enable _Auto Indent", None,
                                          "Toggle automatic auto indentation of text", self.auto_indent_toggled_cb),
                                         ("InsertSpaces", None, "Insert _Spaces Instead of Tabs", None,
                                          "Whether to insert space characters when inserting tabulations",
                                          self.insert_spaces_toggled_cb)])

        action_group.add_radio_actions([("TabWidth4", None, "4", None,
                                         "Set tabulation width to 4 spaces", 4),
                                        ("TabWidth6", None, "6", None,
                                         "Set tabulation width to 6 spaces", 6),
                                        ("TabWidth8", None, "8", None,
                                         "Set tabulation width to 8 spaces", 8),
                                        ("TabWidth10", None, "10", None,
                                         "Set tabulation width to 10 spaces", 10),
                                        ("TabWidth12", None, "12", None,
                                         "Set tabulation width to 12 spaces", 12)],
                                        -1, self.tabs_toggled_cb)

        action_group.add_radio_actions([("IndentWidthUnset", None, "Use Tab Width", None,
                                         "Set indent width same as tab width", -1),
                                        ("IndentWidth4", None, "4", None,
                                         "Set indent width to 4 spaces", 4),
                                        ("IndentWidth6", None, "6", None,
                                         "Set indent width to 6 spaces", 6),
                                        ("IndentWidth8", None, "8", None,
                                         "Set indent width to 8 spaces", 8),
                                        ("IndentWidth10", None, "10", None,
                                         "Set indent width to 10 spaces", 10),
                                        ("IndentWidth12", None, "12", None,
                                         "Set indent width to 12 spaces", 12)],
                                        -1, self.indent_toggled_cb)

        action_group.add_radio_actions([("SmartHomeEndDisabled", None, "Disabled", None,
                                         "Smart Home/End disabled", GtkSource.SmartHomeEndType.DISABLED),
                                        ("SmartHomeEndBefore", None, "Before", None,
                                         "Smart Home/End before", GtkSource.SmartHomeEndType.BEFORE),
                                        ("SmartHomeEndAfter", None, "After", None,
                                         "Smart Home/End after", GtkSource.SmartHomeEndType.AFTER),
                                        ("SmartHomeEndAlways", None, "Always", None,
                                         "Smart Home/End always", GtkSource.SmartHomeEndType.ALWAYS)],
                                        -1, self.smart_home_end_toggled_cb)

        self._ui_manager = Gtk.UIManager()
        self._ui_manager.insert_action_group(action_group, 0)

        try:
            self._ui_manager.add_ui_from_string(ui_description)
        except:
            return

        menu = self._ui_manager.get_widget("/MainMenu")
        self._vbox.pack_start(menu, False, False, 0)

        accel_group = self._ui_manager.get_accel_group()
        self.add_accel_group (accel_group)

        # Add default values
        action_group.get_action("HlSyntax").set_active(True)
        action_group.get_action("HlBracket").set_active(True)
        action_group.get_action("ShowNumbers").set_active(True)
        action_group.get_action("ShowMarks").set_active(True)
        action_group.get_action("ShowMargin").set_active(True)
        action_group.get_action("AutoIndent").set_active(True)
        action_group.get_action("TabWidth8").set_active(True)
        action_group.get_action("IndentWidthUnset").set_active(True)

    def add_source_mark_pixbufs(self):
        attrs = GtkSource.MarkAttributes ();
        color = Gdk.RGBA();
        parsed = color.parse("lightgreen")
        if parsed:
            attrs.set_background(color)
        attrs.set_stock_id(Gtk.STOCK_YES)
        attrs.connect("query-tooltip-markup", self.mark_tooltip_func)
        self._view.set_mark_attributes (self.MARK_TYPE_1, attrs, 1)

        attrs = GtkSource.MarkAttributes ();
        color = Gdk.RGBA();
        parsed = color.parse("pink")
        if parsed:
            attrs.set_background(color)
        attrs.set_stock_id(Gtk.STOCK_NO)
        attrs.connect("query-tooltip-markup", self.mark_tooltip_func)
        self._view.set_mark_attributes (self.MARK_TYPE_2, attrs, 2)

    def remove_all_marks(self):
        start, end = self._buf.get_bounds()
        self._buf.remove_source_marks(start, end, None)

    def mark_tooltip_func(self, attrs, mark):
        i = self._buf.get_iter_at_mark(mark)
        line = i.get_line() + 1
        column = i.get_line_offset()

        if mark.get_category() == self.MARK_TYPE_1:
            return "Line: %d, Column: %d" % (line, column)
        else:
            return "<b>Line</b>: %d\n<i>Column</i>: %d" % (line, column)

    def update_cursor_position(self):
        i = self._buf.get_iter_at_mark(self._buf.get_insert())
        chars = i.get_offset()
        row = i.get_line() + 1
        col = self._view.get_visual_column(i) + 1

        classes = self._buf.get_context_classes_at_iter(i)

        classes_str = ""

        i = 0
        for c in classes:
            if len(classes) != i + 1:
                classes_str += c + ", "
            else:
                classes_str += c

        msg = "char: %d, line: %d, column: %d, classes: %s" % (chars, row, col, classes_str)
        self._pos_label.set_text(msg)

    # Callbacks
    def open_file_cb(self, action, user_data=None):
        chooser = Gtk.FileChooserDialog("Open File...", None,
                                        Gtk.FileChooserAction.OPEN,
                                        (Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL,
                                         Gtk.STOCK_OPEN, Gtk.ResponseType.OK))

        response = chooser.run()

        if response == Gtk.ResponseType.OK:
            filename = chooser.get_filename()

            if filename:
                self.open_file(filename)

        chooser.destroy()

    def print_file_cb(self, action, user_data=None):
        compositor = GtkSource.PrintCompositor.new_from_view (self._view)
        operation = Gtk.PrintOperation ()
        operation.set_job_name (os.path.basename(self._filepath))
        operation.set_show_progress (True)

        # we do blocking pagination
        operation.connect('paginate', self.paginate_cb, compositor)
        operation.connect('draw-page', self.draw_page_cb, compositor)

        operation.run (Gtk.PrintOperationAction.PRINT_DIALOG, None)

    def find_cb(self, action, user_data=None):
        dialog = SearchDialog(self, False, None, None)

        if dialog.is_case_sensitive:
            search_flags = GtkSource.SearchFlags.CASE_INSENSITIVE
        else:
            search_flags = 0

        if dialog.run_search():
            i = self._buf.get_iter_at_mark(self._buf.get_insert())

            searched, start, end = i.forward_search(dialog.get_search_text(),
                                                    search_flags, None)
            if searched:
                self._buf.select_range(start, end)
            else:
                end = i
                i = self._buf.get_start_iter()

                searched, start, end = i.forward_search(dialog.get_search_text(),
                                                        search_flags, end)
                if searched:
                    self._buf.select_range(start, end)

    def replace_cb(self, action, user_data=None):
        dialog = SearchDialog(self, False, None, None)

        if dialog.is_case_sensitive:
            search_flags = GtkSource.SearchFlags.CASE_INSENSITIVE
        else:
            search_flags = 0

        i = self._buf.get_start_iter()

        while True:
            searched, start, end = i.forward_search(dialog.get_search_text(),
                                                    search_flags, None)

            if not searched:
                break

            self._buf.delete(start, end)
            self._buf.insert(start, dialog.get_replace_text())
            i = start

    def quit_cb(self, action, user_data=None):
        _quit()

    def new_view_cb(self, action, user_data=None):
        window = Window()
        window.show_all()

    def hl_syntax_toggled_cb(self, action, user_data=None):
        self._buf.set_highlight_syntax(action.get_active())

    def hl_bracket_toggled_cb(self, action, user_data=None):
        self._buf.set_highlight_matching_brackets(action.get_active())

    def numbers_toggled_cb(self, action, user_data=None):
        self._view.set_show_line_numbers(action.get_active())

    def marks_toggled_cb(self, action, user_data=None):
        self._view.set_show_line_marks(action.get_active())

    def margin_toggled_cb(self, action, user_data=None):
        self._view.set_show_right_margin(action.get_active())

    def hl_line_toggled_cb(self, action, user_data=None):
        self._view.set_highlight_current_line(action.get_active())

    def draw_spaces_toggled_cb(self, action, user_data=None):

        if (action.get_active()):
            draw_spaces = GtkSource.DrawSpacesFlags.ALL
        else:
            draw_spaces = 0
        self._view.set_draw_spaces(draw_spaces)

    def wrap_lines_toggled_cb(self, action, user_data=None):

        if (action.get_active()):
            wrap_mode = Gtk.WrapMode.WORD
        else:
            wrap_mode = Gtk.WrapMode.NONE
        self._view.set_wrap_mode(wrap_mode)

    def auto_indent_toggled_cb(self, action, user_data=None):
        self._view.set_auto_indent(action.get_active())

    def insert_spaces_toggled_cb(self, action, user_data=None):
        self._view.set_insert_spaces_instead_of_tabs(action.get_active())

    def forward_string_cb(self, action, user_data=None):
        insert = self._buf.get_insert()

        it = self._buf.get_iter_at_mark(insert)

        if (self._buf.iter_forward_to_context_class_toggle(it, "string")):
            self._buf.place_cursor(it)
            self._view.scroll_mark_onscreen(insert)

    def backward_string_cb(self, action, user_data=None):
        insert = self._buf.get_insert()

        it = self._buf.get_iter_at_mark(insert)

        if (self._buf.iter_backward_to_context_class_toggle(it, "string")):
            self._buf.place_cursor(it)
            self._view.scroll_mark_onscreen(insert)

    def tabs_toggled_cb(self, action, current, user_data=None):
        self._view.set_tab_width(current.get_current_value())

    def indent_toggled_cb(self, action, current):
        self._view.set_indent_width(current.get_current_value())

    def smart_home_end_toggled_cb(self, action, current, user_data=None):
        self._view.set_smart_home_end(current.get_current_value())

    def about_cb(self, action, user_data=None):
        about = AboutDialog(self)
        about.show()

    def move_cursor_cb(self, buf, cursor_iter, mark, user_data):
        if mark != buf.get_insert():
            return

        self.update_cursor_position()

    def line_mark_activated(self, gutter, place, ev, user_data):
        if ev.button.button == 1:
            mark_type = self.MARK_TYPE_1
        else:
            mark_type = self.MARK_TYPE_2

        mark_list = self._buf.get_source_marks_at_line(place.get_line(), mark_type)

        if mark_list:
            self._buf.delete_mark(mark_list[0])
        else:
            self._buf.create_source_mark(None, mark_type, place)

    def bracket_matched(self, buf, place, state, user_data):
        # FIXME: figure out how to obtain the nick from the enum value
        # print "Bracket match state: '%s'\n" % nick
        if state == GtkSource.BracketMatchType.FOUND:
            bracket = place.get_char()
            row = place.get_line() + 1
            col = place.get_line_offset() + 1
            print "Matched bracket: '%c' at row: %i, col: %i\n" % bracket % row % col

    def open_file(self, filename):
        if os.path.isabs(filename):
            path = filename
        else:
            path = os.path.abspath(filename)

        self._filepath = path

        f = Gio.file_new_for_path(path)

        info = f.query_info("*", 0, None)
        content_type = info.get_content_type()

        mgr = GtkSource.LanguageManager.get_default()
        language = mgr.guess_language(filename, content_type)

        self.remove_all_marks()
        self._buf.set_language(language)
        self._buf.set_highlight_syntax(True)

        self._buf.begin_not_undoable_action()

       # stream = f.read(None)
       # chunk, r = stream.read(4096, None)

        #FIXME: Use Gio
        try:
            txt = open(path, 'r').read()
        except:
            return False

        self._buf.set_text(txt, -1)

        self._buf.end_not_undoable_action()

        self._buf.set_modified(False)

        i = self._buf.get_start_iter()
        self._buf.place_cursor(i)
        return True

    def paginate_cb(self, operation, context, compositor):
        print "Pagination progress: %.2f %%\n" % (compositor.get_pagination_progress () * 100.0)
        if compositor.paginate (context):
            print "Pagination progress: %.2f %%\n" % (compositor.get_pagination_progress () * 100.0)
            operation.set_n_pages (compositor.get_n_pages ())
            return True
        return False

    def draw_page_cb(self, operation, context, page_nr, compositor):
        compositor.draw_page (context, page_nr)

def _quit(*args):
    Gtk.main_quit()

def main(args = []):
    global window

    window = Window()

    if len(args) > 2:
        window.open_file(args[1])
    else:
        window.open_file(args[0])

    window.show_all()
    Gtk.main()

if __name__ == '__main__':
    main(sys.argv)

