#!/usr/bin/env python

import os, os.path
import sys

from gi.repository import Gio, GObject, Pango, Gtk, GdkPixbuf, Gdk, GtkSource

class TestProvider(GObject.Object, GtkSource.CompletionProvider):
    __gtype_name__ = 'TestProvider'

    def __init__(self):
        GObject.Object.__init__(self)

        self.priority = 1

        theme = Gtk.IconTheme.get_default()
        icon = theme.load_icon(Gtk.STOCK_DIALOG_INFO, 16, 0)

        self.proposals = []
        self.proposals.append(GtkSource.CompletionItem.new("Proposal 1", "Proposal 1", icon, "blah 1"))
        self.proposals.append(GtkSource.CompletionItem.new("Proposal 2", "Proposal 2", icon, "blah 2"))
        self.proposals.append(GtkSource.CompletionItem.new("Proposal 3", "Proposal 3", icon, "blah 3"))

    def do_get_name(self):
        return "Test Provider"

    def do_get_priority(self):
        return self.priority

    def do_match(self, context):
        return True

    def do_populate(self, context):
        context.add_proposals(self, self.proposals, True)

    def set_priority(self, priority):
        self.priority = priority

GObject.type_register(TestProvider)

def create_completion():
    prov_words = GtkSource.CompletionWords.new(None, None)
    prov_words.register(view.get_buffer())

    prov_words.set_property("priority", 10)

    completion.add_provider(prov_words)

    test1 = TestProvider()
    completion.add_provider(test1)

    test2 = TestProvider()
    completion.add_provider(test2)
    test2.set_priority(5)

def remember_toggled_cb(button, data):
    completion.set_property("remember-info-visibility", button.get_active())

def select_on_show_toggled_cb(button, data):
    completion.set_property("select-on-show", button.get_active())

def show_headers_toggled_cb(button, data):
    completion.set_property("show-headers", button.get_active())

def show_icons_toggled_cb(button, data):
    completion.set_property("show-icons", button.get_active())

def toggle_active_property(source, dest, name):
    value = source.get_property(name)
    dest.set_property("active", value)

def create_window():
    global window
    global view
    global completion

    window = Gtk.Window()
    window.set_title('GtkSourceView Completion Demo')
    window.set_icon_name('text-editor')
    window.set_default_size(600, 400)
    window.connect_after('destroy', _quit)

    vbox = Gtk.VBox()
    hbox = Gtk.HBox()

    sw = Gtk.ScrolledWindow(hadjustment=None,
                            vadjustment=None)
    sw.set_shadow_type(Gtk.ShadowType.IN)

    view = GtkSource.View.new()
    sw.add(view)

    completion = view.get_completion()

    remember = Gtk.CheckButton.new_with_label("Remember info visibility")
    select_on_show = Gtk.CheckButton.new_with_label("Select first on show")
    show_headers = Gtk.CheckButton.new_with_label("Show headers")
    show_icons = Gtk.CheckButton.new_with_label("Show icons")

    toggle_active_property(completion, remember, "remember-info-visibility")
    toggle_active_property(completion, select_on_show, "select-on-show")
    toggle_active_property(completion, show_headers, "show-headers")
    toggle_active_property(completion, show_icons, "show-icons")

    hbox.pack_start(remember, False, False, 0)
    hbox.pack_start(select_on_show, False, False, 0)
    hbox.pack_start(show_headers, False, False, 0)
    hbox.pack_start(show_icons, False, False, 0)

    vbox.pack_start(sw, True, True, 0)
    vbox.pack_end(hbox, False, False, 0)

    window.add(vbox)

    remember.connect("toggled", remember_toggled_cb, None)
    select_on_show.connect("toggled", select_on_show_toggled_cb, None)
    show_headers.connect("toggled", show_headers_toggled_cb, None)
    show_icons.connect("toggled", show_icons_toggled_cb, None)

    return window

def _quit(*args):
    Gtk.main_quit()

def main(args = []):

    window = create_window()
    create_completion()

    window.show_all()

    Gtk.main()

if __name__ == '__main__':
    main(sys.argv)


