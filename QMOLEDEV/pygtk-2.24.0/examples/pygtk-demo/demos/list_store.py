#!/usr/bin/env python
'''Tree View/List Store

The GtkListStore is used to store data in list form, to be used
later on by a GtkTreeView to display it. This demo builds a
simple GtkListStore and displays it. See the Stock Browser
demo for a more advanced example.'''

import pygtk
pygtk.require('2.0')
import gobject
import gtk

(
    COLUMN_FIXED,
    COLUMN_NUMBER,
    COLUMN_SEVERITY,
    COLUMN_DESCRIPTION
) = range(4)

data = \
((False, 60482, 'Normal', 'scrollable notebooks and hidden tabs'),
 (False, 60620, 'Critical',
  'gdk_window_clear_area(gdkwindow-win32.c) is not thread-safe'),
 (False, 50214, 'Major', 'Xft support does not clean up correctly'),
 (True,  52877, 'Major', 'GtkFileSelection needs a refresh method. '),
 (False, 56070, 'Normal', "Can't click button after setting in sensitive"),
 (True,  56355, 'Normal', 'GtkLabel - Not all changes propagate correctly'),
 (False, 50055, 'Normal', 'Rework width/height computations for TreeView'),
 (False, 58278, 'Normal', "gtk_dialog_set_response_sensitive() doesn't work"),
 (False, 55767, 'Normal', 'Getters for all setters'),
 (False, 56925, 'Normal', 'Gtkcalender size'),
 (False, 56221, 'Normal', 'Selectable label needs right-click copy menu'),
 (True,  50939, 'Normal', 'Add shift clicking to GtkTextView'),
 (False, 6112,  'Enhancement', 'netscape-like collapsable toolbars'),
 (False, 1,     'Normal', 'First bug :=)'))

class ListStoreDemo(gtk.Window):
    def __init__(self, parent=None):
        # create window, etc
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.set_title(self.__class__.__name__)

        self.set_border_width(8)
        self.set_default_size(300, 250)

        vbox = gtk.VBox(False, 8)
        self.add(vbox)

        label = gtk.Label('This is the bug list (note: not based on real data, '
            'it would be nice to have a nice ODBC interface to bugzilla or so, though).')
        vbox.pack_start(label, False, False)

        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
        vbox.pack_start(sw)

        # create tree model
        model = self.__create_model()

        # create tree view
        treeview = gtk.TreeView(model)
        treeview.set_rules_hint(True)
        treeview.set_search_column(COLUMN_DESCRIPTION)

        sw.add(treeview)

        # add columns to the tree view
        self.__add_columns(treeview)

        self.show_all()

    def __create_model(self):
        lstore = gtk.ListStore(
            gobject.TYPE_BOOLEAN,
            gobject.TYPE_UINT,
            gobject.TYPE_STRING,
            gobject.TYPE_STRING)

        for item in data:
            iter = lstore.append()
            lstore.set(iter,
                COLUMN_FIXED, item[COLUMN_FIXED],
                COLUMN_NUMBER, item[COLUMN_NUMBER],
                COLUMN_SEVERITY, item[COLUMN_SEVERITY],
                COLUMN_DESCRIPTION, item[COLUMN_DESCRIPTION])
        return lstore

    def fixed_toggled(self, cell, path, model):
        # get toggled iter
        iter = model.get_iter((int(path),))
        fixed = model.get_value(iter, COLUMN_FIXED)

        # do something with the value
        fixed = not fixed

        # set new value
        model.set(iter, COLUMN_FIXED, fixed)

    def __add_columns(self, treeview):
        model = treeview.get_model()

        # column for fixed toggles
        renderer = gtk.CellRendererToggle()
        renderer.connect('toggled', self.fixed_toggled, model)

        column = gtk.TreeViewColumn('Fixed', renderer, active=COLUMN_FIXED)

        # set this column to a fixed sizing(of 50 pixels)
        column.set_sizing(gtk.TREE_VIEW_COLUMN_FIXED)
        column.set_fixed_width(50)

        treeview.append_column(column)

        # column for bug numbers
        column = gtk.TreeViewColumn('Bug Number', gtk.CellRendererText(),
                                    text=COLUMN_NUMBER)
        column.set_sort_column_id(COLUMN_NUMBER)
        treeview.append_column(column)

        # columns for severities
        column = gtk.TreeViewColumn('Severity', gtk.CellRendererText(),
                                    text=COLUMN_SEVERITY)
        column.set_sort_column_id(COLUMN_SEVERITY)
        treeview.append_column(column)

        # column for description
        column = gtk.TreeViewColumn('Description', gtk.CellRendererText(),
                                     text=COLUMN_DESCRIPTION)
        column.set_sort_column_id(COLUMN_DESCRIPTION)
        treeview.append_column(column)

def main():
    ListStoreDemo()
    gtk.main()

if __name__ == '__main__':
    main()
