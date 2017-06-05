#!/usr/bin/env python
'''Tree View/Editable Cells

This demo demonstrates the use of editable cells in a GtkTreeView.
If you're new to the GtkTreeView widgets and associates, look into the
GtkListStore example first.'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import pygtk
pygtk.require('2.0')
import gobject
import gtk

#   columns
(
  COLUMN_NUMBER,
  COLUMN_PRODUCT,
  COLUMN_EDITABLE
) = range(3)

# data
articles = [
    [ 3, "bottles of coke", True ],
    [ 5, "packages of noodles", True ],
    [ 2, "packages of chocolate chip cookies", True ],
    [ 1, "can vanilla ice cream", True ],
    [ 6, "eggs", True ]
]

class EditableCellsDemo(gtk.Window):
    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.set_title(self.__class__.__name__)
        self.set_border_width(5)
        self.set_default_size(320, 200)

        vbox = gtk.VBox(False, 5)
        self.add(vbox)

        label = gtk.Label("Shopping list (you can edit the cells!)")
        vbox.pack_start(label, False, False)

        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        vbox.pack_start(sw)

        # create model
        model = self.__create_model()

        # create tree view
        treeview = gtk.TreeView(model)
        treeview.set_rules_hint(True)
        treeview.get_selection().set_mode(gtk.SELECTION_SINGLE)

        self.__add_columns(treeview)

        sw.add(treeview)

        # some buttons
        hbox = gtk.HBox(True, 4)
        vbox.pack_start(hbox, False, False)

        button = gtk.Button(stock=gtk.STOCK_ADD)
        button.connect("clicked", self.on_add_item_clicked, model)
        hbox.pack_start(button)

        button = gtk.Button(stock=gtk.STOCK_REMOVE)
        button.connect("clicked", self.on_remove_item_clicked, treeview)
        hbox.pack_start(button)

        self.show_all()

    def __create_model(self):

        # create list store
        model = gtk.ListStore(
            gobject.TYPE_INT,
            gobject.TYPE_STRING,
            gobject.TYPE_BOOLEAN
       )

        # add items
        for item in articles:
            iter = model.append()

            model.set (iter,
                  COLUMN_NUMBER, item[COLUMN_NUMBER],
                  COLUMN_PRODUCT, item[COLUMN_PRODUCT],
                  COLUMN_EDITABLE, item[COLUMN_EDITABLE]
           )
        return model


    def __add_columns(self, treeview):

        model = treeview.get_model()

        # number column
        renderer = gtk.CellRendererText()
        renderer.connect("edited", self.on_cell_edited, model)
        renderer.set_data("column", COLUMN_NUMBER)

        column = gtk.TreeViewColumn("Number", renderer, text=COLUMN_NUMBER,
                               editable=COLUMN_EDITABLE)
        treeview.append_column(column)

        # product column
        renderer = gtk.CellRendererText()
        renderer.connect("edited", self.on_cell_edited, model)
        renderer.set_data("column", COLUMN_PRODUCT)

        column = gtk.TreeViewColumn("Product", renderer, text=COLUMN_PRODUCT,
                               editable=COLUMN_EDITABLE)
        treeview.append_column(column)


    def on_add_item_clicked(self, button, model):
        new_item = [0, "Description here", True]
        articles.append(new_item)

        iter = model.append()
        model.set (iter,
            COLUMN_NUMBER, new_item[COLUMN_NUMBER],
            COLUMN_PRODUCT, new_item[COLUMN_PRODUCT],
            COLUMN_EDITABLE, new_item[COLUMN_EDITABLE]
       )


    def on_remove_item_clicked(self, button, treeview):

        selection = treeview.get_selection()
        model, iter = selection.get_selected()

        if iter:
            path = model.get_path(iter)[0]
            model.remove(iter)

            del articles[ path ]


    def on_cell_edited(self, cell, path_string, new_text, model):

        iter = model.get_iter_from_string(path_string)
        path = model.get_path(iter)[0]
        column = cell.get_data("column")

        if column == COLUMN_NUMBER:
            articles[path][COLUMN_NUMBER] = int(new_text)

            model.set(iter, column, articles[path][COLUMN_NUMBER])

        elif column == COLUMN_PRODUCT:
            old_text = model.get_value(iter, column)
            articles[path][COLUMN_PRODUCT] = new_text

            model.set(iter, column, articles[path][COLUMN_PRODUCT])

def main():
    EditableCellsDemo()
    gtk.main()

if __name__ == '__main__':
    main()
