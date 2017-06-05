#! /usr/bin/env python

import sys
import weakref

import pygtk
pygtk.require('2.0')
import pango
import gtk


class SimpleListTreeModel(gtk.GenericTreeModel):

    def on_get_flags(self):
        return gtk.TREE_MODEL_LIST_ONLY

    def __init__(self, *values):
        gtk.GenericTreeModel.__init__(self)
        self.__values = list(values)

        self.props.leak_references = False

        # This is only needed to make the model gc-safe, since
        # leak_references is False.
        self.__iters = range(0, len(values))

    def on_get_n_columns(self):
        return 0

    def on_get_column_type(self, index):
        raise NotImplementedError

    def on_get_value(self, row, column):
        raise NotImplementedError

    def on_get_iter(self, path):
        return self.__iters[path[0]]

    def on_get_path(self, row):
        return(row,)

    def on_iter_parent(self, row):
        return None

    def on_iter_next(self, row):
        if row + 1 < len(self.__values):
            return self.__iters[row + 1]
        else:
            return None

    def on_iter_has_child(self, row):
        return False

    def on_iter_children(self, row):
        if row is None:
            return self.__iters[0]
        else:
            return None

    def on_iter_n_children(self, row):
        if row is None:
            return len(self.__values)
        else:
            return 0

    def on_iter_nth_child(self, parent, n):
        if parent:
            return None
        else:
            return self.__iters[n]


    def __len__ (self):
        return len(self.__values)

    def __getitem__ (self, key):
        return self.__values[self.get_user_data(key)]

    def __iter__ (self):
        return iter(self.__values)

    def __contains__ (self, value):
        return value in self.__values


    def insert(self, index_or_iter, new_value):
        if isinstance(index_or_iter, gtk.TreeIter):
            index_or_iter = self.get_user_data(index_or_iter)

        self.__values.insert(index_or_iter, new_value)
        self.__iters.append(len(self.__iters))

        titer = self.create_tree_iter(self.__iters[index_or_iter])
        self.row_inserted(self.get_path(titer), titer)


class Food(object):

    def __init__(self, name, description=None):
        self.name        = name
        self.description = description

    def set_to_text_renderer(column, cell, model, iter):
        value = model[iter]

        cell.props.text = value.name

        if value.description is None:
            cell.props.weight = pango.WEIGHT_BOLD
        else:
            cell.props.weight = pango.WEIGHT_NORMAL

    set_to_text_renderer = staticmethod(set_to_text_renderer)


def generate_values():
    yield Food('Fruits')
    yield Food('Apple', 'Round and red. Or green. Or yellow.')
    yield Food('Orange', 'Juicy')
    yield Food('Vegetables')
    yield Food('Tomato', 'Red and juicy.')
    yield Food('Cucumber', 'Long and hard.')

def insert_item(*ignored):
    model, titer = tree_view.get_selection().get_selected()

    if titer is None or model[titer].description is None:
        dialog = gtk.MessageDialog(type=gtk.MESSAGE_ERROR, buttons=gtk.BUTTONS_OK,
                                   message_format='Please select something. And not a header!')
        dialog.run()
        dialog.destroy()
    else:
        model.insert(titer, Food('Random name', 'Some random, but useful description.'))

def show_description(*ignored):
    model, titer = tree_view.get_selection().get_selected()

    if titer is None or model[titer].description is None:
        dialog = gtk.MessageDialog(type=gtk.MESSAGE_ERROR, buttons=gtk.BUTTONS_OK,
                                   message_format='Please select something. And not a header!')
    else:
        dialog = gtk.MessageDialog(buttons=gtk.BUTTONS_OK,
                                   message_format=model[titer].description)

    dialog.run()
    dialog.destroy()

window = gtk.Window()
tree_model = SimpleListTreeModel(*generate_values())
tree_view = gtk.TreeView(tree_model)
scrolled_window = gtk.ScrolledWindow()
vertical_box = gtk.VBox(False, 6)
button_box = gtk.HButtonBox()
insert = gtk.Button('Insert')
description = gtk.Button('Show Description')

scrolled_window.add(tree_view)
vertical_box.pack_start(scrolled_window)
vertical_box.pack_start(button_box, False, False)
button_box.pack_start(insert)
button_box.pack_start(description)
window.add(vertical_box)

tree_view.set_headers_visible(False)

text_renderer = gtk.CellRendererText()
column = gtk.TreeViewColumn(None, text_renderer)

column.set_cell_data_func(text_renderer, Food.set_to_text_renderer)
tree_view.append_column(column)

insert.connect('clicked', insert_item)
description.connect('clicked', show_description)
window.connect('destroy', lambda window: gtk.main_quit())

window.resize(400, 500)
window.show_all()

gtk.main()
