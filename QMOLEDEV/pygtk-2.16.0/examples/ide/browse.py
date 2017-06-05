#!/usr/bin/env python

# This is an example of using dynamic trees (trees where nodes are only
# calculated as they are needed) with pygtk.  This particular example
# allows for the browsing of the variables in a module, allowing the
# descent into classes and modules and other objects with a __dict__
# attribute.

# If this file is run straight, it will let you browse the gtk module.

import pygtk
pygtk.require('2.0')
import gtk

class BrowseVariables(gtk.VBox):
    def __init__(self, name, obj):
        gtk.VBox.__init__(self)
        self.set_spacing(2)
        #
        self.sw = gtk.ScrolledWindow()
        self.sw.set_size_request(300, 200)
        self.sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.pack_start(self.sw)
        self.sw.show()
        #
        self.disp = gtk.Entry()
        self.disp.set_editable(False)
        self.pack_start(self.disp, expand=False)
        self.disp.show()
        #
        self.treestore = gtk.TreeStore(str, object)
        self.tree = gtk.TreeView(self.treestore)
        treeviewcolumn = gtk.TreeViewColumn('Variable',
                                            gtk.CellRendererText(),
                                            text=0)
        self.tree.append_column(treeviewcolumn)
        self.sw.add(self.tree)
        self.tree.show()
        #
        riter = self.treestore.append(None, [name, obj])
        self.treestore.append(riter, ['', None])
        self.tree.connect('test-expand-row', self.expand_row_cb)
        self.tree.connect('test-collapse-row', self.collapse_row_cb)
        self.treeselection = self.tree.get_selection()
        self.treeselection.connect('changed', self.change_selection_cb)
        return
    def change_selection_cb(self, treeselection):
        model, iter = treeselection.get_selected()
        if not iter or not self.disp:
            return
        key = model[iter][0]
        if key == '__builtins__':
            value = key
        else:
            value = model[iter][1]
        self.disp.set_text(str(value))
        return
    def expand_row_cb(self, treeview, riter, path):
        model = treeview.get_model()
        dict = vars(model[riter][1])
        if not dict:
            return True
        citer = model.iter_children(riter)
        model.remove(citer)
        keylist = dict.keys()
        keylist.sort()
        for key in keylist:
            obj = dict[key]
            i = model.append(riter, [key, obj])
            try:
                d = vars(obj)
                if d:
                    model.append(i, ['', d])
            except TypeError:
                pass
        return False
    def collapse_row_cb(self, treeview, riter, path):
        model = treeview.get_model()
        citer = model.iter_children(riter)
        if citer:
            while model.remove(citer):
                pass
        model.append(riter, ['', None])
        return True

class BrowseWindow(gtk.Window):
    def __init__(self, name, dict):
        gtk.Window.__init__(self)
        self.set_title("Browse Window")
        box = gtk.VBox()
        self.add(box)
        box.show()
        browse = BrowseVariables(name, dict)
        browse.set_border_width(10)
        box.pack_start(browse)
        browse.show()
        separator = gtk.HSeparator()
        box.pack_start(separator, expand=False)
        separator.show()
        box2 = gtk.VBox(spacing=10)
        box2.set_border_width(10)
        box.pack_start(box2, expand=False)
        box2.show()
        button = gtk.Button(stock=gtk.STOCK_CLOSE)
        box2.pack_start(button)
        button.set_flags(gtk.CAN_DEFAULT)
        button.grab_default()
        button.show()
        self.close_button = button
        return

if __name__ == '__main__':
    win = BrowseWindow('gtk', gtk)
    win.set_title("Browse gtk")
    win.connect("destroy", lambda w: gtk.main_quit())
    win.connect("delete_event", lambda w,e: gtk.main_quit())
    win.close_button.connect("clicked", lambda w: gtk.main_quit())
    win.show()
    gtk.main()
