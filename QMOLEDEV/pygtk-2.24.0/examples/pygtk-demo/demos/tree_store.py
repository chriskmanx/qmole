#!/usr/bin/env python
'''Tree View/Tree Store

The GtkTreeStore is used to store data in tree form, to be used
later on by a GtkTreeView to display it. This demo builds a simple
GtkTreeStore and displays it. If you're new to the GtkTreeView widgets
and associates, look into the GtkListStore example first.'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import pygtk
pygtk.require('2.0')
import gobject
import gtk

#   columns
(
    HOLIDAY_NAME_COLUMN,
    ALEX_COLUMN,
    HAVOC_COLUMN,
    TIM_COLUMN,
    OWEN_COLUMN,
    DAVE_COLUMN,
    VISIBLE_COLUMN,
    WORLD_COLUMN,
    NUM_COLUMNS
) = range(9)

#   tree data
january = \
[
  ["New Years Day", True, True, True, True, False, True ],
  ["Presidential Inauguration", False, True, False, True, False, False ],
  ["Martin Luther King Jr. day", False, True, False, True, False, False ]
]

february = \
[
  [ "Presidents' Day", False, True, False, True, False, False ],
  [ "Groundhog Day", False, False, False, False, False, False ],
  [ "Valentine's Day", False, False, False, False, True, True ]
]

march = \
[
  [ "National Tree Planting Day", False, False, False, False, False, False ],
  [ "St Patrick's Day", False, False, False, False, False, True ]
]
april = \
[
  [ "April Fools' Day", False, False, False, False, False, True ],
  [ "Army Day", False, False, False, False, False, False ],
  [ "Earth Day", False, False, False, False, False, True ],
  [ "Administrative Professionals' Day", False, False, False, False, False, False ]
]

may = \
[
  [ "Nurses' Day", False, False, False, False, False, False ],
  [ "National Day of Prayer", False, False, False, False, False, False ],
  [ "Mothers' Day", False, False, False, False, False, True ],
  [ "Armed Forces Day", False, False, False, False, False, False ],
  [ "Memorial Day", True, True, True, True, False, True ]
]

june = \
[
  [ "June Fathers' Day", False, False, False, False, False, True ],
  [ "Juneteenth(Liberation of Slaves)", False, False, False, False, False, False ],
  [ "Flag Day", False, True, False, True, False, False ]
]

july = \
[
  [ "Parents' Day", False, False, False, False, False, True ],
  [ "Independence Day", False, True, False, True, False, False ]
]

august = \
[
  [ "Air Force Day", False, False, False, False, False, False ],
  [ "Coast Guard Day", False, False, False, False, False, False ],
  [ "Friendship Day", False, False, False, False, False, False ]
]

september = \
[
  [ "Grandparents' Day", False, False, False, False, False, True ],
  [ "Citizenship Day or Constitution Day", False, False, False, False, False, False ],
  [ "Labor Day", True, True, True, True, False, True ]
]

october = \
[
  [ "National Children's Day", False, False, False, False, False, False ],
  [ "Bosses' Day", False, False, False, False, False, False ],
  [ "Sweetest Day", False, False, False, False, False, False ],
  [ "Mother-in-Law's Day", False, False, False, False, False, False ],
  [ "Navy Day", False, False, False, False, False, False ],
  [ "Columbus Day", False, True, False, True, False, False ],
  [ "Halloween", False, False, False, False, False, True ]
]

november = \
[
  [ "Marine Corps Day", False, False, False, False, False, False ],
  [ "Veterans' Day", True, True, True, True, False, True ],
  [ "Thanksgiving", False, True, False, True, False, False ]
]

december = \
[
  [ "Pearl Harbor Remembrance Day", False, False, False, False, False, False ],
  [ "Christmas", True, True, True, True, False, True ],
  [ "Kwanzaa", False, False, False, False, False, False ]
]


toplevel = \
[
  ["January", False, False, False, False, False, False, january],
  ["February", False, False, False, False, False, False, february],
  ["March", False, False, False, False, False, False, march],
  ["April", False, False, False, False, False, False, april],
  ["May", False, False, False, False, False, False, may],
  ["June", False, False, False, False, False, False, june],
  ["July", False, False, False, False, False, False, july],
  ["August", False, False, False, False, False, False, august],
  ["September", False, False, False, False, False, False, september],
  ["October", False, False, False, False, False, False, october],
  ["November", False, False, False, False, False, False, november],
  ["December", False, False, False, False, False, False, december]
]

class TreeStoreDemo(gtk.Window):
    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.set_title(self.__class__.__name__)
        self.set_default_size(650, 400)
        self.set_border_width(8)

        vbox = gtk.VBox(False, 8)
        self.add(vbox)

        label = gtk.Label("Jonathan's Holiday Card Planning Sheet")
        vbox.pack_start(label, False, False)

        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        vbox.pack_start(sw)

        # create model
        model = self.__create_model()

        # create treeview
        treeview = gtk.TreeView(model)
        treeview.set_rules_hint(True)

        self.__add_columns(treeview)

        sw.add(treeview)

        # expand all rows after the treeview widget has been realized
        treeview.connect('realize', lambda tv: tv.expand_all())

        self.show_all()

    def __create_model(self):

        # create tree store
        model = gtk.TreeStore(
                    gobject.TYPE_STRING,
                    gobject.TYPE_BOOLEAN,
                    gobject.TYPE_BOOLEAN,
                    gobject.TYPE_BOOLEAN,
                    gobject.TYPE_BOOLEAN,
                    gobject.TYPE_BOOLEAN,
                    gobject.TYPE_BOOLEAN,
                    gobject.TYPE_BOOLEAN)

        # add data to the tree store
        for month in toplevel:
            iter = model.append(None)
            model.set(iter,
                HOLIDAY_NAME_COLUMN, month[HOLIDAY_NAME_COLUMN],
                ALEX_COLUMN, False,
                HAVOC_COLUMN, False,
                TIM_COLUMN, False,
                OWEN_COLUMN, False,
                DAVE_COLUMN, False,
                VISIBLE_COLUMN, False,
                WORLD_COLUMN, False
           )

            # add children
            for holiday in month[-1]:
                child_iter = model.append(iter);
                model.set(child_iter,
                    HOLIDAY_NAME_COLUMN, holiday[HOLIDAY_NAME_COLUMN],
                    ALEX_COLUMN, holiday[ALEX_COLUMN],
                    HAVOC_COLUMN, holiday[HAVOC_COLUMN],
                    TIM_COLUMN, holiday[TIM_COLUMN],
                    OWEN_COLUMN, holiday[OWEN_COLUMN],
                    DAVE_COLUMN, holiday[DAVE_COLUMN],
                    VISIBLE_COLUMN, True,
                    WORLD_COLUMN, holiday[WORLD_COLUMN-1]
               )

        return model

    def on_item_toggled(self, cell, path_str, model):

        # get selected column
        column = cell.get_data('column')

        # get toggled iter
        iter = model.get_iter_from_string(path_str)
        toggle_item = model.get_value(iter, column)

        # do something with the value
        toggle_item = not toggle_item

        # set new value
        model.set(iter, column, toggle_item)


    def __add_columns(self, treeview):
        model = treeview.get_model()

        # column for holiday names
        renderer = gtk.CellRendererText()
        renderer.set_property("xalign", 0.0)

        #col_offset = gtk.TreeViewColumn("Holiday", renderer, text=HOLIDAY_NAME_COLUMN)
        column = gtk.TreeViewColumn("Holiday", renderer, text=HOLIDAY_NAME_COLUMN)
        #column = gtk_tree_view_get_column(GTK_TREE_VIEW(treeview), col_offset - 1);
        column.set_clickable(True)

        treeview.append_column(column)

        # alex column */
        renderer = gtk.CellRendererToggle()
        renderer.set_property("xalign", 0.0)
        renderer.set_data("column", ALEX_COLUMN)

        renderer.connect("toggled", self.on_item_toggled, model)

        column = gtk.TreeViewColumn("Alex", renderer, active=ALEX_COLUMN,
                                    visible=VISIBLE_COLUMN, activatable=WORLD_COLUMN)

        # set this column to a fixed sizing(of 50 pixels)
        #column = gtk_tree_view_get_column(GTK_TREE_VIEW(treeview), col_offset - 1);
        column.set_sizing(gtk.TREE_VIEW_COLUMN_FIXED)
        column.set_fixed_width(50)
        column.set_clickable(True)

        treeview.append_column(column)

        # havoc column
        renderer = gtk.CellRendererToggle();
        renderer.set_property("xalign", 0.0)
        renderer.set_data("column", HAVOC_COLUMN)

        renderer.connect("toggled", self.on_item_toggled, model)

        column = gtk.TreeViewColumn("Havoc", renderer, active=HAVOC_COLUMN,
                                    visible=VISIBLE_COLUMN)

        #column = treeview.get_column(col_offset - 1)
        column.set_sizing(gtk.TREE_VIEW_COLUMN_FIXED)
        column.set_fixed_width(50)
        column.set_clickable(True)

        treeview.append_column(column)

        # tim column
        renderer = gtk.CellRendererToggle();
        renderer.set_property("xalign", 0.0)
        renderer.set_data("column", TIM_COLUMN)

        renderer.connect("toggled", self.on_item_toggled, model)

        column = gtk.TreeViewColumn("Tim", renderer, active=TIM_COLUMN,
                                    visible=VISIBLE_COLUMN, activatable=WORLD_COLUMN)

        #column = treeview.get_column(col_offset - 1)
        column.set_sizing(gtk.TREE_VIEW_COLUMN_FIXED)
        column.set_fixed_width(50)
        column.set_clickable(True)

        treeview.append_column(column)

        # owen column
        renderer = gtk.CellRendererToggle();
        renderer.set_property("xalign", 0.0)
        renderer.set_data("column", OWEN_COLUMN)

        renderer.connect("toggled", self.on_item_toggled, model)

        column = gtk.TreeViewColumn("Owen", renderer, active=OWEN_COLUMN,
                                    visible=VISIBLE_COLUMN)

        #column = treeview.get_column(col_offset - 1)
        column.set_sizing(gtk.TREE_VIEW_COLUMN_FIXED)
        column.set_fixed_width(50)
        column.set_clickable(True)

        treeview.append_column(column)

        # dave column
        renderer = gtk.CellRendererToggle();
        renderer.set_property("xalign", 0.0)
        renderer.set_data("column", DAVE_COLUMN)

        renderer.connect("toggled", self.on_item_toggled, model)

        column = gtk.TreeViewColumn("Dave", renderer, active=DAVE_COLUMN,
                                    visible=VISIBLE_COLUMN)

        #column = treeview.get_column(col_offset - 1)
        column.set_sizing(gtk.TREE_VIEW_COLUMN_FIXED)
        column.set_fixed_width(50)
        column.set_clickable(True)

        treeview.append_column(column)

def main():
    TreeStoreDemo()
    gtk.main()

if __name__ == '__main__':
    main()
