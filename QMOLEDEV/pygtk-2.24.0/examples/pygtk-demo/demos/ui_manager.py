#!/usr/bin/env python
'''UI Manager

The GtkUIManager object allows the easy creation of menus
from an array of actions and a description of the menu hierarchy.
'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import pygtk
pygtk.require('2.0')
import gobject
import gtk

def activate_action(action):
    print 'Action "%s" activated' % action.get_name()

def activate_radio_action(action, current):
    print 'Radio action "%s" selected'% current.get_name()

entries = (
  ( "FileMenu", None, "_File" ),               # name, stock id, label
  ( "PreferencesMenu", None, "_Preferences" ), # name, stock id, label
  ( "ColorMenu", None, "_Color"  ),            # name, stock id, label
  ( "ShapeMenu", None, "_Shape" ),             # name, stock id, label
  ( "HelpMenu", None, "_Help" ),               # name, stock id, label
  ( "New", gtk.STOCK_NEW,                      # name, stock id
    "_New", "<control>N",                      # label, accelerator
    "Create a new file",                       # tooltip
    activate_action ),
  ( "Open", gtk.STOCK_OPEN,                    # name, stock id
    "_Open","<control>O",                      # label, accelerator
    "Open a file",                             # tooltip
    activate_action ),
  ( "Save", gtk.STOCK_SAVE,                    # name, stock id
    "_Save","<control>S",                      # label, accelerator
    "Save current file",                       # tooltip
    activate_action ),
  ( "SaveAs", gtk.STOCK_SAVE,                  # name, stock id
    "Save _As...", None,                       # label, accelerator
    "Save to a file",                          # tooltip
    activate_action ),
  ( "Quit", gtk.STOCK_QUIT,                    # name, stock id
    "_Quit", "<control>Q",                     # label, accelerator
    "Quit",                                    # tooltip
    activate_action ),
  ( "About", None,                             # name, stock id
    "_About", "<control>A",                    # label, accelerator
    "About",                                   # tooltip
    activate_action ),
  ( "Logo", "demo-gtk-logo",                   # name, stock id
     None, None,                               # label, accelerator
    "GTK+",                                    # tooltip
    activate_action ),
)

toggle_entries = (
  ( "Bold", gtk.STOCK_BOLD,                    # name, stock id
     "_Bold", "<control>B",                    # label, accelerator
    "Bold",                                    # tooltip
    activate_action,
    True ),                                    # is_active
)

(
  COLOR_RED,
  COLOR_GREEN,
  COLOR_BLUE
) = range(3)

color_entries = (
  ( "Red", None,                               # name, stock id
    "_Red", "<control>R",                      # label, accelerator
    "Blood", COLOR_RED ),                      # tooltip, value
  ( "Green", None,                             # name, stock id
    "_Green", "<control>G",                    # label, accelerator
    "Grass", COLOR_GREEN ),                    # tooltip, value
  ( "Blue", None,                              # name, stock id
    "_Blue", "<control>B",                     # label, accelerator
    "Sky", COLOR_BLUE ),                       # tooltip, value
)

(
  SHAPE_SQUARE,
  SHAPE_RECTANGLE,
  SHAPE_OVAL,
) = range(3)

# GtkRadioActionEntry
shape_entries = (
  ( "Square", None,                            # name, stock id
    "_Square", "<control>S",                   # label, accelerator
    "Square",  SHAPE_SQUARE ),                 # tooltip, value
  ( "Rectangle", None,                         # name, stock id
    "_Rectangle", "<control>R",                # label, accelerator
    "Rectangle", SHAPE_RECTANGLE ),            # tooltip, value
  ( "Oval", None,                              # name, stock id
    "_Oval", "<control>O",                     # label, accelerator
    "Egg", SHAPE_OVAL ),                       # tooltip, value
)

ui_info = \
'''<ui>
  <menubar name='MenuBar'>
    <menu action='FileMenu'>
      <menuitem action='New'/>
      <menuitem action='Open'/>
      <menuitem action='Save'/>
      <menuitem action='SaveAs'/>
      <separator/>
      <menuitem action='Quit'/>
    </menu>
    <menu action='PreferencesMenu'>
      <menu action='ColorMenu'>
        <menuitem action='Red'/>
        <menuitem action='Green'/>
        <menuitem action='Blue'/>
      </menu>
      <menu action='ShapeMenu'>
        <menuitem action='Square'/>
        <menuitem action='Rectangle'/>
        <menuitem action='Oval'/>
      </menu>
      <menuitem action='Bold'/>
    </menu>
    <menu action='HelpMenu'>
      <menuitem action='About'/>
    </menu>
  </menubar>
  <toolbar  name='ToolBar'>
    <toolitem action='Open'/>
    <toolitem action='Quit'/>
    <separator action='Sep1'/>
    <toolitem action='Logo'/>
  </toolbar>
</ui>'''

class UIManagerDemo(gtk.Window):

    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.set_title(self.__class__.__name__)
        self.set_border_width(0)

        actions = gtk.ActionGroup("Actions")
        actions.add_actions(entries)
        actions.add_toggle_actions(toggle_entries)
        actions.add_radio_actions(color_entries, COLOR_RED, activate_radio_action)
        actions.add_radio_actions(shape_entries, SHAPE_OVAL, activate_radio_action)

        ui = gtk.UIManager()
        ui.insert_action_group(actions, 0)
        self.add_accel_group(ui.get_accel_group())

        try:
            mergeid = ui.add_ui_from_string(ui_info)
        except gobject.GError, msg:
            print "building menus failed: %s" % msg

        box1 = gtk.VBox(False, 0)
        self.add(box1)

        box1.pack_start(ui.get_widget("/MenuBar"), False, False, 0)

        label = gtk.Label("Type\n<alt>\nto start")
        label.set_size_request(200, 200)
        label.set_alignment(0.5, 0.5)
        box1.pack_start(label, True, True, 0)

        separator = gtk.HSeparator()
        box1.pack_start(separator, False, True, 0)

        box2 = gtk.VBox(False, 10)
        box2.set_border_width(10)
        box1.pack_start(box2, False, True, 0)

        button = gtk.Button("close")
        button.connect("clicked", lambda b, w=self: w.destroy())
        box2.pack_start(button, True, True, 0)
        button.set_flags(gtk.CAN_DEFAULT)
        button.grab_default()

        self.show_all()

def main():
    UIManagerDemo()
    gtk.main()

if __name__ == '__main__':
    main()
