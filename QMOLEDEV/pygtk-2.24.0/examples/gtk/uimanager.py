#
# Small example of the new GtkUIManager
#
# Johan Dahlin <johan@gnome.org>, 2004
#

import pygtk
pygtk.require('2.0')

import gtk

ui_string = """<ui>
  <menubar name='Menubar'>
    <menu action='FileMenu'>
      <menuitem action='New'/>
      <menuitem action='Open'/>
      <separator/>
      <menuitem action='Close'/>
      <menuitem action='Quit'/>
    </menu>
    <menu action='HelpMenu'>
      <menuitem action='About'/>
    </menu>
  </menubar>
  <toolbar name='Toolbar'>
    <toolitem action='New'/>
    <toolitem action='Open'/>
    <separator/>
    <toolitem action='Quit'/>
  </toolbar>
</ui>"""

class Window(gtk.Window):
    def __init__(self):
        gtk.Window.__init__(self)
        self.set_position(gtk.WIN_POS_CENTER)
        self.set_title('GtkUIManager test app')
        self.connect('delete-event', self.delete_event_cb)
        self.set_size_request(400, 200)
        vbox = gtk.VBox()
        self.add(vbox)

        self.create_ui()
        vbox.pack_start(self.ui.get_widget('/Menubar'), expand=False)
        vbox.pack_start(self.ui.get_widget('/Toolbar'), expand=False)

        sw = gtk.ScrolledWindow()
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        vbox.pack_start(sw)

        textview = gtk.TextView()
        self.buffer = textview.get_buffer()
        sw.add(textview)

        status = gtk.Statusbar()
        vbox.pack_end(status, expand=False)

    def create_ui(self):
        ag = gtk.ActionGroup('WindowActions')
        actions = [
            ('FileMenu', None, '_File'),
            ('New',      gtk.STOCK_NEW, '_New', '<control>N',
             'Create a new file', self.file_new_cb),
            ('Open',     gtk.STOCK_OPEN, '_Open', '<control>O',
             'Open a file', self.file_open_cb),
            ('Close',    gtk.STOCK_CLOSE, '_Close', '<control>W',
             'Close the current window', self.file_close_cb),
            ('Quit',     gtk.STOCK_QUIT, '_Quit', '<control>Q',
             'Quit application', self.file_quit_cb),
            ('HelpMenu', None, '_Help'),
            ('About',    None, '_About', None, 'About application',
             self.help_about_cb),
            ]
        ag.add_actions(actions)
        self.ui = gtk.UIManager()
        self.ui.insert_action_group(ag, 0)
        self.ui.add_ui_from_string(ui_string)
        self.add_accel_group(self.ui.get_accel_group())

    def file_new_cb(self, action):
        w = Window()
        w.show_all()
        gtk.main()

    def file_open_cb(self, action):
        dialog = gtk.FileChooserDialog("Open..", self,
                                       gtk.FILE_CHOOSER_ACTION_OPEN,
                                       (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                        gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)

        filter = gtk.FileFilter()
        filter.set_name("All files")
        filter.add_pattern("*")
        dialog.add_filter(filter)

        dialog.hide()

        if dialog.run() == gtk.RESPONSE_OK:
            filename = dialog.get_filename()
            self.buffer.set_text(file(filename).read())

        dialog.destroy()

    def file_close_cb(self, action):
        self.hide()
        gtk.main_quit()

    def file_quit_cb(self, action):
        raise SystemExit

    def help_about_cb(self, action):
        dialog = gtk.MessageDialog(self,
                                   (gtk.DIALOG_MODAL |
                                    gtk.DIALOG_DESTROY_WITH_PARENT),
                                   gtk.MESSAGE_INFO, gtk.BUTTONS_OK,
                                   "Small example of the new GtkUIManger")
        dialog.run()
        dialog.destroy()

    def delete_event_cb(self, window, event):
        gtk.main_quit()

if __name__ == '__main__':
    w = Window()
    w.show_all()
    gtk.main()
