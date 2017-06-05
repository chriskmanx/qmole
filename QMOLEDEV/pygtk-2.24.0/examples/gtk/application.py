# By Jarek Zgoda and Johan Dahlin

import pygtk
pygtk.require('2.0')
import gtk

ui_string = """<ui>
  <menubar name='Menubar'>
    <menu action='FileMenu'>
      <menuitem action='New'/>
      <menuitem action='Open'/>
      <menuitem action='Save'/>
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
    <toolitem action='Save'/>
    <separator/>
    <toolitem action='Quit'/>
  </toolbar>
</ui>"""


class Application(gtk.Window):
    def __init__(self, title=''):
        gtk.Window.__init__(self)
        self.connect('delete-event', self._on_delete_event)
        self.set_position(gtk.WIN_POS_CENTER)
        self.set_size_request(400, 200)
        self.set_title(title)

        main_vbox = gtk.VBox()
        self.add(main_vbox)
        main_vbox.show()

        uimgr = self._create_ui()
        uimgr.connect('connect-proxy',
                      self._on_uimanager__connect_proxy)
        uimgr.connect('disconnect-proxy',
                      self._on_uimanager__disconnect_proxy)

        menubar = uimgr.get_widget('/Menubar')
        main_vbox.pack_start(menubar, expand=False)
        menubar.show()

        toolbar = uimgr.get_widget('/Toolbar')
        main_vbox.pack_start(toolbar, expand=False)
        toolbar.realize()
        toolbar.show()

        status = gtk.Statusbar()
        main_vbox.pack_end(status, expand=False)
        status.show()
        self.statusbar = status

        self._menu_cix = -1

    def _create_ui(self):
        ag = gtk.ActionGroup('AppActions')
        actions = [
            ('FileMenu', None, '_File'),
            ('New',      gtk.STOCK_NEW, '_New', '<control>N',
             'Create a new file', self._on_action_new),
            ('Open',     gtk.STOCK_OPEN, '_Open', '<control>O',
             'Open a file', self._on_action_open),
            ('Save',     gtk.STOCK_SAVE, '_Save', '<control>S',
             'Save a file', self._on_action_save),
            ('Close',    gtk.STOCK_CLOSE, '_Close', '<control>W',
             'Close the current window', self._on_action_close),
            ('Quit',     gtk.STOCK_QUIT, '_Quit', '<control>Q',
             'Quit application', self._on_action_quit),
            ('HelpMenu', None, '_Help'),
            ('About',    None, '_About', None, 'About application',
             self._on_action_about),
            ]
        ag.add_actions(actions)
        ui = gtk.UIManager()
        ui.insert_action_group(ag, 0)
        ui.add_ui_from_string(ui_string)
        self.add_accel_group(ui.get_accel_group())
        return ui

    def _on_uimanager__connect_proxy(self, uimgr, action, widget):
        tooltip = action.get_property('tooltip')
        if not tooltip:
            return

        if isinstance(widget, gtk.MenuItem):
            cid = widget.connect('select', self._on_menu_item__select,
                                 tooltip)
            cid2 = widget.connect('deselect', self._on_menu_item__deselect)
            widget.set_data('pygtk-app::proxy-signal-ids', (cid, cid2))
        elif isinstance(widget, gtk.ToolButton):
            cid = widget.child.connect('enter', self._on_tool_button__enter,
                                       tooltip)
            cid2 = widget.child.connect('leave', self._on_tool_button__leave)
            widget.set_data('pygtk-app::proxy-signal-ids', (cid, cid2))

    def _on_uimanager__disconnect_proxy(self, uimgr, action, widget):
        cids = widget.get_data('pygtk-app::proxy-signal-ids')
        if not cids:
            return

        if isinstance(widget, gtk.ToolButton):
            widget = widget.child

        for name, cid in cids:
            widget.disconnect(cid)

    def _on_menu_item__select(self, menuitem, tooltip):
        self.statusbar.push(self._menu_cix, tooltip)

    def _on_menu_item__deselect(self, menuitem):
        self.statusbar.pop(self._menu_cix)

    def _on_tool_button__enter(self, toolbutton, tooltip):
        self.statusbar.push(self._menu_cix, tooltip)

    def _on_tool_button__leave(self, toolbutton):
        self.statusbar.pop(self._menu_cix)

    def _on_action_new(self, action):
        self.new()

    def _on_action_open(self, action):
        self.open()

    def _on_action_save(self, action):
        self.save()

    def _on_action_close(self, action):
        self.close()

    def _on_action_quit(self, action):
        self.quit()

    def _on_action_about(self, action):
        self.about()

    def _on_delete_event(self, window, event):
        self.quit()

    # Override in subclass

    def new(self):
        raise NotImplementedError("Open")

    def open(self):
        raise NotImplementedError("Open")

    def save(self):
        raise NotImplementedError("Save")

    def close(self):
        raise NotImplementedError("Close")

    def about(self):
        raise NotImplementedError("About")

    def run(self):
        self.show()
        gtk.main()

    def quit(self):
        gtk.main_quit()

if __name__ == '__main__':
    a = Application(title="TestApp")
    a.run()

