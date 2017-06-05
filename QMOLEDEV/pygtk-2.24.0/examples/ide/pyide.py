#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import gtkcons, gtkdb, gtkprof, edit, dialogs
import os, sys, string

# select a good VT emulator
for vt in 'Eterm', 'nxterm', 'xterm-color', 'xterm', 'rxvt':
    for dirname in string.split(os.environ['PATH'], os.pathsep):
        fullname = os.path.join(dirname, vt)
        if os.path.exists(fullname):
            VT_CMD = fullname + ' -geometry 80x6 -e '
            break
    else:
        continue
    break
else:
    VT_CMD=''  # this is not ideal

ui_string = """<ui>
<menubar>
  <menu action='FileMenu'>
    <menuitem action='FileNew'/>
    <menuitem action='FileOpen'/>
    <separator/>
    <menuitem action='FileExit'/>
  </menu>
  <menu action='EditMenu'>
    <menuitem action='EditCopy'/>
    <menuitem action='EditPaste'/>
    <menuitem action='EditClear'/>
  </menu>
  <placeholder name='OtherMenus'/>
  <menu action='HelpMenu' position='bot'>
    <menuitem action='HelpAbout'/>
  </menu>
</menubar>
</ui>
"""
pythonmenu_uistring = """<ui>
<menubar>
 <placeholder name='OtherMenus'>
  <menu name='PythonMenu' action='PythonMenu'>
    <menuitem action='PythonReload'/>
    <menuitem action='PythonRun'/>
    <menuitem action='PythonDebug'/>
    <menuitem action='PythonProfile'/>
  </menu>
 </placeholder>
</menubar>
</ui>
"""

class Application(gtk.Window):
    def __init__(self):
        gtk.Window.__init__(self, gtk.WINDOW_TOPLEVEL)
        self.connect("destroy", self.quit)
        self.connect("delete_event", self.quit)
        self.set_title("Python")
        self.set_size_request(475, 325)
        self.main_box = gtk.VBox()
        self.add(self.main_box)
        self.main_box.show()
        hdlbox = gtk.HandleBox()
        self.main_box.pack_start(hdlbox, expand=False)
        hdlbox.show()
        actions = [
            ('FileMenu', None, '_File'),
            ('FileNew', gtk.STOCK_NEW, None, None, None, self.file_new),
            ('FileOpen', gtk.STOCK_OPEN, None, None, None, self.file_open),
            ('FileExit', gtk.STOCK_QUIT, None, None, None, self.file_exit),
            ('EditMenu', None, '_Edit'),
            ('EditCopy', gtk.STOCK_COPY, None, None, None, self.edit_copy),
            ('EditPaste', gtk.STOCK_PASTE, None, None, None, self.edit_paste),
            ('EditClear', gtk.STOCK_REMOVE, 'C_lear', None,  None,
             self.edit_clear),
            ('HelpMenu', gtk.STOCK_HELP),
            ('HelpAbout', None, 'A_bout', None, None, self.help_about),
            ]
        python_actions = [
            ('PythonMenu', None, '_Python'),
            ('PythonReload', None, '_Reload Module...', None, None,
             self.python_reload),
            ('PythonRun', None, 'R_un...', None, None, self.python_run),
            ('PythonDebug', None, '_Debug...', None, None, self.python_debug),
            ('PythonProfile', None, 'Pro_file...', None, None,
             self.python_prof),
            ]
        self.ag = gtk.ActionGroup('ide')
        self.ag.add_actions(actions)
        self.ag.add_actions(python_actions)
        self.ui = gtk.UIManager()
        self.ui.insert_action_group(self.ag, 0)
        self.ui.add_ui_from_string(ui_string)
        self.ui.add_ui_from_string(pythonmenu_uistring)
        self.add_accel_group(self.ui.get_accel_group())
        hdlbox.add(self.ui.get_widget('/menubar'))
        #self.ui.get_widget('/menubar').show()
        self.interp = gtkcons.Console(
            namespace={'__builtins__': __builtins__,
                       '__name__': '__main__',
                       '__doc__': None}, quit_cb=self.quit)
        self.main_box.pack_start(self.interp)
        self.interp.show()
        self.interp.init()
        self.editwins = []
        return

    def quit(self, *args):
        for win in self.editwins:
            if win.chk_save(): return
            win.hide()
            win.destroy()
        gtk.main_quit()
        return

    def reload_file(self, fname):
        if not os.path.isfile(fname):
            gtk.MessageDialog(self, gtk.DIALOG_DESTROY_WITH_PARENT,
                              gtk.MESSAGE_ERROR, gtk.BUTTONS_OK,
                              fname + " was not found.")
            return
        dir = os.path.dirname(fname)
        base = os.path.basename(fname)
        if dir not in sys.path: sys.path.insert(0, dir)
        if   string.lower(base[-3:]) == '.py':  base = base[:-3]
        elif string.lower(base[-4:]) == '.pyc': base = base[:-4]
        if not sys.modules.has_key(base):
            self.interp.run('import ' + base)
        else:
            self.interp.run('import ' + base)
            self.interp.run('reload(' + base + ')')
        return

    # execute a python script normally or with the debugger or profiler
    def run_script(self, fname):
        if not fname or not os.path.exists(fname):
            dlg = gtk.MessageDialog(self, gtk.DIALOG_DESTROY_WITH_PARENT,
                                    gtk.MESSAGE_ERROR, gtk.BUTTONS_OK,
                                    "Invalid filename "+fname)
            dlg.run()
            return
        args = dialogs.InputBox("Arguments",
                                "Enter any command line arguments", self)
        if args == None: return
        os.system(VT_CMD+'python "'+fname+'" ' + args + ' &')
        return
    def debug_script(self, fname):
        if not fname or not os.path.exists(fname):
            dlg = gtk.MessageDialog(self, gtk.DIALOG_DESTROY_WITH_PARENT,
                                    gtk.MESSAGE_ERROR, gtk.BUTTONS_OK,
                                    "Invalid filename "+fname)
            dlg.run()
            return
        args = dialogs.InputBox("Arguments",
                                "Enter any command line arguments", self)
        if args == None: return
        os.system(VT_CMD+'python '+gtkdb.__file__+' "'+fname+'" ' +
                  args + ' &')
        return
    def profile_script(self, fname):
        if not fname or not os.path.exists(fname):
            dlg = gtk.MessageDialog(self, gtk.DIALOG_DESTROY_WITH_PARENT,
                                    gtk.MESSAGE_ERROR, gtk.BUTTONS_OK,
                                    "Invalid filename "+fname)
            dlg.run()
            return
        args = dialogs.InputBox("Arguments",
                                "Enter any command line arguments", self)
        if args == None: return
        os.system(VT_CMD+'python '+gtkprof.__file__+' "'+fname+'" ' +
                  args + ' &')
        return

    def add_py_menu(self, ew):
        python_actions = [
            ('PythonMenu', None, '_Python'),
            ('PythonReload', None, '_Reload Module'),
            ('PythonRun', None, 'R_un...', None, None,
             lambda w, ew=ew: self.run_script(ew.fname)),
            ('PythonDebug', None, '_Debug...', None, None,
             lambda w, ew=ew: self.debug_script(ew.fname)),
            ('PythonProfile', None, 'Pro_file...', None, None,
             lambda w, ew=ew: self.profile_script(ew.fname)),
            ]
        ew.ag.add_actions(python_actions)
        ew.ui.add_ui_from_string(pythonmenu_uistring)
        return

    def file_new(self, mi=None):
        ew = edit.EditWindow(quit_cb=self.rem_editwin)
        self.editwins.append(ew)
        self.add_py_menu(ew)
        ew.show()
        ew.set_size_request(0,0)
        return
    def file_open(self, mi=None):
        fname = dialogs.OpenFile('Open', self)
        if fname:
            ew = edit.EditWindow(quit_cb=self.rem_editwin)
            ew.load_file(fname)
            self.editwins.append(ew)
            self.add_py_menu(ew)
            ew.show()
            ew.set_size_request(0,0)
        return
    def rem_editwin(self, win=None, event=None):
        for i in range(len(self.editwins)):
            if self.editwins[i] == win:
                del self.editwins[i]
                break
        return
    def file_exit(self, mi=None):
        self.quit()
        return
    def edit_copy(self, mi=None):
        self.interp.text.copy_clipboard(0)
        return
    def edit_paste(self, mi=None):
        self.interp.line.paste_clipboard(0)
        return
    def edit_clear(self, mi=None):
        self.interp.line.delete_selection()
        return
    def python_reload(self, mi=None):
        print "python_reload"
        return
    def python_run(self, mi=None):
        fname = dialogs.OpenFile("Run", self)
        if fname:
            self.run_script(fname)
        return
    def python_debug(self, mi=None):
        fname = dialogs.OpenFile("Debug", self)
        if fname:
            self.debug_script(fname)
        return
    def python_prof(self, mi=None):
        fname = dialogs.OpenFile("Profile", self)
        if fname:
            self.profile_script(fname)
        return
    def help_about(self, mi=None):
        dlg = gtk.MessageDialog(self, gtk.DIALOG_DESTROY_WITH_PARENT,
                                gtk.MESSAGE_INFO, gtk.BUTTONS_OK,
                                "Copyright (C)\n" \
                                "1998 James Henstridge\n" \
                                "2004 John Finlay\n" \
                                "This program is covered by the GPL>=2")
        dlg.run()
        dlg.hide()
        return

if __name__ == '__main__':
    app = Application()
    app.show()
    app.set_size_request(0,0)
    gtk.main()
