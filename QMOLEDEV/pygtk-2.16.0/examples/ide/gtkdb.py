#!/usr/bin/env python

import sys
import bdb
import repr
import string
import linecache # for linecache.getlines(filename)
import pygtk
pygtk.require('2.0')
import gtk
import dialogs

class PyGTKDb(gtk.Window, bdb.Bdb):
    ui_string = """<ui>
    <toolbar name='Toolbar'>
      <toolitem action='Next'/>
      <toolitem action='Step'/>
      <separator/>
      <toolitem action='Return'/>
      <separator/>
      <toolitem action='Continue'/>
      <toolitem action='Break'/>
      <separator/>
      <toolitem action='Edit'/>
      <toolitem action='Run'/>
      <separator/>
      <toolitem action='Quit'/>
    </toolbar>
    </ui>"""
    def __init__(self):
        gtk.Window.__init__(self)
        bdb.Bdb.__init__(self)
        self.realize()

        self.set_title("PyGTKDb")
        self.connect("destroy", self.do_quit)
        self.connect("delete_event", self.do_quit)

        self.box = gtk.VBox()
        self.add(self.box)
        self.box.show()

        self.add_stock_ids()

        actions = [
            ('Next', 'pyide-next', None, None, "Next statement", self.do_next),
            ('Step', 'pyide-step', None, None, "Step into function",
             self.do_step),
            ('Return', 'pyide-return', None, None,
             "Continue execution to end of function", self.do_return),
            ('Continue', 'pyide-continue', None, None,
             "Continue execution to next break point", self.do_continue),
            ('Break', 'pyide-break', None, None,
             "Toggle break point at selected line", self.do_break),
            ('Edit', 'pyide-edit', None, None,
             "Edit the value of the selected variable", self.do_edit),
            ('Run', 'pyide-run', None, None,
             "Execute some code in the current stack context", self.do_run),
            ('Quit', 'pyide-quit', None, None, "Quit the debugger",
             self.do_quit),
            ]

        self.ag = gtk.ActionGroup('PyIDE Actions')
        self.ag.add_actions(actions)
        self.ui = gtk.UIManager()
        self.ui.insert_action_group(self.ag, 0)
        self.ui.add_ui_from_string(self.ui_string)
        self.add_accel_group(self.ui.get_accel_group())

        self.box.pack_start(self.ui.get_widget('/Toolbar'), expand=False)
        sep = gtk.HSeparator()
        self.box.pack_start(sep, expand=False)
        sep.show()

        vpane = gtk.VPaned()
        self.box.pack_start(vpane)
        vpane.show()

        hpane = gtk.HPaned()
        vpane.add1(hpane)
        hpane.show()

        swin = gtk.ScrolledWindow()
        swin.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        hpane.add1(swin)
        swin.show()

        ls = gtk.ListStore(str)
        self.stackdisp = gtk.TreeView(ls)
        tvc = gtk.TreeViewColumn('Stack Frame', gtk.CellRendererText(),
                                 text=0)
        self.stackdisp.append_column(tvc)
        self.stackdisp.set_size_request(280, 125)
        selection = self.stackdisp.get_selection()
        selection.set_mode(gtk.SELECTION_BROWSE)
        selection.connect("changed", self.update_curstack)

        self.stackdisp.set_border_width(2)
        swin.add(self.stackdisp)
        self.stackdisp.show()

        swin = gtk.ScrolledWindow()
        swin.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        hpane.add2(swin)
        swin.show()

        ls = gtk.ListStore(str, str, str)
        self.vardisp = gtk.TreeView(ls)
        titles = ['local var', 'type', 'value']
        for n in range(len(titles)):
            tvc = gtk.TreeViewColumn(titles[n], gtk.CellRendererText(),
                                     text=n)
            self.vardisp.append_column(tvc)

        selection = self.vardisp.get_selection()
        selection.set_mode(gtk.SELECTION_BROWSE)
        selection.connect("changed", self.update_selectedvar)
        self.vardisp.set_border_width(2)

        self.vardisp.set_border_width(2)
        swin.add(self.vardisp)
        self.vardisp.show()
        self.vardisp.selected = 0
        self.vardisp.varnames = []

        swin = gtk.ScrolledWindow()
        swin.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        vpane.add2(swin)
        swin.show()

        self.minibreak = gtk.Image()
        self.minibreak.set_from_file("minibreak.xpm")
        self.minibreak.show()
        ls = gtk.ListStore(gtk.gdk.Pixbuf, str, str)
        self.filedisp = gtk.TreeView(ls)
        titles = ['break', 'lineno', 'line']
        cell = gtk.CellRendererPixbuf()
        tvc = gtk.TreeViewColumn(None, cell, pixbuf=0)
        tvc.set_min_width(14)
        tvc.set_widget(self.minibreak)
        self.filedisp.append_column(tvc)
        cell = gtk.CellRendererText()
        cell.set_property('xalign', 1.0)
        tvc = gtk.TreeViewColumn(titles[1], cell, text=1)
        self.filedisp.append_column(tvc)
        cell = gtk.CellRendererText()
        tvc = gtk.TreeViewColumn(titles[2], cell, text=2)
        self.filedisp.append_column(tvc)
        self.minibreak = self.minibreak.get_pixbuf()
        selection = self.filedisp.get_selection()
        selection.set_mode(gtk.SELECTION_BROWSE)
        selection.connect("changed", self.update_selection)
        self.filedisp.connect('row-activated', lambda t,p,c: self.do_break())
        self.filedisp.set_border_width(2)
        self.filedisp.set_size_request(600, 200)
        swin.add(self.filedisp)
        self.filedisp.show()

        separator = gtk.HSeparator()
        self.box.pack_start(separator, expand=False)
        separator.show()

        align = gtk.Alignment(0.0, 0.5, 0.0, 0.0)
        self.box.pack_start(align, expand=False)
        align.show()
        self.status = gtk.Label()
        self.status.set_padding(4, 1)
        align.add(self.status)
        self.status.show()

        self.filename = None
        self.selected = 0
        self.blockupdate = 0
        return

    def add_stock_ids(self):
        ids = [
            ('pyide-next', '_Next', gtk.gdk.CONTROL_MASK, gtk.keysyms.N,
             'pyide'),
            ('pyide-step', '_Step', gtk.gdk.CONTROL_MASK, gtk.keysyms.S,
             'pyide'),
            ('pyide-return', '_Return', gtk.gdk.CONTROL_MASK, gtk.keysyms.R,
             'pyide'),
            ('pyide-continue', '_Continue', gtk.gdk.CONTROL_MASK,
             gtk.keysyms.C, 'pyide'),
            ('pyide-break', '_Break', gtk.gdk.CONTROL_MASK, gtk.keysyms.B,
             'pyide'),
            ('pyide-edit', '_Edit', gtk.gdk.CONTROL_MASK, gtk.keysyms.E,
             'pyide'),
            ('pyide-run', 'R_un', gtk.gdk.CONTROL_MASK, gtk.keysyms.U,
             'pyide'),
            ('pyide-quit', '_Quit', gtk.gdk.CONTROL_MASK, gtk.keysyms.Q,
             'pyide'),
            ]
        gtk.stock_add(ids)
        names = ['next', 'step', 'return', 'continue', 'break',
                 'edit', 'run', 'quit']
        self.iconfactory = gtk.IconFactory()
        for name in names:
            iconset = gtk.IconSet(gtk.gdk.pixbuf_new_from_file(name+'.xpm'))
            self.iconfactory.add('pyide-'+name, iconset)
        self.iconfactory.add_default()
        return

    def set_status(self, str):
        self.status.set_text(str)
        return
    def update_selection(self, sel):
        if self.blockupdate: return
        model, iter = sel.get_selected()
        r = model.get_path(iter)[0]
        self.selected = r + 1
        return
    def update_curstack(self, sel):
        if self.blockupdate: return
        model, iter = sel.get_selected()
        r = model.get_path(iter)[0]
        self.curindex = r
        self.curframe = self.stack[self.curindex][0]
        self.lineno = None
        self.update_code_listing()
        self.update_var_listing()
        return
    def update_selectedvar(self, sel):
        model, iter = sel.get_selected()
        if iter:
            r = model.get_path(iter)[0]
            self.vardisp.selected = r
        return
    def set_quit(self):
        self.hide()
        self.destroy()
        bdb.Bdb.set_quit(self)

    def reset(self):
        bdb.Bdb.reset(self)
        self.forget()
    def forget(self):
        self.lineno = None
        self.stack = []
        self.curindex = 0
        self.curframe = None
    def setup(self, f, t):
        self.forget()
        self.stack, self.curindex = self.get_stack(f, t)
        self.curframe = self.stack[self.curindex][0]
        return
    # interaction functions -- overriden from bdb
    def user_line(self, frame):
        # called when we stop or break at this line
        self.interaction(frame, None)
    def user_return(self, frame, return_value):
        # called when a return trap is set here
        frame.f_locals['__return__'] = return_value
        if frame.f_code.co_name:
            func = frame.f_code.co_name
        else:
            func = "<lambda>"
        self.set_status(func + " returned " + repr.repr(return_value))
        self.interaction(frame, None)
    def user_exception(self, frame, (exc_type, exc_value, exc_traceback)):
        frame.f_locals['__exception__'] = exc_type, exc_value
        if type(exc_type) == type(''):
            exc_type_name = exc_type
        else: exc_type_name = exc_type.__name__
        self.set_status(exc_type_name + ':' + repr.repr(exc_value))
        self.interaction(frame, exc_traceback)

    def interaction(self, frame, traceback):
        self.setup(frame, traceback)
        self.update_stack_listing(self.curindex)
        gtk.main()
        self.forget()

    def update_stack_listing(self, curindex):
        self.blockupdate = 1
        model = self.stackdisp.get_model()
        model.clear()
        for i in range(len(self.stack)):
            frame_lineno = self.stack[i]
            row = self.format_stack_entry(frame_lineno, "##!##")
            row = string.split(row, "##!##")[0]
            model.append([row])
        self.blockupdate = 0
        self.stackdisp.scroll_to_cell(curindex, None, True, 1.0, 0.0)
        self.stackdisp.get_selection().select_path(curindex)
        return
    def update_var_listing(self):
        model = self.vardisp.get_model()
        model.clear()
        locals = self.curframe.f_locals
        self.vardisp.varnames = locals.keys()
        self.vardisp.varnames.sort()
        for var in self.vardisp.varnames:
            row = [var, type(locals[var]).__name__, repr.repr(locals[var])]
            model.append(row)
        self.vardisp.get_selection().select_path(0)
        return
    def update_code_listing(self):
        frame = self.curframe
        newfile = frame.f_code.co_filename
        if newfile != self.filename:
            lines = linecache.getlines(newfile)
            self.filename = newfile
            self.blockupdate = 1
            model = self.filedisp.get_model()
            model.clear()
            breaks = self.get_file_breaks(newfile)
            for line in range(len(lines)):
                if line+1 in breaks:
                    model.append([self.minibreak, line+1,
                                  lines[line].rstrip()])
                else:
                    model.append([None, line+1, lines[line].rstrip()])
            self.blockupdate = 0
        self.selected = frame.f_lineno
        lineno = self.selected
        if newfile != '<string>':
            self.filedisp.scroll_to_cell(lineno - 1, None, True, 1.0, 0.0)
            self.filedisp.get_selection().select_path(lineno - 1)
        return
    def do_next(self, _b=None):
        self.set_next(self.curframe)
        gtk.main_quit()
    def do_step(self, _b=None):
        self.set_step()
        gtk.main_quit()
    def do_return(self, _b=None):
        self.set_return(self.curframe)
        gtk.main_quit()
    def do_continue(self, _b=None):
        self.set_continue()
        gtk.main_quit()
    def do_quit(self, _b=None, _e=None):
        self.set_quit()
        gtk.main_quit()
    def do_break(self, _b=None):
        breaks = self.get_file_breaks(self.filename)
        if self.selected in breaks:
            err = self.clear_break(self.filename, self.selected)
            if err:
                self.set_status(err)
                return
            self.filedisp.get_model()[self.selected-1][0] = None
        else:
            err = self.set_break(self.filename, self.selected)
            if err:
                self.set_status(err)
                return
            self.filedisp.get_model()[self.selected-1][0] = self.minibreak
        return
    def do_run(self, _b=None):
        line = dialogs.InputBox("Execute Code", "Enter code to execute:", self)
        if line == None: return
        locals = self.curframe.f_locals
        globals = self.curframe.f_globals
        globals['__privileged__'] = 1
        try:
            code = compile(line + '\n', '<stdin>', 'single')
            exec code in globals, locals
        except:
            if type(sys.exc_type) == type(''):
                exc_type_name = sys.exc_type
            else: exc_type_name = sys.exc_type.__name__
            self.set_status('*** ' + exc_type_name + ': ' +
                            str(sys.exc_value))
            return
        self.update_var_listing()
        return
    def do_edit(self, _b=None):
        locals = self.curframe.f_locals
        varname = self.vardisp.varnames[self.vardisp.selected]
        val = repr.repr(locals[varname])
        value = dialogs.InputBox("Edit Variable",
                                 "Enter new value for " + varname + ":",
                                 self, val)
        if value == None: return
        globals = self.curframe.f_globals
        globals['__privileged__'] = 1
        try:
            val = eval(value, globals, locals)
            self.curframe.f_locals[varname] = val
        except:
            if type(sys.exc_type) == type(''):
                exc_type_name = sys.exc_type
            else: exc_type_name = sys.exc_type.__name__
            self.set_status('*** ' + exc_type_name + ': ' +
                            str(sys.exc_value))
            return
        row = self.vardisp.selected
        model = self.vardisp.get_model()
        model[row][1] = type(val).__name__
        model[row][2] = repr.repr(val)

# this makes up the interface that is compatible with pdb.
def run(statement, globals=None, locals=None):
    win = PyGTKDb()
    win.show()
    win.run(statement, globals, locals)

def runeval(expression, globals=None, locals=None):
    win = PyGTKDb()
    win.show()
    return win.runeval(expression, globals, locals)

def runcall(*args):
    win = PyGTKDb()
    win.show()
    return apply(win.runcall, args)

def set_trace():
    win = PyGTKDb()
    win.show()
    win.set_trace()

def post_mortem(traceback):
    win = PyGTKDb()
    win.show()
    win.reset()
    win.interaction(None, traceback)

def pm():
    post_mortem(sys.last_traceback)

if __name__ == '__main__':
    import os
    if not sys.argv[1:]:
        print "usage: gtkdb.py scriptfile [args ...]"
        sys.exit(2)
    filename = sys.argv[1]
    del sys.argv[0] # delete gtkdb.py
    sys.path.insert(0, os.path.dirname(filename))

    run('execfile("' + filename + '")', {'__name__': '__main__'})
