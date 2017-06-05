"""Print Editor

A small demo editor with printing support"""

import sys

import pygtk
pygtk.require('2.0')
import gobject
import pango
import gtk

main_window = None
filename = None
page_setup = None
settings = None
file_changed = False
buffer = None
statusbar = None
active_prints = []

def update_title():
    if filename is None:
        basename = "Untitled"
    else:
        basename = os.path.basename(filename)
    main_window.set_title("Simple Editor with printing - %s" % basename)

def update_statusbar():
    statusbar.pop(0)
  
    iter = buffer.get_iter_at_mark(buffer.get_insert())

    row = iter.get_line()
    col = iter.get_line_offset()

    print_str = "";
    if active_prints:
        op = active_prints[0]
        print_str = op.get_status_string()
  
    msg = "%d, %d%s %s" % (row, col, (file_changed and " - Modified" or ""),
                           print_str)

    statusbar.push(0, msg)

def update_ui():
    update_title()
    update_statusbar()

def get_text():
    start, end = buffer.get_bounds()
    return buffer.get_text(start, end, False)

def set_text(text):
    buffer.set_text(text)
    global file_changed
    file_changed = False
    update_ui()


def do_new(action):
    global filename
    filename = None
    set_text("")


def load_file(open_filename):
    error_dialog = None
    try:
        contents = file(open_filename).read()
    except IOError, ex:
        error_dialog = gtk.MessageDialog(main_window,
                                         gtk.DIALOG_DESTROY_WITH_PARENT,
                                         gtk.MESSAGE_ERROR,
                                         gtk.BUTTONS_CLOSE,
                                         "Error loading file %s:\n%s" %
                                         (open_filename,
                                          str(ex)))
    else:
        try:
            contents = contents.decode("utf-8")
        except UnicodeDecodeError:
            error_dialog = gtk.MessageDialog(main_window,
                                             gtk.DIALOG_DESTROY_WITH_PARENT,
                                             gtk.MESSAGE_ERROR,
                                             gtk.BUTTONS_CLOSE,
                                             "Error loading file %s:\n%s" %
                                             (open_filename,
                                              "Not valid utf8"))
        else:
            set_text(contents)
    if error_dialog is not None:
        error_dialog.connect("response", lambda w,resp: w.destroy())
        error_dialog.show()


def do_open(action):
    dialog = gtk.FileChooserDialog("Select file",
                                   main_window,
                                   gtk.FILE_CHOOSER_ACTION_OPEN,
                                   (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                    gtk.STOCK_OPEN, gtk.RESPONSE_OK))
    dialog.set_default_response(gtk.RESPONSE_OK)
    response = dialog.run()
    if response == gtk.RESPONSE_OK:
        open_filename = dialog.get_filename()
        load_file(open_filename)
    dialog.destroy()


def save_file(save_filename):
    global filename
    text = get_text()
    error_dialog = None

    try:
        file(save_filename, "w").write(text)
    except IOError, ex:
        error_dialog = gtk.MessageDialog(main_window,
                                         gtk.DIALOG_DESTROY_WITH_PARENT,
                                         gtk.MESSAGE_ERROR,
                                         gtk.BUTTONS_CLOSE,
                                         "Error saving to file %s:\n%s" %
                                         (open_filename,
                                          str(ex)))
        error_dialog.connect("response", lambda w,resp: w.destroy())
        error_dialog.show()
    else:
        if save_filename != filename:
            filename = save_filename
        file_changed = False
        update_ui()


def do_save_as(action):
    dialog = gtk.FileChooserDialog("Select file",
                                   main_window,
                                   gtk.FILE_CHOOSER_ACTION_SAVE,
                                   (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                    gtk.STOCK_SAVE, gtk.RESPONSE_OK))
    dialog.set_default_response(gtk.RESPONSE_OK)
    response = dialog.run()

    if response == gtk.RESPONSE_OK:
        save_filename = dialog.get_filename()
        save_file(save_filename)
  
    dialog.destroy()

def do_save(action):
    if filename is None:
        do_save_as(action)
    else:
        save_file(filename)

class PrintData:
    text = None
    layout = None
    page_breaks = None


def begin_print(operation, context, print_data):
    width = context.get_width()
    height = context.get_height()
    print_data.layout = context.create_pango_layout()
    print_data.layout.set_font_description(pango.FontDescription("Sans 12"))
    print_data.layout.set_width(int(width*pango.SCALE))
    print_data.layout.set_text(print_data.text)

    num_lines = print_data.layout.get_line_count()

    page_breaks = []
    page_height = 0

    for line in xrange(num_lines):
      layout_line = print_data.layout.get_line(line)
      ink_rect, logical_rect = layout_line.get_extents()
      lx, ly, lwidth, lheight = logical_rect
      line_height = lheight / 1024.0
      if page_height + line_height > height:
	  page_breaks.append(line)
	  page_height = 0
      page_height += line_height

    operation.set_n_pages(len(page_breaks) + 1)
    print_data.page_breaks = page_breaks


def draw_page(operation, context, page_nr, print_data):
    assert isinstance(print_data.page_breaks, list)
    if page_nr == 0:
        start = 0
    else:
        start = print_data.page_breaks[page_nr - 1]

    try:
        end = print_data.page_breaks[page_nr]
    except IndexError:
        end = print_data.layout.get_line_count()
    
    cr = context.get_cairo_context()

    cr.set_source_rgb(0, 0, 0)
  
    i = 0
    start_pos = 0
    iter = print_data.layout.get_iter()
    while 1:
        if i >= start:
            line = iter.get_line()
            _, logical_rect = iter.get_line_extents()
            lx, ly, lwidth, lheight = logical_rect
            baseline = iter.get_baseline()
            if i == start:
                start_pos = ly / 1024.0;
            cr.move_to(lx / 1024.0, baseline / 1024.0 - start_pos)
            cr.show_layout_line(line)
        i += 1
        if not (i < end and iter.next_line()):
            break


def do_page_setup(action):
    global settings, page_setup
    if settings is None:
        settings = gtk.PrintSettings()
    page_setup = gtk.print_run_page_setup_dialog(main_window,
                                                 page_setup, settings)


def status_changed_cb(op):
    if op.is_finished():
        active_prints.remove(op)
    update_statusbar()


def do_print(action):
    global settings, page_setup
    print_data = PrintData()
    print_data.text = get_text()
    print_ = gtk.PrintOperation()
    if settings is not None:
        print_.set_print_settings(settings)

    if page_setup is not None:
        print_.set_default_page_setup(page_setup)
  
    print_.connect("begin_print", begin_print, print_data)
    print_.connect("draw_page", draw_page, print_data)

    try:
        res = print_.run(gtk.PRINT_OPERATION_ACTION_PRINT_DIALOG, main_window)
    except gobject.GError, ex:
        error_dialog = gtk.MessageDialog(main_window,
                                         gtk.DIALOG_DESTROY_WITH_PARENT,
                                         gtk._MESSAGE_ERROR,
                                         gtk.BUTTONS_CLOSE,
                                         ("Error printing file:\n%s" % str(ex)))
        error_dialog.connect("response", lambda w,resp: w.destroy())
        error_dialog.show()
    else:
        if res == gtk.PRINT_OPERATION_RESULT_APPLY:
            settings = print_.get_print_settings()

    if not print_.is_finished():
        active_prints.remove(print_)
        update_statusbar()
      
        print_.connect("status_changed", status_changed_cb)


def do_about(action):
    authors = [
        "Alexander Larsson (C version)",
        "Gustavo Carneiro (Python translation)",
        ]
    about = gobject.new(gtk.AboutDialog, name="print test editor",
                        version="0.1", copyright="(C) Red Hat, Inc",
                        comments="Program to demonstrate GTK+ printing.",
                        authors=authors)
    about.set_transient_for(main_window)
    about.show()


def do_quit(action):
    if __name__ == '__main__':
        gtk.main_quit()

entries = [
  ( "FileMenu", None, "_File" ),               # name, stock id, label
  ( "HelpMenu", None, "_Help" ),               # name, stock id, label
  ( "New", gtk.STOCK_NEW,                      # name, stock id
    "_New", "<control>N",                      # label, accelerator
    "Create a new file",                       # tooltip
    do_new ),      
  ( "Open", gtk.STOCK_OPEN,                    # name, stock id
    "_Open","<control>O",                      # label, accelerator
    "Open a file",                             # tooltip
    do_open ), 
  ( "Save", gtk.STOCK_SAVE,                    # name, stock id
    "_Save","<control>S",                      # label, accelerator
    "Save current file",                       # tooltip
    do_save ),
  ( "SaveAs", gtk.STOCK_SAVE,                  # name, stock id
    "Save _As...", None,                       # label, accelerator
    "Save to a file",                          # tooltip
    do_save_as ),
  ( "Quit", gtk.STOCK_QUIT,                    # name, stock id
    "_Quit", "<control>Q",                     # label, accelerator
    "Quit",                                    # tooltip
    do_quit ),
  ( "About", None,                             # name, stock id
    "_About", "<control>A",                    # label, accelerator
    "About",                                   # tooltip
    do_about ),
  ( "PageSetup", None,                         # name, stock id
    "Page _Setup", None,                       # label, accelerator
    "Set up the page",                         # tooltip
    do_page_setup ),
  ( "Print", gtk.STOCK_PRINT,                  # name, stock id
     None, None,                               # label, accelerator
    "Print the document",                      # tooltip
    do_print ),
]

ui_info ="""
<ui>
  <menubar name='MenuBar'>
    <menu action='FileMenu'>
      <menuitem action='New'/>
      <menuitem action='Open'/>
      <menuitem action='Save'/>
      <menuitem action='SaveAs'/>
      <menuitem action='PageSetup'/>
      <menuitem action='Print'/>
      <separator/>
      <menuitem action='Quit'/>
    </menu>
    <menu action='HelpMenu'>
      <menuitem action='About'/>
    </menu>
  </menubar>
</ui>
"""

def buffer_changed_callback(buffer):
    global file_changed
    file_changed = True
    update_statusbar()


def mark_set_callback(buffer, new_location, mark):
    update_statusbar()

def update_resize_grip(widget, event, statusbar):
    if event.changed_mask & (gtk.gdk.WINDOW_STATE_MAXIMIZED | 
                             gtk.gdk.WINDOW_STATE_FULLSCREEN):
        maximized = event.new_window_state & (gtk.gdk.WINDOW_STATE_MAXIMIZED | 
                                              gtk.gdk.WINDOW_STATE_FULLSCREEN)
        statusbar.set_has_resize_grip(not maximized)


def create_window():
    global main_window, statusbar, buffer
    main_window = gtk.Window()
    main_window.set_default_size(400, 600)
    if __name__ == '__main__':
        main_window.connect("delete-event", gtk.main_quit)
    actions = gtk.ActionGroup("Actions")
    actions.add_actions(entries)
  
    ui = gtk.UIManager()
    ui.insert_action_group(actions, 0)
    main_window.add_accel_group(ui.get_accel_group())
    main_window.set_border_width(0)

    ui.add_ui_from_string(ui_info)

    table = gtk.Table(1, 3, False)
    main_window.add(table)

    bar = ui.get_widget("/MenuBar")
    bar.show()
    table.attach(bar, 
                 # /* X direction */       /* Y direction */
                 0, 1,                      0, 1,
                 gtk.EXPAND | gtk.FILL,     0,
                 0,                         0)

    ## Create document 
    sw = gtk.ScrolledWindow()
    sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
    sw.set_shadow_type(gtk.SHADOW_IN)
    table.attach(sw,
		 # /* X direction */       /* Y direction */
                 0, 1,                   1, 2,
                 gtk.EXPAND | gtk.FILL,  gtk.EXPAND | gtk.FILL,
                 0,                      0)
  
    contents = gtk.TextView()
    contents.grab_focus()
    sw.add(contents)

    ## Create statusbar
  
    statusbar = gtk.Statusbar()
    table.attach(statusbar,
		 #/* X direction */       /* Y direction */
                 0, 1,                   2, 3,
                 gtk.EXPAND | gtk.FILL,  0,
                 0,                      0);
    
    ## Show text widget info in the statusbar */
    buffer = contents.get_buffer()
  
    buffer.connect_object("changed",
			  buffer_changed_callback,
                          None)
  
    buffer.connect_object("mark_set", # cursor moved
                          mark_set_callback,
                          None)
  
    main_window.connect_object("window_state_event", 
                               update_resize_grip,
                               statusbar,
                               0)
  
    update_ui()
  
    main_window.show_all()


def main(argv):
    create_window()

    try:
        fname = argv[1]
    except IndexError:
        pass
    else:
        load_file(fname)
  
    gtk.main()

def PrintEditorDemo(win):
    create_window()
    main_window.set_transient_for(win)
    return main_window

if __name__ == '__main__':
    sys.exit(main(sys.argv))
