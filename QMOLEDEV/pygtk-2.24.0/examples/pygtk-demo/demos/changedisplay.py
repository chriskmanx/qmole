#!/usr/bin/env python
'''Change Display

Demonstrates migrating a window between different displays and
screens. A display is a mouse and keyboard with some number of
associated monitors. A screen is a set of monitors grouped
into a single physical work area. The neat thing about having
multiple displays is that they can be on a completely separate
computers, as long as there is a network connection to the
computer where the application is running.

Only some of the windowing systems where GTK+ runs have the
concept of multiple displays and screens. (The X Window System
is the main example.) Other windowing systems can only
handle one keyboard and mouse, and combine all monitors into
a single screen.

This is a moderately complex example, and demonstrates:

- Tracking the currently open displays and screens
- Changing the screen for a window
- Letting the user choose a window by clicking on it
- Using GtkListStore and GtkTreeView
- Using GtkDialog
'''
import pygtk
pygtk.require('2.0')
import gtk
import gobject

# These enumerations provide symbolic names for the columns
# in the two GtkListStore models.
#
(
  DISPLAY_COLUMN_NAME,
  DISPLAY_COLUMN_DISPLAY,
  DISPLAY_NUM_COLUMNS
) = range(3)

(
  SCREEN_COLUMN_NUMBER,
  SCREEN_COLUMN_SCREEN,
  SCREEN_NUM_COLUMNS
) = range(3)

def find_toplevel_at_pointer(display):
    ''' Finds the toplevel window under the mouse pointer, if any.
    '''
    pointer_window = display.get_window_at_pointer()[0]

    # The user data field of a GdkWindow is used to store a pointer
    # to the widget that created it.
    #
    if pointer_window:
        widget = pointer_window.get_user_data()

    return widget and widget.get_toplevel() or None

class QueryForToplevel(gtk.Window):
    ''' Asks the user to click on a window, then waits for them click
        the mouse. When the mouse is released, returns the toplevel
        window under the pointer, or NULL, if there is none.
    '''

    def __init__(self, screen, prompt):
        gtk.Window.__init__(self, gtk.WINDOW_POPUP)
        self.set_screen(screen)
        self.set_modal(True)
        self.set_position(gtk.WIN_POS_CENTER)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)
        self.add(frame)

        label = gtk.Label(prompt)
        label.set_padding(10, 10)
        frame.add(label)

        self.show_all()

    def run(self):
        display = self.get_screen().get_display()
        cursor = gtk.gdk.Cursor(display, gtk.gdk.CROSSHAIR)

        main_context = gobject.main_context_default()
        if (gtk.gdk.pointer_grab(self.window, False,
            gtk.gdk.BUTTON_RELEASE_MASK, None, cursor) == gtk.gdk.GRAB_SUCCESS):
            self.query_clicked = False
            self.connect("button-release-event", self.button_release_event_cb)

            # Process events until clicked is set by button_release_event_cb.
            # We pass in may_block=True since we want to wait if there
            # are no events currently.
            #
            while self.query_clicked is False:
                main_context.iteration(True)

            toplevel = find_toplevel_at_pointer(display)
            if (toplevel == self):
                toplevel = None;

        self.destroy()
        gtk.gdk.flush()     # Really release the grab

        return toplevel

    def button_release_event_cb(self, winref, event):
        self.query_clicked = True
        return True


class LeftAlignButton(gtk.Button):
    ''' If we have a stack of buttons, it often looks better if their contents
        are left-aligned, rather than centered. This class creates a button
        and left-aligns it contents.
    '''
    def __init__(self, label):
        gtk.Button.__init__(self, label)
        child = self.get_children()[0]
        child.set_alignment(0., 0.5)


# Main entry point. If the dialog for this demo doesn't yet exist, creates
# it.
#
class ChangeDisplayDemo(gtk.Dialog):
    size_group = None
    display_model = None
    screen_model = None
    screen_selection = None
    current_display = None
    current_screen = None

    def __init__(self, parent=None):
        gtk.Dialog.__init__(self, "Change Screen or display", parent,
            gtk.DIALOG_NO_SEPARATOR,
            (gtk.STOCK_CLOSE,  gtk.RESPONSE_CLOSE,
             "Change",         gtk.RESPONSE_OK))
        self.set_default_size(300, 400)

        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.connect("response", self.response_cb)
        self.connect("destroy", self.destroy_cb)

        vbox = gtk.VBox(False, 5)
        vbox.set_border_width(8)

        self.vbox.pack_start(vbox, True, True, 0)

        frame = self.__create_display_frame()
        vbox.pack_start(frame, True, True, 0)

        frame = self.__create_screen_frame()
        vbox.pack_start(frame, True, True, 0)

        self.__initialize_displays()

        self.show_all()

    def __initialize_displays(self):
        ''' Adds all currently open displays to our list of displays,
            and set up a signal connection so that we'll be notified
            when displays are opened in the future as well.
        '''
        manager = gtk.gdk.display_manager_get()
        displays = manager.list_displays()

        for item in displays:
            self.add_display(item)
        id = manager.connect("display_opened", self.display_opened_cb)
        manager.set_data('user-callback', id)

    def __create_frame(self, title):
        ''' This function is used both for creating the "Display" and
            "Screen" frames, since they have a similar structure. The
            caller hooks up the right context for the value returned
            in tree_view, and packs any relevant buttons into button_vbox.
        '''
        frame = gtk.Frame(title)

        hbox = gtk.HBox(False, 8)
        hbox.set_border_width(8)
        frame.add(hbox)

        scrollwin = gtk.ScrolledWindow();
        scrollwin.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
        scrollwin.set_shadow_type (gtk.SHADOW_IN)
        hbox.pack_start(scrollwin, True, True, 0)

        tree_view = gtk.TreeView()
        tree_view.set_headers_visible(False)
        scrollwin.add(tree_view)

        selection = tree_view.get_selection()
        selection.set_mode(gtk.SELECTION_BROWSE)

        button_vbox = gtk.VBox(False, 5)
        hbox.pack_start(button_vbox, False, False, 0)

        if self.size_group is None:
            self.size_group = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)

        self.size_group.add_widget(button_vbox)

        return (frame, tree_view, button_vbox)


    def __create_display_frame(self):
        ''' Creates the "Display" frame in the main window.
        '''
        frame, tree_view, button_vbox = self.__create_frame("Display")

        button = LeftAlignButton("_Open...")
        button.connect("clicked", self.open_display_cb)
        button_vbox.pack_start(button, False, False, 0)

        button = LeftAlignButton("_Close")
        button.connect ("clicked", self.close_display_cb)
        button_vbox.pack_start(button, False, False, 0)

        self.display_model = gtk.ListStore(str, object);
        tree_view.set_model(self.display_model)

        column = gtk.TreeViewColumn("Name", gtk.CellRendererText(),
            text=DISPLAY_COLUMN_NAME)
        tree_view.append_column(column)

        selection = tree_view.get_selection()
        selection.connect("changed", self.display_changed_cb)

        return frame

    def __create_screen_frame(self):
        ''' Creates the "Screen" frame in the main window.
        '''
        frame, tree_view, button_vbox = self.__create_frame("Screen")

        self.screen_model = gtk.ListStore(int, object);
        tree_view.set_model(self.screen_model)

        column = gtk.TreeViewColumn("Number", gtk.CellRendererText(),
            text=SCREEN_COLUMN_NUMBER)
        tree_view.append_column(column)

        self.screen_selection = tree_view.get_selection()
        self.screen_selection.connect("changed", self.screen_changed_cb)

        return frame

    def query_change_display(self):
        ''' Prompts the user for a toplevel window to move, and then moves
            that window to the currently selected display
        '''
        screen = self.window.get_screen()

        toplevel = QueryForToplevel(screen,
            "Please select the toplevel\nto move to the new screen").run()

        if toplevel is not None:
            toplevel.set_screen(self.current_screen)
        else:
            screen.get_display().beep()


    def response_cb(self, dialog, response_id):
        ''' Called when the user clicks on a button in our dialog or
            closes the dialog through the window manager. Unless the
            "Change" button was clicked, we destroy the dialog.
        '''
        if response_id == gtk.RESPONSE_OK:
            self.query_change_display()
        else:
            dialog.destroy()

    def open_display_cb(self, button):
        ''' Called when the user clicks on "Open..." in the display
            frame. Prompts for a new display, and then opens a connection
            to that display.
        '''
        dialog = gtk.Dialog("Open Display", self, gtk.DIALOG_MODAL,
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, gtk.STOCK_OK, gtk.RESPONSE_OK))

        dialog.set_default_response(gtk.RESPONSE_OK)
        display_entry = gtk.Entry()
        display_entry.set_activates_default(True)
        dialog_label = gtk.Label("Please enter the name of\nthe new display\n")

        dialog.vbox.add(dialog_label)
        dialog.vbox.add(display_entry)

        display_entry.grab_focus()
        dialog.show_all()

        result = None
        while result is None:
            response_id = dialog.run()
            if response_id != gtk.RESPONSE_OK:
                break;
            new_screen_name = display_entry.get_chars(0, -1)
            print new_screen_name
            if new_screen_name != "":
                result = gtk.gdk.Display(new_screen_name)
                if result is None:
                    error_msg = (
                    "Can't open display :\n\t%s\nplease try another one\n" %
                    (new_screen_name,))
                    dialog_label.set_text(error_msg)

        dialog.destroy()

    def close_display_cb(self, button):
        ''' Called when the user clicks on the "Close" button in the
            "Display" frame. Closes the selected display.
        '''
        if self.current_display:
            self.current_display.close()


    def display_changed_cb(self, selection):
        ''' Called when the selected row in the display list changes.
            Updates info.current_display, then refills the list of
            screens.
        '''
        model, iter = selection.get_selected()
        if iter is not None:
            self.current_display = model.get_value(iter, DISPLAY_COLUMN_DISPLAY)
        else:
            self.current_display = None
        self.fill_screens()

    def screen_changed_cb(self, selection):
        ''' Called when the selected row in the sceen list changes.
            Updates info->current_screen.
        '''
        model, iter = selection.get_selected()
        if iter:
            self.current_screen = model.get(iter, SCREEN_COLUMN_SCREEN)[0]
        else:
            self.current_screen = None;

    def destroy_cb(self, parent):
        self.destroy_info()
        if parent is None:
            gtk.main_quit()


    def fill_screens(self):
        ''' Fills in the screen list based on the current display
        '''
        self.screen_model.clear()
        if self.current_display is not None:
            n_screens = self.current_display.get_n_screens()

            for i in range(n_screens):
                screen = self.current_display.get_screen(i);
                iter = self.screen_model.append()
                self.screen_model.set(iter,
                    SCREEN_COLUMN_NUMBER, i, SCREEN_COLUMN_SCREEN, screen)
                if (i == 0):
                    self.screen_selection.select_iter(iter)

    def display_closed_cb(self, display, is_error, info):
        ''' Called when one of the currently open displays is closed.
            Remove it from our list of displays.
        '''
        iter = self.display_model.get_iter_first()
        while iter:
            tmp_display = self.display_model.get_value(iter, DISPLAY_COLUMN_DISPLAY)
            if (tmp_display == display):
                info.display_model.remove(iter)
                break;
            iter = info.display_model.iter_next()

    def add_display(self, display):
        ''' Adds a new display to our list of displays, and connects
            to the "closed" signal so that we can remove it from the
            list of displays again.
        '''
        name = display.get_name()

        iter = self.display_model.append()
        self.display_model.set(iter,
            DISPLAY_COLUMN_NAME, name, DISPLAY_COLUMN_DISPLAY, display)
        id = display.connect("closed", self.display_closed_cb)
        display.set_data('user-callback', id)

    def display_opened_cb(self, manager, display):
        ''' Called when a new display is opened
        '''
        self.add_display(display)

    def destroy_info(self):
        ''' Cleans up when the toplevel is destroyed; we remove the
            connections we use to track currently open displays.
        '''
        manager = gtk.gdk.display_manager_get()
        displays = manager.list_displays()

        id = manager.get_data('user-callback')
        manager.disconnect(id)

        for tmp_list in displays:
            id = tmp_list.get_data('user-callback')
            tmp_list.disconnect(id)


def main():
    ChangeDisplayDemo()
    gtk.main()

if __name__ == '__main__':
    main()
