#!/usr/bin/env python
"""Text Widget/Automatic scrolling

This example demonstrates how to use the gravity of
GtkTextMarks to keep a text view scrolled to the bottom
while appending text."""

import pygtk
pygtk.require('2.0')
import gobject
import gtk

class AutomaticScrollingDemo(gtk.Window):
    def __init__(self, parent=None):
        # Create the toplevel window
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())

        self.set_title(self.__class__.__name__)
        self.set_default_size(600, 400)
        self.set_border_width(0)

        hbox = gtk.HBox(True, 6)
        self.add(hbox)

        self.create_text_view(hbox, True)
        self.create_text_view(hbox, False)
        self.count_sb = 0
        self.count_se = 0

        self.show_all()

    def create_text_view(self, hbox, scroll_to_end):
        swindow = gtk.ScrolledWindow()
        hbox.pack_start(swindow)
        textview = gtk.TextView()
        swindow.add(textview)

        timeout = self.setup_scroll(textview, scroll_to_end)

        # Remove the timeout in destroy handler, so we don't try to
        # scroll destroyed widget.
        textview.connect("destroy", lambda widget: gobject.source_remove(timeout))

    def setup_scroll(self, textview, scroll_to_end):
        buf = textview.get_buffer()
        itr = buf.get_end_iter()

        if scroll_to_end:
            # If we want to scroll to the end, including horizontal scrolling,
            # then we just create a mark with right gravity at the end of the
            # buffer. It will stay at the end unless explicitely moved with
            # gtk_text_buffer_move_mark.
            buf.create_mark("end", itr, False)

            # Add scrolling timeout.
            return gobject.timeout_add(50, self.scroll_to_end, textview)
        else:
            # If we want to scroll to the bottom, but not scroll horizontally,
            # then an end mark won't do the job. Just create a mark so we can
            # use it with gtk_text_view_scroll_mark_onscreen, we'll position it
            # explicitely when needed. Use left gravity so the mark stays where
            # we put it after inserting new text.
            buf.create_mark("scroll", itr, True)

            # Add scrolling timeout.
            return gobject.timeout_add(100, self.scroll_to_bottom, textview)

    """ Scroll to the end of the buffer. """
    def scroll_to_end(self, textview):
        buf = textview.get_buffer()

        # Get the "end" mark. It's located at the end of buffer because
        # of right gravity
        mark = buf.get_mark("end")
        itr = buf.get_iter_at_mark(mark)

        # and insert some text at its position, the iter will be
        # revalidated after insertion to point to the end of inserted text
        buf.insert(itr, "\n")
        buf.insert(itr, " " * self.count_se)
        buf.insert(itr, "Scroll to end scroll to end scroll to end scroll to end ")

        # Now scroll the end mark onscreen.
        textview.scroll_mark_onscreen(mark)

        # Emulate typewriter behavior, shift to the left if we 
        # are far enough to the right.
        self.count_se += 1
        if self.count_se > 150:
            self.count_se = 0

        return True

    """ Scroll to the bottom of the buffer. """
    def scroll_to_bottom(self, textview):
        buf = textview.get_buffer()

        # Get the end iterator
        itr = buf.get_end_iter()

        # and insert some text at it, the iter will be revalidated
        # after insertion to point to the end of inserted text
        buf.insert(itr, "\n")
        buf.insert(itr, " " * self.count_sb)
        buf.insert(itr, "Scroll to bottom scroll to bottom scroll to bottom scroll to bottom")

        # Move the iterator to the beginning of line, so we don't scroll
        # in horizontal direction
        itr.set_line_offset(0)

        # and place the mark at iter. the mark will stay there after we
        # insert some text at the end because it has right gravity.
        mark = buf.get_mark("scroll")
        buf.move_mark(mark, itr)

        # Scroll the mark onscreen.
        textview.scroll_mark_onscreen(mark)

        # Shift text back if we got enough to the right.
        self.count_sb += 1
        if self.count_sb > 40:
            self.count_sb = 0

        return True

def main():
    AutomaticScrollingDemo()
    gtk.main()

if __name__ == '__main__':
    main()
