#!/usr/bin/env python
'''Text Widget/Hypertext

Usually, tags modify the appearance of text in the view, e.g. making it
bold or colored or underlined. But tags are not restricted to appearance.
They can also affect the behavior of mouse and key presses, as this demo
shows.'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import pygtk
pygtk.require('2.0')
import gtk
import pango

class HypertextDemo(gtk.Window):
    hovering_over_link = False
    hand_cursor = gtk.gdk.Cursor(gtk.gdk.HAND2)
    regular_cursor = gtk.gdk.Cursor(gtk.gdk.XTERM)

    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())

        self.set_title(self.__class__.__name__)
        self.set_default_size(450, 450)
        self.set_border_width(0)

        view = gtk.TextView()
        view.set_wrap_mode(gtk.WRAP_WORD)
        view.connect("key-press-event", self.key_press_event)
        view.connect("event-after", self.event_after)
        view.connect("motion-notify-event", self.motion_notify_event)
        view.connect("visibility-notify-event", self.visibility_notify_event)

        buffer = view.get_buffer()

        sw = gtk.ScrolledWindow()
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.add(sw)
        sw.add(view)

        self.show_page(buffer, 1)

        self.show_all()

    # Links can be activated by pressing Enter.
    def key_press_event(self, text_view, event):
        if (event.keyval == gtk.keysyms.Return or
            event.keyval == gtk.keysyms.KP_Enter):
            buffer = text_view.get_buffer()
            iter = buffer.get_iter_at_mark(buffer.get_insert())
            self.follow_if_link(text_view, iter)
        return False

    # Links can also be activated by clicking.
    def event_after(self, text_view, event):
        if event.type != gtk.gdk.BUTTON_RELEASE:
            return False
        if event.button != 1:
            return False
        buffer = text_view.get_buffer()

        # we shouldn't follow a link if the user has selected something
        try:
            start, end = buffer.get_selection_bounds()
        except ValueError:
            # If there is nothing selected, None is return
            pass
        else:
            if start.get_offset() != end.get_offset():
                return False

        x, y = text_view.window_to_buffer_coords(gtk.TEXT_WINDOW_WIDGET,
            int(event.x), int(event.y))
        iter = text_view.get_iter_at_location(x, y)

        self.follow_if_link(text_view, iter)
        return False


    # Looks at all tags covering the position (x, y) in the text view,
    # and if one of them is a link, change the cursor to the "hands" cursor
    # typically used by web browsers.
    def set_cursor_if_appropriate(self, text_view, x, y):
        hovering = False

        buffer = text_view.get_buffer()
        iter = text_view.get_iter_at_location(x, y)

        tags = iter.get_tags()
        for tag in tags:
            page = tag.get_data("page")
            if page != 0:
                hovering = True
                break

        if hovering != self.hovering_over_link:
            self.hovering_over_link = hovering

        if self.hovering_over_link:
            text_view.get_window(gtk.TEXT_WINDOW_TEXT).set_cursor(self.hand_cursor)
        else:
            text_view.get_window(gtk.TEXT_WINDOW_TEXT).set_cursor(self.regular_cursor)

    # Update the cursor image if the pointer moved.
    def motion_notify_event(self, text_view, event):
        x, y = text_view.window_to_buffer_coords(gtk.TEXT_WINDOW_WIDGET,
            int(event.x), int(event.y))
        self.set_cursor_if_appropriate(text_view, x, y)
        text_view.window.get_pointer()
        return False

    # Also update the cursor image if the window becomes visible
    # (e.g. when a window covering it got iconified).
    def visibility_notify_event(self, text_view, event):
        wx, wy, mod = text_view.window.get_pointer()
        bx, by = text_view.window_to_buffer_coords(gtk.TEXT_WINDOW_WIDGET, wx, wy)

        self.set_cursor_if_appropriate (text_view, bx, by)
        return False

    def insert_link(self, buffer, iter, text, page):
        ''' Inserts a piece of text into the buffer, giving it the usual
            appearance of a hyperlink in a web browser: blue and underlined.
            Additionally, attaches some data on the tag, to make it recognizable
            as a link.
        '''
        tag = buffer.create_tag(None,
            foreground="blue", underline=pango.UNDERLINE_SINGLE)
        tag.set_data("page", page)
        buffer.insert_with_tags(iter, text, tag)


    def show_page(self, buffer, page):
        ''' Fills the buffer with text and interspersed links. In any real
            hypertext app, this method would parse a file to identify the links.
        '''
        buffer.set_text("", 0)
        iter = buffer.get_iter_at_offset(0)
        if page == 1:
            buffer.insert(iter, "Some text to show that simple ")
            self.insert_link(buffer, iter, "hypertext", 3)
            buffer.insert(iter, " can easily be realized with ")
            self.insert_link(buffer, iter, "tags", 2)
            buffer.insert(iter, ".")

        elif page == 2:
            buffer.insert(iter,
                "A tag is an attribute that can be applied to some range of text. "
                "For example, a tag might be called \"bold\" and make the text inside "
                "the tag bold. However, the tag concept is more general than that "
                "tags don't have to affect appearance. They can instead affect the "
                "behavior of mouse and key presses, \"lock\" a range of text so the "
                "user can't edit it, or countless other things.\n", -1)
            self.insert_link(buffer, iter, "Go back", 1)
        elif page == 3:
            tag = buffer.create_tag(None, weight=pango.WEIGHT_BOLD)
            buffer.insert_with_tags(iter, "hypertext:\n", tag)
            buffer.insert(iter,
                "machine-readable text that is not sequential but is organized "
                "so that related items of information are connected.\n")
            self.insert_link(buffer, iter, "Go back", 1)


    def follow_if_link(self, text_view, iter):
        ''' Looks at all tags covering the position of iter in the text view,
            and if one of them is a link, follow it by showing the page identified
            by the data attached to it.
        '''
        tags = iter.get_tags()
        for tag in tags:
            page = tag.get_data("page")
            if page != 0:
                self.show_page(text_view.get_buffer(), page)
                break


def main():
    HypertextDemo()
    gtk.main()

if __name__ == '__main__':
    main()
