#!/usr/bin/env python
'''Tooltip

This is a test of the new gtk tooltip system.  It is a
fairly straight forward port of the example distributed with gtk.
'''

import pygtk
pygtk.require('2.0')
import gtk
import cairo
import gobject
import pango

rects = [
    {"x":10, "y":10, "r":0.0, "g":0.0, "b":0.9, "tooltip":"Blue box!"},
    {"x":200, "y":170, "r":1.0, "g":0.0, "b":0.0, "tooltip":"Red thing"},
    {"x":100, "y":50, "r":0.8, "g":0.8, "b":0.0, "tooltip":"Yellow thing"}
    ]

class TooltipDemo(gtk.Window):
    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.set_title(self.__class__.__name__)
        
        self.set_border_width(10)
        
        box = gtk.VBox(False, 3)
        self.add(box)
        
        # A check button using the tooltip-markup property
        button = gtk.CheckButton("This one uses the tooltip-markup property")
        button.set_tooltip_text("Hello, I am a static tooltip.")
        box.pack_start(button, False, False, 0)
        
        # A check button using the query-tooltip signal
        button = gtk.CheckButton("I use the query-tooltip signal")
        button.props.has_tooltip = True
        button.connect("query-tooltip", self.query_tooltip_cb)
        box.pack_start(button, False, False, 0)
        
        # A label
        label = gtk.Label("I am just a label")
        label.set_selectable(False)
        label.set_tooltip_text("Label & and tooltip")
        box.pack_start(label, False, False, 0)
        
        # A selectable label
        label = gtk.Label("I am a selectable label")
        label.set_selectable(True)
        label.set_tooltip_markup("<b>Another</b> Label tooltip")
        box.pack_start(label, False, False, 0)
        
        # Another one, with a custom tooltip window
        button = gtk.CheckButton("This one has a custom tooltip window!")
        box.pack_start(button, False, False, 0)
        
        tooltip_window = gtk.Window(gtk.WINDOW_POPUP)
        tooltip_button = gtk.Label("blaat!")
        tooltip_window.add(tooltip_button)
        tooltip_button.show()
        
        button.set_tooltip_window(tooltip_window)
        button.connect("query-tooltip", self.query_tooltip_custom_cb)
        button.props.has_tooltip = True
        
        # An insensitive button
        button = gtk.Button("This one is insensitive")
        button.set_sensitive(False)
        button.props.tooltip_text = "Insensitive!"
        box.pack_start(button, False, False, 0)
        
        # Testcases from Kris without a tree view don't exist
        tree_view = gtk.TreeView(self.create_model())
        tree_view.set_size_request(200, 240)
        
        tree_view.insert_column_with_attributes(0, "Test",
                                                 gtk.CellRendererText(),
                                                 text = 0)
        
        tree_view.props.has_tooltip = True
        tree_view.connect("query-tooltip", self.query_tooltip_tree_view_cb)
        tree_view.get_selection().connect("changed",
                                          self.selection_changed_cb, tree_view)
        
        # We cannot get the button on the treeview column directly
        # so we have to use a ugly hack to get it.
        column = tree_view.get_column(0)
        column.set_clickable(True)
        label = gtk.Label("Test")
        column.set_widget(label)
        label.show()
        button = label.get_parent()
        button.props.tooltip_text = "Header"

        box.pack_start(tree_view, False, False, 2)

        # Add an IconView for some more testing
        iconview = gtk.IconView()
        iconview.props.has_tooltip = True
        iconview.connect("query-tooltip", self.query_tooltip_icon_view_cb)
        
        model = gtk.ListStore(str, gtk.gdk.Pixbuf)
        iconview.set_model(model)
        iconview.set_text_column(0)
        iconview.set_pixbuf_column(1)
        
        pixbuf1 = iconview.render_icon(gtk.STOCK_APPLY,gtk.ICON_SIZE_BUTTON)
        model.append(['Apply', pixbuf1])
        
        pixbuf2 = iconview.render_icon(gtk.STOCK_CANCEL,gtk.ICON_SIZE_BUTTON)
        model.append(['Cancel', pixbuf2])
        
        box.pack_start(iconview, False, False, 2)
        
        # And a text view for Matthias
        buffer = gtk.TextBuffer()
        
        iter = buffer.get_end_iter()
        buffer.insert(iter, "Hello, the text ", -1)
        
        tag = buffer.create_tag("bold")
        tag.props.weight = pango.WEIGHT_BOLD
        
        iter = buffer.get_end_iter()
        buffer.insert_with_tags(iter, "in bold", tag)
        
        iter = buffer.get_end_iter()
        buffer.insert(iter, " has a tooltip!", -1)
        
        text_view = gtk.TextView(buffer)
        text_view.set_size_request(200, 50)
        
        text_view.props.has_tooltip = True
        text_view.connect("query-tooltip", self.query_tooltip_text_view_cb, tag)
        
        box.pack_start(text_view, False, False, 2)
        
        # Drawing area
        drawing_area = gtk.DrawingArea()
        drawing_area.set_size_request(320, 240)
        drawing_area.props.has_tooltip = True
        drawing_area.connect("expose_event", self.drawing_area_expose)
        drawing_area.connect("query-tooltip",
                             self.query_tooltip_drawing_area_cb)
        box.pack_start(drawing_area, False, False, 2)
        
        # Done!
        self.show_all()
    
    def query_tooltip_cb(self, widget, x, y, keyboard_tip, tooltip):
        tooltip.set_markup(widget.get_label())
        tooltip.set_icon_from_stock(gtk.STOCK_DELETE, gtk.ICON_SIZE_MENU)
    
        return True
    
    def query_tooltip_custom_cb(self, widget, x, y, keyboard_tip, tooltip):
        color = gtk.gdk.Color(0, 65535, 0)
        window = widget.get_tooltip_window()
    
        window.modify_bg(gtk.STATE_NORMAL, color)
    
        return True
    
    def query_tooltip_text_view_cb(self, widget, x, y,
                                   keyboard_tip, tooltip, data):
        if keyboard_tip:
            offset= widget.props.buffer.cursor_position
            iter = widget.props.buffer.get_iter_at_offset(offset)
        else:
            coords = widget.window_to_buffer_coords(gtk.TEXT_WINDOW_TEXT, x, y)
            ret =widget.get_iter_at_position(coords[0], coords[1])
    
        if ret[0].has_tag(data):
            tooltip.set_text("Tooltip on text tag")
        else:
            return False
    
        return True
    
    def query_tooltip_tree_view_cb(self, widget, x, y, keyboard_tip, tooltip):
        if not widget.get_tooltip_context(x, y, keyboard_tip):
            return False
        else:
            model, path, iter = widget.get_tooltip_context(x, y, keyboard_tip)

            value = model.get(iter, 0)
            tooltip.set_markup("<b>Path %s:</b> %s" %(path[0], value[0]))
            widget.set_tooltip_row(tooltip, path)
            return True

    def query_tooltip_icon_view_cb(self, widget, x, y, keyboard_tip, tooltip):
        if not widget.get_tooltip_context(x, y, keyboard_tip):
            return False
        else:
            model, path, iter = widget.get_tooltip_context(x, y, keyboard_tip)
    
            value = model.get(iter, 0)
            tooltip.set_markup("<b>Path %s:</b> %s" %(path[0], value[0]))
            widget.set_tooltip_item(tooltip, path)
            return True

    def query_tooltip_drawing_area_cb(self, widget, x, y, keyboard_tip,
                                      tooltip, data=None):
        if keyboard_tip:
            return False
        
        for i in range(len(rects)):
            if(rects[i]["x"] < x and x < rects[i]["x"] + 50 \
                    and rects[i]["y"] < y and y < rects[i]["y"] + 50):
                tooltip.set_markup(rects[i]["tooltip"])
                return True;
        return False
    
    def selection_changed_cb(self, selection, tree_view):
        tree_view.trigger_tooltip_query()
    
    def create_model(self):    
        store = gtk.TreeStore(gobject.TYPE_STRING);
        
        # A tree store with some random words ...
        store.append(None, ("File Manager",))
        store.append(None, ("Gossip",))
        store.append(None, ("System Settings",))
        store.append(None, ("The GIMP",))
        store.append(None, ("Terminal",))
        store.append(None, ("Word Processor",))
        
        return(store)
    
    def drawing_area_expose(self, drawing_area, event, data=None):
        cr = drawing_area.window.cairo_create()
        
        cr.rectangle(0, 0,
                     drawing_area.allocation.width,
                     drawing_area.allocation.height)
        cr.set_source_rgb(1.0, 1.0, 1.0)
        cr.fill()
        
        for i in range(len(rects)):
            cr.rectangle(rects[i]["x"], rects[i]["y"], 50, 50)
            cr.set_source_rgb(rects[i]["r"], rects[i]["g"], rects[i]["b"])
            cr.stroke()
        
            cr.rectangle(rects[i]["x"], rects[i]["y"], 50, 50)
            cr.set_source_rgba(rects[i]["r"], rects[i]["g"], rects[i]["b"], 0.5)
            cr.fill()
    
        return False

def main():
    TooltipDemo()
    gtk.main()

if __name__ == '__main__':
    main()