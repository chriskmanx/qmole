#! /usr/bin/env python
# -*- coding: utf-8 -*-
#
# I, Adam Olsen, am the original author of this work.  I hereby
# donate it into the public domain, and relinquish any rights I
# may have in it.
#
# I, Behdad Esfahbod, hereby disclaim any rights for my contributions
# to this code.

from __future__ import division

import sys
import cairo
import pygtk
pygtk.require('2.0')
import gtk
import gtk.gdk
import pango
import gobject

def generate_modes():
    for align_desc, align in [('left', pango.ALIGN_LEFT),
            ('center', pango.ALIGN_CENTER), ('right', pango.ALIGN_RIGHT)]:
        for extent_desc, extentindex in [('logical', 1), ('ink', 0)]:
            for name in ['line', 'run', 'cluster', 'char']:
                if name == 'char' and extent_desc == 'ink':
                    continue
                desc = '%s %s %s' % (align_desc, extent_desc, name)
                yield extentindex, name, align, desc

class ExtentDemo(gtk.Widget):
    def __init__(self, text="""Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor.\n\tسلامی چو بوی خوشِ آشنایی... بر آن ملّتِ دیرام‌دام دیرام..."""):
        gtk.Widget.__init__(self)
        self.text = "foo"
        self.text = text
	self.all_modes = list(generate_modes())
	self.mode_num = 0;

	self.x_margin = 5
	self.y_margin = 5
	self.x_offset = 0
	self.y_offset = 25
	self.font_size = 36

    def do_realize(self):
        self.set_flags(self.flags() | gtk.REALIZED)

        self.window = gtk.gdk.Window(
            self.get_parent_window(),
            width=self.allocation.width,
            height=self.allocation.height,
            window_type=gtk.gdk.WINDOW_CHILD,
            wclass=gtk.gdk.INPUT_OUTPUT,
            event_mask=self.get_events() | gtk.gdk.EXPOSURE_MASK)

        self.window.set_user_data(self)

        self.style.attach(self.window)

        self.style.set_background(self.window, gtk.STATE_NORMAL)
        self.window.move_resize(*self.allocation)

    def do_unrealize(self):
        self.window.destroy()

    def do_size_request(self, requisition):

	width = 800

	layout = self.get_layout(self.get_pango_context())
	layout.set_width (pango.SCALE * (width     - (self.x_offset + 2 * self.x_margin)))
	height = layout.get_pixel_extents ()[1][3] + (self.y_offset + 2 * self.y_margin)

        requisition.width  = width
        requisition.height = height

    def do_expose_event(self, event):
        context = self.window.cairo_create()
        context.rectangle(event.area.x, event.area.y,
                          event.area.width, event.area.height)
        context.clip()
        pangocontext = self.get_pango_context()

        self.draw(context, pangocontext)

        return False


    def get_layout (self, pangocontext):
        font = pango.FontDescription()
        font.set_family("sans")
        font.set_size(self.font_size * pango.SCALE)

        layout = pango.Layout(pangocontext)
        layout.set_font_description(font)
        layout.set_text(self.text)

	return layout

    def draw(self, context, pangocontext):

    	context.set_source_rgb (1, 1, 1)
	context.paint()
    	context.set_source_rgb (0, 0, 0)

	context.translate (self.x_margin, self.y_margin)

        extentindex, name, align, desc = self.all_modes[self.mode_num]

        labellayout = pango.Layout(pangocontext)
        labellayout.set_text('%i: %s' % (self.mode_num + 1, desc))
        context.move_to(0, 0)
        context.show_layout(labellayout)


	context.translate (self.x_offset, self.y_offset)

	layout = self.get_layout (pangocontext)
        width  = self.allocation.width  - (self.x_offset + 2 * self.x_margin)
        layout.set_width(width * pango.SCALE)
        layout.set_alignment(align)

        context.move_to(0, 0)

	#context.layout_path(layout)
	#context.fill()
        context.show_layout(layout)

        context.set_source_rgba(1, 0, 0, 0.5)
    	context.set_line_width (2)
        x, y, width, height = layout.get_pixel_extents()[extentindex]
        context.rectangle(x-1, y-1, width+2, height+2)
        context.stroke()

        context.set_source_rgba(0, 1, 0, 0.7)
    	context.set_line_width (1)

        li = layout.get_iter()

        while True:
            extents = getattr(li, 'get_%s_extents' % name)()
            if name != 'char':
                extents = extents[extentindex]
            x, y, width, height = self._descale(extents)
            context.rectangle(x+.5, y+.5, width-1, height-1)
            context.stroke()

            if not getattr(li, 'next_%s' % name)():
                break

    def cycle_mode_forward(self):
        self.mode_num += 1
        if self.mode_num >= len(self.all_modes):
            self.mode_num = 0
        self.queue_draw()

    def cycle_mode_backward(self):
        self.mode_num -= 1
        if self.mode_num < 0:
            self.mode_num = len(self.all_modes) - 1
        self.queue_draw()

    def key_press_event(self, widget, event):
        if event.string == ' ' or event.keyval == gtk.keysyms.Right:
            self.cycle_mode_forward()
        elif event.keyval == gtk.keysyms.BackSpace or event.keyval == gtk.keysyms.Left:
            self.cycle_mode_backward()
        elif event.string == 'q':
            gtk.main_quit()

    def _descale(self, rect):
        return (i / pango.SCALE for i in rect)

    def run(self):
        window = gtk.Window()
        window.add(self)
        window.connect("destroy", gtk.main_quit)
        window.connect("key-press-event", self.key_press_event)
        window.show_all()

        gtk.main()

gobject.type_register(ExtentDemo)


def main():
    if len (sys.argv) > 2:
        ed = ExtentDemo(sys.argv[2])
    else:
        ed = ExtentDemo()
    if len (sys.argv) > 1:
	mode = int(sys.argv[1])
	while mode > 1:
	    mode -= 1
	    ed.cycle_mode()
    ed.run()

if __name__ == "__main__":
    main()
