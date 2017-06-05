#! /usr/bin/env python
import sys
import math
import cairo
import pygtk
pygtk.require('2.0')
import pango
import pangocairo

RADIUS = 150

def draw_text(cr):
    N_WORDS = 10
    FONT = "Sans Bold 27"

    # Center coordinates on the middle of the region we are drawing
    cr.translate(RADIUS, RADIUS);

    # Create a PangoLayout, set the font and text */
    layout = cr.create_layout()

    layout.set_text("Text")
    layout.set_font_description(pango.FontDescription(FONT))

    # Draw the layout N_WORDS times in a circle
    for i in range(N_WORDS):
        angle = (360 * i) / N_WORDS;
        cr.save()

        # Gradient from red at angle == 60 to blue at angle == 300
        red   = (1 + math.cos((angle - 60)*math.pi/180))/2
        cr.set_source_rgb(red, 0, 1 - red)
        cr.rotate(angle*math.pi/180)

        # Inform Pango to re-layout the text with the new transformation */
        cr.update_layout(layout)

        width, height = layout.get_size()
        cr.move_to(-width/pango.SCALE/2, -RADIUS)
        cr.show_layout(layout)

        cr.restore()

def main(argv):
    if len(argv) != 2:
        print >> sys.stderr, "Usage: cairosimple OUTPUT_BASENAME\n"
        return 1

    filename = argv[1]
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, 2*RADIUS, 2*RADIUS)
    cr = pangocairo.CairoContext(cairo.Context(surface))
    cr.set_source_rgb(1.0, 1.0, 1.0)
    cr.rectangle(0, 0, 2*RADIUS, 2*RADIUS)
    cr.fill()
    draw_text(cr)

    surface.write_to_png(filename + ".png")

    ## output also a PDF file
    surface = cairo.PDFSurface(filename + ".pdf", 2*RADIUS, 2*RADIUS)
    cr = pangocairo.CairoContext(cairo.Context(surface))
    draw_text(cr)
    cr.show_page()
    surface.finish()


if __name__ == '__main__':
    sys.exit(main(sys.argv))
