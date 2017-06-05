## Derived from tests/testprint.c from gtk+ sources

import sys
import math

import pygtk
pygtk.require('2.0')
import cairo
import pango
import gtk

def request_page_setup(operation, context, page_nr, page_setup):
  ## Make the second page landscape mode a5
  if page_nr == 1:
      page_setup.set_orientation(gtk.PAGE_ORIENTATION_LANDSCAPE)
      page_setup.set_paper_size(gtk.PaperSize("iso_a5"))


def draw_page (operation, context, page_nr):
    cr = context.get_cairo()

    ## Draw a red rectangle, as wide as the paper (inside the margins)
    cr.set_source_rgb(1.0, 0, 0)
    cr.rectangle(0, 0, context.get_width(), 50)
  
    cr.fill()

    ## Draw some lines
    cr.move_to(20, 10)
    cr.line_to(40, 20)
    cr.arc(60, 60, 20, 0, math.pi)
    cr.line_to(80, 20)
  
    cr.set_source_rgb(0, 0, 0)
    cr.set_line_width(5)
    cr.set_line_cap(cairo.LINE_CAP_ROUND)
    cr.set_line_join(cairo.LINE_JOIN_ROUND)
  
    cr.stroke()

    ## Draw some text
    layout = context.create_layout()
    layout.set_text("Hello World! Printing is easy")
    layout.set_font_description(pango.FontDescription("sans 28"))

    cr.move_to(30, 20)
    cr.layout_path(layout)

    ## Font Outline
    cr.set_source_rgb(0.93, 1.0, 0.47)
    cr.set_line_width(0.5)
    cr.stroke_preserve()

    ## Font Fill
    cr.set_source_rgb(0, 0.0, 1.0)
    cr.fill()
  

def main(argv):
  ## Test some random drawing, with per-page paper settings
  print_ = gtk.PrintOperation()
  print_.set_nr_of_pages(2)
  print_.set_unit(gtk.UNIT_MM)
  print_.set_pdf_target("test.pdf")
  print_.connect("draw_page", draw_page)
  print_.connect("request_page_setup", request_page_setup)
  res = print_.run()
  return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
