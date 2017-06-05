#!/usr/bin/env python
'''Pixbufs

A GdkPixbuf represents an image, normally in RGB or RGBA format.
Pixbufs are normally used to load files from disk and perform image scaling.
This demo is not all that educational, but looks cool. It was written by
Extreme Pixbuf Hacker Federico Mena Quintero. It also shows off how to use
GtkDrawingArea to do a simple animation.
Look at the Image demo for additional pixbuf usage examples.'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import os
import math

import pygtk
pygtk.require('2.0')
import gobject
import gtk

FRAME_DELAY = 50
CYCLE_LEN = 60
IMAGE_DIR = os.path.join(os.path.dirname(__file__), 'images')
BACKGROUND_NAME = "background.jpg"

image_names = [
    "apple-red.png",
    "gnome-applets.png",
    "gnome-calendar.png",
    "gnome-foot.png",
    "gnome-gmush.png",
    "gnome-gimp.png",
    "gnome-gsame.png",
    "gnu-keys.png"
]

class PixbufsDemo(gtk.Window):
    frame  = None      # frame of the background image
    background = None  # background-pixbuf
    images     = []    # list of pixbufs
    back_width  = 0    # width of background image
    back_height = 0    # height of background image
    timeout_id  = 0    # timeout id
    frame_num   = 0    # number of the current frame
    timeout_id = None

    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect("destroy", lambda *w: gtk.main_quit())
        self.connect("destroy", self.cleanup_callback)
        self.set_title(self.__class__.__name__)
        self.set_resizable(False)

        if not self.load_pixbufs():
            dialog = gtk.MessageDialog(self,
                gtk.DIALOG_DESTROY_WITH_PARENT,
                gtk.MESSAGE_ERROR,
                gtk.BUTTONS_CLOSE,
                "Failed to load an image")
            dialog.connect("response", lambda d, r: d.destroy())
            dialog.show()

        else:
            self.set_size_request(self.back_width, self.back_height)

            self.frame = gtk.gdk.Pixbuf(gtk.gdk.COLORSPACE_RGB, False, 8,
                self.back_width, self.back_height)

            da = gtk.DrawingArea()
            da.connect("expose_event", self.expose_cb)
            self.add(da)

            self.timeout_id = gobject.timeout_add(FRAME_DELAY, self.timeout)

            self.show_all()

    def load_pixbufs(self):
        ''' Loads the images for the demo and returns whether the
            operation succeeded.
        '''
        if self.background is not None:
            return True   # already loaded earlier

        # look in the the current directory where the file is installed
        try:
            self.background = gtk.gdk.pixbuf_new_from_file(
                os.path.join(IMAGE_DIR, BACKGROUND_NAME))
        except gobject.GError, error:
            return False

        self.back_width  = self.background.get_width()
        self.back_height = self.background.get_height()

        for filename in image_names:
            try:
                self.images.append(gtk.gdk.pixbuf_new_from_file(
                    os.path.join(IMAGE_DIR, filename)))
            except gobject.GError, error:
                return False

        return True

    def expose_cb(self, draw_area, event):
        ''' Expose callback for the drawing area. '''
        rowstride = self.frame.get_rowstride()

        # FIXME: what should be the result, string guchar an integer result?
        #pixels = frame.get_pixels() + rowstride * event.area.y + event.area.x * 3
        #pixels = frame.get_pixels()[len(frame.get_pixels()) + rowstride * event.area.y + event.area.x * 3]
        pixels = self.frame.get_pixels()

        draw_area.window.draw_rgb_image(
            draw_area.style.black_gc,
            event.area.x, event.area.y,
            event.area.width, event.area.height,
            'normal',
            pixels, rowstride,
            event.area.x, event.area.y)

        return True

    def cleanup_callback(self, win):
        if self.timeout_id is not None:
            gobject.source_remove(self.timeout_id)
            self.timeout_id = None

    def timeout(self):
        ''' Timeout handler to regenerate the frame. '''
        self.background.copy_area(0, 0, self.back_width, self.back_height,
            self.frame, 0, 0)

        f = float(self.frame_num % CYCLE_LEN) / float(CYCLE_LEN)

        xmid = self.back_width / 2.0
        ymid = self.back_height / 2.0

        radius = min(xmid, ymid) / 2.0

        N_IMAGES = len(image_names)
        for i_name in image_names:
            i = image_names.index(i_name)

            ang = 2.0 * math.pi * i / N_IMAGES - f * 2.0 * math.pi

            iw = self.images[i].get_width()
            ih = self.images[i].get_height()

            r = radius +(radius / 3.0) * math.sin(f * 2.0 * math.pi)

            xpos = math.floor(xmid + r * math.cos(ang) - iw / 2.0 + 0.5)
            ypos = math.floor(ymid + r * math.sin(ang) - ih / 2.0 + 0.5)

            if i % 2 == 0:
                k = math.cos(f * 2.0 * math.pi)
            else:
                k = math.sin(f * 2.0 * math.pi)
            k = 2.0 * k * k
            k = max(0.25, k)

            # satisfy the c-source
            r1 = gtk.gdk.Rectangle()
            r1.x = int(xpos)
            r1.y = int(ypos)
            r1.width  = iw * k
            r1.height = ih * k

            r2 = gtk.gdk.Rectangle()
            r2.x = 0
            r2.y = 0
            r2.width  = self.back_width
            r2.height = self.back_height

            dest = r1.intersect(r2)
            if dest is not None:
                if i % 2 == 0:
                    alpha = int(
                        max(127, math.fabs(255 * math.cos(f * 2.0 * math.pi))))
                else:
                    alpha = int(
                        max(127, math.fabs(255 * math.sin(f * 2.0 * math.pi))))
                self.images[i].composite(
                      self.frame,
                      dest.x, dest.y,
                      dest.width, dest.height,
                      xpos, ypos,
                      k, k,
                      gtk.gdk.INTERP_NEAREST,
                      alpha)

        if self is not None:
            self.queue_draw()

        self.frame_num += 1
        return True

def main():
    PixbufsDemo()
    gtk.main()

if __name__ == '__main__':
    main()
