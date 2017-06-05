#!/usr/bin/env python
'''Images

GtkImage is used to display an image; the image can be in a number of formats.
Typically, you load an image into a GdkPixbuf, then display the pixbuf.
This demo code shows some of the more obscure cases, in the simple case a call
to gtk_image_new_from_file() is all you need.
If you want to put image data in your program as a C variable, use the
make-inline-pixbuf program that comes with GTK+. This way you won't need to
depend on loading external files, your application binary can be self-contained.'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import os

import pygtk
pygtk.require('2.0')
import gobject
import gtk

IMAGEDIR = os.path.join(os.path.dirname(__file__), 'images')
ALPHA_IMAGE = os.path.join(IMAGEDIR, 'alphatest.png')
GTKLOGO_IMAGE = os.path.join(IMAGEDIR, 'gtk-logo-rgb.gif')
BUDDY_IMAGE = os.path.join(IMAGEDIR, 'floppybuddy.gif')

def progressive_prepared_callback(loader, image):
    pixbuf = loader.get_pixbuf()

    # Avoid displaying random memory contents, since the pixbuf
    # isn't filled in yet.
    #images.c -> gdk_pixbuf_fill(pixbuf, 0xaaaaaaff)
    pixbuf.fill(0)
    image.set_from_pixbuf(pixbuf)


def progressive_updated_callback(loader, x, y, width, height, image):
    ''' We know the pixbuf inside the GtkImage has changed, but the image
        itself doesn't know this; so queue a redraw.  If we wanted to be
        really efficient, we could use a drawing area or something
        instead of a GtkImage, so we could control the exact position of
        the pixbuf on the display, then we could queue a draw for only
        the updated area of the image.
    '''
    image.queue_draw()

class ImagesDemo(gtk.Window):
    pixbuf_loader = None
    load_timeout = None
    image_stream = None

    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.connect("destroy", self.cleanup_callback)
        self.set_title(self.__class__.__name__)
        self.set_border_width(8)

        vbox = gtk.VBox(False, 8)
        vbox.set_border_width(8)
        self.add(vbox)

        label = gtk.Label();
        label.set_markup("<u>Image loaded from a file</u>")
        vbox.pack_start(label, False, False, 0)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_IN)

        # The alignment keeps the frame from growing when users resize
        # the window
        align = gtk.Alignment(0.5, 0.5, 0, 0)
        align.add(frame)
        vbox.pack_start(align, False, False, 0)

        image = gtk.Image()

        # use the current directory for the file
        try:
            pixbuf = gtk.gdk.pixbuf_new_from_file(GTKLOGO_IMAGE)
            image.set_from_pixbuf(pixbuf)

        except gobject.GError, error:

            # This code shows off error handling. You can just use
            # gtk_image_new_from_file() instead if you don't want to report
            # errors to the user. If the file doesn't load when using
            # gtk_image_new_from_file(), a "missing image" icon will
            # be displayed instead.

            dialog = gtk.MessageDialog(self,
                gtk.DIALOG_DESTROY_WITH_PARENT,
                gtk.MESSAGE_ERROR,
                gtk.BUTTONS_CLOSE,
                "Unable to open image file 'gtk-logo-rgb.gif': \n%s" % error)

            dialog.connect("response", lambda dlg, resp: dlg.destroy())
            dialog.show()

        frame.add(image)

        # Animation

        label = gtk.Label()
        label.set_markup("<u>Animation loaded from a file</u>")
        vbox.pack_start(label, False, False, 0)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_IN)

        # The alignment keeps the frame from growing when users resize
        # the window

        align = gtk.Alignment(0.5, 0.5, 0, 0)
        align.add(frame)
        vbox.pack_start(align, False, False, 0)

        image = gtk.Image()
        image.set_from_file(BUDDY_IMAGE);

        frame.add(image)

        # Progressive

        label = gtk.Label()
        label.set_markup("<u>Progressive image loading</u>")
        vbox.pack_start(label, False, False, 0)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_IN)

        # The alignment keeps the frame from growing when users resize
        # the window

        align = gtk.Alignment(0.5, 0.5, 0, 0)
        align.add(frame)
        vbox.pack_start(align, False, False, 0)

        # Create an empty image for now; the progressive loader
        # will create the pixbuf and fill it in.

        image = gtk.Image()
        image.set_from_pixbuf(None)
        frame.add(image)

        self.start_progressive_loading(image)

        # Sensitivity control

        button = gtk.ToggleButton("_Insensitive");
        vbox.pack_start(button, False, False, 0)

        button.connect("toggled", self.on_sensitivity_toggled, vbox)

        self.show_all()

    def cleanup_callback(self, win):
        if self.load_timeout != 0:
            gtk.timeout_remove(self.load_timeout)
            self.load_timeout = 0

        if self.pixbuf_loader is not None:
            self.pixbuf_loader.close()
            self.pixbuf_loader = None

        if self.image_stream is not None:
            self.image_stream.close()
            self.image_stream = None


    def on_sensitivity_toggled(self, togglebutton, container):
        children = container.get_children()

        for child in children:

            # don't disable our toggle
            if type(child) != type(togglebutton):
                child.set_sensitive(not togglebutton.get_active())

    def start_progressive_loading(self, image):
        ''' This is obviously totally contrived(we slow down loading
            on purpose to show how incremental loading works).
            The real purpose of incremental loading is the case where
            you are reading data from a slow source such as the network.
            The timeout simply simulates a slow data source by inserting
            pauses in the reading process.
        '''
        self.load_timeout = gtk.timeout_add(150, self.progressive_timeout, image)

    def progressive_timeout(self, image):

        # This shows off fully-paranoid error handling, so looks scary.
        # You could factor out the error handling code into a nice separate
        # function to make things nicer.

        if self.image_stream is not None:    # file is already opened
            try:
                buf = self.image_stream.read(256)
                bytes_read = len(buf)

            except IOError, error:
                dialog = gtk.MessageDialog(self,
                    gtk.DIALOG_DESTROY_WITH_PARENT,
                    gtk.MESSAGE_ERROR,
                    gtk.BUTTONS_CLOSE,
                    "Failure reading image file 'alphatest.png': %s" % error)

                dialog.connect("response", lambda d, r: d.destroy())

                self.image_stream.close()
                self.image_stream = None

                dialog.show()

                self.load_timeout = 0

                return False; # uninstall the timeout

            if not self.pixbuf_loader.write(buf, bytes_read):

                dialog = gtk.MessageDialog(self,
                           gtk.DIALOG_DESTROY_WITH_PARENT,
                           gtk.MESSAGE_ERROR,
                           gtk.BUTTONS_CLOSE,
                           "Failed to load image")

                dialog.connect("response", lambda d, r: d.destroy())

                self.image_stream.close()
                self.image_stream = None

                dialog.show()

                self.load_timeout = 0

                return False # uninstall the timeout

            #if(feof(image_stream)):
            if bytes_read == 0:

                self.image_stream.close()
                self.image_stream = None

                # Errors can happen on close, e.g. if the image
                # file was truncated we'll know on close that
                # it was incomplete.

                if not self.pixbuf_loader.close():

                    dialog = gtk.MessageDialog(self,
                               gtk.DIALOG_DESTROY_WITH_PARENT,
                               gtk.MESSAGE_ERROR,
                               gtk.BUTTONS_CLOSE,
                               "Failed to load image")

                    dialog.connect("response", lambda d, r: d.destroy())
                    dialog.show()

                    self.pixbuf_loader = None

                    self.load_timeout = 0

                    return False # uninstall the timeout

                # if feof(image_stream)
                self.pixbuf_loader = None

        else:    # if(image_stream) ...
            try:
                self.image_stream = open(ALPHA_IMAGE, "rb")

            except IOError, error:
                error_message = "Unable to open image file 'alphatest.png' : %s"

                dialog = gtk.MessageDialog(self,
                    gtk.DIALOG_DESTROY_WITH_PARENT,
                    gtk.MESSAGE_ERROR,
                    gtk.BUTTONS_CLOSE,
                    error_message % error)

                dialog.connect("response", lambda d, r: d.destroy())
                dialog.show()

                self.load_timeout = 0

                return False # uninstall the timeout

            if self.pixbuf_loader is not None:
                self.pixbuf_loader.close()
                self.pixbuf_loader = None

            self.pixbuf_loader = gtk.gdk.PixbufLoader()

            self.pixbuf_loader.connect("area_prepared",
                progressive_prepared_callback, image)

            self.pixbuf_loader.connect("area_updated",
                progressive_updated_callback, image)

        # leave timeout installed
        return True;

def main():
    ImagesDemo()
    gtk.main()

if __name__ == '__main__':
    main()
