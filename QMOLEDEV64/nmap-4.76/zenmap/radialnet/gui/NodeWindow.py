# vim: set fileencoding=utf-8 :

# Copyright (C) 2007, 2008 Insecure.Com LLC.
#
# Author: João Paulo de Souza Medeiros <ignotus21@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

import gtk
import pango

import radialnet.util.drawing as drawing

from radialnet.bestwidgets.windows import *
from radialnet.bestwidgets.boxes import *
from radialnet.bestwidgets.labels import *
from radialnet.gui.Image import Application
from radialnet.gui.NodeNotebook import NodeNotebook


DIMENSION_NORMAL = (600, 400)
DIMENSION_SHADED = (200, 24)



class NodeWindow(BWWindow):
    """
    """
    def __init__(self, node, position, parent):
        """
        """
        BWWindow.__init__(self, gtk.WINDOW_POPUP)
        self.set_decorated(False)
        self.set_keep_above(True)
        self.set_skip_taskbar_hint(True)
        self.move(position[0], position[1])
        self.set_size_request(DIMENSION_NORMAL[0], DIMENSION_NORMAL[1])

        self.__button_press_position = self.get_pointer()

        self.__is_compacted = True
        self.__is_collapsed = False

        self.__node = node
        self.__pressed = False
        self.__parent = parent

        self.connect('button_press_event', self.button_press)
        self.connect('button_release_event', self.button_release)
        self.connect('enter_notify_event', self.enter_notify)
        self.connect('leave_notify_event', self.leave_notify)
        self.connect('motion_notify_event', self.motion_notify)

        self.__title_font = pango.FontDescription('Monospace Bold')

        self.add_events(gtk.gdk.BUTTON_PRESS_MASK |
                        gtk.gdk.BUTTON_RELEASE_MASK |
                        gtk.gdk.POINTER_MOTION_MASK |
                        gtk.gdk.ENTER_NOTIFY |
                        gtk.gdk.LEAVE_NOTIFY |
                        gtk.gdk.POINTER_MOTION_HINT_MASK)

        self.__icon = Application()
        self.__create_widgets()


    def __create_widgets(self):
        """
        """
        self.__content = BWVBox()
        self.__head = BWHBox(spacing=2)

        self.__notebook = NodeNotebook(self.__node)

        # create head elements

        # icon with node's score color
        self.__color_box = gtk.EventBox()
        self.__color_image = gtk.Image()
        self.__color_image.set_from_file(self.__icon.get_icon('border'))
        self.__color_box.add(self.__color_image)
        self.__color_box.set_size_request(15, 15)
        r, g, b = drawing.cairo_to_gdk_color(self.__node.get_draw_info('color'))
        self.__color_box.modify_bg(gtk.STATE_NORMAL, gtk.gdk.Color(r, g, b))

        # title with the node ip and hostname
        self.__title = ""

        if self.__node.get_info('hostname') != None:
            self.__title = self.__node.get_info('hostname') + ' - '

        self.__title += self.__node.get_info('ip')

        self.__title_label = BWSectionLabel(self.__title)
        self.__title_label.modify_font(self.__title_font)

        # icon to collapse window
        self.__collapse_box = gtk.EventBox()
        self.__collapse_img = gtk.Image()
        self.__collapse_img.set_from_file(self.__icon.get_icon('collapse'))
        self.__collapse_box.add(self.__collapse_img)
        self.__collapse_box.connect('button_press_event', self.collapse_window)
        self.__collapse_box.add_events(gtk.gdk.BUTTON_PRESS_MASK)

        # icon to close window
        self.__close_box = gtk.EventBox()
        self.__close_img = gtk.Image()
        self.__close_img.set_from_file(self.__icon.get_icon('close'))
        self.__close_box.add(self.__close_img)
        self.__close_box.connect('button_press_event', self.close_window)
        self.__close_box.add_events(gtk.gdk.BUTTON_PRESS_MASK)

        # packing head elements
        self.__head.bw_pack_start_noexpand_nofill(self.__color_box)
        self.__head.bw_pack_start_expand_fill(self.__title_label)
        self.__head.bw_pack_start_noexpand_nofill(self.__collapse_box)
        self.__head.bw_pack_start_noexpand_nofill(self.__close_box)

        # packing all to content
        self.__content.bw_pack_start_noexpand_nofill(self.__head)
        self.__content.bw_pack_start_expand_fill(self.__notebook)

        # add context to window
        self.add(self.__content)


    def close_window(self, widget, event):
        """
        """
        self.__node.set_draw_info({'over':False})
        self.hide()
        self.__parent.queue_draw()

        return True


    def restore(self, x, y):
        """
        """
        self.move(x, y)
        self.show_all()

        self.__is_collapsed = False
        self.resize(DIMENSION_NORMAL[0], DIMENSION_NORMAL[1])
        self.__collapse_img.set_from_file(self.__icon.get_icon('collapse'))


    def collapse_window(self, widget, event):
        """
        """
        self.present()

        if self.__is_collapsed == False:

            self.__notebook.hide()
            self.__is_collapsed = True
            self.set_size_request(DIMENSION_SHADED[0], DIMENSION_SHADED[1])
            self.resize(DIMENSION_SHADED[0], DIMENSION_SHADED[1])
            self.__collapse_img.set_from_file(self.__icon.get_icon('expand'))

        else:

            self.__notebook.show()
            self.__is_collapsed = False
            self.set_size_request(DIMENSION_NORMAL[0], DIMENSION_NORMAL[1])
            self.resize(DIMENSION_NORMAL[0], DIMENSION_NORMAL[1])
            self.__collapse_img.set_from_file(self.__icon.get_icon('collapse'))

        return True


    def button_press(self, widget, event):
        """
        """
        self.present()
        self.__pressed = True
        self.__button_press_position = self.get_pointer()

        return True


    def button_release(self, widget, event):
        """
        """
        self.__pressed = False

        return True


    def enter_notify(self, widget, event):
        """
        """
        self.__node.set_draw_info({'over':True})
        self.__parent.queue_draw()


    def leave_notify(self, widget, event):
        """
        """
        self.set_keep_above(True)
        self.__node.set_draw_info({'over':False})


    def motion_notify(self, widget, event):
        """
        """
        self.__node.set_draw_info({'over':True})

        x, y, button_state = event.window.get_pointer()

        if button_state & gtk.gdk.BUTTON1_MASK and self.__pressed:

            xw, yw = event.window.get_root_origin()
            xd, yd = self.__button_press_position
            self.move(x + xw - xd, y + yw - yd)

        return True
