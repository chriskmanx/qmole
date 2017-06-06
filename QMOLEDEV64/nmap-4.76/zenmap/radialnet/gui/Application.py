# vim: set fileencoding=utf-8 :

# Copyright (C) 2008 Insecure.Com LLC.
#
# Author(s): João Paulo de Souza Medeiros <ignotus21@gmail.com>
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

from radialnet.util.integration import make_graph_from_nmap_parser
from radialnet.core.Info import INFO
from radialnet.core.XMLHandler import XMLReader
from radialnet.gui.ControlWidget import ControlWidget, ControlFisheye
from radialnet.gui.Toolbar import Toolbar
from radialnet.gui.Image import Pixmaps
from radialnet.gui.RadialNet import *
from radialnet.bestwidgets.windows import *
from radialnet.bestwidgets.boxes import *


DIMENSION = (640, 480)



class Application(BWMainWindow):
    """
    """
    def __init__(self):
        """
        """
        BWMainWindow.__init__(self)
        self.set_default_size(DIMENSION[0], DIMENSION[1])

        self.set_icon(Pixmaps().get_pixbuf('logo'))

        self.__create_widgets()


    def __create_widgets(self):
        """
        """
        self.__hbox = BWHBox(spacing=0)
        self.__vbox = BWVBox(spacing=0)

        self.__radialnet = RadialNet(LAYOUT_WEIGHTED)
        self.__control = ControlWidget(self.__radialnet)
        self.__fisheye = ControlFisheye(self.__radialnet)
        self.__toolbar = Toolbar(self.__radialnet,
                                        self,
                                        self.__control,
                                        self.__fisheye)
        self.__statusbar = BWStatusbar()

        self.__hbox.bw_pack_start_expand_fill(self.__radialnet)
        self.__hbox.bw_pack_start_noexpand_nofill(self.__control)

        self.__vbox.bw_pack_start_noexpand_nofill(self.__toolbar)
        self.__vbox.bw_pack_start_expand_fill(self.__hbox)
        self.__vbox.bw_pack_start_noexpand_nofill(self.__fisheye)
        self.__vbox.bw_pack_start_noexpand_nofill(self.__statusbar)

        self.add(self.__vbox)
        self.set_title(" ".join([INFO['name'], INFO['version']]))
        self.set_position(gtk.WIN_POS_CENTER)
        self.show_all()
        self.connect('destroy', gtk.main_quit)

        self.__radialnet.set_no_show_all(True)
        self.__control.set_no_show_all(True)
        self.__fisheye.set_no_show_all(True)

        self.__radialnet.hide()
        self.__control.hide()
        self.__fisheye.hide()
        self.__toolbar.disable_controls()


    def parse_nmap_xml_file(self, file):
        """
        """
        try:

            self.__parser = XMLReader(file)
            self.__parser.parse()

        except:

            text = 'It is not possible open file: %s.' % file

            alert = BWAlertDialog(self,
                                  primary_text='Error opening file.',
                                  secondary_text=text)

            alert.show_all()

            return False

        self.__radialnet.set_empty()
        self.__radialnet.set_graph(make_graph_from_nmap_parser(self.__parser))
        self.__radialnet.show()

        self.__toolbar.enable_controls()

        return True


    def start(self):
        """
        """
        gtk.main()
