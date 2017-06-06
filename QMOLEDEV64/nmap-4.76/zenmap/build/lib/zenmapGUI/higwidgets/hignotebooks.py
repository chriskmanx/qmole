#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
#         Cleber Rodrigues <cleber.gnu@gmail.com>
#
# This library is free software; you can redistribute it and/or modify 
# it under the terms of the GNU Lesser General Public License as published 
# by the Free Software Foundation; either version 2.1 of the License, or 
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public 
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License 
# along with this library; if not, write to the Free Software Foundation, 
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 

import gtk
import gobject

from higspinner import HIGSpinner
from higboxes import HIGHBox
from higbuttons import HIGButton


class HIGNotebook(gtk.Notebook):
	def __init__(self):
		gtk.Notebook.__init__(self)
		self.popup_enable()

class HIGClosableTabLabel(HIGHBox):
	__gsignals__ = { 'close-clicked' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, ()) }

	def __init__(self, label_text=""):
		gobject.GObject.__init__(self)
		#HIGHBox.__init__(self, spacing=4)

		self.label_text = label_text
		self.__create_widgets()

		#self.propery_map = {"label_text" : self.label.get_label}

	def __create_widgets(self):
		self.label = gtk.Label(self.label_text)
		self.close_image = gtk.Image()
		self.close_image.set_from_stock(gtk.STOCK_CLOSE, gtk.ICON_SIZE_BUTTON)
		self.close_button = HIGButton()
		self.close_button.set_size_request(20, 20)
		self.close_button.set_relief(gtk.RELIEF_NONE)
		self.close_button.set_focus_on_click(False)
		self.close_button.add(self.close_image)

		self.close_button.connect('clicked', self.__close_button_clicked)

		for w in (self.label, self.close_button):
			self.pack_start(w, False, False, 0)

		self.show_all()

		# 	def do_get_property(self, property):
		# 		func = self.property_map.get(property, None)
		# 		if func:
		# 			return func()
		# 		else:
		# 			raise 

	def __close_button_clicked(self, data):
		self.emit('close-clicked')
	
	def get_text(self):
		return self.label.get_text()
	
	def set_text(self, text):
		self.label.set_text(text)
	
	def get_label(self):
		return self.label.get_label()
	
	def set_label(self, label):
		self.label.set_text(label)

gobject.type_register(HIGClosableTabLabel)
		
HIGAnimatedTabLabel = HIGClosableTabLabel
