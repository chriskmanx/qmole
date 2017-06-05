#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import atk

win = gtk.Window()
win.connect('destroy', lambda win: gtk.main_quit())

button = gtk.Button(stock=gtk.STOCK_QUIT)
button.connect('pressed', lambda button: gtk.main_quit())
atk_button = button.get_accessible()
atk_button.set_description('Be careful, clicking this button will exit')

label = gtk.Label('This label describes a button')
atk_label = label.get_accessible()
atk_label.set_description('This is a useless label')
relation_set = atk_label.ref_relation_set()

relation = atk.Relation((atk_button,), atk.RELATION_LABEL_FOR)
relation_set.add(relation)

box = gtk.HBox()
box.pack_start(label)
box.pack_start(button)

win.add(box)
win.show_all()

gtk.main()
