pygtklibdir = $(libdir)/pygtk/2.0
pygtklib_PYTHON = pygtk-demo/pygtk-demo.py
demodir = $(pygtklibdir)/demos
demoimgdir = $(demodir)/images

bin_SCRIPTS = pygtk-demo/pygtk-demo
pygtk-demo/pygtk-demo: pygtk-demo/pygtk-demo.in
	@mkdir -p pygtk-demo
	@sed -e s!\@PYTHON\@!@PYTHON@! \
	     -e s!\@libdir\@!@libdir@! \
		< $< > $@
	@chmod a+x $@

CLEANFILES = pygtk-demo/pygtk-demo

EXTRA_DIST = \
	pygtk-demo/pygtk-demo.in\
	atk/atk-demo.py		\
	gobject/editable-interface.py \
	gobject/properties.py	\
	gobject/signal.py	\
	gtk/application.py		\
	gtk/bin.py		\
	gtk/customtreemodel.py	\
	gtk/filechooser.py	\
	gtk/scrollable.py	\
	gtk/sizegroup.py	\
	gtk/uimanager.py	\
	gtk/widget.py		\
	gtk/testprint.py	\
	glade/README		\
	glade/test.glade	\
	glade/autoconnect.py	\
	glade/test2.glade	\
	glade/glade-demo.py	\
	ide/README		\
	ide/break.xpm		\
	ide/browse.py		\
	ide/continue.xpm	\
	ide/edit.py		\
	ide/edit.xpm		\
	ide/gtkcons.py		\
	ide/gtkdb.py		\
	ide/gtkprof.py		\
	ide/minibreak.xpm	\
	ide/next.xpm		\
	ide/pyide.py		\
	ide/quit.xpm		\
	ide/return.xpm		\
	ide/run.xpm		\
	ide/step.xpm		\
	ide/dialogs.py		\
	pango/utf8-demo.py	\
	pango/pangocairo-simple.py \
	pango/extentdemo.py	\
	simple/README		\
	simple/dnd.py		\
	simple/dndpixmap.py	\
	simple/hello.py		\
	simple/scribble.py	\
	simple/simple.py	\
	simple/tooltip.py


demo_PYTHON = \
	pygtk-demo/demos/__init__.py		\
	pygtk-demo/demos/appwindow.py		\
	pygtk-demo/demos/buttonbox.py		\
	pygtk-demo/demos/changedisplay.py	\
	pygtk-demo/demos/colorsel.py		\
	pygtk-demo/demos/dialogs.py		\
	pygtk-demo/demos/dnd.py			\
	pygtk-demo/demos/dndpixmap.py		\
	pygtk-demo/demos/editable_cells.py	\
	pygtk-demo/demos/entry_completion.py	\
	pygtk-demo/demos/expander.py		\
	pygtk-demo/demos/hypertext.py		\
	pygtk-demo/demos/images.py		\
	pygtk-demo/demos/list_store.py		\
	pygtk-demo/demos/menu.py		\
	pygtk-demo/demos/panes.py		\
	pygtk-demo/demos/pixbufs.py		\
	pygtk-demo/demos/print_editor.py	\
	pygtk-demo/demos/sizegroup.py		\
	pygtk-demo/demos/stock_browser.py	\
	pygtk-demo/demos/textscroll.py		\
	pygtk-demo/demos/textview.py		\
	pygtk-demo/demos/tooltip.py		\
	pygtk-demo/demos/tree_store.py		\
	pygtk-demo/demos/treemodel.py		\
	pygtk-demo/demos/statusicon.py		\
	pygtk-demo/demos/ui_manager.py		\
	pygtk-demo/demos/infobar.py

demoimg_DATA = \
	pygtk-demo/demos/images/alphatest.png		\
	pygtk-demo/demos/images/apple-red.png		\
	pygtk-demo/demos/images/background.jpg		\
	pygtk-demo/demos/images/floppybuddy.gif		\
	pygtk-demo/demos/images/gnome-applets.png	\
	pygtk-demo/demos/images/gnome-calendar.png	\
	pygtk-demo/demos/images/gnome-foot.png		\
	pygtk-demo/demos/images/gnome-gimp.png		\
	pygtk-demo/demos/images/gnome-gmush.png		\
	pygtk-demo/demos/images/gnome-gsame.png		\
	pygtk-demo/demos/images/gnu-keys.png		\
	pygtk-demo/demos/images/gtk-logo-rgb.gif

EXTRA_DIST += $(demoimg_DATA)
