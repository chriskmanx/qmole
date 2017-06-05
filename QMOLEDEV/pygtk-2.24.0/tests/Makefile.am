EXTRA_DIST = $(tests) common.py runtests.py testmodule.py leak.glade

tests =						\
	test_actiongroup.py			\
	test_api.py				\
	test_bin.py				\
	test_button.py				\
	test_color.py				\
	test_container.py			\
	test_conversion.py			\
	test_dialog.py				\
	test_enum.py				\
	test_filechooserdialog.py		\
	test_gdk.py				\
	test_glade.py				\
	test_liststore.py			\
	test_pango.py				\
	test_plug.py				\
	test_radiobutton.py			\
	test_rectangle.py			\
	test_style.py				\
	test_textview.py			\
	test_treeview.py			\
	test_accel_closures.py

GTK_PY_FILES = __init__.py _lazyutils.py compat.py deprecation.py keysyms.py

check-local: $(top_srcdir)/gtk/__init__.py
	@if test "$(top_builddir)" != "$(top_srcdir)"; then \
	   for gtk_py in $(GTK_PY_FILES) ; do \
	     cp $(top_srcdir)/gtk/$${gtk_py} $(top_builddir)/gtk/; \
	   done \
	fi
	@$(PYTHON) $(srcdir)/runtests.py $(top_builddir) $(top_srcdir)
	@if test "$(top_builddir)" != "$(top_srcdir)"; then \
	   for gtk_py in $(GTK_PY_FILES) ; do \
	     rm -f $(top_builddir)/gtk/$${gtk_py}*; \
           done \
	fi
	@rm -fr *.pyc
