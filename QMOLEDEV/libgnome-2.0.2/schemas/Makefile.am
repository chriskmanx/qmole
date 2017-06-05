

schemadir   = $(GCONF_SCHEMA_FILE_DIR)
schema_in_files = \
	desktop_gnome_accessibility_keyboard.schemas.in		\
	desktop_gnome_applications_browser.schemas.in		\
	desktop_gnome_applications_help_viewer.schemas.in		\
	desktop_gnome_applications_terminal.schemas.in		\
	desktop_gnome_applications_window_manager.schemas.in	\
	desktop_gnome_file_views.schemas.in			\
	desktop_gnome_interface.schemas.in				\
	desktop_gnome_peripherals_mouse.schemas.in			\
	desktop_gnome_peripherals_keyboard.schemas.in		\
	desktop_gnome_sound.schemas.in                             \
	desktop_gnome_background.schemas.in			\
	desktop_gnome_url_handlers.schemas.in
schema_DATA = $(schema_in_files:.schemas.in=.schemas)

@INTLTOOL_SCHEMAS_RULE@

# don't do this if we are building in eg. rpm
install-data-local:
	if test -z "$(DESTDIR)" ; then \
		for p in $(schema_DATA) ; do \
			GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(srcdir)/$$p; \
		done \
	fi

EXTRA_DIST = $(schema_DATA)
