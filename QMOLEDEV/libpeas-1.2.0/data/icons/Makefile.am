icons16x16dir = $(datadir)/icons/hicolor/16x16/actions
icons22x22dir = $(datadir)/icons/hicolor/22x22/actions
icons32x32dir = $(datadir)/icons/hicolor/32x32/actions
scalableiconsdir = $(datadir)/icons/hicolor/scalable/actions

icons16x16_DATA = 16x16/libpeas-plugin.png
icons22x22_DATA = 22x22/libpeas-plugin.png
icons32x32_DATA = 32x32/libpeas-plugin.png
scalableicons_DATA = scalable/libpeas-plugin.svg


noinst_DATA = \
	16x16/libpeas-plugin.svg		\
	22x22/libpeas-plugin.svg		\
	32x32/libpeas-plugin.svg

EXTRA_DIST = \
	$(icons16x16_DATA)		\
	$(icons22x22_DATA)		\
	$(icons32x32_DATA)		\
	$(scalableicons_DATA)		\
	$(noinst_DATA)


gtk_update_icon_cache = gtk-update-icon-cache -f -t $(datadir)/icons/hicolor
install-data-hook: update-icon-cache
uninstall-hook: update-icon-cache
update-icon-cache:
	@-if test -z "$(DESTDIR)"; then \
		echo "Updating Gtk icon cache."; \
		$(gtk_update_icon_cache); \
	else \
		echo "*** Icon cache not updated.  After (un)install, run this:"; \
		echo "***   $(gtk_update_icon_cache)"; \
	fi
