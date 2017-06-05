include src/config.mk

ifeq ($(LDFLAGS),)
	LDFLAGS = -Wl
endif

ifeq ($(LDLIBS),)
	LDLIBS = -L/usr/local/lib $(LIBS_VTE_L) -lintl -lglib-2.0 -lgobject-2.0 $(LIBS_VTE)
endif

CFLAGS += $(OPTFLAGS) $(VTEINC)

ivte: src/ivte.o
	$(CC) $(LDFLAGS) src/ivte.o $(LDLIBS) -o src/$(PROG)

src/ivte.o: src/ivte.h

prepare:
	rm -f src/custom.h src/ivte.h src/ivte.o

src/custom.h: prepare $(CONF_FILE)
	(echo '/* This file is generated - please edit $(CONF_FILE) instead */'; \
	grep -v '^ *//' $(CONF_FILE) \
	| sed 's/ CTRL_ALT / CTRL_ALT_FOO /g') > src/custom.h

src/ivte.h: src/custom.h
	sh src/process.sh src/$(PROG)

strip: ivte
	strip --remove-section=.comment --remove-section=.note src/$(PROG)

install:
	ldid -S src/ivte
	install -d $(bindir)
	install -m 755 src/$(PROG) src/showvte $(bindir)
	install -d $(ICON_DIR_INSTALL)
	install -m 644 misc/48.png $(ICON_DIR_INSTALL)/ivte.png
	install -d $(THEME_DIR)/hicolor/16x16/apps
	install -m 644 misc/16.png $(THEME_DIR)/hicolor/16x16/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/20x20/apps
	install -m 644 misc/20.png $(THEME_DIR)/hicolor/20x20/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/22x22/apps
	install -m 644 misc/22.png $(THEME_DIR)/hicolor/22x22/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/24x24/apps
	install -m 644 misc/24.png $(THEME_DIR)/hicolor/24x24/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/32x32/apps
	install -m 644 misc/32.png $(THEME_DIR)/hicolor/32x32/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/36x36/apps
	install -m 644 misc/36.png $(THEME_DIR)/hicolor/36x36/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/40x40/apps
	install -m 644 misc/40.png $(THEME_DIR)/hicolor/40x40/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/48x48/apps
	install -m 644 misc/48.png $(THEME_DIR)/hicolor/48x48/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/64x64/apps
	install -m 644 misc/64.png $(THEME_DIR)/hicolor/64x64/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/72x72/apps
	install -m 644 misc/72.png $(THEME_DIR)/hicolor/72x72/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/96x96/apps
	install -m 644 misc/96.png $(THEME_DIR)/hicolor/96x96/apps/ivte.png
	install -d $(THEME_DIR)/hicolor/scalable/apps
	install -m 644 misc/ivte.svg $(THEME_DIR)/hicolor/scalable/apps
	install -d $(mandir)
	install -m 644 misc/ivte.1 misc/showvte.1 $(mandir)
	install -d $(deskdir)
	install -m 644 misc/ivte.desktop $(deskdir)
	install -d $(GNOME_DEF_APP)
	install -m 644 misc/ivte.xml $(GNOME_DEF_APP)

install-strip: strip
	sh src/install.sh

uninstall:
	sh src/uninstall.sh

clean: src/config.mk
	rm -f src/$(PROG) src/showvte src/ivte.o src/ivte.h misc/ivte.? src/custom.h

distclean: clean
	rm -f src/*.o src/*.mk src/install.sh src/uninstall.sh

src/config.mk:
	./configure --quiet

.PHONY: ivte prepare strip install install-strip uninstall clean distclean
