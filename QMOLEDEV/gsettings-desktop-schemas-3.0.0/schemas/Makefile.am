desktop_gschemas_in_in =						\
	org.gnome.desktop.default-applications.gschema.xml.in.in	\
	org.gnome.desktop.interface.gschema.xml.in.in			\
	org.gnome.desktop.lockdown.gschema.xml.in.in			\
	org.gnome.system.proxy.gschema.xml.in.in			\
	org.gnome.desktop.sound.gschema.xml.in.in			\
	org.gnome.desktop.thumbnail-cache.gschema.xml.in.in		\
	org.gnome.desktop.a11y.keyboard.gschema.xml.in.in		\
	org.gnome.desktop.a11y.applications.gschema.xml.in.in		\
	org.gnome.desktop.a11y.magnifier.gschema.xml.in.in		\
	org.gnome.desktop.a11y.mouse.gschema.xml.in.in			\
	org.gnome.desktop.thumbnailers.gschema.xml.in.in		\
	org.gnome.desktop.session.gschema.xml.in.in			\
	org.gnome.desktop.background.gschema.xml.in.in			\
	org.gnome.desktop.media-handling.gschema.xml.in.in		\
	org.gnome.desktop.screensaver.gschema.xml.in.in

@INTLTOOL_XML_NOMERGE_RULE@

gsettings_ENUM_NAMESPACE = org.gnome.desktop
gsettings_ENUM_FILES = $(top_srcdir)/headers/*.h

desktop_gschemas_in = $(desktop_gschemas_in_in:.xml.in.in=.xml.in)
gsettings_SCHEMAS = $(desktop_gschemas_in:.xml.in=.xml)

%.gschema.xml.in: %.gschema.xml.in.in Makefile
	$(AM_V_GEN) $(SED) -e 's^\@datadir\@^$(datadir)^g' -e 's^\@GETTEXT_PACKAGE\@^$(GETTEXT_PACKAGE)^g' < $< > $@

@GSETTINGS_RULES@

convertdir=$(datadir)/GConf/gsettings
convert_DATA=gsettings-desktop-schemas.convert

EXTRA_DIST =				\
	$(desktop_gschemas_in_in)	\
	$(convert_DATA)

CLEANFILES =					\
	$(gsettings__enum_file)			\
	$(gsettings_SCHEMAS)			\
	$(desktop_gschemas_in)			\
	*.gschema.valid

-include $(top_srcdir)/git.mk
