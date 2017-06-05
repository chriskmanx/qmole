
gsettings_SCHEMAS = \
	org.gnome.crypto.cache.gschema.xml \
	org.gnome.crypto.pgp.gschema.xml

@GSETTINGS_RULES@

convert_DATA = \
	org.gnome.crypto.cache.convert \
	org.gnome.crypto.pgp.convert

convertdir = \
	$(datarootdir)/GConf/gsettings

EXTRA_DIST = \
	$(convert_DATA) \
	$(gsettings_SCHEMAS)