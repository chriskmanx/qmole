dist_gq_bin_SCRIPTS = geeqie-import

gq_desktopdir = $(appdir)/applications
gq_desktop_in_files = geeqie-import-geeqie.desktop.in geeqie-import-gqview.desktop.in
gq_desktop_DATA = $(gq_desktop_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

EXTRA_DIST = $(gq_desktop_DATA) $(gq_desktop_in_files)
