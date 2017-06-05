soundlistdir = $(sysconfdir)/sound/events

soundlist_in_files = \
	gnome-2.soundlist.in		\
	gtk-events-2.soundlist.in

soundlist_DATA = $(soundlist_in_files:.soundlist.in=.soundlist)

@INTLTOOL_SOUNDLIST_RULE@

EXTRA_DIST = $(soundlist_DATA)
