if BUILD_DEMO_PLUGIN
demo_dir = demo
endif

if BUILD_SPAMASSASSIN_PLUGIN
spamassasssin_dir = spamassassin
endif

if BUILD_BOGOFILTER_PLUGIN
bogofilter_dir = bogofilter
endif

if BUILD_DILLO_VIEWER_PLUGIN
dillo_viewer_dir = dillo_viewer
endif

if BUILD_TRAYICON_PLUGIN
trayicon_dir = trayicon
endif

if BUILD_PGPCORE_PLUGIN
pgpcore_dir = pgpcore
if BUILD_PGPMIME_PLUGIN
pgpmime_dir = pgpmime
endif
if BUILD_PGPINLINE_PLUGIN
pgpinline_dir = pgpinline
endif
if BUILD_SMIME_PLUGIN
smime_dir = smime
endif
endif

SUBDIRS = $(demo_dir) \
	$(spamassasssin_dir) \
	$(bogofilter_dir) \
	$(dillo_viewer_dir) \
	$(trayicon_dir) \
	$(pgpcore_dir) \
	$(pgpmime_dir) \
	$(pgpinline_dir) \
	$(smime_dir)
