SUBDIRS=FbTk
EXTRA_DIST=default.xpm
bin_PROGRAMS=fbdesk


fbdesk_SOURCES=FbDesk.hh FbDesk.cc Icon.hh Icon.cc main.cc \
	InputField.hh InputField.cc IntResMenuItem.hh IntResMenuItem.cc \
	IconConfig.hh IconConfig.cc


fbdesk_CXXFLAGS=-IFbTk
fbdesk_LDADD=FbTk/libFbTk.a

