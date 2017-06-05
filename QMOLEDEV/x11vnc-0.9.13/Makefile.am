EXTRA_DIST=tightvnc-1.3dev5-vncviewer-alpha-cursor.patch RELEASE-NOTES README.LibVNCServer

if HAVE_SYSTEM_LIBVNCSERVER
SUBDIRS=x11vnc classes
DIST_SUBDIRS=x11vnc classes
else
SUBDIRS=libvncserver libvncclient x11vnc classes
DIST_SUBDIRS=libvncserver libvncclient x11vnc classes
endif

if WITH_X11VNC
X11VNC=x11vnc
endif

#SUBDIRS=libvncserver libvncclient x11vnc classes
#DIST_SUBDIRS=libvncserver libvncclient x11vnc classes


includedir=$(prefix)/include/rfb
#include_HEADERS=rfb.h rfbconfig.h rfbint.h rfbproto.h keysym.h rfbregion.h

if HAVE_SYSTEM_LIBVNCSERVER
else
include_HEADERS=rfb/rfb.h rfb/rfbconfig.h rfb/rfbint.h rfb/rfbproto.h \
	rfb/keysym.h rfb/rfbregion.h rfb/rfbclient.h
endif

$(PACKAGE)-$(VERSION).tar.gz: dist

if HAVE_RPM
# Rule to build RPM distribution package
rpm: $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE).spec
	cp $(PACKAGE)-$(VERSION).tar.gz @RPMSOURCEDIR@
	rpmbuild -ba $(PACKAGE).spec
endif

t:
	$(MAKE) -C test test

