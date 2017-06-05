# $Id: Makefile.am,v 1.6 2001/09/22 16:25:29 mej Exp $

EXTRA_HEADERS = array.h avl_tree.h dlinked_list.h iterator_if.h		\
linked_list.h list_if.h map_if.h mbuff.h obj.h objpair.h regexp.h	\
socket.h str.h sysdefs.h tok.h types.h url.h vector_if.h

install-exec-hook:
	$(mkinstalldirs) $(DESTDIR)$(includedir)/$(PACKAGE)
	for i in $(EXTRA_HEADERS) ; do \
	    $(INSTALL_DATA) $(srcdir)/$$i $(DESTDIR)$(includedir)/$(PACKAGE)/ ; \
	done

uninstall-hook:
	rm -rf $(DESTDIR)$(includedir)/$(PACKAGE)

EXTRA_DIST = $(EXTRA_HEADERS) sysdefs.h.in types.h.in

all: types.h sysdefs.h

types.h: types.h.in
	(cd $(top_srcdir) && ./config.status)

sysdefs.h: sysdefs.h.in
	(cd $(top_srcdir) && ./config.status)
MAINTAINERCLEANFILES = Makefile.in
