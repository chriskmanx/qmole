SUBDIRS = src csslint docs
SUBDIRS += tests

PKG_CONFIG_FILE_UNINSTALLED=libcroco-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@-uninstalled.pc
PKG_CONFIG_FILE=libcroco-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@.pc

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=$(PKG_CONFIG_FILE)

INCLUDE_DIR_NAME=@PACKAGE@-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@
extraincludedir=$(includedir)/$INCLUDE_DIR_NAME

CROCO_CONFIG_FILE_UNINSTALLED=croco-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@-config-uninstalled
CROCO_CONFIG_FILE=croco-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@-config
bin_SCRIPTS=$(CROCO_CONFIG_FILE)

#man_MANS = croco-config.1

EXTRA_DIST= croco-config.in $(PKG_CONFIG_FILE) TODO Doxyfile \
HACKING COPYING.LIB COPYING libcroco.spec libcroco-zip.in 

all-local: $(PKG_CONFIG_FILE_UNINSTALLED) $(PKG_CONFIG_FILE) \
$(CROCO_CONFIG_FILE_UNINSTALLED) $(CROCO_CONFIG_FILE)


%-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@.pc: %.pc
	cp $< $@

%-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@-uninstalled.pc: %.pc
	cp $< $@

%-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@-config: %-config
	cp $< $@

%-@LIBCROCO_MAJOR_VERSION@.@LIBCROCO_MINOR_VERSION@-config-uninstalled: %-config
	cp $< $@

clean-local: cleantest
distclean-local:
	-rm ./croco-0.6-config ./libcroco-0.6.pc ./libcroco-0.6-uninstalled.pc ./croco-0.6-config-uninstalled

cleantest:
	if test -x $(top_srcdir)/tests/testctl ; then $(top_srcdir)/tests/testctl cleanup ; fi

cleantar:
	@(rm -f libcroco*.tar.gz)
	@(rm -rf $(top_srcdir)/rpmbuildroot)

rpm: cleantar
	@(mkdir -p $(top_srcdir)/rpmbuildroot ; $(MAKE) dist && rpmbuild  -ta --buildroot $(top_srcdir)/rpmbuildroot $(distdir).tar.gz)

test:	all
	if test -x $(top_srcdir)/tests/testctl ; then $(top_srcdir)/tests/testctl cleanup ; $(top_srcdir)/tests/testctl run ; fi 

apidoc:
	if ! test -d docs/apis ; then  mkdir -p docs/apis ;  fi ;
	doxygen Doxyfile
