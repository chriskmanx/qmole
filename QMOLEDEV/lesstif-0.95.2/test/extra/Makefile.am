#
# $Header: /cvsroot/lesstif/lesstif/test/extra/Makefile.am,v 1.4 2002/05/15 11:12:23 amai Exp $
#
MAINTAINERCLEANFILES=Makefile.in

SUBDIRS	= \
      cmap daniel gl megabutton newtoggleb \
      smartmb spinbutton togglebplus Xlab


if MotifTests

motif-tests: motif-tests-recursive

motif-tests-recursive:
	@set fnord $(MAKEFLAGS); amf=$$2; \
	dot_seen=no; \
	target=`echo $@ | sed s/-recursive//`; \
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  echo "Making $$target in $$subdir"; \
	  if test "$$subdir" = "."; then \
	    dot_seen=yes; \
	    local_target="$$target-am"; \
	  else \
	    local_target="$$target"; \
	  fi; \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $$local_target) \
	   || case "$$amf" in *=*) exit 1;; *k*) fail=yes;; *) exit 1;; esac; \
	done;
endif

bin_SCRIPTS   = testall
testall: ../testall.in
	(cd .. && $(MAKE) testall)
	cp ../testall ./testall
	chmod 755 testall
