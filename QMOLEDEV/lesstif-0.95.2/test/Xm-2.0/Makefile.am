#
# $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/Makefile.am,v 1.15 2002/05/14 11:23:44 amai Exp $
#
MAINTAINERCLEANFILES=Makefile.in

SUBDIRS	= . combobox container draw grabshell icong messages notebook \
                rendition spinbox translations trait xme

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
	    local_target="$$target"; \
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
