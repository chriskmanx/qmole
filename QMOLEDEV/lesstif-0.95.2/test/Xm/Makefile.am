#
# $Header: /cvsroot/lesstif/lesstif/test/Xm/Makefile.am,v 1.20 2002/05/15 10:49:06 amai Exp $
#
MAINTAINERCLEANFILES=Makefile.in

SUBDIRS	= . \
      arrowbg arrowbutton baseclass bulletinboard cascadebutton \
	cascadebuttong command cutpaste dialogs \
	drag_drop drag_help drag_icon drag_move drag_pixmaps drag_simple \
	drawingarea drawnbutton dynamic_drag extobj filesb fonts \
	form frame geometry imagecache label labelg list \
	mainw menushell messagebox misc mwm panedw \
	protocols pushbg pushbutton region reptype resconvert resind \
	rowcolumn scale screen scrollbar scrolledwindow selectionbox \
	selections separator sepg tearoff text \
	textf togglebg togglebutton tracking traversal \
	vasimple vendor virtualkeys xmos xmstring 


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
