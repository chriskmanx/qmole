cursordir = @cursordir@/redglass/cursors

all: Makefile.cursors

Makefile.cursors:
	$(top_srcdir)/genmakefile.sh $(srcdir)/Makefile.cfg

if REDGLASS
include Makefile.cursors
endif
