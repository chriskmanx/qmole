cursordir = @cursordir@/whiteglass/cursors

all: Makefile.cursors

Makefile.cursors:
	$(top_srcdir)/genmakefile.sh $(srcdir)/Makefile.cfg > $@

if WHITEGLASS
include Makefile.cursors
endif
