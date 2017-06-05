SUBDIRS = c

if ENABLE_GJS
SUBDIRS += gjs
endif

if ENABLE_PYTHON
SUBDIRS += python
endif

if ENABLE_SEED
SUBDIRS += seed
endif

gcov:
	@for dir in $(SUBDIRS); do \
	   (cd $$dir && $(MAKE) $(AM_MAKEFLAGS) $(@)); \
	 done
