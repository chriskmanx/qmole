noinst_DATA = \
	extension-gjs.js	\
	extension-gjs.plugin	\
	extension-seed.js	\
	extension-seed.plugin

extension-js.%:
	test -e $@

%.js: extension-js.js
	$(AM_V_GEN) $(LN_S) $< $@

%.plugin: extension-js.plugin
	$(AM_V_GEN) cp $< $@ && \
	$(SED) -i -e 's/JS_LOADER/$(@:extension-%.plugin=%)/' $@

EXTRA_DIST = \
	extension-js.js 	\
	extension-js.plugin

CLEANFILES = $(noinst_DATA)
DISTCLEANFILES = $(noinst_DATA)
