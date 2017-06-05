TESTS = check.sh

LANGUAGES =			\
	ada.lang		\
	asp.lang		\
	awk.lang		\
	bibtex.lang		\
	boo.lang		\
	changelog.lang		\
	c.lang			\
	cg.lang			\
	chdr.lang		\
	cmake.lang		\
	cpp.lang		\
	csharp.lang		\
	css.lang		\
	cuda.lang		\
	def.lang		\
	desktop.lang		\
	diff.lang		\
	d.lang			\
	docbook.lang		\
	dosbatch.lang		\
	dot.lang		\
	dpatch.lang		\
	dtd.lang		\
	eiffel.lang		\
	erlang.lang		\
	fcl.lang		\
	forth.lang		\
	fortran.lang		\
	fsharp.lang		\
	gap.lang		\
	glsl.lang		\
	gtk-doc.lang		\
	gtkrc.lang		\
	haddock.lang		\
	haskell.lang		\
	haskell-literate.lang	\
	html.lang		\
	idl.lang		\
	ini.lang		\
	java.lang		\
	javascript.lang		\
	latex.lang		\
	libtool.lang		\
	lua.lang		\
	m4.lang			\
	makefile.lang		\
	mallard.lang		\
	msil.lang		\
	nemerle.lang		\
	nsis.lang		\
	objc.lang		\
	ocaml.lang		\
	ocl.lang		\
	octave.lang		\
	ooc.lang		\
	pascal.lang		\
	perl.lang		\
	php.lang		\
	pkgconfig.lang		\
	po.lang			\
	python.lang		\
	prolog.lang		\
	R.lang			\
	rpmspec.lang		\
	ruby.lang		\
	scheme.lang		\
	sh.lang			\
	sparql.lang		\
	sql.lang		\
	styles.rng		\
	t2t.lang		\
	tcl.lang		\
	texinfo.lang		\
	vala.lang		\
	vbnet.lang		\
	verilog.lang		\
	vhdl.lang		\
	xml.lang		\
	xslt.lang		\
	yacc.lang

STYLES =			\
	classic.xml		\
	cobalt.xml		\
	kate.xml		\
	oblivion.xml		\
	tango.xml

languagesdir = $(datadir)/gtksourceview-2.0/language-specs
languages_DATA = $(LANGUAGES) language.rng language2.rng language.dtd \
		 check.sh convert.py

stylesdir = $(datadir)/gtksourceview-2.0/styles
styles_DATA = $(STYLES) styles.rng

EXTRA_DIST = $(languages_DATA) $(styles_DATA)

-include $(top_srcdir)/git.mk
