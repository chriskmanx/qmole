#
# $Header: /cvsroot/lesstif/lesstif/doc/lessdox/functions/Makefile.am,v 1.13 2005/03/26 06:52:25 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in
EXTRA_DIST = $(manone_DATA) $(manthree_DATA) $(manfive_DATA)
SUFFIXES=	.1 .3 .5 .html

manonedir=	@mandir@/man1
manthreedir=	@mandir@/man3
manfivedir=	@mandir@/man5

manone_DATA=
manthree_DATA=
manfive_DATA=


rootdir=	$(prefix)/LessTif
docdir=	$(rootdir)/doc
htmldir=	$(docdir)/html

html_DATA=	${manone_DATA:.1=.html} ${manthree_DATA:.3=.html} \
                ${manfive_DATA:.5=.html}

manone_HTML=	${manone_DATA:.1=.html}
manthree_HTML=	${manthree_DATA:.3=.html}
manfive_HTML=	${manfive_DATA:.5=.html}

CLEANFILES =	$(manone_HTML) $(manthree_HTML) $(manfive_HTML)

.1.html:
	$(MAN2HTML) -f <$(srcdir)/$*.1 >$*.html

.3.html:
	$(MAN2HTML) -f <$(srcdir)/$*.3 >$*.html

.5.html:
	$(MAN2HTML) -f <$(srcdir)/$*.5 >$*.html
