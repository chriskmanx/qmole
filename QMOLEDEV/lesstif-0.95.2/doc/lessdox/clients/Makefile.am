#
# $Header: /cvsroot/lesstif/lesstif/doc/lessdox/clients/Makefile.am,v 1.22 2005/03/26 06:52:25 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in
EXTRA_DIST = $(manone_DATA) $(manthree_DATA) $(manfive_DATA)
SUFFIXES=	.1 .3 .5 .html

manonedir=	@mandir@/man1
manthreedir=	@mandir@/man3
manfivedir=	@mandir@/man5

manone_DATA=	mwm.1 xmbind.1 lesstif.1 uil.1 ltversion.1
manthree_DATA=
manfive_DATA=	mwmrc.5 VirtualBindings.5


rootdir=	$(prefix)/LessTif
docdir=	$(rootdir)/doc
htmldir=	$(docdir)/html

html_DATA=	mwm.html mwmrc.html xmbind.html lesstif.html uil.html \
            VirtualBindings.html

manone_HTML=	${manone_DATA:.1=.html}
manthree_HTML=	${manthree_DATA:.3=.html}
manfive_HTML=	${manfive_DATA:.5=.html}

CLEANFILES =	$(manone_HTML) $(manthree_HTML) $(manfive_HTML)

# Do we really need this rule?!
$(html_DATA): $(MAN2HTML)

.1.html:
	$(MAN2HTML) -f <$(srcdir)/$*.1 >$*.html

.3.html:
	$(MAN2HTML) -f <$(srcdir)/$*.3 >$*.html

.5.html:
	$(MAN2HTML) -f <$(srcdir)/$*.5 >$*.html


mwmrc.html:	mwmrc.5
mwm.html:	mwm.1
lesstif.html:	lesstif.1
uil.html:	uil.1
VirtualBindings.html: VirtualBindings.5
