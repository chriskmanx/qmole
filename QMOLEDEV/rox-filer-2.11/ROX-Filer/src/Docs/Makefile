HELP=../../Help
RES=../..
MAN=../../..

TO_HTML = to_html.xsl

all: Manual.valid ${HELP}/Manual.html ${MAN}/rox.1 \
     Manual-fr.valid ${HELP}/Manual-fr.html \
     Manual-it.valid ${HELP}/Manual-it.html 

%.valid: %.xml
	xmllint --valid --noout --postvalid $<

${HELP}/%.html: %.xml $(TO_HTML)
	xsltproc -o $@ $(TO_HTML) $<

${MAN}/rox.1: Manual.xml to_man.xsl
	xsltproc -o $@ to_man.xsl $<
