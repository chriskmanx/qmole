if MANUAL_PDF
EXTRA_DIST=claws-mail-manual.pdf
manualde_DATA=claws-mail-manual.pdf
manualdedir=$(manualdir)/de

claws-mail-manual.pdf: \
	../../account.xml \
	../../ack.xml \
	../../addrbook.xml \
	../../advanced.xml \
	../../faq.xml \
	../../glossary.xml \
	../../gpl.xml \
	../../handling.xml \
	../../intro.xml \
	../../keyboard.xml \
	../../plugins.xml \
	../../starting.xml \
	../../claws-mail-manual.xml 
	-docbook2pdf -o . ../../claws-mail-manual.xml
	
maintainer-clean:
	-rm claws-mail-manual.pdf claws-mail-manual.fo claws-mail-manual.xsl
endif
