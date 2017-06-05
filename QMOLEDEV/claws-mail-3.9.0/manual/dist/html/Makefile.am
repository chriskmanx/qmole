if MANUAL_HTML
EXTRA_DIST=claws-mail-manual.html
manualde_DATA=claws-mail-manual.html
manualdedir=$(manualdir)/en

claws-mail-manual.html: \
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
	-env SP_ENCODING=xml docbook2html -u -o . \
		../../claws-mail-manual.xml
	-mv claws-mail-manual.html claws-mail-manual.html.raw
	-sed 's,^><META,><META http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"><META,' < claws-mail-manual.html.raw > claws-mail-manual.html
	
maintainer-clean:
	-rm claws-mail-manual.html
	-rm claws-mail-manual.html.raw
endif
