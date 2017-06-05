if MANUAL_PS
EXTRA_DIST=claws-mail-manual.ps
manualde_DATA=claws-mail-manual.ps
manualdedir=$(manualdir)/pl

claws-mail-manual.ps: \
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
	-docbook2ps -o . ../../claws-mail-manual.xml
	
maintainer-clean:
	-rm claws-mail-manual.ps
endif