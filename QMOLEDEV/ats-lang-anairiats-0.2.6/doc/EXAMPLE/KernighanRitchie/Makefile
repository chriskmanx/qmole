#
#
# Makefile for some K&R examples
#
#

######

.PHONY: all
all:: checkall
all:: cleanall

######

checkall:: ; make -C Chapter01 checkall
checkall:: ; make -C Chapter02 checkall
checkall:: ; make -C Chapter03 checkall
checkall:: ; make -C Chapter04 checkall
checkall:: ; make -C Chapter05 checkall
checkall:: ; make -C Chapter06 checkall
checkall:: ; make -C Chapter07 checkall
checkall:: ; make -C Chapter08 checkall

######

cleanall:: ; make -C  Chapter01 cleanall
cleanall:: ; make -C  Chapter02 cleanall
cleanall:: ; make -C  Chapter03 cleanall
cleanall:: ; make -C  Chapter04 cleanall
cleanall:: ; make -C  Chapter05 cleanall
cleanall:: ; make -C  Chapter06 cleanall
cleanall:: ; make -C  Chapter07 cleanall
cleanall:: ; make -C  Chapter08 cleanall

######

html:: ; make -C Chapter01 html
html:: ; make -C Chapter02 html
html:: ; make -C Chapter03 html
html:: ; make -C Chapter04 html
html:: ; make -C Chapter05 html
html:: ; make -C Chapter06 html
html:: ; make -C Chapter07 html
html:: ; make -C Chapter08 html  

######

upload:: upload01
upload01:: ; scp Chapter01/*.dats Chapter01/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter01

upload:: upload02
upload02:: ; scp Chapter02/*.dats Chapter02/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter02

upload:: upload03
upload03:: ; scp Chapter03/*.dats Chapter03/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter03

upload:: upload04
upload04:: ; scp Chapter04/*.dats Chapter04/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter04

upload:: upload05
upload05:: ; scp Chapter05/*.dats Chapter05/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter05

upload:: upload06
upload06:: ; scp Chapter06/*.dats Chapter06/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter06

upload:: upload07
upload07:: ; scp Chapter07/*.dats Chapter07/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter07

upload:: upload08
upload08:: ; scp Chapter08/*.dats Chapter08/*_dats.html \
  ats-hwxi,ats-lang@web.sourceforge.net:htdocs/EXAMPLE/KernighanRitchie/Chapter08

###### end of [Makefile] ######
