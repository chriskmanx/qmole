#
#
# Makefile for K&R examples in Chapter 1
#
#

######

ATSUSRQ="$(ATSHOME)"
ifeq ($(ATSUSRQ),"")
ATSUSRQ="/usr"
endif

######

ATSCC=$(ATSUSRQ)/bin/atscc
ATSOPT=$(ATSUSRQ)/bin/atsopt

######

.PHONY: all
all:: checkall
all:: cleanall

######

checkall::
cleanall:: clean
cleanall:: ; $(RMF) *_?ats.html 

######

checkall:: hello
cleanall:: ; $(RMF) hello
hello: hello.dats
	$(ATSCC) -o hello hello.dats

checkall:: fahrenheit_celsius
cleanall:: ; $(RMF) fahrenheit_celsius
fahrenheit_celsius: fahrenheit_celsius.dats
	$(ATSCC) -o fahrenheit_celsius fahrenheit_celsius.dats

checkall:: echo
cleanall:: ; $(RMF) echo
echo: echo.dats ; $(ATSCC) -o echo echo.dats

checkall:: charcnt
cleanall:: ; $(RMF) charcnt
charcnt: charcnt.dats ; $(ATSCC) -o charcnt charcnt.dats

checkall:: linecnt
cleanall:: ; $(RMF) linecnt
linecnt: linecnt.dats ; $(ATSCC) -o linecnt linecnt.dats

checkall:: wc
cleanall:: ; $(RMF) wc
wc: wc.dats ; $(ATSCC) -o wc wc.dats

checkall:: dsocnt
cleanall:: ; $(RMF) dsocnt
dsocnt: digit_space_other_cnt.dats
	$(ATSCC) -o dsocnt digit_space_other_cnt.dats

checkall:: power
cleanall:: ; $(RMF) power
power: power.dats ; $(ATSCC) -o power power.dats

checkall:: longest_line
cleanall:: ; $(RMF) longest_line
longest_line: longest_line.dats
	$(ATSCC) -o longest_line longest_line.dats

checkall:: longest_line_alt
cleanall:: ; $(RMF) longest_line_alt
longest_line_alt: longest_line_alt.dats
	$(ATSCC) -o longest_line_alt longest_line_alt.dats

######

html:: ; $(ATSOPT) --posmark_html -d hello.dats > hello_dats.html
html:: ; $(ATSOPT) --posmark_html -d fahrenheit_celsius.dats > fahrenheit_celsius_dats.html
html:: ; $(ATSOPT) --posmark_html -d echo.dats > echo_dats.html
html:: ; $(ATSOPT) --posmark_html -d charcnt.dats > charcnt_dats.html
html:: ; $(ATSOPT) --posmark_html -d linecnt.dats > linecnt_dats.html
html:: ; $(ATSOPT) --posmark_html -d wc.dats > wc_dats.html
html:: ; $(ATSOPT) --posmark_html -d digit_space_other_cnt.dats > digit_space_other_dats.html
html:: ; $(ATSOPT) --posmark_html -d power.dats > power_dats.html
html:: ; $(ATSOPT) --posmark_html -d longest_line.dats > longest_line_dats.html
html:: ; $(ATSOPT) --posmark_html -d longest_line_alt.dats > longest_line_alt_dats.html

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o

###### end of [Makefile] ######
