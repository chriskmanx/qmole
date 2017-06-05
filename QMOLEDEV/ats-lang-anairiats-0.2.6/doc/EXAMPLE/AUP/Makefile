######
#
# Makefile file for some examples translated from the book:
# Advanced UNIX Programming (2nd edition) by Marc Rochkind
#
######

#
# Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
# Start Time: Summer, 2009
# Second Time: Fall, 2010
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
all: checkall

######

checkall: compall cleanall

######

compall:: testlock
testlock: AUP_2_4_3.dats; $(ATSCC) -o $@ $^

compall:: backward
backward: AUP_2_13.dats
	$(ATSCC) -o $@ $^ && ./$@ "AUP_2_13.dats"

compall:: backward2
backward2: AUP_2_14.dats
	$(ATSCC) -o $@ $^ && ./$@ "AUP_2_14.dats"

#######

compall:: pathnames
pathnames: AUP_3_4.dats; $(ATSCC) -o $@ $^ && ./$@

compall:: longls
longls: AUP_3_5.dats; $(ATSCC) -o $@ $^ && ./$@

compall:: getcwdx
getcwdx: AUP_3_6_4.dats; $(ATSCC) -o $@ $^ && ./getcwdx

#######

compall:: test421
test421: AUP_4_2_1.dats; $(ATSCC) -o $@ $^

compall:: test4221
test4221: AUP_4_2_2_1.dats; $(ATSCC) -o $@ $^

compall:: test4222
test4222: AUP_4_2_2_2.dats; $(ATSCC) -o $@ $^

compall:: test423
test423: AUP_4_2_3.dats; $(ATSCC) -o $@ $^

compall:: test435
test435: AUP_4_3_5.dats; $(ATSCC) -o $@ $^

compall:: test47
test47: AUP_4_7.dats; $(ATSCC) -o $@ $^ && ./test47

compall:: test48
test48: AUP_4_8.dats; $(ATSCC) -o $@ $^ -lcurses

#######

compall:: test52
test52: AUP_5_2.dats; $(ATSCC) -o $@ $^ && ./test52

#######

getargs.o: utils/getargs.c
	$(ATSCC) -Wall -c -o $@ $<

compall:: aupsh1
aupsh1: AUP_5_4.dats getargs.o
	$(ATSCC) -o $@ $< getargs.o

compall:: aupsh2
aupsh2: AUP_5_6.dats getargs.o
	$(ATSCC) -o $@ $< getargs.o

compall:: aupsh3
aupsh3: AUP_5_10.dats getargs.o
	$(ATSCC) -o $@ $< getargs.o

#######

compall:: test511
test511: AUP_5_11.dats
	$(ATSCC) -Wall -o $@ AUP_5_11.dats \
     && ./test511

#######

compall:: aupnice
aupnice: AUP_5_15.dats
	$(ATSCC) -Wall -o $@ $<

#######

compall:: test5173
test5173: AUP_5_17_3.dats
	$(ATSCC) -D_ATS_MULTITHREAD -Wall -o $@ AUP_5_17_3.dats -lpthread \
     && ./test5173

#######

errinfo.o: utils/errinfo.sats utils/errinfo.dats
	$(ATSCC) -IATS utils -I utils -Wall -c -o $@ utils/errinfo.dats

compall:: test631
test631: AUP_6_3_1.dats errinfo.o
	$(ATSCC) -I utils -Wall -o $@ $< errinfo.o && ./test631

compall:: test65
test65: AUP_6_5.dats errinfo.o
	$(ATSCC) -I utils -Wall -o $@ $< errinfo.o && ./test65

#######

compall:: test811
test811: AUP_8_1_1.dats; $(ATSCC) -I utils -Wall -o $@ $< && ./test811

compall:: test813
test813: AUP_8_1_3.dats; $(ATSCC) -I utils -Wall -o $@ $<

compall:: test814
test814: AUP_8_1_4.dats; $(ATSCC) -I utils -Wall -o $@ $< && ./test814

compall:: test823
test823: AUP_8_2_3.dats; $(ATSCC) -I utils -Wall -o $@ $< && ./test823

compall:: test826
test826: AUP_8_2_6.dats; $(ATSCC) -I utils -Wall -o $@ $< && ./test826

compall:: test83
test83: AUP_8_3.dats; $(ATSCC) -D_XOPEN_SOURCE -Wall -o $@ $< && ./test83

compall:: test8811
test8811: AUP_8_8_1_1.dats; $(ATSCC) -Wall -o $@ $< && ./test8811

compall:: test8812
test8812: AUP_8_8_1_2.dats; $(ATSCC) -Wall -o $@ $< && ./test8812

#######

compall:: test911
test911: AUP_9_1_1.dats; $(ATSCC) -Wall -o $@ $<

compall:: test917
test917: AUP_9_1_7.dats; $(ATSCC) -Wall -o $@ $<

compall:: test923
test923: AUP_9_2_3.dats; $(ATSCC) -Wall -o $@ $< && ./test923

compall:: test971
test971: AUP_9_7_1.dats; $(ATSCC) -Wall -o $@ $<

compall:: test972
test972: AUP_9_7_2.dats; $(ATSCC) -Wall -o $@ $< && ./test972

compall:: test974
test974: AUP_9_7_4.dats; $(ATSCC) -Wall -o $@ $<

compall:: test975 # it needs [clock_gettime] and [clock_getres]
test975: AUP_9_7_5.dats; $(ATSCC) -Wall -o $@ $< -lrt && ./test975

#######

xref:: ; $(ATSOPT) --posmark_xref=XREF -d \
  $(ATSHOME)/utils/xref/ats_prelude_xref.dats > /dev/null
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_2_4_3.dats > AUP_2_4_3_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_2_13.dats > AUP_2_13_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_2_14.dats > AUP_2_14_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_3_4.dats > AUP_3_4_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_3_5.dats > AUP_3_5_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_3_6_4.dats > AUP_3_6_4_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_4_2_1.dats > AUP_4_2_1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_4_2_2_1.dats > AUP_4_2_2_1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_4_2_2_2.dats > AUP_4_2_2_2_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_4_2_3.dats > AUP_4_2_3_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_4_3_5.dats > AUP_4_3_5_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_4_7.dats > AUP_4_7_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_4_8.dats > AUP_4_8_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_5_2.dats > AUP_5_2_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_5_4.dats > AUP_5_4_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_5_6.dats > AUP_5_6_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_5_10.dats > AUP_5_10_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_5_11.dats > AUP_5_11_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_5_15.dats > AUP_5_15_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_5_17_3.dats > AUP_5_17_3_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_6_3_1.dats > AUP_6_3_1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_6_5.dats > AUP_6_5_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_8_1_1.dats > AUP_8_1_1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_8_1_3.dats > AUP_8_1_3_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_8_1_4.dats > AUP_8_1_4_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_8_2_3.dats > AUP_8_2_3_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_8_2_6.dats > AUP_8_2_6_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_8_3.dats > AUP_8_3_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_8_8_1_1.dats > AUP_8_8_1_1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_9_1_1.dats > AUP_9_1_1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_9_1_7.dats > AUP_9_1_7_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_9_2_3.dats > AUP_9_2_3_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_9_7_1.dats > AUP_9_7_1_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_9_7_2.dats > AUP_9_7_2_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_9_7_4.dats > AUP_9_7_4_dats.html
xref:: ; $(ATSOPT) --posmark_xref=XREF -d AUP_9_7_5.dats > AUP_9_7_5_dats.html

######

RMF = rm -f

#######

clean::
	$(RMF) *~
	$(RMF) *_?ats.c *_?ats.o
	$(RMF) MySocket
	$(RMF) errinfo.o
	$(RMF) getargs.o

cleanall:: clean
	$(RMF) ./*_?ats.html
	$(RMF) ./XREF/*.html
cleanall::
	$(RMF) testlock
	$(RMF) backward
	$(RMF) backward2
	$(RMF) pathnames
	$(RMF) longls
	$(RMF) getcwdx
cleanall::
	$(RMF) test421
	$(RMF) test4221
	$(RMF) test4222
	$(RMF) test423
	$(RMF) test435
	$(RMF) test47
	$(RMF) test48
cleanall::
	$(RMF) test52
	$(RMF) aupsh1
	$(RMF) aupsh2
	$(RMF) aupsh3
	$(RMF) test511
	$(RMF) aupnice
	$(RMF) test5173
cleanall::
	$(RMF) test631
	$(RMF) test65
cleanall::
	$(RMF) test811
	$(RMF) test813
	$(RMF) test814
	$(RMF) test823
	$(RMF) test826
	$(RMF) test83
	$(RMF) test8811
	$(RMF) test8812
cleanall::
	$(RMF) test911
	$(RMF) test917
	$(RMF) test923
	$(RMF) test971
	$(RMF) test972
	$(RMF) test974
	$(RMF) test975

###### end of [Makefile] ######
