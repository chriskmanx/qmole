#
#
# A simple (single threaded) HTTP server implemented in ATS
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
all:: checkall cleanall

checkall::
cleanall:: clean

######

#
# If on SunOS, please add [-lsocket]
#

checkall:: server
server: server.dats
	$(ATSCC) -D_ATS_GCATS -o server -O2 server.dats
cleanall:: ; $(RMF) server

checkall:: server2
server2: server2.dats
	$(ATSCC) -D_ATS_GCATS -o server2 -O2 server2.dats
cleanall:: ; $(RMF) server2

######

DIR=/home/fac2/hwxi/public_html/ATS/EXAMPLE/HttpServer

update::
	cp Makefile $(DIR)
	cp server.dats $(DIR)

######

RMF = rm -f

######

clean:
	$(RMF) *~
	$(RMF) *_dats.c *_sats.c

###### end of [Makefile] ######
