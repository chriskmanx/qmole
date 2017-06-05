## $XTermId: Makefile.in,v 1.177 2011/04/22 08:51:33 tom Exp $
# -----------------------------------------------------------------------------
# this file is part of xterm
#
# Copyright 1997-2010,2011 by Thomas E. Dickey
#
#                         All Rights Reserved
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE ABOVE LISTED COPYRIGHT HOLDER(S) BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# Except as contained in this notice, the name(s) of the above copyright
# holders shall not be used in advertising or otherwise to promote the
# sale, use or other dealings in this Software without prior written
# authorization.
# -----------------------------------------------------------------------------

SHELL		= /bin/sh

#### Start of system configuration section. ####

srcdir		= .


x		= 
o		= .o

CC		= /usr/bin/gcc
CPP		= /usr/bin/gcc -E
AWK		= gawk
LINK		= $(CC) $(CFLAGS)

CTAGS		= ctags
ETAGS		= etags

LN_S		= ln -s
RM              = rm -f
LINT		= 

INSTALL		= /usr/bin/install -c
INSTALL_PROGRAM	= ${INSTALL}
INSTALL_SCRIPT	= ${INSTALL}
INSTALL_DATA	= ${INSTALL} -m 644
transform	= s,x,x,

EXTRA_CFLAGS	= 
EXTRA_CPPFLAGS	= 
EXTRA_LOADFLAGS	= 

CPPFLAGS	= -I. -I$(srcdir) -DHAVE_CONFIG_H   -D_DARWIN_C_SOURCE  -DFUNCPROTO=15 -DOSMAJORVERSION=10 -DOSMINORVERSION=4  -I/usr/local/include/freetype2 $(EXTRA_CPPFLAGS)
CFLAGS		= -g -O2 $(EXTRA_CFLAGS)
LDFLAGS		=  
LIBS		=  -L/usr/local/lib -lXft -lXrender -lfontconfig -lfreetype -lX11   -L/usr/local/lib -lXaw7 -lXmu -lXt -lX11 -lSM -lICE   -L/usr/local/lib -lXt -lX11 -lSM -lICE   -lncurses  

prefix		= /usr/local
exec_prefix	= ${prefix}

manext		= 1
bindir		= ${exec_prefix}/bin
libdir		= ${exec_prefix}/lib
mandir		= ${prefix}/man/man$(manext)
appsdir		= '$(exec_prefix)/lib/X11/app-defaults'
icondir		= '$(exec_prefix)/share/pixmaps'

#### End of system configuration section. ####

DESTDIR		=
BINDIR		= $(DESTDIR)$(bindir)
LIBDIR		= $(DESTDIR)$(libdir)
MANDIR		= $(DESTDIR)$(mandir)
APPSDIR		= $(DESTDIR)$(appsdir)
ICONDIR		= $(DESTDIR)$(icondir)

INSTALL_DIRS    = $(BINDIR) $(APPSDIR) $(ICONDIR) $(MANDIR)

CLASS		= XTerm
EXTRAHDR	=  Tekparse.h
EXTRASRC	=  TekPrsTbl.c Tekproc.c
EXTRAOBJ	=  TekPrsTbl.o Tekproc.o

          SRCS1 = button.c cachedGCs.c charproc.c charsets.c cursor.c \
	  	  data.c doublechr.c fontutils.c input.c \
		  linedata.c main.c menu.c misc.c \
		  print.c ptydata.c scrollback.c \
		  screen.c scrollbar.c tabs.c util.c xstrings.c xtermcap.c \
		  VTPrsTbl.c $(EXTRASRC)
          OBJS1 = button$o cachedGCs$o charproc$o charsets$o cursor$o \
	  	  data$o doublechr$o fontutils$o input$o \
		  linedata$o main$o menu$o misc$o \
		  print$o ptydata$o scrollback$o \
		  screen$o scrollbar$o tabs$o util$o xstrings$o xtermcap$o \
		  VTPrsTbl$o $(EXTRAOBJ)
          SRCS2 = resize.c xstrings.c
          OBJS2 = resize$o xstrings$o
           SRCS = $(SRCS1) $(SRCS2)
           OBJS = $(OBJS1) $(OBJS2)
           HDRS = VTparse.h data.h error.h main.h menu.h proto.h \
                  ptyx.h version.h xstrings.h xterm.h xtermcap.h $(EXTRAHDR)
       PROGRAMS = xterm$x resize$x

all :	$(PROGRAMS)
################################################################################
.SUFFIXES : .i .def .hin .html .$(manext) .ms .man .ps .pdf .txt

.c$o :
	
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $(srcdir)/$*.c

.c.i :
	
	$(CPP) -C $(CPPFLAGS) $*.c >$@

.def.hin :
	grep '^CASE_' $< | $(AWK) '{printf "#define %s %d\n", $$1, n++}' >$@

.man.$(manext) :
	$(SHELL) $(srcdir)/minstall.sh "$(INSTALL_DATA)" $< $@ $(appsdir)

.$(manext).html :
	GROFF_NO_SGR=stupid $(SHELL) -c "tbl $*.$(manext) | groff -Thtml -man" >$@

.$(manext).ps :
	$(SHELL) -c "tbl $*.$(manext) | groff -man" >$@

.$(manext).txt :
	GROFF_NO_SGR=stupid $(SHELL) -c "tbl $*.$(manext) | nroff -Tascii -man | col -bx" >$@

.ms.html :
	GROFF_NO_SGR=stupid $(SHELL) -c "tbl $*.ms | groff -Thtml -ms" >$@

.ms.ps :
	$(SHELL) -c "tbl $*.ms | groff -ms" >$@

.ms.txt :
	GROFF_NO_SGR=stupid $(SHELL) -c "tbl $*.ms | nroff -Tascii -ms | col -bx" >$@

.ps.pdf :
	ps2pdf $*.ps
################################################################################
main$o : main.h
misc$o : version.h

$(OBJS1) : xterm.h ptyx.h xtermcfg.h
main$o resize$o screen$o : xterm_io.h

xterm$x : $(OBJS1)
	$(SHELL) $(srcdir)/plink.sh $(LINK) $(LDFLAGS) -o $@ $(OBJS1) $(LIBS) $(EXTRA_LOADFLAGS)

resize$x : $(OBJS2)
	$(SHELL) $(srcdir)/plink.sh $(LINK) $(LDFLAGS) -o $@ $(OBJS2) $(LIBS)

256colres.h :
	-rm -f $@
	perl $(srcdir)/256colres.pl > $@

88colres.h :
	-rm -f $@
	perl $(srcdir)/88colres.pl > $@

charproc$o : main.h 
################################################################################
actual_xterm  = `echo xterm|        sed '$(transform)'`
actual_resize = `echo resize|       sed '$(transform)'`
actual_uxterm = `echo uxterm|       sed '$(transform)'`
actual_k8term = `echo koi8rxterm|   sed '$(transform)'`

binary_xterm  = $(actual_xterm)$x
binary_resize = $(actual_resize)$x
binary_uxterm = $(actual_uxterm)
binary_k8term = $(actual_k8term)

install \
install-bin \
install-full :: xterm$x resize$x $(BINDIR)
	$(SHELL) $(srcdir)/sinstall.sh  "$(INSTALL_PROGRAM)" xterm$x   $(BINDIR)/$(binary_xterm)
#	$(INSTALL_PROGRAM) xterm$x $(BINDIR)/$(binary_xterm)
	$(INSTALL_PROGRAM) -m  755 resize$x $(BINDIR)/$(binary_resize)

install \
install-bin \
install-scripts \
install-full ::
	@$(SHELL) -c "name=\"$(binary_xterm)\"; \
		dest=\"$(binary_uxterm)\"; \
		echo \"... installing $(BINDIR)/\$$dest\"; \
		if test \"\$$name\" != \"xterm\"; then \
			sed -e \"s,=xterm,=\$$name,\" $(srcdir)/uxterm >uxterm.tmp; \
			$(INSTALL_SCRIPT) -m  755 uxterm.tmp $(BINDIR)/\$$dest; \
			rm -f uxterm.tmp; \
		else \
			$(INSTALL_SCRIPT) -m  755 $(srcdir)/uxterm $(BINDIR)/\$$dest; \
		fi"
	@$(SHELL) -c "name=\"$(binary_xterm)\"; \
		dest=\"$(binary_k8term)\"; \
		echo \"... installing $(BINDIR)/\$$dest\"; \
		if test \"\$$name\" != \"xterm\"; then \
			sed -e \"s,=xterm,=\$$name,\" $(srcdir)/koi8rxterm >k8term.tmp; \
			$(INSTALL_SCRIPT) -m  755 k8term.tmp $(BINDIR)/\$$dest; \
			rm -f k8term.tmp; \
		else \
			$(INSTALL_SCRIPT) -m  755 $(srcdir)/koi8rxterm $(BINDIR)/\$$dest; \
		fi"
	@-$(SHELL) -c "name=\"$(binary_xterm)\"; \
		if test NONE != NONE ; then \
		cd $(BINDIR) && ( \
			rm -f NONE ; \
			$(LN_S) \$$name NONE ; \
			echo \"... created symbolic link:\" ; \
			ls -l \$$name NONE ) ; \
		fi"

install \
install-man \
install-full :: $(MANDIR)
	$(SHELL) $(srcdir)/minstall.sh "$(INSTALL_DATA)" $(srcdir)/xterm.man    $(MANDIR)/$(actual_xterm).$(manext)  $(appsdir)
	$(SHELL) $(srcdir)/minstall.sh "$(INSTALL_DATA)" $(srcdir)/resize.man   $(MANDIR)/$(actual_resize).$(manext) $(appsdir)
	$(SHELL) $(srcdir)/minstall.sh "$(INSTALL_DATA)" $(srcdir)/uxterm.man   $(MANDIR)/$(actual_uxterm).$(manext) $(appsdir)
	$(SHELL) $(srcdir)/minstall.sh "$(INSTALL_DATA)" $(srcdir)/koi8rxterm.man   $(MANDIR)/$(actual_k8term).$(manext) $(appsdir)
	@-$(SHELL) -c "if test NONE != NONE ; then cd $(MANDIR) && rm -f NONE.$(manext) ; fi"
	@-$(SHELL) -c "if test NONE != NONE ; then cd $(MANDIR) && $(LN_S) $(actual_xterm).$(manext) NONE.$(manext) ; fi"
	@-$(SHELL) -c "if test NONE != NONE ; then cd $(MANDIR) && echo '... created symbolic link:' && ls -l $(actual_xterm).$(manext) NONE.$(manext) ; fi"

APP_NAMES = XTerm UXTerm KOI8RXTerm

install \
install-app \
install-full :: $(APPSDIR)
	@-$(SHELL) -c 'for s in $(APP_NAMES); \
	do \
		echo "** $$s"; \
		d=`echo $$s | sed -e s/XTerm/$(CLASS)/`; \
		echo installing $(APPSDIR)/$$d; \
		sed -e s/XTerm/$(CLASS)/ $(srcdir)/$$s.ad >XTerm.tmp; \
		$(INSTALL_DATA) XTerm.tmp $(APPSDIR)/$$d; \
		echo installing $(APPSDIR)/$$d-color; \
		sed -e s/XTerm/$$d/ $(srcdir)/XTerm-col.ad >XTerm.tmp; \
		$(INSTALL_DATA) XTerm.tmp $(APPSDIR)/$$d-color; \
	done'
	@rm -f XTerm.tmp
	@echo "... installed app-defaults"

install \
install-icon \
install-full :: $(ICONDIR)
	@$(INSTALL_DATA) $(srcdir)/icons/xterm-color_32x32.xpm $(ICONDIR)
	@$(INSTALL_DATA) $(srcdir)/icons/xterm-color_48x48.xpm $(ICONDIR)
	@$(INSTALL_DATA) $(srcdir)/icons/xterm_32x32.xpm $(ICONDIR)
	@$(INSTALL_DATA) $(srcdir)/icons/xterm_48x48.xpm $(ICONDIR)
	@echo "... installed icons"

install ::
	@echo 'Completed installation of executables and documentation.'
	@echo 'Use "make install-ti" to install terminfo description.'

TERMINFO_DIR = $(DESTDIR)/usr/lib/terminfo
SET_TERMINFO = TERMINFO=$(TERMINFO_DIR)

install-full \
install-ti :: $(TERMINFO_DIR)
	@$(SHELL) -c "$(SET_TERMINFO) $(srcdir)/run-tic.sh $(srcdir)/terminfo"
	@echo 'Completed installation of terminfo description.'

install-full \
install-tc ::
	@-$(SHELL) -c "test -f /etc/termcap && echo 'You must install the termcap entry manually by editing /etc/termcap'"

installdirs : $(INSTALL_DIRS)
################################################################################
uninstall \
uninstall-bin \
uninstall-full ::
	-$(RM) $(BINDIR)/$(binary_xterm)
	-$(RM) $(BINDIR)/$(binary_resize)
	@-$(SHELL) -c "if test NONE != NONE ; then cd $(BINDIR) && rm -f NONE; fi"

uninstall \
uninstall-bin \
uninstall-scripts \
uninstall-full ::
	-$(RM) $(BINDIR)/$(binary_uxterm)
	-$(RM) $(BINDIR)/$(binary_k8term)

uninstall \
uninstall-man \
uninstall-full ::
	-$(RM) $(MANDIR)/$(actual_xterm).$(manext)
	-$(RM) $(MANDIR)/$(actual_resize).$(manext)
	-$(RM) $(MANDIR)/$(actual_uxterm).$(manext)
	-$(RM) $(MANDIR)/$(actual_k8term).$(manext)
	@-$(SHELL) -c "if test NONE != NONE ; then cd $(MANDIR) && rm -f NONE.$(manext); fi"

uninstall \
uninstall-app \
uninstall-full ::
	@-$(SHELL) -c 'for s in $(APP_NAMES); \
	do \
		echo "** $$s"; \
		d=`echo $$s | sed -e s/XTerm/$(CLASS)/`; \
		echo uninstalling $(APPSDIR)/$$d; \
		$(RM) $(APPSDIR)/$$d; \
		echo uninstalling $(APPSDIR)/$$d-color; \
		$(RM) $(APPSDIR)/$$d-color; \
	done'

uninstall \
uninstall-icon \
uninstall-full ::
	-$(RM) $(ICONDIR)/xterm-color_32x32.xpm
	-$(RM) $(ICONDIR)/xterm_32x32.xpm
	-$(RM) $(ICONDIR)/xterm-color_48x48.xpm
	-$(RM) $(ICONDIR)/xterm_48x48.xpm
################################################################################
# Desktop-utils does not provide an uninstall, and is not uniformly available.
#DESKTOP_FLAGS = 
#install-desktop \
#install-full ::
#	$(SHELL) df-install $(DESKTOP_FLAGS) $(srcdir)/xterm.desktop
#	$(SHELL) df-install $(DESKTOP_FLAGS) $(srcdir)/uxterm.desktop
################################################################################
mostlyclean :
	-$(RM) *$o *.[is] XtermLog.* .pure core *~ *.bak *.BAK *.out *.tmp

clean : mostlyclean
	-$(RM) $(PROGRAMS)

distclean :: clean
	-$(RM) Makefile config.status config.cache config.log xtermcfg.h
#	-$(RM) df-install

distclean \
docs-clean ::
	-$(RM) *.ps *.pdf *.png
	-$(SHELL) -c 'for p in xterm resize uxterm koi8rxterm; \
	do \
		$(RM) $$p.html $$p.$(manext) $$p.txt; \
	done'
	-$(RM) ctlseqs.html ctlseqs.$(manext)

realclean : distclean
	-$(RM) tags TAGS

maintainer-clean : realclean
	-$(RM) 256colres.h 88colres.h
################################################################################
terminfo.out : terminfo		; tic -a -I -1 terminfo >$@
termcap.out : termcap		; tic -a -C -U termcap >$@
################################################################################
docs-ctlseqs \
docs :: ctlseqs.html \
	ctlseqs.pdf \
	ctlseqs.ps \
	ctlseqs.txt
ctlseqs.html : ctlseqs.ms
ctlseqs.pdf : ctlseqs.ps
ctlseqs.ps : ctlseqs.ms
ctlseqs.txt : ctlseqs.ms
################################################################################
docs-resize \
docs :: resize.html \
	resize.pdf \
	resize.ps \
	resize.txt
resize.html : resize.$(manext)
resize.pdf : resize.ps
resize.ps : resize.$(manext)
resize.txt : resize.$(manext)
################################################################################
docs-xterm \
docs :: xterm.html \
	xterm.pdf \
	xterm.ps \
	xterm.txt
xterm.html : xterm.$(manext)
xterm.pdf : xterm.ps
xterm.ps : xterm.$(manext)
xterm.txt : xterm.$(manext)
################################################################################
docs-uxterm \
docs :: uxterm.html \
	uxterm.pdf \
	uxterm.ps \
	uxterm.txt
uxterm.html : uxterm.$(manext)
uxterm.pdf : uxterm.ps
uxterm.ps : uxterm.$(manext)
uxterm.txt : uxterm.$(manext)
################################################################################
docs-koi8rxterm \
docs :: koi8rxterm.html \
	koi8rxterm.pdf \
	koi8rxterm.ps \
	koi8rxterm.txt
koi8rxterm.html : koi8rxterm.$(manext)
koi8rxterm.pdf : koi8rxterm.ps
koi8rxterm.ps : koi8rxterm.$(manext)
koi8rxterm.txt : koi8rxterm.$(manext)
################################################################################
lint :
	$(LINT) $(CPPFLAGS) $(SRCS1)
	$(LINT) $(CPPFLAGS) $(SRCS2)

tags :
	$(CTAGS) $(SRCS) $(HDRS)

TAGS :
	$(ETAGS) $(SRCS) $(HDRS)

$(TERMINFO_DIR) $(INSTALL_DIRS) :
	mkdir -p $@

ALWAYS :

depend : $(TABLES)
	makedepend -- $(CPPFLAGS) -- $(SRCS)

# DO NOT DELETE THIS LINE -- make depend depends on it.
