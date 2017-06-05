# Makefile for program source directory in GNU NLS utilities package.
# Copyright (C) 1995, 1996, 1997 by Ulrich Drepper <drepper@gnu.ai.mit.edu>
#
# This file file be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.
#
# - Modified by Owen Taylor <otaylor@redhat.com> to use GETTEXT_PACKAGE
#   instead of PACKAGE and to look for po2tbl in ./ not in intl/
#
# - Modified by jacob berkman <jacob@ximian.com> to install
#   Makefile.in.in and po2tbl.sed.in for use with glib-gettextize

GETTEXT_PACKAGE = gtk20
PACKAGE = gtk+
VERSION = 2.24.17

SHELL = /bin/sh


srcdir = .
top_srcdir = ..


prefix = /usr/local
exec_prefix = ${prefix}
datadir = ${datarootdir}
datarootdir = ${prefix}/share
libdir = ${exec_prefix}/lib
localedir = $(libdir)/locale
gnulocaledir = $(datadir)/locale
gettextsrcdir = $(datadir)/glib-2.0/gettext/po
subdir = po
install_sh = ${SHELL} /home/chris/gtk+-2.24.17/install-sh
mkdir_p = $(install_sh) -d

INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

CC = /usr/bin/gcc
GENCAT = @GENCAT@
GMSGFMT = /usr/local/bin/msgfmt
MSGFMT = /usr/local/bin/msgfmt
XGETTEXT = :
MSGMERGE = msgmerge

DEFS = -DHAVE_CONFIG_H
CFLAGS = -DGDK_PIXBUF_DISABLE_DEPRECATED -g -O2 -Wall
CPPFLAGS =  -DG_DISABLE_SINGLE_INCLUDES -DATK_DISABLE_SINGLE_INCLUDES -DGDK_PIXBUF_DISABLE_SINGLE_INCLUDES -DGTK_DISABLE_SINGLE_INCLUDES  

INCLUDES = -I.. -I$(top_srcdir)/intl

COMPILE = $(CC) -c $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(XCFLAGS)

SOURCES = 
POFILES =  af.po am.po ang.po ar.po as.po ast.po az.po az_IR.po be.po be@latin.po bg.po bn.po bn_IN.po br.po bs.po ca.po ca@valencia.po crh.po cs.po cy.po da.po de.po dz.po el.po en_CA.po en_GB.po eo.po es.po et.po eu.po fa.po fi.po fr.po ga.po gl.po gu.po he.po hi.po hr.po hu.po hy.po ia.po id.po io.po is.po it.po ja.po ka.po kk.po kn.po ko.po ku.po li.po lt.po lv.po mai.po mi.po mk.po ml.po mn.po mr.po ms.po my.po nb.po nds.po ne.po nl.po nn.po nso.po oc.po or.po pa.po pl.po ps.po pt.po pt_BR.po ro.po ru.po rw.po si.po sk.po sl.po sq.po sr.po sr@latin.po sr@ije.po sv.po ta.po te.po th.po tk.po tr.po tt.po ug.po uk.po ur.po uz.po uz@cyrillic.po vi.po wa.po xh.po yi.po zh_CN.po zh_HK.po zh_TW.po
GMOFILES =  af.gmo am.gmo ang.gmo ar.gmo as.gmo ast.gmo az.gmo az_IR.gmo be.gmo be@latin.gmo bg.gmo bn.gmo bn_IN.gmo br.gmo bs.gmo ca.gmo ca@valencia.gmo crh.gmo cs.gmo cy.gmo da.gmo de.gmo dz.gmo el.gmo en_CA.gmo en_GB.gmo eo.gmo es.gmo et.gmo eu.gmo fa.gmo fi.gmo fr.gmo ga.gmo gl.gmo gu.gmo he.gmo hi.gmo hr.gmo hu.gmo hy.gmo ia.gmo id.gmo io.gmo is.gmo it.gmo ja.gmo ka.gmo kk.gmo kn.gmo ko.gmo ku.gmo li.gmo lt.gmo lv.gmo mai.gmo mi.gmo mk.gmo ml.gmo mn.gmo mr.gmo ms.gmo my.gmo nb.gmo nds.gmo ne.gmo nl.gmo nn.gmo nso.gmo oc.gmo or.gmo pa.gmo pl.gmo ps.gmo pt.gmo pt_BR.gmo ro.gmo ru.gmo rw.gmo si.gmo sk.gmo sl.gmo sq.gmo sr.gmo sr@latin.gmo sr@ije.gmo sv.gmo ta.gmo te.gmo th.gmo tk.gmo tr.gmo tt.gmo ug.gmo uk.gmo ur.gmo uz.gmo uz@cyrillic.gmo vi.gmo wa.gmo xh.gmo yi.gmo zh_CN.gmo zh_HK.gmo zh_TW.gmo
DISTFILES = LINGUAS ChangeLog Makefile.in.in POTFILES.in POTFILES.skip $(GETTEXT_PACKAGE).pot \
$(POFILES) $(GMOFILES) $(SOURCES)

POTFILES = \
	../gdk/directfb/gdkdisplay-directfb.c \
	../gdk/gdk.c \
	../gdk/gdkdisplaymanager.c \
	../gdk/gdkpango.c \
	../gdk/gdkscreen.c \
	../gdk/gdkwindow.c \
	../gdk/keyname-table.h \
	../gdk/win32/gdkmain-win32.c \
	../gdk/x11/gdkmain-x11.c \
	../gdk/x11/gdkapplaunchcontext-x11.c \
	../gtk/gtkaboutdialog.c \
	../gtk/gtkaccelgroup.c \
	../gtk/gtkaccellabel.c \
	../gtk/gtkaccelmap.c \
	../gtk/gtkaccessible.c \
	../gtk/gtkaction.c \
	../gtk/gtkactiongroup.c \
	../gtk/gtkactivatable.c \
	../gtk/gtkadjustment.c \
	../gtk/gtkalignment.c \
	../gtk/gtkarrow.c \
	../gtk/gtkaspectframe.c \
	../gtk/gtkassistant.c \
	../gtk/gtkbbox.c \
	../gtk/gtkbin.c \
	../gtk/gtkbox.c \
	../gtk/gtkbuildable.c \
	../gtk/gtkbuilder.c \
	../gtk/gtkbuilderparser.c \
	../gtk/gtkbutton.c \
	../gtk/gtkcalendar.c \
	../gtk/gtkcelleditable.c \
	../gtk/gtkcelllayout.c \
	../gtk/gtkcellrendereraccel.c \
	../gtk/gtkcellrenderer.c \
	../gtk/gtkcellrenderercombo.c \
	../gtk/gtkcellrendererpixbuf.c \
	../gtk/gtkcellrendererprogress.c \
	../gtk/gtkcellrendererspin.c \
	../gtk/gtkcellrendererspinner.c \
	../gtk/gtkcellrenderertext.c \
	../gtk/gtkcellrenderertoggle.c \
	../gtk/gtkcellview.c \
	../gtk/gtkcheckbutton.c \
	../gtk/gtkcheckmenuitem.c \
	../gtk/gtkclipboard.c \
	../gtk/gtkclipboard-quartz.c \
	../gtk/gtkclist.c \
	../gtk/gtkcolorbutton.c \
	../gtk/gtkcolorsel.c \
	../gtk/gtkcolorseldialog.c \
	../gtk/gtkcombobox.c \
	../gtk/gtkcomboboxentry.c \
	../gtk/gtkcombo.c \
	../gtk/gtkcontainer.c \
	../gtk/gtkctree.c \
	../gtk/gtkcurve.c \
	../gtk/gtkcustompaperunixdialog.c \
	../gtk/gtkdialog.c \
	../gtk/gtkdnd.c \
	../gtk/gtkdnd-quartz.c \
	../gtk/gtkdrawingarea.c \
	../gtk/gtkeditable.c \
	../gtk/gtkentry.c \
	../gtk/gtkentrybuffer.c \
	../gtk/gtkentrycompletion.c \
	../gtk/gtkeventbox.c \
	../gtk/gtkexpander.c \
	../gtk/gtkfilechooserbutton.c \
	../gtk/gtkfilechooser.c \
	../gtk/gtkfilechooserdefault.c \
	../gtk/gtkfilechooserdialog.c \
	../gtk/gtkfilechooserembed.c \
	../gtk/gtkfilechooserentry.c \
	../gtk/gtkfilechoosersettings.c \
	../gtk/gtkfilechooserutils.c \
	../gtk/gtkfilechooserwidget.c \
	../gtk/gtkfilefilter.c \
	../gtk/gtkfilesel.c \
	../gtk/gtkfilesystem.c \
	../gtk/gtkfilesystemmodel.c \
	../gtk/gtkfixed.c \
	../gtk/gtkfontbutton.c \
	../gtk/gtkfontsel.c \
	../gtk/gtkframe.c \
	../gtk/gtkgamma.c \
	../gtk/gtkgc.c \
	../gtk/gtkhandlebox.c \
	../gtk/gtkhbbox.c \
	../gtk/gtkhbox.c \
	../gtk/gtkhpaned.c \
	../gtk/gtkhruler.c \
	../gtk/gtkhscale.c \
	../gtk/gtkhscrollbar.c \
	../gtk/gtkhseparator.c \
	../gtk/gtkhsv.c \
	../gtk/gtkiconfactory.c \
	../gtk/gtkicontheme.c \
	../gtk/gtkiconview.c \
	../gtk/gtkimage.c \
	../gtk/gtkimagemenuitem.c \
	../gtk/gtkimcontext.c \
	../gtk/gtkimcontextsimple.c \
	../gtk/gtkimmodule.c \
	../gtk/gtkimmulticontext.c \
	../gtk/gtkinfobar.c \
	../gtk/gtkinputdialog.c \
	../gtk/gtkinvisible.c \
	../gtk/gtkitem.c \
	../gtk/gtkitemfactory.c \
	../gtk/gtklabel.c \
	../gtk/gtklayout.c \
	../gtk/gtklinkbutton.c \
	../gtk/gtklist.c \
	../gtk/gtklistitem.c \
	../gtk/gtkliststore.c \
	../gtk/gtkmain.c \
	../gtk/gtkmenubar.c \
	../gtk/gtkmenu.c \
	../gtk/gtkmenuitem.c \
	../gtk/gtkmenushell.c \
	../gtk/gtkmenutoolbutton.c \
	../gtk/gtkmessagedialog.c \
	../gtk/gtkmisc.c \
	../gtk/gtkmodules.c \
	../gtk/gtkmountoperation.c \
	../gtk/gtkmountoperation-stub.c \
	../gtk/gtkmountoperation-x11.c \
	../gtk/gtknotebook.c \
	../gtk/gtkobject.c \
	../gtk/gtkoldeditable.c \
	../gtk/gtkoptionmenu.c \
	../gtk/gtkorientable.c \
	../gtk/gtkpagesetup.c \
	../gtk/gtkpagesetupunixdialog.c \
	../gtk/gtkpaned.c \
	../gtk/gtkpapersize.c \
	../gtk/gtkpathbar.c \
	../gtk/gtkpixmap.c \
	../gtk/gtkplug.c \
	../gtk/gtkpreview.c \
	../gtk/gtkprintbackend.c \
	../gtk/gtkprinter.c \
	../gtk/gtkprinteroptionwidget.c \
	../gtk/gtkprintjob.c \
	../gtk/gtkprintoperation.c \
	../gtk/gtkprintoperationpreview.c \
	../gtk/gtkprintoperation-unix.c \
	../gtk/gtkprintoperation-win32.c \
	../gtk/gtkprintunixdialog.c \
	../gtk/gtkprogressbar.c \
	../gtk/gtkprogress.c \
	../gtk/gtkradioaction.c \
	../gtk/gtkradiobutton.c \
	../gtk/gtkradiomenuitem.c \
	../gtk/gtkradiotoolbutton.c \
	../gtk/gtkrange.c \
	../gtk/gtkrc.c \
	../gtk/gtkrecentaction.c \
	../gtk/gtkrecentchooser.c \
	../gtk/gtkrecentchooserdefault.c \
	../gtk/gtkrecentchoosermenu.c \
	../gtk/gtkrecentmanager.c \
	../gtk/gtkruler.c \
	../gtk/gtkscalebutton.c \
	../gtk/gtkscale.c \
	../gtk/gtkscrollbar.c \
	../gtk/gtkscrolledwindow.c \
	../gtk/gtkselection.c \
	../gtk/gtkseparator.c \
	../gtk/gtkseparatortoolitem.c \
	../gtk/gtksettings.c \
	../gtk/gtksizegroup.c \
	../gtk/gtksocket.c \
	../gtk/gtkspinbutton.c \
	../gtk/gtkspinner.c \
	../gtk/gtkstatusbar.c \
	../gtk/gtkstatusicon.c \
	../gtk/gtkstock.c \
	../gtk/gtkstyle.c \
	../gtk/gtktable.c \
	../gtk/gtktearoffmenuitem.c \
	../gtk/gtktextbuffer.c \
	../gtk/gtktextbufferrichtext.c \
	../gtk/gtktextbufferserialize.c \
	../gtk/gtktext.c \
	../gtk/gtktextchild.c \
	../gtk/gtktextdisplay.c \
	../gtk/gtktextiter.c \
	../gtk/gtktextlayout.c \
	../gtk/gtktextmark.c \
	../gtk/gtktexttag.c \
	../gtk/gtktexttagtable.c \
	../gtk/gtktextutil.c \
	../gtk/gtktextview.c \
	../gtk/gtkthemes.c \
	../gtk/gtktipsquery.c \
	../gtk/gtktoggleaction.c \
	../gtk/gtktogglebutton.c \
	../gtk/gtktoggletoolbutton.c \
	../gtk/gtktoolbar.c \
	../gtk/gtktoolbutton.c \
	../gtk/gtktoolitem.c \
	../gtk/gtktoolitemgroup.c \
	../gtk/gtktoolpalette.c \
	../gtk/gtktoolshell.c \
	../gtk/gtktooltips.c \
	../gtk/gtktrayicon-x11.c \
	../gtk/gtktree.c \
	../gtk/gtktreednd.c \
	../gtk/gtktreeitem.c \
	../gtk/gtktreemodel.c \
	../gtk/gtktreemodelfilter.c \
	../gtk/gtktreemodelsort.c \
	../gtk/gtktreeselection.c \
	../gtk/gtktreesortable.c \
	../gtk/gtktreestore.c \
	../gtk/gtktreeview.c \
	../gtk/gtktreeviewcolumn.c \
	../gtk/gtktypeutils.c \
	../gtk/gtkuimanager.c \
	../gtk/gtkvbbox.c \
	../gtk/gtkvbox.c \
	../gtk/gtkviewport.c \
	../gtk/gtkvolumebutton.c \
	../gtk/gtkvpaned.c \
	../gtk/gtkvruler.c \
	../gtk/gtkvscale.c \
	../gtk/gtkvscrollbar.c \
	../gtk/gtkvseparator.c \
	../gtk/gtkwidget.c \
	../gtk/gtkwindow.c \
	../gtk/gtkwindow-decorate.c \
	../gtk/paper_names_offsets.c \
	../gtk/updateiconcache.c \
	../modules/input/gtkimcontextxim.c \
	../modules/input/imam-et.c \
	../modules/input/imcedilla.c \
	../modules/input/imcyrillic-translit.c \
	../modules/input/iminuktitut.c \
	../modules/input/imipa.c \
	../modules/input/immultipress.c \
	../modules/input/imthai.c \
	../modules/input/imti-er.c \
	../modules/input/imti-et.c \
	../modules/input/imviqr.c \
	../modules/input/imxim.c \
	../modules/printbackends/cups/gtkprintbackendcups.c \
	../modules/printbackends/file/gtkprintbackendfile.c \
	../modules/printbackends/lpr/gtkprintbackendlpr.c \
	../modules/printbackends/papi/gtkprintbackendpapi.c \
	../modules/printbackends/test/gtkprintbackendtest.c \
	../tests/testfilechooser.c

CATALOGS =  af.gmo am.gmo ang.gmo ar.gmo as.gmo ast.gmo az.gmo az_IR.gmo be.gmo be@latin.gmo bg.gmo bn.gmo bn_IN.gmo br.gmo bs.gmo ca.gmo ca@valencia.gmo crh.gmo cs.gmo cy.gmo da.gmo de.gmo dz.gmo el.gmo en_CA.gmo en_GB.gmo eo.gmo es.gmo et.gmo eu.gmo fa.gmo fi.gmo fr.gmo ga.gmo gl.gmo gu.gmo he.gmo hi.gmo hr.gmo hu.gmo hy.gmo ia.gmo id.gmo io.gmo is.gmo it.gmo ja.gmo ka.gmo kk.gmo kn.gmo ko.gmo ku.gmo li.gmo lt.gmo lv.gmo mai.gmo mi.gmo mk.gmo ml.gmo mn.gmo mr.gmo ms.gmo my.gmo nb.gmo nds.gmo ne.gmo nl.gmo nn.gmo nso.gmo oc.gmo or.gmo pa.gmo pl.gmo ps.gmo pt.gmo pt_BR.gmo ro.gmo ru.gmo rw.gmo si.gmo sk.gmo sl.gmo sq.gmo sr.gmo sr@latin.gmo sr@ije.gmo sv.gmo ta.gmo te.gmo th.gmo tk.gmo tr.gmo tt.gmo ug.gmo uk.gmo ur.gmo uz.gmo uz@cyrillic.gmo vi.gmo wa.gmo xh.gmo yi.gmo zh_CN.gmo zh_HK.gmo zh_TW.gmo
CATOBJEXT = .gmo
INSTOBJEXT = .mo

.SUFFIXES:
.SUFFIXES: .c .o .po .pox .gmo .mo .msg .cat

.c.o:
	$(COMPILE) $<

.po.pox:
	$(MAKE) $(GETTEXT_PACKAGE).pot
	$(MSGMERGE) $< $(srcdir)/$(GETTEXT_PACKAGE).pot -o $*.pox

.po.mo:
	$(MSGFMT) -o $@ $<

.po.gmo:
	file=$(srcdir)/`echo $* | sed 's,.*/,,'`.gmo \
	  && rm -f $$file && $(GMSGFMT) -c -o $$file $<

.po.cat:
	sed -f ../intl/po2msg.sed < $< > $*.msg \
	  && rm -f $@ && $(GENCAT) $@ $*.msg


all: all-yes

all-yes: $(CATALOGS)
all-no:

$(srcdir)/$(GETTEXT_PACKAGE).pot: $(POTFILES)
	$(XGETTEXT) --default-domain=$(GETTEXT_PACKAGE) --directory=$(top_srcdir) \
	  --add-comments --keyword=_ --keyword=N_ --keyword=C_:1c,2 --keyword=NC_:1c,2 \
	  --keyword=g_dngettext:2,3 \
	  --flag=g_dngettext:2:pass-c-format \
          --flag=g_strdup_printf:1:c-format \
          --flag=g_string_printf:2:c-format \
          --flag=g_string_append_printf:2:c-format \
          --flag=g_error_new:3:c-format \
          --flag=g_set_error:4:c-format \
          --flag=g_markup_printf_escaped:1:c-format \
          --flag=g_log:3:c-format \
          --flag=g_print:1:c-format \
          --flag=g_printerr:1:c-format \
          --flag=g_printf:1:c-format \
          --flag=g_fprintf:2:c-format \
          --flag=g_sprintf:2:c-format \
          --flag=g_snprintf:3:c-format \
          --flag=g_scanner_error:2:c-format \
          --flag=g_scanner_warn:2:c-format \
          --flag=gtk_message_dialog_format_secondary_markup:2:c-format \
          --flag=gtk_message_dialog_format_secondary_text:2:c-format \
          --flag=gtk_message_dialog_new:5:c-format \
          --flag=gtk_message_dialog_new_with_markup:5:c-format \
	  --files-from=$(srcdir)/POTFILES.in \
	&& test ! -f $(GETTEXT_PACKAGE).po \
	   || ( rm -f $(srcdir)/$(GETTEXT_PACKAGE).pot \
		&& mv $(GETTEXT_PACKAGE).po $(srcdir)/$(GETTEXT_PACKAGE).pot )

install: install-exec install-data
install-exec:
install-data: install-data-yes
install-data-no: all
install-data-yes: all
	$(mkdir_p) $(DESTDIR)$(datadir); \
	catalogs='$(CATALOGS)'; \
	for cat in $$catalogs; do \
	  cat=`basename $$cat`; \
	  case "$$cat" in \
	    *.gmo) destdir=$(gnulocaledir);; \
	    *)     destdir=$(localedir);; \
	  esac; \
	  lang=`echo $$cat | sed 's/\$(CATOBJEXT)$$//'`; \
	  dir=$(DESTDIR)$$destdir/$$lang/LC_MESSAGES; \
	  $(mkdir_p) $$dir; \
	  if test -r $$cat; then \
	    $(INSTALL_DATA) $$cat $$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT); \
	    echo "installing $$cat as $$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT)"; \
	  else \
	    $(INSTALL_DATA) $(srcdir)/$$cat $$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT); \
	    echo "installing $(srcdir)/$$cat as" \
		 "$$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT)"; \
	  fi; \
	  if test -r $$cat.m; then \
	    $(INSTALL_DATA) $$cat.m $$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT).m; \
	    echo "installing $$cat.m as $$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT).m"; \
	  else \
	    if test -r $(srcdir)/$$cat.m ; then \
	      $(INSTALL_DATA) $(srcdir)/$$cat.m \
		$$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT).m; \
	      echo "installing $(srcdir)/$$cat as" \
		   "$$dir/$(GETTEXT_PACKAGE)$(INSTOBJEXT).m"; \
	    else \
	      true; \
	    fi; \
	  fi; \
	done
	if test "$(PACKAGE)" = "glib"; then \
	  $(mkdir_p) $(DESTDIR)$(gettextsrcdir); \
	  $(INSTALL_DATA) $(srcdir)/Makefile.in.in \
			  $(DESTDIR)$(gettextsrcdir)/Makefile.in.in; \
	else \
	  : ; \
	fi

# Define this as empty until I found a useful application.
installcheck:

uninstall:
	catalogs='$(CATALOGS)'; \
	for cat in $$catalogs; do \
	  cat=`basename $$cat`; \
	  lang=`echo $$cat | sed 's/\$(CATOBJEXT)$$//'`; \
	  rm -f $(DESTDIR)$(localedir)/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE)$(INSTOBJEXT); \
	  rm -f $(DESTDIR)$(localedir)/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE)$(INSTOBJEXT).m; \
	  rm -f $(DESTDIR)$(gnulocaledir)/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE)$(INSTOBJEXT); \
	  rm -f $(DESTDIR)$(gnulocaledir)/$$lang/LC_MESSAGES/$(GETTEXT_PACKAGE)$(INSTOBJEXT).m; \
	done
	if test "$(PACKAGE)" = "glib"; then \
	  rm -f $(DESTDIR)$(gettextsrcdir)/Makefile.in.in; \
	fi

check: all

dvi info tags TAGS ID:

mostlyclean:
	rm -f core core.* *.pox $(GETTEXT_PACKAGE).po *.old.po cat-id-tbl.tmp
	rm -fr *.o

clean: mostlyclean

distclean: clean
	rm -f Makefile Makefile.in POTFILES *.mo *.msg *.cat *.cat.m

maintainer-clean: distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."
	rm -f $(GMOFILES)

distdir = ../$(GETTEXT_PACKAGE)-$(VERSION)/$(subdir)
dist distdir: $(DISTFILES)
	dists="$(DISTFILES)"; \
	for file in $$dists; do \
	  ln $(srcdir)/$$file $(distdir) 2> /dev/null \
	    || cp -p $(srcdir)/$$file $(distdir); \
	done

update-po: Makefile
	$(MAKE) $(GETTEXT_PACKAGE).pot
	tmpdir=`pwd`; \
	cd $(srcdir); \
	catalogs='$(CATALOGS)'; \
	for cat in $$catalogs; do \
	  cat=`basename $$cat`; \
	  lang=`echo $$cat | sed 's/\$(CATOBJEXT)$$//'`; \
	  echo "$$lang:"; \
	  if $(MSGMERGE) $$lang.po $(GETTEXT_PACKAGE).pot -o $$tmpdir/$$lang.new.po; then \
	    if cmp $$lang.po $$tmpdir/$$lang.new.po >/dev/null 2>&1; then \
	      rm -f $$tmpdir/$$lang.new.po; \
            else \
	      if mv -f $$tmpdir/$$lang.new.po $$lang.po; then \
	        :; \
	      else \
	        echo "msgmerge for $$lang.po failed: cannot move $$tmpdir/$$lang.new.po to $$lang.po" 1>&2; \
	        rm -f $$tmpdir/$$lang.new.po; \
	        exit 1; \
	      fi; \
	    fi; \
	  else \
	    echo "msgmerge for $$cat failed!"; \
	    rm -f $$tmpdir/$$lang.new.po; \
	  fi; \
	done

# POTFILES is created from POTFILES.in by stripping comments, empty lines
# and Intltool tags (enclosed in square brackets), and appending a full
# relative path to them
POTFILES: POTFILES.in
	( if test 'x$(srcdir)' != 'x.'; then \
	    posrcprefix='$(top_srcdir)/'; \
	  else \
	    posrcprefix="../"; \
	  fi; \
	  rm -f $@-t $@ \
	    && (sed -e '/^#/d' 						\
		    -e "s/^\[.*\] +//" 					\
		    -e '/^[ 	]*$$/d' 				\
		    -e "s@.*@	$$posrcprefix& \\\\@" < $(srcdir)/$@.in	\
		| sed -e '$$s/\\$$//') > $@-t \
	    && chmod a-w $@-t \
	    && mv $@-t $@ )

Makefile: Makefile.in.in ../config.status POTFILES
	cd .. \
	  && $(SHELL) ./config.status $(subdir)/$@.in

# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
