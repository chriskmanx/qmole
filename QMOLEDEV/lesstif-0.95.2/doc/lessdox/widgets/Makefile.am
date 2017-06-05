#
# $Header: /cvsroot/lesstif/lesstif/doc/lessdox/widgets/Makefile.am,v 1.30 2005/03/26 06:52:25 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in
EXTRA_DIST = $(manone_DATA) $(manthree_DATA) $(manfive_DATA)
SUFFIXES=	.1 .3 .5 .html

manonedir=	@mandir@/man1
manthreedir=	@mandir@/man3
manfivedir=	@mandir@/man5

manone_DATA=
manthree_DATA= \
	XmClipboard.3 LessTifInternals.3 \
	XmArrowButton.3 XmArrowButtonGadget.3 XmBulletinBoard.3 \
	XmCascadeButton.3 XmCascadeButtonGadget.3 \
	XmComboBox.3 XmCommand.3 XmDesktop.3 \
	XmDialogShell.3 XmDialogShellExt.3 XmDisplay.3 \
	XmDragContext.3 XmDragIcon.3 XmDragOverShell.3 \
	XmDrawingArea.3 XmDrawnButton.3 XmDropSiteManager.3 \
	XmDropTransfer.3 XmFileSelectionBox.3 XmForm.3 \
	XmFrame.3 XmGadget.3 XmGrabShell.3 \
	XmIconGadget.3 XmLabel.3 \
	XmLabelGadget.3 XmList.3 \
	XmMainWindow.3 XmManager.3 XmMenuShell.3 \
	XmMessageBox.3 XmNotebook.3 XmPanedWindow.3 \
	XmPrimitive.3 XmProtocol.3 XmPushButton.3 \
	XmPushButtonGadget.3 XmRowColumn.3 \
	XmSash.3 XmScale.3 XmScreen.3 \
	XmScrollBar.3 XmScrolledWindow.3 XmSelectionBox.3 \
	XmSeparator.3 XmSeparatorGadget.3 \
	XmSpinBox.3 XmString.3 XmTearOffButton.3 XmText.3 \
	XmTextField.3 XmToggleButton.3 \
	XmToggleButtonGadget.3 XmVendorShell.3 XmWorld.3 \
	ApplicationShell.3 Composite.3 Core.3 Object.3 OverrideShell.3 \
	Shell.3 TopLevelShell.3 TransientShell.3 WmShell.3 \
	Rect.3 UnNamedObj.3 Constraint.3 VendorShell.3

manfive_DATA=

rootdir=	$(prefix)/LessTif
docdir=	$(rootdir)/doc
htmldir=	$(docdir)/html

html_DATA=	${manone_DATA:.1=.html} ${manthree_DATA:.3=.html} \
                ${manfive_DATA:.5=.html}

manone_HTML=	${manone_DATA:.1=.html}
manthree_HTML=	${manthree_DATA:.3=.html}
manfive_HTML=	${manfive_DATA:.5=.html}

CLEANFILES =	$(manone_HTML) $(manthree_HTML) $(manfive_HTML)

$(html_DATA): $(MAN2HTML)

.1.html:
	$(MAN2HTML) -f <$(srcdir)/$*.1 >$*.html

.3.html:
	$(MAN2HTML) -f <$(srcdir)/$*.3 >$*.html

.5.html:
	$(MAN2HTML) -f <$(srcdir)/$*.5 >$*.html

#
# This installs an extra symbolic link to make the HTML links work right
# in both the http://www.lesstif.org and the locally installed case.
#
install-data-hook:
	if [ "$(htmldir)" = "$(docdir)/html" ]; then \
	    if [ ! -h $(DESTDIR)$(docdir)/Lessdox ]; then \
	        mkdir -p $(DESTDIR)$(docdir) && \
	        cd $(DESTDIR)$(docdir) && \
	        $(LN_S) html Lessdox; \
	    fi; \
	else \
	    if [ ! -h $(DESTDIR)$(docdir)/Lessdox ]; then \
	        mkdir -p $(DESTDIR)$(docdir) && \
	        cd $(DESTDIR)$(docdir) && \
	        echo && \
	        echo 'WARNING:' && \
	        echo '  Creating a not-so-nice symlink!!!' && \
	        echo '  See Makefile for details.' && \
	        echo && \
	        $(LN_S) $(htmldir) Lessdox; \
	    fi; \
	fi
# This still is ugly.  It will not produce a "nice" relative symlink if
# "$(htmldir)" does not equal "$(docdir)/html".
#
# NOTE: $(htmldir) must be an absolute path, of course.  But hey ...
