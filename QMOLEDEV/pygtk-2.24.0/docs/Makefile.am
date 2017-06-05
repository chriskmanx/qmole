BUILDDIR = $(top_builddir)/docs

REF_VERSION = $(PYGTK_MAJOR_VERSION).$(PYGTK_MINOR_VERSION)
FULL_VERSION = $(REF_VERSION).$(PYGTK_MICRO_VERSION)

HTML_STYLE = $(PYGOBJECT_DATADIR)/pygobject/xsl/ref-html-style.xsl
PDF_STYLE = $(PYGOBJECT_DATADIR)/pygobject/xsl/pdf-style.xsl

XMLFILES = 						\
	reference/pygtk2-ref.xml			\
	reference/pygtk-atkaction.xml			\
	reference/pygtk-atk-classes.xml			\
	reference/pygtk-atkcomponent.xml		\
	reference/pygtk-atk-constants.xml		\
	reference/pygtk-atkdocument.xml			\
	reference/pygtk-atkeditabletext.xml		\
	reference/pygtk-atkgobjectaccessible.xml	\
	reference/pygtk-atkhyperlink.xml		\
	reference/pygtk-atkhypertext.xml		\
	reference/pygtk-atkimage.xml			\
	reference/pygtk-atkimplementor.xml		\
	reference/pygtk-atknoopobjectfactory.xml	\
	reference/pygtk-atknoopobject.xml		\
	reference/pygtk-atkobjectfactory.xml		\
	reference/pygtk-atkobject.xml			\
	reference/pygtk-atkrectangle.xml		\
	reference/pygtk-atkregistry.xml			\
	reference/pygtk-atkrelationset.xml		\
	reference/pygtk-atkrelation.xml			\
	reference/pygtk-atkselection.xml		\
	reference/pygtk-atkstateset.xml			\
	reference/pygtk-atkstreamablecontent.xml	\
	reference/pygtk-atktable.xml			\
	reference/pygtk-atktext.xml			\
	reference/pygtk-atkvalue.xml			\
	reference/pygtk-gdkapplaunchcontext.xml		\
	reference/pygtk-gdkatom.xml			\
	reference/pygtk-gdkcairocontext.xml		\
	reference/pygtk-gdk-classes.xml			\
	reference/pygtk-gdkcolormap.xml			\
	reference/pygtk-gdkcolor.xml			\
	reference/pygtk-gdk-constants.xml		\
	reference/pygtk-gdkcursor.xml			\
	reference/pygtk-gdkdevice.xml			\
	reference/pygtk-gdkdisplaymanager.xml		\
	reference/pygtk-gdkdisplay.xml			\
	reference/pygtk-gdkdragcontext.xml		\
	reference/pygtk-gdkdrawable.xml			\
	reference/pygtk-gdkevent.xml			\
	reference/pygtk-gdkfont.xml			\
	reference/pygtk-gdk-functions.xml		\
	reference/pygtk-gdkgc.xml			\
	reference/pygtk-gdkimage.xml			\
	reference/pygtk-gdkkeymap.xml			\
	reference/pygtk-gdkpangorenderer.xml		\
	reference/pygtk-gdkpixbufanimationiter.xml	\
	reference/pygtk-gdkpixbufanimation.xml		\
	reference/pygtk-gdkpixbufloader.xml		\
	reference/pygtk-gdkpixbuf.xml			\
	reference/pygtk-gdkpixmap.xml			\
	reference/pygtk-gdkrectangle.xml		\
	reference/pygtk-gdkregion.xml			\
	reference/pygtk-gdkscreen.xml			\
	reference/pygtk-gdkwindow.xml			\
	reference/pygtk-gdkvisual.xml			\
	reference/pygtk-glade-classes.xml		\
	reference/pygtk-gladexml.xml			\
	reference/pygtk-gtkaboutdialog.xml		\
	reference/pygtk-gtkaccelgroup.xml		\
	reference/pygtk-gtkaccellabel.xml		\
	reference/pygtk-gtkaccelmap.xml			\
	reference/pygtk-gtkaccessible.xml		\
	reference/pygtk-gtkactiongroup.xml		\
	reference/pygtk-gtkaction.xml			\
	reference/pygtk-gtkactivatable.xml		\
	reference/pygtk-gtkadjustment.xml		\
	reference/pygtk-gtkalignment.xml		\
	reference/pygtk-gtkarrow.xml			\
	reference/pygtk-gtkaspectframe.xml		\
	reference/pygtk-gtkassistant.xml		\
	reference/pygtk-gtkbin.xml			\
	reference/pygtk-gtkborder.xml			\
	reference/pygtk-gtkbox.xml			\
	reference/pygtk-gtkbuttonbox.xml		\
	reference/pygtk-gtkbuildable.xml		\
	reference/pygtk-gtkbuilder.xml			\
	reference/pygtk-gtkbutton.xml			\
	reference/pygtk-gtkcalendar.xml			\
	reference/pygtk-gtkcelleditable.xml		\
	reference/pygtk-gtkcelllayout.xml		\
	reference/pygtk-gtkcellrendereraccel.xml	\
	reference/pygtk-gtkcellrenderercombo.xml	\
	reference/pygtk-gtkcellrendererpixbuf.xml	\
	reference/pygtk-gtkcellrendererprogress.xml	\
	reference/pygtk-gtkcellrendererspin.xml		\
	reference/pygtk-gtkcellrendererspinner.xml		\
	reference/pygtk-gtkcellrenderertext.xml		\
	reference/pygtk-gtkcellrenderertoggle.xml	\
	reference/pygtk-gtkcellrenderer.xml		\
	reference/pygtk-gtkcellview.xml			\
	reference/pygtk-gtkcheckbutton.xml		\
	reference/pygtk-gtkcheckmenuitem.xml		\
	reference/pygtk-gtk-classes.xml			\
	reference/pygtk-gtkclipboard.xml		\
	reference/pygtk-gtkcolorbutton.xml		\
	reference/pygtk-gtkcolorselectiondialog.xml	\
	reference/pygtk-gtkcolorselection.xml		\
	reference/pygtk-gtkcomboboxentry.xml		\
	reference/pygtk-gtkcombobox.xml			\
	reference/pygtk-gtkcomboboxtext.xml			\
	reference/pygtk-gtkcombo.xml			\
	reference/pygtk-gtk-constants.xml		\
	reference/pygtk-gtkcontainer.xml		\
	reference/pygtk-gtkcurve.xml			\
	reference/pygtk-gtkdialog.xml			\
	reference/pygtk-gtkdrawingarea.xml		\
	reference/pygtk-gtkeditable.xml			\
	reference/pygtk-gtkentrycompletion.xml		\
	reference/pygtk-gtkentry.xml			\
	reference/pygtk-gtkentrybuffer.xml			\
	reference/pygtk-gtkeventbox.xml			\
	reference/pygtk-gtkexpander.xml			\
	reference/pygtk-gtkfilechooserbutton.xml	\
	reference/pygtk-gtkfilechooserdialog.xml	\
	reference/pygtk-gtkfilechooserwidget.xml	\
	reference/pygtk-gtkfilechooser.xml		\
	reference/pygtk-gtkfilefilter.xml		\
	reference/pygtk-gtkfileselection.xml		\
	reference/pygtk-gtkfixed.xml			\
	reference/pygtk-gtkfontbutton.xml		\
	reference/pygtk-gtkfontselectiondialog.xml	\
	reference/pygtk-gtkfontselection.xml		\
	reference/pygtk-gtkframe.xml			\
	reference/pygtk-gtk-functions.xml		\
	reference/pygtk-gtkgammacurve.xml		\
	reference/pygtk-gtkhandlebox.xml		\
	reference/pygtk-gtkhbox.xml			\
	reference/pygtk-gtkhbuttonbox.xml		\
	reference/pygtk-gtkhpaned.xml			\
	reference/pygtk-gtkhruler.xml			\
	reference/pygtk-gtkhscale.xml			\
	reference/pygtk-gtkhscrollbar.xml		\
	reference/pygtk-gtkhseparator.xml		\
	reference/pygtk-gtkiconfactory.xml		\
	reference/pygtk-gtkiconinfo.xml			\
	reference/pygtk-gtkiconset.xml			\
	reference/pygtk-gtkiconsource.xml		\
	reference/pygtk-gtkicontheme.xml		\
	reference/pygtk-gtkiconview.xml			\
	reference/pygtk-gtkimagemenuitem.xml		\
	reference/pygtk-gtkimage.xml			\
	reference/pygtk-gtkimcontextsimple.xml		\
	reference/pygtk-gtkimcontext.xml		\
	reference/pygtk-gtkimmulticontext.xml		\
    reference/pygtk-gtkinfobar.xml		\
	reference/pygtk-gtkinputdialog.xml		\
	reference/pygtk-gtkinvisible.xml		\
	reference/pygtk-gtkitemfactory.xml		\
	reference/pygtk-gtkitem.xml			\
	reference/pygtk-gtklabel.xml			\
	reference/pygtk-gtklayout.xml			\
	reference/pygtk-gtklinkbutton.xml		\
	reference/pygtk-gtkliststore.xml		\
	reference/pygtk-gtkmenubar.xml			\
	reference/pygtk-gtkmenuitem.xml			\
	reference/pygtk-gtkmenushell.xml		\
	reference/pygtk-gtkmenutoolbutton.xml		\
	reference/pygtk-gtkmenu.xml			\
	reference/pygtk-gtkmessagedialog.xml		\
	reference/pygtk-gtkmisc.xml			\
	reference/pygtk-gtkmountoperation.xml		\
	reference/pygtk-gtknotebook.xml			\
	reference/pygtk-gtkobject.xml			\
    reference/pygtk-gtkoffscreenwindow.xml			\
	reference/pygtk-gtkoptionmenu.xml		\
	reference/pygtk-gtkpagesetupunixdialog.xml	\
	reference/pygtk-gtkpagesetup.xml		\
	reference/pygtk-gtkpaned.xml			\
	reference/pygtk-gtkpapersize.xml		\
	reference/pygtk-gtkplug.xml			\
	reference/pygtk-gtkprintcontext.xml		\
	reference/pygtk-gtkprinter.xml			\
	reference/pygtk-gtkprintjob.xml			\
	reference/pygtk-gtkprintoperationpreview.xml	\
	reference/pygtk-gtkprintoperation.xml		\
	reference/pygtk-gtkprintsettings.xml		\
	reference/pygtk-gtkprintunixdialog.xml		\
	reference/pygtk-gtkprogressbar.xml		\
	reference/pygtk-gtkradioaction.xml		\
	reference/pygtk-gtkradiobutton.xml		\
	reference/pygtk-gtkradiomenuitem.xml		\
	reference/pygtk-gtkradiotoolbutton.xml		\
	reference/pygtk-gtkrange.xml			\
	reference/pygtk-gtkrcstyle.xml			\
	reference/pygtk-gtkrecentaction.xml		\
	reference/pygtk-gtkrecentchooserdialog.xml	\
	reference/pygtk-gtkrecentchoosermenu.xml	\
	reference/pygtk-gtkrecentchooserwidget.xml	\
	reference/pygtk-gtkrecentchooser.xml		\
	reference/pygtk-gtkrecentfilter.xml		\
	reference/pygtk-gtkrecentinfo.xml		\
	reference/pygtk-gtkrecentmanager.xml		\
	reference/pygtk-gtkrequisition.xml		\
	reference/pygtk-gtkruler.xml			\
	reference/pygtk-gtkscale.xml			\
	reference/pygtk-gtkscalebutton.xml		\
	reference/pygtk-gtkscrollbar.xml		\
	reference/pygtk-gtkscrolledwindow.xml		\
	reference/pygtk-gtkselectiondata.xml		\
	reference/pygtk-gtkseparatormenuitem.xml	\
	reference/pygtk-gtkseparatortoolitem.xml	\
	reference/pygtk-gtkseparator.xml		\
	reference/pygtk-gtksettings.xml			\
	reference/pygtk-gtksizegroup.xml		\
	reference/pygtk-gtksocket.xml			\
	reference/pygtk-gtkspinbutton.xml		\
    reference/pygtk-gtkspinner.xml		\
	reference/pygtk-gtkstatusbar.xml		\
	reference/pygtk-gtkstatusicon.xml		\
	reference/pygtk-gtk-stock-items.xml		\
	reference/pygtk-gtkstyle.xml			\
	reference/pygtk-gtktable.xml			\
	reference/pygtk-gtktearoffmenuitem.xml		\
	reference/pygtk-gtktextattributes.xml		\
	reference/pygtk-gtktextbuffer.xml		\
	reference/pygtk-gtktextchildanchor.xml		\
	reference/pygtk-gtktextiter.xml			\
	reference/pygtk-gtktextmark.xml			\
	reference/pygtk-gtktexttagtable.xml		\
	reference/pygtk-gtktexttag.xml			\
	reference/pygtk-gtktextview.xml			\
	reference/pygtk-gtktoggleaction.xml		\
	reference/pygtk-gtktogglebutton.xml		\
	reference/pygtk-gtktoggletoolbutton.xml		\
	reference/pygtk-gtktoolbar.xml			\
	reference/pygtk-gtktoolbutton.xml		\
	reference/pygtk-gtktoolitem.xml			\
    reference/pygtk-gtktoolitemgroup.xml	\
    reference/pygtk-gtktoolpalette.xml	    \
	reference/pygtk-gtktoolshell.xml		\
	reference/pygtk-gtktooltip.xml			\
	reference/pygtk-gtktooltips.xml			\
	reference/pygtk-gtktreedragdest.xml		\
	reference/pygtk-gtktreedragsource.xml		\
	reference/pygtk-gtktreeiter.xml			\
	reference/pygtk-gtktreemodelfilter.xml		\
	reference/pygtk-gtktreemodelsort.xml		\
	reference/pygtk-gtktreemodel.xml		\
	reference/pygtk-gtktreerowreference.xml		\
	reference/pygtk-gtktreeselection.xml		\
	reference/pygtk-gtktreesortable.xml		\
	reference/pygtk-gtktreestore.xml		\
	reference/pygtk-gtktreeviewcolumn.xml		\
	reference/pygtk-gtktreeview.xml			\
	reference/pygtk-gtkuimanager.xml		\
	reference/pygtk-gtkvbox.xml			\
	reference/pygtk-gtkvbuttonbox.xml		\
	reference/pygtk-gtkviewport.xml			\
	reference/pygtk-gtkvolumebutton.xml		\
	reference/pygtk-gtkvpaned.xml			\
	reference/pygtk-gtkvruler.xml			\
	reference/pygtk-gtkvscale.xml			\
	reference/pygtk-gtkvscrollbar.xml		\
	reference/pygtk-gtkvseparator.xml		\
	reference/pygtk-gtkwidget.xml			\
	reference/pygtk-gtkwindowgroup.xml		\
	reference/pygtk-gtkwindow.xml			\
	reference/pygtk-hierarchy.xml			\
	reference/pygtk-introduction.xml		\
	reference/pygtk-pangoattribute.xml		\
	reference/pygtk-pangoattriter.xml		\
	reference/pygtk-pangoattrlist.xml		\
	reference/pygtk-pangocairocairocontext.xml	\
	reference/pygtk-pangocairocairofontmap.xml	\
	reference/pygtk-pangocairo-classes.xml		\
	reference/pygtk-pangocairo-constants.xml	\
	reference/pygtk-pangocairo-functions.xml	\
	reference/pygtk-pango-classes.xml		\
	reference/pygtk-pangocolor.xml			\
	reference/pygtk-pango-constants.xml		\
	reference/pygtk-pangocontext.xml		\
	reference/pygtk-pangofontdescription.xml	\
	reference/pygtk-pangofontface.xml		\
	reference/pygtk-pangofontfamily.xml		\
	reference/pygtk-pangofontmap.xml		\
	reference/pygtk-pangofontmetrics.xml		\
	reference/pygtk-pangofontsetsimple.xml		\
	reference/pygtk-pangofontset.xml		\
	reference/pygtk-pangofont.xml			\
	reference/pygtk-pango-functions.xml		\
	reference/pygtk-pangoglyphstring.xml		\
	reference/pygtk-pangolanguage.xml		\
	reference/pygtk-pangolayoutiter.xml		\
	reference/pygtk-pangolayoutline.xml		\
	reference/pygtk-pangolayout.xml			\
	reference/pygtk-pango-markup.xml		\
	reference/pygtk-pangorenderer.xml		\
	reference/pygtk-pangotabarray.xml		\
	reference/pygtk-pygtkgenericcellrenderer.xml	\
	reference/pygtk-pygtkgenerictreemodel.xml	\
	reference/pygtk-pygtktreemodelrowiter.xml	\
	reference/pygtk-pygtktreemodelrow.xml		\
	reference/pygtk-unixprint-classes.xml		\
	reference/pygtk-unixprint-constants.xml		\
	reference/pygtk-unixprint-functions.xml

HTML_FILES =						\
	html/atk-class-reference.html			\
	html/atk-constants.html				\
	html/class-atkaction.html			\
	html/class-atkcomponent.html			\
	html/class-atkdocument.html			\
	html/class-atkeditabletext.html			\
	html/class-atkgobjectaccessible.html		\
	html/class-atkhyperlink.html			\
	html/class-atkhypertext.html			\
	html/class-atkimage.html			\
	html/class-atkimplementor.html			\
	html/class-atknoopobjectfactory.html		\
	html/class-atknoopobject.html			\
	html/class-atkobjectfactory.html		\
	html/class-atkobject.html			\
	html/class-atkrectangle.html			\
	html/class-atkregistry.html			\
	html/class-atkrelation.html			\
	html/class-atkrelationset.html			\
	html/class-atkselection.html			\
	html/class-atkstateset.html			\
	html/class-atkstreamablecontent.html		\
	html/class-atktable.html			\
	html/class-atktext.html				\
	html/class-atkvalue.html			\
	html/class-gdkatom.html				\
	html/class-gdkapplaunchcontext.html		\
	html/class-gdkcairocontext.html			\
	html/class-gdkcolor.html			\
	html/class-gdkcolormap.html			\
	html/class-gdkcursor.html			\
	html/class-gdkdevice.html			\
	html/class-gdkdisplay.html			\
	html/class-gdkdisplaymanager.html		\
	html/class-gdkdragcontext.html			\
	html/class-gdkdrawable.html			\
	html/class-gdkevent.html			\
	html/class-gdkgc.html				\
	html/class-gdkimage.html			\
	html/class-gdkkeymap.html			\
	html/class-gdkpangorenderer.html		\
	html/class-gdkpixbufanimation.html		\
	html/class-gdkpixbufanimationiter.html		\
	html/class-gdkpixbuf.html			\
	html/class-gdkpixbufloader.html			\
	html/class-gdkpixmap.html			\
	html/class-gdkrectangle.html			\
	html/class-gdkregion.html			\
	html/class-gdkscreen.html			\
	html/class-gdkwindow.html			\
	html/class-gdkvisual.html			\
	html/class-gladexml.html			\
	html/class-gtkaboutdialog.html			\
	html/class-gtkaccelgroup.html			\
	html/class-gtkaccellabel.html			\
	html/class-gtkaccelmap.html			\
	html/class-gtkaccessible.html			\
	html/class-gtkactiongroup.html			\
	html/class-gtkaction.html			\
	html/class-gtkactivatable.html			\
	html/class-gtkadjustment.html			\
	html/class-gtkalignment.html			\
	html/class-gtkarrow.html			\
	html/class-gtkaspectframe.html			\
	html/class-gtkassistant.html			\
	html/class-gtkbin.html				\
	html/class-gtkborder.html			\
	html/class-gtkbox.html				\
	html/class-gtkbuttonbox.html			\
	html/class-gtkbuildable.html			\
	html/class-gtkbuilder.html			\
	html/class-gtkbutton.html			\
	html/class-gtkcalendar.html			\
	html/class-gtkcelleditable.html			\
	html/class-gtkcelllayout.html			\
	html/class-gtkcellrendereraccel.html		\
	html/class-gtkcellrenderercombo.html		\
	html/class-gtkcellrenderer.html			\
	html/class-gtkcellrendererpixbuf.html		\
	html/class-gtkcellrendererprogress.html		\
	html/class-gtkcellrendererspin.html		\
	html/class-gtkcellrendererspinner.html		\
	html/class-gtkcellrenderertext.html		\
	html/class-gtkcellrenderertoggle.html		\
	html/class-gtkcellview.html			\
	html/class-gtkcheckbutton.html			\
	html/class-gtkcheckmenuitem.html		\
	html/class-gtkclipboard.html			\
	html/class-gtkcolorbutton.html			\
	html/class-gtkcolorselectiondialog.html		\
	html/class-gtkcolorselection.html		\
	html/class-gtkcomboboxentry.html		\
	html/class-gtkcombobox.html			\
	html/class-gtkcomboboxtext.html			\
	html/class-gtkcombo.html			\
	html/class-gtkcontainer.html			\
	html/class-gtkcurve.html			\
	html/class-gtkdialog.html			\
	html/class-gtkdrawingarea.html			\
	html/class-gtkeditable.html			\
	html/class-gtkentrycompletion.html		\
	html/class-gtkentry.html			\
	html/class-gtkentrybuffer.html			\
	html/class-gtkeventbox.html			\
	html/class-gtkexpander.html			\
	html/class-gtkfilechooserbutton.html		\
	html/class-gtkfilechooserdialog.html		\
	html/class-gtkfilechooser.html			\
	html/class-gtkfilechooserwidget.html		\
	html/class-gtkfilefilter.html			\
	html/class-gtkfileselection.html		\
	html/class-gtkfixed.html			\
	html/class-gtkfontbutton.html			\
	html/class-gtkfontselectiondialog.html		\
	html/class-gtkfontselection.html		\
	html/class-gtkframe.html			\
	html/class-gtkgammacurve.html			\
	html/class-gtkhandlebox.html			\
	html/class-gtkhbox.html				\
	html/class-gtkhbuttonbox.html			\
	html/class-gtkhpaned.html			\
	html/class-gtkhruler.html			\
	html/class-gtkhscale.html			\
	html/class-gtkhscrollbar.html			\
	html/class-gtkhseparator.html			\
	html/class-gtkiconfactory.html			\
	html/class-gtkiconinfo.html			\
	html/class-gtkiconset.html			\
	html/class-gtkiconsource.html			\
	html/class-gtkicontheme.html			\
	html/class-gtkiconview.html			\
	html/class-gtkimage.html			\
	html/class-gtkimagemenuitem.html		\
	html/class-gtkimcontext.html			\
	html/class-gtkimcontextsimple.html		\
	html/class-gtkimmulticontext.html		\
    html/class-gtkinfobar.html		\
	html/class-gtkinputdialog.html			\
	html/class-gtkinvisible.html			\
	html/class-gtkitemfactory.html			\
	html/class-gtkitem.html				\
	html/class-gtklabel.html			\
	html/class-gtklayout.html			\
	html/class-gtklinkbutton.html			\
	html/class-gtkliststore.html			\
	html/class-gtkmenubar.html			\
	html/class-gtkmenu.html				\
	html/class-gtkmenuitem.html			\
	html/class-gtkmenushell.html			\
	html/class-gtkmenutoolbutton.html		\
	html/class-gtkmessagedialog.html		\
	html/class-gtkmisc.html				\
	html/class-gtkmountoperation.html		\
	html/class-gtknotebook.html			\
	html/class-gtkobject.html			\
    html/class-gtkoffscreenwindow.html			\
	html/class-gtkoptionmenu.html			\
	html/class-gtkpagesetup.html			\
	html/class-gtkpagesetupunixdialog.html		\
	html/class-gtkpaned.html			\
	html/class-gtkpapersize.html			\
	html/class-gtkplug.html				\
	html/class-gtkprintcontext.html			\
	html/class-gtkprinter.html			\
	html/class-gtkprintjob.html			\
	html/class-gtkprintoperation.html		\
	html/class-gtkprintoperationpreview.html	\
	html/class-gtkprintsettings.html		\
	html/class-gtkprintunixdialog.html		\
	html/class-gtkprogressbar.html			\
	html/class-gtkradioaction.html			\
	html/class-gtkradiobutton.html			\
	html/class-gtkradiomenuitem.html		\
	html/class-gtkradiotoolbutton.html		\
	html/class-gtkrange.html			\
	html/class-gtkrcstyle.html			\
	html/class-gtkrecentaction.html			\
	html/class-gtkrecentchooserdialog.html		\
	html/class-gtkrecentchooser.html		\
	html/class-gtkrecentchoosermenu.html		\
	html/class-gtkrecentchooserwidget.html		\
	html/class-gtkrecentfilter.html			\
	html/class-gtkrecentinfo.html			\
	html/class-gtkrecentmanager.html		\
	html/class-gtkrequisition.html			\
	html/class-gtkruler.html			\
	html/class-gtkscale.html			\
	html/class-gtkscalebutton.html			\
	html/class-gtkscrollbar.html			\
	html/class-gtkscrolledwindow.html		\
	html/class-gtkselectiondata.html		\
	html/class-gtkseparator.html			\
	html/class-gtkseparatormenuitem.html		\
	html/class-gtkseparatortoolitem.html		\
	html/class-gtksettings.html			\
	html/class-gtksizegroup.html			\
	html/class-gtksocket.html			\
	html/class-gtkspinbutton.html			\
    html/class-gtkspinner.html			\
	html/class-gtkstatusbar.html			\
	html/class-gtkstatusicon.html			\
	html/class-gtkstyle.html			\
	html/class-gtktable.html			\
	html/class-gtktearoffmenuitem.html		\
	html/class-gtktextattributes.html		\
	html/class-gtktextbuffer.html			\
	html/class-gtktextchildanchor.html		\
	html/class-gtktextiter.html			\
	html/class-gtktextmark.html			\
	html/class-gtktexttag.html			\
	html/class-gtktexttagtable.html			\
	html/class-gtktextview.html			\
	html/class-gtktoggleaction.html			\
	html/class-gtktogglebutton.html			\
	html/class-gtktoggletoolbutton.html		\
	html/class-gtktoolbar.html			\
	html/class-gtktoolbutton.html			\
	html/class-gtktoolitem.html			\
    html/class-gtktoolitemgroup.html	\
    html/class-gtktoolpalette.html	    \
	html/class-gtktoolshell.html			\
	html/class-gtktooltip.html			\
	html/class-gtktooltips.html			\
	html/class-gtktreedragdest.html			\
	html/class-gtktreedragsource.html		\
	html/class-gtktreeiter.html			\
	html/class-gtktreemodelfilter.html		\
	html/class-gtktreemodel.html			\
	html/class-gtktreemodelsort.html		\
	html/class-gtktreerowreference.html		\
	html/class-gtktreeselection.html		\
	html/class-gtktreesortable.html			\
	html/class-gtktreestore.html			\
	html/class-gtktreeviewcolumn.html		\
	html/class-gtktreeview.html			\
	html/class-gtkuimanager.html			\
	html/class-gtkvbox.html				\
	html/class-gtkvbuttonbox.html			\
	html/class-gtkviewport.html			\
	html/class-gtkvolumebutton.html			\
	html/class-gtkvpaned.html			\
	html/class-gtkvruler.html			\
	html/class-gtkvscale.html			\
	html/class-gtkvscrollbar.html			\
	html/class-gtkvseparator.html			\
	html/class-gtkwidget.html			\
	html/class-gtkwindowgroup.html			\
	html/class-gtkwindow.html			\
	html/class-hierarchy.html			\
	html/class-pangoattribute.html			\
	html/class-pangoattriterator.html		\
	html/class-pangoattrlist.html			\
	html/class-pangocairocairocontext.html		\
	html/class-pangocairocairofontmap.html		\
	html/class-pangocolor.html			\
	html/class-pangocontext.html			\
	html/class-pangofontdescription.html		\
	html/class-pangofontface.html			\
	html/class-pangofontfamily.html			\
	html/class-pangofont.html			\
	html/class-pangofontmap.html			\
	html/class-pangofontmetrics.html		\
	html/class-pangofontset.html			\
	html/class-pangofontsetsimple.html		\
	html/class-pangoglyphstring.html		\
	html/class-pangolanguage.html			\
	html/class-pangolayout.html			\
	html/class-pangolayoutiter.html			\
	html/class-pangolayoutline.html			\
	html/class-pangorenderer.html			\
	html/class-pangotabarray.html			\
	html/class-pygtkgenericcellrenderer.html	\
	html/class-pygtkgenerictreemodel.html		\
	html/class-pygtktreemodelrow.html		\
	html/class-pygtktreemodelrowiter.html		\
	html/gdk-class-reference.html			\
	html/gdk-constants.html				\
	html/gdk-functions.html				\
	html/glade-class-reference.html			\
	html/gtk-class-reference.html			\
	html/gtk-constants.html				\
	html/gtk-functions.html				\
	html/gtk-stock-items.html			\
	html/index.html					\
	html/index.sgml					\
	html/pangocairo-class-reference.html		\
	html/pangocairo-constants.html			\
	html/pangocairo-functions.html			\
	html/pango-class-reference.html			\
	html/pango-constants.html			\
	html/pango-functions.html			\
	html/pango-markup-language.html			\
	html/pygtk.devhelp				\
	html/pygtk-introduction.html			\
	html/unixprint-class-reference.html		\
	html/unixprint-constants.html			\
	html/unixprint-functions.html


CSS_FILES = $(PYGOBJECT_PYGDOCS)/style.css

TARGET_DIR = $(datadir)/gtk-doc/html/pygtk
CSSdir = $(TARGET_DIR)

EXTRA_DIST = 				\
	cursors icons images		\
	$(HTML_FILES)			\
	$(XMLFILES) 			\
	$(XSLFILES)			\
	$(FIX_XREF)

dist-hook:
	rm -rf `find $(distdir)/cursors -name .svn`
	rm -rf `find $(distdir)/icons -name .svn`
	rm -rf `find $(distdir)/images -name .svn`

install-data-local:
	installfiles=`echo $(srcdir)/html/*`; \
	if test "$$installfiles" = '$(srcdir)/html/*'; \
	then echo '-- Nothing to install' ; \
	else \
	  $(mkinstalldirs) $(DESTDIR)$(TARGET_DIR) \
			$(DESTDIR)$(TARGET_DIR)/icons \
			$(DESTDIR)$(TARGET_DIR)/images \
			$(DESTDIR)$(TARGET_DIR)/cursors; \
	  for i in $$installfiles; do \
	    echo '-- Installing '$$i ; \
	    $(INSTALL_DATA) $$i $(DESTDIR)$(TARGET_DIR); \
	  done; \
	  echo '-- Installing $(srcdir)/html/index.sgml' ; \
	  $(INSTALL_DATA) $(srcdir)/html/index.sgml $(DESTDIR)$(TARGET_DIR) || :; \
	  for i in $(srcdir)/icons/*; do \
	    echo '-- Installing '$$i ; \
	    $(INSTALL_DATA) $$i $(DESTDIR)$(TARGET_DIR)/icons; \
	  done; \
	  for i in $(srcdir)/images/*; do \
	    echo '-- Installing '$$i ; \
	    $(INSTALL_DATA) $$i $(DESTDIR)$(TARGET_DIR)/images; \
	  done; \
	  for i in $(srcdir)/cursors/*; do \
	    echo '-- Installing '$$i ; \
	    $(INSTALL_DATA) $$i $(DESTDIR)$(TARGET_DIR)/cursors; \
	  done; \
	fi

uninstall-local:
	rm -rf $(DESTDIR)$(TARGET_DIR)/*

if ENABLE_DOCS

BUILT_SOURCES = reference/builddate.xml $(HTML_FILES)
DISTCLEANFILES = reference/builddate.xml $(HTML_FILES)

CSS_DATA = $(CSS_FILES)

install-data-hook:
	@$(PYGOBJECT_FIXXREF) -i $(PYGOBJECT_PYGDOCS) $(DESTDIR)$(TARGET_DIR)

reference/builddate.xml:
	@$(PYTHON) -c 'import datetime; print datetime.date.today()' > $@


# Do not add a builddate.xml dependency here, because the generated html
# documentation will be included in the tarball so xsltproc is not needed to
# install the documentation. Adding a builddate.xml dependency will force
# the documentation to be rebuilt when it doesn't need to be.
$(HTML_FILES): $(XMLFILES) $(XSLFILES)
	@xsltproc --nonet --xinclude -o $(BUILDDIR)/html/ \
		  --path $(BUILDDIR)/reference:$(srcdir)/reference \
                  --stringparam gtkdoc.bookname "pygtk" \
                  --stringparam gtkdoc.version ${REF_VERSION} \
                  $(HTML_STYLE) $(srcdir)/reference/pygtk2-ref.xml

# pdf: $(REFERENCE_DEPS) reference/builddate.xml
# 	xsltproc --nonet --xinclude -o pygtk-ref.fo \
# 	         $(PDF_STYLE) reference/pygtk2-ref.xml
# 	pdfxmltex pygtk-ref.fo >output </dev/null


else

install-data-hook:

endif
