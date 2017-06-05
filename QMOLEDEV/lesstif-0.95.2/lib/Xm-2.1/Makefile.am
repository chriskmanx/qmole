#
# $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Makefile.am,v 1.31 2005/11/01 09:02:57 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

# libdir = $(exec_prefix)/lib
libXm_la_LDFLAGS=	-version-info 2:1  $(X_LIBS)  -no-undefined

#
# Sources in this directory
#
SRCS_1_2 = \
	ArrowB.c ArrowBG.c AtomMgr.c BaseClass.c BulletinBoard.c \
	Cache.c CascadeB.c CascadeBG.c Command.c CutPaste.c \
	DebugUtil.c Desktop.c Dest.c DialogS.c Display.c \
	DragBS.c DragC.c DragIcon.c DragOverS.c \
	DragUnder.c DrawingArea.c DrawnB.c DropSMgr.c DropSMgrI.c \
	DropTrans.c ExtObject.c FileSB.c FontList.c Form.c Frame.c \
	Gadget.c GadgetUtil.c GeoUtils.c Hash.c ImageCache.c \
	LTglob.c Label.c LabelG.c List.c \
	MapEvent.c MainW.c Manager.c MenuShell.c MenuUtil.c \
	MessageB.c Mwm.c Obsolete.c Primitive.c \
	Protocols.c PushB.c PushBG.c RCUtils.c ReadImage.c \
	Region.c RepType.c ResInd.c RowColumn.c Sash.c \
	Scale.c Screen.c ScrollBar.c ScrolledW.c Selection.c \
	SelectionBox.c Separator.c SeparatoG.c Shadow.c ShellExt.c \
	Simple.c TearOff.c TearOffB.c Text.c TextF.c TextIn.c TextOut.c \
	TextStrSo.c ToggleB.c ToggleBG.c TrackLoc.c Transltns.c \
	Traversal.c TravAct.c UniqueEvent.c VaSimple.c VirtKeys.c \
	Visual.c World.c XmString.c XmIm.c Xmos.c \
	misc.c LTCvt.c LT@Editres@Editres.c Vendor.c ResConvert.c

SRCS_2_0 = \
	ComboBox.c Container.c \
	Direction.c Draw.c GrabShell.c \
	Notebook.c PanedW.c PixConv.c PopupUtil.c \
	RenderTable.c SpinBox.c Trait.c TranslNew.c \
	Xme.c IconG.c Transfer.c \
	XpmAttrib.c XpmCrBufFrI.c XpmCrBufFrP.c XpmCrDatFrI.c XpmCrDatFrP.c Xpmcreate.c \
	XpmCrIFrBuf.c XpmCrIFrDat.c XpmCrIFrP.c XpmCrPFrBuf.c XpmCrPFrDat.c XpmCrPFrI.c \
	Xpmdata.c Xpmhashtab.c XpmImage.c XpmInfo.c Xpmmisc.c Xpmparse.c XpmRdFToBuf.c \
	XpmRdFToDat.c XpmRdFToI.c XpmRdFToP.c Xpmrgb.c Xpmscan.c XpmWrFFrBuf.c \
	XpmWrFFrDat.c XpmWrFFrI.c XpmWrFFrP.c Xpms_popen.c \
	XmTabList.c

SRCS_2_1 =	Messages.c Print.c PrintShell.c SSpinB.c XmStrDefs.c

if UseXDND
XDND_SRC=xdnd.c
else
XDND_SRC=
endif

SRCS	= ${SRCS_2_1} ${SRCS_2_0} ${SRCS_1_2} ${XDND_SRC}

libXm_la_LIBADD = $(X_PRE_LIBS) -lXt $(XPLIB) $(X_LIBS) -lX11 $(X_EXTRA_LIBS)

#if Have_Libxp
#libXm_la_LIBADD = -lXt $(X_PRE_LIBS) -lXp -lXext -lX11 \
#                  $(X_EXTRA_LIBS)
#else
#libXm_la_LIBADD = $(X_LIBS) -lXt $(X_PRE_LIBS) -lX11 \
#                  $(X_EXTRA_LIBS)
#endif

AM_CPPFLAGS=	-I$(top_srcdir)/include/Motif-2.1 \
		-I$(top_builddir)/include/Motif-2.1 \
		${X_CFLAGS}

# INCLUDES=	-I$(top_srcdir)/include/Motif-2.1 \
# 		-I$(top_builddir)/include/Motif-2.1 \
# 		-I$(top_srcdir)/include/Motif-2.0 \
# 		-I$(top_builddir)/include/Motif-2.0 \
# 		-I$(top_srcdir)/include/Motif-1.2 \
# 		-I$(top_builddir)/include/Motif-1.2 \
# 		-I$(top_srcdir)/include \
# 		-I$(top_builddir)/include \
# 		${X_CFLAGS}

#
# ResConvert.c contains dependencies on the configuration script contents.
# (LessTif version numbers.)
#
ResConvert.lo:	$(top_srcdir)/configure.in
#
# Install headers
#
# Lets put the headers in a sub directory to avoid
# poluting /usr/local with more directories for default installs.
#

lib_LTLIBRARIES=	libXm.la

libXm_la_SOURCES= ${SRCS}
