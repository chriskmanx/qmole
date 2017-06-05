/**
 *
 * $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/XmI/XmI.h,v 1.4 2006/04/19 18:42:22 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

/*
 * LessTif-specific functions/variables.  Use at the cost of incompatibility
 * with Motif.
 * YOU SHOULD NOT CALL CALL THESE FUNCTIONS IF YOU DON'T KNOW WHAT YOU'RE
 * DOING!!
 * Correction: Some of these functions are totally undocumented Motif calls.
   WE don't know if we got them right, so you'd better not bank on them.
 */

#ifndef _XMI_XMI_H
#define _XMI_XMI_H

#include <Xm/XmP.h>
#include <Xm/ScreenP.h>
#include <Xm/ManagerP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/GadgetP.h>
#include <Xm/DrawP.h>
#include <Xm/RowColumnP.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <XmI/MacrosI.h>

#ifdef	USE_XFT
#include <X11/Xft/Xft.h>
#endif

/*
 * extra resources
 */
#ifndef XmNdefaultVirtualBindings
#define XmNdefaultVirtualBindings	"defaultVirtualBindings"
#endif

#define _XA_MOTIF_DEFAULT_BINDINGS	"_MOTIF_DEFAULT_BINDINGS"

/*
 * STRING AND FONTLIST INTERNALS
 */
extern XmFontList __XmFontListAlloc(int numberOfEntries);
extern void _XmFontListEntryFree(XmFontListEntry entry);
XmFontListEntry _XmFontListEntryCreate(void);

/* Experimental _XmRenderTableFinalise() */
void __XmRenderTableFinalise(Widget, XmFontList, _XmString);
void _XmRenderTableFinalise(Widget, XmFontList, XmString);
void _XmRenderTableFinaliseTag(Widget, XmFontList, char *);

XmRenderTable _XmRenderTablePushRendition(XmRenderTable, XmRendition);
XmRenderTable _XmRenderTablePopRendition(XmRenderTable, XmRendition);

typedef struct __XmRenditionRec {
	char		*tag;
	XmFontType	type;
	XFontSet	font;
	int		count;
	Pixel		rendition_background,
			rendition_foreground;
	char		*font_name;
	unsigned char	load_model,
			strike_thru_type,
			underline_type;
	XmTabList	tab_list;
	Display		*dpy;
#ifdef	USE_XFT
	/* experimental - use of Xft, see http://fontconfig.org */
	char		*font_style,
/*			*family,	Use font_name instead. */
			*font_foundry,
			*font_encoding;
	int		font_size,
			pixel_size,
			font_slant,
			font_spacing,
			font_weight;
	XftPattern	*pattern;
	XftFont		*xft_font;
	XftColor	xft_foreground, xft_background;
	int		font_average_width,	/* Calculated by LessTif,	*/
			font_average_height;	/* depend only on xft_font	*/
#endif
} *_XmRendition;

struct __XmRenderTableRec {
	unsigned int	count;
	Display		*dpy;
	_XmRendition	*renditions;	/* An array of pointers */
};

struct _XmFontListContextRec {
    XmFontList fontlist;
    int current_entry;
};

struct __XmStringExtRec {
    unsigned char tag;
    unsigned char len;
    unsigned char data[1];
};

struct __XmStringComponentRec {
    XmStringComponentType type;
    int length;
    char *data;
    short font;
};
typedef struct __XmStringComponentRec _XmStringComponentRec, *_XmStringComponent;

struct __XmStringRec {
    struct __XmStringComponentRec **components;
    int number_of_components;
};

struct __XmStringContextRec {
    struct __XmStringRec *string;
    int current_component;
};

/*
 * NOTE: The first two fields in this structure MUST match those
 * in struct __XmStringContextRec!!!
 */
struct _XmtStringContextRec {
    struct __XmStringRec *string;
    int current_component;
    char *text;
    short textlen;
    char *charset;
    XmStringDirection direction;
    Boolean separator;
};

struct __XmParseMappingRec {
	XtPointer	client_data;
	XmIncludeStatus	include_status;
	XmParseProc	invoke_parse_proc;
	XtPointer	pattern;
	XmTextType	pattern_type;
	XmString	substitute;
};

/* ADDED FOR EXTERNAL FORM */
#define XmSTRING_COMPONENT_XMSTRING     (XmSTRING_COMPONENT_LOCALE_TEXT + 1)
#define XmSTRING_TAG                    0xDFU
#define XmSTRING_LENGTH                 0x80U

/*
 * XmIm stuff
 */
typedef unsigned char XmIMInputPolicy;

#define	IP_INHERIT_POLICY	0
#define	IP_PER_WIDGET		1
#define	IP_PER_SHELL		2

void _XmImSendSpot(Widget w);

/*
 * GENERIC PROTOTYPES
 */
XmScreenInfo *_XmGetScreenInfo(Widget w);

String _XmMakeDialogName(String name);
void _XmError(Widget w, const char *message, ...);

/*
 * Dimension variables (below) are typed as int because Dimension is
 * unsigned. However, we need to check whether they become negative, hence
 * the theoretically incorrect type.
 * We don't do GetValues on them so we should be ok.
 */
typedef struct
{
    /* NOTE: The next four lines ABSOLUTELY MUST MATCH the first
     * three lines in XmMWValues!!!!!!! */
    Boolean   ShowVSB, ShowHSB, HasHSB, HasVSB;
    Position  HsbX, HsbY, VsbX, VsbY, ClipX, ClipY, WorkX, WorkY;
    int       HsbW, HsbH, VsbW, VsbH, ClipW, ClipH, WorkW, WorkH;
    int       SwY, SwW, SwH;
}
XmSWValues;

typedef struct
{
    /* NOTE: The next four lines ABSOLUTELY MUST MATCH the first
     * three lines in XmSWValues!!!!! */
    Boolean   ShowVSB, ShowHSB, HasHSB, HasVSB;
    Position  HsbX, HsbY, VsbX, VsbY, ClipX, ClipY, WorkX, WorkY;
    int       HsbW, HsbH, VsbW, VsbH, ClipW, ClipH, WorkW, WorkH;
    int       SwY, MwW, MwH;
    int       mbw, mbh, cww, cwh, mww, mwh, www, wwh;
    int       s1w, s1h, s2w, s2h, s3w, s3h;
    Position  mbx, mby, cwx, cwy, mwx, mwy, wwx, wwy;
    Position  s1x, s1y, s2x, s2y, s3x, s3y;
}
XmMWValues;

/* default procs */
void _XmCascadePixmapDefault(Widget w, int offset, XrmValue *val);

/* Find the VendorShell Extension Object */
Widget _LtFindVendorExt(Widget);

#ifdef USE_XFT
extern void _XmXftSetClipRectangles(Widget w, Position x, Position y,
	XRectangle *rects, int n);
#endif
/*
 * fontlist prototypes
 */
XmFontList _XmFontListCreateDefault(Display *disp);
XmFontListEntry _XmFontListEntryFromTag(XmFontList fontlist, char *tag);

/* for vendor */
void _XmInitProtocols(Widget w);
void _XmDestroyProtocols(Widget w);
#ifdef LESSTIF_EDITRES
void _XmNSEEditResCheckMessages(Widget w, XtPointer data,
                                XEvent *event, Boolean *cont);
#endif

/* Things for Label/LabelG */

void _XmLabelGetPixmapSize(Widget w, Pixmap Pix,
                           Dimension *width, Dimension *height);

/* For buttons */
#define ACTIVATE_DELAY	100

/* GeomUtils : A few of these I'm not sure of*/
XtGeometryResult _XmGMReplyToQueryGeometry(Widget w,
                                           XtWidgetGeometry *request,
                                           XtWidgetGeometry *reply);
XtGeometryResult _XmGMHandleQueryGeometry(Widget w,
                                          XtWidgetGeometry *proposed,
                                          XtWidgetGeometry *answer,
                                          Dimension margin_width,
                                          Dimension margin_height,
                                          unsigned char resize_policy);
void _XmGMEnforceMargin(Widget w,
                        Dimension margin_width,
                        Dimension margin_height,
                        Boolean useSetValues);
void _XmGMCalcSize(Widget w,
                   Dimension margin_w, Dimension margin_h,
                   Dimension *retw, Dimension *reth);
void _XmGMDoLayout(Widget w,
                   Dimension margin_w, Dimension margin_h,
                   unsigned char resize_policy, short adjust);
XtGeometryResult _XmGMHandleGeometryManager(Widget w,
                                            Widget instigator,
                                            XtWidgetGeometry *desired,
                                            XtWidgetGeometry *allowed,
                                            Dimension margin_width,
                                            Dimension margin_height,
                                            unsigned char resize_policy,
                                            Boolean allow_overlap);
Boolean _XmGMOverlap(Widget w, Widget instigator,
                     Position x, Position y,
                     Dimension width, Dimension height);

/* for DialogS.c */
void _XmBbMap(Widget w);
void _XmBbUnmap(Widget w);

/* for ImageCache */
void _XmSetupImageCache(void);

/* from MenuUtil.c */
void _XmFakeExpose(Widget menu_shell);
Boolean _XmMenuGetInPMMode(Widget w);

/* used as the operation parameter for _XmMenuFocus. */
enum {
  XmMENU_FOCUS_SAVE=0,
  XmMENU_FOCUS_RESTORE,
  XmMENU_FOCUS_SET
};

/* from MessageB.c */
void _XmMessageBoxInstallImages(Widget w);

/* from misc (and for primitives and gadgets) */
void _XmInstallStippleImages(Widget w);

#define XmEVEN_STIPPLE_IMAGE	"xm_even_stipple"
#define XmODD_STIPPLE_IMAGE	"xm_odd_stipple"

/* from RCUtils.c */
void _XmRCSetMargins(Widget rc);
XtGeometryResult _XmRCAdjustSize(Widget rc, Widget instig, XtWidgetGeometry *instig_request);
void _XmRCPreferredSize(Widget rc, XtWidgetGeometry *rcg);

/* from ResInd */
void _XmExportXmString(Widget w, int offset, XtArgVal *value);
void _XmExportString(Widget w, int offset, XtArgVal *value);

/* from Manager.c */
#ifndef MCEPTR
#define MCEPTR(cl) \
    ((XmManagerClassExt *)(&(((XmManagerWidgetClass)(cl))->manager_class.extension)))
#endif
#ifndef _XmGetManagerClassExtPtr
#define _XmGetManagerClassExtPtr(cl, o) \
    ((*MCEPTR(cl) && (((*MCEPTR(cl))->record_type) == (o))) \
        ? MCEPTR(cl) \
        : ((XmManagerClassExt *)_XmGetClassExtensionPtr(((XmGenericClassExt *)MCEPTR(cl)), (o))))
#endif

void _XmManagerInstallAccelerator(Widget m, Widget w, String s);
void _XmManagerInstallMnemonic(Widget m, Widget w, KeySym mn);
void _XmManagerUninstallAccelerator(Widget m, Widget w);
void _XmManagerUninstallMnemonic(Widget m, Widget w);


/* for MapEvent.c */
int _XmMapKeyEvents(String str, int **eventType, KeySym **keysym, Modifiers **modifiers);

/* for ScrolledW */
/* T. Straumann: */
void _XmScrolledWPreferredSize(Widget w, Widget instigator,
				      XtWidgetGeometry *instigator_geom, XmSWValues * vals);
void _XmScrolledWLayout(Widget w, Widget instigator,
				      XtWidgetGeometry *instigator_geom, XmSWValues * vals);
void _XmScrolledWConfigureChildren(Widget w, Widget instigator,
				      XtWidgetGeometry *instigator_geom, XmSWValues * vals);

/* from SelectionBox.c */
Boolean _XmSelectionBoxMatch(XmSelectionBoxWidget w);


/* for TearOff */
void _XmPushButtonSetTranslation(Widget, int);

/* Text */
void _XmChangeVSB(XmTextWidget w, XmTextPosition pos);
void _XmRedisplayHBar(XmTextWidget w, int offset);
int  _XmFontCalculateAverageCharacterWidth(Widget w, XFontStruct *fs);

void _XmCursorOverstrike(Widget w); /* formerly: CursorOverstrike */

/* Traversal */
void _XmSetFocusResetFlag(Widget w, Boolean value);
Boolean _XmGetFocusResetFlag(Widget w);
Widget _XmGetActiveTabGroup(Widget widget);
Widget _XmFindTopMostShell(Widget widget);

/* VirtKeys */
/*
 * This is for handling of ALT, META, and other modifier keys when parsing
 * the virtual binding(s). In contrast to the closed software foundation,
 * LessTif is able to configure the current setting of modifiers and to
 * adjust itself to the right modifier masks.
 * USING THIS STUFF WILL MAKE YOUR APPLICATION MORE USEFUL BUT WILL BREAK
 * THE COMPATIBILITY. THIS IS PRIMARILY FOR INTERNAL USE.
 */
typedef enum _XmModifierLevels {
    ALTModifier = 0,
    METAModifier,
    SUPERModifier,
    HYPERModifier,
    /* This one must be the last! */ MAX_MODIFIERS
} XmModifierLevels;

typedef Modifiers XmModifierMaskSet[MAX_MODIFIERS];
typedef Modifiers *XmModifierMaskSetReference;

XmModifierMaskSetReference _XmGetModifierMappingsForDisplay(Display *Dsp);
void _XmInvalidateModifierMappingsForDisplay(Display *Dsp);
void _XmRefreshVirtKeys(Widget w);

/* Visual */

void _XmAddBackgroundToColorCache(Screen *screen, Colormap color_map,
                                  String background_spec,
                                  int rgb_fallback, XrmValue *val);
void _XmInvalidateColorCache(Boolean default_only);
void _XmPickupUnspecifiedPixmaps(Display *dpy);

/* XmString */

void _XmStringBaselines(XmFontList fontlist, _XmString string,
                        Position y, Dimension *baselines);
void _XmStringUpdate(XmFontList fontlist, _XmString string);
/*
 * Behaviour control for XmFormLayout, XmRowColumnLayout.
 * This is their second parameter.
 */
#define Mode_Normal     0x00    
#define Mode_Test       0x01    
#define Mode_Resize     0x02

/*
 * Extra color tools.
 */
Pixel _XmWhitePixelOfObject(Widget);
Pixel _XmBlackPixelOfObject(Widget);

/*
 * Thread stuff (lib/Xm/misc.c).
 */
void _XmAppLock(XtAppContext appc);
void _XmAppUnlock(XtAppContext appc);
void _XmProcessLock(void);
void _XmProcessUnlock(void);
void _XmObjectLock(Widget w);
void _XmObjectUnlock(Widget w);


/*
 * from LTCvt.c
 */
#ifdef NONSTANDARD_CONVERTERS
void _XmRegisterNSEConverters(void);
#endif

/*
 * from Shadow.c 
 */
/* old backward compatibility stuff */
void _XmDrawSquareButton(Widget w, int x, int y, int size,
                         GC top_gc, GC bottom_gc, GC select_gc, Boolean fill);
void _XmDrawDiamondButton(Widget w, int x, int y, int size,
                          GC top_gc, GC bottom_gc, GC select_gc, Boolean fill);

void _XmDrawPolygonShadow(Display *display, Drawable drawable,
                          GC top_gc, GC bottom_gc, XPoint *points, int point_count,
                          Dimension shadow_thickness, unsigned char shadow_type);


/*
 * From Xmos.c: private, but exported stuff from Motif 1.2
 * which is used within LessTif, also for the 2.x libs
 */
XmString 
_XmOSGetLocalizedString(char *reserved, Widget w, String resourceName, String value);
int _XmMicroSleep(long secs);
String _XmOSGetHomeDirName(void);
String _XmOSInitPath(String file_name, String env_pathname, Boolean *user_path);
void _XmSleep(unsigned int secs);
String _XmOSGetHomeDirName(void);
int _XmOSFileCompare(XmConst void *sp1, XmConst void *sp2);
void _XmOSQualifyFileSpec(String dirSpec, String filterSpec,
		String *pQualifiedDir, String *pQualifiedPattern);
String _XmOSFindPatternPart(String fileSpec);
void _XmOSGetDirEntries(String qualifiedDir, String matchPattern,
		unsigned char fileType, Boolean matchDotsLiterally,
		Boolean listWithFullPath, String **pEntries,
		unsigned int *pNumEntries, unsigned int *pNumAlloc);
void _XmOSBuildFileList(String dirPath, String pattern,
		unsigned char typeMask, String **pEntries,
		unsigned int *pNumEntries, unsigned int *pNumAlloc);


/* The following are definitions for 2.x stuff, work in progress.
   Since are not public they can be changed at any time ... ?!? */
struct _XmTabRec {
	float value;
	unsigned char units;
	XmOffsetModel offset_model;
	unsigned char alignment;
	char *decimal;
};

struct _XmTabListRec {
	struct _XmTabRec	**tabs;
	int			count;
};

/* defined in Trait.c, but used outside; not a symbol from OSF/Motif */
void _XmInitTraits(void);

#ifdef	USE_XFT
/*
 * XftDraw cache functions, implemented in lib/Xm/FontList.c
 */
XftDraw * _XmXftDrawCreate(Display *dpy, Window w);
void _XmXftDrawDestroy(Display *dpy, Window w, XftDraw *d);
void _XmXftDrawString(Display *dpy, Window wd, XmRendition r, int bpc, Position x, Position y, char *s, int len);
void _XmXftFontAverageWidth(Widget w, XtPointer f, int *wid, int *ht);
#endif

/* from Vendor.c */
extern Boolean _LtCheckClassOfVendorShell(Widget w);

/*
 *   Some general purpose macros follow:
 */
#define _XmMin(a,b) (((a)<(b))?(a):(b))
#define _XmMax(a,b) (((a)>(b))?(a):(b))


#endif /* _XMI_XMI_H */
