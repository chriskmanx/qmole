/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextF.c,v 1.11 2008/01/02 19:42:57 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/TextF.c,v 1.11 2008/01/02 19:42:57 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <string.h>
#include <stddef.h>    /* for  wchar_t */
#include <limits.h>    /* for INT_MAX */
#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif

#include <X11/Xatom.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/DrawP.h>
#include <Xm/ScreenP.h>
#include <Xm/TextFP.h>
#include <Xm/TransltnsP.h>
#include <Xm/CutPaste.h>
#include <Xm/AtomMgr.h>
#include <Xm/DragC.h>
#include <Xm/DropSMgr.h>
#include <Xm/DropTransP.h>

#include <Xm/XmIm.h>
/* For traits : */
#include <Xm/TraitP.h>
#include <Xm/AccTextT.h>

#include <XmI/AtomMgrI.h>
#include <XmI/DragDropI.h>
#include <XmI/MacrosI.h>

#include <XmI/DebugUtil.h>

static void _XmTextFSetRenderTable(Widget w, int o, XrmValue *v);
/*
 * There was an old comment here saying to look in the mailing
 * list archive for the reason why we were not using shared GCs.
 * However, the XmText widget appears to work well this way
 * (with shared GCs) so I'm changing the default here too.
 * This means we'll use XtAllocateGC in XmText and XmTextField
 * from now on.
 * Hmm backing it out again, shared GCs require you to set the
 * dynamic fields before *every* use of the GC. This is probably
 * not a good idea here.
 *
 * Please look at comments in lib/Xm/Text.c - all this GC sharing
 * business is a good idea, but not in these circumstances.
 * Leave these comments here so we don't make the same mistakes
 * four or five times !
 */
#undef	USE_SHARED_GC

#undef	TextF_FontTextWidth
#define TextF_FontTextWidth(w,s,l)	(int)_XmTextF_FontTextWidth((Widget)w, s, l)

/* For 2.0 width calculation behavior : */
#define USE_AVERAGE_WIDTH 1

static XtPointer _XmTextF_TraitGetValue(Widget w, int format);
static void _XmTextF_TraitSetValue(Widget w, XtPointer value, int format);
static int _XmTextF_TraitPreferredFormat(Widget w);

static XtTranslations tf_evnt2_trans,tf_evnt3_trans;

#define	DBG(a) \
	DEBUGOUT(_LtDebug(__FILE__, NULL, "%s\n", a))
#define DBGW(a) \
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "%s\n", a))
#define DBG1(a,b) \
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "%s: %d\n", a, (int)b))

typedef void (*RedrawProc)(XmTextFieldWidget);

static void _XmTextFieldSetEditable(Widget w, Boolean e);
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget, Widget, ArgList, Cardinal *);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static void resize(Widget w);
static Boolean set_values(Widget, Widget, Widget, ArgList, Cardinal *);
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void _XmTextFieldExportValue(Widget w, int offset, XtArgVal *value);
static Boolean drag_convert_proc(Widget w, Atom *selection,
				 Atom *target, Atom *type_return,
				 XtPointer *value_return,
				 unsigned long *length_return,
				 int *format_return);
static void drag_transfer_proc(Widget dw, XtPointer client, Atom *selection,
				 Atom *type,
				 XtPointer value,
				 unsigned long *length,
				 int *format);
static void drag_drop_finish(Widget w,
			     XtPointer client_data,
			     XtPointer call_data);
static void process_drop(Widget w,
			     XtPointer client_data,
			     XtPointer call_data);


int _XmTextF_FontTextWidth(Widget w, char *s, int l);

enum ModifyTextCursorPosition {
  CP_Reset,             /* Reset cursor position to 0 */
  CP_StartOfInsert,     /* Leave cursor at start of inserted text */
  CP_EndOfInsert,       /* Leave cursor at end of inserted text */
  CP_Unchanged          /* Don't change insert position */
} ;

static XmTextScanType default_selection_array[] = {
	XmSELECT_POSITION,	/* One click */
	XmSELECT_WORD,		/* Two clicks */
	XmSELECT_LINE,		/* Guess what ? */
};

/* Trait record */
XmAccessTextualTraitRec  _XmTextFTraitRec =
	{	/* version */		0,
		/* getValue */		_XmTextF_TraitGetValue,
		/* setValue */		_XmTextF_TraitSetValue,
		/* preferredFormat */	_XmTextF_TraitPreferredFormat,
};

static int default_selection_array_count = XtNumber(default_selection_array);

#define Offset(field) XtOffsetOf(XmTextFieldRec, text.field)
#define OffsetOf(rec,field) XtOffsetOf(XmTextFieldRec, rec.field)

/* Resources for the TextField class */
static XtResource resources[] =
{
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNlosingFocusCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(losing_focus_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNfocusCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(focus_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNmodifyVerifyCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(modify_verify_callback),
	XtRCallback, (XtPointer)NULL
    },
    {
	XmNmodifyVerifyCallbackWcs, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(wcs_modify_verify_callback),
	XtRCallback, (XtPointer)NULL
    },
    {
	XmNmotionVerifyCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(motion_verify_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNgainPrimaryCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(gain_primary_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNlosePrimaryCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(lose_primary_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNvalueChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNvalue, XmCValue, XmRString,
	sizeof(String), Offset(value),
	XmRString, (XtPointer)""
	/*XtRImmediate, (XtPointer)XmUNSPECIFIED*/
	/* FIX ME: Motif has XmRString, wacky value here */
    },
    {
	XmNvalueWcs, XmCValueWcs, XmRValueWcs,
	sizeof(wchar_t *), Offset(wc_value),
	XmRString, (XtPointer)NULL
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)5
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)5
    },
    {
	XmNcursorPosition, XmCCursorPosition, XmRTextPosition,
	sizeof(XmTextPosition), Offset(cursor_position),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNcolumns, XmCColumns, XmRShort,
	sizeof(short), Offset(columns),
	XmRImmediate, (XtPointer)20
    },
    {
	XmNmaxLength, XmCMaxLength, XmRInt,
	sizeof(int), Offset(max_length),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNblinkRate, XmCBlinkRate, XmRInt,
	sizeof(int), Offset(blink_rate),
	XmRImmediate, (XtPointer)500
    },
    /*
     * FontList and RenderTable resources : order is important !
     * check_set_render_table must be initialized before call
     * to _XmTextFSetRenderTable().
     */
    {
	"keep.off", "Keep.off", XmRBoolean,
	sizeof(Boolean), Offset(check_set_render_table),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font_list),
	XmRCallProc, (XtPointer)_XmTextFSetRenderTable
    },
    {
	XmNrenderTable, XmCRenderTable, XmRRenderTable,
	sizeof(XmFontList), Offset(font_list),
	XmRCallProc, (XtPointer)_XmTextFSetRenderTable
    },
    /* End of fontList and renderTable */
    {
	XmNselectionArray, XmCSelectionArray, XmRPointer,
	sizeof(XtPointer), Offset(selection_array),
	XmRImmediate, (XtPointer)default_selection_array /*NULL*/
	/* FIX ME: Motif has XmRInt, wacky value here */
    },
    {
	XmNselectionArrayCount, XmCSelectionArrayCount, XmRInt,
	sizeof(int), Offset(selection_array_count),
	XmRInt, (XtPointer)&default_selection_array_count /*3*/
	/* FIX ME: Motif has XmRInt, (XtPointer)whacko value here */
    },
    {
	XmNresizeWidth, XmCResizeWidth, XmRBoolean,
	sizeof(Boolean), Offset(resize_width),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpendingDelete, XmCPendingDelete, XmRBoolean,
	sizeof(Boolean), Offset(pending_delete),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNeditable, XmCEditable, XmRBoolean,
	sizeof(Boolean), Offset(editable),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNcursorPositionVisible, XmCCursorPositionVisible, XmRBoolean,
	sizeof(Boolean), Offset(cursor_position_visible),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNverifyBell, XmCVerifyBell, XmRBoolean,
	sizeof(Boolean), Offset(verify_bell),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNselectThreshold, XmCSelectThreshold, XmRInt,
	sizeof(int), Offset(threshold),
	XmRImmediate, (XtPointer)5
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), XtOffsetOf(XmTextFieldRec, primitive.navigation_type),
	XmRImmediate, (XtPointer)XmTAB_GROUP
    },
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNvalue,
	sizeof(String), Offset(value),
	_XmTextFieldExportValue , NULL
    },
    {
	XmNvalueWcs,
	sizeof(wchar_t *), Offset(wc_value),
	NULL /* FIX ME */ , NULL
    },
};

#undef Offset
#undef OffsetOf

static void TextSet(XmTextFieldWidget w, char *s);
static void Draw(XmTextFieldWidget w);
static void DrawAll(XmTextFieldWidget w);
static void DrawInsert(XmTextFieldWidget w);
static void MassiveChangeDraw(XmTextFieldWidget w);
static void DrawTextReposition(XmTextFieldWidget);
static void ClearHighlight(XmTextFieldWidget w);
static void DrawSingleHighlight(XmTextFieldWidget w);
static void CursorDraw(XmTextFieldWidget w);
static void CursorErase(XmTextFieldWidget w);
static Boolean DoCursorMove(XmTextFieldWidget w, XEvent *ev, 
                            XmTextPosition pos, Boolean highlight, 
                            Boolean drawit) ;
static Boolean CursorPosition(XmTextFieldWidget w);
static Boolean CursorMassiveAdjust(XmTextFieldWidget w);
static void CursorSet(XmTextFieldWidget w, Boolean focus);
static void VerifyBell(XmTextFieldWidget w);
static void PrimarySelectionComplete(XmTextFieldWidget w, Time time);

void _XmTextFieldFocusIn(Widget, XEvent *, String *, Cardinal *);
void _XmTextFieldFocusOut(Widget, XEvent *, String *, Cardinal *);
static void activate(Widget, XEvent *, String *, Cardinal *);
static void backward_character(Widget, XEvent *, String *, Cardinal *);
static void backward_word(Widget, XEvent *, String *, Cardinal *);
static void beep(Widget, XEvent *, String *, Cardinal *);
static void beginning_of_line(Widget, XEvent *, String *, Cardinal *);
static void clear_selection(Widget, XEvent *, String *, Cardinal *);
static void copy_clipboard(Widget, XEvent *, String *, Cardinal *);
static void copy_primary(Widget, XEvent *, String *, Cardinal *);
static void copy_to(Widget, XEvent *, String *, Cardinal *);
static void cut_clipboard(Widget, XEvent *, String *, Cardinal *);
static void cut_primary(Widget, XEvent *, String *, Cardinal *);
static void delete_next_character(Widget, XEvent *, String *, Cardinal *);
static void delete_previous_character(Widget, XEvent *, String *, Cardinal *);
static void delete_next_word(Widget, XEvent *, String *, Cardinal *);
static void delete_previous_word(Widget, XEvent *, String *, Cardinal *);
static void delete_selection(Widget, XEvent *, String *, Cardinal *);
static void delete_to_end_of_line(Widget, XEvent *, String *, Cardinal *);
static void delete_to_start_of_line(Widget, XEvent *, String *, Cardinal *);
static void delete_all(Widget, XEvent *, String *, Cardinal *);
static void deselect_all(Widget, XEvent *, String *, Cardinal *);
static void do_quick_action(Widget, XEvent *, String *, Cardinal *);
static void end_of_line(Widget, XEvent *, String *, Cardinal *);
static void enter(Widget, XEvent *, String *, Cardinal *);
static void extend_adjust(Widget, XEvent *, String *, Cardinal *);
static void extend_end(Widget, XEvent *, String *, Cardinal *);
static void extend_start(Widget, XEvent *, String *, Cardinal *);
static void forward_character(Widget, XEvent *, String *, Cardinal *);
static void forward_word(Widget, XEvent *, String *, Cardinal *);
static void grab_focus(Widget, XEvent *, String *, Cardinal *);
/*static void Help(Widget, XEvent *, String *, Cardinal *);*/
static void insert_string(Widget, XEvent *, String *, Cardinal *);
static void key_select(Widget, XEvent *, String *, Cardinal *);
static void kill_next_character(Widget, XEvent *, String *, Cardinal *);
static void kill_next_word(Widget, XEvent *, String *, Cardinal *);
static void kill_previous_character(Widget, XEvent *, String *, Cardinal *);
static void kill_previous_word(Widget, XEvent *, String *, Cardinal *);
static void kill_selection(Widget, XEvent *, String *, Cardinal *);
static void kill_to_end_of_line(Widget, XEvent *, String *, Cardinal *);
static void kill_to_start_of_line(Widget, XEvent *, String *, Cardinal *);
static void leave(Widget, XEvent *, String *, Cardinal *);
static void move_destination(Widget, XEvent *, String *, Cardinal *);
static void move_to(Widget, XEvent *, String *, Cardinal *);
static void next_tab_group(Widget, XEvent *, String *, Cardinal *);
static void page_left(Widget, XEvent *, String *, Cardinal *);
static void page_right(Widget, XEvent *, String *, Cardinal *);
static void paste_clipboard(Widget, XEvent *, String *, Cardinal *);
static void paste_primary(Widget, XEvent *, String *, Cardinal *);
static void prev_tab_group(Widget, XEvent *, String *, Cardinal *);
static void process_bdrag(Widget, XEvent *, String *, Cardinal *);
static void process_cancel(Widget, XEvent *, String *, Cardinal *);
static void process_home(Widget, XEvent *, String *, Cardinal *);
static void process_return(Widget, XEvent *, String *, Cardinal *);
static void process_tab(Widget, XEvent *, String *, Cardinal *);
static void quick_copy_set(Widget, XEvent *, String *, Cardinal *);
static void quick_cut_set(Widget, XEvent *, String *, Cardinal *);
static void redraw_display(Widget, XEvent *, String *, Cardinal *);
static void secondary_adjust(Widget, XEvent *, String *, Cardinal *);
static void secondary_notify(Widget, XEvent *, String *, Cardinal *);
static void secondary_start(Widget, XEvent *, String *, Cardinal *);
static void select_adjust(Widget, XEvent *, String *, Cardinal *);
static void select_all(Widget, XEvent *, String *, Cardinal *);
static void select_end(Widget, XEvent *, String *, Cardinal *);
static void select_start(Widget, XEvent *, String *, Cardinal *);
static void self_insert(Widget, XEvent *, String *, Cardinal *);
static void set_anchor(Widget, XEvent *, String *, Cardinal *);
static void set_insertion_point(Widget, XEvent *, String *, Cardinal *);
static void set_selection_hint(Widget, XEvent *, String *, Cardinal *);
static void toggle_add_mode(Widget, XEvent *, String *, Cardinal *);
static void traverse_home(Widget, XEvent *, String *, Cardinal *);
static void traverse_next(Widget, XEvent *, String *, Cardinal *);
static void traverse_prev(Widget, XEvent *, String *, Cardinal *);
static void unkill(Widget, XEvent *, String *, Cardinal *);
static void unmap(Widget, XEvent *, String *, Cardinal *);

static void SetSingleHighlight(Widget, int, int);

/* action table table */

/* MLM FIX ME -- the EventBindings[1-2] (Transltns.c) are
 * current unused. */
static XtActionsRec actions[] =
{
    {"self-insert", self_insert},
    {"delete-previous-character", delete_previous_character},
    {"delete-next-character", delete_next_character},
    {"delete-previous-word", delete_previous_word},
    {"delete-next-word", delete_next_word},
    {"delete-to-end-of-line", delete_to_end_of_line},
    {"delete-to-start-of-line", delete_to_start_of_line},
    {"activate", activate},
    {"process-cancel", process_cancel},
    {"process-bdrag", process_bdrag},
    {"backward-character", backward_character},
    {"forward-character", forward_character},
    {"backward-word", backward_word},
    {"forward-word", forward_word},
    {"end-of-line", end_of_line},
    {"beginning-of-line", beginning_of_line},
    {"page-left", page_left},
    {"page-right", page_right},
    {"key-select", key_select},
    {"grab-focus", grab_focus},
    {"move-destination", move_destination},
    {"extend-start", extend_start},
    {"extend-adjust", extend_adjust},
    {"extend-end", extend_end},
    {"delete-selection", delete_selection},
    {"clear-selection", clear_selection},
    {"cut-primary", cut_primary},
    {"copy-primary", copy_primary},
    {"set-anchor", set_anchor},

    {"toggle-add-mode", toggle_add_mode},
    {"select-all", select_all},
    {"deselect-all", deselect_all},
    {"secondary-start", secondary_start},
    {"secondary-adjust", secondary_adjust},
    {"copy-to", copy_to},
    {"move-to", move_to},
    {"quick-cut-set", quick_cut_set},
    {"quick-copy-set", quick_copy_set},
    {"do-quick-action", do_quick_action},
    {"cut-clipboard", cut_clipboard},
    {"copy-clipboard", copy_clipboard},
    {"paste-clipboard", paste_clipboard},
    {"traverse-next", traverse_next},
    {"traverse-prev", traverse_prev},
    {"traverse-home", traverse_home},
    {"next-tab-group", next_tab_group},
    {"prev-tab-group", prev_tab_group},
    {"focusIn", _XmTextFieldFocusIn},
    {"focusOut", _XmTextFieldFocusOut},
    {"enter", enter},
    {"leave", leave},

    {"beep", beep},
    {"delete-all", delete_all},
    {"Help", _XmPrimitiveHelp},
    /* rws 1 Jan 1998
    {"Help", Help},
    */
    {"insert-string", insert_string},
    {"kill-next-character", kill_next_character},
    {"kill-next-word", kill_next_word},
    {"kill-previous-character", kill_previous_character},
    {"kill-previous-word", kill_previous_word},
    {"kill-selection", kill_selection},
    {"kill-to-end-of-line", kill_to_end_of_line},
    {"kill-to-start-of-line", kill_to_start_of_line},
    {"paste-primary", paste_primary},
    {"process-home", process_home},
    {"process-return", process_return},
    {"process-tab", process_tab},
    {"redraw-display", redraw_display},
    {"secondary-notify", secondary_notify},
    {"select-adjust", select_adjust},
    {"select-end", select_end},
    {"select-start", select_start},
    {"set-insertion-point", set_insertion_point},
    {"set-selection-hint", set_selection_hint},
    {"unkill", unkill},
    {"unmap", unmap},
};

#if 0
static XmBaseClassExtRec _XmTextFCoreClassExtRec =
{
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ NULL,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ {0},
    /* get_values_prehook        */ NULL,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};
#endif

XmPrimitiveClassExtRec _XmTextFPrimClassExtRec =
{
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};

XmTextFieldClassRec xmTextFieldClassRec =
{
    /* Core class part */
  {
	/* superclass            */ (WidgetClass) & xmPrimitiveClassRec,
	/* class_name            */ "XmTextField",
	/* widget_size           */ sizeof(XmTextFieldRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmTextF_EventBindings1,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL /*XtInheritDisplayAccelerator*/,
	/* extension             */ (XtPointer)NULL /*&_XmTextFCoreClassExtRec*/
  },
    /* Primitive Class part */
  {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ NULL,
	/* arm_and_activate_proc */ NULL,
	/* synthetic resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer) & _XmTextFPrimClassExtRec,
  },
    /* TextField Class part */
  {
	/* extension */ NULL
  }
};


WidgetClass xmTextFieldWidgetClass = (WidgetClass)&xmTextFieldClassRec;


/*
 * Common initialization procedures -------------------------------------------
 */

static void
FontSize(XmTextFieldWidget w)
{
	XmFontListEntry entry = NULL;
	XFontStruct *fs = NULL;
	int i, j, num, max, min;

    for (i = 0; TextF_FontList(w)->renditions[i]->tag != NULL; i++) {
	if (!strcmp(XmFONTLIST_DEFAULT_TAG, TextF_FontList(w)->renditions[i]->tag))
	{
	    entry = TextF_FontList(w)->renditions[i];
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"FontSize: XmFONTLIST_DEFAULT_TAG\n"));
	    break;
	}
    }

    if (!entry)
    {
	/* We didn't find a decent font list; try again but be more lenient */
	for (i = 0; TextF_FontList(w)->renditions[i]->tag != NULL; i++) {
	    if (!strcmp(XmSTRING_DEFAULT_CHARSET,
	    		TextF_FontList(w)->renditions[i]->tag)) {
		entry = TextF_FontList(w)->renditions[i];
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"FontSize: XmSTRING_DEFAULT_CHARSET\n"));
		break;
	    }
	}
    }

    if (!entry)
    {
	/* We didn't find a decent font list; try again but be more lenient */
	for (i = 0; TextF_FontList(w)->renditions[i]->tag != NULL; i++) {
	    if (!strcmp("", TextF_FontList(w)->renditions[i]->tag)) {
		entry = TextF_FontList(w)->renditions[i];
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"FontSize: empty tag\n"));
		break;
	    }
	}
    }

    if (! entry) {	/* Grab the first thing you find */
	if (TextF_FontList(w) && TextF_FontList(w)->renditions)
		entry = TextF_FontList(w)->renditions[0];
    }

#ifdef	USE_XFT
	if (entry && entry->type == XmFONT_IS_XFT) {
		/* FIX ME */
		TextF_UseXft(w) = True;
	} else
#endif
		TextF_UseXft(w) = False;

    /* Still nothing; grab the default */
    if (!entry) {
	XmFontList	fl = TextF_FontList(w);
	TextF_FontList(w) = _XmFontListCreateDefault(XtDisplay((Widget)w));
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"TF FontSize: overwriting FontList (old %p new %p)\n",
		fl, TextF_FontList(w)));
	entry = TextF_FontList(w)->renditions[0];
    }
#ifdef	USE_XFT
    if (entry->type == XmFONT_IS_XFT) {
	TextF_FontAverageWidth(w) = entry->xft_font->max_advance_width;
	TextF_FontAverageWidth(w) = entry->font_average_width;	/* FIX ME HACK */
	/* FIX ME */
    } else
#endif
    if (entry->type == XmFONT_IS_FONTSET) {
	XFontStruct **foo;
	char **bar;

	if ((num = XFontsOfFontSet((XFontSet)(entry->font), &foo, &bar)) < 1) {
	    fs = NULL;
	} else {
	    fs = foo[0];
	    max = 0; min = INT_MAX;
	    for (j=0; j<num; j++){
		    if (foo[j]->max_bounds.width  > max)
			    max = foo[j]->max_bounds.width;
		    if (foo[j]->min_bounds.width  < min)
			    min = foo[j]->min_bounds.width;
	    }
	    TextF_FontAverageWidth(w) = (max + min) / 2;
        }
    } else {
	fs = (XFontStruct *)entry->font;
    	TextF_FontAverageWidth(w) =
		(fs->max_bounds.width + fs->min_bounds.width) / 2;
    }
    TextF_Font(w) = fs;

#ifdef	USE_XFT
    if (entry->type == XmFONT_IS_XFT) {
	TextF_FontHeight(w) = entry->xft_font->height;
	/*
	 * TextF_FontAscent(w) and TextF_FontDescent(w) are macros that point
	 * into the TextF(w).font structure. So in the XFT case they must not be
	 * used.
	TextF_FontAscent(w) = entry->xft_font->ascent;
	TextF_FontDescent(w) = entry->xft_font->descent;
	 */
    } else
#endif
	TextF_FontHeight(w) = TextF_FontAscent(w) + TextF_FontDescent(w);

    if ( TextF_FontAverageWidth(w) <= 0 ) {
	TextF_FontAverageWidth(w) = _XmFontCalculateAverageCharacterWidth((Widget)w, fs);
    }

/*
 *	This will not work for Xft
 *	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
 *		      "FontInit: ascent=%d descent=%d height=%d avewidth=%d\n",
 *			  TextF_FontAscent(w), TextF_FontDescent(w),
 *			  TextF_FontHeight(w), TextF_FontAverageWidth(w)));
 */
}

static void
FontInitialize(XmTextFieldWidget w)
{
    /* If FontList is NULL, track up the widget hierarchy for a VendorShell
     * or a BulletinBoard & use that FontList
     */
    if (!TextF_FontList(w))
    {
	Widget parent = XtParent(w);

	while (parent)
	{
	    if (XmIsBulletinBoard(parent) || XmIsVendorShell(parent)) {
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			"TF FontInitialize: overwriting FontList\n"));
		XtVaGetValues(parent,
			      XmNtextFontList, &TextF_FontList(w),
			      NULL);
                /* Watch out, the parent might free its copy. */
                TextF_FontList(w) = XmFontListCopy(TextF_FontList(w));
                TextF_FontListCreated(w) = True;
		break;
	    }

	    parent = XtParent(parent);
	}
	/* If the FontList is still NULL or we have an unspecified font, use
	 * the default.
	 */
	if (!TextF_FontList(w))
	{
	    TextF_FontList(w) = _XmGetDefaultFontList((Widget)w, XmTEXT_FONTLIST);

            /* _XmGetDefaultFontList has been changed to return a copy. */
            TextF_FontListCreated(w) = True;
	}
    }
    else
    {
	/* we created this, so free it when the time comes */
	TextF_FontListCreated(w) = True;
    }

    FontSize(w);
}

static void
ChangeDrawGC(XmTextFieldWidget w)
{
    XGCValues values;
    XtGCMask mask, dynamic, dontcare;

    if (TextF_DrawGC(w)) {
#ifdef USE_SHARED_GC
       XtReleaseGC((Widget)w, TextF_DrawGC(w));
#else
       XFreeGC(XtDisplay(w), TextF_DrawGC(w));
#endif
     }

    /*
     * DrawGC
     *      Need to change :        ClipRectangles
     */
    mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
    dynamic = GCClipMask | GCClipXOrigin | GCClipYOrigin |
	GCForeground | GCBackground;
    dontcare = 0;

    values.line_style = LineSolid;
    values.line_width = 0;
    values.fill_style = FillSolid;
#ifdef	USE_XFT
    if (! TextF_UseXft(w)) {
	values.font = TextF_Font(w)->fid;
	mask |= GCFont;
    }
#else
    mask |= GCFont;
    values.font = TextF_Font(w)->fid;
#endif
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.clip_x_origin = 0;
    values.clip_y_origin = 0;

	if (! w->core.sensitive) {
		Screen	*s = XtScreen((Widget)w);

		mask |= GCStipple;
		values.fill_style = FillStippled;
		values.stipple = XmGetPixmapByDepth(s, "50_foreground", 1, 0, 1);
	}

#ifdef	USE_SHARED_GC
    TextF_DrawGC(w) = XtAllocateGC((Widget)w, 0, mask, &values,
				   dynamic, dontcare);
#else
    TextF_DrawGC(w) = XCreateGC(XtDisplay(w), XtWindow(w), mask, &values);
#endif
    TextF_DrawGCInverted(w) = False;
}


static void
GCInitialize(XmTextFieldWidget w)
{
    XGCValues values;
    XtGCMask mask, dynamic, dontcare;

    TextF_DrawGC(w) = NULL;
    ChangeDrawGC(w);

    /*
     * Cursor GC
     *      Need to change :        Tile/Stipple Origin, Stipple
     */
    values.line_style = LineSolid;
    values.line_width = 0;
    values.fill_style = FillStippled;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    mask = GCLineStyle | GCLineWidth | GCFillStyle |
	GCForeground | GCBackground;
    dynamic = GCTileStipXOrigin | GCTileStipYOrigin | GCStipple;
    dontcare = 0;
#ifdef	USE_SHARED_GC
    TextF_CursorGC(w) = XtAllocateGC((Widget)w, 0, mask, &values,
				     dynamic, dontcare);
#else
    TextF_CursorGC(w) = XCreateGC(XtDisplay(w), XtWindow(w), mask, &values);
#endif

    /*
     * CopyGC
     *      At least this one is not changed anywhere :-)
     */
    values.line_style = LineSolid;
    values.line_width = 0;
    values.fill_style = FillSolid;
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);
    mask = GCLineStyle | GCLineWidth | GCFillStyle |
	GCForeground | GCBackground;

    TextF_CopyGC(w) = XtGetGC((Widget)w, mask, &values);
}

static void
GCPixmap(XmTextFieldWidget w)
{
	static char dots[] = {2, 1, 1};
	GC gc;
	int width, width_erase, height;
	Display *display;
	XGCValues values;
	XtGCMask mask;

	values.line_style = LineSolid;
	values.line_width = 0;
	values.fill_style = FillSolid;
	values.foreground = 0;
	values.background = 0;

	display = XtDisplay((Widget)w);
	width = 5;
#ifdef	USE_XFT
	if (TextF_UseXft(w)) {
		/* Help !! Not sure why this is needed.
		* Crash in nedit's font browser when XFT is enabled. */
		if (TextF_XftFont(w)) {
			width_erase = 2 * TextF_XftFont(w)->max_advance_width;
			height = TextF_XftFont(w)->height;
		} else {
			if (w->text.font_list->renditions[0]->type == XmFONT_IS_FONT) {
				XFontStruct *fs = w->text.font_list->renditions[0]->font;
				width_erase = 2 * fs->max_bounds.width;
				height = fs->max_bounds.ascent
				+ fs->max_bounds.descent;
			} else {
				width_erase = 2 * TextF_Font(w)->max_bounds.width;
				height = TextF_Font(w)->max_bounds.ascent
					+ TextF_Font(w)->max_bounds.descent;
			}
		}
	} else {
		width_erase = 2 * TextF_Font(w)->max_bounds.width;
		height = TextF_FontHeight(w);
	}
#else
	width_erase = 2 * TextF_Font(w)->max_bounds.width;
	height = TextF_FontHeight(w);
#endif
	if (TextF_CursorIBeam(w)) {
		XFreePixmap(XtDisplay((Widget)w), TextF_CursorIBeam(w));
	}

	TextF_CursorIBeam(w) = (Pixmap)NULL;
	if (TextF_CursorStipple(w)) {
		XFreePixmap(XtDisplay((Widget)w), TextF_CursorStipple(w));
	}

	TextF_CursorStipple(w) = (Pixmap)NULL;
	if (TextF_CursorSave(w)) {
		XFreePixmap(XtDisplay((Widget)w), TextF_CursorSave(w));
	}

	TextF_CursorSave(w) = (Pixmap)NULL;
	TextF_CursorSaveValid(w) = False;

	if (height > 0) {
		TextF_CursorIBeam(w) = XCreatePixmap(display,
				RootWindowOfScreen(XtScreen((Widget)w)),
				width, height, 1);

		TextF_CursorStipple(w) = XCreatePixmap(display,
				RootWindowOfScreen(XtScreen((Widget)w)),
				width, height, 1);

		TextF_CursorSave(w) = XCreatePixmap(display,
				RootWindowOfScreen(XtScreen((Widget)w)),
				width_erase, height, w->core.depth);

		values.line_style = LineSolid;
		values.line_width = 0;
		values.fill_style = FillSolid;
		values.foreground = 0;
		values.background = 0;
		mask = GCLineStyle | GCLineWidth | GCFillStyle | GCForeground | GCBackground;
		gc = XCreateGC(display, TextF_CursorIBeam(w), mask, &values);

		XFillRectangle(display, TextF_CursorIBeam(w), gc, 0, 0, width, height);
		XFillRectangle(display, TextF_CursorStipple(w), gc, 0, 0, width, height);

		XSetForeground(display, gc, 1);
		XDrawLine(display, TextF_CursorIBeam(w), gc, 2, 1, 2, height - 2);
		XDrawLine(display, TextF_CursorIBeam(w), gc, 0, 0, 4, 0);
		XDrawLine(display, TextF_CursorIBeam(w), gc, 0, height - 1, 4, height - 1);

		XSetLineAttributes(display, gc, 0, LineOnOffDash, CapRound, JoinRound);
		XSetDashes(display, gc, 0, &dots[1], (int)dots[0]);
		XDrawLine(display, TextF_CursorStipple(w), gc, 2, 1, 2, height - 2);
		XDrawLine(display, TextF_CursorStipple(w), gc, 1, 0, 3, 0);
		XDrawLine(display, TextF_CursorStipple(w), gc, 1, height - 1, 3, height - 1);

		XFreeGC(display, gc);
	}
}

static void
GCClip(XmTextFieldWidget w)
{
	XRectangle clip;
	Dimension	bw, bh;

	bw = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
		+ TextF_MarginWidth(w);
	bh = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
		TextF_MarginHeight(w);
	clip.x = 0;
	clip.y = 0;
	clip.width = TextF_ViewWidth(w);
	clip.height = TextF_ViewHeight(w);

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "GCClip(%d,%d)\n",
		TextF_ViewWidth(w), TextF_ViewHeight(w)));

#ifdef	USE_XFT
	if (TextF_UseXft(w)) {
#if 1
		_XmXftSetClipRectangles((Widget)w,
			TextF_XDraw(w), TextF_YDraw(w), &clip, 1);
#else
		_XmXftSetClip((Widget)w,
			TextF_XDraw(w), TextF_YDraw(w),
			TextF_ViewWidth(w), TextF_ViewHeight(w));
#endif
	} else
#endif
	{
		/* These calls change the GCs */
		XSetClipRectangles(XtDisplay((Widget)w), TextF_DrawGC(w),
			TextF_XDraw(w), TextF_YDraw(w), &clip, 1, Unsorted);
	}

	GCPixmap(w);
	CursorSet(w, TextF_HasFocus(w));
}

static void
SizeRecalc(XmTextFieldWidget w)
{
	Dimension	bw, bh;

	bw = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
		+ TextF_MarginWidth(w);
	bh = Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
		TextF_MarginHeight(w);

	TextF_ViewWidth(w) = XtWidth(w) - 2 * bw;
	TextF_XDraw(w) = bw;
	TextF_YDraw(w) = bh;

#ifdef	USE_XFT
	if (TextF_UseXft(w)) {
		TextF_YOffset(w) = TextF_YDraw(w) + TextF_XftFont(w)->ascent;
		TextF_ViewHeight(w) = TextF_XftFont(w)->height;
	} else
#endif
	{
		TextF_YOffset(w) = TextF_YDraw(w) + TextF_FontAscent(w);
		TextF_ViewHeight(w) = TextF_FontHeight(w);
	}

	if (TextF_YDraw(w) + TextF_ViewHeight(w) > XtHeight(w)
			- Prim_HighlightThickness(w) - Prim_ShadowThickness(w)) {
		TextF_ViewHeight(w) = XtHeight(w) - TextF_YDraw(w) -
			Prim_HighlightThickness(w) - Prim_ShadowThickness(w);
	}

#ifdef	USE_XFT
	if (TextF_UseXft(w)) {
		TextF_Columns(w) = (XtWidth(w)
		- 2 * Prim_ShadowThickness(w)
		- 2 * TextF_MarginWidth(w)) / TextF_XftFont(w)->max_advance_width;
		TextF_Columns(w) = (XtWidth(w)
		- 2 * Prim_ShadowThickness(w)
		- 2 * TextF_MarginWidth(w)) / TextF_FontAverageWidth(w);	/* FIX ME HACK */
	} else
#endif
	{
#ifdef	USE_AVERAGE_WIDTH
		TextF_Columns(w) = (XtWidth(w)
			- 2 * Prim_ShadowThickness(w)
			- 2 * TextF_MarginWidth(w)) / TextF_FontAverageWidth(w);
#else
		if (TextF_FontMaxWidth(w))
			TextF_Columns(w) = (XtWidth(w)
				- 2 * Prim_ShadowThickness(w)
				- 2 * TextF_MarginWidth(w)) / TextF_FontMaxWidth(w);
		else if (TextF_FontAverageWidth(w))
			TextF_Columns(w) = (XtWidth(w)
				- 2 * Prim_ShadowThickness(w)
				- 2 * TextF_MarginWidth(w)) / TextF_FontAverageWidth(w);
#endif
	}

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"SizeRecalc size %d %d -> offset %d,%d view dim %d %d cols %d\n",
		XtWidth(w), XtHeight(w),
		TextF_XOffset(w), TextF_YOffset(w),
		TextF_ViewWidth(w), TextF_ViewHeight(w),
		TextF_Columns(w)));
}

static void
TextF_InitialiseHighlight(Widget w)
{
    /* This is called repeatedly - avoid memory leak
    ** reuse original allocation!
    */
    if (TextF_Highlight(w).list == NULL)
    {
	TextF_Highlight(w).list = (_XmHighlightRec *)XtCalloc(8, sizeof(_XmHighlightRec));
	TextF_Highlight(w).maximum = 8;
    }

    TextF_Highlight(w).number = 2;
    TextF_Highlight(w).list[0].position = 0;
    TextF_Highlight(w).list[0].mode = XmHIGHLIGHT_NORMAL;
    TextF_Highlight(w).list[1].position = INT_MAX;
    TextF_Highlight(w).list[1].mode = XmHIGHLIGHT_NORMAL;
    TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
}

/*
 * Methods --------------------------------------------------------------------
 */

static void
class_initialize(void)
{
    /* Motif does not have this method */
    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmTextField class_initialize\n"));

    if (!XmeTraitSet((XtPointer)xmTextFieldWidgetClass,
   	             XmQTaccessTextual, (XtPointer)&_XmTextFTraitRec))
    {
	_XmWarning(NULL, "XmTextField ClassInitialize: XmeTraitSet failed\n");
    }
    tf_evnt2_trans=XtParseTranslationTable(_XmTextF_EventBindings2);
    tf_evnt3_trans=XtParseTranslationTable(_XmTextF_EventBindings3);
}


static void
class_part_initialize(WidgetClass widget_class)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmTextField class_part_initialize\n"));
    _XmFastSubclassInit(widget_class, XmTEXT_FIELD_BIT);
}


static void
initialize(Widget request, Widget tnew, ArgList args, Cardinal *num)
{
	XmTextFieldWidget	w = (XmTextFieldWidget)tnew;
	Atom			import_target[3];
	Arg			ds_args[6];
	int			n = 0;
	char			*temp;

	DEBUGOUT(_LtDebug(__FILE__, tnew,
		"initialize : %i args\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  new_w X %5i Y %5i W %5i H %5i\n",
		*num,
		XtX(request), XtY(request),
		XtWidth(request), XtHeight(request),
		XtX(tnew), XtY(tnew),
		XtWidth(tnew), XtHeight(tnew)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, tnew, args, *num, False));

	/* deal with the translations -- someone tell me if this is wrong... */
#if 1
	XtAugmentTranslations((Widget)w,tf_evnt2_trans);
	XtAugmentTranslations((Widget)w,tf_evnt3_trans);
#endif

	w->text.extension =
		(XtPointer)XtMalloc(sizeof(XmTextFieldPartLesstifExtension));

	/* set up the initial text string */
	TextF_SelectionText(w) = NULL;
	TextF_TextWidth(w) = TextF_OldTextWidth(w) = 0;

	/* Get the font information */
	if (TextF_FontList(w)) {
		XmFontList	fl = TextF_FontList(w);
		TextF_FontList(w) = XmFontListCopy(TextF_FontList(w));
#if 0
		/* This crashes several tools (xpdf, Xinvest). */
		if (fl)
			XmFontListFree(fl);
#endif
	}
	FontInitialize(w);

	if (TextF_Value(w))
		TextF_Length(w) = strlen(TextF_Value(w));
	else
		TextF_Length(w) = 0;

	TextF_Alloc(w) = TextF_Length(w) + TF_ALLOC_SIZE;

	temp = (char *)XtMalloc(TextF_Alloc(w));
	if (TextF_Value(w))
		strcpy(temp, TextF_Value(w));
	else
		strcpy(temp, "");
	TextF_Value(w) = temp;

	TextF_TextWidth(w) = TextF_FontTextWidth(w, TextF_Value(w),
						 TextF_Length(w));

	if (XtWidth(request) == (Dimension)0) {
		/* the user didn't say how wide */
#ifdef	USE_XFT
		if (TextF_UseXft(w)) {
			XtWidth(w) = (2 * Prim_ShadowThickness(w) +
				2 * Prim_HighlightThickness(w) +
				2 * TextF_MarginWidth(w) +
				TextF_Columns(w)
				* TextF_XftFont(w)->max_advance_width);
			XtWidth(w) = (2 * Prim_ShadowThickness(w) +
				2 * Prim_HighlightThickness(w) +
				2 * TextF_MarginWidth(w) +
				TextF_Columns(w)
				* TextF_XftFont(w)->max_advance_width / 2);	/* FIX ME HACK */
		} else
#endif
		{
#ifdef	USE_AVERAGE_WIDTH
			XtWidth(w) = (2 * Prim_ShadowThickness(w) +
				2 * Prim_HighlightThickness(w) +
				2 * TextF_MarginWidth(w) +
				TextF_Columns(w) * TextF_FontAverageWidth(w));
#else
			XtWidth(w) = (2 * Prim_ShadowThickness(w) +
				2 * Prim_HighlightThickness(w) +
				2 * TextF_MarginWidth(w) +
				TextF_Columns(w) * TextF_FontMaxWidth(w));
#endif
		}
	} else {
		/* the user specified a width, so fit the columns to it. */
#ifdef	USE_XFT
		if (TextF_UseXft(w)) {
			TextF_Columns(w) = (XtWidth(w)
				- 2 * Prim_ShadowThickness(w)
				- 2 * TextF_MarginWidth(w)) /
				TextF_XftFont(w)->max_advance_width;
			TextF_Columns(w) = (XtWidth(w)
				- 2 * Prim_ShadowThickness(w)
				- 2 * TextF_MarginWidth(w)) /
				TextF_FontAverageWidth(w);	/* FIX ME HACK */
		} else
#endif
		{
#ifdef	USE_AVERAGE_WIDTH
			TextF_Columns(w) = (XtWidth(w)
				- 2 * Prim_ShadowThickness(w)
				- 2 * TextF_MarginWidth(w)) /
				TextF_FontAverageWidth(w);
#else
			TextF_Columns(w) = (XtWidth(w)
				- 2 * Prim_ShadowThickness(w)
				- 2 * TextF_MarginWidth(w)) /
				TextF_FontMaxWidth(w);
#endif
		}
	}

	if (XtHeight(request) == (Dimension)0) {
#ifdef	USE_XFT
		if (TextF_UseXft(w)) {
			XtHeight(w) = (2 * Prim_ShadowThickness(w) +
				2 * Prim_HighlightThickness(w) +
				2 * TextF_MarginHeight(w) +
				TextF_XftFont(w)->height);
		} else
#endif
		{
			XtHeight(w) = (2 * Prim_ShadowThickness(w) +
				2 * Prim_HighlightThickness(w) +
				2 * TextF_MarginHeight(w) +
				TextF_FontHeight(w));
		}
	}

	SizeRecalc(w);

	/* initialize cursor position */
	if (TextF_CursorPos(w) > 0) {
		if (TextF_CursorPos(w) > TextF_Length(w)) {
			TextF_CursorPos(w) = TextF_Length(w);
		}
	} else {
		TextF_CursorPos(w) = 0;
	}

	TextF_OldCursorX(w) = -1;

	/* use TextF_InitialiseHighlight (sp) to do everything here
	** instead of later, also avoids memory leak of old XtMalloc
	** NOTE: must null out TextF_Highlight(w).list
	*/

	TextF_Highlight(w).list = NULL;
	TextF_InitialiseHighlight(tnew);

	TextF_OldHighlightStart(w) = TextF_OldHighlightEnd(w) = -1;
	TextF_XOffset(w) = TextF_OldXOffset(w) = 0;

	/* initialize booleans */
	TextF_TimerId(w) = (XtIntervalId)0;
	TextF_SelectId(w) = (XtIntervalId)0;

	w->text.last_time = (Time)0;

	w->text.sarray_index = 0;

	TextF_Echo(w) = True;
	TextF_AllowSelection(w) = True;
	TextF_HasFocus(w) = False;
	TextF_AddMode(w) = False;

	/* initialize pixmaps and graphics contexts */
	TextF_CursorIBeam(w) = (Pixmap)NULL;
	TextF_CursorStipple(w) = (Pixmap)NULL;
	TextF_CursorSave(w) = (Pixmap)NULL;

	/* Use DrawGC as a flag to see if the GCs have been created */
	TextF_DrawGC(w) = NULL;

	/* initialize default DropSite */
	import_target[0] = XmInternAtom(XtDisplay(tnew), _XA_COMPOUND_TEXT, False);
	import_target[1] = XmInternAtom(XtDisplay(tnew), _XA_TEXT, False);
	import_target[2] = XA_STRING;
	XtSetArg(ds_args[n], XmNimportTargets, import_target); n++;
	XtSetArg(ds_args[n], XmNnumImportTargets, 3); n++;
	XtSetArg(ds_args[n], XmNdropSiteOperations, XmDROP_COPY|XmDROP_MOVE); n++;
	XtSetArg(ds_args[n], XmNdropProc, process_drop); n++;

	XmDropSiteRegister(tnew, ds_args, n);

	w->text.max_char_size = 1;		/* FIX ME */
}


static void
realize(Widget aw, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "realize\n"));

#define	superclass	(&xmPrimitiveClassRec)
    (*superclass->core_class.realize) (aw, value_mask, attributes);
#undef	superclass

    GCInitialize(w);
    GCClip(w);
    CursorDraw(w);

    _XmTextFieldSetEditable(aw, TextF_Editable(w));
}


static void
destroy(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    DEBUGOUT(_LtDebug(__FILE__, aw, "destroy"));

    XmImCloseXIM(aw);

    if (TextF_SelectId(aw))
    {
	XtRemoveTimeOut(TextF_SelectId(aw));
	TextF_SelectId(aw) = 0;
    }

    if (TextF_TimerId(aw))
    {
	XtRemoveTimeOut(TextF_TimerId(aw));
	TextF_TimerId(aw) = 0;
    }

#ifdef USE_SHARED_GC
    XtReleaseGC((Widget)w, TextF_DrawGC(w));
    XtReleaseGC((Widget)w, TextF_CursorGC(w));
#else
    /* amai: this used to be the Xt call as well as used above.
             Unfortunately we either remove it at the wrong time
	     or without an appropriate check and since the X calls
	     are less forgiving I introduced this check ... */
    if(XtIsRealized(w)) {
       XFreeGC(XtDisplay(w), TextF_DrawGC(w));
       XFreeGC(XtDisplay(w), TextF_CursorGC(w));
    }
#endif
    XtReleaseGC((Widget)w, TextF_CopyGC(w));

    if (TextF_FontListCreated(w)) {
	XmFontListFree(TextF_FontList(w));
    }

    if (TextF_SelectionText(w)) {
	XtFree(TextF_SelectionText(w));
    }

    XtFree(TextF_Value(w));

#ifdef	USE_XFT
    /* FIX ME */
#endif

    XtFree((char *)w->text.extension);
    XtUninstallTranslations(aw);

    /* This is probably not necessary...
    ** Use the macro... should we null out the old pointer?
    */
    XtFree((char *)TextF_Highlight(w).list);
    TextF_Highlight(w).list = NULL;

    /* amai: not sure this is necessary ...
             Check out the register calls for more info.
    XmDropSiteUnregister(aw); */
}


static void
expose(Widget aw, XEvent *event, Region region)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    DEBUGOUT(_LtDebug(__FILE__, aw, "expose\n"));

    if (!XtIsRealized(aw)) {
	return;
    }
    /* rws 30 Mar 1998
       Xbae examples/matrix puts TextF into an infinite loop of exposes
       without this!!!!!
     */
    if (event && event->xexpose.count != 0) return;

    /* Then draw ourselves on top of it */
    CursorErase(w);
    DrawAll(w);

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		   XtWidth(w) - 2 * Prim_HighlightThickness(w),
		   XtHeight(w) - 2 * Prim_HighlightThickness(w),
		   Prim_ShadowThickness(w),
		   XmSHADOW_IN);

    /*
       pgw@hungry.com 8/17/98
       Replaced obsolete routines _XmHighlightBorder and _XmUnhighlightBorder
       with the following code which was cloned from ArrowB.c. 
    */ 
    if (Prim_Highlighted(w)) {
        (*PrimC_BorderHighlight(XtClass(w))) (aw);
    } else {
        (*PrimC_BorderUnhighlight(XtClass(w))) (aw);
    }
}


static void
resize(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    DEBUGOUT(_LtDebug(__FILE__, aw,
		      "resize => wid %d ht %d\n", XtWidth(aw), XtHeight(aw)));

    SizeRecalc(w);

    /* Only adjust the GCs if they have already been created! */
    if (TextF_DrawGC(w)) {
	GCClip(w);
    }

    if (XtIsRealized(aw)) {
	MassiveChangeDraw(w);
    }
}


static Boolean
set_values(Widget current, Widget request, Widget reply,
	   ArgList args, Cardinal *nargs)
{
    XmTextFieldWidget	w = (XmTextFieldWidget)current;
    XmTextFieldWidget	new_w = (XmTextFieldWidget)reply;
    Boolean	redraw = False;
    int		ave_width;

    DEBUGOUT(_LtDebug(__FILE__, reply,
		      "set_values: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *nargs,
		      XtX(current), XtY(current),
		      XtWidth(current), XtHeight(current),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(reply), XtY(reply),
		      XtWidth(reply), XtHeight(reply)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, reply, args, *nargs, False));

	if (new_w->core.sensitive != w->core.sensitive) {
		XGCValues	v;
		int		m;

		if (! new_w->core.sensitive) {
			m = GCFillStyle | GCStipple;
			v.fill_style = FillStippled;
			v.stipple = XmGetPixmapByDepth(XtScreen(new_w), "50_foreground", 1, 0, 1);
		} else {
			m = GCFillStyle;
			v.fill_style = FillSolid;
		}

		if (XtIsRealized(new_w))
			XChangeGC(XtDisplay(new_w), TextF_DrawGC(new_w), m, &v);
		redraw = True;
	}

	/* rws 13 Mar 1997
	 * If we are not realized there are no GC's to free and more important
	 * we cannot create any since we do not have a window.
	 */
	if (XtIsRealized(w) && (Prim_Foreground(w) != Prim_Foreground(new_w) ||
	                    XtBackground(w) != XtBackground(new_w))) {
#ifdef USE_SHARED_GC
		XtReleaseGC((Widget)w, TextF_DrawGC(w));
		XtReleaseGC((Widget)w, TextF_CursorGC(w));
#else
		XFreeGC(XtDisplay(w), TextF_DrawGC(w));
		XFreeGC(XtDisplay(w), TextF_CursorGC(w));
#endif
		XtReleaseGC((Widget)w, TextF_CopyGC(w));

		GCInitialize(new_w);
		GCClip(new_w);

		redraw = True;
	}

	if ((TextF_CursorPos(w) != TextF_CursorPos(new_w)) ||
		(TextF_CursorPositionVisible(w) != TextF_CursorPositionVisible(new_w))) {
		redraw = True;
	}
	if (TextF_Value(w) != TextF_Value(new_w)) {
		XmAnyCallbackStruct vc_cbs;
		char *temp;

		redraw = True;

		/* save ptr to the new string */
		temp = TextF_Value(new_w);

		/* need to get old address for malloc */
		TextF_Value(new_w) = TextF_Value(w);

		/* XtRealloc is called here on TextF_Value(new_w) */
		TextSet(new_w, temp);

		TextF_InitialiseHighlight((Widget)new_w);

		TextF_CursorPos(new_w) = 0;

		if (TextF_ValueChangedCallback(new_w)) {
			vc_cbs.reason = XmCR_VALUE_CHANGED;
			vc_cbs.event = NULL;
			XtCallCallbacks((Widget)new_w, XmNvalueChangedCallback, &vc_cbs);
		}

		DEBUGOUT(_LtDebug(__FILE__, reply,
			"set_values: %s\n", TextF_Value(new_w)));
	}

	if (TextF_Editable(w) != TextF_Editable(new_w)) {
		_XmTextFieldSetEditable((Widget)new_w, TextF_Editable(new_w));
		redraw = True;
	}

#ifdef	USE_XFT
	if (TextF_UseXft(w)) {
		ave_width = TextF_XftFont(w)->max_advance_width;
		ave_width = TextF_FontAverageWidth(w);	/* FIX ME HACK */
	} else
#endif
	{
#ifdef	USE_AVERAGE_WIDTH 
		ave_width = TextF_FontAverageWidth(w);
#else
		ave_width = TextF_FontMaxWidth(w);
#endif
	}

	if (TextF_FontList(w) != TextF_FontList(new_w)) {
		/* Grab this value before FontList(w) is free'd */

		/* FIX ME */
		if (TextF_FontList(new_w) == NULL) {
			FontInitialize(new_w);
		} else {
			if (TextF_FontListCreated(w))
				XmFontListFree(TextF_FontList(w));
			DEBUGOUT(_LtDebug(__FILE__, (Widget)new_w,
				"TF SetValues: copying FontList\n"));
			TextF_FontList(new_w) = XmFontListCopy(TextF_FontList(new_w));
			TextF_FontListCreated(new_w) = True;
		}

		FontSize(new_w);

	/* rws 27 Mar 1997
	 * If we are not realized there are no GC's to free and more important
	 * we cannot create any since we do not have a window.
	 */
	if (XtIsRealized(new_w)) {
		ChangeDrawGC(new_w);
	}

	redraw = True;

	/* Try to change the size */
	XtWidth(w) = (2 * Prim_ShadowThickness(w) +
		2 * Prim_HighlightThickness(w) +
		2 * TextF_MarginWidth(w) +
		TextF_Columns(w) * ave_width);
	}

	if (TextF_Columns(w) != TextF_Columns(new_w)) {
		XtWidth(new_w) = (2 * Prim_ShadowThickness(new_w) +
			  2 * Prim_HighlightThickness(new_w) +
			  2 * TextF_MarginWidth(new_w) +
			  TextF_Columns(new_w) * ave_width);
		XtHeight(w) = (2 * Prim_ShadowThickness(w) +
			2 * Prim_HighlightThickness(w) +
			2 * TextF_MarginHeight(w) +
			TextF_FontHeight(w));
	}

	return redraw;
}


static XtGeometryResult
query_geometry(Widget aw, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XtWidgetGeometry a;

	memset(&a, 0, sizeof(XtWidgetGeometry));

    if (XtIsRealized(w)) {
#ifdef	USE_XFT
	a.request_mode = CWWidth | CWHeight;
	a.width = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
		2 * TextF_MarginWidth(w) +
		TextF_Columns(w) * TextF_FontAverageWidth(w));

	if (TextF_UseXft(aw)) {
		a.height = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
			2 * TextF_MarginHeight(w) +
			TextF_XftFont(w)->ascent + TextF_XftFont(w)->descent);
	} else {
		a.height = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
			2 * TextF_MarginHeight(w) +
			TextF_Font(w)->max_bounds.ascent + TextF_Font(w)->max_bounds.descent);
	}
#else	/* USE_XFT */
#ifdef	USE_AVERAGE_WIDTH
	/* The code below (in #else) uses MAXIMUM values for the font size, where
	 * everything else in this source uses the font's average character width.
	 * Make it conform ...
	 *
	 * Danny 31/10/96
	 */
	a.request_mode = CWWidth | CWHeight;
	a.width = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
		   2 * TextF_MarginWidth(w) +
		   TextF_Columns(w) * TextF_FontAverageWidth(w));

	a.height = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
		    2 * TextF_MarginHeight(w) +
	     TextF_Font(w)->max_bounds.ascent + TextF_Font(w)->max_bounds.descent);
#else
	a.request_mode = CWWidth | CWHeight;
	a.width = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
		   2 * TextF_MarginWidth(w) +
		   TextF_Columns(w) * TextF_FontMaxWidth(w));

	a.height = (2 * Prim_ShadowThickness(w) + 2 * Prim_HighlightThickness(w) +
		    2 * TextF_MarginHeight(w) +
		    TextF_Font(w)->max_bounds.ascent +
		    TextF_Font(w)->max_bounds.descent);
#endif
#endif	/* USE_XFT */
	*answer = a;
    }
    else
    {
    	answer->width = XtWidth(aw);
    	answer->height = XtHeight(aw);
    }

    return _XmGMReplyToQueryGeometry(aw, proposed, answer);
}

/*
 * String manipulation procedures ---------------------------------------------
 */

static void
TextSet(XmTextFieldWidget w, char *s)
{
    int len;

    if (s)
    {
	len = strlen(s);
	if (len > TextF_Alloc(w))
	{
	    TextF_Alloc(w) += len;
	    TextF_Value(w) = XtRealloc(TextF_Value(w), TextF_Alloc(w));
	}

	strcpy(TextF_Value(w), s);

	TextF_Length(w) = len;
	TextF_TextWidth(w) = TextF_OldTextWidth(w) =
	    TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w));

	if ((TextF_MaxLength(w) > 0) && (TextF_Length(w) > TextF_MaxLength(w)))
	{
	    TextF_MaxLength(w) = TextF_Length(w);
	}
    }
}


static void
TextDelete(XmTextFieldWidget w, int start, int len)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "TextDelete(from %d len %d)\n", start, len));

    if (len > 0)
    {
	for (i = start + len; i < TextF_Length(w); i++)
	{
	    TextF_Value(w)[i - len] = TextF_Value(w)[i];
	}

	TextF_Length(w) -= len;
	TextF_TextWidth(w) = TextF_FontTextWidth(w, TextF_Value(w),
						 TextF_Length(w));
	TextF_Value(w)[TextF_Length(w)] = 0;
    }
}


/* returns value indicating if the text can be redrawn using the fast
 * method */
static Boolean
TextInsert(XmTextFieldWidget w, char *buf, int len)
{
    int i;
    Boolean fast_insert;

    fast_insert = True;
    if (len > 0)
    {
	if (TextF_Length(w) + len > TextF_MaxLength(w))
	{
	    VerifyBell(w);
	    fast_insert = False;
	}
	else
	{
	    if (TextF_HighlightStart(w) >= 0) {
		fast_insert = False;
	    }

	    if (TextF_Length(w) + len > TextF_Alloc(w))
	    {
		i = TF_ALLOC_SIZE;
		if (i < len)
		    i = len;
		TextF_Alloc(w) += i + 1;
		TextF_Value(w) = XtRealloc(TextF_Value(w), TextF_Alloc(w));

		DBG1("TextInsert: Alloced new space\n", TextF_Alloc(w));
	    }

	    for (i = TextF_Length(w) - 1; i >= TextF_CursorPos(w); i--)
	    {
		TextF_Value(w)[i + len] = TextF_Value(w)[i];
	    }

	    strncpy(&TextF_Value(w)[TextF_CursorPos(w)], buf, len);

	    TextF_FastInsertCursorStart(w) = TextF_CursorPos(w);
	    TextF_FastInsertTextLen(w) = len;
	    TextF_Length(w) += len;

	    TextF_TextWidth(w) = TextF_FontTextWidth(w, TextF_Value(w),
						     TextF_Length(w));
	    TextF_Value(w)[TextF_Length(w)] = 0;
	}
    }

    return fast_insert;
}


/* Replace the text from start to end with len chars of str.  Calls the
   modify verify callback to allow the applicaation chance to check the
   change, then calls TextDelete to delete the old contents followed by
   TextInsert to insert the new */
static void
ModifyText(XmTextFieldWidget w, XEvent *ev,
           int start, int end, char *str, int len,
           RedrawProc redraw, enum ModifyTextCursorPosition cursormove)
{
    XmTextVerifyCallbackStruct mv_cbs;
    XmTextBlockRec             tb;
    Boolean                    text_was_modified = True;
    XmAnyCallbackStruct        vc_cbs;
    XmTextPosition             cursorpos = TextF_CursorPos(w);
    XmTextPosition             saved_cursorpos = TextF_CursorPos(w);

    mv_cbs.reason = XmCR_MODIFYING_TEXT_VALUE;
    mv_cbs.doit = True;
    mv_cbs.event = ev;
    /* Make sure start < end for callback */
    if (start < end)
    {
        mv_cbs.startPos = start;
        mv_cbs.endPos = end;
    }
    else
    {
        mv_cbs.startPos = end;
        mv_cbs.endPos = start;

    }
    /* SG 23/08/1998 if replacing an area set the new insertion cursor
       to the end of the new text.
     */
    mv_cbs.currInsert = cursorpos;
    if(mv_cbs.startPos != mv_cbs.endPos)
    {
        mv_cbs.newInsert = mv_cbs.startPos + len;
    }
    else
    {
        mv_cbs.newInsert = cursorpos;
    }

    if (str)
    {
        tb.ptr = XtMalloc(len + 1);	/* NULL-terminate */
        tb.length = len;
        tb.format = XmFMT_8_BIT;
        strncpy(tb.ptr, str, len);
        tb.ptr[len] = '\0';             /* NULL-terminate */
    }
    else
    {
	/* Motif sets ptr to NULL for delete, not to NULL string */
        tb.ptr = NULL;
        tb.length = 0;
        tb.format = XmFMT_8_BIT;
    }
    mv_cbs.text = &tb;
    if (TextF_ModifyVerifyCallback(w))
    {
        XtCallCallbacks((Widget)w, XmNmodifyVerifyCallback, &mv_cbs);
    }
    /* FIXME ? */
    /* SG 23/08/1998 this is wrong as the callback structure is NOT
       the same for wide characters, given that the rest of wide character
       support is missing - I think it preferable to skip this for now.
     */
#if 0
    if (TextF_WcsModifyVerifyCallback(w))
    {
	XtCallCallbacks((Widget)w, XmNmodifyVerifyCallbackWcs, &mv_cbs);
    }
#endif
    if (mv_cbs.doit)
    {
        if (mv_cbs.startPos < mv_cbs.endPos)
        {
            TextDelete(w, mv_cbs.startPos, mv_cbs.endPos - mv_cbs.startPos);
            cursorpos = mv_cbs.startPos;
            text_was_modified = True ;
        }
        if (mv_cbs.endPos < mv_cbs.startPos)
        {
            TextDelete(w, mv_cbs.endPos, mv_cbs.startPos - mv_cbs.endPos);
            cursorpos = mv_cbs.endPos;
            text_was_modified = True ;
        }
        
        TextF_CursorPos(w) = cursorpos;

        if ((len = mv_cbs.text->length) > 0)
        {
            char *buf = mv_cbs.text->ptr;

            if (TextF_Length(w) + len > TextF_MaxLength(w))
            {
                VerifyBell(w);
            }
            else
            {
                /* If we didn't want a MassiveChangeDraw and no text
                   is highlighted and we haven't deleted any text then
                   we can use the fast redraw proc. */
                if (redraw == Draw && TextF_HighlightStart(w) < 0 &&
                    !text_was_modified)
                {
                    redraw = DrawInsert;
                }
                CursorErase(w);
                TextInsert(w, buf, len);
                text_was_modified = True ;
                if (cursormove == CP_EndOfInsert)
                  cursorpos += len;
            }
        }
                
        if (text_was_modified)
        {
            redraw(w);
            if (TextF_ValueChangedCallback(w))
            {
                vc_cbs.reason = XmCR_VALUE_CHANGED;
                vc_cbs.event = ev;
                XtCallCallbacks((Widget)w, XmNvalueChangedCallback, &vc_cbs);
            }
        }
        TextF_CursorPos(w) = saved_cursorpos;
        if (cursormove == CP_Reset)
          cursorpos = 0;
        if (cursormove == CP_Unchanged)
          cursorpos = saved_cursorpos;
        DoCursorMove(w, ev, cursorpos, True, True);
    }
    else
    {
        VerifyBell(w);

    }
    if (tb.ptr)
    {
	XtFree(tb.ptr);
    }
}


static XmTextPosition
TextPixelToSelectionPos(XmTextFieldWidget w, int x)
{
    XmTextPosition pos;

    pos = 0;

    x -= (int)TextF_XDraw(w) + TextF_XOffset(w);

    /* check if the cursor is before the 1st character */
    if (x <= 0)
    {
	pos = 0;
    }

    /* OK, how 'bout after the last character */
    else if (x > TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w)))
    {
	pos = TextF_Length(w);
    }

    /* must be in between somewhere... */
    else
    {
	int prevTot, tot, i, diff, mb;
	
	prevTot = tot = 0;
	pos = -1;
	diff = x;
	mb = 0;
	for (i = 0; i < TextF_Length(w); i++)
	{
	    tot = TextF_FontTextWidth(w, TextF_Value(w), i);
	    if (x < tot)
	    {
		pos = i;
		if (tot - x > diff)
		    pos = pos - 1 - mb;

		break;
	    }
	    if (i && prevTot == tot)
		mb++;
	    else
		mb = 0;
	    prevTot = tot;
	    diff = x - tot;
	}

	if (pos < 0)
	{
	    pos = TextF_Length(w);
	}

    }
    return pos;
}


static XmTextPosition
TextPixelToPos(XmTextFieldWidget w, int x)
{
    XmTextPosition i, tot, cur, pos;

    pos = 0;

    x -= (int)TextF_XDraw(w) + TextF_XOffset(w);

    /* check if the cursor is before the 1st character */
    if (x <= 0)
    {
	pos = 0;
    }

    /* OK, how 'bout after the last character */
    else if (x > TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w)))
    {
	pos = TextF_Length(w);
    }
    /* must be in between somewhere... */
    else
    {
	tot = 0;
	pos = -1;
	for (i = 0; i < TextF_Length(w); i++)
	{
	    cur = TextF_FontTextWidth(w, &TextF_Value(w)[i], 1);
	    if (x < tot + cur)
	    {
		pos = i;
		break;
	    }
	    tot += cur;
	}
	if (pos < 0)
	{
	    pos = TextF_Length(w);
	}
    }
    return pos;
}


/*
 * Private drawing functions --------------------------------------------------
 */

static Boolean
MakePositionVisible(XmTextFieldWidget w, XmTextPosition pos)
{
	int x, start, end;
	Boolean moved;

	moved = False;
	x = TextF_FontTextWidth(w, TextF_Value(w), pos);
	start = -TextF_XOffset(w);
	end = start + TextF_ViewWidth(w);

	if (x < start) {
		TextF_XOffset(w) = -x;
		moved = True;
	} else if (x > end) {
		TextF_XOffset(w) = TextF_ViewWidth(w) - x;
		moved = True;
	}

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"MakePositionVisible(pos %d) start=%d end=%d x=%d moved=%s\n",
		pos, start, end, x, _LtDebugBoolean2String(moved)));

	return moved;
}

#ifdef	USE_XFT
static void
DrawTextXft(XmTextFieldWidget w, int start, int end, XmHighlightMode highlight)
{
	int		x, len;
	Dimension	wid, bw, bh;
	Position	e;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DrawTextXft(%d %d %d)\n",
		start, end, highlight));

	len=end-start;

	if (TextF_Length(w) > 0) {
		XmHighlightMode mode = highlight;	/* Danny FIX ME */

		if (start < 0) {
			return;
		} else if (end < start) {
			int temp;

			temp = start;
			start = end;
			end = temp;
		}

		if (start > TextF_Length(w)) {
			return;
		} else if (end > TextF_Length(w)) {
			end = TextF_Length(w);
		}

		x = TextF_XDraw(w) + TextF_XOffset(w) +
			TextF_FontTextWidth(w, TextF_Value(w), start);
		wid = TextF_FontTextWidth(w, TextF_Value(w) + start, len);

		/* Simplistic clip */
		bw = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
			+ TextF_MarginWidth(w);
		bh = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
			+ TextF_MarginHeight(w);

		if (x + wid + bw > XtWidth(w)) {
			wid = XtWidth(w) - bw - x;
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				"DrawText: clip %d\n", wid));
		}
		e = wid + TextF_XOffset(w);

		/* Simplified version */
		/* Clear prior to redraw */
		if (mode == XmHIGHLIGHT_SELECTED) {
			XSetForeground(XtDisplay(w), TextF_DrawGC(w), Prim_Foreground(w));
			XFillRectangle(XtDisplay((Widget)w), XtWindow(w),
				TextF_DrawGC(w),
				bw,
				bh,
				XtWidth(w) - 2 * bw,
				XtHeight(w) - 2 * bh);

			XSetForeground(XtDisplay(w), TextF_DrawGC(w), XtBackground(w));
			XSetBackground(XtDisplay(w), TextF_DrawGC(w), Prim_Foreground(w));

			TextF_DrawGCInverted(w) = True;
		} else {
			XClearArea(XtDisplay((Widget)w), XtWindow(w),
				bw, bh,
				XtWidth(w) - 2 * bw,
				XtHeight(w) - 2 * bh,
				False);

			XSetForeground(XtDisplay(w), TextF_DrawGC(w), Prim_Foreground(w));
			XSetBackground(XtDisplay(w), TextF_DrawGC(w), XtBackground(w));

			TextF_DrawGCInverted(w) = False;
		}

		if (len > 0) {
			/* i18n */
			if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_FONTSET) {
				XmbDrawString(XtDisplay(w), XtWindow(w),
					(XFontSet)(TextF_FontList(w)->renditions[0]->font),
					TextF_DrawGC(w),
					x, TextF_YOffset(w),
					&TextF_Value(w)[start], len);
			} else if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_XFT) {
				_XmXftDrawString(XtDisplay(w), XtWindow(w),
					TextF_FontList(w)->renditions[0],
					w->text.max_char_size,
					x, TextF_YOffset(w),
					&TextF_Value(w)[start], len);
			} else {
				XDrawString(XtDisplay(w), XtWindow(w), TextF_DrawGC(w),
					x, TextF_YOffset(w),
					&TextF_Value(w)[start], len);
			}
			if (mode == XmHIGHLIGHT_SECONDARY_SELECTED) {
				/* underline the text */
				int wid;

				wid=TextF_FontTextWidth(w, &TextF_Value(w)[start], len);
				XDrawLine(XtDisplay(w), XtWindow(w), TextF_DrawGC(w),
					x, TextF_YOffset(w) + 1,
					x + wid, TextF_YOffset(w) + 1);
			}
		}
	}
}
#endif

/*
 * Actually draw a range of text onto the widget
 */
static void
DrawText(XmTextFieldWidget w, int start, int end, XmHighlightMode highlight)
{
	int		x, len;
	Dimension	wid, bw, bh;
	Position	e;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DrawText(%d %d %d)\n",
		start, end, highlight));

	if (!TextF_Echo(w)) {
		return;
	}

	len=end-start;
	if (len <= 0)
		return;	/* FIX ME should we do this ? */

#ifdef	USE_XFT
	if (TextF_UseXft(w)) {
		DrawTextXft(w, start, end, highlight);
		return;
	}
#endif
	if (TextF_Length(w) > 0) {
		XmHighlightMode mode = highlight;	/* Danny FIX ME */

		if (start < 0) {
			return;
		} else if (end < start) {
			int temp;

			temp = start;
			start = end;
			end = temp;
		}

		if (start > TextF_Length(w)) {
			return;
		} else if (end > TextF_Length(w)) {
			end = TextF_Length(w);
		}
	
		x = TextF_XDraw(w) + TextF_XOffset(w) +
			TextF_FontTextWidth(w, TextF_Value(w), start);
		wid = TextF_FontTextWidth(w, TextF_Value(w) + start, len);

		/* Simplistic clip */
		bw = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
			+ TextF_MarginWidth(w);
		bh = Prim_ShadowThickness(w) + Prim_HighlightThickness(w)
			+ TextF_MarginHeight(w);

		if (x + wid + bw > XtWidth(w)) {
			wid = XtWidth(w) - bw - x;
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DrawText: clip %d\n", wid));
		}
		e = wid + TextF_XOffset(w);

		/* Simplified version */
		if (mode == XmHIGHLIGHT_SELECTED) {
			/* Clear prior to redraw */
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DrawText-a\n"));
			XSetForeground(XtDisplay(w), TextF_DrawGC(w), Prim_Foreground(w));
			XFillRectangle(XtDisplay((Widget)w), XtWindow(w),
				TextF_DrawGC(w),
				x,
				bh,
				wid /* XtWidth(w) - 2 * bw */,
				XtHeight(w) - 2 * bh);
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "XFillRectangle[%d,%d,%d,%d]\n",
				x,
				bh,
				wid,
				XtHeight(w) - 2 * bh
				));

			XSetForeground(XtDisplay(w), TextF_DrawGC(w), XtBackground(w));
			XSetBackground(XtDisplay(w), TextF_DrawGC(w), Prim_Foreground(w));

			TextF_DrawGCInverted(w) = True;
		} else {
			int	xx = x,
				ww = wid;

			/* Protect against overwriting the left/right borders. */
			if (x < bw) {
				xx = bw;
				ww -= bw - x;
			}

			if (XtWidth(w) < xx + ww + bw)
				ww = XtWidth(w) - 2 * bw - xx;

			/* Clear prior to redraw */
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DrawText-b\n"));
			DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "XClearArea[%d,%d,%d,%d]\n",
				xx, bh,
				ww,
				XtHeight(w) - 2 * bh));

			if (ww > 0)
				XClearArea(XtDisplay((Widget)w), XtWindow(w),
					xx, bh,
					ww /* XtWidth(w) - 2 * bw */,
					XtHeight(w) - 2 * bh,
					False);

			XSetForeground(XtDisplay(w), TextF_DrawGC(w), Prim_Foreground(w));
			XSetBackground(XtDisplay(w), TextF_DrawGC(w), XtBackground(w));

			TextF_DrawGCInverted(w) = False;
		}

		if (len > 0) {
			/* i18n */
			if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_FONTSET) {
#if 0
				DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
					"DrawText: XmbDrawString\n"));
#endif
				XmbDrawString(XtDisplay(w), XtWindow(w),
					(XFontSet)(TextF_FontList(w)->renditions[0]->font),
					TextF_DrawGC(w),
					x, TextF_YOffset(w),
					&TextF_Value(w)[start], len);
			} else {
#if 0
				DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
					"DrawText: XDrawString\n"));
#endif
				XDrawString(XtDisplay(w), XtWindow(w), TextF_DrawGC(w),
					x, TextF_YOffset(w),
					&TextF_Value(w)[start], len);
			}
			if (mode == XmHIGHLIGHT_SECONDARY_SELECTED) {
				/* underline the text */
				int wid;

				wid=TextF_FontTextWidth(w, &TextF_Value(w)[start], len);
				XDrawLine(XtDisplay(w), XtWindow(w), TextF_DrawGC(w),
					x, TextF_YOffset(w) + 1,
					x + wid, TextF_YOffset(w) + 1);
			}
		}
	}
}

static void
DrawTextRange(XmTextFieldWidget w, int start, int end)
{
	int		i;
	XmHighlightMode	mode;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "DrawTextRange[%d %d]\n", start, end));
	if (!TextF_Echo(w)) {
		return;
	}

    if (TextF_Length(w) > 0) {
	if (start < 0) {
		return;
	} else if (end < start) {
		int temp;

		temp = start;
		start = end;
		end = temp;
	}

	mode = TextF_Highlight(w).list[0].mode;
	for (i=0; i<TextF_Highlight(w).number-1; i++) {
		if (TextF_Highlight(w).list[i].position == start) {
			if (TextF_Highlight(w).list[i+1].position < end) {
				DrawText(w, start, TextF_Highlight(w).list[i+1].position, mode);
			} else {
				DrawText(w, start, end, mode);
			}
			mode = TextF_Highlight(w).list[i+1].mode;
		} else if (TextF_Highlight(w).list[i].position < start &&
				start < TextF_Highlight(w).list[i+1].position) {
			if (TextF_Highlight(w).list[i].position != start)
				DrawText(w, TextF_Highlight(w).list[i].position, start, mode);
			if (TextF_Highlight(w).list[i+1].position < end) {
				DrawText(w, start, TextF_Highlight(w).list[i+1].position, mode);
			} else {
				DrawText(w, start, end, mode);
			}
			mode = TextF_Highlight(w).list[i+1].mode;
		} else if (start < TextF_Highlight(w).list[i].position &&
				TextF_Highlight(w).list[i+1].position < end) {
			DrawText(w, TextF_Highlight(w).list[i].position,
				TextF_Highlight(w).list[i+1].position,
				TextF_Highlight(w).list[i].mode);
			mode = TextF_Highlight(w).list[i+1].mode;
		} else if (TextF_Highlight(w).list[i].position == end) {
			/* ?? */
		} else if (TextF_Highlight(w).list[i].position < end &&
				end < TextF_Highlight(w).list[i+1].position) {
			DrawText(w, TextF_Highlight(w).list[i].position,
				end,
				TextF_Highlight(w).list[i].mode);
		} else {
			/* ?? */
		}
	}
    }
}


static void
DrawTextReposition(XmTextFieldWidget w)
{
    int xsrc, xdest, width, start, end;

    if (!TextF_Echo(w))
    {
	return;
    }
    if (!XtIsRealized((Widget)w))
    {
	return;
    }

    if (TextF_XOffset(w) < TextF_OldXOffset(w))
    {
	xsrc = TextF_OldXOffset(w) - TextF_XOffset(w);
	xdest = 0;
	width = TextF_ViewWidth(w) - xsrc + 1;

	/* Need to redraw some characters at the end. */
	end = TextF_CursorPos(w);
	start = TextF_OldCursorPos(w);
    }
    else if (TextF_XOffset(w) > TextF_OldXOffset(w))
    {
	xsrc = 0;
	xdest = TextF_XOffset(w) - TextF_OldXOffset(w);
	width = TextF_ViewWidth(w) - xdest + 1;

	/* Need to redraw some characters at the beginning. */

	start = TextF_CursorPos(w);
	end = TextF_OldCursorPos(w);
    }
    else
    {
	return;
    }

    if (width > 0)
    {
	if (_LtDebugInDebug(__FILE__, (Widget)w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			      "Reposition: xoff=%d old=%d src=%d dest=%d"
			      " width=%d refresh %d-%d\n",
			    TextF_XOffset(w), TextF_OldXOffset(w), xsrc, xdest,
			      width, start, end));
	}

	XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w),
		  TextF_DrawGC(w),
		  TextF_XDraw(w) + xsrc, TextF_YDraw(w),
		  (unsigned int)width, (unsigned int)TextF_ViewHeight(w),
		  TextF_XDraw(w) + xdest, TextF_YDraw(w));

	/*
	 * Erase to the end of the window (if necessary).  This removes any
	 * garbage that might be left over from the XCopyArea
	 */
	if (xdest < xsrc)
	{
	    XClearArea(XtDisplay(w), XtWindow(w),
		       TextF_XDraw(w) + xdest + width, TextF_YDraw(w),
		       xsrc - xdest, (unsigned int)TextF_ViewHeight(w),
		       False);

	    if (_LtDebugInDebug(__FILE__, (Widget)w))
	    {
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				  "Reposition: erasing x=%d y=%d w=%d h=%d\n",
				TextF_XDraw(w) + xdest + width, TextF_YDraw(w),
			     xsrc - xdest, (unsigned int)TextF_ViewHeight(w)));
	    }

	}

	if (start == end)
	{
	    end++;
	}

	DrawTextRange(w, start, end);
    }
    else
    {
	/* redraw the entire visible string */
	start = TextPixelToPos(w, TextF_XDraw(w));
	end = TextPixelToPos(w, TextF_XDraw(w) + TextF_ViewWidth(w)) + 1;

	DrawTextRange(w, start, end);

	if (_LtDebugInDebug(__FILE__, (Widget)w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			      "Reposition: xoff=%d old=%d refresh %d-%d\n",
			   TextF_XOffset(w), TextF_OldXOffset(w), start, end));
	}

    }

    TextF_OldXOffset(w) = TextF_XOffset(w);
}


static void
EraseXRange(XmTextFieldWidget w, int start, int end)
{
    int x1, x2;

    x1 = TextF_XOffset(w) + start;
    x2 = TextF_XOffset(w) + end;
    if (x1 < 0)
    {
	x1 = 0;
    }
    if (x2 > TextF_ViewWidth(w))
    {
	x2 = TextF_ViewWidth(w);
    }
    if (x2 > x1)
    {
	XClearArea(XtDisplay(w), XtWindow(w),
		   TextF_XDraw(w) + x1, TextF_YDraw(w),
		   x2 - x1 + 1,
		   TextF_ViewHeight(w), False);
    }
}


static void
DrawTextWithCopyArea(XmTextFieldWidget w)
{
    int x, insert_width;
    int xsrc, xdest, width;

    if (!TextF_Echo(w))
    {
	return;
    }
    if (!XtIsRealized((Widget)w))
    {
	return;
    }

    x = TextF_XOffset(w);
    insert_width = TextF_FontTextWidth(w,
			       &TextF_Value(w)[TextF_FastInsertCursorStart(w)],
				       TextF_FastInsertTextLen(w));

    if (CursorPosition(w))
    {
	/*
	 *  if the text is scrolled, then:
	 * 1.  the cursor is at the end
	 * 2.  the copy will move to the left.
	 */
	xsrc = 0;
	width = TextF_OldCursorX(w) + x;
	xdest = TextF_ViewWidth(w) - (x + TextF_OldCursorX(w)) - insert_width;

	XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w),
		  TextF_DrawGC(w),
		  TextF_XDraw(w) + xsrc, TextF_YDraw(w),
		  (unsigned int)width, (unsigned int)TextF_ViewHeight(w),
		  TextF_XDraw(w) + xdest, TextF_YDraw(w));

	if (_LtDebugInDebug(__FILE__, (Widget)w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			      "DrawTextWCA: x=%d xsrc=%d xdest=%d width=%d\n",
			      x, xsrc, xdest, width));
	}

    }
    else
    {
	/*
	 * the text hasn't been scrolled, so:
	 * 1.  the text left of the cursor won't change
	 * 2.  the stuff after the cursor will be moved right.
	 */
	xsrc = TextF_FontTextWidth(w,
				   TextF_Value(w),
				   TextF_FastInsertCursorStart(w)) + x;

	width = TextF_ViewWidth(w) - xsrc;
	xdest = xsrc + insert_width;

	XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w),
		  TextF_DrawGC(w),
		  TextF_XDraw(w) + xsrc, TextF_YDraw(w),
		  (unsigned int)width, (unsigned int)TextF_ViewHeight(w),
		  TextF_XDraw(w) + xdest, TextF_YDraw(w));

	if (_LtDebugInDebug(__FILE__, (Widget)w))
	{
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			      "DrawTextWCA: x=%d xsrc=%d xdest=%d width=%d\n",
			      x, xsrc, xdest, width));
	}
    }

    DrawTextRange(w, TextF_FastInsertCursorStart(w),
		  TextF_FastInsertCursorStart(w) + TextF_FastInsertTextLen(w));

    if (TextF_TextWidth(w) < TextF_OldTextWidth(w))
    {
	EraseXRange(w, TextF_TextWidth(w), TextF_OldTextWidth(w));
    }

    TextF_OldTextWidth(w) = TextF_TextWidth(w);
    TextF_OldXOffset(w) = TextF_XOffset(w);
}


static void
DrawAllText(XmTextFieldWidget w)
{
    if (!TextF_Echo(w))
    {
	return;
    }

    DrawTextRange(w, 0, TextF_Length(w));

    if (TextF_TextWidth(w) < TextF_OldTextWidth(w))
    {
	EraseXRange(w, TextF_TextWidth(w), TextF_OldTextWidth(w));
    }

    TextF_OldTextWidth(w) = TextF_TextWidth(w);
    TextF_OldXOffset(w) = TextF_XOffset(w);
    TextF_OldHighlightStart(w) = TextF_HighlightStart(w);
    TextF_OldHighlightEnd(w) = TextF_HighlightEnd(w);
}


/* Cursor functions -------------------------------------------------------- */

static Boolean
CursorPosition(XmTextFieldWidget w)
{
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "CursorPosition(%d)\n", TextF_CursorPos(w)));
	if (TextF_CursorPos(w) < 0) {
		TextF_CursorPos(w) = 0;
	} else if (TextF_CursorPos(w) > TextF_Length(w)) {
		TextF_CursorPos(w) = TextF_Length(w);
	}

	return MakePositionVisible(w, TextF_CursorPos(w));
}


static Boolean
CursorMassiveAdjust(XmTextFieldWidget w)
{
    int start, end, last;
    Boolean moved;

    moved = False;
    end = TextF_FontTextWidth(w, TextF_Value(w), TextF_CursorPos(w));

    if (TextF_HighlightStart(w) >= 0)
    {
	start = TextF_FontTextWidth(w,
			TextF_Value(w),
			TextF_Highlight(w).list[1].position); /* TextF_HighlightStart(w) */
    }
    else
    {
	start = end;
    }

    if (end < TextF_ViewWidth(w))
    {
	if (TextF_XOffset(w) < 0)
	{
	    TextF_XOffset(w) = 0;
	    moved = True;
	}
    }
    else if (start >= TextF_XOffset(w) &&
	     end < TextF_XOffset(w) + TextF_ViewWidth(w))
    {
	return moved;
    }
    else
    {
	last = TextF_FontTextWidth(w, TextF_Value(w), TextF_Length(w));
	if (start - end > TextF_ViewWidth(w))
	{
	    if (last - end > TextF_ViewWidth(w))
	    {
		TextF_XOffset(w) = TextF_ViewWidth(w) - last;
	    }
	    else
	    {
		TextF_XOffset(w) = TextF_ViewWidth(w) - end;
	    }
	}
	else if (end > TextF_ViewWidth(w))
	{
	    TextF_XOffset(w) = TextF_ViewWidth(w) - end;
	}
	else
	{
	    TextF_XOffset(w) = 0;
	}
	moved = True;
    }

    return moved;
}


static void
CursorSet(XmTextFieldWidget w, Boolean focus)
{
	TextF_HasFocus(w) = focus;

	/* These change the GC !! (Danny) */
	if (focus) {
		XSetStipple(XtDisplay((Widget)w), TextF_CursorGC(w), TextF_CursorIBeam(w));
	} else {
		XSetStipple(XtDisplay((Widget)w), TextF_CursorGC(w), TextF_CursorStipple(w));
	}
}

/*
 * For bug #1074547, we're moving the I-beam one pixel to the left.
 * This macro defines the amount.
 */
#define	HACK_BUG1074547	-1

static void
CursorSaveUnderIBeam(XmTextFieldWidget w, int x)
{
	if (!XtIsRealized((Widget)w)) {
		return;
	}

	/* Make sure that the text is drawn before saving the cursor */
	XFlush(XtDisplay((Widget)w));

	/* save the area under the cursor */
	XFillRectangle(XtDisplay((Widget)w), TextF_CursorSave(w), TextF_CopyGC(w),
			0, 0, 5, TextF_ViewHeight(w));

	XCopyArea(XtDisplay((Widget)w), XtWindow((Widget)w), TextF_CursorSave(w),
			TextF_CursorGC(w),
			x - 2, TextF_YDraw(w),
			5, TextF_ViewHeight(w), 0, 0);

	TextF_CursorSaveValid(w) = True;
}


static void
CursorRestoreUnderIBeam(XmTextFieldWidget w)
{
	int x;

	if (!XtIsRealized((Widget)w)) {
		return;
	}

	/* Make sure that the text is drawn before erasing the cursor */
	XFlush(XtDisplay((Widget)w));

	x = TextF_OldCursorX(w) + TextF_XDraw(w) + TextF_XOffset(w) + HACK_BUG1074547;

	XCopyArea(XtDisplay((Widget)w), TextF_CursorSave(w), XtWindow((Widget)w),
			TextF_CursorGC(w), 0, 0, 5, TextF_ViewHeight(w),
			x - 2, TextF_YDraw(w));

	TextF_CursorSaveValid(w) = False;
}


static void
CursorDrawIBeam(XmTextFieldWidget w, int x)
{
	/* save the area under the cursor */
	CursorSaveUnderIBeam(w, x);

	/* These change the GC !! (Danny) */
	XSetTSOrigin(XtDisplay((Widget)w), TextF_CursorGC(w),
			x - 2, TextF_YOffset(w) - TextF_FontAscent(w));

	XFillRectangle(XtDisplay((Widget)w), XtWindow((Widget)w), TextF_CursorGC(w),
			x - 2, TextF_YDraw(w), 5, TextF_ViewHeight(w));
}


static void
CursorDraw(XmTextFieldWidget w)
{
    int x;

    if (TextF_CursorPositionVisible(w))
    {
	x = TextF_FontTextWidth(w, TextF_Value(w), TextF_CursorPos(w));

	CursorErase(w);
	TextF_OldCursorPos(w) = TextF_CursorPos(w);
	TextF_OldCursorX(w) = x;

	x += TextF_XDraw(w) + TextF_XOffset(w) + HACK_BUG1074547;

	CursorDrawIBeam(w, x);

	TextF_BlinkOn(w) = True;
    }
}


static void
CursorErase(XmTextFieldWidget w)
{
    if (TextF_CursorSaveValid(w))
    {
	CursorRestoreUnderIBeam(w);
    }

    TextF_BlinkOn(w) = False;
}


static void
ClearHighlight(XmTextFieldWidget w)
{
	if (!TextF_Echo(w))
		return;

	if (TextF_HighlightStart(w) >= 0) {
		CursorErase(w);
		TextF_InitialiseHighlight((Widget)w);

		/*
		 * FIX ME this can probably be done more efficiently
		 * Old version :
		 * DrawText(w, TextF_HighlightStart(w), TextF_HighlightEnd(w), False);
		 */
		DrawAllText(w);

		CursorDraw(w);
		TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
	}

	TextF_OldHighlightStart(w) = TextF_OldHighlightEnd(w) = -1;
}

static void
SetSingleHighlight(Widget w, int left, int right)
{
	if (left < 0)
		left = 0;
	if (right > TextF_Length(w))
		right = TextF_Length(w);

	TextF_HighlightStart(w) = left;
	TextF_HighlightEnd(w) = right;

	if (left < right) {
		TextF_Highlight(w).number = 4;
		TextF_Highlight(w).list[0].position = 0;
		TextF_Highlight(w).list[0].mode = XmHIGHLIGHT_NORMAL;
		TextF_Highlight(w).list[1].position = left;
		TextF_Highlight(w).list[1].mode = XmHIGHLIGHT_SELECTED;
		TextF_Highlight(w).list[2].position = right;
		TextF_Highlight(w).list[2].mode = XmHIGHLIGHT_NORMAL;
		TextF_Highlight(w).list[3].position = INT_MAX;
		TextF_Highlight(w).list[4].mode = XmHIGHLIGHT_NORMAL;
	} else {
		TextF_InitialiseHighlight(w);
	}
}

static void
DrawSingleHighlight(XmTextFieldWidget w)
{
	int	l, r;

	if (!TextF_Echo(w)) {
		return;
	}

	if (!XtIsRealized(w)) {
		return;
	}
	if (TextF_Highlight(w).number == 4) {
		l = TextF_Highlight(w).list[1].position;
		r = TextF_Highlight(w).list[2].position;

		DrawText(w, l, r, True);
	}
}

/*
 * Special redraw function after a text insertion
 */
static void
DrawInsert(XmTextFieldWidget w)
{
    if (!XtIsRealized(w))
    {
	return;
    }

    /*  CursorErase must be called before this */
    DrawTextWithCopyArea(w);
    CursorDraw(w);
}


/*
 * Special redraw function after a cursor reposition (i.e.: no text changes)
 */
static void
DrawMove(XmTextFieldWidget w)
{
    if (!XtIsRealized(w))
    {
	return;
    }

    CursorErase(w);
    if (CursorPosition(w))
    {
	DrawTextReposition(w);
    }

    CursorDraw(w);
}


/*
 * Redraw the entire widget, but don't scroll the window much
 */
static void
Draw(XmTextFieldWidget w)
{
    if (!XtIsRealized(w))
    {
	return;
    }

    CursorErase(w);

    CursorPosition(w);

    DrawAllText(w);

    CursorDraw(w);
}


/*
 * Clear window & redraw the entire widget
 */
static void
DrawAll(XmTextFieldWidget w)
{
    if (!XtIsRealized(w))
    {
	return;
    }

    XClearArea(XtDisplay(w), XtWindow(w),
	       Prim_HighlightThickness(w),
	       Prim_HighlightThickness(w),
	       XtWidth(w) - 2 * Prim_HighlightThickness(w),
	       XtHeight(w) - 2 * Prim_HighlightThickness(w),
	       False);

    CursorPosition(w);

    DrawAllText(w);

    CursorDraw(w);
}


/*
 * Like Draw(), but has different rules about scrolling the window to
 * put the cursor in a good place
 */
static void
MassiveChangeDraw(XmTextFieldWidget w)
{
    if (!XtIsRealized(w))
    {
	return;
    }

    CursorErase(w);

    CursorMassiveAdjust(w);

    DrawAllText(w);

    CursorDraw(w);
}



/*
 * text search procedures -----------------------------------------------------
 */

static Boolean
InWord(char s)
{
    switch (s)
    {
    case ' ':
    case '\t':
    case '\n':
    case '!':
    case '?':
    /* rws 17 Aug 1997
       I do not think this should be included. Double clicking on a word
       that has a '.' in it will include the '.' in Motif.
     */
    /* case '.':*/
    case ',':
	return False;

    default:
	return True;
    }
}


static XmTextPosition
WordEnd(XmTextFieldWidget w, XmTextPosition pos)
{
    while ((pos < TextF_Length(w)) && InWord(TextF_Value(w)[pos]))
    {
	pos++;
    }

    return pos;
}


static XmTextPosition
WordStart(XmTextFieldWidget w, XmTextPosition pos)
{
    while ((pos > 0) && InWord(TextF_Value(w)[pos - 1]))
    {
	pos--;
    }

    return pos;
}


static XmTextPosition
SkipForward(XmTextFieldWidget w, XmTextPosition pos)
{
    while ((pos < TextF_Length(w)) && !InWord(TextF_Value(w)[pos]))
    {
	pos++;
    }

    return pos;
}


static XmTextPosition
SkipBackward(XmTextFieldWidget w, XmTextPosition pos)
{
    while ((pos > 0) && !InWord(TextF_Value(w)[pos - 1]))
    {
	pos--;
    }

    return pos;
}


static XmTextScanType
ScanType(XmTextFieldWidget w)
{
    XmTextScanType type;

    if (TextF_SelectionArray(w))
    {
	if (w->text.sarray_index >= TextF_SelectionArrayCount(w))
	{
	    w->text.sarray_index = 0;
	}

	type = TextF_SelectionArray(w)[w->text.sarray_index];
    }
    else
    {
	switch (w->text.sarray_index)
	{
	case 1:
	    type = XmSELECT_WORD;
	    break;

	case 2:
	    type = XmSELECT_ALL;
	    break;

	default:
	    type = XmSELECT_POSITION;
	    w->text.sarray_index = 0;
	    break;
	}
    }

    return type;
}


static XmTextPosition
ScanTypeStart(XmTextFieldWidget w, XmTextPosition pos)
{
    switch (ScanType(w))
    {
    case XmSELECT_WORD:
	pos = WordStart(w, pos);
	break;

    case XmSELECT_WHITESPACE:
	pos = SkipBackward(w, pos);
	break;

    case XmSELECT_LINE:
    case XmSELECT_PARAGRAPH:
    case XmSELECT_ALL:
	pos = 0;
	break;

    case XmSELECT_POSITION:
    default:
	break;
    }

    return pos;
}


static XmTextPosition
ScanTypeEnd(XmTextFieldWidget w, XmTextPosition pos)
{
    switch (ScanType(w))
    {
    case XmSELECT_WORD:
	pos = WordEnd(w, pos);
	break;

    case XmSELECT_WHITESPACE:
	pos = SkipForward(w, pos);
	break;

    case XmSELECT_LINE:
    case XmSELECT_PARAGRAPH:
    case XmSELECT_ALL:
	pos = TextF_Length(w);
	break;

    case XmSELECT_POSITION:
    default:
	break;
    }
    return pos;
}


/*
 * High level text insert and delete routines ---------------------------------
 */

static void
VerifyBell(XmTextFieldWidget w)
{
    if (TextF_VerifyBell(w))
    {
	XBell(XtDisplay((Widget)w), 50);
    }
}


static Boolean
DoCursorMove(XmTextFieldWidget w, XEvent *ev,
	     XmTextPosition pos, Boolean highlight, Boolean drawit)
{
    XmTextVerifyCallbackStruct cbs;

    if (pos > TextF_Length(w))
    {
	pos = TextF_Length(w);
    }

    cbs.doit = True;
    if (TextF_MotionVerifyCallback(w))
    {
	cbs.reason = XmCR_MOVING_INSERT_CURSOR;
	cbs.event = ev;
	cbs.currInsert = TextF_CursorPos(w);
	cbs.newInsert = pos;
	cbs.startPos = cbs.endPos = 0;
	cbs.text = NULL;

	XtCallCallbacks((Widget)w, XmNmotionVerifyCallback, &cbs);

	if (cbs.doit)
	{
	    pos = cbs.newInsert;
	}
    }

    if (cbs.doit)
    {
	/*
	 * Add mode means don't mess with selection when moving the cursor.
	 */
	if (! TextF_AddMode(w)) {
		if (highlight) {
		    ClearHighlight(w);
		}
	}

	TextF_CursorPos(w) = pos;
	if (drawit)
	{
	    DrawMove(w);
	}
    }
    else
    {
	VerifyBell(w);
    }

    (void)_XmImSendSpot((Widget)w);
    return cbs.doit;
}


static void
DoInsert(XmTextFieldWidget w, XEvent *ev, char *buf, int len)
{
    if (len <= 0)
    {
	return;
    }

    if (!TextF_Editable(w))
    {
	VerifyBell(w);
	return;
    }

    if (_LtDebugInDebug(__FILE__, (Widget)w))
    {
    char *tmp;

    	/* buf is not necessarily NULL terminated!!!! */
    	tmp = XtMalloc(len+1);
    	strncpy(tmp,buf,len);
    	tmp[len] = '\0';
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "DoInsert(buf '%s' len %d)\n", tmp, len));
    	XtFree(tmp);
    }

    if (len <= 0)
    {
	return;
    }

    if (w->text.has_primary && TextF_PendingDelete(w) &&
        TextF_CursorPos(w) >= TextF_HighlightStart(w) &&
        TextF_CursorPos(w) <= TextF_HighlightEnd(w))
    {
	int start, end;
	
	start = TextF_HighlightStart(w);
	end = TextF_HighlightEnd(w);
	XmTextFieldSetSelection((Widget)w,-1,-1,ev?ev->xkey.time:CurrentTime);
        ModifyText(w, ev,
                   start, end,
                   buf, len,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
    else if (TextF_CursorPos(w) <= TextF_Length(w))
    {
	XmTextFieldSetSelection((Widget)w,-1,-1,ev?ev->xkey.time:CurrentTime);
        ModifyText(w, ev,
                   TextF_CursorPos(w), TextF_CursorPos(w),
                   buf, len,
                   Draw, CP_EndOfInsert);
    }
}

static void
DoScanType(XmTextFieldWidget w, XEvent *ev, XmTextPosition pos)
{
    switch (ScanType(w))
    {
    case XmSELECT_POSITION:
	DoCursorMove(w, ev, pos, True, True);

	TextF_HighlightPivot(w) = TextF_CursorPos(w);
	break;

    default:
	TextF_HighlightPivot(w) = TextF_HighlightStart(w) =
	    ScanTypeStart(w, pos);
	TextF_HighlightEnd(w) = ScanTypeEnd(w, pos);

	/* FIX ME - what event type should this be? */
	PrimarySelectionComplete(w, ev->xbutton.time);
	Draw(w);
	break;
    }
}



/*
 * Selection and Clipboard utilities ------------------------------------------
 */

static void
ExtendHighlight(XmTextFieldWidget w)
{
    int x, pos;

    if (!TextF_AllowSelection(w))
    {
	return;
    }

    x = w->text.select_pos_x;

    if (x < (int)TextF_XDraw(w))
    {
	pos = TextPixelToSelectionPos(w, (int)0);
	if (pos > 0)
	{
	    pos--;
	}
    }
    else if (x > (int)(TextF_XDraw(w) + TextF_ViewWidth(w)))
    {
	pos = TextPixelToSelectionPos(w,
				   (int)(TextF_XDraw(w) + TextF_ViewWidth(w)));
	if (pos < TextF_Length(w))
	{
	    pos++;
	}
    }
    else
    {
	pos = TextPixelToSelectionPos(w, x);
    }

    if (pos == TextF_CursorPos(w))
    {
	return;
    }
    DoCursorMove(w, NULL, pos, False, True);

    if (pos < TextF_HighlightPivot(w))
    {
	pos = TextF_HighlightStart(w) = ScanTypeStart(w, pos);

	TextF_HighlightEnd(w) = ScanTypeEnd(w, TextF_HighlightPivot(w));
    }
    else
    {
	TextF_HighlightStart(w) = ScanTypeStart(w, TextF_HighlightPivot(w));

	pos = TextF_HighlightEnd(w) = ScanTypeEnd(w, pos);
    }

    SetSingleHighlight((Widget)w, TextF_HighlightStart(w), TextF_HighlightEnd(w));
    CursorErase(w);

    if (MakePositionVisible(w, pos))
    {
	DrawTextReposition(w);
    }

	DrawSingleHighlight(w);
	CursorDraw(w);
}


static void
ExtendTimer(XtPointer client_data, XtIntervalId *idp)
{
    XmTextFieldWidget w = (XmTextFieldWidget)client_data;
    int highlight_time;

    highlight_time = XtGetMultiClickTime(XtDisplay((Widget)w)) / 2;

    ExtendHighlight(w);

    TextF_SelectId(w) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w),
					(unsigned long)highlight_time,
					ExtendTimer,
					(XtPointer)w);

}


static Boolean
ConvertSelection(Widget aw, Atom *selection, Atom *target, Atom *type,
		 XtPointer *value, unsigned long *length, int *format)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    Atom atom_comp_text;
    Atom atom_text;

    DEBUGOUT(_LtDebug(__FILE__, aw, "ConvertSelection\n"));

    atom_comp_text =  XmInternAtom(XtDisplay(aw), "COMPOUND_TEXT", False);
    atom_text =  XmInternAtom(XtDisplay(aw), "TEXT", False);

    if (*target == XA_STRING){
        *length = (long)TextF_SelectionLen(w);
    /* rws 10 Aug 1998
       We, as the selection owner, must allocate this storage and Xt will
       XtFree it, since we do not provide an XtSelectionDoneProc
     */
        *value = XtNewString(TextF_SelectionText(w));
        *type = XA_STRING;
    }
    else if (*target == atom_comp_text || *target == atom_text){

	    XTextProperty prop;
	    char *buf;
	    int ret;

	    buf = XtMalloc(TextF_SelectionLen(w) + 1);
            strncpy(buf, TextF_SelectionText(w), TextF_SelectionLen(w));
	    buf[TextF_SelectionLen(w)] = '\0';

	    ret = XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
	    			XCompoundTextStyle, &prop);
		
	    XtFree(buf);
	    if (ret != 0){
	    	*length = 0;
	    	*value  = NULL;
	    }
	    else{
	    	buf = XtMalloc(prop.nitems + 1);
		memcpy(buf, prop.value, prop.nitems);
		buf[prop.nitems] = '\0';
	    	*length = prop.nitems;
	    	*value  = buf;
	    }
	   *type = atom_comp_text;

    }
    else
	return False;

    *format = 8;
    return True;
}


/* ARGSUSED */
static void
LoseSelection(Widget aw, Atom *selection)
{
	XmTextFieldWidget w = (XmTextFieldWidget)aw;

	DEBUGOUT(_LtDebug(__FILE__, aw, "LoseSelection\n"));

	if(w->text.has_primary) {
		w->text.has_primary = False;
		if (TextF_Echo(w) && TextF_HighlightStart(w) >= 0) {
			CursorErase(w);
			DrawText(w, TextF_HighlightStart(w), TextF_HighlightEnd(w), False);
			CursorDraw(w);
		}
		TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
		TextF_OldHighlightStart(w) = TextF_OldHighlightEnd(w) = -1;
	}
}

/* Danny HERE */

/* For Selection */
typedef struct {
    XEvent	*ev;
    int		use_ctext;
} SelectionClientData;


/* ARGSUSED */
static void
RequestSelection(Widget aw, XtPointer client, Atom *selection, Atom *type,
		 XtPointer value, unsigned long *length, int *format)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    SelectionClientData *client_data = (SelectionClientData*)client;
    XEvent *ev = (XEvent*)client_data->ev;

    DEBUGOUT(_LtDebug("COPYTO", aw, "%s:RequestSelection(%d)\n\t%s >%s< length %d\n", 
    	__FILE__, __LINE__,
    	*selection == XA_PRIMARY ? "XA_PRIMARY" : "XA_SECONDARY",
    	!value ? "NULL" : value,
    	*length));

    if ((!value) || (*length == 0))
    {
	if (*selection == XA_SECONDARY)
	{
	    if (client_data->use_ctext == 1){
		client_data->use_ctext = 0;
		XtGetSelectionValue(aw, XA_SECONDARY, XA_STRING,
				RequestSelection,
				(XtPointer)client_data, ev->xbutton.time);
	    }
	    else{
		client_data->use_ctext = 1;
		XtGetSelectionValue(aw, XA_PRIMARY,
				XmInternAtom(XtDisplay(aw), "COMPOUND_TEXT", False),
				RequestSelection,
				(XtPointer)client_data, ev->xbutton.time);
	    }
	}
	else
	{
	    if (client_data->use_ctext == 1){
		client_data->use_ctext = 0;
		XtGetSelectionValue(aw, XA_PRIMARY, XA_STRING,
				RequestSelection,
				(XtPointer)client_data, ev->xbutton.time);
	    }
	    else{
	        DBGW("RequestSelection: no selection available");
	        XtFree((char *)client);
	    }
	}
    }
    else
    {
	if (*selection == XA_SECONDARY)
	{
	    XTextProperty prop;
	    int count, i;
	    char **list;

	    if (*type == XA_STRING){
		char *buf;

		buf = XtMalloc(*length + 1);
		strncpy(buf, (char *)value, *length);
		buf[*length] = '\0';

		/* Convert from XA_STRING to COMPOUND_TEXT */
		XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
				XCompoundTextStyle, &prop);

		 XtFree(buf);
	    }
	    else{
	        prop.value = (unsigned char *)value;
	        prop.encoding = *type;
	        prop.format = *format;
	        prop.nitems = *length;
	    }

	    XmbTextPropertyToTextList(XtDisplay(w), &prop, &list, &count);

	    for(i=0; i<count; i++){
	    	DoInsert(w, ev, list[i], strlen(list[i]));
	    }
	}
	else
	{
	    XmTextPosition pos;

	    pos = TextPixelToSelectionPos(w, ev->xbutton.x);
	    DEBUGOUT(_LtDebug("COPYTO", aw, "%s:RequestSelection(%d) - %d %d\n", 
		__FILE__, __LINE__, ev->xbutton.x, pos));

	    DBG1("event pos", pos);

	    if (_LtDebugInDebug(__FILE__, (Widget)w))
	    {
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			 "RequestSelection: inserting '%s' length=%d at pos: %d\n",
				  (char *)value, (int)(*length), pos));
	    }

	    if (DoCursorMove(w, ev, pos, True, True))
	    {

	    	XTextProperty prop;
	    	int count, i;
	    	char **list;

		if (*type == XA_STRING){
		    char *buf;
		    
		    buf = XtMalloc(*length + 1);
            	    strncpy(buf, (char *)value, *length);
	    	    buf[*length] = '\0';

		    /* Convert from XA_STRING to COMPOUND_TEXT */
	    	    XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
	    			XCompoundTextStyle, &prop);

		    XtFree(buf);
		}
		else{
		    prop.value = (unsigned char *)value;
		    prop.encoding = *type;
		    prop.format = *format;
		    prop.nitems = *length;
		}

	    	XmbTextPropertyToTextList(XtDisplay(w), &prop, &list, &count);

	    	for(i=0; i<count; i++){
	    	    DoInsert(w, ev, list[i], strlen(list[i]));
	    	}
	    }
	}
	XtFree((char *)client);
	XtFree((char *)value);
    }
}


static void
PrimarySelectionComplete(XmTextFieldWidget w, Time time)
{
    int len;

    if (TextF_SelectionText(w))
    {
	XtFree(TextF_SelectionText(w));
    }

    TextF_SelectionText(w) = NULL;
    TextF_SelectionLen(w) = 0;
    len = TextF_HighlightEnd(w) - TextF_HighlightStart(w);

	SetSingleHighlight((Widget)w, TextF_HighlightStart(w), TextF_HighlightEnd(w));

    if (len > 0)
    {
        w->text.has_primary = True;
	TextF_SelectionLen(w) = len;
	TextF_SelectionText(w) = XtMalloc(len);
	strncpy(TextF_SelectionText(w),
		&TextF_Value(w)[TextF_HighlightStart(w)],
		len);

	XtOwnSelection((Widget)w, XA_PRIMARY, time,
		       ConvertSelection, LoseSelection, NULL);

#ifdef DEBUG
	XChangeProperty(XtDisplay((Widget)w),
			DefaultRootWindow(XtDisplay((Widget)w)),
			XA_CUT_BUFFER0, XA_STRING, 8, PropModeReplace,
			(unsigned char *)TextF_SelectionText(w), len);
#endif
    }
    else
    {
        /* SG 23/08/1998, we will get here after a single mouse click
           which needs to clear the primary selection state.
         */
        XmTextFieldSetSelection((Widget)w,-1,-1,time);
    }
}



/*
 * Action procedures ----------------------------------------------------------
 */

static void
_BlinkCursorCallback(XtPointer client_data, XtIntervalId *idp)
{
    XmTextFieldWidget w = (XmTextFieldWidget)client_data;


    /* Force to get forcus to display strings quickly */
    XmImSetFocusValues((Widget)client_data, NULL, 0);

    if (TextF_BlinkOn(w))
    {
	CursorErase(w);
    }
    else
    {
	CursorDraw(w);
    }

    TextF_TimerId(w) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)w),
				       TextF_BlinkRate(w),
				       _BlinkCursorCallback,
				       (XtPointer)w);
}


/* the focus in action routine changes the cursor to be filled,
 * and sets it blinking. */
extern void
_XmTextFieldFocusIn(Widget w, XEvent *event,
		    String *params, Cardinal *num_params)
{
    XmTextVerifyCallbackStruct cbs;
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("_XmTextFieldFocusIn");

    if ( event->type == FocusIn && event->xany.send_event && !TextF_HasFocus(tfw) )
    {
	CursorErase(tfw);
	CursorSet(tfw, True);

	if (TextF_BlinkRate(tfw) > 0 && !TextF_TimerId(tfw))
	{
	    TextF_TimerId(tfw) = XtAppAddTimeOut(
				     XtWidgetToApplicationContext((Widget)tfw),
						    TextF_BlinkRate(tfw),
						    _BlinkCursorCallback,
						    (XtPointer)tfw);
	}
	else
	{
	    CursorDraw(tfw);
	}

	if (TextF_FocusCallback(tfw))
	{
	    cbs.reason = XmCR_FOCUS;
	    cbs.event = event;
	    cbs.currInsert = cbs.newInsert = TextF_CursorPos(tfw);
	    cbs.startPos = cbs.endPos = 0;
	    cbs.text = NULL;

	    XtCallCallbackList((Widget)tfw, TextF_FocusCallback(tfw), &cbs);
	}

    	if (TextF_Editable(tfw))
    	{
	    XmImSetFocusValues(w, NULL, 0);
    	}

    }

    /*
     * 980827 - pgw@hungry.com: Fix for highlight defect.
     *
     * Must call PrimitiveFocusIn routine to show highlight border. 
     * This fixes the following situation for Xm/text/test10.c:
     * The focus is on a button or the text widget.  Our application loses focus
     * by switching to another application with the mouse.  Next, click on the
     * the text field in our application.  If the routine below is not called the
     * widget will not show the highlight border because the primitive never
     * gets the focus in event.
     *
     * This routine will now be called twice under this circumstance.
     */
    XtCallActionProc((Widget)tfw, "PrimitiveFocusIn", event,
			 params, *num_params);

}


/* the focus out action routine changes the cursor to be stippled. */
extern void
_XmTextFieldFocusOut(Widget w, XEvent *event,
		     String *params, Cardinal *num_params)
{
    XmTextVerifyCallbackStruct cbs;
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("_XmTextFieldFocusOut");

    if (TextF_HasFocus(tfw))
    {
	if (TextF_TimerId(tfw))
	{
	    XtRemoveTimeOut(TextF_TimerId(tfw));
	    TextF_TimerId(tfw) = 0;
	}

	CursorErase(tfw);
	CursorSet(tfw, False);
	CursorDraw(tfw);

	if (TextF_LosingFocusCallback(tfw))
	{
	    cbs.reason = XmCR_LOSING_FOCUS;
	    cbs.event = event;
	    cbs.currInsert = cbs.newInsert = TextF_CursorPos(tfw);
	    cbs.startPos = cbs.endPos = 0;
	    cbs.text = NULL;

	    if (XtIsManaged(w))
	    {
		XtCallCallbackList((Widget)tfw, TextF_LosingFocusCallback(tfw),
			       &cbs);
	    }
	}

    	if (TextF_Editable(tfw))
    	{
	    XmImUnsetFocus((Widget)tfw);
    	}
    }

    /*
     * 980827 - pgw@hungry.com: Fix for highlight defect.
     */
    XtCallActionProc((Widget)tfw, "PrimitiveFocusOut", event,
			 params, *num_params);
}


static void
activate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmAnyCallbackStruct cbs;
    Widget parent;

    DBGW("activate");


    cbs.reason = XmCR_ACTIVATE;
    cbs.event = event;

    XtCallCallbackList(w, TextF_ActivateCallback((XmTextFieldWidget)w), &cbs);
/*
   rws 12 Jul 1997
   Pressing return in a TextField that is part of a pre-defined dialog
   should be the same as pressing the OK button in said dialog.
 */
    parent = w;
    while ((parent = XtParent(parent)) != NULL)
    {
	if ((XmIsSelectionBox(parent) || XmIsMessageBox(parent)) && XmIsDialogShell(XtParent(parent)))
	{
	    DEBUGOUT(_LtDebug(__FILE__, parent,
			      "Manager parent activate in TextF\n"));

	    XtCallActionProc(parent, "ManagerParentActivate",
			     event, params, *num_params);
	}
    }
}


static void
backward_character(Widget w, XEvent *ev,
		   String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    DBGW("backward_character");

    if (TextF_CursorPos(tfw) > 0)
    {
	XmTextPosition pos;
	char	*ptr;
	int	next, i;

        ptr = TextF_Value(tfw);
	next = TextF_FontTextWidth(tfw, ptr, TextF_CursorPos(tfw)-1);
	pos = TextF_CursorPos(tfw)-1;
	for(i=TextF_CursorPos(tfw)-2; i>=0; i--){
	    if (next > TextF_FontTextWidth(tfw, ptr, i)){
		pos = i + 1;
		break;
	    }
	    if (next == TextF_FontTextWidth(tfw, ptr, i)){
		pos = i;
	    }
	}

	DoCursorMove(tfw, ev, pos, False, True);
    }
}


static void
backward_word(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    DBGW("backward_word");

    if (TextF_CursorPos(tfw) > 0)
    {
	XmTextPosition pos;

	pos = WordStart(tfw, SkipBackward(tfw, TextF_CursorPos(tfw)));

	DoCursorMove(tfw, ev, pos, False, True);
    }
}


static void
beep(Widget w, XEvent *ev,
     String *params, Cardinal *num_params)
{
    DBGW("beep");

    XBell(XtDisplay(w), 50);
}


static void
beginning_of_line(Widget w, XEvent *ev,
		  String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("beginning_of_line");

    if (TextF_CursorPos(tfw) > 0)
    {
	XmTextPosition pos;

	pos = 0;

	DoCursorMove(tfw, ev, pos, False, True);
    }
}


static void
clear_selection(Widget w, XEvent *ev,
		String *params, Cardinal *num_params)
{
int pos;
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("clear_selection");

    /* SG - 22/08/1998 This apparently replaces the current selection
       with spaces (except for newlines), I am not sure if actually
       clearing the primary selection after this is correct or not,
       O'Reilly doesn't specify.
     */
    if (!TextF_Editable(tfw) || !tfw->text.has_primary)
    {
        VerifyBell(tfw);
	return;
    }

    for(pos = TextF_HighlightStart(tfw);pos<TextF_HighlightEnd(tfw);pos++)
    {
        if(TextF_Value(tfw)[pos] != '\n')
            TextF_Value(tfw)[pos] = ' ';
    }
    XmTextFieldClearSelection(w,ev->xkey.time);
}


/*
 * This is e.g. the Shift<Key>osfInsert action:
 * copy selected text to the clipboard, which is the secondary
 * selection in X.
 *
 * The XmClipboard API does everything for us.
 */
static void
copy_clipboard(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("copy_clipboard");

    XmTextFieldCopy(w, ev->xkey.time);
}


static void
copy_primary(Widget w, XEvent *ev,
	     String *params, Cardinal *num_params)
{
    DBGW("copy_primary");

    /* paste from the primary into text field at the cursor
     * position */
}


static void
copy_to(Widget w, XEvent *ev,
	String *params, Cardinal *num_params)
{
/*
   If secondary exists copy it to just before the insertion cursor.  If not
   copy the primary to the pointer location.
 */
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    DBGW("copy_to");
    DEBUGOUT(_LtDebug("COPYTO", w, "%s:copy_to(%d) - %d\n", 
    	__FILE__, __LINE__, ev->xbutton.x));

    if (TextF_AllowSelection(tfw))
    {
	SelectionClientData *client_data;

	client_data = XtNew(SelectionClientData);
	client_data->ev = XtNew(XEvent);

	*client_data->ev = *ev;
	client_data->use_ctext = 1;

	XtGetSelectionValue(w, XA_SECONDARY,
			XmInternAtom(XtDisplay(w), "COMPOUND_TEXT", False),
			RequestSelection,
			(XtPointer)client_data , ev->xbutton.time);
    }
}


static void
cut_clipboard(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    XmTextFieldCut(w, ev->xbutton.time);
}


static void
cut_primary(Widget w, XEvent *ev,
	    String *params, Cardinal *num_params)
{
    DBGW("cut_primary");
}


static void
delete_next_character(Widget w, XEvent *ev,
		      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("delete_next_character");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    if (tfw->text.has_primary && TextF_PendingDelete(tfw) &&
        TextF_CursorPos(tfw) >= TextF_HighlightStart(tfw) &&
        TextF_CursorPos(tfw) <= TextF_HighlightEnd(tfw))
    {
	int start, end;

	start = TextF_HighlightStart(w);
	end = TextF_HighlightEnd(w);
	XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
        ModifyText(tfw, ev,
                   start, end,
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
    else if (TextF_CursorPos(tfw) < TextF_Length(tfw))
    {
	XmTextPosition pos;
	char    *ptr;
        int     curr, i;

        ptr = TextF_Value(tfw);
	curr = TextF_FontTextWidth(tfw, ptr, TextF_CursorPos(tfw));
	pos = TextF_CursorPos(tfw);
	for(i=TextF_CursorPos(tfw)+1; i<=TextF_Length(tfw); i++){
	    if (curr < TextF_FontTextWidth(tfw, ptr, i)){
		pos = i;
		break;
	    }
	}

        ModifyText(tfw, ev,
                   TextF_CursorPos(tfw), pos,
                   (char *)0, 0,
                   Draw, CP_EndOfInsert);
    }
}


static void
delete_previous_character(Widget w, XEvent *ev,
			  String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("delete_previous_character");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    if (tfw->text.has_primary && TextF_PendingDelete(tfw) &&
        TextF_CursorPos(tfw) >= TextF_HighlightStart(tfw) &&
        TextF_CursorPos(tfw) <= TextF_HighlightEnd(tfw))
    {
	int start, end;

	start = TextF_HighlightStart(w);
	end = TextF_HighlightEnd(w);
	XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
        ModifyText(tfw, ev,
                   start, end,
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
    else if (TextF_CursorPos(tfw) > 0)
    {
	XmTextPosition pos;
        char    *ptr;
        int     next, i;

        ptr = TextF_Value(tfw);
	next = TextF_FontTextWidth(tfw, ptr, TextF_CursorPos(tfw)-1);
	pos = TextF_CursorPos(tfw)-1;
	for(i=TextF_CursorPos(tfw)-2; i>=0; i--){
	    if (next > TextF_FontTextWidth(tfw, ptr, i)){
		pos = i + 1;
		break;
	    }
	    if (next == TextF_FontTextWidth(tfw, ptr, i)){
		pos = i;
	    }
	}

        ModifyText(tfw, ev,
                   pos, TextF_CursorPos(tfw),
                   (char *)0, 0,
                   Draw, CP_EndOfInsert);
    }
}


static void
delete_next_word(Widget w, XEvent *ev,
		 String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("delete_next_word");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    if (tfw->text.has_primary && TextF_PendingDelete(tfw) &&
        TextF_CursorPos(tfw) >= TextF_HighlightStart(tfw) &&
        TextF_CursorPos(tfw) <= TextF_HighlightEnd(tfw))
    {
	int start, end;

	start = TextF_HighlightStart(w);
	end = TextF_HighlightEnd(w);
	XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
        ModifyText(tfw, ev,
                   start, end,
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
    else if (TextF_CursorPos(tfw) < TextF_Length(tfw))
    {
        ModifyText(tfw, ev,
                   TextF_CursorPos(tfw), 
                   SkipForward(tfw, WordEnd(tfw, TextF_CursorPos(tfw))),
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
}


static void
delete_previous_word(Widget w, XEvent *ev,
		     String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("delete_previous_word");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    if (tfw->text.has_primary && TextF_PendingDelete(tfw) &&
        TextF_CursorPos(tfw) >= TextF_HighlightStart(tfw) &&
        TextF_CursorPos(tfw) <= TextF_HighlightEnd(tfw))
    {
	int start, end;

	start = TextF_HighlightStart(w);
	end = TextF_HighlightEnd(w);
	XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
        ModifyText(tfw, ev,
                   start, end,
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
    else if (TextF_CursorPos(tfw) > 0)
    {
        ModifyText(tfw, ev,
                   WordStart(tfw, SkipBackward(tfw, TextF_CursorPos(tfw))),
                   TextF_CursorPos(tfw),
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
}


static void
delete_selection(Widget w, XEvent *ev,
		 String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("delete_selection");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    if (tfw->text.has_primary)
    {
	int start, end;

	start = TextF_HighlightStart(w);
	end = TextF_HighlightEnd(w);
	XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
        ModifyText(tfw, ev,
                   start, end,
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
}


static void
delete_to_end_of_line(Widget w, XEvent *ev,
		      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("delete_to_end_of_line");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    if (TextF_CursorPos(tfw) < TextF_Length(tfw))
    {
        XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
        ModifyText(tfw, ev,
                   TextF_CursorPos(tfw), TextF_Length(tfw),
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
}


static void
delete_to_start_of_line(Widget w, XEvent *ev,
			String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("delete_to_start_of_line");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    if (TextF_CursorPos(tfw) > 0)
    {
        XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
        ModifyText(tfw, ev,
                   0, TextF_CursorPos(tfw),
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);
    }
}


static void
delete_all(Widget w, XEvent *ev,
	   String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    DBGW("delete_all");

    if (!TextF_Editable(tfw))
    {
        VerifyBell(tfw);
	return;
    }

    XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
    ModifyText(tfw, ev,
               0, TextF_Length(tfw),
               (char *)0, 0,
               MassiveChangeDraw, CP_Reset);
}


static void
deselect_all(Widget w, XEvent *ev,
	     String *params, Cardinal *num_params)
{
    DBGW("deselect_all");
    XmTextFieldSetSelection(w,-1,-1,ev->xkey.time);
}


static void
do_quick_action(Widget w, XEvent *ev,
		String *params, Cardinal *num_params)
{
    DBGW("do_quick_action");
}


static void
end_of_line(Widget w, XEvent *ev,
	    String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("end_of_line");

    if (TextF_CursorPos(tfw) < TextF_Length(tfw))
    {
	XmTextPosition pos;

	pos = TextF_Length(tfw);

	DoCursorMove(tfw, ev, pos, False, True);
    }
}


static void
enter(Widget w, XEvent *ev,
      String *params, Cardinal *num_params)
{

    DBGW("enter");

    /* do some textfield specific stuff */

    XtCallActionProc(w, "PrimitiveEnter", ev, params, *num_params);

}


/*
 * extend-adjust : selects text from the anchor to the pointer position and
 *      deselects text outside that range.
 */
static void
extend_adjust(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("extend_adjust");

    if (!TextF_AllowSelection(tfw))
    {
        return;
    }

    tfw->text.select_pos_x = ev->xbutton.x;

    if (ev->xbutton.x < TextF_XDraw(tfw) ||
	ev->xbutton.x > TextF_XDraw(tfw) + TextF_ViewWidth(tfw))
    {
	if (TextF_SelectId(tfw))
	{
	    ExtendHighlight(tfw);
	}
	else
	{
	    ExtendTimer((XtPointer)tfw, (XtIntervalId)0);
	}
    }
    else
    {
	if (TextF_SelectId(tfw))
	{
	    XtRemoveTimeOut(TextF_SelectId(tfw));
	    TextF_SelectId(tfw) = (XtIntervalId)0;
	}

	ExtendHighlight(tfw);
    }
}


/*
 * extend-end() : moves the insertion cursor to the position of the pointer
 */
static void
extend_end(Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
	XmTextFieldWidget tfw = (XmTextFieldWidget)w;

	DBGW("extend_end");

	if (!TextF_AllowSelection(tfw)) {
		return;
	}

	if (TextF_SelectId(tfw)) {
		XtRemoveTimeOut(TextF_SelectId(tfw));
		TextF_SelectId(tfw) = (XtIntervalId)0;
	}

	PrimarySelectionComplete(tfw, ev->xbutton.time);
}


/*
 * extend-start() : adjusts the anchor using te balance-beam method. Selects
 * text from the anchor to the pointer position and deselects text outside
 * that range.
 */
static void
extend_start(Widget w, XEvent *ev,
	     String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    int pos;

    DBGW("extend_start");

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    if (!TextF_AllowSelection(tfw))
    {
	return;
    }

    pos = TextPixelToSelectionPos(tfw, ev->xbutton.x);
    DoCursorMove(tfw, ev, pos, False, True);

    if (TextF_HighlightStart(tfw) < 0)
    {
	TextF_HighlightStart(tfw) =
	    TextF_HighlightEnd(tfw) =
	    TextF_HighlightPivot(tfw) = TextF_CursorPos(tfw);
    }

    if (TextF_CursorPos(tfw) < TextF_HighlightPivot(tfw))
    {
	TextF_HighlightStart(tfw) = TextF_CursorPos(tfw);
    }
    else
    {
	TextF_HighlightEnd(tfw) = TextF_CursorPos(tfw);
    }
}


static void
forward_character(Widget w, XEvent *ev,
		  String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("forward_character");

    if (TextF_CursorPos(tfw) < TextF_Length(tfw))
    {
	XmTextPosition pos;
        char    *ptr;
        int     curr, i;

        ptr = TextF_Value(tfw);
	curr = TextF_FontTextWidth(tfw, ptr, TextF_CursorPos(tfw));
	pos = TextF_CursorPos(tfw);
	for(i=TextF_CursorPos(tfw)+1; i<=TextF_Length(tfw); i++){
	    if (curr < TextF_FontTextWidth(tfw, ptr, i)){
		pos = i;
		break;
	    }
	}

	DoCursorMove(tfw, ev, pos, False, True);
    }
}


static void
forward_word(Widget w, XEvent *ev,
	     String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    DBGW("forward_word");

    if (TextF_CursorPos(tfw) < TextF_Length(tfw))
    {
	XmTextPosition pos;

	pos = SkipForward(tfw, WordEnd(tfw, TextF_CursorPos(tfw)));

	DoCursorMove(tfw, ev, pos, False, True);
    }
}


static void
grab_focus(Widget w, XEvent *ev,
	   String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    XmTextPosition pos;

    DBGW("grab_focus");

    XmProcessTraversal((Widget)tfw, XmTRAVERSE_CURRENT);

    if (!TextF_AllowSelection(tfw))
    {
	return;
    }

    pos = TextPixelToSelectionPos(tfw, ev->xbutton.x);

    if ((tfw->text.last_time + XtGetMultiClickTime(XtDisplay((Widget)tfw))) >
	ev->xbutton.time)
    {
	tfw->text.sarray_index++;
    }
    else
    {
	tfw->text.sarray_index = 0;
    }

    DoScanType(tfw, ev, pos);

    tfw->text.last_time = ev->xbutton.time;
}


#if 0
static void
Help(Widget w, XEvent *ev,
     String *params, Cardinal *num_params)
{
    DBGW("help");
}
#endif


static void
insert_string(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    DBGW("insert_string");
}


static void
key_select(Widget w, XEvent *ev,
	   String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    XmTextPosition old,left,right;
    DBGW("key_select");

    if(*num_params == 0)
    {
        return;
    }

    old = TextF_CursorPos(tfw);
    left = TextF_HighlightStart(tfw);
    right = TextF_HighlightEnd(tfw);
    if(strcmp(params[0], "left") == 0)
    {
        backward_character(w,ev,params,num_params);
        if (old == TextF_CursorPos(tfw))
        {
            return; 
        }

        if (tfw->text.has_primary)
        {
            if (old == left)
            {
                left = TextF_CursorPos(tfw);
            }

            if(old == right)
            {
                right = TextF_CursorPos(tfw);
            }
        }
        else
        {
            left = TextF_CursorPos(tfw);
            right = old;
        }

        XmTextFieldSetSelection(w,left,right,ev->xkey.time);
        return;
    }

    if (strcmp(params[0], "right") == 0)
    {
        forward_character(w,ev,params,num_params);
        if (old == TextF_CursorPos(tfw))
        {
            return;
        }

        if (tfw->text.has_primary)
        {
            if (old == left)
            {
                left = TextF_CursorPos(tfw);
            }

            if(old == right)
            {
                right = TextF_CursorPos(tfw);
            }
        }
        else
        {
            right = TextF_CursorPos(tfw);
            left = old;
        }

        XmTextFieldSetSelection(w,left,right,ev->xkey.time);
        return;
    }
}


static void
kill_next_character(Widget w, XEvent *ev,
		    String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("kill_next_character");

    delete_next_character((Widget)tfw, ev, params, num_params);
}


static void
kill_next_word(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("kill_next_word");

    delete_next_word((Widget)tfw, ev, params, num_params);
}


static void
kill_previous_character(Widget w, XEvent *ev,
			String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("kill_previous_character");

    delete_previous_character((Widget)tfw, ev, params, num_params);
}


static void
kill_previous_word(Widget w, XEvent *ev,
		   String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("kill_previous_word");

    delete_previous_word((Widget)tfw, ev, params, num_params);
}


static void
kill_selection(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("kill_selection");
}


static void
kill_to_end_of_line(Widget w, XEvent *ev,
		    String *params, Cardinal *num_params)
{
    DBGW("kill_to_end_of_line");
}


static void
kill_to_start_of_line(Widget w, XEvent *ev,
		      String *params, Cardinal *num_params)
{
    DBGW("kill_to_start_of_line");
}


static void
leave(Widget w, XEvent *ev,
      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("leave");

    /* do some textfield specific stuff */

    XtCallActionProc((Widget)tfw, "PrimitiveLeave", ev, params, *num_params);

}


static void
move_destination(Widget w, XEvent *ev,
		 String *params, Cardinal *num_params)
{
    DBGW("move_destination");
}


static void
move_to(Widget w, XEvent *ev,
	String *params, Cardinal *num_params)
{
    DBGW("move_to");
}


static void
next_tab_group(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("next_tab_group");

    XtCallActionProc(w, "PrimitiveNextTabGroup", ev, params, *num_params);
}


static void
page_left(Widget w, XEvent *ev,
	  String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("page_left");

    if (TextF_CursorPos(tfw) > 0)
    {
	XmTextPosition pos;

	pos = TextPixelToPos(tfw, TextF_XDraw(tfw) - TextF_ViewWidth(tfw));

	DoCursorMove(tfw, ev, pos, True, True);
    }
}


static void
page_right(Widget w, XEvent *ev,
	   String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("page_right");

    if (TextF_CursorPos(tfw) < TextF_Length(tfw))
    {
	XmTextPosition pos;

	pos = TextPixelToPos(tfw, TextF_XDraw(tfw) + TextF_ViewWidth(tfw));

	DoCursorMove(tfw, ev, pos, True, True);
    }
}


static void
paste_clipboard(Widget w, XEvent *ev,
		String *params, Cardinal *num_params)
{
    DBGW("paste_clipboard");

    XmTextFieldPaste(w);
}


static void
paste_primary(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    DBGW("paste_primary");
}


static void
prev_tab_group(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("prev_tab_group");

    XtCallActionProc(w, "PrimitivePrevTabGroup", ev, params, *num_params);
}


static void
process_bdrag(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    Atom export_target[3];
    Arg args[10];
    int n = 0;
    Widget dc;
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    XmTextPosition position;

    _XmObjectLock(w);
    DBGW("process_bdrag");

    /* If widget has no selection or cursor is not in selection,
       then return. */
    position = TextPixelToSelectionPos(tfw, ev->xbutton.x);

    if (!TextF_SelectionText(tfw)||
    	position < TextF_HighlightStart(tfw) ||
	position >= TextF_HighlightEnd(tfw))
    {

    	_XmObjectUnlock(w);
	return;
    }

    /* set up some stuff */
    export_target[0] = XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    export_target[1] = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    export_target[2] = XA_STRING;

    XtSetArg(args[n], XmNexportTargets, export_target); n++;
    XtSetArg(args[n], XmNnumExportTargets, 3); n++;
    XtSetArg(args[n], XmNdragOperations, XmDROP_COPY|XmDROP_MOVE); n++;
    XtSetArg(args[n], XmNclientData, w); n++;
    XtSetArg(args[n], XmNconvertProc, drag_convert_proc); n++;
    XtSetArg(args[n], XmNsourceCursorIcon, _XmGetTextualDragIcon(w)); n++;

    dc = XmDragStart(w, ev, args, n);

    if (dc)
    {
	XtAddCallback(dc, XmNdragDropFinishCallback, drag_drop_finish, NULL);
    }
    _XmObjectUnlock(w);
}


static void
drag_drop_finish(Widget w, XtPointer client_data, XtPointer call_data)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:drag_drop_finish(%d)\n",
    	__FILE__, __LINE__));
}


static Boolean
drag_convert_proc(Widget w, Atom *selection,
		  Atom *target, Atom *type_return,
		  XtPointer *value_return,
		  unsigned long *length_return,
		  int *format_return)
{
    XmTextFieldWidget tfw;
    Widget src;
    Atom atom_comp_text;
    Atom atom_text;
    Atom atom_motif_drop;
    Atom atom_delete;

    atom_comp_text =  XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    atom_text = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    atom_motif_drop =  XmInternAtom(XtDisplay(w), _XA_MOTIF_DROP, False);
    atom_delete =  XmInternAtom(XtDisplay(w), _XA_DELETE, False);

    if (*selection != atom_motif_drop)
    {
    	DEBUGOUT(_LtDebug(__FILE__, w, "selection is not MOTIF_DROP\n"));
	return False;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "We're dealing with MOTIF_DROP\n"));

    XtVaGetValues(w, XmNclientData, &src, NULL);
    tfw = (XmTextFieldWidget)src;
    _XmObjectLock(src);
 
    if (*target == XA_STRING){
        *length_return = (long)TextF_SelectionLen(tfw);
        *value_return = XtNewString(TextF_SelectionText(tfw));
        *type_return = XA_STRING;
    }
    else if (*target == atom_comp_text || *target == atom_text){

	    XTextProperty prop;
	    char *buf;
	    int ret;

	    buf = XtMalloc(TextF_SelectionLen(tfw) + 1);
            strncpy(buf, TextF_SelectionText(tfw), TextF_SelectionLen(tfw));
	    buf[TextF_SelectionLen(tfw)] = '\0';

	    ret = XmbTextListToTextProperty(XtDisplay(tfw), &buf, 1,
	    			XCompoundTextStyle, &prop);
		
	    XtFree(buf);
	    if (ret != 0){
	    	*length_return = 0;
	    	*value_return  = NULL;
	    }
	    else{
	    	buf = XtMalloc(prop.nitems + 1);
		memcpy(buf, prop.value, prop.nitems);
		buf[prop.nitems] = '\0';
	    	*length_return = prop.nitems;
	    	*value_return  = buf;
	    }
	   *type_return = atom_comp_text;

    }
    else if (*target == atom_delete)
    {
        /* DELETE Selected Text */
	int start, end;

	start = TextF_HighlightStart((Widget)src);
	end = TextF_HighlightEnd((Widget)src);
	XmTextFieldSetSelection((Widget)src,-1,-1,CurrentTime);
        ModifyText(tfw, NULL,
                   start,
		   end,
                   (char *)0, 0,
                   MassiveChangeDraw, CP_EndOfInsert);

        *value_return = NULL;
        *type_return = XmInternAtom(XtDisplay(w), "NULL", False);
        *length_return = 0;
    }
    else
    {
        _XmObjectUnlock(src);
	return False;
    }

    *format_return = 8;

    _XmObjectUnlock(src);
    return True;
}


static void
drag_transfer_proc(Widget dw, XtPointer client, Atom *selection, Atom *type,
		 XtPointer value, unsigned long *length, int *format)
{
    XmTextFieldWidget w = (XmTextFieldWidget)client;
    Atom atom_comp_text;
    Atom atom_text;
    Atom atom_null;
    Arg args[2];
    char *atom_name;

    atom_comp_text =  XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    atom_text = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    atom_null =  XmInternAtom(XtDisplay(w), "NULL", False);

    atom_name = XGetAtomName(XtDisplay(dw), *type);

    if (*type != atom_null && (!value || 
    	(*type != atom_comp_text && *type != atom_text && *type != XA_STRING)))
    {

	DBGW("drag_transfer_proc: DnD Transfer Failed(TextField)");

        XtSetArg(args[0], XmNtransferStatus, XmTRANSFER_FAILURE);
        XtSetArg(args[1], XmNnumDropTransfers, 0);
        XtSetValues(dw, args, 2);

    }
    else if (*type != atom_null)
    {
	XTextProperty prop;
	int count, i;
	char **list;

	if (*type == XA_STRING){
	    char *buf;

	    buf = XtMalloc(*length + 1);
	    strncpy(buf, (char *)value, *length);
	    buf[*length] = '\0';

	    /* Convert from XA_STRING to COMPOUND_TEXT */
	    XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
			XCompoundTextStyle, &prop);

	    XtFree(buf);
        }
        else{
            prop.value = (unsigned char *)value;
            prop.encoding = *type;
            prop.format = *format;
            prop.nitems = *length;
        }

        XmbTextPropertyToTextList(XtDisplay(w), &prop, &list, &count);

        for(i=0; i<count; i++){
    	    DoInsert(w, NULL, list[i], strlen(list[i]));
        }

    }

    if (value)
        XtFree((char *)value);
}


static void
process_drop(Widget w, XtPointer client_data, XtPointer call_data)
{
    XmDropProcCallbackStruct *DropInfo =
        (XmDropProcCallbackStruct *) call_data;
    XmDropTransferEntryRec  entry[2];
    Atom atom_comp_text;
    Atom atom_text;
    Atom atom_null;
    Atom *export_targets;
    Atom target;
    Cardinal num_export_targets;
    Arg     args[10];
    Cardinal    n = 0;
    XmTextPosition pos;
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    Boolean comp_text, text, string; 

    atom_comp_text =  XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    atom_text = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
    atom_null = XmInternAtom(XtDisplay(w), "NULL", False);

    if ((DropInfo->dropAction == XmDROP) && 
        ((DropInfo->operation == XmDROP_COPY) ||
	 (DropInfo->operation == XmDROP_MOVE)))
    {

        /* Get valid target */
        XtSetArg(args[n], XmNexportTargets, &export_targets); n++;
        XtSetArg(args[n], XmNnumExportTargets, &num_export_targets); n++;
        XtGetValues(DropInfo->dragContext, args, n);
	comp_text = text = string = False;
        for (n = 0; n < num_export_targets; n++) {
            if (export_targets[n] == atom_comp_text)
                comp_text = True;
	    else if (export_targets[n] == atom_text)
                text = True;
	    else if (export_targets[n] == XA_STRING)
                string = True;
        }
	if (comp_text)
	    target = atom_comp_text;
	else if (text)
	    target = atom_text;
	else if (string)
	    target = XA_STRING;
	else
	    target = atom_null;

        entry[0].target = target;
        entry[0].client_data = (XtPointer) w;

	n = 0;
	if (target == atom_null)
	{
            XtSetArg(args [n], XmNtransferStatus, XmTRANSFER_FAILURE); n++;
            XtSetArg(args [n], XmNnumDropTransfers, 0); n++;
            DropInfo->operation = XmDROP_NOOP;
            DropInfo->dropSiteStatus = XmINVALID_DROP_SITE;
	}
	else if (DropInfo->operation == XmDROP_MOVE)
	{
            entry[1].target = XmInternAtom(XtDisplay(w), _XA_DELETE, False);
            entry[1].client_data = (XtPointer) w;
            XtSetArg(args [n], XmNnumDropTransfers, 2); n++;
	    DropInfo->dropSiteStatus = XmVALID_DROP_SITE;
	}
	else 
	{
            XtSetArg(args [n], XmNnumDropTransfers, 1); n++;
	    DropInfo->dropSiteStatus = XmVALID_DROP_SITE;
	}
        XtSetArg(args [n], XmNdropTransfers, entry); n++;
        XtSetArg(args [n], XmNtransferProc, drag_transfer_proc); n++;

	/* FIX ME We should move cursor at this point? */
        pos = TextPixelToSelectionPos(tfw, DropInfo->x);
	DoCursorMove(tfw, NULL, pos, True, True);
    }
    else /* veto */
    {
        XtSetArg(args [n], XmNtransferStatus, XmTRANSFER_FAILURE); n++;
        XtSetArg(args [n], XmNnumDropTransfers, 0); n++;
        DropInfo->operation = XmDROP_NOOP;
        DropInfo->dropSiteStatus = XmINVALID_DROP_SITE;
    }

    XmDropTransferStart (DropInfo->dragContext, args, n);    

}


static void
process_cancel(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("process_cancel");
    /* rws 15 Jan 2000
       There is more to it than this. It needs to cancel some operations that
       may be in progress, otherwise do this. Look at the man page.
     */
    if (XmIsManager(XtParent(w)))
    {
    	XtCallActionProc(XtParent(w), "ManagerParentCancel", ev, params, *num_params);
    }
}


static void
process_home(Widget w, XEvent *ev,
	     String *params, Cardinal *num_params)
{
    DBGW("process_home");
}


static void
process_return(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("process_return");
}


static void
process_tab(Widget w, XEvent *ev,
	    String *params, Cardinal *num_params)
{
    DBGW("process_tab");
}


static void
quick_copy_set(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("quick_copy_set");
}


static void
quick_cut_set(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    DBGW("quick_cut_set");
}


static void
redraw_display(Widget w, XEvent *ev,
	       String *params, Cardinal *num_params)
{
    DBGW("redraw_display");
    Draw((XmTextFieldWidget)w);
}


static void
secondary_adjust(Widget w, XEvent *ev,
		 String *params, Cardinal *num_params)
{
    DBGW("secondary_adjust");
}


static void
secondary_notify(Widget w, XEvent *ev,
		 String *params, Cardinal *num_params)
{
    DBGW("secondary_notify");
}


static void
secondary_start(Widget w, XEvent *ev,
		String *params, Cardinal *num_params)
{
    DBGW("secondary_start");
}


static void
select_adjust(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    DBGW("select_adjust");
}


static void
select_all(Widget w, XEvent *ev,
	   String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("select_all");

    TextF_HighlightPivot(tfw) = TextF_HighlightStart(tfw) = 0;
    TextF_HighlightEnd(tfw) = TextF_Length(tfw);

    /* FIX ME - what event type should this be? */
    PrimarySelectionComplete(tfw, ev->xbutton.time);

    Draw(tfw);
}


static void
select_end(Widget w, XEvent *ev,
	   String *params, Cardinal *num_params)
{
    DBGW("select_end");
}


static void
select_start(Widget w, XEvent *ev,
	     String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("select_start");

    grab_focus((Widget)tfw, ev, params, num_params);
}


static void
self_insert(Widget w, XEvent *ev,
	    String *params, Cardinal *num_params)
{
#define INSERTCHARBUFSIZ 128
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;
    char buf[INSERTCHARBUFSIZ], *buf2;
    KeySym keysym;
    int len, status;

    DBGW("self_insert");

    len = XmImMbLookupString((Widget)tfw,
			     (XKeyPressedEvent *)ev, buf,
			     INSERTCHARBUFSIZ,
			     &keysym,
			     &status);

    /* Handle Oveflow case. */
    buf2 = XtMalloc(len+1);
    if (status == XBufferOverflow)
	XmImMbLookupString((Widget)tfw, (XKeyPressedEvent *)ev, buf2, len, &keysym, &status);
    else
	strncpy(buf2, buf, len);

    if (_LtDebugInDebug(__FILE__, (Widget)tfw))
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)tfw,
			  "XmImMbLookupString => %d\n", len));

	DEBUGOUT(_LtDebug0(__FILE__, (Widget)tfw, "\tStatus %s\n",
			   (status == XLookupNone) ? "none" :
			   (status == XLookupChars) ? "chars" :
			   (status == XLookupBoth) ? "both" :
			   (status == XLookupKeySym) ? "keysym" :
			   (status == XBufferOverflow) ? "overflow" : "????"));

	if (status == XLookupBoth || status == XLookupKeySym)
	{
	    DEBUGOUT(_LtDebug0(__FILE__, (Widget)tfw,
			       "\tKeySym 0x%X\n", keysym));
	}
	if (len > 0)
	{
	    int i;

	    DEBUGOUT(_LtDebug0(__FILE__, (Widget)tfw, "\tBuffer "));
	    for (i = 0; i < len; i++)
	    {
		DEBUGOUT(_LtDebug(__FILE__, (Widget)tfw, " %X", 0xFF & buf[i]));
	    }

	    DEBUGOUT(_LtDebug0(__FILE__, (Widget)tfw, "\n"));
	}
    }

    if (len > 0)
    {	/* FIX ME */
	if (status == XLookupBoth || status == XLookupChars)
	{
	    DoInsert(tfw, ev, buf2, len);
	}
    }
    XtFree(buf2);
}


static void
set_anchor(Widget w, XEvent *ev,
	   String *params, Cardinal *num_params)
{
    DBGW("set_anchor");
}


static void
set_insertion_point(Widget w, XEvent *ev,
		    String *params, Cardinal *num_params)
{
    DBGW("set_insertion_point");
}


static void
set_selection_hint(Widget w, XEvent *ev,
		   String *params, Cardinal *num_params)
{
    DBGW("set_selection_hint");
}


static void
toggle_add_mode(Widget w, XEvent *ev,
		String *params, Cardinal *num_params)
{
	DBGW("toggle_add_mode");
	TextF_AddMode(w) = ! TextF_AddMode(w);
}


static void
traverse_home(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("traverse_home");

    XmProcessTraversal((Widget)tfw, XmTRAVERSE_HOME);
}


static void
traverse_next(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("traverse_next");

    XmProcessTraversal((Widget)tfw, XmTRAVERSE_NEXT);
}


static void
traverse_prev(Widget w, XEvent *ev,
	      String *params, Cardinal *num_params)
{
    XmTextFieldWidget tfw = (XmTextFieldWidget)w;

    DBGW("traverse_prev");

    XmProcessTraversal((Widget)tfw, XmTRAVERSE_PREV);
}


static void
unkill(Widget w, XEvent *ev,
       String *params, Cardinal *num_params)
{
    DBGW("unkill");
}


static void
unmap(Widget w, XEvent *ev,
      String *params, Cardinal *num_params)
{
    DBGW("unmap");
}


/*
 * Public functions -----------------------------------------------------------
 */

extern Widget
XmCreateTextField(Widget parent, char *name, Arg *arglist, Cardinal argCount)
{
	Widget	r;

	_XmObjectLock(parent);
	r = XtCreateWidget(name, xmTextFieldWidgetClass, parent,
			  arglist, argCount);
	_XmObjectUnlock(parent);
	return r;
}


extern void
XmTextFieldClearSelection(Widget aw, Time time)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);

    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }
    /* FIX ME */
    /* SG 22/08/1998, O'Reilly says this deselects text without effecting
       the clipboard or selection properties - it only effects the selected
       text in the widget, however it also says it clears the primary
       selection and indicates communication with the clipboard hence the
       time parameter. This appears to do the right thing in that it
       disowns the selection and clears the selected text in the
       widget.
    */
    XmTextFieldSetSelection(aw,-1,-1,time);
    _XmObjectUnlock(aw);
}


/*
 * XmTextFieldCopy
 *
 * this function must copy the selected area of the widget
 * into the clipboard
 */
Boolean
XmTextFieldCopy(Widget aw, Time time)
{
    XmTextFieldWidget	w = (XmTextFieldWidget)aw;
    Display		*dpy;
    Window		win;
    int			r;
    long		item_id;
    XmString		lb = NULL;

    _XmObjectLock(aw);
    if (!XmIsTextField(aw)) {
	_XmObjectUnlock(aw);
	return False;
    }
    if (aw == NULL || XtWindow(aw) == 0) {
	_XmObjectUnlock(aw);
	return False;
    }

    dpy = XtDisplay(aw);
    win = XtWindow(aw);

    if (TextF_SelectionText(w) == NULL) {
	_XmObjectUnlock(aw);
	return False;
    }

    lb = XmStringCreateLocalized("XmTextField");

    while ((r = XmClipboardStartCopy(dpy, win, lb, time,
		NULL, NULL, &item_id)) == XmClipboardLocked)
	;

    XmStringFree(lb);

    if (r != XmClipboardSuccess) {
	_XmObjectUnlock(aw);
	return False;
    }

    while ((r = XmClipboardCopy(dpy, win, item_id, "STRING",
		TextF_SelectionText(w), TextF_SelectionLen(w),
		0, NULL)) == XmClipboardLocked)
	;

    if (r != XmClipboardSuccess) {
	(void) XmClipboardEndCopy(dpy, win, item_id);
	_XmObjectUnlock(aw);
	return False;
    }

    while ((r = XmClipboardEndCopy(dpy, win, item_id)) == XmClipboardLocked)
	;

    if (r == XmClipboardSuccess) {
	_XmObjectUnlock(aw);
	return True;
    } else {
	_XmObjectUnlock(aw);
	return False;
    }
}


extern Boolean
XmTextFieldCut(Widget aw, Time time)
{
    XmTextFieldWidget	w = (XmTextFieldWidget)aw;
    int first,last;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return False;
    }

    if (TextF_SelectionText(w) == NULL || !TextF_Editable(w)) {
	_XmObjectUnlock(aw);
	return False;
    }

    if (XmTextFieldCopy(aw, time) == False) {
	_XmObjectUnlock(aw);
	return False;
    }

    last = TextF_HighlightEnd(w);
    first = TextF_HighlightStart(w);

    XmTextFieldSetSelection(aw,-1,-1,CurrentTime);
    ModifyText(w,(XEvent *)0,first,last,NULL,0,MassiveChangeDraw,CP_Unchanged);
            
    _XmObjectUnlock(aw);
    return True;
}


extern int
XmTextFieldGetBaseline(Widget aw)
{
    XmTextFieldWidget	w = (XmTextFieldWidget)aw;
    int			b;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return 0;
    }

    b = TextF_YOffset(w);
    _XmObjectUnlock(aw);
    return b;
}


extern XmTextPosition
XmTextFieldGetCursorPosition(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XmTextPosition	p;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return 0;
    }

    p = TextF_CursorPos(w);
    _XmObjectUnlock(aw);
    return p;
}


extern Boolean
XmTextFieldGetEditable(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    Boolean		b;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return 0;
    }

    b = TextF_Editable(w);
    _XmObjectUnlock(aw);
    return b;
}


extern XmTextPosition
XmTextFieldGetInsertionPosition(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XmTextPosition	p;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return 0;
    }

    p = TextF_CursorPos(w);
    _XmObjectUnlock(aw);
    return p;
}


extern XmTextPosition
XmTextFieldGetLastPosition(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XmTextPosition	p;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return 0;
    }

    p =  TextF_Length(w);
    _XmObjectUnlock(aw);
    return p;
}


extern int
XmTextFieldGetMaxLength(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    int	l;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return 0;
    }

    l = TextF_MaxLength(w);
    _XmObjectUnlock(aw);
    return l;
}


extern char *
XmTextFieldGetSelection(Widget aw)
{
    char *substr;
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return NULL;
    }

    /* FIX ME */
    /* SG 22/08/1998 */
    if(TextF_SelectionText(w) != NULL) {
	int nchars = TextF_SelectionLen(w);

        substr = XtMalloc(nchars+1); 
        strncpy(substr,TextF_SelectionText(w),nchars);
        substr[nchars] = '\0';

	_XmObjectUnlock(aw);
        return substr;
    }

    _XmObjectUnlock(aw);
    return NULL;
}


extern Boolean
XmTextFieldGetSelectionPosition(Widget aw,
				XmTextPosition *left, XmTextPosition *right)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    Boolean		b;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return False;
    }

    /* FIX ME */
    /* SG 22/08/1998 , what is wrong with this ? */
    *left = TextF_HighlightStart(w);
    *right = TextF_HighlightEnd(w);
    b = w->text.has_primary;

    _XmObjectUnlock(aw);
    return b;
}


extern wchar_t *
XmTextFieldGetSelectionWcs(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return NULL;
    }
    /* FIX ME */

    _XmObjectUnlock(aw);
    return NULL;
}


extern char *
XmTextFieldGetString(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    char *ret;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	ret = XtMalloc(1);
	*ret = '\0';

	_XmObjectUnlock(aw);
	return ret;
    }

    ret = XtMalloc(TextF_Length(w) + 1);
    strncpy(ret, TextF_Value(w), TextF_Length(w));
    ret[TextF_Length(w)] = '\0';

    _XmObjectUnlock(aw);
    return ret;
}


extern wchar_t *
XmTextFieldGetStringWcs(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w)) {
	_XmObjectUnlock(aw);
	return NULL;
    }
    /* FIX ME */

    _XmObjectUnlock(aw);
    return NULL;
}


extern int
XmTextFieldGetSubstring(Widget aw, XmTextPosition start, int num_chars,
			int buffer_size, char *buffer)
{
    int len;
    int retval = XmCOPY_SUCCEEDED;
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(aw))
    {
	_XmObjectUnlock(aw);
	return XmCOPY_FAILED;
    }

    if (start < 0 || start > TextF_Length(w))
    {
	_XmObjectUnlock(aw);
	return XmCOPY_FAILED;
    }


    if (start + num_chars > TextF_Length(w))
    {
	len = TextF_Length(w) - start;
    } else {
	len = num_chars;
    }

    if(len < num_chars) {
	retval = XmCOPY_TRUNCATED;
    } /* no else: retval may be updated to XmCOPY_FAILED in next test */

    if(len > buffer_size) {
	retval = XmCOPY_FAILED;
    } else {
        strncpy(buffer, TextF_Value(w) + start, len);
        buffer[len] = '\0';
    }

    _XmObjectUnlock(aw);
    return retval;
}


extern int
XmTextFieldGetSubstringWcs(Widget aw, XmTextPosition start,
			   int num_chars, int buffer_size,
			   wchar_t *buffer)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return XmCOPY_FAILED;
    }
    /* FIX ME */

    _XmObjectUnlock(aw);
    return 0;
}


extern void
XmTextFieldInsert(Widget aw, XmTextPosition pos, char *str)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XmTextPosition len;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    if (str && ((len = strlen(str)) > 0) && pos >= 0 && pos <= TextF_Length(w))
    {
	TextF_CursorPos(w) = TextF_HighlightStart(w) = TextF_HighlightEnd(w) = pos;
	TextInsert(w, str, len);
	MassiveChangeDraw(w);
    }
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldInsertWcs(Widget aw, XmTextPosition position, wchar_t *wcstring)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }
    /* FIX ME */
    _XmObjectUnlock(aw);
}


/* This is apparently unused - Jamie, 1 Feb 1999 */
#if 0
/* ARGSUSED */
static void
ClipboardRequestSelection(Widget aw,
		XtPointer client,
		Atom *selection,
		Atom *type,
		XtPointer value,
		unsigned long *length,
		int *format)
{
    XmTextFieldWidget	w = (XmTextFieldWidget)aw;
    Time		t;

    t = XtLastTimestampProcessed(XtDisplay(aw));

    if (value == NULL || *length == 0)
    {
	DBGW("ClipboardRequestSelection: no selection available");

	switch ((int)client) {
	case 0:
		XtGetSelectionValue(aw, XA_SECONDARY,
			XmInternAtom(XtDisplay(aw), "COMPOUND_TEXT", False),
			ClipboardRequestSelection,
			(XtPointer)1, t);
		break;
	case 1:
		XtGetSelectionValue(aw, XA_PRIMARY, XA_STRING,
			ClipboardRequestSelection,
			(XtPointer)2, t);
		break;
	case 2:
		XtGetSelectionValue(aw, XA_PRIMARY,
			XmInternAtom(XtDisplay(aw), "COMPOUND_TEXT", False),
			ClipboardRequestSelection,
			(XtPointer)3, t);
		break;
	default:
		DBGW("ClipboardRequestSelection: really no selection");
	}
    }
    else
    {
	XmTextPosition pos;

	pos = TextF_CursorPos(w);

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		"ClipboardRequestSelection: inserting '%s' len %d at %d\n",
			      (char *)value, (int)(*length), pos));

	if (DoCursorMove(w, NULL, pos, True, True))
	{
	    DoInsert(w, NULL, (char *)value, (int)(*length));
	}
    }
}
#endif


/*
 * This should try to get the secondary selection,
 * but if it doesn't find that it should try to get
 * the primary selection instead.
 *
 * Complication is that this can apply to multiple formats :
 *	XA_STRING, but COMPOUND_TEXT as well.
 */
extern Boolean
XmTextFieldPaste(Widget aw)
{
    XmTextFieldWidget	w = (XmTextFieldWidget)aw;
    Display		*dpy;
    Window		win;
    int			r;
    unsigned long	len, nbytes;
    long		private_id;
    char		*buf;
    XmTextPosition	pos;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return False;
    }

    if (aw == NULL) {
	_XmObjectUnlock(aw);
	return False;
    }

    dpy = XtDisplay(aw);
    win = XtWindow(aw);

    if (win == 0) {
	_XmObjectUnlock(aw);
	return False;
    }

    /* FIX ME */
    DEBUGOUT(_LtDebug(__FILE__, aw, "XmTextFieldPaste\n"));

    while ((r = XmClipboardStartRetrieve(XtDisplay(aw), XtWindow(aw),
		XtLastTimestampProcessed(XtDisplay(aw)))) == XmClipboardLocked)
	;
    if (r != XmClipboardSuccess) {
	_XmObjectUnlock(aw);
	return False;
    }

    /* Figure out how much we need to paste */
    while ((r = XmClipboardInquireLength(dpy, win, "STRING", &len))
		== XmClipboardLocked)
	;
    if (r != XmClipboardSuccess) {
	_XmObjectUnlock(aw);
	return False;
    }

    buf = XtMalloc(len + 1);

    while ((r = XmClipboardRetrieve(dpy, win, "STRING",
		buf, len, &nbytes, &private_id)) == XmClipboardLocked)
	;

    if (r != XmClipboardSuccess) {
	XtFree(buf);
	_XmObjectUnlock(aw);
	return False;
    }
    DEBUGOUT(_LtDebug(__FILE__, aw,
	"XmTextFieldPaste: should be getting %ld bytes\n", nbytes));

    while ((r = XmClipboardEndRetrieve(dpy, win)) == XmClipboardLocked)
	;
    if (r != XmClipboardSuccess) {
	XtFree(buf);
	_XmObjectUnlock(aw);
	return False;
    }

    /* Now insert the text ... */
    pos = TextF_CursorPos(w);

    if (DoCursorMove(w, NULL, pos, True, True))
    {
	DoInsert(w, NULL, (char *)buf, len);
    }
    XtFree(buf);
    _XmObjectUnlock(aw);
    return True;
}


extern Boolean
XmTextFieldPosToXY(Widget aw, XmTextPosition position, Position *x, Position *y)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    int pixel;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return False;
    }

    if (position < 0 || position > TextF_Length(w))
    {
	_XmObjectUnlock(aw);
	return False;
    }

    pixel = TextF_XOffset(w) + TextF_FontTextWidth(w, TextF_Value(w), position);
    if (pixel < TextF_XDraw(w) || pixel > (TextF_XDraw(w) + TextF_ViewWidth(w)))
    {
	_XmObjectUnlock(aw);
	return False;
    }

    *x = (Position)pixel;
    *y = (Position)TextF_YOffset(w);

    _XmObjectUnlock(aw);
    return True;
}


extern Boolean
XmTextFieldRemove(Widget aw)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    int first,last; 

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return False;
    }

    if(TextF_SelectionText(w) == NULL || !TextF_Editable(w))
    {
	_XmObjectUnlock(aw);
        return False;
    }

    last = TextF_HighlightEnd(w);
    first = TextF_HighlightStart(w);

    XmTextFieldSetSelection(aw,-1,-1,CurrentTime);
    ModifyText(w,(XEvent *)0,first,last,NULL,0,MassiveChangeDraw,CP_EndOfInsert);
            
    _XmObjectUnlock(aw);
    return True;
}


extern void
XmTextFieldReplace(Widget aw, XmTextPosition first,
		   XmTextPosition last, char *str)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XmTextPosition len;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    if (str)
    {
	len = strlen(str);
	if (last > TextF_Length(w))
	    last = TextF_Length(w);
	if (first <= last)
	{
            XmTextFieldSetSelection(aw,-1,-1,CurrentTime);
            ModifyText(w, (XEvent *)0,
                       first, last,
                       str, len,
                       MassiveChangeDraw, CP_EndOfInsert);
	}
    }
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldReplaceWcs(Widget aw, XmTextPosition from_pos,
		      XmTextPosition to_pos, wchar_t *wcstring)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }
    /* FIX ME */
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldSetAddMode(Widget aw, Boolean state)
{
	XmTextFieldWidget w = (XmTextFieldWidget)aw;

	_XmObjectLock(aw);
	if (XmIsText(w)) {
		_XmObjectUnlock(aw);
		XmTextSetAddMode(aw, state);
		return;
	} else if (!XmIsTextField(w)) {
		_XmObjectUnlock(aw);
		_XmWarning(aw, "XmTextFieldSetAddMode: widget has invalid class");
		return;
	}
	TextF_CursorAddMode(w) = state;
	_XmObjectUnlock(aw);
}


extern void
XmTextFieldSetCursorPosition(Widget aw, XmTextPosition pos)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    if (pos >= 0 && pos <= TextF_Length(w))
    {
	TextF_CursorPos(w) = pos;
        (void)_XmImSendSpot((Widget)aw);

	MassiveChangeDraw(w);
    }
    _XmObjectUnlock(aw);
}


static void
_XmTextFieldSetEditable(Widget w, Boolean e)
{
    if (!XtIsRealized(w))
    {
	return;
    }

    if (e)
    {				/* Becomes editable */
	Arg args[10];
	int nargs;

	XmImRegister(w, 0);

	nargs = 0;
	XtSetArg(args[nargs], XmNbackground, XtBackground(w));
	nargs++;
	XtSetArg(args[nargs], XmNforeground, Prim_Foreground(w));
	nargs++;
	XmImSetValues(w, args, nargs);
    }
    else
    {				/* Becomes un-editable */
	XmImUnregister(w);
    }
}


extern void
XmTextFieldSetEditable(Widget aw, Boolean editable)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    if (TextF_Editable(w) != editable)
    {
	_XmTextFieldSetEditable(aw, editable);
    }

    TextF_Editable(w) = editable;
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldSetHighlight(Widget aw, XmTextPosition left, XmTextPosition right,
                        XmHighlightMode mode)
{
	int		i, j;
	XmHighlightMode	em;

	/*
	 * This call does no explicit selection; it just
	 * changes the visual appearance of the text!
	 */
	XmTextFieldWidget w = (XmTextFieldWidget)aw;

	_XmObjectLock(aw);
	if (!XmIsTextField(w)) {
		_XmObjectUnlock(aw);
		return;
	}
#if 0
	/* Simplistic implementation */
	TextF_HighlightMode(w) = mode;
	switch(mode) {
	case XmHIGHLIGHT_SELECTED:
		/* toggle to "reverse video" */
		TextF_HighlightStart(w) = left;
		TextF_HighlightEnd(w)   = right;
		break;
	case XmHIGHLIGHT_SECONDARY_SELECTED:
		/* toggle to underlining */
		/* FIX ME ! */
		TextF_HighlightStart(w) = left;
		TextF_HighlightEnd(w)   = right;
		break;	 
	case XmHIGHLIGHT_NORMAL:
	default:
		/* remove highlighting */
		TextF_HighlightStart(w) = TextF_HighlightEnd(w) = -1;
		break;
	}
#else
	/* Figure out the original mode for the 'right' point */
	for (i=0; i<TextF_Highlight(w).number-1; i++)
		if (TextF_Highlight(w).list[i].position < right
				&& right < TextF_Highlight(w).list[i+1].position)
			em = TextF_Highlight(w).list[i].mode;

	/* Insert an entry for the 'left' */
	for (i=0; i<TextF_Highlight(w).number; i++) {
		if (TextF_Highlight(w).list[i].position == left) {
			TextF_Highlight(w).list[i].mode = mode;
		} else if (TextF_Highlight(w).list[i].position < left
			&& left < TextF_Highlight(w).list[i+1].position) {
			if (TextF_Highlight(w).number == TextF_Highlight(w).maximum) {
				TextF_Highlight(w).maximum += 8;
				TextF_Highlight(w).list = (_XmHighlightRec *)XtRealloc(
					(char *)TextF_Highlight(w).list,
					sizeof(_XmHighlightRec) * TextF_Highlight(w).maximum);
			}
			TextF_Highlight(w).number ++;
			for (j=TextF_Highlight(w).number; j>i; j--)
				TextF_Highlight(w).list[j] = TextF_Highlight(w).list[j-1];
			TextF_Highlight(w).list[i+1].position = left;
			TextF_Highlight(w).list[i+1].mode = mode;
		}
	}

	/* Insert an entry for the 'right' */
	for (i=0; i<TextF_Highlight(w).number; i++) {
		if (TextF_Highlight(w).list[i].position == right) {
			TextF_Highlight(w).list[i].mode = em;
		} else if (TextF_Highlight(w).list[i].position < right
			&& right < TextF_Highlight(w).list[i+1].position) {
			if (TextF_Highlight(w).number == TextF_Highlight(w).maximum) {
				TextF_Highlight(w).maximum += 8;
				TextF_Highlight(w).list = (_XmHighlightRec *)XtRealloc(
					(char *)TextF_Highlight(w).list,
					sizeof(_XmHighlightRec) * TextF_Highlight(w).maximum);
			}
			TextF_Highlight(w).number ++;
			for (j=TextF_Highlight(w).number; j>i; j--)
				TextF_Highlight(w).list[j] = TextF_Highlight(w).list[j-1];
			TextF_Highlight(w).list[i+1].position = right;
			TextF_Highlight(w).list[i+1].mode = em;
		}
	}
#endif
	_XmObjectUnlock(aw);
}


extern void
XmTextFieldSetInsertionPosition(Widget aw, XmTextPosition pos)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    if (pos >= 0 && pos <= TextF_Length(w))
    {
	TextF_CursorPos(w) = pos;

	MassiveChangeDraw(w);
    }
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldSetMaxLength(Widget aw, int max_length)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    TextF_MaxLength(w) = max_length;

    if (TextF_Length(w) > max_length)
    {
	TextF_Length(w) = max_length;
	TextF_Value(w)[max_length] = '\0';

	if (TextF_CursorPos(w) > max_length)
	{
	    TextF_CursorPos(w) = max_length;
	}

	MassiveChangeDraw(w);
    }
    _XmObjectUnlock(aw);
}


/* ARGSUSED */
extern void
XmTextFieldSetSelection(Widget aw, XmTextPosition start, XmTextPosition end, Time time)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    Boolean gain = False;
    int len;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmTextFieldSetSelection %d %d\n",
		      start, end));

	SetSingleHighlight(aw, start, end);

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    if (!XtIsRealized(w))
    {
	_XmObjectUnlock(aw);
	return;
    }

    /* SG 22/08/1998 surely we need to clean up any previous selection
       as in PrimarySelectionComplete or we get a memory leak.
     */
    if (TextF_SelectionText(w))
    {
	XtFree(TextF_SelectionText(w));
    }

    TextF_SelectionText(w) = NULL;
    TextF_SelectionLen(w) = 0;

    if (start > TextF_Length(w))
    {
    	start = TextF_Length(w);
    }
    if (end > TextF_Length(w))
    {
    	end = TextF_Length(w);
    }

    if (start >= end)
    {				/* No decent selection */
        /* SG 23/08/1998, get straight out if there was no selection anyway */
        if(!w->text.has_primary)
        {
	    _XmObjectUnlock(aw);
            return;
        }

	XtDisownSelection(aw, XA_PRIMARY, time);
	w->text.has_primary = False;
        if(TextF_Echo(w))
        {
	    CursorErase(w);
	    DrawText(w, TextF_HighlightStart(w), TextF_HighlightEnd(w), False);
	    CursorDraw(w);
        }
        /* SG 23/08/1998 parts of this code check start == -1 to see
           if there is no highlight, so setting these to zero was
           not consistent.
         */
	TextF_OldHighlightStart(w) = TextF_HighlightStart(w) = -1;
	TextF_OldHighlightEnd(w) = TextF_HighlightEnd(w) = -1;
	w->text.prim_time = time;
    }
    else
    {
	/* Remember whether we used to have the selection */
	gain = (w->text.has_primary == False);
	/* Xt, callbacks */

	DEBUGOUT(_LtDebug(__FILE__, aw,
			  "XtOwnSelection(_, XA_PRIMARY, ...)\n"));
	
	if (XtOwnSelection(aw, XA_PRIMARY, time,
			    ConvertSelection,
			    LoseSelection, NULL))
	{
	    /* We have a decent selection; indicate in memory */
	    w->text.has_primary = True;
	    TextF_HighlightStart(w) = start;
            /* SG 23/08/1998 this prevents left keyboard selection extend
               from working
	    TextF_CursorPos(w) = end;
             */
	    TextF_HighlightEnd(w) = end;
	    w->text.prim_time = time;
	    len = TextF_HighlightEnd(w) - TextF_HighlightStart(w);
	    if (len > 0)
	    {
		TextF_SelectionLen(w) = len;
		TextF_SelectionText(w) = XtMalloc(len);
		strncpy(TextF_SelectionText(w),
			&TextF_Value(w)[TextF_HighlightStart(w)],
			len);
	    }

            if(TextF_Echo(w))
            {
                CursorErase(w);
	        DrawSingleHighlight(w);
                CursorDraw(w);
            }
	}
	else
	{
	    gain = False;
	}

	
	if (gain)
	{
	    XmAnyCallbackStruct cbs;
	    
	    cbs.reason = XmCR_GAIN_PRIMARY;
	    cbs.event = NULL;	/* Have no information nested this deep */
	    
	    XtCallCallbackList(aw, TextF_GainPrimaryCallback(w),
			       (XtPointer)&cbs);
	}
    }
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldSetString(Widget aw, char *str)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XmTextPosition len = 0;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }
    
    if (str)
    {
	len = strlen(str);
        XmTextFieldSetSelection(aw,-1,-1,CurrentTime);
        ModifyText(w, (XEvent *)0,
                   0, TextF_Length(w),
                   str, len,
                   MassiveChangeDraw, CP_Reset);
    }
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldSetStringWcs(Widget aw, wchar_t *wc_value)
{
    _XmObjectLock(aw);
    /* FIX ME */
    _XmObjectUnlock(aw);
}


extern void
XmTextFieldShowPosition(Widget aw, XmTextPosition position)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return;
    }
    /* FIX ME */
    _XmObjectUnlock(aw);
}


extern XmTextPosition
XmTextFieldXYToPos(Widget aw, Position x, Position y)
{
    XmTextFieldWidget w = (XmTextFieldWidget)aw;
    XmTextPosition r;

    _XmObjectLock(aw);
    if (!XmIsTextField(w))
    {
	_XmObjectUnlock(aw);
	return 0;
    }

    r =  TextPixelToSelectionPos(w, x);
    _XmObjectUnlock(aw);
    return r;
}


static void
_XmTextFieldExportValue(Widget w, int offset, XtArgVal *value)
{
    *value = (XtArgVal)XmTextFieldGetString(w);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmTextFieldExportValue: value '%s'\n",
		      (char *)*value));
}

static XtPointer
_XmTextF_TraitGetValue(Widget w, int format)
{   
	switch (format) {
	case XmFORMAT_XmSTRING:
		return (XtPointer)XmStringCreateSimple(TextF_Value(w));
	case XmFORMAT_MBYTE:    
		return XmTextFieldGetString(w); /* FIX ME */
	case XmFORMAT_WCS:
		return XmTextFieldGetString(w); /* FIX ME */
	default:
		return NULL;
	}
}       


static void
_XmTextF_TraitSetValue(Widget w, XtPointer value, int format)
{
	char    *s;
 
	switch (format) {
	case XmFORMAT_XmSTRING:
	    if (XmStringGetLtoR((XmString)value, XmFONTLIST_DEFAULT_TAG, &s)) { 
		XmTextFieldSetString(w, s);
	    }
	    return;
	case XmFORMAT_MBYTE:
	    /* FIX ME */
	    XmTextFieldSetString(w, (char *)value);
	    return;
	case XmFORMAT_WCS:
	    /* FIX ME */
	    XmTextFieldSetString(w, (char *)value);
	    return;
	default:
	    return;
	}
}


static int
_XmTextF_TraitPreferredFormat(Widget w)
{
	return XmFORMAT_WCS;
}


/* XTextWidth clone for TextFieldWidget - KF */
extern int
_XmTextF_FontTextWidth(Widget w, char *s, int l)
{

    if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_FONTSET) {
	return XmbTextEscapement((XFontSet)(TextF_FontList(w)->renditions[0]->font), s, l);
    }
#ifdef	USE_XFT
    else if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_XFT) {
	XGlyphInfo	ext;

	XftTextExtents8(XtDisplay(w),
		TextF_XftFont(w),
		(unsigned char*)s, l,
		&ext);

	return ext.width;
    }
#endif
    else {
	return XTextWidth(TextF__(w).font, s, l);
    }

}

/* Replaced TextF_FontMaxWidth - KF */
#ifndef USE_AVERAGE_WIDTH
static int
_XmTextF_FontMaxWidth(Widget w)
{

    if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_FONTSET) {
	XFontStruct **foo;
	char **bar;
	int i, num, max;
        
	num = XFontsOfFontSet((XFontSet)TextF_FontList(w)->renditions[0]->font, &foo, &bar);
	max = 0;
	for (i=0; i<num; i++){
	    if (foo[i]->max_bounds.width  > max)
	        max = foo[i]->max_bounds.width;
	}
	return max; 
#ifdef	USE_XFT
    } else if (TextF_FontList(w)->renditions[0]->type == XmFONT_IS_XFT) {
	XGlyphInfo	ext;

	XftTextExtents8(XtDisplay(w),
		TextF_XftFont(w),
		s, l,
		&ext);

	return ext.width;
#endif
    } else {
	return TextF_Font(w)->max_bounds.width;
    }

}
#endif /* ifndef USE_AVERAGE_WIDTH */

extern Boolean
XmTextFieldCopyLink(Widget widget,
                            Time time)
{
   _XmWarning(NULL, "XmTextCopyLink(): not yet implemented!");
   return False;
}

static void
_XmTextFSetRenderTable(Widget w, int o, XrmValue *v)
{
	XmTextFieldWidget	tf = (XmTextFieldWidget)w;
	static const XmFontList	nullfl = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextFSetRenderTable (%d)\n",
		tf->text.check_set_render_table));

	++tf->text.check_set_render_table;
	switch (tf->text.check_set_render_table)
	{
	case 1:
                /*
                 * Either the font list or render table resource has
                 * not been set, but do not know yet if both have not
                 * been set.  For now, preserve the value in case one
                 * of the resources has been set.      
                 */
		v->addr = (char *)&(tf->text.font_list);
		break;

	case 2:
                /*
		 * Neither the font list nor render table resource has
                 * been set.  To avoid relying on the structure having
		 * been zero filled by the Xt library, ensure the
		 * font_list element is set to NULL.
		 */
		v->addr = (char*)&nullfl;
		break;

	default:
                /* This should never happen. */
                v->addr = NULL;
                break;
	}
}
