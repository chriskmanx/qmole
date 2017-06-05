/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/baseclass/test3.c,v 1.3 2001/06/11 08:26:30 amai Exp $
 * class dump to figure out the extensions
 */
#include <stdlib.h>
#include <stdio.h>
#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/LabelP.h>
#include <Xm/PushBP.h>
#include <Xm/TearOffBP.h>
#include <X11/ObjectP.h>
#include <Xm/ExtObjectP.h>

#include "../../common/Test.h"

/*
 * things of note:
 * InitializeRootWrapper is jammed into objectClassRec.initialize, probably
 * during the call to _XmInitializeExtensions (seems the likely place for
 * it, but it could be done during _XmBaseClassPartInitialize on a first
 * time called basis.  On second thought, this won't work -- it has to
 * be done from _XmInitializeExtensions, as there is a class_part_initialize
 * pre- and post- hook.
 *
 * When, for example, a new label is created, the initialize call
 * chain proceeds to InitializeRootWrapper (the object class is the
 * root of all widget classes).  InitializeRootWrapper calls the
 * initialize_prehook, and patches the calling class's initialize
 * method with InitLeafWrapper (if it hasn't already been patched).
 *
 * The call chain unwinds back to the Label class initialize method, which
 * has been replaced with InitLeafWrapper.  InitLeafWrapper then calls
 * the original Label initialize method (which was saved off in the
 * WrapperData for the label Class, and then calls the initializePosthook
 * in the base class extension record.
 *
 * If you want to see this behavior, link in a debug Xt library with
 * test3.motif, run gdb on test3.motif, and set breakpoints in the functions
 * oinit, oinit_hook, linit, linit_prehook, and linit_posthook.  Then
 * run, dump the label class record, continue and dump, etc, etc.  You will
 * see the values change.
 *
 * Talk about some wacky stuff.
 *
 * One other thing you'll notice: xmLabelWidgetClassRec.resize is
 * ResizeWrapper4 (dunno what _that_ means yet).
 *
 * Another: Primitive has ResizeWrapper3.
 *
 * Another: PushButton has ResizeWrapper5.
 *
 * Note2:  I don't know yet where the other root/leaf wrappers are installed.
 * Gotta find that out, still.
 * Next day: Guess what, same place.  objectClass.class_part_initialize is
 * set to ClassPartInitRootWrapper, objectClass.set_values is set to
 * SetValuesRootWrapper, objectClass.get_values_hook is set to
 * GetValuesRootWrapper.
 *
 * Wow.  The *Leaf overrides *aren't* permanent.  Only for the duration of
 * the call for the subclass being manipulated.  Fixes the multiple call
 * problem nicely.
 */
void
dump_wrapper(XmWrapperData rec) {
    printf("WRAPPER DATA:\n");
    printf("Next: %p Class %p\n",
	rec->next, rec->widgetClass);

    if (rec->next) {
	printf("RECURSION::::::::::::::::::::::::::::::\n");
	dump_wrapper(rec->next);
	printf("END RECURSION::::::::::::::::::::::::::::::\n");
    }
    if (rec->widgetClass)
	printf(" name: %s\n", rec->widgetClass->core_class.class_name);
    else
	printf(" name: (null)\n");
    printf("InitLeave: %p SetValLeaf: %p GetValLeaf: %p\n",
	rec->initializeLeaf, rec->setValuesLeaf, rec->getValuesLeaf);
    printf("realize: %p ClassPartInitLeaf: %p resize: %p\n",
	rec->realize, rec->classPartInitLeaf, rec->resize);
    printf("GeomManager: %p\n", rec->geometry_manager);
    printf("END WRAPPER DATA\n\n");
}

void
dump_bce(XmBaseClassExt rec) {
    printf("BASE CLASS EXTENSION:\n");
    printf("InitPreHook: %p SetValPreHook: %p\n",
	rec->initializePrehook, rec->setValuesPrehook);
    printf("InitPostHook: %p SetValPostHook: %p\n",
	rec->initializePosthook, rec->setValuesPosthook);
    printf("SecondaryClass: %p SecondaryCreate: %p\n",
	rec->secondaryObjectClass, rec->secondaryObjectCreate);
    printf("GetSecResData: %p flags: %02x%02x%02x%02x\n",
	rec->getSecResData, rec->flags[0], rec->flags[1], rec->flags[2], rec->flags[3]);
    printf("GetValPreHook: %p GetValPostHook: %p\n",
	rec->getValuesPrehook, rec->getValuesPosthook);
    printf("ClassPartInitPre: %p ClassPartInitPost: %p\n",
	rec->classPartInitPrehook, rec->classPartInitPosthook);
    printf("ExtRes: %p CompiledExtRes: %p NumExtRes %d\n",
	rec->classPartInitPrehook, rec->classPartInitPosthook,
	rec->num_ext_resources);
    printf("UseSubRes: %d WidgetNav: %p FocusChange: %p Wrapper: %p\n",
	rec->use_sub_resources, rec->widgetNavigable, rec->focusChange,
	rec->wrapperData);
    if (rec->wrapperData)
	dump_wrapper((XmWrapperData)rec->wrapperData);
    printf("END BASE CLASS EXTENSION\n\n");
}

void
dump_core(CoreClassPart *class, Boolean uninit) {
    int i;
    XmGenericClassExt ptr;

    printf("CORE CLASS PART:\n");
    printf("Superclass: %p  ClassName: %s WidgetSize: %d\n",
	class->superclass, class->class_name, class->widget_size);
    printf("ClassInit: %p ClassPartInit %p ClassInited: %d\n",
	class->class_initialize, class->class_part_initialize,
	class->class_inited);
    printf("Init: %p InitHook: %p Realize: %p\n",
	class->initialize, class->initialize_hook, class->realize);
    printf("Actions:\n");
    for (i = 0; i < class->num_actions; i++) {
	if (uninit) {
		printf("  [%d]: %s : %p\n",
			i, class->actions[i].string, class->actions[i].proc);
	}
    }
    printf("CompressMot: %d CompressExpose: %d CompressEnterLeave: %d\n",
	class->compress_motion, class->compress_exposure,
	class->compress_enterleave);
    printf("VisibuleInterest: %d Destroy: %p Resize: %p\n",
	class->visible_interest, class->destroy, class->resize);
    printf("Expose: %p SetValues: %p SetValHook: %p\n",
	class->expose, class->set_values, class->set_values_hook);
    printf("SetValAlmost: %p GetValues: %p AcceptFocus: %p\n",
	class->set_values_almost, class->get_values_hook, class->accept_focus);
    if (class->tm_table) {
	    printf("Version: %ld CallbackPrivate: %p Translations:\n%s\n",
		class->version, class->callback_private, class->tm_table);
    }
    else {
	    printf("Version: %ld CallbackPrivate: %p Translations:\nNULL\n",
		class->version, class->callback_private);
    }
    printf("QueryGeom: %p DisplayAcc: %p Extension: %p\n",
	class->query_geometry, class->display_accelerator, class->extension);

    for (ptr = (XmGenericClassExt)class->extension;
	 ptr != NULL;
	 ptr = ptr->next_extension) {
	printf("record_type: %d version: %ld record_size: %d\n",
		ptr->record_type, ptr->version, ptr->record_size);
	if (ptr->record_type == XmQmotif)
		dump_bce((XmBaseClassExt)ptr);
    }
    printf("END CORE CLASS PART\n\n");
}

void
dump_primitive(XmPrimitiveClassPart *pwc) {
   XmPrimitiveClassExt ptr;

   printf("PRIMITIVE CLASS PART:\n");
   printf("BorderHighlight: %p BorderUnhighlight: %p\n",
	pwc->border_highlight, pwc->border_unhighlight);
   if (pwc->translations)
	printf("Translations: %s\n", pwc->translations);
   printf("ArmAndAct: %p Extension: %p\n",
	pwc->arm_and_activate, pwc->extension);

   for (ptr = (XmPrimitiveClassExt)pwc->extension;
	ptr != NULL;
	ptr = ptr->next_extension) {
	printf("record_type: %d version: %ld record_size: %d\n",
		ptr->record_type, ptr->version, ptr->record_size);
	printf("widget_baseline: %p widget_display_rect: %p\n",
		ptr->widget_baseline, ptr->widget_display_rect);
	printf("widget_margins: %p\n",
		ptr->widget_margins);
   }
    printf("END PRIMITIVE CLASS PART\n\n");
}

static void (*olpre)(Widget r, Widget n, ArgList args, Cardinal *num_args);
static void (*olpost)(Widget r, Widget n, ArgList args, Cardinal *num_args);
static void (*oli)(Widget r, Widget n, ArgList args, Cardinal *num_args);
static void (*ooi)(Widget r, Widget n, ArgList args, Cardinal *num_args);
static void (*oih)(Widget n, ArgList args, Cardinal *num_args);
static void (*ocpi)(WidgetClass wc);

void
linit_prehook(Widget r, Widget n, ArgList args, Cardinal *num_args)
{
    printf("PREHOOK\n");
    dump_core(&xmTearOffButtonClassRec.core_class, False);
    if (olpre)
	(*olpre)(r, n, args, num_args);
    dump_core(&xmTearOffButtonClassRec.core_class, False);
}

void
linit_posthook(Widget r, Widget n, ArgList args, Cardinal *num_args)
{
    printf("POSTHOOK\n");
    dump_core(&xmPushButtonClassRec.core_class, False);
    if (olpost)
	(*olpost)(r, n, args, num_args);
    dump_core(&xmPushButtonClassRec.core_class, False);
}

void
linit(Widget r, Widget n, ArgList args, Cardinal *num_args)
{
    printf("INITIALIZE\n");
    dump_core(&xmPushButtonClassRec.core_class, False);
    if (oli)
	(*oli)(r, n, args, num_args);
    dump_core(&xmPushButtonClassRec.core_class, False);
}

void
oinit(Widget r, Widget n, ArgList args, Cardinal *num_args)
{
    printf("OBJECT INITIALIZE\n");
    dump_core(&xmPushButtonClassRec.core_class, False);
    if (!ooi) {
	fprintf(stderr, "NO OBJ INIT METHOD!\n");
	abort();
    }
    (*ooi)(r, n, args, num_args);
    dump_core(&xmPushButtonClassRec.core_class, False);
}

void
oinit_hook(Widget n, ArgList args, Cardinal *num_args)
{
    printf("INITIALIZE HOOK\n");
    dump_core(&xmPushButtonClassRec.core_class, False);
    if (!oih) {
	fprintf(stderr, "NO OBJ INIT METHOD!\n");
    }
    else
	(*oih)(n, args, num_args);
    dump_core(&xmPushButtonClassRec.core_class, False);
}

void
ocpinit(WidgetClass wc)
{
    printf("CLASS PART INITIALIZE ROOT\n");
    (*ocpi)(wc);
    dump_core(&xmPushButtonClassRec.core_class, False);
}

int
main(int argc, char **argv) {
    Widget toplevel, one;
    XtAppContext app;

    printf("FIRST:\n");
    dump_core(&xmPrimitiveClassRec.core_class, True);
    dump_primitive(&xmPrimitiveClassRec.primitive_class);
    dump_core(&xmPushButtonClassRec.core_class, True);
    dump_primitive(&xmPushButtonClassRec.primitive_class);

    toplevel = XtAppInitialize(&app, argv[0], NULL, 0, &argc, argv,
				 NULL, NULL, 0);

    printf("SECOND:\n");
    dump_core(&xmPrimitiveClassRec.core_class, True);
    dump_primitive(&xmPrimitiveClassRec.primitive_class);
    dump_core(&xmPushButtonClassRec.core_class, True);
    dump_primitive(&xmPushButtonClassRec.primitive_class);

    olpre = ((XmBaseClassExt)(xmPushButtonClassRec.core_class.extension))->initializePrehook;
    ((XmBaseClassExt)(xmPushButtonClassRec.core_class.extension))->initializePrehook
	 = linit_prehook;
    
    olpost = ((XmBaseClassExt)(xmPushButtonClassRec.core_class.extension))->initializePosthook;
    ((XmBaseClassExt)(xmPushButtonClassRec.core_class.extension))->initializePosthook
	 = linit_posthook;

#if 0
    oli = xmPushButtonClassRec.core_class.initialize;
    xmPushButtonClassRec.core_class.initialize = linit;

    ooi = objectClassRec.object_class.initialize;
    objectClassRec.object_class.initialize = oinit;

    oih = objectClassRec.object_class.initialize_hook;
    objectClassRec.object_class.initialize_hook = oinit_hook;

    ocpi = objectClassRec.object_class.class_part_initialize;
    objectClassRec.object_class.class_part_initialize = ocpinit;

    fprintf(stderr, "OBJEXT: %08x\n", objectClassRec.object_class.extension);
#endif
    
    one = XtCreateWidget("two", xmPushButtonWidgetClass, toplevel, NULL, 0);

    printf("THIRD:\n");
    dump_core(&xmPushButtonClassRec.core_class, False);
    dump_primitive(&xmPrimitiveClassRec.primitive_class);
    dump_core(&xmPushButtonClassRec.core_class, False);
    dump_primitive(&xmPushButtonClassRec.primitive_class);

    XtManageChild(one);

    printf("FOURTH:\n");
    dump_core(&xmPrimitiveClassRec.core_class, False);
    dump_primitive(&xmPrimitiveClassRec.primitive_class);
    dump_core(&xmPushButtonClassRec.core_class, False);
    dump_primitive(&xmPushButtonClassRec.primitive_class);

    XtRealizeWidget(toplevel);

    printf("FIFTH:\n");
    dump_core(&xmPrimitiveClassRec.core_class, False);
    dump_primitive(&xmPrimitiveClassRec.primitive_class);
    dump_core(&xmPushButtonClassRec.core_class, False);
    dump_primitive(&xmPushButtonClassRec.primitive_class);

    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,   30,   25, 0,0,0, /* two */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );
    exit(0);
}


