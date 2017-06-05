/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test56.c,v 1.15 2001/05/16 09:19:46 amai Exp $
   simulate an openDX interactor on the control panel switching
   from vertical to horizontal then back.
 */
 
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/FormP.h>
#include <Xm/Label.h>

#include "../../common/Test.h"

#define IGNORE_TEMPVALUE

static char *FallBack[] =
{
    "*.borderWidth: 1",
    NULL
};

extern int GlobalErrors;

/* Fake Form sub-class */
/* Fake FormP.h */
typedef enum {FormConstraint, 
	      ConstraintInitialize, 
	      ConstraintSetValues, 
	      GeometryManager, 
	      ChangeManaged, 
	      QueryGeometry,
	      Geometry
} ExpectedType;

typedef struct {
    ExpectedType type;
    String name;
    Position x, y;
    Dimension width, height;

    unsigned char left_type;
    String left_widget;
    int left_percent;
    int left_offset;
    int left_value;
    int left_tempValue;

    unsigned char right_type;
    String right_widget;
    int right_percent;
    int right_offset;
    int right_value;
    int right_tempValue;

    unsigned char top_type;
    String top_widget;
    int top_percent;
    int top_offset;
    int top_value;
    int top_tempValue;

    unsigned char bottom_type;
    String bottom_widget;
    int bottom_percent;
    int bottom_offset;
    int bottom_value;
    int bottom_tempValue;

    String next_sibling;
    Boolean sorted;
    Boolean resizable;
    Dimension preferred_width;
    Dimension preferred_height;

    Dimension form_width;
    Dimension form_height;

    unsigned long request_mode;
    Position geox, geoy;
    Dimension geowidth, geoheight, geoborder_width;
} ExpectedResults;

typedef struct {
    ExpectedResults *expected;
    int expected_count;
    int expected_index;
} FakeFormPart;

typedef struct _FakeFormRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    XmManagerPart manager;
    XmBulletinBoardPart bulletin_board;
    XmFormPart form;
    FakeFormPart fake_form;
} FakeFormRec;

typedef struct {
    XtPointer extension;
} FakeFormClassPart;

typedef struct _FakeFormClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart manager_class;
    XmBulletinBoardClassPart bulletin_board_class;
    XmFormClassPart form_class;
    FakeFormClassPart fake_form_class;
} FakeFormClassRec;

extern FakeFormClassRec fakeFormClassRec;
/* Fake FormP.h */
/* Fake Form.h */
#ifndef expectedItems
#define expectedItems "expectedItems"
#endif
#ifndef ExpectedItems
#define ExpectedItems "ExpectedItems"
#endif
#ifndef expectedCount
#define expectedCount "expectedCount"
#endif
#ifndef ExpectedCount
#define ExpectedCount "ExpectedCount"
#endif
extern WidgetClass fakeFormWidgetClass;
typedef struct _FakeFormRec *FakeFormWidget;
typedef struct _FakeFormClassRec *FakeFormWidgetClass;
/* Fake Form.h */
/* Fake Form.c */


static String
AttachmentType2String(unsigned char type)
{
    switch(type)
    {
    case XmATTACH_NONE:
    	return("XmATTACH_NONE");
    	break;
    case XmATTACH_FORM:
    	return("XmATTACH_FORM");
    	break;
    case XmATTACH_OPPOSITE_FORM:
    	return("XmATTACH_OPPOSITE_FORM");
    	break;
    case XmATTACH_WIDGET:
    	return("XmATTACH_WIDGET");
    	break;
    case XmATTACH_OPPOSITE_WIDGET:
    	return("XmATTACH_OPPOSITE_WIDGET");
    	break;
    case XmATTACH_POSITION:
    	return("XmATTACH_POSITION");
    	break;
    case XmATTACH_SELF:
    	return("XmATTACH_SELF");
    	break;
    default:
    	return("UNKNOWN");
    	break;
    }
}

static String
Type2String(ExpectedType type)
{
    switch(type)
    {
    case FormConstraint:
    	return("FormConstraint");
    	break;
    case ConstraintInitialize:
    	return("ConstraintInitialize");
    	break;
    case ConstraintSetValues:
    	return("ConstraintSetValues");
    	break;
    case GeometryManager:
    	return("GeometryManager");
    	break;
    case ChangeManaged:
    	return("ChangeManaged");
    	break;
    case QueryGeometry:
    	return("QueryGeometry");
    	break;
    case Geometry:
    	return("Geometry");
    	break;
    default:
    	return("UNKNOWN");
    	break;
    }
}

static void
PrintGeometry(Widget w, XtWidgetGeometry *geo)
{
FakeFormWidget Form = (FakeFormWidget)(w);

    if (Form->fake_form.expected_index < Form->fake_form.expected_count)
    {
	printf("%s(%s) ", Type2String(Geometry), Type2String(Form->fake_form.expected[Form->fake_form.expected_index].type));
	if (Form->fake_form.expected[Form->fake_form.expected_index].type != Geometry)
	{
	    printf("Bad\n");
	    GlobalErrors++;
	}
	else
	{
	XtWidgetGeometry tmp;

	    printf("%s(", XdbWidgetGeometry2String(geo));
    tmp.request_mode = Form->fake_form.expected[Form->fake_form.expected_index].request_mode;
    tmp.x = Form->fake_form.expected[Form->fake_form.expected_index].geox;
    tmp.y = Form->fake_form.expected[Form->fake_form.expected_index].geoy;
    tmp.width = Form->fake_form.expected[Form->fake_form.expected_index].geowidth;
    tmp.height = Form->fake_form.expected[Form->fake_form.expected_index].geoheight;
	    printf("%s", XdbWidgetGeometry2String(&tmp));
	    printf(") ");
	    if (geo->request_mode == Form->fake_form.expected[Form->fake_form.expected_index].request_mode &&
	        (!(geo->request_mode & CWX) || (geo->request_mode & CWX && geo->x == Form->fake_form.expected[Form->fake_form.expected_index].geox)) &&
	        (!(geo->request_mode & CWY) || (geo->request_mode & CWY && geo->y == Form->fake_form.expected[Form->fake_form.expected_index].geoy)) &&
	        (!(geo->request_mode & CWWidth) || (geo->request_mode & CWWidth && geo->width == Form->fake_form.expected[Form->fake_form.expected_index].geowidth)) &&
	        (!(geo->request_mode & CWHeight) || (geo->request_mode & CWHeight && geo->height == Form->fake_form.expected[Form->fake_form.expected_index].geoheight)) &&
	        (!(geo->request_mode & CWBorderWidth) || (geo->request_mode & CWBorderWidth && geo->border_width == Form->fake_form.expected[Form->fake_form.expected_index].geoborder_width))
	        )
	    {
		printf("Okay\n");
	    }
	    else
	    {
		printf("Bad\n");
		GlobalErrors++;
	    }
	}
	Form->fake_form.expected_index++;
    }
    else
    {
	printf("{Geometry, \"%s\", %i, %i, %i, %i, ", XtName(w), XtX(w), XtY(w), XtWidth(w), XtHeight(w));
	printf("0, \"\", 0,0,0,0,");
	printf("0, \"\", 0,0,0,0,");
	printf("0, \"\", 0,0,0,0,");
	printf("0, \"\", 0,0,0,0,");

	printf("\"\", False, False, 0, 0, %d, %d, ", XtWidth(w), XtHeight(w));
	printf("0 | ");
	printf("%s", geo->request_mode & CWX ? "CWX | " : "");
	printf("%s", geo->request_mode & CWY ? "CWY | " : "");
	printf("%s", geo->request_mode & CWWidth ? "CWWidth | " : "");
	printf("%s", geo->request_mode & CWHeight ? "CWHeight | " : "");
	printf("%s", geo->request_mode & CWBorderWidth ? "CWBorderWidth | " : "");
	printf("%s", geo->request_mode & CWSibling ? "CWSibling | " : "");
	printf("%s", geo->request_mode & CWStackMode ? "CWStackMode | " : "");
	printf("0,");
	printf(" %d, %d,", geo->request_mode & CWX ? geo->x : 0, geo->request_mode & CWY ? geo->y : 0);
	printf(" %d,", geo->request_mode & CWWidth ? geo->width : 0);
	printf(" %d,", geo->request_mode & CWHeight ? geo->height : 0);
	printf(" %d", geo->request_mode & CWBorderWidth ? geo->border_width : 0);
	printf("},\n");
    }
}

static void
PrintConstraints(Widget w)
{
int i;
XmFormConstraint constraints = &(((XmFormConstraintPtr)(w->core.constraints))->form);
FakeFormWidget Form = (FakeFormWidget)XtParent(w);

    if (Form->fake_form.expected_index < Form->fake_form.expected_count)
    {
	printf("%s(%s)", Type2String(FormConstraint), Type2String(Form->fake_form.expected[Form->fake_form.expected_index].type));
	if (Form->fake_form.expected[Form->fake_form.expected_index].type != FormConstraint)
	{
	    printf("Bad\n");
	    GlobalErrors++;
	}
	else
	{
	    printf(" %s(%s) %ix%i%+i%+i(%ix%i%+i%+i) ", 
	    	XtName(w), Form->fake_form.expected[Form->fake_form.expected_index].name, 
	    	XtWidth(w),
	    	XtHeight(w),
	    	XtX(w),
	    	XtY(w),
	    	Form->fake_form.expected[Form->fake_form.expected_index].width, 
	    	Form->fake_form.expected[Form->fake_form.expected_index].height, 
	    	Form->fake_form.expected[Form->fake_form.expected_index].x, 
	    	Form->fake_form.expected[Form->fake_form.expected_index].y
	    	);
	    if (strcmp(Form->fake_form.expected[Form->fake_form.expected_index].name, XtName(w)) != 0 ||
		Form->fake_form.expected[Form->fake_form.expected_index].width != XtWidth(w) ||
	        Form->fake_form.expected[Form->fake_form.expected_index].height != XtHeight(w) ||
	        Form->fake_form.expected[Form->fake_form.expected_index].x != XtX(w) ||
		Form->fake_form.expected[Form->fake_form.expected_index].y != XtY(w))
	    {
		printf("Bad\n");
		GlobalErrors++;
	    }
	    else
	    {
		printf("Okay\n");
	    }
	    printf("\tLeft   %s(%s) \"%s\"(\"%s\") %i(%i) %i(%i) %i(%i) %i(%i) ", 
	    	AttachmentType2String(constraints->att[0].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].left_type),
	    	constraints->att[0].w ?  XtName(constraints->att[0].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].left_widget,
	    	constraints->att[0].percent, Form->fake_form.expected[Form->fake_form.expected_index].left_percent,
	    	constraints->att[0].offset, Form->fake_form.expected[Form->fake_form.expected_index].left_offset,
	    	constraints->att[0].value, Form->fake_form.expected[Form->fake_form.expected_index].left_value,
	    	constraints->att[0].tempValue, Form->fake_form.expected[Form->fake_form.expected_index].left_tempValue
	    	);
	    if (
	    	strcmp(AttachmentType2String(constraints->att[0].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].left_type)) != 0 ||
	    	strcmp(constraints->att[0].w ?  XtName(constraints->att[0].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].left_widget) != 0 ||
	    	constraints->att[0].percent != Form->fake_form.expected[Form->fake_form.expected_index].left_percent ||
	    	constraints->att[0].offset != Form->fake_form.expected[Form->fake_form.expected_index].left_offset ||
	    	constraints->att[0].value != Form->fake_form.expected[Form->fake_form.expected_index].left_value
#ifndef IGNORE_TEMPVALUE
	    	|| constraints->att[0].tempValue != Form->fake_form.expected[Form->fake_form.expected_index].left_tempValue
#endif
	    	)
	    {
		printf("Bad\n");
		GlobalErrors++;
	    }
	    else
	    {
		printf("Okay\n");
	    }

	    printf("\tRight  %s(%s) \"%s\"(\"%s\") %i(%i) %i(%i) %i(%i) %i(%i) ", 
	    	AttachmentType2String(constraints->att[1].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].right_type),
	    	constraints->att[1].w ?  XtName(constraints->att[1].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].right_widget,
	    	constraints->att[1].percent, Form->fake_form.expected[Form->fake_form.expected_index].right_percent,
	    	constraints->att[1].offset, Form->fake_form.expected[Form->fake_form.expected_index].right_offset,
#ifdef LESSTIF_VERSION
	    	constraints->att[1].value + constraints->att[0].value, Form->fake_form.expected[Form->fake_form.expected_index].right_value,
#else
	    	constraints->att[1].value, Form->fake_form.expected[Form->fake_form.expected_index].right_value,
#endif
	    	constraints->att[1].tempValue, Form->fake_form.expected[Form->fake_form.expected_index].right_tempValue
	    	);
	    if (
	    	strcmp(AttachmentType2String(constraints->att[1].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].right_type)) != 0 ||
	    	strcmp(constraints->att[1].w ?  XtName(constraints->att[1].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].right_widget) != 0 ||
	    	constraints->att[1].percent != Form->fake_form.expected[Form->fake_form.expected_index].right_percent ||
	    	constraints->att[1].offset != Form->fake_form.expected[Form->fake_form.expected_index].right_offset ||
#ifdef LESSTIF_VERSION
	    	constraints->att[1].value + constraints->att[0].value != Form->fake_form.expected[Form->fake_form.expected_index].right_value
#else
	    	constraints->att[1].value != Form->fake_form.expected[Form->fake_form.expected_index].right_value
#endif
#ifndef IGNORE_TEMPVALUE
	    	|| constraints->att[1].tempValue != Form->fake_form.expected[Form->fake_form.expected_index].right_tempValue
#endif
	    	)
	    {
		printf("Bad\n");
		GlobalErrors++;
	    }
	    else
	    {
		printf("Okay\n");
	    }

	    printf("\tTop    %s(%s) \"%s\"(\"%s\") %i(%i) %i(%i) %i(%i) %i(%i) ", 
	    	AttachmentType2String(constraints->att[2].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].top_type),
	    	constraints->att[2].w ?  XtName(constraints->att[2].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].top_widget,
	    	constraints->att[2].percent, Form->fake_form.expected[Form->fake_form.expected_index].top_percent,
	    	constraints->att[2].offset, Form->fake_form.expected[Form->fake_form.expected_index].top_offset,
	    	constraints->att[2].value, Form->fake_form.expected[Form->fake_form.expected_index].top_value,
	    	constraints->att[2].tempValue, Form->fake_form.expected[Form->fake_form.expected_index].top_tempValue
	    	);
	    if (
	    	strcmp(AttachmentType2String(constraints->att[2].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].top_type)) != 0 ||
	    	strcmp(constraints->att[2].w ?  XtName(constraints->att[2].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].top_widget) != 0 ||
	    	constraints->att[2].percent != Form->fake_form.expected[Form->fake_form.expected_index].top_percent ||
	    	constraints->att[2].offset != Form->fake_form.expected[Form->fake_form.expected_index].top_offset ||
	    	constraints->att[2].value != Form->fake_form.expected[Form->fake_form.expected_index].top_value
#ifndef IGNORE_TEMPVALUE
	    	|| constraints->att[2].tempValue != Form->fake_form.expected[Form->fake_form.expected_index].top_tempValue
#endif
	    	)
	    {
		printf("Bad\n");
		GlobalErrors++;
	    }
	    else
	    {
		printf("Okay\n");
	    }

	    printf("\tBottom %s(%s) \"%s\"(\"%s\") %i(%i) %i(%i) %i(%i) %i(%i) ", 
	    	AttachmentType2String(constraints->att[3].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].bottom_type),
	    	constraints->att[3].w ?  XtName(constraints->att[3].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].bottom_widget,
	    	constraints->att[3].percent, Form->fake_form.expected[Form->fake_form.expected_index].bottom_percent,
	    	constraints->att[3].offset, Form->fake_form.expected[Form->fake_form.expected_index].bottom_offset,
#ifdef LESSTIF_VERSION
	    	constraints->att[3].value + constraints->att[2].value , Form->fake_form.expected[Form->fake_form.expected_index].bottom_value,
#else
	    	constraints->att[3].value, Form->fake_form.expected[Form->fake_form.expected_index].bottom_value,
#endif
	    	constraints->att[3].tempValue, Form->fake_form.expected[Form->fake_form.expected_index].bottom_tempValue
	    	);
	    if (strcmp(AttachmentType2String(constraints->att[3].type), AttachmentType2String(Form->fake_form.expected[Form->fake_form.expected_index].bottom_type)) != 0 ||
	    	strcmp(constraints->att[3].w ?  XtName(constraints->att[3].w) : "" , Form->fake_form.expected[Form->fake_form.expected_index].bottom_widget) != 0 ||
	    	constraints->att[3].percent != Form->fake_form.expected[Form->fake_form.expected_index].bottom_percent ||
	    	constraints->att[3].offset != Form->fake_form.expected[Form->fake_form.expected_index].bottom_offset ||
#ifdef LESSTIF_VERSION
	    	constraints->att[3].value + constraints->att[2].value != Form->fake_form.expected[Form->fake_form.expected_index].bottom_value
#else
	    	constraints->att[3].value != Form->fake_form.expected[Form->fake_form.expected_index].bottom_value
#endif
#ifndef IGNORE_TEMPVALUE
	    	|| constraints->att[3].tempValue != Form->fake_form.expected[Form->fake_form.expected_index].bottom_tempValue
#endif
	    	)
	    {
		printf("Bad\n");
		GlobalErrors++;
	    }
	    else
	    {
		printf("Okay\n");
	    }
	    printf("\t\"%s\"(\"%s\") %s(%s) %s(%s) %ix%i(%ix%i) %ix%i(%ix%i) ", 
	    	constraints->next_sibling ? XtName(constraints->next_sibling) : "", Form->fake_form.expected[Form->fake_form.expected_index].next_sibling,
	    	constraints->sorted ? "True" : "False", Form->fake_form.expected[Form->fake_form.expected_index].sorted ? "True" : "False",
	    	constraints->resizable ? "True" : "False", Form->fake_form.expected[Form->fake_form.expected_index].resizable ? "True" : "False",
	    	constraints->preferred_width, constraints->preferred_height, Form->fake_form.expected[Form->fake_form.expected_index].preferred_width, Form->fake_form.expected[Form->fake_form.expected_index].preferred_height,
	    	XtWidth(Form), XtHeight(Form), Form->fake_form.expected[Form->fake_form.expected_index].form_width, Form->fake_form.expected[Form->fake_form.expected_index].form_height
	    	);
	    if (
	    	/* constraints->next_sibling != Form->fake_form.expected[Form->fake_form.expected_index].next_sibling || */
	    	/*constraints->sorted != Form->fake_form.expected[Form->fake_form.expected_index].sorted ||*/
	    	constraints->resizable != Form->fake_form.expected[Form->fake_form.expected_index].resizable ||
	    	constraints->preferred_width != Form->fake_form.expected[Form->fake_form.expected_index].preferred_width ||
	    	constraints->preferred_height != Form->fake_form.expected[Form->fake_form.expected_index].preferred_height ||
	    	XtWidth(Form) != Form->fake_form.expected[Form->fake_form.expected_index].form_width ||
	    	XtHeight(Form) != Form->fake_form.expected[Form->fake_form.expected_index].form_height)
	    {
		printf("Bad\n");
		GlobalErrors++;
	    }
	    else
	    {
		printf("Okay\n");
	    }
	}
	Form->fake_form.expected_index++;
    }
    else
    {
	printf("{FormConstraint, \"%s\", %i, %i, %i, %i, ", XtName(w), XtX(w), XtY(w), XtWidth(w), XtHeight(w));
	for (i = 0; i < 4; i++)
	{
	    printf("%s, \"%s\", %i, %i, %i, %i, ", 
		AttachmentType2String(constraints->att[i].type),
		constraints->att[i].w ?  XtName(constraints->att[i].w) : "",
		constraints->att[i].percent,
		constraints->att[i].offset,
		constraints->att[i].value,
		constraints->att[i].tempValue
		);
	}
	printf("\"%s\", %s, %s, %i, %i, %i, %i",
	    constraints->next_sibling ? XtName(constraints->next_sibling) : "",
	    constraints->sorted ? "True" : "False",
	    constraints->resizable ? "True" : "False",
	    constraints->preferred_width,
	    constraints->preferred_height,
	    XtWidth(Form), XtHeight(Form)
	    );
	printf("},\n");
    }
}

static void
constraint_initialize(Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
FakeFormWidget Form = (FakeFormWidget)XtParent(new_w);

    printf("/* constraint_initialize - begin %s %s */\n", XtName(XtParent(new_w)), XtName(new_w));
    if (Form->fake_form.expected_index < Form->fake_form.expected_count)
    {
	if (Form->fake_form.expected[Form->fake_form.expected_index].type != ConstraintInitialize)
	{
	    printf("Wrong method (%s)\n", Type2String(Form->fake_form.expected[Form->fake_form.expected_index].type));
	    GlobalErrors++;
	}
	else
	{
	    printf("Right method\n");
	}
	Form->fake_form.expected_index++;
    }
    else
    {
	printf("{ConstraintInitialize},\n");
    }
    /*
       This is a chained method. Therefore the Form's method has already been called
    (((ConstraintWidgetClass)XtClass(XtParent(new_w))->core_class.superclass)->constraint_class.initialize)(request, new_w, args, num_args);
    */
    printf("/* request */\n");
    PrintConstraints(request);
    printf("/* new_w */\n");
    PrintConstraints(new_w);
    printf("/* constraint_initialize - end   %s %s */\n", XtName(XtParent(new_w)), XtName(new_w));
    return;
}

static Boolean
constraint_set_values(Widget current, Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
FakeFormWidget Form = (FakeFormWidget)XtParent(new_w);
Boolean result = False;

    printf("/* constraint_set_values - begin %s %s */\n", XtName(XtParent(new_w)), XtName(new_w));
    if (Form->fake_form.expected_index < Form->fake_form.expected_count)
    {
	if (Form->fake_form.expected[Form->fake_form.expected_index].type != ConstraintSetValues)
	{
	    printf("Wrong method (%s)\n", Type2String(Form->fake_form.expected[Form->fake_form.expected_index].type));
	    GlobalErrors++;
	}
	else
	{
	    printf("Right method\n");
	}
	Form->fake_form.expected_index++;
    }
    else
    {
	printf("{ConstraintSetValues},\n");
    }
    /*
       This is a chained method. Therefore the Form's method has already been called
    PrintConstraints(current);
    PrintConstraints(request);
    PrintConstraints(new_w);
    result = (((ConstraintWidgetClass)XtClass(XtParent(new_w))->core_class.superclass)->constraint_class.set_values)(current, request, new_w, args, num_args);
    printf("\n");
    */
    PrintConstraints(current);
    PrintConstraints(request);
    PrintConstraints(new_w);
    printf("/* constraint_set_values - end   %s %s */\n", XtName(XtParent(new_w)), XtName(new_w));
    return(result);
}

static void
change_managed(Widget w)
{
int i;
FakeFormWidget Form = (FakeFormWidget)(w);

    printf("/* change_managed - begin %s */\n", XtName(w));
    if (Form->fake_form.expected_index < Form->fake_form.expected_count)
    {
	if (Form->fake_form.expected[Form->fake_form.expected_index].type != ChangeManaged)
	{
	    printf("Wrong method (%s)\n", Type2String(Form->fake_form.expected[Form->fake_form.expected_index].type));
	    GlobalErrors++;
	}
	else
	{
	    printf("Right method\n");
	}
	Form->fake_form.expected_index++;
    }
    else
    {
	printf("{ChangeManaged},\n");
    }
    for (i = 0; i < ((CompositeWidget)w)->composite.num_children; i++)
    {
	PrintConstraints(((CompositeWidget)w)->composite.children[i]);
    }
    (((CompositeWidgetClass)XtClass(w)->core_class.superclass)->composite_class.change_managed)(w);
    printf("\n");
    for (i = 0; i < ((CompositeWidget)w)->composite.num_children; i++)
    {
	PrintConstraints(((CompositeWidget)w)->composite.children[i]);
    }
    printf("/* change_managed - end   %s */\n", XtName(w));
    return;
}

static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
XtGeometryResult result;
int i;
FakeFormWidget Form = (FakeFormWidget)XtParent(w);

    printf("/* geometry_manager - begin %s %s */\n", XtName(XtParent(w)), XtName(w));
    if (Form->fake_form.expected_index < Form->fake_form.expected_count)
    {
	if (Form->fake_form.expected[Form->fake_form.expected_index].type != GeometryManager)
	{
	    printf("Wrong method (%s)\n", Type2String(Form->fake_form.expected[Form->fake_form.expected_index].type));
	    GlobalErrors++;
	}
	else
	{
	    printf("Right method\n");
	}
	Form->fake_form.expected_index++;
    }
    else
    {
	printf("{GeometryManager},\n");
    }
    printf("/* request (%s) reply (%s) */\n", XdbWidgetGeometry2String(request), XdbWidgetGeometry2String(reply));
    for (i = 0; i < ((CompositeWidget)XtParent(w))->composite.num_children; i++)
    {
	PrintConstraints(((CompositeWidget)XtParent(w))->composite.children[i]);
    }
    result = (((CompositeWidgetClass)XtClass(XtParent(w))->core_class.superclass)->composite_class.geometry_manager)(w, request, reply);
    for (i = 0; i < ((CompositeWidget)XtParent(w))->composite.num_children; i++)
    {
	PrintConstraints(((CompositeWidget)XtParent(w))->composite.children[i]);
    }
    printf("/* request (%s) reply (%s) answer %s */\n", XdbWidgetGeometry2String(request), XdbWidgetGeometry2String(reply), XdbGeometryResult2String(result));
    printf("/* geometry_manager - end   %s %s */\n", XtName(XtParent(w)), XtName(w));
    return(result);
}

static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *desired)
{
XtGeometryResult result;
int i;
FakeFormWidget Form = (FakeFormWidget)(w);

    printf("/* query_geometry - begin %s */\n", XtName(w));
    if (Form->fake_form.expected_index < Form->fake_form.expected_count)
    {
	if (Form->fake_form.expected[Form->fake_form.expected_index].type != QueryGeometry)
	{
	    printf("Wrong method (%s)\n", Type2String(Form->fake_form.expected[Form->fake_form.expected_index].type));
	    GlobalErrors++;
	}
	else
	{
	    printf("Right method\n");
	}
	Form->fake_form.expected_index++;
    }
    else
    {
	printf("{QueryGeometry},\n");
    }
    printf("/* proposed (%s) desired (%s) */\n", XdbWidgetGeometry2String(proposed), XdbWidgetGeometry2String(desired));
    PrintGeometry(w, proposed);
    PrintGeometry(w, desired);
    for (i = 0; i < ((CompositeWidget)(w))->composite.num_children; i++)
    {
	PrintConstraints(((CompositeWidget)(w))->composite.children[i]);
    }
    result = (((CompositeWidgetClass)XtClass(w)->core_class.superclass)->core_class.query_geometry)(w, proposed, desired);
    for (i = 0; i < ((CompositeWidget)(w))->composite.num_children; i++)
    {
	PrintConstraints(((CompositeWidget)(w))->composite.children[i]);
    }
    PrintGeometry(w, proposed);
    PrintGeometry(w, desired);
    printf("/* proposed (%s) desired (%s) result (%s) */\n", XdbWidgetGeometry2String(proposed), XdbWidgetGeometry2String(desired), XdbGeometryResult2String(result));
    printf("/* query_geometry - end   %s */\n", XtName(w));
    return(result);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    ((FakeFormWidget)new_w)->fake_form.expected_index = 0;
    (((CompositeWidgetClass)XtClass(new_w)->core_class.superclass)->core_class.initialize)(request, new_w, args, num_args);
}

#define Offset(field) XtOffsetOf(FakeFormRec, fake_form.field)
static XtResource resources[] =
{
    {
	expectedItems, ExpectedItems, XtRPointer,
	sizeof(XtPointer), Offset(expected),
	XmRImmediate, NULL
    },
    {
	expectedCount, ExpectedCount, XmRInt,
	sizeof(int), Offset(expected_count),
	XmRImmediate, (XtPointer)0
    },
};
#undef Offset

/* *INDENT-OFF* */
FakeFormClassRec fakeFormClassRec = {
    /* Core Class Part */
    {
    /* superclass            */ (WidgetClass) & xmFormClassRec,
    /* class_name            */ "FakeForm",
    /* widget_size           */ sizeof(FakeFormRec),
    /* class_initialize      */ NULL,
    /* class_part_initialize */ NULL,
    /* class_inited          */ False,
    /* initialize            */ initialize,
    /* initialize_hook       */ NULL,
    /* realize               */ XtInheritRealize,
    /* actions               */ NULL,
    /* num_actions           */ 0,
    /* resources             */ resources,
    /* num_resources         */ XtNumber(resources),
    /* xrm_class             */ NULLQUARK,
    /* compress_motion       */ True,
    /* compress_exposure     */ XtExposeCompressMultiple,
    /* compress_enterleave   */ True,
    /* visible_interest      */ True,
    /* destroy               */ NULL,
    /* resize                */ XtInheritResize,
    /* expose                */ XtInheritExpose,
    /* set_values            */ NULL,
    /* set_values_hook       */ NULL,
    /* set_values_almost     */ XtInheritSetValuesAlmost,
    /* get_values_hook       */ NULL,
    /* accept_focus          */ NULL,
    /* version               */ XtVersion,
    /* callback offsets      */ NULL,
    /* tm_table              */ XtInheritTranslations,
    /* query_geometry        */ query_geometry,
    /* display_accelerator   */ XtInheritDisplayAccelerator,
    /* extension             */ NULL,
    },
    /* Composite Class Part */
    {
    /* geometry manager */	geometry_manager,
    /* change_managed   */	change_managed,
    /* insert_child     */	XtInheritInsertChild,
    /* delete_child     */	XtInheritDeleteChild,
    /* extension        */	NULL,
    },
    /* Constraint Class Part */
    {
    /* subresources      */ NULL,
    /* subresource_count */ 0,
    /* constraint_size   */ sizeof(XmFormConstraintRec),
    /* initialize        */ constraint_initialize,
    /* destroy           */ NULL,
    /* set_values        */ constraint_set_values,
    /* extension         */ NULL,
    },
    /* Manager Class Part */
    {
    /* translations           */ XmInheritTranslations,
    /* syn_resources          */ NULL,
    /* num_syn_resources      */ 0,
    /* syn_constraint_res     */ NULL,
    /* num_syn_constraint_res */ 0,
    /* parent_process         */ XmInheritParentProcess,
    /* extension              */ NULL,
    },
    /* XmBulletinBoard part */
    {
    /* always_install_accelerators  */ False,
    /* geo_matrix_create            */ NULL,
    /* focus_moved_proc             */ XmInheritFocusMovedProc,
    /* extension */              NULL,
    },
    /* XmForm part */
    {
    /* extension */              NULL,
    },
    /* FakeForm Class Part */
    {
    /* extension */              NULL,
    }
};
/* *INDENT-ON* */

WidgetClass fakeFormWidgetClass = (WidgetClass)&fakeFormClassRec;
Widget
CreateFakeForm(Widget parent,
		  char *name,
		  Arg *arglist,
		  Cardinal argCount)
{
    return XtCreateWidget(name, fakeFormWidgetClass, parent, arglist, argCount);
}
/* Fake Form.c */
/* Fake Form sub-class */

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget Shell;
    Widget Form;
    Widget TopLabel, BottomLabel;
    Arg args[10];
    int n;
    ExpectedResults expected[] = {
/* constraint_initialize - begin MyForm TopLabelTopLabel */
{ConstraintInitialize},
/* request */
{FormConstraint, "TopLabelTopLabel", 0, 0, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 0, 0, 0, 0},
/* new_w */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
/* constraint_initialize - end   MyForm TopLabelTopLabel */
/* constraint_initialize - begin MyForm BottomLabel */
{ConstraintInitialize},
/* request */
{FormConstraint, "BottomLabel", 0, 0, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 0, 0, 0, 0},
/* new_w */
{FormConstraint, "BottomLabel", 0, 0, 70, 17, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
/* constraint_initialize - end   MyForm BottomLabel */
/* constraint_set_values - begin MyForm TopLabelTopLabel */
{ConstraintSetValues},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
/* constraint_set_values - end   MyForm TopLabelTopLabel */
/* constraint_set_values - begin MyForm BottomLabel */
{ConstraintSetValues},
{FormConstraint, "BottomLabel", 0, 0, 70, 17, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
{FormConstraint, "BottomLabel", 0, 0, 70, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
{FormConstraint, "BottomLabel", 0, 0, 70, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, "", False, True, 65535, 40, 0, 0},
/* constraint_set_values - end   MyForm BottomLabel */
/* change_managed - begin MyForm */
{ChangeManaged},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 0, 0, "", False, True, 65535, 65535, 0, 0},
{FormConstraint, "BottomLabel", 0, 0, 70, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 0, 0, "", False, True, 65535, 40, 0, 0},

{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
/* change_managed - end   MyForm */
/* constraint_set_values - begin MyForm BottomLabel */
{ConstraintSetValues},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 42},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 42},
{FormConstraint, "BottomLabel", 1, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
/* constraint_set_values - end   MyForm BottomLabel */
/* geometry_manager - begin MyForm BottomLabel */
{GeometryManager},
/* request (x 1 y 0) reply (CWStackMode h 23 bw 31486) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
/* request (x 1 y 0) reply (CWStackMode h 23 bw 31486) answer No */
/* geometry_manager - end   MyForm BottomLabel */
/* constraint_set_values - begin MyForm TopLabelTopLabel */
{ConstraintSetValues},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "TopLabelTopLabel", 1, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
/* constraint_set_values - end   MyForm TopLabelTopLabel */
/* geometry_manager - begin MyForm TopLabelTopLabel */
{GeometryManager},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm TopLabelTopLabel */
/* constraint_set_values - begin MyForm BottomLabel */
{ConstraintSetValues},
{FormConstraint, "BottomLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
{FormConstraint, "BottomLabel", 0, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
{FormConstraint, "BottomLabel", 103, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
/* constraint_set_values - end   MyForm BottomLabel */
/* geometry_manager - begin MyForm BottomLabel */
{GeometryManager},
/* request (x 103) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "BottomLabel", 0, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
/* request (x 103) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm BottomLabel */
/* constraint_set_values - begin MyForm TopLabelTopLabel */
{ConstraintSetValues},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "TopLabelTopLabel", 1, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "BottomLabel", True, True, 100, 17, 204, 42},
/* constraint_set_values - end   MyForm TopLabelTopLabel */
/* geometry_manager - begin MyForm TopLabelTopLabel */
{GeometryManager},
/* request (x 1 h 40) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
/* request (x 1 h 40) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm TopLabelTopLabel */
/* constraint_set_values - begin MyForm TopLabelTopLabel */
{ConstraintSetValues},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 42, 42, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "TopLabelTopLabel", 1, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 42, 42, "BottomLabel", True, True, 100, 17, 204, 42},
/* constraint_set_values - end   MyForm TopLabelTopLabel */
/* geometry_manager - begin MyForm TopLabelTopLabel */
{GeometryManager},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 42, 42, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 42, "BottomLabel", True, True, 100, 17, 204, 42},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 204, 42},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm TopLabelTopLabel */
/* constraint_set_values - begin MyForm BottomLabel */
{ConstraintSetValues},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_NONE, "TopLabelTopLabel", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 204, 204, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
{FormConstraint, "BottomLabel", 1, 0, 100, 40, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
/* constraint_set_values - end   MyForm BottomLabel */
/* geometry_manager - begin MyForm BottomLabel */
{GeometryManager},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 102, 0, 100, 40, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 30, 0, 70, 40, XmATTACH_NONE, "", 0, 65535, 30, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm BottomLabel */
/* constraint_set_values - begin MyForm TopLabelTopLabel */
{ConstraintSetValues},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "TopLabelTopLabel", 1, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
/* constraint_set_values - end   MyForm TopLabelTopLabel */
/* geometry_manager - begin MyForm TopLabelTopLabel */
{GeometryManager},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 30, 0, 70, 40, XmATTACH_NONE, "", 0, 65535, 30, 30, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 42},
{FormConstraint, "BottomLabel", 30, 0, 70, 40, XmATTACH_NONE, "", 0, 65535, 30, 30, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 42},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm TopLabelTopLabel */
/* constraint_set_values - begin MyForm BottomLabel */
{ConstraintSetValues},
{FormConstraint, "BottomLabel", 30, 0, 70, 40, XmATTACH_NONE, "", 0, 65535, 30, 30, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_NONE, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 61},
{FormConstraint, "BottomLabel", 30, 0, 70, 40, XmATTACH_FORM, "", 0, 65535, 30, 30, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 42, 42, "", True, True, 70, 40, 102, 61},
{FormConstraint, "BottomLabel", 1, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
/* constraint_set_values - end   MyForm BottomLabel */
/* geometry_manager - begin MyForm BottomLabel */
{GeometryManager},
/* request (x 1 y 19 w 100) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 30, 0, 70, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
/* request (x 1 y 19 w 100) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm BottomLabel */
/* change_managed - begin MyForm */
{ChangeManaged},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},

{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
/* change_managed - end   MyForm */
/* constraint_set_values - begin MyForm TopLabelTopLabel */
{ConstraintSetValues},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "TopLabelTopLabel", 1, 0, 100, 59, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 61, 19, "BottomLabel", True, True, 100, 17, 102, 61},
/* constraint_set_values - end   MyForm TopLabelTopLabel */
/* geometry_manager - begin MyForm TopLabelTopLabel */
{GeometryManager},
/* request (x 1 h 59) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 61, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 59, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 61, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
/* request (x 1 h 59) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm TopLabelTopLabel */
/* constraint_set_values - begin MyForm TopLabelTopLabel */
{ConstraintSetValues},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 59, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 61, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 59, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 61, 19, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "TopLabelTopLabel", 1, 0, 100, 59, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 61, 61, "BottomLabel", True, True, 100, 17, 102, 61},
/* constraint_set_values - end   MyForm TopLabelTopLabel */
/* geometry_manager - begin MyForm TopLabelTopLabel */
{GeometryManager},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) */
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 59, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 61, 61, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
{FormConstraint, "TopLabelTopLabel", 0, 0, 100, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 61, "BottomLabel", True, True, 100, 17, 102, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 102, 61},
/* request (x 1) reply (GEOMETRY_NO_FIELDS) answer No */
/* geometry_manager - end   MyForm TopLabelTopLabel */
/* 0 errors */
/* query_geometry - begin MyForm */
{QueryGeometry},
/* proposed (GEOMETRY_NO_FIELDS) desired (GEOMETRY_NO_FIELDS) */
{Geometry, "MyForm", 0, 0, 204, 61, 0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,"", False, False, 0, 0, 204, 61, 0 | 0, 0, 0, 0, 0, 0},
{Geometry, "MyForm", 0, 0, 204, 61, 0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,"", False, False, 0, 0, 204, 61, 0 | 0, 0, 0, 0, 0, 0},
{FormConstraint, "TopLabelTopLabel", 0, 0, 202, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 204, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 61, "BottomLabel", True, True, 100, 17, 204, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 204, 61},
{FormConstraint, "TopLabelTopLabel", 0, 0, 202, 17, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 204, 102, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_NONE, "", 0, 65535, 19, 19, "BottomLabel", True, True, 100, 17, 204, 61},
{FormConstraint, "BottomLabel", 0, 19, 100, 40, XmATTACH_FORM, "", 0, 65535, 0, 0, XmATTACH_FORM, "", 0, 65535, 102, 102, XmATTACH_WIDGET, "TopLabelTopLabel", 0, 65535, 19, 19, XmATTACH_FORM, "", 0, 65535, 61, 61, "", True, True, 70, 40, 204, 61},
{Geometry, "MyForm", 0, 0, 204, 61, 0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,"", False, False, 0, 0, 204, 61, 0 | 0, 0, 0, 0, 0, 0},
{Geometry, "MyForm", 0, 0, 204, 61, 0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,0, "", 0,0,0,0,"", False, False, 0, 0, 204, 61, 0 | CWWidth | CWHeight | 0, 0, 0, 102, 19, 0},
/* proposed (GEOMETRY_NO_FIELDS) desired (w 102 h 19) result (Almost) */
/* query_geometry - end   MyForm */
/* 0 errors */
};

    XtSetLanguageProc(NULL, NULL, NULL);

    Shell = XtVaAppInitialize(&app, "test56", NULL, 0, &argc, argv, FallBack, NULL);
    XtVaSetValues(Shell,
		  XmNallowShellResize, True,
		  NULL);

    Form = CreateFakeForm(Shell, "MyForm", NULL, 0);
    /*
    Form = XmCreateForm(Shell, "MyForm", NULL, 0);
    */
    XtVaSetValues(Form,
    	expectedItems, expected,
    	expectedCount, XtNumber(expected),
    	NULL);

    n = 0;
    TopLabel = XmCreateLabel(Form, "TopLabelTopLabel", args, n);
    n = 0;
    BottomLabel = XmCreateLabel(Form, "BottomLabel", args, n);
  XtVaSetValues(TopLabel,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNbottomAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	XmNrightAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_FORM,
  	XmNheight, 40,
  	NULL);
    XtManageChild(TopLabel);
    XtManageChild(BottomLabel);
    XtManageChild(Form);
    XtRealizeWidget(Shell);
  LessTifTestWaitForIt(Shell);
#if 1
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(TopLabel,
  	XmNrightAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_FORM,
  	XmNleftAttachment, XmATTACH_WIDGET,
  	XmNleftWidget, TopLabel,
  	NULL);
  XtVaSetValues(TopLabel,
  	XmNbottomAttachment, XmATTACH_FORM,
  	NULL);
  LessTifTestWaitForIt(Shell);

  XtVaSetValues(TopLabel,
  	XmNbottomAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_NONE,
  	XmNleftAttachment, XmATTACH_NONE,
  	NULL);
  XtVaSetValues(TopLabel,
  	XmNrightAttachment, XmATTACH_FORM,
  	NULL);
  XtVaSetValues(BottomLabel,
  	XmNtopAttachment, XmATTACH_WIDGET,
  	XmNtopWidget, TopLabel,
  	XmNleftAttachment, XmATTACH_FORM,
  	NULL);
  LessTifTestWaitForIt(Shell);
#if 1
  XtVaSetValues(Shell,
		  XmNallowShellResize, False,
		  NULL);
  XtUnmanageChild(BottomLabel);
  XtVaSetValues(TopLabel,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
  XtVaSetValues(TopLabel,
		  XmNbottomAttachment, XmATTACH_NONE,
		  NULL);
#endif
#if 1
  LessTifTestWaitForIt(Shell);
	{
	Dimension width;
	XtWidgetGeometry geo;

	    XtVaGetValues(Shell,
	    	XmNwidth, &width,
	    	NULL);
	    XtVaSetValues(Shell,
	    	XmNwidth, 2 * width,
	    	NULL);
	    XtQueryGeometry(Form, NULL, &geo);
	}
#endif
#endif
  printf("/* %i errors */\n", GlobalErrors);
    LessTifTestMainLoop(Shell);
    exit(0);
}
