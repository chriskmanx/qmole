/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test16.c,v 1.8 2001/05/16 13:10:19 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Text.h>

#include "../../common/Test.h"

static char *FallBack[] = {
    "*Text.editMode: XmMULTI_LINE_EDIT",
    "*Text.columns: 20",
    "*Text.rows: 10",
    NULL
};

#ifndef XmUNSPECIFIED
#define XmUNSPECIFIED (~0)
#endif

extern int GlobalErrors;

typedef struct _CallbackResultStruct {
	int reason;
	Boolean has_event;
	Boolean doit;
	XmTextPosition currInsert, newInsert;
	XmTextPosition startPos, endPos;
	Boolean has_text;
	char *ptr;
	int length;
	XmTextFormat format;
} _CallbackResult;

_CallbackResult Results[] = { /* insert results of Motif run after this */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  0,  0,  0,  0,  True, "12345",  5,  XmFMT_8_BIT},
/* test 1 0 0 >< */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  0,  0,  0,  0,  True, "54321",  5,  XmFMT_8_BIT},
{XmCR_VALUE_CHANGED, False, False,  0,  0,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 2 0 5 >54321< */
{XmCR_MOVING_INSERT_CURSOR, False,  True,  0,  3,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 3 3 5 >54321< */
{XmCR_MOVING_INSERT_CURSOR, False,  True,  3,  1,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 4 3 5 >54321< */
{XmCR_MOVING_INSERT_CURSOR, False,  True,  3,  5,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 5 5 5 >54321< */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  5,  5,  0,  2,  True, "12",  2,  XmFMT_8_BIT},
{XmCR_VALUE_CHANGED, False, False,  0,  0,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 6 5 5 >12321< */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  5,  5,  2,  4,  True, "4",  1,  XmFMT_8_BIT},
{XmCR_MOVING_INSERT_CURSOR, False,  True,  5,  4,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
{XmCR_VALUE_CHANGED, False, False,  0,  0,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 7 4 4 >1241< */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  4,  4,  2,  3,  True, "34",  2,  XmFMT_8_BIT},
{XmCR_MOVING_INSERT_CURSOR, False,  True,  4,  5,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
{XmCR_VALUE_CHANGED, False, False,  0,  0,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 8 5 5 >12341< */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  5,  5,  4,  5,  True, "",  0,  XmFMT_8_BIT},
{XmCR_MOVING_INSERT_CURSOR, False,  True,  5,  4,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
{XmCR_VALUE_CHANGED, False, False,  0,  0,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 9 4 4 >1234< */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  4,  4,  3,  3,  True, "5",  1,  XmFMT_8_BIT},
{XmCR_MOVING_INSERT_CURSOR, False,  True,  4,  5,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
{XmCR_VALUE_CHANGED, False, False,  0,  0,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 10 5 5 >12354< */
{XmCR_MODIFYING_TEXT_VALUE, False,  True,  5,  5,  3,  5,  True, "45",  2,  XmFMT_8_BIT},
{XmCR_VALUE_CHANGED, False, False,  0,  0,  0,  0, False,  NULL ,  0, XmUNSPECIFIED},
/* test 11 5 5 >12345< */
};

static void
verify(Widget w, XtPointer client_data, XmTextVerifyPtr cbs)
{
static int index = 0;

    if (index >= XtNumber(Results))
    {
	printf("{");
	switch (cbs->reason)
	{
	case XmCR_MODIFYING_TEXT_VALUE:
	    printf("XmCR_MODIFYING_TEXT_VALUE");
	    /* reason, event, doit, currInsert, newInsert, startPos, endPos, text */
	    break;
	case XmCR_MOVING_INSERT_CURSOR:
	    printf("XmCR_MOVING_INSERT_CURSOR");
	    cbs->text = NULL;
	    /* reason, event, doit, curInsert, newInsert */
	    break;
	case XmCR_VALUE_CHANGED:
	    printf("XmCR_VALUE_CHANGED");
	    break;
	default:
	    printf("XmCR_UNSPECIFIED");
	    break;
	}
	if (cbs->reason == XmCR_VALUE_CHANGED)
	{
	    printf(", %5s, %5s, %2li, %2li, %2li, %2li, %5s, %1s%s%1s, %2i, %12s",
		"False",
		"False",
		0,
		0,
		0,
		0,
		"False",
		"",
		"NULL",
		"",
		0,
		"XmUNSPECIFIED"
		);
	    printf("},\n");
	}
	else
	{
	    printf(", %5s, %5s, %2li, %2li, %2li, %2li, %5s, %1s%s%1s, %2i, %12s",
		cbs->event ? "True" : "False",
		cbs->doit ? "True" : "False",
		cbs->currInsert,
		cbs->newInsert,
		cbs->text ? cbs->startPos : 0,
		cbs->text ? cbs->endPos : 0,
		cbs->text ? "True" : "False",
		cbs->text ? (cbs->text->ptr ? "\"" : "") : "",
		cbs->text ? (cbs->text->ptr ? cbs->text->ptr : "NULL") : "NULL",
		cbs->text ? (cbs->text->ptr ? "\"" : "") : "",
		cbs->text ? cbs->text->length : 0,
		(cbs->text ? (cbs->text->format == XmFMT_8_BIT ? "XmFMT_8_BIT" : (cbs->text->format == XmFMT_16_BIT ? "XmFMT_16_BIT" : "XmUNSPECIFIED")) : "XmUNSPECIFIED")
		);
	    printf("},\n");
	}
    }
    else
    {
    	if (cbs->reason != Results[index].reason)
    	{
	    fprintf(stderr, "Result[%i].reason - Got %s, should be %s\n",
	    	index,
	    	cbs->reason == XmCR_MODIFYING_TEXT_VALUE ? "XmCR_MODIFYING_TEXT_VALUE" : cbs->reason == XmCR_MOVING_INSERT_CURSOR ? "XmCR_MOVING_INSERT_CURSOR" : cbs->reason == XmCR_VALUE_CHANGED ? "XmCR_VALUE_CHANGED" : "UNKNOWN",
	    	Results[index].reason == XmCR_MODIFYING_TEXT_VALUE ? "XmCR_MODIFYING_TEXT_VALUE" : Results[index].reason == XmCR_MOVING_INSERT_CURSOR ? "XmCR_MOVING_INSERT_CURSOR" : Results[index].reason == XmCR_VALUE_CHANGED ? "XmCR_VALUE_CHANGED" : "UNKNOWN"
	    	);
	    GlobalErrors++;
    	}
	if (cbs->reason == XmCR_VALUE_CHANGED)
	{
	}
	else
	{
	    if ((cbs->event && Results[index].has_event != True) ||
		(!cbs->event && Results[index].has_event != False))
	    {
		fprintf(stderr, "Result[%i].event - Got event %s, should be %s\n",
		    index,
		    cbs->event ? "True" : "False",
		    Results[index].has_event ? "True" : "False"
		    );
		GlobalErrors++;
	    }
	    if (cbs->doit != Results[index].doit)
	    {
		fprintf(stderr, "Result[%i].doit - Got %s, should be %s\n",
		    index,
		    cbs->doit ? "True" : "False",
		    Results[index].doit ? "True" : "False"
		    );
		GlobalErrors++;
	    }
	    if (cbs->currInsert != Results[index].currInsert)
	    {
		fprintf(stderr, "Result[%i].currInsert - Got %li, should be %li\n",
		    index,
		    cbs->currInsert,
		    Results[index].currInsert
		    );
		GlobalErrors++;
	    }
	    if (cbs->newInsert != Results[index].newInsert)
	    {
		fprintf(stderr, "Result[%i].newInsert - Got %li, should be %li\n",
		    index,
		    cbs->newInsert,
		    Results[index].newInsert
		    );
		GlobalErrors++;
	    }
	    if (cbs->reason == XmCR_MOVING_INSERT_CURSOR)
	    {
		cbs->text = NULL;
	    }
	    if (
	    cbs->reason == XmCR_MODIFYING_TEXT_VALUE && 
	    cbs->startPos != Results[index].startPos)
	    {
		fprintf(stderr, "Result[%i].startPos - Got %li, should be %li\n",
		    index,
		    cbs->startPos,
		    Results[index].startPos
		    );
		GlobalErrors++;
	    }
	    if (
	    cbs->reason == XmCR_MODIFYING_TEXT_VALUE && 
	    cbs->endPos != Results[index].endPos)
	    {
		fprintf(stderr, "Result[%i].endPos - Got %li, should be %li\n",
		    index,
		    cbs->endPos,
		    Results[index].endPos
		    );
		GlobalErrors++;
	    }
	    if (cbs->reason == XmCR_MOVING_INSERT_CURSOR &&
		((cbs->text && Results[index].has_text != True) ||
		(!cbs->text && Results[index].has_text != False)))
	    {
		fprintf(stderr, "Result[%i].text - Got text %s, should be %s\n",
		    index,
		    cbs->text ? "True" : "False",
		    Results[index].has_text ? "True" : "False"
		    );
		GlobalErrors++;
	    }
	    if (cbs->reason == XmCR_MOVING_INSERT_CURSOR &&
		cbs->text && strcmp(cbs->text->ptr, Results[index].ptr) != 0)
	    {
		fprintf(stderr, "Result[%i].text - Got %s, should be %s\n",
		    index,
		    cbs->text->ptr,
		    Results[index].ptr
		    );
		GlobalErrors++;
	    }
	    if (cbs->reason == XmCR_MOVING_INSERT_CURSOR &&
		cbs->text && cbs->text->length != Results[index].length)
	    {
		fprintf(stderr, "Result[%i].length - Got %li, should be %li\n",
		    index,
		    cbs->text->length,
		    Results[index].length
		    );
		GlobalErrors++;
	    }
	    if (cbs->reason == XmCR_MOVING_INSERT_CURSOR &&
		cbs->text && cbs->text->format != Results[index].format)
	    {
		fprintf(stderr, "Result[%i].format - Got %i, should be %i\n",
		    index,
		    cbs->text->format,
		    Results[index].format
		    );
		GlobalErrors++;
	    }
    	}
    	index++;
    }
    switch (cbs->reason)
    {
    case XmCR_MODIFYING_TEXT_VALUE:
	/* reason, event, doit, currInsert, newInsert, startPos, endPos, text */
	if (strcmp(cbs->text->ptr, "12345") == 0)
	{
	    cbs->doit = False;
	}
	break;
    case XmCR_MOVING_INSERT_CURSOR:
	cbs->text = NULL;
	/* reason, event, doit, curInsert, newInsert */
	if (cbs->newInsert == 1)
	{
	    cbs->doit = False;
	}
	if (cbs->newInsert == 5)
	{
	    cbs->newInsert = 2;
	}
	break;
    default:
	break;
    }
}

int
main(int argc, char **argv)
{
  XtAppContext	app;
  Widget Shell;
  Widget Text;
  Widget BottomLabel;

  XtSetLanguageProc(NULL, NULL, NULL);

  Shell = XtVaAppInitialize(&app, "Shell", NULL, 0, &argc, argv, FallBack, NULL);

  Text = XmCreateText(Shell,"Text",NULL,0);
  XtAddCallback(Text, XmNmotionVerifyCallback, (XtCallbackProc)verify, NULL);
  XtAddCallback(Text, XmNmodifyVerifyCallback, (XtCallbackProc)verify, NULL);
  XtAddCallback(Text, XmNvalueChangedCallback, (XtCallbackProc)verify, NULL);

  XtManageChild(Text);

  XtRealizeWidget(Shell);
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  374,  663,  138,  148, 0,0,0, /* Text */
};
  PrintDetails(Shell, Expected);
}
  LessTifTestWaitForIt(Shell);
  XmTextSetString(Text, "12345");
  printf("/* test 1 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 0 || XmTextGetLastPosition(Text) != 0)
  {
  	printf("/* test 1 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextSetString(Text, "54321");
  printf("/* test 2 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 0 || XmTextGetLastPosition(Text) != 5)
  {
  	printf("/* test 2 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextSetInsertionPosition(Text, 3);
  printf("/* test 3 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 3)
  {
  	printf("/* test 3 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextSetInsertionPosition(Text, 1);
  printf("/* test 4 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 3)
  {
  	printf("/* test 4 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextSetInsertionPosition(Text, 5);
  printf("/* test 5 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 5)
  {
  	printf("/* test 5 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextReplace(Text, 0, 2, "12");
  printf("/* test 6 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 5 || strcmp(XmTextGetString(Text), "12321") != 0)
  {
  	printf("/* test 6 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextReplace(Text, 2, 4, "4");
  printf("/* test 7 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 4 || strcmp(XmTextGetString(Text), "1241") != 0)
  {
  	printf("/* test 7 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextReplace(Text, 2, 3, "34");
  printf("/* test 8 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 5 || strcmp(XmTextGetString(Text), "12341") != 0)
  {
  	printf("/* test 8 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextReplace(Text, 4, 5, "");
  printf("/* test 9 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 4 || strcmp(XmTextGetString(Text), "1234") != 0)
  {
  	printf("/* test 9 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextInsert(Text, 3, "5");
  printf("/* test 10 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 5 || strcmp(XmTextGetString(Text), "12354") != 0)
  {
  	printf("/* test 10 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  XmTextReplace(Text, 3, 5, "45");
  printf("/* test 11 %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  if (XmTextGetInsertionPosition(Text) != 5 || strcmp(XmTextGetString(Text), "12345") != 0)
  {
  	printf("/* test 11 error %li %li >%s< */\n", XmTextGetInsertionPosition(Text), XmTextGetLastPosition(Text), XmTextGetString(Text));
  	GlobalErrors++;
  }
  LessTifTestMainLoop(Shell);
  exit(0);
}
