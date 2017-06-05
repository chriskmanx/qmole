/*
 * Example originally written by Bruno Tatibouet <tati@ufc.univ-fcomte.fr>,
 * (thanks Bruno !) expanded later to show more cases (XtSetValues on several
 * resources of the input and output objects).
 * The original example only showed that resources at creation time didn't
 * have effect on the input and output objects.
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>

char *fallback[] = {
    "text5.fontList: "
    "-adobe-symbol-medium-r-normal--14-100-*-*-p-*-adobe-fontspecific=small,"
    "-adobe-symbol-medium-r-normal--24-240-*-*-p-*-adobe-fontspecific=large",
    NULL
};

XFontStruct* fontB ;
XmFontList fl;
XmFontListEntry fe ;
char *font1 =
	"-adobe-symbol-medium-r-normal--14-100-*-*-p-*-adobe-fontspecific",
     *font2 =
	"-adobe-symbol-medium-r-normal--24-240-*-*-p-*-adobe-fontspecific",
     *font3 =
	"this-font-doesn't-exist",
     *font4 =
      "-adobe-symbol-medium-r-normal--14-100-*-*-p-*-adobe-fontspecific=small,"
      "-adobe-symbol-medium-r-normal--24-240-*-*-p-*-adobe-fontspecific=large";

void doit(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "Help ... (%s)\n", (char *)client);
}

int main (int argc, char* argv[])
{  
  XtAppContext app_context;  
  Widget toplevel, rc, t1, t2, t3, t4, t5;
  Arg options[15] ;
  int ac ;
  unsigned char txt[5] = { 0xf0, 0xA7, 0x61, 0x57, '\0' } ;
  
  toplevel = XtAppInitialize (&app_context, "Exemple1", NULL, 0, &argc,
	argv, fallback, NULL, 0) ;

  fontB = XLoadQueryFont (XtDisplay(toplevel), font1) ;
  fe = XmFontListEntryCreate ("tagFontB", XmFONT_IS_FONT, fontB) ;
  fl = XmFontListAppendEntry (NULL,fe) ;

/* Form */
  rc = XmCreateRowColumn(toplevel, "rc", NULL, 0);
  XtManageChild(rc);

/* Text with font set at creation */
  ac = 0 ;
  XtSetArg (options[ac], XmNfontList, fl); ac++  ;  
  t1 = XmCreateText (rc, "text1", options, ac) ;
  XtManageChild (t1) ;
  XmTextSetString(t1,(char*)txt) ;

/* Text with font set with setvalues */
  t2 = XmCreateText (rc, "text2", NULL, 0);
  XtManageChild (t2) ;

  fontB = XLoadQueryFont (XtDisplay(toplevel), font2);
  fe = XmFontListEntryCreate ("tagFontB", XmFONT_IS_FONT, fontB);
  fl = XmFontListAppendEntry (NULL,fe);
  ac = 0;
  XtSetArg (options[ac], XmNfontList, fl); ac++  ;  
  XtSetValues(t2, options, ac);
  XmTextSetString(t2 ,(char*)txt) ;

/* Text with font set with setvalues, except the font doesn't exist */
  t3 = XmCreateText (rc, "text3", NULL, 0);
  XtManageChild (t3) ;

  fontB = XLoadQueryFont (XtDisplay(toplevel), font3);
  fe = XmFontListEntryCreate ("tagFontB", XmFONT_IS_FONT, fontB);
  fl = XmFontListAppendEntry (NULL,fe);
  ac = 0;
  XtSetArg (options[ac], XmNfontList, fl); ac++  ;  
  XtSetValues(t3, options, ac);
  XmTextSetString(t3 ,(char*)txt) ;
/* Even stranger things .. */
  t4 = XmCreateText (rc, "text4", NULL, 0);
  XtManageChild (t4) ;

  fontB = XLoadQueryFont (XtDisplay(toplevel), font4);
  fe = XmFontListEntryCreate ("tagFontB", XmFONT_IS_FONT, fontB);
  fl = XmFontListAppendEntry (NULL,fe);
  ac = 0;
  XtSetArg (options[ac], XmNfontList, fl); ac++  ;  
  XtSetValues(t4, options, ac);
  XmTextSetString(t4 ,(char*)txt) ;

  t5 = XmCreateText (rc, "text5", NULL, 0);
  XtManageChild (t5) ;
  XmTextSetString(t5 ,(char*)txt) ;

  XtAddCallback(t5, XmNhelpCallback, doit, "text widget");
  XtAddCallback(rc, XmNhelpCallback, doit, "global");

/* Do it */
  XtRealizeWidget (toplevel) ;

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  324,  199, 0,0,0, /* rc */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  318,   38, 0,0,0, /* text1 */
   CWWidth | CWHeight | CWX | CWY,    3,   44,  318,   50, 0,0,0, /* text2 */
   CWWidth | CWHeight | CWX | CWY,    3,   97,  318,   31, 0,0,0, /* text3 */
   CWWidth | CWHeight | CWX | CWY,    3,  131,  318,   31, 0,0,0, /* text4 */
   CWWidth | CWHeight | CWX | CWY,    3,  165,  318,   31, 0,0,0, /* text5 */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  return (0);
}
