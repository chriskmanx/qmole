/* $Id: test25.c,v 1.2 2001/01/03 05:18:28 rwscott Exp $
   Check whether the 
*/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/List.h>

#include "../../common/Test.h"

char *labels[6] =
      {"LessTif", "Rocks", "But", "Might", "Has a", "Bug"};
XmString xmStrings[7];



static void browse(Widget w, XtPointer closure, XtPointer cdata)
{
  XmListCallbackStruct *p = (XmListCallbackStruct *)cdata;
  if (p->reason == XmCR_BROWSE_SELECT) {
    char *str;
    if (!XmStringGetLtoR(p->item, XmSTRING_DEFAULT_CHARSET, &str)) { return ; }
    printf("%d: %s\n", p->item_position, str);
    XtFree(str);
  }
}


int main(int c, char **v)
{
  XtAppContext cnx;
  int i;
  Widget top  = XtAppInitialize(&cnx, v[0], NULL, 0, &c, v, NULL, NULL, 0);
  Widget list = XmCreateScrolledList(top, "List", NULL, 0);

  for (i = 0; i < 6; i++)
    {
      xmStrings[i] = XmStringCreateSimple (labels[i]);
    }
  XtVaSetValues (list,
		 XmNitemCount, 6,
		 XmNitems, xmStrings,
		 NULL);

  XtManageChild(list);
  XtAddCallback(list, XmNbrowseSelectionCallback, browse, NULL);
  XtRealizeWidget(top);
  LessTifTestMainLoop(top);
  /*
  XtAppMainLoop(cnx);
  */
  return 0;
}
