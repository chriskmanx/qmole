/* $Header: /cvsroot/lesstif/lesstif/test/Xm/form/test49.c,v 1.3 2001/05/16 09:37:13 amai Exp $
To:          lesstif@hungry.com
From:        Joerg Bredno <jbredno@bootes.imib.RWTH-Aachen.DE>
Subject:     TestSuite
Date:        Wed, 17 Mar 1999 13:08:30 +0100 (MET)
*/

/* TestSuite for LessTif ATTACH_OPPOSITE_FORM */
/* Sorry for german comments                  */
/* It's just the language I grew up with      */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/ScrollBar.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>


int main(int argc, char *argv[])
{
  int			ac;			/* Argumentzaehler fuer X-Argumente */
  Arg			al[16];			/* Speicher fuer X-Argumente */
  XEvent		Event;
  int			cxFont,cx20,cx30;
  int			cyFont,cy20,cy30;
  Widget		children[16];
  Widget		WgShell,WgMenBar,WgForm,WgDrawing,WgEdge,WgHorzScr,WgVertScr;
  XSetWindowAttributes	xswa;
  XtAppContext		AppContext;		/* Hauptfenster besitzt Applikation Context */
  Display		*display;

  ac = 0;					/* X-Argumente fuer Hauptfenster */
  XtSetArg(al[ac],
  	   XmNtitle,
  	   "test suite");
  ac++;						/* Applikationsname ins Fenster */
  
  WgShell = XtAppInitialize(&AppContext,	/* Initialisierung X, Toolkit */
  			    "test suite",	/* und Hauptfenster erzeugen */
			    (XrmOptionDescList)NULL,
			    0,
			    &argc,
			    argv,
			    (String*)NULL,
			    al,
			    ac);

  display = XtDisplay(WgShell);			/* Verbindung zum Bildschirm lesen */
  ac = 0;
  WgForm = XmCreateForm(WgShell,		/* Rutschfeste Auflage fuer andere */
			"WG_Form",		/* Widgets erzeugen */
			al,
			ac);
  
  WgVertScr = XmCreateScrollBar(WgForm,	/* Diesen erzeugen */
				"WG_VertScr",
				al,
				ac);
  ac = 0;					/* Argumente initialisieren */
  XtSetArg(al[ac],
	   XmNorientation,
	   XmHORIZONTAL);
  ac++;					/* Umschalten vertikal (default)->horizontal */
  WgHorzScr = XmCreateScrollBar(WgForm,
				"WG_HorzScr",
				al,
				ac );	/* Scrollbalken erstellen */

  ac = 0;					/* Argumente initialisieren */
  XtSetArg(al[ac],
	   XmNmarginWidth,
	   0);
  ac++;
  XtSetArg(al[ac],
	   XmNmarginHeight,
	   0);					/* Kein Rand in Drawing-Area */
  ac++;

  WgDrawing = XmCreateDrawingArea(WgForm,	/* Drawing-Area erstellen */
				  "WG_Drawing",
				  al,
				  ac);
  WgMenBar = XmCreateMenuBar(WgForm,	/* Menuezeile erstellen */
			     "WG_MenBar",
			     al,
			     ac);
    
  WgEdge = XmCreateFrame(WgForm,		/* leergebliebene, untere, rechte Ecke */
			 "WG_Edge",
			 al,
			 ac );			/* ToDo: erscheint auch ohne Scrollbalken */

  cyFont = 14;
  cxFont = 10;					/* I know I should read them */

  cy20 = cyFont*3/2;				/* Neue Masseinheiten fuer die Fenster anlegen */
  cy30 = cyFont*2;
  cx20 = cxFont*3/2;

  ac = 0;					/* Einstellungen zu diesem treffen */
  XtSetArg(al[ac],
	   XmNtopAttachment,
	   XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac],
	   XmNtopOffset,
	   cy30);			/* Abstand halten (Hoehe Menuezeile) */
  ac++;
  XtSetArg(al[ac],
	   XmNbottomAttachment,
	   XmATTACH_FORM);
  ac++;					/* am unteren Rand */
  XtSetArg(al[ac],
	   XmNbottomOffset,
	   cy20);
  ac++;
  XtSetArg(al[ac],
	   XmNleftAttachment,
	   XmATTACH_OPPOSITE_FORM);
  ac++;
  XtSetArg(al[ac], XmNleftOffset, -cy20);
  ac++;					/* rechts anliegend, Breite */
  XtSetArg(al[ac],
	   XmNrightAttachment,
	   XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac],
	   XmNrightOffset,0);
  ac++;
  XtSetValues(WgVertScr,
	      al,
	      ac);				/* Werte an X */
  ac = 0;
  XtSetArg(al[ac],XmNtopAttachment,XmATTACH_OPPOSITE_FORM);
  ac++;
  XtSetArg(al[ac],XmNtopOffset,-cy20);
  ac++;
  XtSetArg(al[ac],XmNbottomAttachment, XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac],XmNbottomOffset, 0);
  ac++;
  XtSetArg(al[ac],XmNleftAttachment,XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac],XmNleftOffset, 0);
  ac++;
  XtSetArg(al[ac],XmNrightAttachment,XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac],XmNrightOffset,cy20);
  ac++;
  XtSetValues(WgHorzScr,
	      al,
	      ac);
  ac = 0;					/* Einstellungen Drawing-Area */
  XtSetArg(al[ac],XmNtopAttachment,XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac],XmNtopOffset,cy30);
  ac++;
  XtSetArg(al[ac],XmNbottomAttachment,XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac],XmNbottomOffset,cy20);
  ac++;					/* Platz am unteren Rand lassen */
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
  ac++;						/* Am linken Rand anlegen */
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
  ac++;						/* Am rechten Rand */
  XtSetArg(al[ac],XmNrightOffset,cy20);	/* Platz lassen */
  ac++;
  XtSetArg(al[ac],XmNresizePolicy,XmRESIZE_NONE);
  ac++;						/* geerbt von G_Lib */
  XtSetValues(WgDrawing,
	      al,
	      ac);
  ac = 0;
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac], XmNtopOffset, 0);
  ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_OPPOSITE_FORM);
  ac++;
  XtSetArg(al[ac], XmNbottomOffset, -cy30);
  ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac], XmNleftOffset, 0);
  ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac], XmNleftOffset, 0);
  ac++;
  XtSetValues (WgMenBar,
	       al,
	       ac);				/* Geometrie fuer Menue festlegen */
    
  ac = 0;					/* und rechte untere Ecke */
  XtSetArg(al[ac], XmNtopAttachment, XmATTACH_OPPOSITE_FORM);
  ac++;
  XtSetArg(al[ac], XmNtopOffset, -cy20);
  ac++;
  XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
  ac++;
  XtSetArg(al[ac], XmNleftAttachment, XmATTACH_OPPOSITE_FORM);
  ac++;
  XtSetArg(al[ac], XmNleftOffset, -cy20);
  ac++;
  XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
  ac++;
  XtSetValues(WgEdge,
	      al,ac);				/* ebenfalls plazieren */
  
  ac = 0;
  children[ac] = WgVertScr;		/* diesen in Liste der Childwindows */
  ac++;
  children[ac] = WgHorzScr;
  ac++;
  children[ac] = WgMenBar;
  ac++;
  children[ac] = WgDrawing;			/* Drawing-Area */
  ac++;
  children[ac] = WgEdge;			/* und das unnoetige Ding in der Ecke */
  ac++;					/* immer in Liste der Childwindows */
    
  XtManageChildren(children,ac);		/* Liste der Children managen lassen */
  XtManageChild(WgForm);			/* Form (Schablone) managen lassen */
  XtRealizeWidget(WgShell);			/* Rahmenfenster und Children darstellen */
    
  xswa.bit_gravity = ForgetGravity;		/* Neuausgabe bei allen Groessenaenderungen */
  XChangeWindowAttributes(XtDisplay(WgDrawing),
			  XtWindow(WgDrawing),
			  CWBitGravity,
			  &xswa);		/* Einstellung fuer WgDrawing */
  
  ac = 0;					/* Setzen der Fenstergroesse */
  XtSetArg(al[ac],
	   XmNwidth,
	   80*cx20);
  ac++;
  XtSetArg(al[ac],
	   XmNheight,
	   25*cy20);
  ac++;
  XtSetValues(WgDrawing,al,ac);		/* Drawing-Area zieht "Drumherum" nach */
  
  {
  	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   74,  125,  153, 0,0,0, /* WG_Form */
   CWWidth | CWHeight | CWX | CWY,  104,   28,   21,  104, 0,0,0, /* WG_VertScr */
   CWWidth | CWHeight | CWX | CWY,    0,  132,  104,   21, 0,0,0, /* WG_HorzScr */
   CWWidth | CWHeight | CWX | CWY,    0,   28,  104,  104, 0,0,0, /* WG_Drawing */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  125,   28, 0,0,0, /* WG_MenBar */
   CWWidth | CWHeight | CWX | CWY,  104,  132,   21,   21, 0,0,0, /* WG_Edge */ 
    };
    PrintDetails(WgShell,Expected);
};
  }
  LessTifTestMainLoop(WgShell);
#if 0
  while (1 == 1)				/* Event-Loop ist endlos (kein Ruecksprung!) */
    {
      XtAppNextEvent(AppContext,&Event);	/* Event (Timer, Interaktion o.a.) lesen */
      XtDispatchEvent(&Event);			/* und von X weiterleiten lassen */
    }
#endif

  exit(0);
}
