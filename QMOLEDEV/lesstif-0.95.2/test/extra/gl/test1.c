/*
 * $Header: /cvsroot/lesstif/lesstif/test/extra/gl/test1.c,v 1.1 2002/05/14 22:49:04 dannybackx Exp $
 *
 * My application's main drawing window is an OpenGL 
 * window that is based in a xmFrameWidget class using 
 * GLwCreateMDrawingArea to build the window. I've 
 * noticed that with LessTif, there are about 50 pixel 
 * missing from the top of the window. Also, resizing 
 * the main dialog window does not resize the OpenGL 
 * child window. I don't have these problems with Motif. 
 * 
 * I see these problems in LessTif in the cygwin Windows 
 * 2000 version. I am able to use Motif for the Windows 
 * 2000 Interix 2.2, and various Unix systems. None of 
 * the Motif versions display the problems. Note, there 
 * are two other smaller OpenGL windows in the 
 * application that are missing a smaller number of 
 * pixels. I don't expect that this can be fixed easily, 
 * but I thought I'd report the problem. 
 * 
 * Here's the code fragment that builds the main OpenGL 
 * window. The default size of the window is 900x700. 
 * 
 * 
 * 	Thanks, 
 * 
 * 	Frank Ortega 
 */

#include "LTTconfig.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <Xm/XmAll.h>

#ifdef HAVE_GL_GLWMDRAWA_H
#include <GL/GLwMDrawA.h>
#else
#error No <GL/GLwMDrawA.h> available
#endif

Display	*dpy;
Window win;
GLXContext ctx;
XtAppContext	appc;
Widget		top, form, drawframe, glw;
Arg		al[10];
int		ac;


static void
draw_expose_callback(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "draw_expose_callback(wid %p win %p)\n",
		w, XtWindow(w));
   printf("Redraw event\n");

   glClear( GL_COLOR_BUFFER_BIT );

   glColor3f( 1.0, 1.0, 0.0 );
   glRectf( -0.8, -0.8, 0.8, 0.8 );

   glXSwapBuffers( dpy, XtWindow(w));
}

static void
do_resize(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "do_resize\n");

   printf("Resize event\n");
   glViewport( 0, 0, XtWidth(w), XtHeight(w) );
   glMatrixMode( GL_PROJECTION );
   glLoadIdentity();
   glOrtho( -1.0, 1.0, -1.0, 1.0, -1.0, 1.0 );
}

static Window make_rgb_db_window(Widget w)
{
   int attrib[] = { GLX_RGBA,
		    GLX_RED_SIZE, 1,
		    GLX_GREEN_SIZE, 1,
		    GLX_BLUE_SIZE, 1,
		    GLX_DOUBLEBUFFER,
		    None };
	int scrnum;
	XSetWindowAttributes attr;
	unsigned long mask;
	Window root, win;
	GLXContext ctx;
	XVisualInfo *visinfo;
	unsigned width = XtWidth(w),
		height = XtHeight(w);

	scrnum = DefaultScreen( dpy );
	root = RootWindow( dpy, scrnum );

	visinfo = glXChooseVisual( dpy, scrnum, attrib );
	if (!visinfo) {
		printf("Error: couldn't get an RGB, Double-buffered visual\n");
		exit(1);
	}

	win = XtWindow(w);

	ctx = glXCreateContext( dpy, visinfo, NULL, True );
	glXMakeCurrent( dpy, win, ctx );

	return win;
}

#if 0
static void event_loop( Display *dpy )
{
   XEvent event;

   while (1) {
      XNextEvent( dpy, &event );

      switch (event.type) {
	 case Expose:
	    redraw( dpy, event.xany.window );
	    break;
	 case ConfigureNotify:
	    resize( event.xconfigure.width, event.xconfigure.height );
	    break;
      }
   }
}
#endif

#if 0
/* From glxdemo.c */
int main( int argc, char *argv[] )
{
   Display *dpy;
   Window win;

   dpy = XOpenDisplay(NULL);

   win = make_rgb_db_window( dpy, 300, 300 );

   glShadeModel( GL_FLAT );
   glClearColor( 0.5, 0.5, 0.5, 1.0 );

   XMapWindow( dpy, win );

   event_loop( dpy );
   return 0;
}
#endif

int attrib[] = {
	GLX_RGBA,
	GLX_RED_SIZE,	1,
	GLX_GREEN_SIZE,	1,
	GLX_BLUE_SIZE,	1,
	GLX_DOUBLEBUFFER,
	None
};

#if 0
static void
draw_expose_callback(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "draw_expose_callback(wid %p win %p)\n",
		w, XtWindow(w));
#if 1
	work(NULL);
#else
	XtAppAddWorkProc(appc, work, NULL);
#endif
}
#endif

static void
init_window(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "init_window\n");
	win = make_rgb_db_window(w);
	glShadeModel( GL_FLAT );
	glClearColor( 0.5, 0.5, 0.5, 1.0 );
}

static void
input(Widget w, XtPointer client, XtPointer call)
{
	fprintf(stderr, "input\n");
}

int main(int argc, char *argv[])
{
	XtSetLanguageProc(NULL, NULL, NULL);
	top = XtVaAppInitialize(&appc, "Label", NULL, 0, &argc, argv, NULL, NULL);
	dpy = XtDisplay(top);

	ac = 0;
	XtSetArg(al[ac], XmNwidth, 500); ac++;
	XtSetArg(al[ac], XmNheight, 300); ac++;
	form = XmCreateForm(top, "form", al, ac);
	XtManageChild(form);

	/* Create OpenGL drawing window frame. */ 
	drawframe = XtVaCreateManagedWidget("Drawframe", 
		xmFrameWidgetClass, form, 
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		10,
		XmNbottomAttachment,	XmATTACH_FORM,
		XmNbottomOffset,	10,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		10,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNrightOffset,		10,
#if 0
		XmNtopAttachment, XmATTACH_WIDGET, 
		XmNtopWidget, objxrottxt, 
		XmNleftAttachment, XmATTACH_WIDGET, 
		XmNleftWidget, maglabel, 
		XmNrightAttachment, XmATTACH_FORM, 
		XmNbottomAttachment, XmATTACH_FORM, 
		XmNheight, windowheight+6, 
		XmNwidth, windowwidth+6, 
#endif
		NULL); 
	XtManageChild (drawframe); 

	/* Create OpenGL drawing window. */ 
	ac = 0;
	XtSetArg(al[ac], GLwNattribList,	attrib); ac++;
#if 1
	XtSetArg(al[ac], XmNheight,	280); ac++; 
	XtSetArg(al[ac], XmNwidth,	480); ac++; 
#else
	XtSetArg(al[ac], XmNheight,windowheight); ac++; 
	XtSetArg(al[ac], XmNwidth,windowwidth); ac++; 
	XtSetArg(al[ac], GLwNvisualInfo, vi); ac++;
#endif

	glw = GLwCreateMDrawingArea(drawframe,"Glw", al, ac); 
	XtManageChild (glw); 
	XtAddCallback(glw,GLwNexposeCallback,draw_expose_callback, (caddr_t) 0); 
	XtAddCallback(glw,GLwNresizeCallback,do_resize, (caddr_t) 0); 
	XtAddCallback(glw,GLwNginitCallback,init_window, (caddr_t) 0); 
	XtAddCallback(glw,GLwNinputCallback,input, (caddr_t) 0); 

	XtRealizeWidget(top);
	
#if 0
	XtAppMainLoop(appc);
#else
	LessTifTestMainLoop(top);
#endif
}
