/**
 *  $Id: xmbind.c,v 1.9 2001/09/10 10:07:20 amai Exp $
 *
 * The xmbind client -- which is good for configuring the virtual 
 *                      key bindings.
 *
 * Written by Harald Albrecht,
 * albrecht@igpm.rwth-aachen.de
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

#define VERSION_TEXT "GNU LessTif xmbind client 1.02"

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>
#include <Xm/MwmUtil.h>
#include <Xm/VirtKeysP.h>
#include <Xm/XmosP.h>

#include <XmI/XmI.h>


static String AppName;	    /* this client's current name as found in argv[0] */


/*
 * Print the release version info and exit.
 */
static void 
PrintVersion(void)
{
    fprintf(stdout, "%s (Motif %1i.%1i compatible)\n",
                    VERSION_TEXT, XmVERSION, XmREVISION);
    exit(0);
}				/* PrintVersion */


/*
 * Spit out the help and exit.
 */
static void 
PrintHelp(void)
{
    fprintf(stdout, "\
Usage: %s [OPTION] [FILE]\n\n\
-display    specifies the display to use\n\
--help      display this help and exit\n\
--version   output version information and exit\n\
\n\
With no FILE specified, the file .motifbind in the user's home directory\n\
is used. If this file is not found, xmbind loads the default virtual key\n\
bindings.\n", AppName);
    exit(0);
}			/* PrintHelp */


/*
 * If there is no file specified, first try to load .motifbind from
 * the user's $HOME directory. If that fails, install the default
 * key bindings.
 */
static int
LoadDefaultBindings(Display *Dsp)
{
    String Bindings, Filename, Home, MotifBind;
    Atom MotifBindings, MotifDefaultBindings;

    MotifBindings = XmInternAtom(Dsp, _XA_MOTIF_BINDINGS, False);
    MotifDefaultBindings = XmInternAtom(Dsp, _XA_MOTIF_DEFAULT_BINDINGS, False);
    Home = _XmOSGetHomeDirName();
    if (Home == NULL)
	Home = "";
    MotifBind = ".motifbind";
    Filename = XtMalloc(strlen(Home) + 1 + strlen(MotifBind) + 1);
    strcpy(Filename, Home);
    strcat(Filename, "/");
    strcat(Filename, MotifBind);
    if (_XmVirtKeysLoadFileBindings(Filename, &Bindings))
    {
	XChangeProperty(Dsp, RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
			MotifBindings, XA_STRING, 8, PropModeReplace,
			(unsigned char *)Bindings, strlen(Bindings) + 1);
	/*
	 * During creation of the TopLevel shell a XmDisplay was created
	 * and the bindings parsed. This may result in a _MOTIF_DEFAULT_-
	 * BINDINGS property, which we will now remove. The is perfectly
	 * compliant what the csf does in the xmbind client.
	 */
	XDeleteProperty(Dsp, RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
			MotifDefaultBindings);
    }
    else
    {
	_XmVirtKeysLoadFallbackBindings(Dsp, &Bindings);
	/*
	 * Delete any old _MOTIF_BINDINGS property which is still lurking
	 * around.
	 */
	XDeleteProperty(Dsp, RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
			MotifBindings);
    }
    XFlush(Dsp);
    XtFree(Filename);
    return 0;
}			/* LoadDefaultBindings */


/*
 * Try to load the specified binding file and stick it to the root
 * window of the display. This leads to the following interesting question:
 * As a display can have multiple screens to which one we should stick
 * our property? IMHO (--aldi) this should be the root window of screen #0,
 * and NOT the default screen. This is because users may change the default
 * screen dynamically and only screen #0 is always there and secure. In
 * fact, this seems to be what the CSF is currently doing.
 */
static int
LoadBinding(Display *Dsp, String Filename)
{
    String Bindings;
    Atom MotifBindings, MotifDefaultBindings;

    if (_XmVirtKeysLoadFileBindings(Filename, &Bindings))
    {
	MotifBindings = XmInternAtom(Dsp, _XA_MOTIF_BINDINGS, False);
	MotifDefaultBindings = XmInternAtom(Dsp, _XA_MOTIF_DEFAULT_BINDINGS,
					    False);
	XChangeProperty(Dsp, RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
			MotifBindings, XA_STRING, 8, PropModeReplace,
			(unsigned char *)Bindings, strlen(Bindings) + 1);
	XDeleteProperty(Dsp, RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
			MotifDefaultBindings);
	XFlush(Dsp);
      return 0;
    }
    else
    {
	fprintf(stderr, "\
%s: can't load bindings file `%s'\n", AppName, Filename);
	return 1;
    }
}			/* LoadBinding */


/*
 * This is the main part of the xmbind client. Here we parse the remaining
 * command options and do the appropiate things (dump cores, crash the X
 * server, kill other clients, and, and, and...) Because there are some
 * things already available in the LessTif toolkit, this keeps the re-
 * sulting code slim.
 */
int 
main(int argc, char *argv[])
{
    XtAppContext AppContext;
    Widget TopLevel;
    int i, rc;

    TopLevel = XtAppInitialize(&AppContext, "Xmbind",
			       NULL, 0,
			       &argc, argv,
			       NULL,
			       NULL, 0);
    /*
     * Now parse the remaining command line options. Only the first file
     * name found will be parsed and stick to the root window's property.
     */
    AppName = argv[0];
    for (i = 1; i < argc; i++)
    {
	if (strcmp(argv[i], "--help") == 0)
	    PrintHelp();
	else if (strcmp(argv[i], "--version") == 0)
	    PrintVersion();
	else if (argv[i][0] == '-')
	{
	    fprintf(stderr, "\
%s: unrecognized option `%s'\nTry `%s --help' for more information.\n",
		    AppName, argv[i], AppName);
	    exit(1);
	}
	else
	{
	    rc=LoadBinding(XtDisplay(TopLevel), argv[i]);
	    exit(rc);
	}
    }
    rc=LoadDefaultBindings(XtDisplay(TopLevel));

    exit(rc);
}		/* main */
