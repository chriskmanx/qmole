/*  Copyright 1992, 1993, 1994 John Bovey, University of Kent at Canterbury.
 *
 *  Redistribution and use in source code and/or executable forms, with
 *  or without modification, are permitted provided that the following
 *  condition is met:
 *
 *  Any redistribution must retain the above copyright notice, this
 *  condition and the following disclaimer, either as part of the
 *  program source code included in the redistribution or in human-
 *  readable materials provided with the redistribution.
 *
 *  THIS SOFTWARE IS PROVIDED "AS IS".  Any express or implied
 *  warranties concerning this software are disclaimed by the copyright
 *  holder to the fullest extent permitted by applicable law.  In no
 *  event shall the copyright-holder be liable for any damages of any
 *  kind, however caused and on any theory of liability, arising in any
 *  way out of the use of, or inability to use, this software.
 *
 *  -------------------------------------------------------------------
 *
 *  In other words, do not misrepresent my work as your own work, and
 *  do not sue me if it causes problems.  Feel free to do anything else
 *  you wish with it.
 */


#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#ifdef MAGIC_COOKIE
#include <X11/Xauth.h>
#endif /* MAGIC_COOKIE */
#include <X11/Xresource.h>
#include <X11/cursorfont.h>
#include <sys/socket.h>
#include <netdb.h>
#include "xvt.h"
#include "command.h"
#include "ttyinit.h"
#include "xsetup.h"
#include "screen.h"
/*#include "sbar.h"*/

static int error_handler(Display *,XErrorEvent *);
static int io_error_handler(Display *);
static char *scopy(char *);
static int extract_nonX_args(int, char **);
static unsigned char *get_resource(char *,char *);
static int affirmative(unsigned char *);
static void extract_resources(void);
static void create_window(int,char **);

#define XVT_CLASS	"XTerm"
/*#define SBAR_WIDTH	15*/	/* width of scroll bar */

#define VT_EVENTS	(	ExposureMask |\
				EnterWindowMask|\
				LeaveWindowMask |\
				ButtonPressMask |\
				ButtonReleaseMask |\
				Button1MotionMask \
			)

#define MW_EVENTS	(	KeyPressMask |\
				FocusChangeMask |\
				StructureNotifyMask \
			)

/*
#define SB_EVENTS	(	ExposureMask |\
				EnterWindowMask|\
				LeaveWindowMask |\
				Button2MotionMask |\
				ButtonReleaseMask |\
				ButtonPressMask \
			)
*/

/*  External global variables that are initialised at startup.
 */
Display		*display;
Window		vt_win;		/* vt100 window */
/*Window		sb_win;*/		/* scroll bar window */
Window		main_win;	/* parent window */
Colormap	colormap;
XFontStruct	*mainfont;	/* main font structure */
XFontStruct	*boldfont;	/* bold font structure */
GC 		txgc;		/* GC for drawing text */
GC 		negc;		/* GC for moving areas without graphics exposure */
GC		hlgc;		/* GC used for highlighting selections */
GC		cugc;		/* GC used for the text cursor */
/*GC		sbgc;*/		/* GC used for drawing the scrollbar */
unsigned long	foreground;		/* foreground pixel value */
unsigned long	background;		/* background pixel value */
int		reverse_wrap = 0;	/* enable reverse wrapround */
int		reverse_video = 0;	/* select reverse video */
int		debugging = 0;		/* enable debugging output */
int		messages = 0;		/* flag to enable messages */

static char		*xvt_name;		/* the name the program is run under */
static char		*res_name;		/* the resource name */
static char		*window_name;		/* window name for titles etc. */
static char		*icon_name;		/* name to display in the icon */
static char		*name_name = NULL;	/* name set with -name option */
static int		screen;			/* the X screen number */
static Visual		*visual;
static XrmDatabase	rDB;			/* merged resources database */
static unsigned long	border;			/* border pixel value */
static int		border_width = 1;
static int		save_lines = DEF_SAVED_LINES;	/* number of saved lines */
static unsigned long	cursorclr;
static XColor		foreground_color;
static XColor		background_color;
static XColor		cursor_color;
static int		iconic = 0;		/* start up iconized */
static int		logshell = 0;		/* flag nonzero if using a login shell */
static int		eight_bit_input = 1;	/* eight bit input enabled */
static int		console_flag = 0;	/* true if we want a console window */

/*static int		show_scrollbar = 0;*/	/* scroll-bar displayed if true */

#define OPTABLESIZE	26

static XrmOptionDescRec optable[] =
{
    {"-display",	".display",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-geometry",	"*geometry",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-background",	"*background",	XrmoptionSepArg,	(caddr_t)NULL},
    {"-bg",		"*background",  XrmoptionSepArg,        (caddr_t)NULL},
    {"-foreground",	"*foreground",  XrmoptionSepArg,        (caddr_t)NULL},
    {"-fg",		"*foreground",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-bd",		"*borderColor",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-bw",		"*borderWidth",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-font",	"*font",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-fn",		"*font",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-fb",		"*boldFont",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-title",	"*title",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-T",		"*title",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-n",		"*iconName",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-sl",		"*saveLines",	XrmoptionSepArg,        (caddr_t)NULL},
    {"-cc",		"*charClass",	XrmoptionSepArg,	(caddr_t)NULL},
    {"-cr",		"*cursorColor",	XrmoptionSepArg,	(caddr_t)NULL},
    /*{"-sb",		"*scrollBar",	XrmoptionNoArg,		"on"},*/
    {"-rw",		"*reverseWrap",	XrmoptionNoArg,		"on"},
    {"-rv",		"*reverseVideo",XrmoptionNoArg,		"on"},
    {"-msg",	"*messages",	XrmoptionNoArg,		"on"},
    {"-iconic",	"*iconic",	XrmoptionNoArg,		"on"},
    {"-8",		"*eightBitInput",XrmoptionNoArg,	"on"},
    {"-7",		"*eightBitInput",XrmoptionNoArg,	"off"},
    {"-ls",		"*loginShell",	XrmoptionNoArg,		"on"},
    {"-sf",		"*sunFunctionKeys",XrmoptionNoArg,	"on"},
    {"-debug",	"*debug",	XrmoptionNoArg,         "on"},
};

/*
static char *usearray[] =
{
    "-e <command> <arg> ...	execute command with arguments - must be last argument",
    "-display <name>	specify the display (server)",
    "-geometry <spec>	the initial window geometry",
    "-background <colour>	background colour",
    "-bg <colour>		same as -background",
    "-foreground <colour>	foreground colour",
    "-fg <colour>		same as -foreground",
    "-bd <colour>		border colour",
    "-bw <count>		border width",
    "-font <fontname>	normal font",
    "-fn <fontname		same as -font",
    "-fb <fontname>		font used for bold text",
    "-name <name>		name used for matching X resources",
    "-title <text>		text in window titlebar",
    "-T <text>		same as -title",
    "-n <text>		name in icon or icon window",
    "-sl <count>		number of lines saved after scrolling off window",
    "-cc <char-class>	character classes for double click",
    "-cr <colour>		text cursor colour",
    "-sb			provide an initial scrollbar",
    "-rw			enable reverse wrap",
    "-rv			run in reverse video",
    "-msg			allow messages",
    "-iconic			start up already iconized",
    "-8			process eight-bit characters",
    "-7			strip input and output characters to 7 bits",
    "-ls			run a login shell",
    "-C			connect to console (not all systems)",
    "-console		same as -C",
    "-sf			use Sun function key escape codes",
    NULL
};
*/

static XSizeHints sizehints =
{
    PMinSize | PResizeInc | PBaseSize,
    0, 0, 80, 24,	/* x, y, width and height */
    1, 1,		/* Min width and height */
    0, 0,		/* Max width and height */
    1, 1,		/* Width and height increments */
    {0, 0}, {0, 0},	/* Aspect ratio - not used */
    2 * MARGIN, 2 * MARGIN,		/* base size */
    0
};

/*  Return true if we should be running a login shell.
 */
int is_logshell()
{
    return(logshell);
}

/*  Return true is we are handling eight bit characters.
 */
int is_eightbit()
{
    return(eight_bit_input);
}

/*  Return true if this window should be a console.
 */
int is_console()
{
    return(console_flag);
}

/*  Error handling function, tidy up and then exit.
 */
static int error_handler(Display* dpy, XErrorEvent* evp)
{
    quit(1);
    return(0);
}

static int io_error_handler(Display* dpy)
{
    quit(1);
    return(0);
}

/*  Utility function to return a malloced copy of a string.
 */
static char *scopy(char* str)
{
    char *s;

    if ((s = malloc(strlen((char *)str) + 1)) == NULL)
        abort();
    strcpy(s,str);
    return(s);
}

/*  Do any necessary preprocessing of the environment ready for passing to
 *  the command.
 */
void fix_environment()
{
    int i, j, k;
#ifdef LC_ENV
    int cols, lines;
#endif /* LC_ENV */
    char **com_env;
    extern char **environ;
    static char *elist[] =
    {
        "TERM=",
        "DISPLAY=",
        "WINDOWID=",
#ifdef LC_ENV
        "COLUMNS=",
        "LINES=",
#endif /* LC_ENV */
        NULL
    };
    char buf[500];


    for (i = 0; environ[i] != NULL; i++)
        ;
    com_env = (char **)cmalloc((i + 4) * sizeof(char *));
    for (i = j = 0; environ[i] != NULL; i++)
    {
        for (k = 0; elist[k] != NULL; k++)
            if (strncmp(elist[k],environ[i],strlen(elist[k])) == 0)
                break;
        if (elist[k] == NULL)
            com_env[j++] = environ[i];
    }
    com_env[j++] = scopy(TERM_ENV);
    sprintf(buf,"DISPLAY=%.400s",DisplayString(display));
    com_env[j++] = scopy(buf);
    sprintf(buf,"WINDOWID=%d",(int)main_win);
    com_env[j++] = scopy(buf);
#ifdef LC_ENV
    scr_get_size(&cols,&lines);
    sprintf(buf,"LINES=%d",lines);
    com_env[j++] = scopy(buf);
    sprintf(buf,"COLUMNS=%d",cols);
    com_env[j++] = scopy(buf);
#endif /* LC_ENV */
    com_env[j] = NULL;
    environ = com_env;
}

/*  Take a pass through the arguments extracting any that do not correspond
 *  to X resources.  Recognised arguments are removed from the list and
 *  the new value of argc is returned.
 */
static int extract_nonX_args(int argc, char** argv)
{
    int i, j;
    char *s;

    xvt_name = argv[0];
    res_name = NULL;
    for (j = i = 1; i < argc; i++)
    {
        if (strcmp(argv[i],"-name") == 0)
        {
            if (argv[++i] != NULL)
            {
                res_name = scopy(argv[i]);
                name_name = scopy(argv[i]);
            }
            else
                error("missing -name argument");
        }
        else if (strcmp(argv[i],"-C") == 0 || strcmp(argv[i],"-console") == 0)
        {
            console_flag = 1;
        }
        else
            argv[j++] = argv[i];
    }
    argv[j] = NULL;
    if (res_name == NULL)
    {
        s = strrchr(xvt_name,'/');
        if (s != NULL)
            s++;
        else
            s = xvt_name;
        res_name = scopy(s);
    }
    return(j);
}

/*  Open the display, initialise the rDB resources database and create the
 *  window.  if command is non-null it is the name of -e option command.
 *  iargc and iargv are the original argc, argv so the can be written to a
 *  COMMAND resource.
 */
void init_display(int argc, char** argv, int iargc, char** iargv, char* command)
{
    char str1[256],str2[256];
    char *display_name = NULL;
    XrmDatabase commandlineDB, serverDB;
    XrmValue value;
    XGCValues gcv;
    char *str_type;
    char *s;

    argc = extract_nonX_args(argc,argv);

    icon_name = name_name != NULL ? name_name : command;
    window_name = name_name != NULL ? name_name : command;

    XrmInitialize();
    commandlineDB = NULL;
    XrmParseCommand(&commandlineDB,optable,OPTABLESIZE,res_name,&argc,argv);

    /*
    if (argc > 1)
    {
        usage(0);
        exit(1);
    }
    */

    /*  See if there was a display named in the command line
     */
    sprintf(str1,"%.100s.display",res_name);
    sprintf(str2,"%.100s.Display",XVT_CLASS);
    if (XrmGetResource(commandlineDB,str1,str2,&str_type,&value) == True)
    {
        strncpy(str1,value.addr,(int)value.size);
        display_name = str1;
    }

    if ((display = XOpenDisplay(display_name)) == NULL)
    {
        error("can't open display %s",XDisplayName(display_name));
        quit(1);
    }

    /*  Get the resources from the server if there are any.
     */
    if ((s = XResourceManagerString(display)) != NULL)
    {
        serverDB = XrmGetStringDatabase(s);
        XrmMergeDatabases(serverDB,&rDB);
    }
    XrmMergeDatabases(commandlineDB,&rDB);
    screen = DefaultScreen(display);
    visual = DefaultVisual(display,screen);
    colormap = DefaultColormap(display,screen);
    extract_resources();

#ifdef DEBUG_X
    XSynchronize(display,True),XSetErrorHandler(abort);
#else
    if (!debugging)
    {
        XSetErrorHandler(error_handler);
        XSetIOErrorHandler(io_error_handler);
    }
#endif

    /*  Get colormap entries and create the graphics contexts.
     */
    if (foreground == 0)
        XParseColor(display,colormap,"black",&foreground_color);
    if (background == 0)
        XParseColor(display,colormap,"white",&background_color);
    if (reverse_video == 1 && foreground == 0 && background == 0)
    {
        XColor temp_color;
        temp_color = background_color;
        background_color = foreground_color;
        foreground_color = temp_color;
    }
    if (cursorclr == 0 || visual->class == StaticGray
            || visual->class == StaticColor || visual->class == TrueColor)
    {

        /*  We are not using a colored text cursor so we can use
         *  read only shared colormap entries for foreground and background.
         */
        if (XAllocColor(display,colormap,&foreground_color) != 0)
            foreground = foreground_color.pixel;
        else
        {
            error("can't allocate foreground color %s - using black",s);
            foreground = BlackPixel(display,screen);
        }
        if (XAllocColor(display,colormap,&background_color) != 0)
            background = background_color.pixel;
        else
        {
            error("can't allocate background color %s - using white",s);
            background = WhitePixel(display,screen);
        }
        if (background == foreground)
        {
            /*  If the background and foreground colours end up the same then
             *  try and remap the background to something different.  This
             *  will only work on a monochrome display.
             */
            char *bgclr;
            bgclr =  foreground == BlackPixel(display,screen) ? "white" : "black";
            XParseColor(display,colormap,bgclr,&background_color);
            if (XAllocColor(display,colormap,&background_color) != 0)
                background = background_color.pixel;
        }
        cursorclr = foreground;
    }
    else
    {
        unsigned long plane_masks[2];
        unsigned long pixels[1];

        if (XAllocColorCells(display,colormap,False,plane_masks,2,pixels,1) == 0)
        {
            error("Cannot allocate colormap cells");
            foreground = BlackPixel(display,screen);
            background = background_color.pixel;
            cursorclr = foreground;
        }
        else
        {
            foreground_color.pixel = foreground = pixels[0];
            foreground_color.flags = DoRed | DoBlue | DoGreen;
            XStoreColor(display,colormap,&foreground_color);
            background_color.pixel = background = pixels[0] | plane_masks[0];
            background_color.flags = DoRed | DoBlue | DoGreen;
            XStoreColor(display,colormap,&background_color);
            background_color.pixel = pixels[0] | plane_masks[1];
            XStoreColor(display,colormap,&background_color);
            cursor_color.pixel = cursorclr = pixels[0] |
                                             plane_masks[0] | plane_masks[1];
            cursor_color.flags = DoRed | DoBlue | DoGreen;
            XStoreColor(display,colormap,&cursor_color);
        }
    }

    create_window(iargc,iargv);

    gcv.foreground = foreground;
    gcv.background = background;
    gcv.font = mainfont->fid;
    txgc = XCreateGC(display,main_win,GCForeground|GCBackground|GCFont,&gcv);

    gcv.graphics_exposures = False;
    negc = XCreateGC(display,main_win,
                     GCForeground|GCBackground|GCGraphicsExposures,&gcv);

    gcv.foreground = foreground;
    gcv.background = background;
    /*sbgc = XCreateGC(display,main_win,GCForeground|GCBackground|GCFont,&gcv);*/

    gcv.function = GXinvert;
    gcv.plane_mask = foreground ^ background;
    hlgc = XCreateGC(display,main_win,GCFunction | GCPlaneMask,&gcv);

    gcv.function = GXinvert;
    gcv.plane_mask = cursorclr ^ background;
    cugc = XCreateGC(display,main_win,GCFunction | GCPlaneMask,&gcv);

    /*  initialise the screen data structures.
     */
    scr_init(save_lines);
    /*sbar_init();*/
}

/*  Extract the named resource from the database and return a pointer to a static
 *  string containing it.
 */
static unsigned char *get_resource(char* name, char* class)
{
    static unsigned char resource[256];
    unsigned char str1[256], str2[256];
    XrmValue value;
    char *str_type;

    sprintf((char *)str1,"%.100s.%.100s",res_name,name);
    sprintf((char *)str2,"%.100s.%.100s",XVT_CLASS,class);
    if (XrmGetResource(rDB,(char*)str1,(char*)str2,&str_type,&value) == True)
    {
        if (value.size > 255)
            value.size = 255;
        strncpy((char *)resource,value.addr,(int)value.size);
        return(resource);
    }

    /*  The following is added for compatibility with xterm.
     */
    sprintf((char *)str1,"%.100s.vt100.%.100s",res_name,name);
    sprintf((char *)str2,"%.100s.VT100.%.100s",XVT_CLASS,class);
    if (XrmGetResource(rDB,(char*)str1,(char*)str2,&str_type,&value) == True)
    {
        if (value.size > 255)
            value.size = 255;
        strncpy((char *)resource,value.addr,(int)value.size);
        return(resource);
    }
    return(NULL);
}

/*  Return true if s is "on" or "yes".
 */
static int affirmative(unsigned char* s)
{
    if (strcmp((char *)s,"on") == 0)
        return(1);
    if (strcmp((char *)s,"yes") == 0)
        return(1);
    return(0);
}

/*  Extract the resource fields that are needed to open the window.
 */
static void extract_resources()
{
    unsigned char *s;
    int x, y;
    unsigned int width, height;
    int flags;
    XColor color;

    /*  First get the font since we need it to set the size.
     */
    if ((s = get_resource("font","Font")) == NULL)
        s = (unsigned char *)DEF_FONT;
    if ((mainfont = XLoadQueryFont(display,(char*)s)) == NULL)
    {
        error("can't access font %s, trying %s",s,FIXED_FONT);
        if ((mainfont = XLoadQueryFont(display,FIXED_FONT)) == NULL)
        {
            error("can't access font %s - quitting",FIXED_FONT);
            quit(1);
        }
    }
    sizehints.width_inc = XTextWidth(mainfont,"M",1);
    sizehints.height_inc = mainfont->ascent + mainfont->descent;

    /*  Determine whether debugging is enabled.
     */
    if ((s = get_resource("debug","Debug")) != NULL)
        debugging = affirmative(s);

    /*  Determine whether to allow messages.
     */
    if ((s = get_resource("messages","Messages")) != NULL)
        messages = affirmative(s);

    /*  Determine whether to start up iconized.
     */
    if ((s = get_resource("iconic","Iconic")) != NULL)
        iconic = affirmative(s);

    /*  Determine whether to display the scrollbar.
     */
    /*
    if ((s = get_resource("scrollBar","ScrollBar")) != NULL)
        show_scrollbar = affirmative(s);
    if (show_scrollbar)
        sizehints.base_width += SBAR_WIDTH;
    */

    if ((s = get_resource("borderWidth","BorderWidth")) != NULL)
        border_width = atoi((char *)s);
    flags = 0;
    if ((s = get_resource("geometry","Geometry")) != NULL)
        flags = XParseGeometry((char*)s,&x,&y,&width,&height);

    if (flags & WidthValue)
    {
        sizehints.width = width;
        sizehints.flags |= USSize;
    }
    if (flags & HeightValue)
    {
        sizehints.height = height;
        sizehints.flags |= USSize;
    }
    sizehints.width = sizehints.width * sizehints.width_inc + sizehints.base_width;
    sizehints.height = sizehints.height * sizehints.height_inc + sizehints.base_height;
    sizehints.min_width = sizehints.width_inc + sizehints.base_width;
    sizehints.min_height = sizehints.height_inc + sizehints.base_height;
    if (flags & XValue)
    {
        if (flags & XNegative)
            x = DisplayWidth(display,screen) + x - sizehints.width - 2 * border_width;
        sizehints.x = x;
        sizehints.flags |= USPosition;
    }
    if (flags & YValue)
    {
        if (flags & YNegative)
            y = DisplayHeight(display,screen) + y - sizehints.height - 2 * border_width;
        sizehints.y = y;
        sizehints.flags |= USPosition;
    }
    if (flags & XNegative)
        sizehints.win_gravity = flags & YNegative
                                ? SouthEastGravity
                                : NorthEastGravity;
    else
        sizehints.win_gravity = flags & YNegative
                                ? SouthWestGravity
                                : NorthWestGravity;
    sizehints.flags |= PWinGravity;

    /*  Do the foreground, background and border colours.
     */
    border = foreground;
    if ((s = get_resource("borderColor","BorderColor")) != NULL)
    {
        if (XParseColor(display,colormap,(char*)s,&color) == 0)
            error("invalid border color %s",s);
        else if (XAllocColor(display,colormap,&color) == 0)
            error("can't allocate color %s",s);
        else
            border = color.pixel;
    }
    foreground = 0;
    if ((s = get_resource("foreground","Foreground")) != NULL)
    {
        if (XParseColor(display,colormap,(char*)s,&foreground_color) != 0)
            foreground = 1;
        else
            error("invalid foreground color %s",s);
    }
    background = 0;
    if ((s = get_resource("background","Background")) != NULL)
    {
        if (XParseColor(display,colormap,(char*)s,&background_color) != 0)
            background = 1;
        else
            error("invalid background color %s",s);
    }
    cursorclr = 0;
    if ((s = get_resource("cursorColor","CursorColor")) != NULL)
    {
        if (XParseColor(display,colormap,(char*)s,&cursor_color) != 0)
            cursorclr = 1;
        else
            error("invalid cursor color %s",s);
    }

    /*  Get the window and icon names
     */
    if ((s = get_resource("iconName","IconName")) != NULL)
    {
        icon_name = scopy((char *)s);
        window_name = scopy((char *)s);
    }

    if ((s = get_resource("title","Title")) != NULL)
        window_name = scopy((char *)s);

    if (window_name == NULL)
        window_name = scopy(res_name);
    if (icon_name == NULL)
        icon_name = scopy(res_name);

    /*  Extract the bold font if there is one.
     */
    if ((s = get_resource("boldFont","BoldFont")) != NULL)
        if ((boldfont = XLoadQueryFont(display,(char*)s)) == NULL)
            error("can't access font %s\n",s);

    /*  Get the character class.
     */
    if ((s = get_resource("charClass","CharClass")) != NULL)
        scr_char_class(s);

    /*  Get the reverse wrapround flag.
     */
    if ((s = get_resource("reverseWrap","ReverseWrap")) != NULL)
        reverse_wrap = affirmative(s);

    /*  Get the reverse video flag.
     */
    if ((s = get_resource("reverseVideo","ReverseVideo")) != NULL)
        reverse_video = affirmative(s);

    /*  Get the eight bit flag.
     */
    if ((s = get_resource("eightBitInput","EightBitInput")) != NULL)
        eight_bit_input = affirmative(s);

    /*  Get the login shell flag.
     */
    if ((s = get_resource("loginShell","LoginShell")) != NULL)
        logshell = affirmative(s);

    /*  Determine whether to use Sun function key escape codes.
     */
    if ((s = get_resource("sunFunctionKeys","SunFunctionKeys")) != NULL)
        set_sun_function_keys(affirmative(s));

    /*  extract xvt specific arguments.
     */
    if ((s = get_resource("saveLines","SaveLines")) != NULL)
        save_lines = atoi((char *)s);
}

/*  Open the window.
 */
static void create_window(int argc, char** argv)
{
    XTextProperty wname, iname;
    XClassHint class;
    XWMHints wmhints;
    Cursor cursor;

    main_win = XCreateSimpleWindow(display,DefaultRootWindow(display),
                                   sizehints.x,sizehints.y,sizehints.width,sizehints.height,
                                   border_width,foreground,background);

    if (XStringListToTextProperty(&window_name,1,&wname) == 0)
    {
        error("cannot allocate window name");
        quit(1);
    }
    if (XStringListToTextProperty(&icon_name,1,&iname) == 0)
    {
        error("cannot allocate icon name");
        quit(1);
    }
    class.res_name = res_name;
    class.res_class = XVT_CLASS;
    wmhints.input = True;
    wmhints.initial_state = iconic ? IconicState : NormalState;
    wmhints.flags = InputHint | StateHint;
    XSetWMProperties(display,main_win,&wname,&iname,argv,argc,
                     &sizehints,&wmhints,&class);
    XFree(iname.value);
    XFree(wname.value);
    XSelectInput(display,main_win,MW_EVENTS);

    /*
    sb_win = XCreateSimpleWindow(display,main_win,-1,-1,SBAR_WIDTH - 1,
                                 sizehints.height,1,border,background);
    cursor = XCreateFontCursor(display,XC_sb_v_double_arrow);
    XRecolorCursor(display,cursor,&foreground_color,&background_color);
    XDefineCursor(display,sb_win,cursor);
    XSelectInput(display,sb_win,SB_EVENTS);
    */

    vt_win = XCreateSimpleWindow(display,main_win,0,0, sizehints.width,
                                 sizehints.height,0,border,background);
    /*
    if (show_scrollbar)
    {
        XMoveWindow(display,vt_win,SBAR_WIDTH,0);
        XResizeWindow(display,vt_win,sizehints.width - SBAR_WIDTH,sizehints.height);
    }
    */
    cursor = XCreateFontCursor(display,XC_xterm);
    XRecolorCursor(display,cursor,&foreground_color,&background_color);
    XDefineCursor(display,vt_win,cursor);
    XSelectInput(display,vt_win,VT_EVENTS);
}

/*  Map the window
 */
void map_window()
{
#ifdef LC_ENV
    XEvent event;
#endif /* LC_ENV */

    XMapWindow(display,vt_win);

    /*
    if (show_scrollbar)
        XMapWindow(display,sb_win);
    */
    XMapWindow(display,main_win);

#ifdef LC_ENV

    /*  Setup the window now so that we can add LINES and COLUMNS to
     *  the environment.
     */
    XMaskEvent(display,ExposureMask,&event);
    resize_window();
    scr_reset();
#endif /* LC_ENV */
}

/*  Called after a possible window size change.  If the window size has changed
 *  initiate a redraw by resizing the subwindows and return 1.  If the window
 *  size has not changed then return 0;
 */
int resize_window()
{
    Window root;
    int x, y;
    unsigned int width, height, bdr_width, depth;
    static unsigned int last_width = 0, last_height = 0;

    XGetGeometry(display,main_win,&root,&x,&y,&width,&height,&bdr_width,&depth);
    if (height == last_height && width == last_width)
        return (0);
    last_height = height;
    last_width = width;
    /*
    if (show_scrollbar)
    {
        XResizeWindow(display,sb_win,SBAR_WIDTH - 1,height);
        XResizeWindow(display,vt_win,width - SBAR_WIDTH,height);
    }
    else
    */
        XResizeWindow(display,vt_win,width,height);
    return(1);
}

/*  Toggle scrollbar.
 */
/*
void switch_scrollbar()
{
    Window root;
    int x, y;
    unsigned int width, height, bdr_width, depth;

    XGetGeometry(display,main_win,&root,&x,&y,&width,&height,&bdr_width,&depth);
    if (show_scrollbar)
    {
        XUnmapWindow(display,sb_win);
        XMoveWindow(display,vt_win,0,0);
        width -= SBAR_WIDTH;
        sizehints.base_width -= SBAR_WIDTH;
        sizehints.width = width;
        sizehints.height = height;
        sizehints.flags = USSize | PMinSize | PResizeInc | PBaseSize;
        XSetWMNormalHints(display,main_win,&sizehints);
        XResizeWindow(display,main_win,width,height);
        show_scrollbar = 0;
    }
    else
    {
        XMapWindow(display,sb_win);
        XMoveWindow(display,vt_win,SBAR_WIDTH,0);
        width += SBAR_WIDTH;
        sizehints.base_width += SBAR_WIDTH;
        sizehints.width = width;
        sizehints.height = height;
        sizehints.flags = USSize | PMinSize | PResizeInc | PBaseSize;
        XSetWMNormalHints(display,main_win,&sizehints);
        XResizeWindow(display,main_win,width,height);
        show_scrollbar = 1;
    }
}
*/

/*  Change the window name displayed in the title bar.
 */
void change_window_name(unsigned char* str)
{
    XTextProperty name;

    if (XStringListToTextProperty((char **)&str,1,&name) == 0)
    {
        error("cannot allocate window name");
        return;
    }
    XSetWMName(display,main_win,&name);
    XFree(name.value);
}

/*  Change the icon name.
 */
void change_icon_name(unsigned char* str)
{
    XTextProperty name;

    if (XStringListToTextProperty((char **)&str,1,&name) == 0)
    {
        error("cannot allocate icon name");
        return;
    }
    XSetWMIconName(display,main_win,&name);
    XFree(name.value);
}

/*  Print an error message.
 */
/*VARARGS1*/
void error(char *fmt,...)
{
    va_list args;

    va_start(args,fmt);
    fprintf(stderr,"%s: ",xvt_name);
    vfprintf(stderr,fmt,args);
    va_end(args);
    fprintf(stderr,"\n");
}

/*  Print out a usage message and exit.
 */
/*
void usage(int full)
{
    int i;

    if (full)
    {
        fprintf(stderr,"Xvt: Tiny terminal application based on Xvt 2.1, by John Bovey\n");
        fprintf(stderr,"Permitted arguments are:\n");
        for (i = 0; usearray[i] != NULL; i++)
            fprintf(stderr,"%s\n",usearray[i]);
    }
    else
    {
        fprintf(stderr,"xvt: unrecognised argument\n");
        fprintf(stderr,"Type xvt -help for an option list.\n");
    }
}
*/

/*  Send a 'Magic Cookie' authorisation string to the command.
 */
void send_auth()
{
#ifdef MAGIC_COOKIE
    static char hexdigits[] = "0123456789abcdef";
    char *display_name, *nptr, *dot;
    char *buf, *optr;
    Xauth *auth;
    int i, nlen, len;
    struct hostent *h;
    char hostname[64];

    display_name = DisplayString(display);

    if ((nptr = strchr(display_name, ':')) == NULL)
        return;

    if (nptr == display_name || nptr - display_name > sizeof(hostname))
        return;

    memcpy(hostname, display_name, nptr - display_name);
    hostname[nptr - display_name] = '\0';
    ++nptr;

    if ((h = gethostbyname(hostname)) == NULL)
        return;
    if (h->h_addrtype != AF_INET)
        return;

    if ((dot = strchr(nptr, '.')) != NULL)
        nlen = dot - nptr;
    else
        nlen = strlen(nptr);

    auth = XauGetAuthByAddr(FamilyInternet, 4, h->h_addr_list[0],
                            nlen, nptr, 0, "");
    if (auth == NULL)
        return;

    len = 2 + 2 +
          2 + auth->address_length +
          2 + auth->number_length +
          2 + auth->name_length +
          2 + auth->data_length;

    if ((buf = (char *)cmalloc(len * 2 + 1)) == NULL)
    {
        XauDisposeAuth(auth);
        return;
    }

    optr = buf;

#define PUTSHORT(o, n)	  *o++ = (n >> 8) & 0xff, *o++ = n & 0xff
#define PUTBYTES(o, s, n) PUTSHORT(o, n), memcpy(o, s, n), o += n

    PUTSHORT(optr, (len - 2) * 2);
    PUTSHORT(optr, auth->family);

    PUTBYTES(optr, auth->address, auth->address_length);
    PUTBYTES(optr, auth->number, auth->number_length);
    PUTBYTES(optr, auth->name, auth->name_length);
    PUTBYTES(optr, auth->data, auth->data_length);

#undef PUTSHORT
#undef PUTBYTES

    if (optr != buf + len)
        abort();

    for (i = len - 1; i >= 0; --i)
    {
        buf[i * 2 + 1] = hexdigits[buf[i] & 0xf];
        buf[i * 2] = hexdigits[(buf[i] >> 4) & 0xf];
    }

    buf[len * 2] = '\r';
    send_string((unsigned char *)buf, len * 2 + 1);

    free(buf);
    XauDisposeAuth(auth);

    return;
#endif /* MAGIC_COOKIE */
}
