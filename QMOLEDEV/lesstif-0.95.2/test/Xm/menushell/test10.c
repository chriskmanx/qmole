/* $Header: /cvsroot/lesstif/lesstif/test/Xm/menushell/test10.c,v 1.4 2003/07/12 18:36:14 dannybackx Exp $
 * Simulate bug #566315 "NEdit - new window ignores all input"
 */

#include <Xm/XmAll.h>
#include <Xm/MenuShell.h>
#include <X11/keysym.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/fcntl.h>
#include <sys/stat.h>

Widget toplevel;
Widget menuShell;

XtAppContext TheAppContext;
Display	*TheDisplay;

#define	MAX_ARGS	32
#define	MAXPATHLEN	256
#define MAX_LIST_KEYSTROKES	16
#define MAX_LIST_KESTROKE_WAIT	16

#define	GFN_OK		123
#define	GFN_CANCEL	124
#define	SHORT		1

static String fallback[] = {
	"*a-open.accelerator:	Ctrl<Key>o",
	"*b-open.accelerator:	Ctrl<Key>o",
	"*c-open.accelerator:	Ctrl<Key>o",
	"*d-open.accelerator:	Ctrl<Key>o",
	"*a-exit.accelerator:	Ctrl<Key>c",
	NULL
};

static int SelectResult = GFN_CANCEL;
static char *HelpExist = "This is some help text";
static int ErrorDone;                   /* Flag to mark dialog completed   */
static Widget ErrorDialog;              /* Dialog widget for error msgs    */
typedef void (*menuCallbackProc)();
#define MENU_WIDGET(w) (XmGetPostedFromWidget(XtParent(w)))

static int compareThruSlash(const char *string1, const char *string2);
static char *nextSlash(char *ptr);
static char *prevSlash(char *ptr);
static void copyThruSlash(char **toString, char **fromString);
static void makeListTypeable(Widget listW);
int CompressPathname(char *pathname);
static void closeCB(Widget w, XtPointer window, XtPointer callData);

/*
 * Yes this leaks like hell
 */
char *ComposeName(char *p, char *s)
{
	char *r = XtMalloc(strlen(p) + strlen(s)+2);
	strcpy(r, p);
	strcat(r, "-");
	strcat(r, s);
	return r;
}

static void
EditExistingFile(void *inWindow, const char *name,
        const char *path, int flags, char *geometry, int iconic,
        const char *languageMode);

extern const char
*GetCurrentDir(void)
/* return non-NULL value for the current working directory.
   If system call fails, provide a fallback value */
{
  static char curdir[MAXPATHLEN];

  if (!getcwd(curdir, MAXPATHLEN)) {
     perror("NEdit: getcwd() fails");
     strcpy(curdir, ".");
  }
  return (curdir);
}



#define SET_ONE_RSRC(widget, name, newValue) \
{ \
    static Arg tmpargs[1] = {{name, (XtArgVal)0}}; \
    tmpargs[0].value = (XtArgVal)newValue; \
    XtSetValues(widget, tmpargs, 1); \
}

int
CompressPathname(char *pathname)
{
    char *buf, *inPtr, *outPtr;
    struct stat statbuf;

    /* (Added by schwarzenberg)
    ** replace multiple slashes by a single slash
    */
    inPtr=pathname;
    buf=(char *)malloc(strlen(pathname)+2);
    outPtr=buf;
    while (*inPtr) {
        *outPtr=*inPtr++;
        if(*outPtr=='/') {
            while(*inPtr=='/')
                inPtr++;
        }
        outPtr++;
    }
    *outPtr=0;
    strcpy(pathname, buf);

    /* compress out . and .. */
    inPtr = pathname;
    outPtr = buf;
    /* copy initial / */
    copyThruSlash(&outPtr, &inPtr);
    while (inPtr != NULL) {
        /* if the next component is "../", remove previous component */
        if (compareThruSlash(inPtr, "../")) {
                *outPtr = 0;
            /* If the ../ is at the beginning, or if the previous component
               is a symbolic link, preserve the ../.  It is not valid to
               compress ../ when the previous component is a symbolic link
               because ../ is relative to where the link points.  If there's
               no S_ISLNK macro, assume system does not do symbolic links. */
#ifdef S_ISLNK
            if(outPtr-1 == buf || (lstat(buf, &statbuf) == 0 &&
                    S_ISLNK(statbuf.st_mode))) {
                copyThruSlash(&outPtr, &inPtr);
            } else
#endif
            {
                /* back up outPtr to remove last path name component */
                outPtr = prevSlash(outPtr);
                inPtr = nextSlash(inPtr);
            }
        } else if (compareThruSlash(inPtr, "./")) {
            /* don't copy the component if it's the redundant "./" */
            inPtr = nextSlash(inPtr);
        } else {
            /* copy the component to outPtr */
            copyThruSlash(&outPtr, &inPtr);
        }
    }
    /* updated pathname with the new value */
    if (strlen(buf)>MAXPATHLEN) {
       fprintf(stderr, "NEdit: CompressPathname(): file name too long %s\n",
               pathname);
       free(buf);
       return 1;
    }
    else {
       strcpy(pathname, buf);
       free(buf);
       return 0;
    }
}

static char
*nextSlash(char *ptr)
{
    for(; *ptr!='/'; ptr++) {
        if (*ptr == '\0')
            return NULL;
    }
    return ptr + 1;
}

static char
*prevSlash(char *ptr)
{
    for(ptr -= 2; *ptr!='/'; ptr--);
    return ptr + 1;
}

static int
compareThruSlash(const char *string1, const char *string2)
{
    while (TRUE) {
        if (*string1 != *string2)
            return FALSE;
        if (*string1 =='\0' || *string1=='/')
            return TRUE;
        string1++;
        string2++;
    }
}

static void
copyThruSlash(char **toString, char **fromString)
{
    char *to = *toString;
    char *from = *fromString;

    while (TRUE) {
        *to = *from;
        if (*from =='\0') {
            *fromString = NULL;
            return;
        }
        if (*from=='/') {
            *toString = to + 1;
            *fromString = from + 1;
            return;
        }
        from++;
        to++;
    }
}

int
NormalizePathname(char *pathname)
{
    /* if this is a relative pathname, prepend current directory */
#ifdef __EMX__
    /* OS/2, ...: welcome to the world of drive letters ... */
    if (!_fnisabs(pathname)) {
#else
    if (pathname[0] != '/') {
#endif
        char *oldPathname; 
        size_t len; 
    
        /* make a copy of pathname to work from */
        oldPathname=(char *)malloc(strlen(pathname)+1);
        strcpy(oldPathname, pathname);
        /* get the working directory and prepend to the path */
        strcpy(pathname, GetCurrentDir());
        /* check for trailing slash, or pathname being root dir "/":
           don't add a second '/' character as this may break things
           on non-un*x systems */
        len=strlen(pathname); /* GetCurrentDir() returns non-NULL value */
        if ( len==0 ? 1 : pathname[len-1] != '/' ) {
           strcat(pathname, "/"); 
        } 
        strcat(pathname, oldPathname);
        free(oldPathname);
    }

    /* compress out .. and . */
    return CompressPathname(pathname);
}


int
ParseFilename(const char *fullname, char *filename, char *pathname)
{
    int fullLen = strlen(fullname);
    int i, pathLen, fileLen;

#ifdef VMS
    /* find the last ] or : */
    for (i=fullLen-1; i>=0; i--) {
        if (fullname[i] == ']' || fullname[i] == ':')
            break;
    }
#else  /* UNIX */
    char *viewExtendPath;
    int scanStart;

    /* For clearcase version extended paths, slash characters after the "@@/"
       should be considered part of the file name, rather than the path */
    if ((viewExtendPath = strstr(fullname, "@@/")) != NULL)
        scanStart = viewExtendPath - fullname - 1;
    else
        scanStart = fullLen - 1;

    /* find the last slash */
    for (i=scanStart; i>=0; i--) {
        if (fullname[i] == '/')
            break;
    }
#endif

    /* move chars before / (or ] or :) into pathname,& after into filename */
    pathLen = i + 1;
    fileLen = fullLen - pathLen;
    if (pathname) {
        if (pathLen > MAXPATHLEN) {
            return 1;
        }
        strncpy(pathname, fullname, pathLen);
        pathname[pathLen] = 0;
    }
    if (filename) {
        if (fileLen > MAXPATHLEN) {
            return 2;
        }
        strncpy(filename, &fullname[pathLen], fileLen);
        filename[fileLen] = 0;
    }

#ifdef VMS
    return 0;
#else     /* UNIX specific... Modify at a later date for VMS */
    if(pathname)
        return NormalizePathname(pathname);
    else
        return 0;
#endif
}

void AddMotifCloseCallback(Widget shell, XtCallbackProc closeCB, void *arg)
{
    static Atom wmpAtom, dwAtom = 0;
    Display *display = XtDisplay(shell);

    /* deactivate the built in delete response of killing the application */
    XtVaSetValues(shell, XmNdeleteResponse, XmDO_NOTHING, NULL);

    /* add a delete window protocol callback instead */
    if (dwAtom == 0) {
        wmpAtom = XmInternAtom(display, "WM_PROTOCOLS", FALSE);
        dwAtom = XmInternAtom(display, "WM_DELETE_WINDOW", FALSE);
    }
    XmAddProtocolCallback(shell, wmpAtom, dwAtom, closeCB, arg);
}


void ManageDialogCenteredOnPointer(Widget dialogChild)
{
    Widget shell = XtParent(dialogChild);
    Boolean mappedWhenManaged;

    XtVaGetValues(shell, XmNmappedWhenManaged, &mappedWhenManaged, NULL);
    XtVaSetValues(shell, XmNmappedWhenManaged, False, NULL);
    XtManageChild(dialogChild);
    XtMapWidget(shell);
    XtVaSetValues(shell, XmNmappedWhenManaged, mappedWhenManaged, NULL);

}

static void doErrorDialog(const char *errorString, const char *filename)
{
    char string[255];
    XmString mString;

    ErrorDone = False;

    sprintf(string, errorString, filename);
    mString = XmStringCreateLtoR(string, XmSTRING_DEFAULT_CHARSET);

    SET_ONE_RSRC(ErrorDialog, XmNmessageString, mString);
    XmStringFree(mString);
    ManageDialogCenteredOnPointer(ErrorDialog);

    while (!ErrorDone)
        XtAppProcessEvent (XtWidgetToApplicationContext(ErrorDialog), XtIMAll);

    XtUnmanageChild(ErrorDialog);
}


static void existOkCB(Widget w, Boolean * client_data,
               XmFileSelectionBoxCallbackStruct *call_data)
{
    char *filename;                   /* name of chosen file             */
    int  fd;                          /* file descriptor                 */
    int  length;                      /* length of file name             */

	fprintf(stderr, "existOkCB\n");
    XmStringGetLtoR(call_data->value, XmSTRING_DEFAULT_CHARSET, &filename);
    SelectResult = GFN_OK;
    length = strlen(filename);
    if (length == 0 || filename[length-1] == '/') {
        doErrorDialog("Please select a file to open", NULL);
        XtFree(filename);
        return;
    } else    if ((fd = open(filename, O_RDONLY,0))  == -1) {
        doErrorDialog("Error: can't open %s ", filename);
        XtFree(filename);
        return;
    } else
        close(fd);
    XtFree(filename);

    *client_data = True;                /* done with dialog             */
}

static void existCancelCB(Widget w, Boolean * client_data, caddr_t call_data)
{
    SelectResult = GFN_CANCEL;
    *client_data = True;                /* done with dialog             */
}

static void existHelpCB(Widget w, Widget helpPanel, caddr_t call_data)
{
    ManageDialogCenteredOnPointer(helpPanel);
}

static void closeCB(Widget w, XtPointer window, XtPointer callData)
{
	exit(0);
}

int HandleCustomExistFileSB(Widget existFileSB, char *filename)
{
    Boolean   done_with_dialog=False; /* ok to destroy dialog flag         */
    char      *fileString;            /* C string for file selected        */
    char      *dirString;             /* C string for dir of file selected */
    XmString  cFileString;            /* compound string for file selected */
    XmString  cDir;                   /* compound directory selected       */
    XmString  cPattern;               /* compound filter pattern           */
    Widget    help;                   /* help window form dialog           */
#if XmVersion < 1002
    int       i;
#endif

    XtAddCallback(existFileSB, XmNokCallback, (XtCallbackProc)existOkCB,
            &done_with_dialog);
    XtAddCallback(existFileSB, XmNcancelCallback, (XtCallbackProc)existCancelCB,
            &done_with_dialog);
    AddMotifCloseCallback(XtParent(existFileSB), (XtCallbackProc)existCancelCB,
            &done_with_dialog);
//    help = createPanelHelp(existFileSB, HelpExist, "Selecting Files to Open");
//    createErrorDialog(existFileSB);
//    XtAddCallback(existFileSB, XmNhelpCallback, (XtCallbackProc)existHelpCB,
//            (char *)help);
//    if (DefaultDirectory != NULL || DefaultPattern != NULL)
//        XtVaSetValues(existFileSB, XmNdirectory, DefaultDirectory,
//                XmNpattern, DefaultPattern, NULL);

#ifndef SGI_CUSTOM
    makeListTypeable(XmFileSelectionBoxGetChild(existFileSB,XmDIALOG_LIST));
    makeListTypeable(XmFileSelectionBoxGetChild(existFileSB,XmDIALOG_DIR_LIST));
#if XmVersion >= 1002
    XtVaSetValues(existFileSB, XmNinitialFocus, XtParent(
            XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_LIST)), NULL);
#endif
#endif
    ManageDialogCenteredOnPointer(existFileSB);

#if 0
    /* Typing in the directory list is dependent on the list being in the
       same form of alphabetical order expected by the character processing
       routines.  As of about 1.2.3, some Motif libraries seem to have a
       different idea of ordering than is usual for Unix directories.
       To sort them properly, we have to patch the directory and file
       searching routines to re-sort the lists when they change */
    XtVaGetValues(existFileSB, XmNdirSearchProc, &OrigDirSearchProc,
            XmNfileSearchProc, &OrigFileSearchProc, NULL);
    XtVaSetValues(existFileSB, XmNdirSearchProc, replacementDirSearchProc,
            XmNfileSearchProc, replacementFileSearchProc, NULL);
    sortWidgetList(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_DIR_LIST));
    sortWidgetList(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_LIST));
#endif /* SGI_CUSTOM */

    while (!done_with_dialog)
        XtAppProcessEvent(XtWidgetToApplicationContext(existFileSB), XtIMAll);

    if (SelectResult == GFN_OK) {
        XtVaGetValues(existFileSB, XmNdirSpec, &cFileString, XmNdirectory,
                &cDir, XmNpattern, &cPattern, NULL);
        /* Undocumented: file selection box widget allocates copies of these
           strings on getValues calls.  I have risked freeing them to avoid
           memory leaks, since I assume other developers have made this same
           realization, therefore OSF can't easily go back and change it */
//        if (DefaultDirectory != NULL) XmStringFree(DefaultDirectory);
//        if (DefaultPattern != NULL) XmStringFree(DefaultPattern);
//        DefaultDirectory = cDir;
//        DefaultPattern = cPattern;
        XmStringGetLtoR(cFileString, XmSTRING_DEFAULT_CHARSET, &fileString);
        /* Motif 2.x seem to contain a bug that causes it to return only
           the relative name of the file in XmNdirSpec when XmNpathMode is set
           to XmPATH_MODE_RELATIVE (through X resources), although the man
           page states that it always returns the full path name. We can
           easily work around this by checking that the first character of the
           file name is a `/'. */
        if (fileString[0] == '/') {
            /* The directory name is already present in the file name or
               the user entered a full path name. */
            strcpy(filename, fileString);
        } else {
            /* Concatenate the directory name and the file name */
            XmStringGetLtoR(cDir, XmSTRING_DEFAULT_CHARSET, &dirString);
            strcpy(filename, dirString);
            strcat(filename, fileString);
            XtFree(dirString);
        }
        XmStringFree(cFileString);
        XtFree(fileString);
    }
    XtDestroyWidget(existFileSB);
    return SelectResult;
}

static int nKeystrokes = 0; /* Global key stroke history counter */
static void listCharEH(Widget w, XtPointer callData, XEvent *event,
        Boolean *continueDispatch)
{
    char charString[5], c, *itemString;
    int nChars, nItems, i, cmp, selectPos, topPos, nVisible;
    XmString *items;
    KeySym kSym;
    char name[MAXPATHLEN], path[MAXPATHLEN];
    static char keystrokes[MAX_LIST_KEYSTROKES];
    static Time lastKeyTime = 0;

    /* Get the ascii character code represented by the event */
    nChars = XLookupString((XKeyEvent *)event, charString, sizeof(charString),
            &kSym, NULL);
    c = charString[0];

    /* Process selected control keys, but otherwise ignore the keystroke
       if it isn't a single printable ascii character */
    *continueDispatch = False;
    if (kSym==XK_BackSpace || kSym==XK_Delete) {
        nKeystrokes = nKeystrokes > 0 ? nKeystrokes-1 : 0;
        return;
    } else if (kSym==XK_Clear || kSym==XK_Cancel || kSym==XK_Break) {
        nKeystrokes = 0;
        return;
    } else if (nChars!=1 || c<0x021 || c>0x07e) {
        *continueDispatch = True;
        return;
    }

    /* Throw out keystrokes and start keystroke accumulation over from
       scratch if user waits more than MAX_LIST_KESTROKE_WAIT milliseconds */
    if (((XKeyEvent *)event)->time - lastKeyTime > MAX_LIST_KESTROKE_WAIT)
        nKeystrokes = 0;
    lastKeyTime = ((XKeyEvent *)event)->time;

    /* Accumulate the current keystroke, just beep if there are too many */
    if (nKeystrokes >= MAX_LIST_KEYSTROKES)
        XBell(XtDisplay(w), 0);
    else
#ifdef VMS
        keystrokes[nKeystrokes++] = toupper(c);
#else
        keystrokes[nKeystrokes++] = c;
#endif

    /* Get the items (filenames) in the list widget */
    XtVaGetValues(w, XmNitems, &items, XmNitemCount, &nItems, NULL);

    /* compare them with the accumulated user keystrokes & decide the
       appropriate line in the list widget to select */
    selectPos = 0;
    for (i=0; i<nItems; i++) {
        XmStringGetLtoR(items[i], XmSTRING_DEFAULT_CHARSET, &itemString);
        if (ParseFilename(itemString, name, path) != 0) {
           XtFree(itemString);
           return;
        }
        XtFree(itemString);
        cmp = strncmp(name, keystrokes, nKeystrokes);
        if (cmp == 0) {
            selectPos = i+1;
            break;
        } else if (cmp > 0) {
            selectPos = i;
            break;
        }
    }

    /* Make the selection, and make sure it will be visible */
    XmListSelectPos(w, selectPos, True);
    if (selectPos == 0) /* XmListSelectPos curiously returns 0 for last item */
        selectPos = nItems + 1;
    XtVaGetValues(w, XmNtopItemPosition, &topPos,
            XmNvisibleItemCount, &nVisible, NULL);
    if (selectPos < topPos)
        XmListSetPos(w, selectPos-2 > 1 ? selectPos-2 : 1);
    else if (selectPos > topPos+nVisible-1)
        XmListSetBottomPos(w, selectPos+2 <= nItems ? selectPos+2 : 0);
    /* For LessTif 0.89.9. Obsolete now? */
    XmListSelectPos(w, selectPos, True);
}


static Widget CreateMenu(Widget parent, char *name, char *label,
        char mnemonic, Widget *cascadeBtn, int mode)
{
    Widget menu, cascade;
    XmString st1;

    menu = XmCreatePulldownMenu(parent, name, NULL, 0);
    cascade = XtVaCreateWidget(name, xmCascadeButtonWidgetClass, parent, 
        XmNlabelString, st1=XmStringCreateSimple(label),
        XmNsubMenuId, menu, NULL);
    XmStringFree(st1);
    if (mnemonic != 0)
        XtVaSetValues(cascade, XmNmnemonic, mnemonic, NULL);
#ifdef SGI_CUSTOM
    if (mode == SHORT || !GetPrefShortMenus())
        XtManageChild(cascade);
    if (mode == FULL)
        addToToggleShortList(cascade);
#else
    XtManageChild(cascade);
#endif
    if (cascadeBtn != NULL)
        *cascadeBtn = cascade;
    return menu;
} 

static void
makeListTypeable(Widget listW)
{
    XtAddEventHandler(listW, KeyPressMask, False, listCharEH, NULL);
}   

static Widget createMenuItem(Widget parent, char *name, char *label,
        char mnemonic, menuCallbackProc callback, void *cbArg, int mode)
{
    Widget button;
    XmString st1;

    button = XtVaCreateWidget(name, xmPushButtonWidgetClass, parent,
            XmNlabelString, st1=XmStringCreateSimple(label),
            XmNmnemonic, mnemonic, NULL);
    XtAddCallback(button, XmNactivateCallback, (XtCallbackProc)callback, cbArg);
    XmStringFree(st1);
    XtManageChild(button);
    return button;
}

int GetExistingFilename (Widget parent, char *promptString, char *filename)
{
    int       n;                      /* number of arguments               */
    Arg       args[MAX_ARGS];         /* arg list                          */
    Widget    existFileSB;            /* widget file select box            */
    XmString  titleString;            /* compound string for dialog title  */
       
    n = 0;
    titleString = XmStringCreateSimple(promptString);
    XtSetArg(args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
    XtSetArg(args[n], XmNdialogTitle, titleString); n++;
    XtSetArg(args[n], XmNresizePolicy, XmRESIZE_GROW); n++;
    existFileSB = XmCreateFileSelectionDialog(parent,"FileSelect",args,n);
    XmStringFree(titleString);

//    if (RemoveRedundantTextField)
//        XtUnmanageChild(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_TEXT));
    XtUnmanageChild(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_SELECTION_LABEL));

    XtVaSetValues(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_FILTER_LABEL),
            XmNmnemonic, 'l', 
            XmNuserData, XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_FILTER_TEXT),
            NULL);
    XtVaSetValues(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_DIR_LIST_LABEL),
            XmNmnemonic, 'D',
            XmNuserData, XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_DIR_LIST),
            NULL);
    XtVaSetValues(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_LIST_LABEL),
            XmNmnemonic, promptString[strspn(promptString, "lD")],
            XmNuserData, XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_LIST),
            NULL);
//    AddDialogMnemonicHandler(existFileSB, FALSE);
//    RemapDeleteKey(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_FILTER_TEXT));
//    RemapDeleteKey(XmFileSelectionBoxGetChild(existFileSB, XmDIALOG_TEXT));

    return HandleCustomExistFileSB(existFileSB, filename);
}

int PromptForExistingFile(Widget parent, char *prompt, char *fullname)
{
    char *savedDefaultDir;
    int retVal;
    
    /* Temporarily set default directory to window->path, prompt for file,
       then, if the call was unsuccessful, restore the original default
       directory */ 
//    savedDefaultDir = GetFileDialogDefaultDirectory();
//    if (*window->path != '\0')
//        SetFileDialogDefaultDirectory(window->path);
    retVal = GetExistingFilename(toplevel, prompt, fullname);
//    if (retVal != GFN_OK)
//        SetFileDialogDefaultDirectory(savedDefaultDir);
//    if (savedDefaultDir != NULL)
//        XtFree(savedDefaultDir);
    return retVal;
}   

static void exitAP(Widget w, XEvent *event, String *args, Cardinal *nArgs)
{
	exit(0);
}

static void openAP(Widget w, XEvent *event, String *args, Cardinal *nArgs)
{
//    WindowInfo *window = WidgetToWindow(w);
    char filename[MAXPATHLEN], pathname[MAXPATHLEN];

fprintf(stderr, "openAP()\n");

    if (*nArgs == 0) {
        fprintf(stderr, "NEdit: open action requires file argument\n");
        return;
    }
    if (ParseFilename(args[0], filename, pathname) != 0) {
        fprintf(stderr, "NEdit: invalid file name for open action: %s\n",
                args[0]);
        return;
    }
    EditExistingFile(NULL, filename, pathname, 0, NULL, False, NULL);
//    CheckCloseDim();
}

static void openDialogAP(Widget w, XEvent *event, String *args, Cardinal *nArgs)
{
//    WindowInfo *window = WidgetToWindow(w);
    char fullname[MAXPATHLEN], *params[1];
    int response;

    response = PromptForExistingFile(w, "File to Edit", fullname);
    if (response != GFN_OK)
        return;
    params[0] = fullname;
fprintf(stderr, "openDialogAP() : call 'open' action proc\n");
    XtCallActionProc(w, "open", event, params, 1);
//    CheckCloseDim();
}

static XtActionsRec Actions[] = {
	{"open", openAP},
	{"open_dialog", openDialogAP},
	{"exit", exitAP},
};

static void doActionCB(Widget w, XtPointer clientData, XtPointer callData)
{
    Widget menu = MENU_WIDGET(w);

//    HidePointerOnKeyedEvent(WidgetToWindow(menu)->lastFocus,
//            ((XmAnyCallbackStruct *)callData)->event);
    XtCallActionProc(w, (char *)clientData,
            ((XmAnyCallbackStruct *)callData)->event, NULL, 0);
}

Widget CreateWindow(char *name)
{
	Widget	shell, mw, stats, mb, pane;
	Arg	al[32];
	int	ac = 0;

	XtSetArg(al[ac], XmNtitle, name); ac++;
	XtSetArg(al[ac], XmNdeleteResponse, XmDO_NOTHING); ac++;
	XtSetArg(al[ac], XmNiconName, name); ac++;
	XtSetArg(al[ac], XmNinitialState, NormalState); ac++;
	shell = XmCreateDialogShell(toplevel, name, al, ac);
	AddMotifCloseCallback(shell, (XtCallbackProc)closeCB, NULL);

	ac = 0;
	XtSetArg(al[ac], XmNwidth, 100); ac++;
	XtSetArg(al[ac], XmNheight, 60); ac++;
	mw = XmCreateMainWindow(shell, ComposeName(name, "main"), al, ac);
	XtManageChild(mw);

	mb = XmCreateMenuBar(mw, ComposeName(name, "menubar"), NULL, 0);
	XtManageChild(mb);
	pane = CreateMenu(mb,
		ComposeName(name, "fileMenu"),
		ComposeName(name, "File"), 0, NULL, SHORT);
	createMenuItem(pane,
		ComposeName(name, "open"),
		ComposeName(name, "Open..."), 'O', doActionCB, "open_dialog", SHORT);
	createMenuItem(pane,
		ComposeName(name, "exit"),
		ComposeName(name, "Exit"), 'x', doActionCB, "exit", SHORT);

	stats = XtVaCreateWidget(ComposeName(name, "statsAreaForm"), xmFormWidgetClass, mw,
		NULL);
	XtManageChild(stats);

	return shell;
}

static void
EditExistingFile(void *inWindow, const char *name,
        const char *path, int flags, char *geometry, int iconic,
        const char *languageMode)

{
	fprintf(stderr, "EditExistingFile()\n");
//	CreateWindow("editexisting");
	XtRealizeWidget(CreateWindow("b"));
}

int
main(int argc, char **argv)
{
	toplevel = XtAppInitialize(&TheAppContext, "menushell1",
                             NULL, 0, &argc, argv, fallback, NULL, 0);
	TheDisplay = XtDisplay(toplevel);
	XtAppAddActions(TheAppContext, Actions, XtNumber(Actions));

	XtRealizeWidget(CreateWindow("a"));

	XtAppMainLoop(TheAppContext);
}
