/**
 *
 * $Id: DebugUtil.c,v 1.14 2006/04/19 18:42:22 dannybackx Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2002, 2004, 2005 LessTif Development Team 
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

static const char rcsid[] = "$Id: DebugUtil.c,v 1.14 2006/04/19 18:42:22 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>

#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#endif

#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/Xresource.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/DrawingAP.h>
#include <Xm/Display.h>
#include <Xm/MwmUtil.h>

/* for strcasecmp(), etc. */
#include <XmI/LTmisc.h>

/* This includes <dmalloc.h> if required */
#include <XmI/DebugUtil.h>


#ifdef WITH_DMALLOC
/* internal interface from Intrinsics.
 * The "const" is probably not in the real sources... 
 */
extern void _XtAllocError(const char *);

#else

/* Our Xt*alloc() replacements for dmalloc. To keep the public/external
   library entries unchanged we also supply stubs for those if
   not using dmalloc. */
XtPointer   _LtDebugMalloc(const char *f,  int l, Cardinal size);
XtPointer   _LtDebugCalloc(const char *f,  int l, Cardinal count, Cardinal size);
XtPointer   _LtDebugRealloc(const char *f, int l, XtPointer p, Cardinal size);
void        _LtDebugFree(const char *f,    int l, XtPointer p);
#endif

/* "Correct" some defines: */
#ifdef	_LtDebugInDebug
#undef	_LtDebugInDebug
#endif

Boolean _LtDebugInDebug(const char *fn, Widget w);


#ifndef LESSTIF_PRODUCTION
/* All external interfaces need to be in place for both, 
   debug and production build. We may just reduce them
   to empty stubs ... */
static void _LtDebugOpenFile(void);
static Boolean __LtDebugPrintWidgetID(void);
static void sighandler(int signo);
static Boolean siginstall(void);
#endif

/*
 *  some #defines being used here
 */
#undef DEBUGSIG        /* debug the 'signal feature' */
#undef DEBUGVALIDATE   /* debug the ValidateSource(), etc. routines */
#undef PRINT_STATE     /* add info to __LtDebugPrintTree() output */


/*
 * Functionality in this file is influenced at run time by these
 * environment variables :
 *
 * - DEBUG_SOURCES : colon-separated list of files from which debug output
 *      is generated.
 *      Special values "all" and "none" have obvious meaning.
 *      You may block file from the list by adding a "-" prefix,
 *      this is obviously only useful in conjunction with "all",
 *      e.g. DEBUG_SOURCES=all:-Vendor.c:-XmString
 *      Using an asterisk as a wildcard is also supported, e.g.
 *      DEBUG_SOURCES=Text*
 *      The code may not be failsafe for some pathological combinations,
 *      but we don't expect anyone to debug the debugging code ... ;-)
 *
 * - DEBUG_PRINT_WIDGETID : if this variable exists, then all widgets printed
 *      with _LtDebug etc. will also print their widget ID. If the variable
 *      doesn't exist, then they only print their name for identification.
 *
 * - DEBUG_FILE : if this variable exists, then the file is used for output.
 *      If "%p" is part of the value of DEBUG_FILE, then it is replaced by the
 *      process id.
 *      "stdout" and "stderr" are recognized and have their special
 *      obvious meaning.
 *
 * - DEBUG_SIGNAL: define to the macro which belongs to the signal which
 *      should be accepted by the program as a switch to toggle
 *      debugging on/off. No value or "none" will turn the feature off.
 *      Valid values are compile time dependent (WRT libXm),
 *      check below what's actually supported.
 *
 *  - DEBUG_TOGGLE: initial value whether debugging should be
 *      enabled or disabled upon program start. Valid Values are on/off.
 *      e.g. DEBUG_TOGGLE=off
 *
 * If the C macro LESSTIF_PRODUCTION is defined, then _LtDebug etc. don't
 *      work. Note: this is a compile time option.
 * To have maximum performance, sequences of _LtDebug statements should be
 *      surrounded by an if (_LtDebugInDebug(__FILE__, w)) statement.
 *      _LtDebugInDebug is False when LESSTIF_PRODUCTION is defined.
 */



#ifndef LESSTIF_PRODUCTION
static Boolean _LtDebugFlag = True;
static FILE *_LtDebugFile = NULL;
typedef void (*sighandler_t)(int);
#endif


#define	_LtDebugNONE			0
#define	_LtDebugINT			1
#define	_LtDebugSTRING			2
#define	_LtDebugXMSTRING		3
#define	_LtDebugCHAR			4
#define	_LtDebugSHORT			5
#define	_LtDebugATTACHMENT		6
#define	_LtDebugWIDGET			7
#define	_LtDebugBOOLEAN			8
#define	_LtDebugSELECTION_POLICY	9
#define	_LtDebugXMSTRING_LIST		10	/* bingo */
#define _LtDebugDIALOG_STYLE		11
#define _LtDebugEDIT_MODE		12
#define _LtDebugALIGNMENT		13
#define _LtDebugSB_DISPLAY_POLICY	14
#define _LtDebugLIST_SIZE_POLICY	15
#define _LtDebugSB_PLACEMENT		16
#define _LtDebugRESIZE_POLICY		17
#define _LtDebugCOMBOBOX_TYPE		18
#define _LtDebugSCROLLING_POLICY	19
#define _LtDebugDRAG_TYPE		20
#define	_LtDebugMWM_INPUT_MODE		21
#define	_LtDebugDELETE_RESPONSE		22
#define	_LtDebugRENDERTABLE		23



static const struct
{
    const char *name;
    int t;
    const char *related;
} _LtDebugTypes[] =
{
    { XmNdragInitiatorProtocolStyle, _LtDebugDRAG_TYPE, NULL } ,
    { XmNdragReceiverProtocolStyle, _LtDebugDRAG_TYPE, NULL } ,
    { XmNscrollingPolicy, _LtDebugSCROLLING_POLICY, NULL } ,
    { XmNmaximum, _LtDebugINT, NULL } ,
    { XmNminimum, _LtDebugINT, NULL } ,
    { XmNsliderSize, _LtDebugINT, NULL } ,
    { XmNpageIncrement, _LtDebugINT, NULL } ,
    { XmNincrement, _LtDebugINT, NULL } ,
    { XmNx, _LtDebugINT, NULL } ,
    { XmNy, _LtDebugINT, NULL } ,
    { XmNwidth, _LtDebugINT, NULL } ,
    { XmNheight, _LtDebugINT, NULL } ,
    { XmNlabelString, _LtDebugXMSTRING, NULL } ,
    { XmNmessageString, _LtDebugXMSTRING, NULL } ,
    { XmNrowColumnType, _LtDebugNONE, NULL } ,
    { XmNbuttonSet, _LtDebugNONE, NULL } ,
    { XmNbuttonCount, _LtDebugINT, NULL } ,
    { XmNoptionLabel, _LtDebugXMSTRING, NULL } ,
    { XmNdirectory, _LtDebugXMSTRING, NULL } ,
    { XmNoptionMnemonic, _LtDebugCHAR, NULL } ,
    { XmNrows, _LtDebugSHORT, NULL } ,
    { XmNcolumns, _LtDebugSHORT, NULL } ,
    { XmNmarginWidth, _LtDebugINT, NULL } ,
    { XmNmarginHeight, _LtDebugINT, NULL } ,
    { XmNmarginTop, _LtDebugINT, NULL } ,
    { XmNmarginBottom, _LtDebugINT, NULL } ,
    { XmNmarginLeft, _LtDebugINT, NULL } ,
    { XmNmarginRight, _LtDebugINT, NULL } ,
    { XmNselectionArrayCount, _LtDebugINT, NULL } ,
    { XmNshadowThickness, _LtDebugINT, NULL } ,
    { XmNhighlightThickness, _LtDebugINT, NULL } ,
    { XmNtopAttachment, _LtDebugATTACHMENT, NULL } ,
    { XmNbottomAttachment, _LtDebugATTACHMENT, NULL } ,
    { XmNleftAttachment, _LtDebugATTACHMENT, NULL } ,
    { XmNrightAttachment, _LtDebugATTACHMENT, NULL } ,
    { XmNtopOffset, _LtDebugINT, NULL } ,
    { XmNbottomOffset, _LtDebugINT, NULL } ,
    { XmNleftOffset, _LtDebugINT, NULL } ,
    { XmNrightOffset, _LtDebugINT, NULL } ,
    { XmNtopPosition, _LtDebugINT, NULL } ,
    { XmNbottomPosition, _LtDebugINT, NULL } ,
    { XmNleftPosition, _LtDebugINT, NULL } ,
    { XmNrightPosition, _LtDebugINT, NULL } ,
    { XmNdefaultButton, _LtDebugWIDGET, NULL } ,
    { XmNmessageWindow, _LtDebugWIDGET, NULL } ,
    { XmNtopWidget, _LtDebugWIDGET, NULL } ,
    { XmNbottomWidget, _LtDebugWIDGET, NULL } ,
    { XmNleftWidget, _LtDebugWIDGET, NULL } ,
    { XmNrightWidget, _LtDebugWIDGET, NULL } ,
    { XmNsensitive, _LtDebugBOOLEAN, NULL } ,
    { XmNresizable, _LtDebugBOOLEAN, NULL } ,
    { XmNmustMatch, _LtDebugBOOLEAN, NULL } ,
    { XmNresizeHeight, _LtDebugBOOLEAN, NULL } ,
    { XmNresizeWidth, _LtDebugBOOLEAN, NULL } ,
    { XmNfractionBase, _LtDebugINT, NULL } ,
    { XmNhorizontalSpacing, _LtDebugINT, NULL } ,
    { XmNverticalSpacing, _LtDebugINT, NULL } ,
    { XmNrubberPositioning, _LtDebugBOOLEAN, NULL } ,
    { XmNitemCount, _LtDebugINT, NULL } ,
    { XmNfileListItemCount, _LtDebugINT, NULL } ,
    { XmNtextString, _LtDebugXMSTRING, NULL } ,
    { XmNdirSpec, _LtDebugXMSTRING, NULL } ,
    { XmNdirMask, _LtDebugXMSTRING, NULL } ,
    { XmNitems, _LtDebugXMSTRING_LIST, XmNitemCount } ,				/* bingo */
    { XmNselectionPolicy, _LtDebugSELECTION_POLICY, NULL } ,
    { XmNautoUnmanage, _LtDebugBOOLEAN, NULL } ,
    { XmNdialogStyle, _LtDebugDIALOG_STYLE, NULL } ,
    { XmNshowAsDefault, _LtDebugSHORT, NULL } ,
    { XmNeditable, _LtDebugBOOLEAN, NULL } ,
    { XmNmaxLength, _LtDebugINT, NULL } ,
    { XmNdirListItemCount, _LtDebugINT, NULL } ,
    { XmNfileListItemCount, _LtDebugINT, NULL } ,
    { XmNeditMode, _LtDebugEDIT_MODE, NULL } ,
    { XmNalignment, _LtDebugALIGNMENT, NULL } ,
    { XmNrecomputeSize, _LtDebugBOOLEAN, NULL } ,
    { XmNdirectoryValid, _LtDebugBOOLEAN, NULL } ,
    { XmNlistUpdated, _LtDebugBOOLEAN, NULL } ,
    { XmNhorizontalScrollBar, _LtDebugWIDGET, NULL } ,
    { XmNverticalScrollBar, _LtDebugWIDGET, NULL } ,
    { XmNworkWindow, _LtDebugWIDGET, NULL } ,
    { XmNmenuBar, _LtDebugWIDGET, NULL } ,
    { XmNcommandWindow, _LtDebugWIDGET, NULL } ,
    { XmNvisibleItemCount, _LtDebugSHORT, NULL } ,
    { XmNdefaultButtonShadowThickness, _LtDebugSHORT, NULL } ,
    { XmNset, _LtDebugBOOLEAN, NULL } ,
    { XmNtraversalOn, _LtDebugBOOLEAN, NULL } ,
    { XmNspacing, _LtDebugSHORT, NULL } ,
    { XmNscrollBarDisplayPolicy, _LtDebugSB_DISPLAY_POLICY, NULL } ,
    { XmNlistSizePolicy, _LtDebugLIST_SIZE_POLICY, NULL } ,
    { XmNscrollBarPlacement, _LtDebugSB_PLACEMENT, NULL } ,
    { XmNuserData, _LtDebugNONE, NULL } ,
    { XmNallowShellResize, _LtDebugBOOLEAN, NULL } ,
    { XmNresizePolicy, _LtDebugRESIZE_POLICY, NULL } ,
    { XmNradioBehavior, _LtDebugBOOLEAN, NULL } ,
    { XmNradioAlwaysOne, _LtDebugBOOLEAN, NULL } ,
    { XmNnumColumns, _LtDebugSHORT, NULL } ,
    { XmNinitialFocus, _LtDebugWIDGET, NULL } ,
    { XmNmwmInputMode, _LtDebugMWM_INPUT_MODE, NULL } ,
    { XmNmappedWhenManaged,	_LtDebugBOOLEAN,	NULL } ,
    { XmNdeleteResponse,	_LtDebugDELETE_RESPONSE,	NULL } ,
    { XmNwidthInc,	_LtDebugSHORT,	NULL } ,
    { XmNheightInc,	_LtDebugSHORT,	NULL } ,
    { XmNbaseWidth,	_LtDebugSHORT,	NULL } ,
    { XmNbaseHeight,	_LtDebugSHORT,	NULL } ,
    { XmNminWidth,	_LtDebugSHORT,	NULL } ,
    { XmNminHeight,	_LtDebugSHORT,	NULL } ,
    { XmNtitle,	_LtDebugSTRING,	NULL } ,
    { XmNiconName,	_LtDebugSTRING,	NULL } ,
    { XmNcancelLabelString,	_LtDebugXMSTRING,	NULL } ,
    { XmNcomboBoxType, _LtDebugCOMBOBOX_TYPE, NULL } ,
    { XmNlargeCellWidth, _LtDebugINT, NULL } ,
    { XmNlargeCellHeight, _LtDebugINT, NULL } ,
    { XmNacceleratorText,	_LtDebugXMSTRING,	NULL },
    { XmNrenderTable, _LtDebugRENDERTABLE, NULL },
    { XmNfontList, _LtDebugRENDERTABLE, NULL },
    { XmNborderWidth,	_LtDebugSHORT, NULL },
    { NULL, 0, NULL }
    /* the end */
};




#ifndef LESSTIF_PRODUCTION
/* catches the signal defined by DEBUG_SIGNAL and toggles the
   global debugging flag.
   Avoid calling C-runtime functions from within here if possible 
   (which in turn might also raise signals) */
static void
sighandler (int signo)
{

#ifdef DEBUGSIG
fputs ("sighandler(): signal caught\n", stderr);
#endif

  /* switch debugging on/off */
  _LtDebugToggle();
  /* re-install ourselves: perhaps not always necessary, but OTOH
     it shouldn't hurt!? */
  signal (signo, sighandler);
}


static Boolean
siginstall (void)
{
  const char *ptr;
#define NOSIG -1
  static int signo = NOSIG;

  ptr = getenv ("DEBUG_SIGNAL");
  if (ptr)
    {
#ifdef DEBUGSIG
      fprintf (stderr, "siginstall(): trying to catch %s\n", ptr);
#endif
      if ((*ptr == '\0')||(strcasecmp(ptr, "none")==0))
	{
	  fprintf(stderr, "siginstall(): empty value for DEBUG_SIGNAL\n");
	}
#if defined(SIGBREAK)
      else if (strcmp (ptr, "SIGBREAK") == 0)
	signo = SIGBREAK;
#endif
#if defined(SIGUNUSED)
      else if (strcmp (ptr, "SIGUNUSED") == 0)
	signo = SIGUNUSED;
#endif
#if defined(SIGUSR1)
      else if (strcmp (ptr, "SIGUSR1") == 0)
	signo = SIGUSR1;
#endif
#if defined(SIGUSR2)
      else if (strcmp (ptr, "SIGUSR2") == 0)
	signo = SIGUSR2;
#endif
#if defined(SIGUSR3)
      else if (strcmp (ptr, "SIGUSR3") == 0)
	signo = SIGUSR3;
#endif
      else
	fprintf(stderr, "siginstall(): unknown signal in DEBUG_SIGNAL: %s\n",
	        ptr);
    }	/* if (ptr) */

  if (signo == NOSIG)
    {
      return False;
    }
  else
    {
      sighandler_t sigrc;

#ifdef DEBUGSIG
      fprintf (stderr, "siginstall(): installing %p on signal %i\n", sighandler, signo);
#endif
      sigrc=signal (signo, sighandler);
      if (sigrc == SIG_ERR)
         return False;
      else
         return True;
    } /* signo != NOSIG */
}		/* siginstall() */
#endif /* LESSTIF_PRODUCTION */


/* some initialization.
   Does never fail; return value indicates whether debugging 
   is en- or disabled currently */
extern Boolean
_LtDebugInit(void) {
#ifdef LESSTIF_PRODUCTION
    return False;
#else
    static Boolean init=False;

    if (init)
       return _LtDebugFlag;
    else
      {

       const char *ptr;

       ptr=getenv("DEBUG_TOGGLE");
       if (ptr && (strcmp(ptr, "off")==0))
          _LtDebugFlag=False;

       _LtDebugOpenFile();
       siginstall();
       init = True;
       return _LtDebugFlag;
       }
#endif
}

#ifndef LESSTIF_PRODUCTION
static void 
_LtDebugOpenFile(void)
{
    const char *s;
    char *fn;

    if (_LtDebugFile)
    {
      /* already done */
	return;
    }

    /* The rest here is the initialization code.
       Might be slow and long, since it's done only once */

    s = getenv("DEBUG_FILE");
    if ((s == NULL)||(*s=='\0')||((strcmp(s, "stderr") == 0)))
       _LtDebugFile = stderr;  /* default/fallback value */
    else if (strcmp(s, "stdout") == 0)
       /* the user wants to mix our output with the stdout of the
        * user application.
        */ 
       _LtDebugFile = stdout;
    
    if (_LtDebugFile) 
    {
       /* disable buffering for stdout/stderr */
       setbuf(_LtDebugFile, NULL);
       return;
    }

    
#ifdef HAVE_GETPID
    if (strstr(s, "%p"))
    {
        char *formatstr, *p;
	
        fn = XtMalloc(strlen(s) + 10);
        formatstr=XtMalloc(strlen(s)+1);
        strcpy(formatstr, s);
        p = strstr(formatstr, "%p");
        *(p + 1) = 'd';
        sprintf(fn, s, getpid());
        XtFree(formatstr);
    }
    else
#endif /* HAVE_GETPID */
    {
        fn = XtMalloc(strlen(s) + 1);
        strcpy(fn, s);
    }

    /* "a" means append, create if doesn't exist */
    _LtDebugFile = fopen(fn, "a");
    if (_LtDebugFile == NULL)
    {
        /* if fopen() fails do something reasonable */
        fprintf(stderr, "_LtDebugOpenFile(): Can't open file %s\n", fn);
        _LtDebugFile = stderr;
        /* disable buffering */
        setbuf(_LtDebugFile, NULL);
    }
    else
    {
        /* disable buffering for file */
        setbuf(_LtDebugFile, NULL);
    }
    XtFree(fn);
}

/*
 * See if 'fn' refers to a source file which is allowed to produce
 * debugging output.
 * The ".c" suffix for sources is not required (i.e. Form is equivalent
 * to Form.c).
 */
static Boolean
ValidateSource(const char *fn)
{
    static Boolean init=False;
    static const char *sourcelist;
    typedef struct {
       char *fn;
       Boolean shortmatch;
       size_t len;
    } list_entry_t;
    static list_entry_t **poslist=NULL, **neglist=NULL;
    static int positems=0, negitems=0; 
    static Boolean flag_all=False, flag_none=False;


    if (!init)
    {
       /* Do initialization once and for all.
          Might be a long, slow procedure, but should speed all
          upcoming calls to this routine! */
       sourcelist = getenv("DEBUG_SOURCES");
       if (sourcelist==NULL) {
          /* for compatibility with earlier versions */
          sourcelist = getenv("DEBUGSOURCES");
       }
       /* No keyword "all" or "none" */       
       if ( sourcelist && (strcasecmp(sourcelist, "all")!=0) && (strcasecmp(sourcelist, "none")!=0) ) 
         {
          const char *s, *p;

          s = sourcelist;
          while (s && *s)
          { 
             char *dotptr, *asteriskptr;
             list_entry_t *newitem;
             Cardinal len;
             
             p = strchr(s, ':');
             if (p)
                len=(p-s);
             else
                len=strlen(s);

             newitem=(list_entry_t *)XtMalloc(sizeof(list_entry_t));
             if (*s=='-')
                {
		/* cut the '-' out: */
                len--;
                s++;

                neglist=(list_entry_t **)XtRealloc((char *)neglist, sizeof (list_entry_t*) * (negitems+1));
                neglist[negitems]=newitem;
                negitems++;
                }
             else
                {
                poslist=(list_entry_t **)XtRealloc((char *)poslist, sizeof (list_entry_t*) * (positems+1));
                poslist[positems]=newitem;
                positems++;
                }
           newitem->fn=XtMalloc(len+1);     
	     strncpy(newitem->fn, s, len);
	     newitem->fn[len]='\0';
	     /* Cut the file extensions */
	     if ((dotptr=strrchr(newitem->fn, '.')))
	        *dotptr='\0';
	     
	     /* Check for shortmatch, asterisk */
	     if ((asteriskptr=strchr(newitem->fn, '*')))
	     {
	        *asteriskptr='\0';
	        newitem->shortmatch=True;
	        newitem->len=strlen(newitem->fn);
	     }
	     else
	     {
	        newitem->shortmatch=False;
	        newitem->len=0;
	     }
	     /* proceed to next entry */
             if (p)
                s = p+1;
             else
                s = p; 
          } /* while() */       
       }
        
       /* set some flags to indicate situations where no explicit
          search is later on required: */

       /* list does not exist, is empty or set to "none" */
       if ((!sourcelist) || (!*sourcelist) || (strcasecmp(sourcelist, "none")==0) )
	  flag_none = True;
       /* "all" Sources */
       else if (strcasecmp(sourcelist, "all") == 0)
          flag_all = True;

       init = True;

#ifdef DEBUGVALIDATE
       {
       int i;
       fprintf(stderr, "VS() init\n");
       for (i=0;i<positems;i++)
         {
          fprintf(stderr, "positem[%i]=%s # Short=%i\n", 
                  i, poslist[i].fn, poslist[i].shortmatch);
          }
       for (i=0;i<negitems;i++)
         {
          fprintf(stderr, "negitem[%i]=%s # Short=%i\n", 
                  i, neglist[i].fn, neglist[i].fn.shortmatch);
          }
       }
#endif
    }  /* if (!init) */

    /* the most simple cases: */
    if (flag_none)
	return False;
    else if (flag_all)
        return True;
    else 
       {
       /* OK, we need to check explicitly ... */
       const char *lastslash;
       char *lastdot;
       char shortfn[256]; /* dynamic memory would be too 'expensive' */

       /* First we have to prepare the file name as passed to this routine:
          the __FILE__ macro as inserted from CPP may contain an optional path
          and certainly features a file extension (".c").
          Our DEBUG_SOURCES shouldn't have this, so we have to strip that. */
	  
       lastslash = strrchr(fn, '/');
       if (lastslash && *(lastslash+1)!='\0')
          strncpy(shortfn, lastslash+1, sizeof(shortfn)-1);
       else
          strncpy(shortfn, fn, sizeof(shortfn)-1);
       lastdot=strrchr(shortfn, '.');
       if (lastdot)
          *lastdot='\0';

       /* we might have a negative list */
       if (strcmp(poslist[0]->fn, "all")==0)
          {
           int i;
           for (i=0; i<negitems; i++)
             {
              if (neglist[i]->shortmatch)
              {
                 if (strncmp(shortfn, neglist[i]->fn, neglist[i]->len)==0)
                    return False;
              }
              else
                  if (strcmp(shortfn, neglist[i]->fn)==0)
                    return False;
              }
           return True;
          }
       else /* now check in the positive list */
          {
           int i;
           for (i=0; i<positems; i++)
              {
               if (poslist[i]->shortmatch)
               {   
                  if (strncmp(shortfn, poslist[i]->fn, poslist[i]->len)==0)
                     return True;
                }
               else
               {
                  if (strcmp(shortfn, poslist[i]->fn)==0)
                     return True;
                }
               }
           return False; /* no matching entry found */
          }
      }
} /* ValidateSource() */

#endif /* !LESSTIF_PRODUCTION */


extern void 
_LtDebug(const char *fn, Widget w, const char *fmt, ...)
{
#ifndef LESSTIF_PRODUCTION
    va_list ap;

#ifdef DEBUGVALIDATE
fprintf(stderr, "ValidateSource(%s)=%s\n",
        fn, _LtDebugBoolean2String(ValidateSource(fn)));
#endif
    if (_LtDebugInit() && ValidateSource(fn))
    {
	if (w)
	{
	    if (__LtDebugPrintWidgetID())
	    {
		fprintf(_LtDebugFile, "%s %s [%p]: ",
			w->core.widget_class->core_class.class_name,
			XtName(w), w);
	    }
	    else
	    {
		fprintf(_LtDebugFile, "%s %s: ",
			w->core.widget_class->core_class.class_name, XtName(w));
	    }
	}
	else
	{
	    fprintf(_LtDebugFile, "(null widget): ");
	}

	va_start(ap, fmt);
	vfprintf(_LtDebugFile, fmt, ap);
	va_end(ap);

	fflush(_LtDebugFile);
    }
#endif /* !LESSTIF_PRODUCTION */
}


extern void 
_LtDebug2(const char *fn, Widget w, Widget c, const char *fmt, ...)
{
#ifndef LESSTIF_PRODUCTION
    va_list ap;

    if (_LtDebugInit() && ValidateSource(fn))
    {
	if (w && c)
	{
	    if (__LtDebugPrintWidgetID())
	    {
		fprintf(_LtDebugFile, "%s %s [%p] (child %s [%p]): ",
			w->core.widget_class->core_class.class_name,
			XtName(w), w, XtName(c), c);
	    }
	    else
	    {
		fprintf(_LtDebugFile, "%s %s (child %s): ",
			w->core.widget_class->core_class.class_name,
			XtName(w), XtName(c));
	    }
	}
	else if (w)
	{
	    if (__LtDebugPrintWidgetID())
	    {
		fprintf(_LtDebugFile, "%s %s [%p] (child NULL): ",
			w->core.widget_class->core_class.class_name,
			XtName(w), w);
	    }
	    else
	    {
		fprintf(_LtDebugFile, "%s %s (child NULL): ",
			w->core.widget_class->core_class.class_name,
			XtName(w));
	    }
	}
	else
	{
	    fprintf(_LtDebugFile, "(null widget): ");
	}

	va_start(ap, fmt);
	vfprintf(_LtDebugFile, fmt, ap);
	va_end(ap);
    }
#endif /* !LESSTIF_PRODUCTION */
}


extern void 
_LtDebug0(const char *fn, Widget w, const char *fmt, ...)
{
#ifndef LESSTIF_PRODUCTION
    va_list ap;
    
    if (_LtDebugInit() && ValidateSource(fn))
    {
	va_start(ap, fmt);
	vfprintf(_LtDebugFile, fmt, ap);
	va_end(ap);
    }
#endif /* !LESSTIF_PRODUCTION */
}


/* amai: why does it take a widget here as an argument; any good
         reason for it?? */
extern Boolean 
_LtDebugInDebug(const char *fn, Widget w)
{
#ifdef LESSTIF_PRODUCTION
    return False;
#else
    return ValidateSource(fn);
#endif
}


extern const char *
_LtDebugState(Widget w)
{
    if (XtIsRealized(w))
    {
	if (XtIsManaged(w))
	{
	    return "realized, managed";
	}
	else
	{
	    return "realized, not managed";
	}
    }
    else
    {
	if (XtIsManaged(w))
	{
	    return "not realized, managed";
	}
	else
	{
	    return "not realized, not managed";
	}
    }
}


/* extern interface to turn on/off debugging output */
extern void
_LtDebugSet(Boolean flag)
{
#ifndef LESSTIF_PRODUCTION
  if (flag)
     _LtDebugFlag = True;
  else
     _LtDebugFlag = False;
#endif
}


/* In rare circumstances when one can't afford to code in
   arguments to a function call for _LtSetDebug() here's
   an alternative call */
extern void
_LtDebugToggle(void)
{
#ifndef LESSTIF_PRODUCTION
   _LtDebugFlag = !_LtDebugFlag;
#endif
}


/* Allow user to query the state of Debugging System */
extern Boolean
_LtDebugQueryState(void)
{
#ifndef LESSTIF_PRODUCTION
  return _LtDebugFlag;
#else
  return False;
#endif
}


#ifndef LESSTIF_PRODUCTION
static Boolean
__LtDebugPrintWidgetID(void)
{
    static const char *env;
    static int checked=False;

    if (!checked) {
       env = getenv("DEBUG_PRINT_WIDGETID");
       checked=True;
    }
    if (env)
       return True;
    else
       return False;
}


static void 
__LtDebugPrintTree(Widget w, int level)
{
    int i;
    Cardinal c;
    CompositeWidget cw = (CompositeWidget)w;

    if (w == NULL)
    {
	return;
    }

    _LtDebugOpenFile();

    for (i = 0; i < level; i++)
    {
	fprintf(_LtDebugFile, "\t");
    }

    fprintf(_LtDebugFile, "%s : %p/%ld", XtName(w), w, XtWindow(w));
    fprintf(_LtDebugFile, "(%s) geo %d %d %d %d",
	    w->core.widget_class->core_class.class_name,
	    XtX(w), XtY(w), XtWidth(w), XtHeight(w));
#ifdef PRINT_STATE
    fprintf(_LtDebugFile, " state: %s %s",
	    _LtDebugState(w), w->core.mapped_when_managed ? "mwm" : "");
#endif
    fprintf(_LtDebugFile, "\n");
    if (XtIsSubclass(w, compositeWidgetClass))
    {
	for (c = 0; c < cw->composite.num_children; c++)
	{
	    __LtDebugPrintTree(cw->composite.children[c], level + 1);
	}
    }

    for (c = 0; c < cw->core.num_popups; c++)
    {
	__LtDebugPrintTree(cw->core.popup_list[c], level + 1);
    }
}
#endif


extern void 
_LtDebugPrintTree(Widget w)
{
#ifndef LESSTIF_PRODUCTION
    __LtDebugPrintTree(w, 0);
#endif
}


extern void 
_LtDebugPrintCompleteTree(Widget w)
{
#ifndef LESSTIF_PRODUCTION
    Widget ww = w;

    while (ww)
    {
	w = ww;
	ww = XtParent(w);
    }

    __LtDebugPrintTree(w, 0);
#endif
}

extern const char *
_LtDebugDeleteResponse2String(int d)
{
	switch (d) {
	case XmDESTROY:
		return "XmDESTROY";
	case XmUNMAP:
		return "XmUNMAP";
	case XmDO_NOTHING:
		return "XmDO_NOTHING";
	default:
		return "??";
	}
}

extern void
_LtDebugPrintArgList(const char *fn, Widget w, ArgList al, int n, Boolean Get)
{
#ifndef LESSTIF_PRODUCTION
    int i;
    unsigned num;

    if (_LtDebugFlag && ValidateSource(fn))
    {
	_LtDebugOpenFile();

	for (i = 0; i < n; i++)
	{
	    int at;

	    for (at = 0; _LtDebugTypes[at].name; at++)
	    {
		if (strcmp(al[i].name, _LtDebugTypes[at].name) == 0)
		{
		    break;
		}
	    }

	    if (_LtDebugTypes[at].name == NULL)
	    {
		fprintf(_LtDebugFile, "Arg[%d] : %s (not handled FIX ME)\n", i, al[i].name);
		continue;
	    }

	    switch (_LtDebugTypes[at].t)
	    {
	    case _LtDebugNONE:
		fprintf(_LtDebugFile, "Arg[%d] : %s\n", i, al[i].name);
		break;

	    case _LtDebugINT:
		fprintf(_LtDebugFile, "Arg[%d] : %s %d\n", i, al[i].name,
			(Get) ? *(int *)al[i].value
			: (int)al[i].value);
		break;

	    case _LtDebugSHORT:
		fprintf(_LtDebugFile, "Arg[%d] : %s %d\n", i, al[i].name,
			(Get) ? *(short *)al[i].value
			: (short)al[i].value);
		break;

	    case _LtDebugSTRING:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name, (char *)al[i].value);
		break;

	    case _LtDebugXMSTRING:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			_LtDebugXmString2String((XmString)al[i].value));
		break;

	    case _LtDebugCHAR:
		fprintf(_LtDebugFile, "Arg[%d] : %s %c\n", i, al[i].name, (char)al[i].value);
		break;

	    case _LtDebugALIGNMENT:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugAlignment2String(*(unsigned char *)al[i].value)
			: _LtDebugAlignment2String((unsigned char)al[i].value));
		break;

	    case _LtDebugRESIZE_POLICY:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugResizePolicy2String(*(unsigned char *)al[i].value)
			: _LtDebugResizePolicy2String((unsigned char)al[i].value));
		break;

	    case _LtDebugSB_DISPLAY_POLICY:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugSBDisplayPolicy2String(*(unsigned char *)al[i].value)
			: _LtDebugSBDisplayPolicy2String((unsigned char)al[i].value));
		break;

	    case _LtDebugDRAG_TYPE:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugDragType2String(*(unsigned char *)al[i].value)
			: _LtDebugDragType2String((unsigned char)al[i].value));
		break;

	    case _LtDebugSCROLLING_POLICY:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugScrollingPolicy2String(*(unsigned char *)al[i].value)
			: _LtDebugScrollingPolicy2String((unsigned char)al[i].value));
		break;

	    case _LtDebugSB_PLACEMENT:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugSBPlacement2String(*(unsigned char *)al[i].value)
			: _LtDebugSBPlacement2String((unsigned char)al[i].value));
		break;

	    case _LtDebugLIST_SIZE_POLICY:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugListSizePolicy2String(*(unsigned char *)al[i].value)
			: _LtDebugListSizePolicy2String((unsigned char)al[i].value));
		break;

	    case _LtDebugATTACHMENT:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugAttachment2String(*(unsigned char *)al[i].value)
			: _LtDebugAttachment2String((unsigned char)al[i].value));
		break;

	    case _LtDebugDIALOG_STYLE:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
		    (Get) ? _LtDebugDialogStyle2String(*(unsigned char *)al[i].value)
			: _LtDebugDialogStyle2String((unsigned char)al[i].value));
		break;

	    case _LtDebugWIDGET:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			(Get)
			? ((al[i].value && *(Widget *)al[i].value)
			   ? XtName(*(Widget *)al[i].value)
			   : "(null)")
			: (al[i].value
			   ? XtName((Widget)al[i].value)
			   : "(null)"));
		break;

	    case _LtDebugEDIT_MODE:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			(Get) ? _LtDebugEditMode2String(*(Boolean *)al[i].value)
			: _LtDebugEditMode2String((Boolean)al[i].value));
		break;

	    case _LtDebugBOOLEAN:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			(Get) ? _LtDebugBoolean2String(*(Boolean *)al[i].value)
			: _LtDebugBoolean2String((Boolean)al[i].value));
		break;

	    case _LtDebugSELECTION_POLICY:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			_LtDebugSelectionPolicy2String(al[i].value));
		break;

	    case _LtDebugXMSTRING_LIST:	/* Need related info !! */
		num = 0xdeadbeef;

		XtVaGetValues(w, _LtDebugTypes[at].related, &num, NULL);

		if (num != 0xdeadbeef)
		{
		    int j;

		    fprintf(_LtDebugFile, "Arg[%d] : %s(%d):\n", i, al[i].name, num);
		    for (j = 0; j < (int)num; j++)
		    {
			fprintf(_LtDebugFile, "\tItem %d '%s'\n", j,
			    _LtDebugXmString2String(((XmString *)(al[i].value))[j]));
		    }
		}

		break;

	    case _LtDebugMWM_INPUT_MODE:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			(Get) ? _LtDebugMwmInput2String(*(int *)al[i].value)
			: _LtDebugMwmInput2String((int)al[i].value));
		break;

	    case _LtDebugDELETE_RESPONSE:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			(Get) ? _LtDebugDeleteResponse2String(*(int *)al[i].value) :
			_LtDebugDeleteResponse2String((int)al[i].value));
		break;

	    case _LtDebugCOMBOBOX_TYPE:
		fprintf(_LtDebugFile, "Arg[%d] : %s %s\n", i, al[i].name,
			_LtDebugComboBoxType2String(al[i].value));
		break;

	    case _LtDebugRENDERTABLE:
		{
			XmRenderTable	rt = (XmRenderTable)al[i].value;
			char		s[80];
			sprintf(s, "Arg[%d] : %s", i, al[i].name);
			_LtDebugPrintRenderTable(fn, w, s, rt);
		}
		break;

	    }
	}
    }
#endif/* !LESSTIF_PRODUCTION */
}

void
_LtDebugPrintRenderTable(const char *fn, Widget w, char *s, XmRenderTable rt)
{
#ifndef LESSTIF_PRODUCTION
	int		x;

	if (_LtDebugFlag && ValidateSource(fn)) {
		_LtDebugOpenFile();
		fprintf(_LtDebugFile, "%s %p (%d renditions)\n", s, rt, rt->count);

		for (x=0; x<rt->count; x++) {
			switch (rt->renditions[x]->type) {
			case XmFONT_IS_FONT:
				fprintf(_LtDebugFile, "\t%d - %s [%s], %p\n",
					x,
					"XmFONT_IS_FONT",
					rt->renditions[x]->font_name ?
					rt->renditions[x]->font_name : "(null font name)",
					rt->renditions[x]->font);
				break;
			case XmFONT_IS_FONTSET:
				fprintf(_LtDebugFile, "\t%d - %s [%s], %p\n",
					x,
					"XmFONT_IS_FONTSET",
					rt->renditions[x]->font_name ?
					rt->renditions[x]->font_name : "(null font name)",
					rt->renditions[x]->font);
				break;
#ifdef	USE_XFT
			case XmFONT_IS_XFT:
				fprintf(_LtDebugFile, "\t%d - %s [%s/%s/%s/%s,%d], %p\n",
					x,
					"XmFONT_IS_XFT",
					rt->renditions[x]->font_name,
					rt->renditions[x]->font_style,
					rt->renditions[x]->font_foundry,
					rt->renditions[x]->font_encoding,
					rt->renditions[x]->font_size,
					rt->renditions[x]->font);
				break;
#endif
#ifdef	USE_BIDI
			case XmFONT_IS_XOC:
				fprintf(_LtDebugFile, "\t%d - %s\n",
					x,
					"XmFONT_IS_XOC");
				break;
#endif
			default:
				break;
			}
		}
	}
#endif/* !LESSTIF_PRODUCTION */
}

/*
 * Allow unconditional printing to the _LtDebugFile
 */
extern void
_LtDebugPrintString(const char *s)
{
#ifndef LESSTIF_PRODUCTION
    _LtDebugOpenFile();
    fprintf(_LtDebugFile, "%s", s);
#endif
}



/* The following calls shouldn't depend on the complete debugging 
   subsystem, i.e. things like _LtDebugFile, etc. */

extern const char *
_LtDebugComboBoxType2String(unsigned char type)
{
    switch(type)
    {
    case XmDROP_DOWN_LIST:
    	return("XmDROP_DOWN_LIST");
    case XmDROP_DOWN_COMBO_BOX:
    	return("XmDROP_DOWN_COMBO_BOX");
    case XmCOMBO_BOX:
    	return("XmCOMBO_BOX");
    default:
	return("UNKNOWN");
    }
}

extern const char *
_LtDebugGeoAction2String(int action)
{
	switch(action)
	{
	case XmGET_ACTUAL_SIZE:
		return("XmGET_ACTUAL_SIZE");
	case XmGET_PREFERRED_SIZE:
		return("XmGET_PREFERRED_SIZE");
	case XmGEO_PRE_SET:
		return("XmGEO_PRE_SET");
	case XmGEO_POST_SET:
		return("XmGEO_POST_SET");
	default:
		return("Unknown geo action");
	}
}


extern const char *
_LtDebugGeometryResult2String(XtGeometryResult r)
{
    switch (r)
    {
    case XtGeometryYes:
	return "Yes";

    case XtGeometryNo:
	return "No";

    case XtGeometryAlmost:
	return "Almost";

    case XtGeometryDone:
	return "Done";

    default:
	return "(invalid geometry result)";
    }
}


extern const char *
_LtDebugDragAndDropMessageType2String(unsigned char r)
{
    switch (r)
    {
    case XmTOP_LEVEL_ENTER:
	return "TOP_LEVEL_ENTER";

    case XmTOP_LEVEL_LEAVE:
	return "TOP_LEVEL_LEAVE";

    case XmDRAG_MOTION:
	return "DRAG_MOTION";

    case XmDROP_SITE_ENTER:
	return "DROP_SITE_ENTER";

    case XmDROP_SITE_LEAVE:
	return "DROP_SITE_LEAVE";

    case XmDROP_START:
	return "DROP_START";

    case XmDROP_FINISH:
	return "DROP_FINISH";

    case XmDRAG_DROP_FINISH:
	return "DRAG_DROP_FINISH";

    case XmOPERATION_CHANGED:
	return "OPERATION_CHANGED";

    default:
	return "UNKNOWN";
    }
}


extern const char *
_LtDebugDragType2String(unsigned char r)
{
    switch (r)
    {
    case XmDRAG_NONE:
	return "XmDRAG_NONE";

    case XmDRAG_DROP_ONLY:
	return "XmDRAG_DROP_ONLY";

    case XmDRAG_PREFER_PREREGISTER:
	return "XmDRAG_PREFER_PREREGISTER";

    case XmDRAG_PREREGISTER:
	return "XmDRAG_PREREGISTER";

    case XmDRAG_PREFER_DYNAMIC:
	return "XmDRAG_PREFER_DYNAMIC";

    case XmDRAG_PREFER_RECEIVER:
	return "XmDRAG_PREFER_RECEIVER";

    case XmDRAG_DYNAMIC:
	return "XmDRAG_DYNAMIC";

    default:
	return "UNKNOWN";
    }
}


extern const char *
_LtDebugScrollingPolicy2String(unsigned char r)
{
    switch (r)
    {
    case XmAUTOMATIC:
	return "XmAUTOMATIC";

    case XmCONSTANT:
	return "XmCONSTANT";

    default:
	return "UNKNOWN";
    }
}

extern const char *
_LtDebugMwmInput2String(int a)
{
    switch (a)
    {
    case MWM_INPUT_MODELESS:
	return "MWM_INPUT_MODELESS";

    case MWM_INPUT_PRIMARY_APPLICATION_MODAL:
	return "MWM_INPUT_PRIMARY_APPLICATION_MODAL or MWM_INPUT_APPLICATION_MODAL";

    case MWM_INPUT_FULL_APPLICATION_MODAL:
	return "MWM_INPUT_FULL_APPLICATION_MODAL";

    case MWM_INPUT_SYSTEM_MODAL:
	return "MWM_INPUT_SYSTEM_MODAL";
#if 0
    case MWM_INPUT_APPLICATION_MODAL:
	return "MWM_INPUT_APPLICATION_MODAL";
#endif
    default:
	return "(invalid input style)";
    }
}

extern const char *
_LtDebugDialogStyle2String(int a)
{
    switch (a)
    {
    case XmDIALOG_WORK_AREA:
	return "XmDIALOG_WORK_AREA or XmDIALOG_MODELESS";

    /*
    case XmDIALOG_MODELESS:
	return "XmDIALOG_MODELESS";
	*/

    case XmDIALOG_PRIMARY_APPLICATION_MODAL:
	return "XmDIALOG_PRIMARY_APPLICATION_MODAL or XmDIALOG_APPLICATION_MODAL";

    case XmDIALOG_FULL_APPLICATION_MODAL:
	return "XmDIALOG_FULL_APPLICATION_MODAL";

    case XmDIALOG_SYSTEM_MODAL:
	return "XmDIALOG_SYSTEM_MODAL";

    /*
    case XmDIALOG_APPLICATION_MODAL:
	return "XmDIALOG_APPLICATION_MODAL";
	*/

    default:
	return "(invalid dialog style)";
    }
}


extern const char *
_LtDebugAttachment2String(int a)
{
    switch (a)
    {
    case XmATTACH_FORM:
	return "XmATTACH_FORM";

    case XmATTACH_OPPOSITE_FORM:
	return "XmATTACH_OPPOSITE_FORM";

    case XmATTACH_WIDGET:
	return "XmATTACH_WIDGET";

    case XmATTACH_OPPOSITE_WIDGET:
	return "XmATTACH_OPPOSITE_WIDGET";

    case XmATTACH_NONE:
	return "XmATTACH_NONE";

    case XmATTACH_POSITION:
	return "XmATTACH_POSITION";

    case XmATTACH_SELF:
	return "XmATTACH_SELF";

    default:
	return "(invalid attachment)";
    }
}


extern const char *
_LtDebugMenuFocusOp2String(int f)
{
    switch (f)
    {
    case XmMENU_FOCUS_SAVE:
    	return ("XmMENU_FOCUS_SAVE");
    case XmMENU_FOCUS_RESTORE:
    	return ("XmMENU_FOCUS_RESTORE");
    case XmMENU_FOCUS_SET:
    	return ("XmMENU_FOCUS_SET");
    default:
    	return("Unknown focus op");
    }
}


extern const char *
_LtDebugMenuEnum2String(int f)
{
    switch (f)
    {
    case XmMENU_POPDOWN:
	return "XmMENU_POPDOWN";

    case XmMENU_PROCESS_TREE:
	return "XmMENU_PROCESS_TREE";

    case XmMENU_TRAVERSAL:
	return "XmMENU_TRAVERSAL";

    case XmMENU_SHELL_POPDOWN:
	return "XmMENU_SHELL_POPDOWN";

    case XmMENU_CALLBACK:
	return "XmMENU_CALLBACK";

    case XmMENU_BUTTON:
	return "XmMENU_BUTTON";

    case XmMENU_CASCADING:
	return "XmMENU_CASCADING";

    case XmMENU_SUBMENU:
	return "XmMENU_SUBMENU";

    case XmMENU_ARM:
	return "XmMENU_ARM";

    case XmMENU_DISARM:
	return "XmMENU_DISARM";

    case XmMENU_BAR_CLEANUP:
	return "XmMENU_BAR_CLEANUP";

    case XmMENU_STATUS:
	return "XmMENU_STATUS";

    case XmMENU_MEMWIDGET_UPDATE:
	return "XmMENU_MEMWIDGET_UPDATE";

    case XmMENU_BUTTON_POPDOWN:
	return "XmMENU_BUTTON_POPDOWN";

    case XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL:
	return "XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL";

    case XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL:
	return "XmMENU_RESTORE_TEAROFF_TO_TOPLEVEL_SHELL";

    case XmMENU_RESTORE_TEAROFF_TO_MENUSHELL:
	return "XmMENU_RESTORE_TEAROFF_TO_MENUSHELL";

    case XmMENU_GET_LAST_SELECT_TOPLEVEL:
	return "XmMENU_GET_LAST_SELECT_TOPLEVEL";

    case XmMENU_TEAR_OFF_ARM:
	return "XmMENU_TEAR_OFF_ARM";

    default:
	return "??";
    }
}


extern const char *
_LtDebugBoolean2String(Boolean b)
{
    if (b)
	return "True";
    else
	return "False";
}


extern const char *
_LtDebugXmString2String(XmString xms)
{
  static char *s = NULL;

    if (s) {
      XtFree(s);
      s=NULL;
    }
    if (xms == (XmString)XmUNSPECIFIED)
    {
	return "XmUNSPECIFIED";
    }
    if ( ! XmStringGetLtoR(xms, XmFONTLIST_DEFAULT_TAG, &s) || s == NULL )
    {
	return "(null)";
    }

    return s;
}


extern const char *
_LtDebugPacking2String(unsigned char p)
{
    static char res[40];

    switch (p)
    {
    case XmPACK_COLUMN:
	return "XmPACK_COLUMN";

    case XmPACK_TIGHT:
	return "XmPACK_TIGHT";

    case XmPACK_NONE:
	return "XmPACK_NONE";

    default:
	sprintf(res, "Invalid packing %d", p);
	return res;
    }
}


extern const char *
_LtDebugRcType2String(unsigned char t)
{
    static char res[40];

    switch (t)
    {
    case XmWORK_AREA:
	return "XmWORK_AREA";

    case XmMENU_BAR:
	return "XmMENU_BAR";

    case XmMENU_PULLDOWN:
	return "XmMENU_PULLDOWN";

    case XmMENU_POPUP:
	return "XmMENU_POPUP";

    case XmMENU_OPTION:
	return "XmMENU_OPTION";

    default:
	sprintf(res, "Invalid RC Type %d", t);
	return res;
    }
}


extern const char *
_LtDebugWidgetGeometry2String(XtWidgetGeometry *g)
{
    static char o1[128], o2[128], b[20];
    static char  *out = NULL;
    int i;

    if (g == NULL)
    {
	return "NULL_GEOMETRY";
    }

    if (g->request_mode == 0)
    {
	return "GEOMETRY_NO_FIELDS";
    }

/* Some magic to ensure you can call this sucker twice in one C function call */
    if (out == &o1[0])
    {
	out = &o2[0];
    }
    else
    {
	out = &o1[0];
    }

    out[0] = '\0';
#if 0
    {
	char	t[16];
	strcat(out, "{");
	sprintf(t, "%d,", g->request_mode);
	strcat(out, t);
	if (g->request_mode & CWX) strcat(out, "x");
	if (g->request_mode & CWY) strcat(out, "y");
	if (g->request_mode & CWWidth) strcat(out, "w");
	if (g->request_mode & CWHeight) strcat(out, "h");
	if (g->request_mode & CWBorderWidth) strcat(out, "b");
	strcat(out, "} ");
    }
#endif

    if (g->request_mode & CWX)
    {
	sprintf(b, "x %d ", g->x);
	strcat(out, b);
    }
    if (g->request_mode & CWY)
    {
	sprintf(b, "y %d ", g->y);
	strcat(out, b);
    }
    if (g->request_mode & CWWidth)
    {
	sprintf(b, "w %d ", g->width);
	strcat(out, b);
    }
    if (g->request_mode & CWHeight)
    {
	sprintf(b, "h %d ", g->height);
	strcat(out, b);
    }
    if (g->request_mode & CWBorderWidth)
    {
	sprintf(b, "bw %d ", g->border_width);
	strcat(out, b);
    }

    for (i = 0; out[i]; i++)
    {
    }

    if (i > 0 && out[i - 1] == ' ')
    {
	out[i - 1] = '\0';
    }

    return out;
}


extern const char *
_LtDebugEditMode2String(int n)
{
    switch (n)
    {
    case XmMULTI_LINE_EDIT:
	return "XmMULTI_LINE_EDIT";

    case XmSINGLE_LINE_EDIT:
	return "XmSINGLE_LINE_EDIT";

    default:
	return "???";
    }
}


extern const char *
_LtDebugSelectionPolicy2String(int n)
{
    switch (n)
    {
    case XmSINGLE_SELECT:
	return "XmSINGLE_SELECT";

    case XmBROWSE_SELECT:
	return "XmBROWSE_SELECT";

    case XmMULTIPLE_SELECT:
	return "XmMULTIPLE_SELECT";

    case XmEXTENDED_SELECT:
	return "XmEXTENDED_SELECT";

    default:
	return "???";
    }
}


extern const char *
_LtDebugResizePolicy2String(int n)
{
	switch (n) {
	case XmRESIZE_NONE:		return "XmRESIZE_NONE";
	case XmRESIZE_GROW:		return "XmRESIZE_GROW";
	case XmRESIZE_ANY:		return "XmRESIZE_ANY";
	case XmRESIZE_SWINDOW:		return "XmRESIZE_SWINDOW";
	default:			return "XmNscrollBarDisplayPolicy - illegal";
	}
}


extern const char *
_LtDebugSBDisplayPolicy2String(int n)
{
    switch (n)
    {
    case XmSTATIC:
	return "XmSTATIC";

    case XmAS_NEEDED:
	return "XmAS_NEEDED";

    default:
	return "XmNscrollBarDisplayPolicy - illegal";
    }
}


extern const char *
_LtDebugSBPlacement2String(int n)
{
    switch (n)
    {
    case XmTOP_LEFT:
	return "XmTOP_LEFT";

    case XmBOTTOM_LEFT:
	return "XmBOTTOM_LEFT";

    case XmTOP_RIGHT:
	return "XmTOP_RIGHT";

    case XmBOTTOM_RIGHT:
	return "XmBOTTOM_RIGHT";

    default:
	return "XmNscrollBarPlacement - illegal";
    }
}


extern const char *
_LtDebugListSizePolicy2String(int n)
{
    switch (n)
    {
    case XmVARIABLE:
	return "XmVARIABLE";

    case XmCONSTANT:
	return "XmCONSTANT";

    case XmRESIZE_IF_POSSIBLE:
	return "XmRESIZE_IF_POSSIBLE";

    default:
	return "XmNlistSizePolicy - illegal";
    }
}


extern const char *
_LtDebugAlignment2String(int n)
{
    switch (n)
    {
    case XmALIGNMENT_BEGINNING:
	return "XmALIGNMENT_BEGINNING";

    case XmALIGNMENT_CENTER:
	return "XmALIGNMENT_CENTER";

    case XmALIGNMENT_END:
	return "XmALIGNMENT_END";

    default:
	return "XmALIGNMENT - illegal";
    }
}


extern const char *
_LtDebugMenuType2String(int n)
{
    switch (n)
    {
    case XmMENU_OPTION:
	return "XmMENU_OPTION";

    case XmMENU_POPUP:
	return "XmMENU_POPUP";

    case XmMENU_PULLDOWN:
	return "XmMENU_PULLDOWN";

    default:
	return "???";
    }
}


extern const char *
_LtDebugNavigability2String(unsigned char n)
{
    switch (n)
    {
    case XmDESCENDANTS_NAVIGABLE:
	return "XmDESCENDANTS_NAVIGABLE";

    case XmDESCENDANTS_TAB_NAVIGABLE:
	return "XmDESCENDANTS_TAB_NAVIGABLE";

    case XmCONTROL_NAVIGABLE:
	return "XmCONTROL_NAVIGABLE";

    case XmNOT_NAVIGABLE:
	return "XmNOT_NAVIGABLE";

    case XmTAB_NAVIGABLE:
	return "XmTAB_NAVIGABLE";

    default:
	return "???";
    }
}


extern const char *
_LtDebugHighlightMode2String(int mode)
{
    switch (mode)
    {
    case XmHIGHLIGHT_NORMAL:
	return "NORMAL";

    case XmHIGHLIGHT_SELECTED:
	return "SELECTED";

    case XmHIGHLIGHT_SECONDARY_SELECTED:
	return "SECONDARY_SELECTED";

    default:
	return "???";
    }
}


extern const char *
_LtDebugReason2String(int reason)
{
	switch (reason) {
	case XmCR_NONE:	                   return "XmCR_NONE";
	case XmCR_HELP:	                   return "XmCR_HELP";
	case XmCR_VALUE_CHANGED:           return "XmCR_VALUE_CHANGED";
	case XmCR_INCREMENT:               return "XmCR_INCREMENT";
	case XmCR_DECREMENT:               return "XmCR_DECREMENT";
	case XmCR_PAGE_INCREMENT:          return "XmCR_PAGE_INCREMENT";
	case XmCR_PAGE_DECREMENT:          return "XmCR_PAGE_DECREMENT";
	case XmCR_TO_TOP:                  return "XmCR_TO_TOP";
	case XmCR_TO_BOTTOM:               return "XmCR_TO_BOTTOM";
	case XmCR_DRAG:                    return "XmCR_DRAG";
	case XmCR_ACTIVATE:                return "XmCR_ACTIVATE";
	case XmCR_ARM:                     return "XmCR_ARM";
	case XmCR_DISARM:                  return "XmCR_DISARM";
	case XmCR_DUMMY13:                 return "XmCR_DUMMY13";
	case XmCR_DUMMY14:                 return "XmCR_DUMMY14";
	case XmCR_DUMMY15:                 return "XmCR_DUMMY15";
	case XmCR_MAP:                     return "XmCR_MAP";
	case XmCR_UNMAP:                   return "XmCR_UNMAP";
	case XmCR_FOCUS:                   return "XmCR_FOCUS";
	case XmCR_LOSING_FOCUS:            return "XmCR_LOSING_FOCUS";
	case XmCR_MODIFYING_TEXT_VALUE:    return "XmCR_MODIFYING_TEXT_VALUE";
	case XmCR_MOVING_INSERT_CURSOR:    return "XmCR_MOVING_INSERT_CURSOR";
	case XmCR_EXECUTE:                 return "XmCR_EXECUTE";
	case XmCR_SINGLE_SELECT:           return "XmCR_SINGLE_SELECT";
	case XmCR_MULTIPLE_SELECT:         return "XmCR_MULTIPLE_SELECT";
	case XmCR_EXTENDED_SELECT:         return "XmCR_EXTENDED_SELECT";
	case XmCR_BROWSE_SELECT:           return "XmCR_BROWSE_SELECT";
	case XmCR_DEFAULT_ACTION:          return "XmCR_DEFAULT_ACTION";
	case XmCR_CLIPBOARD_DATA_REQUEST:  return "XmCR_CLIPBOARD_DATA_REQUEST";
	case XmCR_CLIPBOARD_DATA_DELETE:   return "XmCR_CLIPBOARD_DATA_DELETE";
	case XmCR_CASCADING:               return "XmCR_CASCADING";
	case XmCR_OK:                      return "XmCR_OK";
	case XmCR_CANCEL:                  return "XmCR_CANCEL";
	case XmCR_DUMMY33:                 return "XmCR_DUMMY33";
	case XmCR_APPLY:                   return "XmCR_APPLY";
	case XmCR_NO_MATCH:                return "XmCR_NO_MATCH";
	case XmCR_COMMAND_ENTERED:         return "XmCR_COMMAND_ENTERED";
	case XmCR_COMMAND_CHANGED:         return "XmCR_COMMAND_CHANGED";
	case XmCR_EXPOSE:                  return "XmCR_EXPOSE";
	case XmCR_RESIZE:                  return "XmCR_RESIZE";
	case XmCR_INPUT:                   return "XmCR_INPUT";
	case XmCR_GAIN_PRIMARY:            return "XmCR_GAIN_PRIMARY";
	case XmCR_LOSE_PRIMARY:            return "XmCR_LOSE_PRIMARY";
	case XmCR_CREATE:                  return "XmCR_CREATE";
	case XmCR_TEAR_OFF_ACTIVATE:       return "XmCR_TEAR_OFF_ACTIVATE";
	case XmCR_TEAR_OFF_DEACTIVATE:     return "XmCR_TEAR_OFF_DEACTIVATE";
	case XmCR_OBSCURED_TRAVERSAL:      return "XmCR_OBSCURED_TRAVERSAL";
	case XmCR_FOCUS_MOVED:             return "XmCR_FOCUS_MOVED";
	case XmCR_DUMMY48:                 return "XmCR_DUMMY48";
	case XmCR_DUMMY49:                 return "XmCR_DUMMY49";
	case XmCR_DUMMY50:                 return "XmCR_DUMMY50";
	case XmCR_DUMMY51:                 return "XmCR_DUMMY51";
	case XmCR_DUMMY52:                 return "XmCR_DUMMY52";
	case XmCR_DUMMY53:                 return "XmCR_DUMMY53";
	case XmCR_REPOST:                  return "XmCR_REPOST";
	case XmCR_COLLAPSED:               return "XmCR_COLLAPSED";
	case XmCR_EXPANDED:                return "XmCR_EXPANDED";
	case XmCR_SELECT:                  return "XmCR_SELECT";
	case XmCR_DRAG_START:              return "XmCR_DRAG_START";
	case XmCR_NO_FONT:                 return "XmCR_NO_FONT";
	case XmCR_NO_RENDITION:            return "XmCR_NO_RENDITION";
	case XmCR_POST:                    return "XmCR_POST";
	case XmCR_SPIN_NEXT:               return "XmCR_SPIN_NEXT";
	case XmCR_SPIN_PRIOR:              return "XmCR_SPIN_PRIOR";
	case XmCR_SPIN_FIRST:              return "XmCR_SPIN_FIRST";
	case XmCR_SPIN_LAST:               return "XmCR_SPIN_LAST";
	case XmCR_PAGE_SCROLLER_INCREMENT: return "XmCR_PAGE_SCROLLER_INCREMENT";
	case XmCR_PAGE_SCROLLER_DECREMENT: return "XmCR_PAGE_SCROLLER_DECREMENT";
	case XmCR_MAJOR_TAB:               return "XmCR_MAJOR_TAB";
	case XmCR_MINOR_TAB:               return "XmCR_MINOR_TAB";
	case XmCR_PDM_NONE:                return "XmCR_PDM_NONE";
	case XmCR_PDM_START_VXAUTH:        return "XmCR_PDM_START_VXAUTH";
	case XmCR_PDM_START_PXAUTH:        return "XmCR_PDM_START_PXAUTH";
	case XmCR_PDM_UP:                  return "XmCR_PDM_UP";
	case XmCR_PDM_OK:                  return "XmCR_PDM_OK";
	case XmCR_PDM_CANCEL:              return "XmCR_PDM_CANCEL";
	case XmCR_PDM_START_ERROR:         return "XmCR_PDM_START_ERROR";
	case XmCR_PDM_EXIT_ERROR:          return "XmCR_PDM_EXIT_ERROR";
	case XmCR_PROTOCOLS:               return "XmCR_PROTOCOLS";
	default:                           return "???";
	}
}


extern const char *
_LtDebugFocusChange2String(XmFocusChange c)
{
	switch (c) {
	case XmFOCUS_IN:        return "XmFOCUS_IN";
	case XmFOCUS_OUT:       return "XmFOCUS_OUT";
	case XmENTER:           return "XmENTER";
	case XmLEAVE:           return "XmLEAVE";
	default:                return "???";
	}
}


extern const char *
_LtDebugNavigationType2String(XmNavigationType nt)
{
	switch (nt) {
	case XmNONE:                 return "XmNONE";
	case XmTAB_GROUP:            return "XmTAB_GROUP";
	case XmSTICKY_TAB_GROUP:     return "XmSTICKY_TAB_GROUP";
	case XmEXCLUSIVE_TAB_GROUP:  return "XmEXCLUSIVE_TAB_GROUP";
	default:                     return "???";
	}
}


extern const char *
_LtDebugEventType2String(int type)
{
	switch (type)
	{
	case KeyPress:          return("KeyPress");
	case KeyRelease:        return("KeyRelease");
	case ButtonPress:       return("ButtonPress");
	case ButtonRelease:     return("ButtonRelease");
	case KeymapNotify:      return("KeymapNotify");
	case MotionNotify:      return("MotionNotify");
	case EnterNotify:       return("EnterNotify");
	case LeaveNotify:       return("LeaveNotify");
	case FocusIn:           return("FocusIn");
	case FocusOut:          return("FocusOut");
	case Expose:            return("Expose");
	case GraphicsExpose:    return("GraphicsExpose");
	case NoExpose:          return("NoExpose");
	case ColormapNotify:    return("ColormapNotify");
	case PropertyNotify:    return("PropertyNotify");
	case VisibilityNotify:  return("VisibilityNotify");
	case ResizeRequest:     return("ResizeRequest");
	case CirculateNotify:   return("CirculateNotify");
	case ConfigureNotify:   return("ConfigureNotify");
	case DestroyNotify:     return("DestroyNotify");
	case GravityNotify:     return("GravityNotify");
	case MapNotify:         return("MapNotify");
	case ReparentNotify:    return("ReparentNotify");
	case UnmapNotify:       return("UnmapNotify");
	case CreateNotify:      return("CreateNotify");
	case CirculateRequest:  return("CirculateRequest");
	case ConfigureRequest:  return("ConfigureRequest");
	case MapRequest:        return("MapRequest");
	case MappingNotify:     return("MappingNotify");
	case ClientMessage:     return("ClientMessage");
	case SelectionClear:    return("SelectionClear");
	case SelectionNotify:   return("SelectionNotify");
	case SelectionRequest:  return("SelectionRequest");
	default:                return("UNKNOWN");
	}
}


extern const char *
_LtDebugFocusMode2String(int type)
{
	switch (type)
	{
	case NotifyNormal:        return("NotifyNormal");
	case NotifyGrab:	        return("NotifyGrab");
	case NotifyUngrab:        return("NotifyUngrab");
	case NotifyWhileGrabbed:  return("NotifyWhileGrabbed");
	default:                  return("UNKNOWN");
	}
}


extern const char *
_LtDebugFocusDetail2String(int type)
{
	switch (type)
	{
	case NotifyAncestor:          return("NotifyAncestor");
	case NotifyDetailNone:        return("NotifyDetailNone");
	case NotifyInferior:          return("NotifyInferior");
	case NotifyNonlinear:         return("NotifyNonlinear");
	case NotifyNonlinearVirtual:  return("NotifyNonlinearVirtual");
	case NotifyPointer:           return("NotifyPointer");
	case NotifyPointerRoot:       return("NotifyPointerRoot");
	case NotifyVirtual:           return("NotifyVirtual");
	default:                      return("UNKNOWN");
	}
}

extern const char *
_LtDebugFrameChildType2String(int action)
{
	switch(action)
	{
	case XmFRAME_GENERIC_CHILD:
		return("XmFRAME_GENERIC_CHILD");
	case XmFRAME_WORKAREA_CHILD:
		return("XmFRAME_WORKAREA_CHILD");
	case XmFRAME_TITLE_CHILD:
		return("XmFRAME_TITLE_CHILD");
	default:
		return("Unknown frame childtype");
	}
}

extern void
_LtDebugAction(const char *fn, Widget w, const String action,
		const String *params, const Cardinal *num_params)
{
#ifndef LESSTIF_PRODUCTION

	if (_LtDebugInit() && ValidateSource(fn)) {
  		int	i;

		if (w) {
			if (__LtDebugPrintWidgetID()) {
				fprintf(_LtDebugFile, "%s %s [%p]: ",
					w->core.widget_class->core_class.class_name,
					XtName(w), w);
	    		} else {
				fprintf(_LtDebugFile, "%s %s: ",
					w->core.widget_class->core_class.class_name, XtName(w));
	    		}
		} else {
			fprintf(_LtDebugFile, "(null widget): ");
		}

		fprintf(_LtDebugFile, "Action %s(", action);
		if (*num_params) {
			fprintf(_LtDebugFile, "%s", params[0]);
		}
		for (i=1; i<(int)*num_params; i++) {
			fprintf(_LtDebugFile, ", %s", params[i]);
		}
		fprintf(_LtDebugFile, ")\n");

		fflush(_LtDebugFile);
	}
#endif /* !LESSTIF_PRODUCTION */
}

/*
 * For use with dmalloc, a malloc debugging package.
 * Mimick Xt behaviour ...
 * NEVER call them directly!
 */

extern XtPointer
_LtDebugMalloc(const char *f, int l, Cardinal size)
{
    XtPointer r=NULL;
#ifdef WITH_DMALLOC
    if (size == 0)
    {
	size = 1;
    }

    r = _malloc_leap(f, l, size);

    if (r == NULL)
    {
	_XtAllocError("malloc");
    }
#endif
    return r;
}


extern XtPointer
_LtDebugCalloc(const char *f, int l, Cardinal count, Cardinal size)
{
    XtPointer p=NULL;
#ifdef WITH_DMALLOC
    if (!size)
    {
	count = size = 1;
    }

    p = _calloc_leap(f, l, count, size);

    if (p == NULL)
    {
	_XtAllocError("calloc");
    }
#endif
    return p;
}


extern XtPointer
_LtDebugRealloc(const char *f, int l, XtPointer p, Cardinal size)
{
    XtPointer r=NULL;
#ifdef WITH_DMALLOC
    if (p == NULL)
    {
	return _malloc_leap(f, l, size);
    }

    r = _realloc_leap(f, l, p, size);
    if (r == NULL)
    {
	_XtAllocError("realloc");
    }
#endif
    return r;
}

extern void
_LtDebugFree(const char *f, int l, XtPointer p)
{
#ifdef WITH_DMALLOC
    if (p)
    {
	_free_leap(f, l, p);
    }
#endif
}

/*
 * Only include this when necessary.
 * What is this really dependent on ?
 */
extern void
_LtDebugPrintStackTrace()
{
#ifndef	LESSTIF_PRODUCTION
#ifdef HAVE_BACKTRACE
	void	*trace[32];
	char	**messages = (char **)0;
	int	i, trace_size = 0;

	_LtDebugInit();
	trace_size = backtrace(trace, 32);
	messages = (char **)backtrace_symbols(trace, trace_size);
	fprintf(_LtDebugFile, "[bt] Stack trace\n");
	for (i=0; i<trace_size; i++)
		fprintf(_LtDebugFile, "\t[%s]\n", messages[i]);
	fflush(_LtDebugFile);
#endif
#endif
}

extern void
_LtDebugPrintManagedChildren(const char *fn, Widget w, const char *title)
{
#ifndef	LESSTIF_PRODUCTION
	int	i;

	_LtDebugInit();
	if (! ValidateSource(fn))
		return;

	for (i=0; i<MGR_NumChildren(w); i++) {
		Widget	c = MGR_Children(w)[i];
		fprintf(_LtDebugFile, "\t%s %d - %s - managed %d\n",
			title, i, XtName(c),
			XtIsManaged(c) ? 1 : 0);

	}
#endif
}

extern const char *
_LtDebugVisualPolicy2String(int vp)
{
	switch (vp) {
	case XmCONSTANT:		return "XmCONSTANT";
	case XmVARIABLE:		return "XmVARIABLE";
	case XmRESIZE_IF_POSSIBLE:	return "XmRESIZE_IF_POSSIBLE";
	default:			return "???";
	}
}

extern const char *
_LtDebugScrollPolicy2String(int vp)
{
	switch (vp) {
	case XmAPPLICATION_DEFINED:	return "XmAPPLICATION_DEFINED";
	case XmAUTOMATIC:		return "XmAUTOMATIC";
	default:			return "???";
	}
}

extern const char *
_LtDebugXmStringComponentType2String(int ct)
{
	switch (ct) {
	case XmSTRING_COMPONENT_UNKNOWN:	return "XmSTRING_COMPONENT_UNKNOWN";
	case XmSTRING_COMPONENT_CHARSET:	return "XmSTRING_COMPONENT_CHARSET";
	case XmSTRING_COMPONENT_TEXT:		return "XmSTRING_COMPONENT_TEXT";
	case XmSTRING_COMPONENT_DIRECTION:	return "XmSTRING_COMPONENT_DIRECTION";
	case XmSTRING_COMPONENT_SEPARATOR:	return "XmSTRING_COMPONENT_SEPARATOR";
	case XmSTRING_COMPONENT_LOCALE_TEXT:	return "XmSTRING_COMPONENT_LOCALE_TEXT";
	case XmSTRING_COMPONENT_LOCALE:		return "XmSTRING_COMPONENT_LOCALE";
	case XmSTRING_COMPONENT_WIDECHAR_TEXT:	return "XmSTRING_COMPONENT_WIDECHAR_TEXT";
	case XmSTRING_COMPONENT_LAYOUT_PUSH:	return "XmSTRING_COMPONENT_LAYOUT_PUSH";
	case XmSTRING_COMPONENT_LAYOUT_POP:	return "XmSTRING_COMPONENT_LAYOUT_POP";
	case XmSTRING_COMPONENT_RENDITION_BEGIN: return "XmSTRING_COMPONENT_RENDITION_BEGIN";
	case XmSTRING_COMPONENT_RENDITION_END:	return "XmSTRING_COMPONENT_RENDITION_END";
	case XmSTRING_COMPONENT_TAB:		return "XmSTRING_COMPONENT_TAB";
	default:				return "??";
	}
}
