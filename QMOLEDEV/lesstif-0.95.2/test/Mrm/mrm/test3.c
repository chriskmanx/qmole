/* 
 * $Header: /cvsroot/lesstif/lesstif/test/Mrm/mrm/test3.c,v 1.2 2002/05/03 12:03:41 amai Exp $
 * motifanim.c
 * Build executable with:
 *    gcc -g -Wall -I/usr/X11R6/LessTif/Motif1.2/include
-I/usr/X11R6/include -I. -c motifanim.c
 *    gcc -g -Wall -L/usr/X11R6/lib -L/usr/X11R6/LessTif/Motif1.2/lib
-L. -o motifanim motifanim.o -lXm -lXt -lMrm -lX11
 */
 
/* 

Date: Tue, 14 Nov 2000 15:52:34 +0300
From: Andrei Rezvov <rezvov@niisi.msk.ru>
Organization: NIISI RAN
X-Mailer: Mozilla 4.51 [en] (X11; I; Linux 2.2.17 sparc64)
X-Accept-Language: en
MIME-Version: 1.0
To: lesstif@lesstif.org
Subject: libMrm problem report + bugfix proposal
Content-Type: text/plain; charset=koi8-r
Content-Transfer-Encoding: 7bit
Status: RO
Content-Length: 10224
Lines: 323

Hello,

I found that format offered by SourceForge is not perfect for reports
with several source files attached, so I'm sending one via email.
Sorry if this is wrong way of submitting bugs/fixes. We really need
working Lesstif Mrm stuff.

Best regards,
    Andrei Rezvov

================================================
Lesstif version/platform:
    all recent versions (since 1999/05/28) of Lesstif,
    Linux Redhat 6.2 (and possibly all others)
Problem brief description:
    BUG: libMrm dumps core
How to reproduce the problem:
    The tests for libMrm are not included in the Lesstif distribution,
so please find below
the sources of a very simple sample program demonstrating the problem.

*/
 
#include <stdio.h>
#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>

static MrmHierarchy     s_MrmHierarchy;

static char             *vec[1] = { "test3.uid" };
static int              vecnum = sizeof(vec) / sizeof(char*);

static MrmCode          class ;

Display                 *display;
XtAppContext            app_context;

/******************************************************************
 *  Main program
 */
int main(argc, argv)
     int    argc;
     String argv[];
{
    Widget toplevel, motifanimmain = NULL ;

    MrmInitialize ();

    toplevel = XtAppInitialize(&app_context, "demos",
                               NULL , 0,
                               &argc,
                               argv,
                               NULL,
                               NULL, 0);

    if (MrmOpenHierarchyPerDisplay (XtDisplay(toplevel),
                        vecnum,          /* number of files         */
                        vec,             /* files           */
                        NULL,            /* os_ext_list (null)   */
                        &s_MrmHierarchy) /* ptr to returned id   */
                        != MrmSUCCESS) {
        printf ("can't open hierarchy\n");
        exit(0);
     }

    if (MrmFetchWidget (s_MrmHierarchy,
                        "motifanim_main",
                        toplevel,
                        &motifanimmain,
                        &class)
                        != MrmSUCCESS) {
        printf("can't fetch interface\n");
        exit(0);
    }

    XtManageChild(motifanimmain);

    XtRealizeWidget(toplevel);

    /* XtAppMainLoop(app_context); */
    
    LessTifTestMainLoop(toplevel);

    /* UNREACHABLE */
    return (0);
}
