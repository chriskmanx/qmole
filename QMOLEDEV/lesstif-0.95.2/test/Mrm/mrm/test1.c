/**
 *
 * $Id: test1.c,v 1.2 2000/09/25 11:41:26 amai Exp $
 *
 * open.c
 *
 **/

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>


int
main(int argc, 
     char **argv)
{
    XtAppContext app;
    Widget toplevel, w;
    MrmHierarchy hierarchy;
    static String uid_file_list[] = { "hello_world.uid" };
    Cardinal status;
    MrmCode	cl;

    MrmInitialize();
    
    toplevel = XtVaAppInitialize(&app,
		  	         "open",
			         NULL, 0,
			         &argc, argv,
			         NULL, NULL);

#if 1
    status = MrmOpenHierarchyPerDisplay(XtDisplay(toplevel),
					XtNumber (uid_file_list),
					uid_file_list,
					NULL, &hierarchy);
#else
    status = MrmOpenHierarchy( XtNumber (uid_file_list),
					uid_file_list,
					NULL, &hierarchy);
#endif

    if (status != MrmSUCCESS)
    {
	fprintf (stderr, "Unable to open hello_world.uid file.\n");
	exit(1);
    }

    if (MrmFetchWidget(hierarchy, "hello_world_main",toplevel, &w, &cl) != MrmSUCCESS) {
	fprintf (stderr, "MrmFetchWidget failed\n");
	exit(1);
    }

    XtManageChild(w);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app);

    MrmCloseHierarchy(hierarchy);
    exit(0);
}
