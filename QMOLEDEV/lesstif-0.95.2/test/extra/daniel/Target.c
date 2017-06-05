/* $Header: /cvsroot/lesstif/lesstif/test/extra/daniel/Target.c,v 1.2 2002/05/15 10:55:06 amai Exp $ */
/***********************************************************/
/* Copyright 1996 Daniel Dardailler.  
Permission to use, copy, modify, distribute, and sell this software
for any purpose is hereby granted without fee, provided that the above
copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation,
and that the name of Daniel Dardailler not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  Daniel Dardailler makes no representations
about the suitability of this software for any purpose.  It is
provided "as is" without express or implied warranty.
************************************************************/
/***** Targets/Index stuff */
/* incomplete support: search for unsupported */


#include <stdlib.h>
#include <stdio.h>

#include "DndP.h"

static Atom atom_motif_window, atom_target_list ;

static void
InitAtoms(Display * dpy) 
{
    if (atom_motif_window) return ; /* already Initialized */

    /* Init atoms used in the com */
    atom_motif_window = XInternAtom(dpy, "_MOTIF_DRAG_WINDOW", False);
    atom_target_list = XInternAtom(dpy, "_MOTIF_DRAG_TARGETS", False);
}


static Window 
MotifWindow(Display *display )
{
    Atom            type;
    int             format;
    unsigned long   size;
    unsigned long   bytes_after;
    Window         *property = NULL;
    Window	    motif_window ;

    /* this version does no caching, so it's slow: round trip each time */

    if ((XGetWindowProperty (display, DefaultRootWindow(display),
                             atom_motif_window,
                             0L, 100000L, False, AnyPropertyType,
                             &type, &format, &size, &bytes_after, 
			     (unsigned char **) &property) == Success) &&
         (type != None)) {
	motif_window = *property;
    } else {
	XSetWindowAttributes sAttributes;

	/* really, this should be done on a separate connection,
	   with XSetCloseDownMode (RetainPermanent), so that
	   others don't have to recreate it; hopefully, some real 
	   Motif application will be around to do it */

	sAttributes.override_redirect = True;
	sAttributes.event_mask = PropertyChangeMask;
	motif_window = XCreateWindow (display,
				     DefaultRootWindow (display),
				     -170, -560, 1, 1, 0, 0,
				     InputOnly, CopyFromParent,
				     (CWOverrideRedirect |CWEventMask),
				     &sAttributes);
	XMapWindow (display, motif_window);
    }

    if (property) {
	XFree ((char *)property);
    }

    return (motif_window);
}


static DndTargetsTable TargetsTable(Display *display)
{
    Atom            type;
    int             format;
    unsigned long   size;
    unsigned long   bytes_after;
    Window motif_window = MotifWindow(display) ;
    DndTargets * target_prop;
    DndTargetsTable targets_table ;
    int i,j ;
    char * target_data ;

    /* this version does no caching, so it's slow: round trip each time */
    /* ideally, register for property notify on this target_list
       atom and update when necessary only */

    if ((XGetWindowProperty (display, motif_window,	
			     atom_target_list, 0L, 100000L,	
			     False, atom_target_list,	
			     &type, &format,	&size, &bytes_after,
			     (unsigned char **) &target_prop) != Success) ||
           type == None) {
	printf("Get Targets cannot get property on motif_window\n");
	return NULL ;
    }

    if (target_prop->protocol_version != DND_PROTOCOL_VERSION) {
	printf("Get Targets warning: D&D protocol mismatch\n");
    }

    if (target_prop->byte_order != _DndByteOrder()) {
	/* need to swap num_target_lists and size */
	SWAP2BYTES(target_prop->num_target_lists);
	SWAP4BYTES(target_prop->data_size);
    }
    
    /* now parse DndTarget prop data in a TargetsTable */

    targets_table = (DndTargetsTable)malloc(sizeof(DndTargetsTableRec));
    targets_table->num_entries = target_prop->num_target_lists ;
    targets_table->entries = (DndTargetsTableEntry)
	malloc(sizeof(DndTargetsTableEntryRec) * target_prop->num_target_lists);

    target_data = (char*)target_prop + sizeof(*target_prop) ;

    for (i=0 ; i < targets_table->num_entries; i++) {
	CARD16 num_targets ;
	CARD32 atom ;

	memcpy(&num_targets, target_data, 2);
	target_data += 2;

	/* potential swap needed here */
	if (target_prop->byte_order != _DndByteOrder())
	    SWAP2BYTES(num_targets);

	targets_table->entries[i].num_targets = num_targets ;
	targets_table->entries[i].targets = (Atom *)
	    malloc(sizeof(Atom) * targets_table->entries[i].num_targets);


	for (j = 0; j < num_targets; j++) {
	    memcpy(&atom, target_data, 4 );
	    target_data += 4;

	    /* another potential swap needed here */
	    if (target_prop->byte_order != _DndByteOrder())
		SWAP4BYTES(atom);

	    targets_table->entries[i].targets[j] = (Atom) atom ;
	}
    }

    if (target_prop) {
        XFree((char *)target_prop);
    } 

    return targets_table ;
}

static int 
AtomCompare(void *atom1, void *atom2 )
{
    return (*((Atom *) atom1) - *((Atom *) atom2));
}

extern int
_DndTargetsToIndex(Display * display,
		   Atom * targets, int num_targets)
{
    int		i, j;
    Atom		*sorted_targets;
    DndTargetsTable	targets_table;
    int index = -1 ;

    InitAtoms(display) ;

    if (!(targets_table = TargetsTable (display))) {
	printf("_DndTargetsToIndex: cannot find the target table data\n");
        return -1 ;
    }

    /* sort the given target list */
    sorted_targets = (Atom *) malloc(sizeof(Atom) * num_targets);
    memcpy (sorted_targets, targets, sizeof(Atom) * num_targets);
    qsort ((void *)sorted_targets, (size_t)num_targets, (size_t)sizeof(Atom),
           AtomCompare);

    /* look for a match */

    for (i = 0; i < targets_table->num_entries; i++) {
	if (num_targets == targets_table->entries[i].num_targets) {
            for (j = 0; j < num_targets; j++) {
	        if (sorted_targets[j] != 
		    targets_table->entries[i].targets[j]) {
	            break;
		}
	    }
	    if (j == num_targets) {
	        index = i ;
		break ;
	    }
	}
    }
    
    XFree ((char *)sorted_targets);
    /* free the target table and its guts */
    for (i=0 ; i < targets_table->num_entries; i++) 
	XFree((char*)targets_table->entries[i].targets);
    XFree((char*)targets_table);

    if (index == -1 )
	printf("DndTargetsToIndex: non existing target list: unsupported\n");
        /* to support: need to grab the server, add our target list
	   to the property, return index = num_entries++, and ungrab */
    return index ;
}

int
_DndIndexToTargets(Display * display,
		   int index,
		   Atom ** targets)
{
    DndTargetsTable	targets_table;
    int i ;

    /* again, slow: no caching here, alloc/free each time */

    InitAtoms(display) ;

    printf("index %d\n", index);

    if (!(targets_table = TargetsTable (display)) ||
	(index >= targets_table->num_entries)) {
        return -1;
    }

    /* transfer the correct target list index */
    *targets = (Atom*)malloc(sizeof(Atom)*targets_table->
			    entries[index].num_targets);
    memcpy((char*)*targets,
	   (char*)targets_table->entries[index].targets,
	   sizeof(Atom)*targets_table->entries[index].num_targets);

    /* free the target table and its guts */
    for (i=0 ; i < targets_table->num_entries; i++) 
	XFree((char*)targets_table->entries[i].targets);
    XFree((char*)targets_table);

    return targets_table->entries[index].num_targets;
}
