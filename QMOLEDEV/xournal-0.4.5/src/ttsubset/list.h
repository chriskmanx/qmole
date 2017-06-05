/*
 * Copyright © 2002, 2003 Sun Microsystems, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of Sun Microsystems, Inc. nor the names of 
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any kind.
 *
 * ALL EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES,
 * INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED.
 * SUN AND ITS LICENSORS SHALL NOT BE LIABLE FOR ANY DAMAGES OR
 * LIABILITIES SUFFERED BY LICENSEE AS A RESULT OF OR RELATING TO USE,
 * MODIFICATION OR DISTRIBUTION OF THE SOFTWARE OR ITS DERIVATIVES.
 * IN NO EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE,
 * PROFIT OR DATA, OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL,
 * INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE
 * THEORY OF LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE
 * SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 */

/* $Id: list.h,v 1.3 2004/09/10 18:59:06 jody Exp $ */
/* @(#)list.h 1.6 03/02/06 SMI */

/*
 * @file list.h
 * @brief Bidirectional list class header file
 * @author Alexander Gelfenbain
 * @version 1.0
 *
 */

#ifndef __CLIST_H
#define __CLIST_H

#ifdef __cplusplus
extern "C"{
#endif

#include <glib.h>

/*
 * List of void * pointers
 */

typedef struct _list *list;

/*- constructors and a destructor */
list listNewEmpty(void);
list listNewCopy(list);
list listNewConcat(list, list);                             /* concatenates elements in two lists and creates a new list with them */
void listDispose(list);
void listSetElementDtor(list, GDestroyNotify f);           /*- this function will be executed when the element is removed via listRemove() or listClear() */

/*- assignment */
list listCopy(list to, list from);

/*- queries */
void * listCurrent(list);
int    listCount(list);
int    listIsEmpty(list);
int    listAtFirst(list);
int    listAtLast(list);
int    listPosition(list);                        /* Expensive! */

/*- search */
int    listFind(list, void *);                    /* Returns true/false */

/*- positioning functions */
/*- return the number of elements by which the current position in the list changes */
int    listNext(list);
int    listPrev(list);
int    listSkipForward(list, int n);
int    listSkipBackward(list, int n);
int    listToFirst(list);
int    listToLast(list);
int    listPositionAt(list, int n);               /* Expensive! */

/*- adding and removing elements */
list   listAppend(list, void *);
list   listPrepend(list, void *);
list   listInsertAfter(list, void *);
list   listInsertBefore(list, void *);
list   listConcat(list lhs, list rhs);            /* appends all elements of rhs to lhs and returns lhs */
                                                  
list   listRemove(list);                          /* removes the current element */
list   listClear(list);                           /* removes all elements */

/*- forall */
void   listForAll(list, void (*f)(void *));

/*- conversion */
void **listToArray(list);                         /* XXX listToArray does not duplicate the elements, just copies pointers to them */
                                                  /* so you can't call listRemove(), listClear(), or listDispose() until you are done */
                                                  /* with the array. */

#ifdef __cplusplus
}
#endif


#endif /* __CLIST_H */
