/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/xmos/test1.c,v 1.2 2001/08/15 08:04:37 amai Exp $
 * test for _XmOSGetHomeDirName()
 *
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/XmosP.h>


int
main(int argc,
     char **argv)
{
    printf ("Home directory = %s\n", _XmOSGetHomeDirName());
    exit(0);
}
