/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/exbo.h>
#include <dap/conn.h>

/* external function definitions */
void 
connfree(struct conn * p)
{
  if (p != (struct conn *) (0)) {
    p->retry = CONN_RETRY_NO;
    connclose(p);
    exbofree(p->retry_time);
    bfree(p->name);
    bfree((char *) p);
  }
  return;
}
