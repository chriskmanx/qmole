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
#include <dap/kvp.h>

/* external function definitions */
struct kvp *
kvpalloc(void *key, void *val)
{
  struct kvp *p = (struct kvp *) balloc(sizeof(*p));

  p->key = key;
  p->d = val;

  return p;
}
