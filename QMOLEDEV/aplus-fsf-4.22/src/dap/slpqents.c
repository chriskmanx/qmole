/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* head node for global list of scheduled sleep queue entries */

/* header file inclusions */
#include <dap/node.h>
#include <dap/slpq.h>

/* external data definitions */
struct node slpqents = {&slpqents, &slpqents, (void *) (0)};
