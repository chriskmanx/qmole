/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.                */
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <stdlib.h>
#include <dap/dap.h>
#include <a/fncdcls.h>

/* external function definitions */
void sInstall()
{
  char *atree;
  char *sfile;

  if ((atree = getenv("ATREE")) == (char *)(0))
  {
    atree = "/usr/local/a+";
  }
  sfile = bnstring(atree, "/lib/s.+", (char *)(0));
  loadafile(sfile,0);
  bfree(sfile);
  return;
}

