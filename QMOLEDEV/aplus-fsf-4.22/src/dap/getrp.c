/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Douglas P. Kingston III */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <rpcsvc/ypclnt.h>
#include <dap/remprog.h>
#include <dap/balloc.h>

static char *domain;
static char *map = "remprogs";
static char *file = "/etc/remprogs";
static char *val;
static struct remprog *rps;

static void compress(char *fromptr);

getrpbyname(char *name, struct remprog ** rpp, int *nrpp)
{
  FILE *fp;
  int len, ret;
  int i;
  char buf[128];

  if (domain == (char *) (0))
    (void) yp_get_default_domain(&domain);

  if (val)
    bfree(val);
  val = (char *) (0);
  if (rps)
    bfree((char *) rps);
  rps = (struct remprog *) (0);

  ret = yp_match(domain, map, name, strlen((DEV_STRARG) name), &val, &len);
  switch (ret) {
  case YPERR_BADARGS:
  case YPERR_MAP:
  case YPERR_KEY:
  case YPERR_NOMORE:
  case YPERR_ACCESS:
    return (ret);
  case 0:
    return (rp_parse_value(val, rpp, nrpp));
  }

  /* Try opening the file directly */
  if ((fp = fopen(file, "r")) == (FILE *) (0))
    return (ret);		/* return YP error */
  while (fgets(buf, sizeof(buf), fp) == buf) {
    for (i = 0; buf[i] != 0; i++)
      if (isspace(buf[i]))
	break;
    buf[i] = 0;
    if (strcmp((DEV_STRARG) buf, (DEV_STRARG) name) == 0) {
      (void) fclose(fp);
      return (rp_parse_value(&buf[i + 1], rpp, nrpp));
    }
  }
  (void) fclose(fp);
  return (ret);			/* return YP error */
}

rp_parse_value(char *value, struct remprog ** rpp, int *nrpp)
{
  char *cp = value;
  char *p;
  struct remprog *rp;
  int i, nrp = 1;

  compress(value);
  for (cp = value; *cp; cp++)
    if (*cp == ' ')
      nrp++;

  rps = (struct remprog *) balloc_r((nrp + 1) * sizeof(*rps));
  if (rps == (struct remprog *) (0))
    return (YPERR_RESRC);
  rp = &rps[nrp];
  rp->rp_host = rp->rp_proto = (char *) 0;
  rp->rp_prognum = 0;

  compress(value);
  cp = value;
  p = value;
  for (i = 0; i < nrp; i++) {
    if (cp = (char *) strchr((DEV_STRARG) cp, ' ')) {
      *cp = 0;
      cp++;
    }
    rps[i].rp_host = p;
    p = (char *) strchr((DEV_STRARG) p, ':');
    *p = 0;
    p++;
    rps[i].rp_prognum = atoi(p);
    p = (char *) strchr((DEV_STRARG) p, ':');
    p++;
    rps[i].rp_proto = p;
    p = cp;
  }
  *rpp = rps;
  *nrpp = nrp;
  return (0);
}

static void
compress(char *fromptr)
{
  char chr, *toptr = fromptr;

  chr = ' ';			/* init to skip leading spaces  */
  while ((*toptr = *fromptr++) != '\0') {	/* convert newlines and tabs
						 * to */
    if (isspace(*toptr)) {	/* only save first space      */
      if (chr != ' ')
	*toptr++ = chr = ' ';
    } else
      chr = *toptr++;
  }

  if (chr == ' ')		/* remove trailing space if any */
    *--toptr = '\0';
  return;
}
