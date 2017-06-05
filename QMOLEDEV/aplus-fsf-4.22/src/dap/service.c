/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Jordan Hayes */

#include <string.h>
#include <sys/param.h>
#if defined(HAVE_SVR4)
#include <netdb.h>
#endif
#include <rpcsvc/ypclnt.h>
#include <dap/balloc.h>
#include <dap/Warn.h>
#include <dap/remprog.h>

/* Internal function definations */
static int GetRP(char *name, struct remprog ** rp_arg, struct remprog ** rpp_arg);

static int count = 0, current = 0;
static struct remprog *rp, *rpp;

#define	MAX_TRIES	3

int
GetService(char *name)
{
  if (count > 0)
    count = current = 0;

  return (count = GetRP(name, &rp, &rpp));
}

static int
GetRP(char *name, struct remprog ** rp_arg, struct remprog ** rpp_arg)
{
  static char fnc[] = "GetRP";
  int cnt, tries = 0;

  for (;;) {
    switch (getrpbyname(name, rp_arg, &cnt)) {

      /* Success */
    case 0:
      *rpp_arg = *rp_arg;
      return (cnt);

      /* Not necessarily fatal errors - worth retrying */
    case YPERR_RPC:
    case YPERR_DOMAIN:
    case YPERR_MAP:
    case YPERR_KEY:
    case YPERR_YPERR:
    case YPERR_PMAP:
    case YPERR_YPBIND:
    case YPERR_YPSERV:
    case YPERR_RESRC:
    case YPERR_NOMORE:
    case YPERR_NODOM:
    case YPERR_BADDB:
    case YPERR_VERS:
#ifdef YPERR_BUSY
    case YPERR_BUSY:
#endif
#ifdef YPERR_ACCESS
    case YPERR_ACCESS:
#endif
      if (++tries > MAX_TRIES) {
	Warn("%t %s: Cannot find service '%s'\n", fnc, name);
	return (-1);
      }
      break;

      /* Fatal errors */
    case YPERR_BADARGS:	/* Args to function are bad */
    default:
      Warn("%t %s: Cannot find service '%s'\n", fnc, name);
      return (-1);
    }
  }
  /* NOTREACHED */
}

int
NextService(char **host, long int *program, char **protocol)
{
  static char fnc[] = "NextService";

  /* eh? */
  if (count == 0) {
    Warn("%t %s: Next before get?\n", fnc);
    return (-1);
  }
  /* fill in the blanks */
  if (host != (char **) (0))
    *host = bstring(rpp->rp_host);
  if (program != (long *) (0))
    *program = rpp->rp_prognum;
  if (protocol != (char **) (0))
    *protocol = bstring(rpp->rp_proto);

  current++, rpp++;
  if (current == count)
    current = 0, rpp = rp;
  return (current);
}

ServEnt *
GetServ(char *name)
{
  ServEnt *r;

  /* init */
  r = (ServEnt *) balloc(sizeof(*r));

  if ((r->count = GetRP(name, &r->rp, &r->rpp)) == -1) {
    bfree((char *) r);
    return (ServEnt *) NULL;
  }
  r->name = bstring(name);
  r->host = (char *) balloc(MAXHOSTNAMELEN);
  bzero(r->host, MAXHOSTNAMELEN);
  r->protocol = (char *) balloc(MAXHOSTNAMELEN);
  bzero(r->protocol, MAXHOSTNAMELEN);
  r->index = 0;

  return (r);
}

int
NextServ(ServEnt * sp)
{
  static char fnc[] = "NextServ";

  if (sp->count < 1) {
    Warn("%t %s: Next before get???\n", fnc);
    return (-1);
  }
  if (sp->host != (char *) (0))
    (void) strcpy((DEV_STRARG) sp->host, (DEV_STRARG) sp->rpp->rp_host);
  sp->program = sp->rpp->rp_prognum;
  if (sp->protocol != (char *) (0))
    (void) strcpy((DEV_STRARG) sp->protocol, (DEV_STRARG) sp->rpp->rp_proto);
  if (++(sp->index) == sp->count) {
    sp->index = 0;
    sp->rpp = sp->rp;
  } else
    sp->rpp++;

  return (sp->index);
}

void
ServEntDestroy(ServEnt * sp)
{
  bfree(sp->name);
  bfree(sp->host);
  bfree(sp->protocol);
  bfree((char *) sp);
}

/* For backward compatibility with old (depricated) interface */
get_service_by_name(char *name, char **host, long int *program, char **protocol)
{
  if ((GetService(name) < 0)
      || (NextService(host, program, protocol) < 0))
    return (-1);
  return (0);
}
