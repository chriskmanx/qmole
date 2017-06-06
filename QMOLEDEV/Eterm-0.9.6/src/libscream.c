/****************************************************************************
 * scream::libscream.c                         Azundris <scream@azundris.com>
 *
 * routines for terminal emulators to connect to screen and/or scream daemons.
 * libscream is a double-transparency layer -- it abstracts the backend
 * (screen or a replacement, locally or ssh-tunneled) to the front-end
 * (a terminal-emulation such as Eterm, konsole, or multi-gnome-terminal)
 * and vice versa.  several sessions can be open at once.
 *
 * Lesser GNU Public Licence applies.
 * Distributed with Eterm under the same license terms as Eterm itself.
 * Thread-safe:  untested
 *
 * 2002/04/19  Azundris  incept
 * 2002/05/04  Azundris  support for esoteric screens, thanks to Till
 * 2002/05/12  Azundris  edit display names, send statement, tab completion
 * 2002/05/13  Azundris  ssh tunnel through firewall
 * 2002/05/17  Azundris  supports systemwide screenrc (thanks mej)
 * 2002/05/18  Azundris  remote handling improved (thanks tillsan, tfing)
 * 2002/05/21  Azundris  code restructuring, basic tab tear-off
 * 2002/06/04  Azundris  advanced tab tear-off
 * 2002/06/05  Azundris  basic twin support
 * 2002/06/11  Azundris  more  twin support
 * 2005/01/04  Azundris  send statements rather than hotkeys wherever possible
 ***************************************************************************/



#undef NS_DEBUG

#include <stdio.h>              /* stderr, fprintf, snprintf() */
#include <string.h>             /* bzero() */
#include <pwd.h>                /* getpwuid() */
#include <sys/types.h>          /* getpwuid() */
#include <sys/stat.h>           /* stat() */
#include <unistd.h>             /* getuid() */
#include <stdlib.h>             /* atoi() */
#include <netdb.h>              /* getservbyname() */
#include <netinet/in.h>         /* ntohs() */
#include <limits.h>             /* PATH_MAX */
#include <ctype.h>              /* isspace() */
#include <errno.h>              /* errno */
#include <sys/socket.h>

#include "config.h"
#include "feature.h"

/* use libast if we have it */
#ifdef DEBUG_ESCREEN
#  include <libast.h>
#else
#  define MALLOC(a) malloc(a)
#  define FREE(a) free(a)
#  define STRDUP(a) strdup(a)
#  ifdef NS_DEBUG
#    define D_ESCREEN(a)  fprintf(stderr,a);
#  else
#    define D_ESCREEN(a)
#  endif
#endif

#include "scream.h"             /* structs, defs, headers */
#include "screamcfg.h"          /* user-tunables */

#ifdef NS_HAVE_TWIN
#  include <Tw/Tw.h>
#  include <Tw/Tw1.h>
#  include <Tw/Twerrno.h>
#endif

#ifndef MAXPATHLEN
#  ifdef PATH_MAX
#    define MAXPATHLEN PATH_MAX
#  elif defined(MAX_PATHLEN)
#    define MAXPATHLEN MAX_PATHLEN
#  endif
#endif



/***************************************************************************/
/* module-global vars */
/**********************/



static long err_inhibit = 0;    /* bits. avoid telling same error twice. */
static _ns_sess *sa = NULL;     /* anchor for session list */
static _ns_hop *ha = NULL;      /* anchor for hop list */



/***************************************************************************/
/* forward declarations */
/************************/



static void ns_desc_hop(_ns_hop *, char *);
static int ns_parse_screenrc(_ns_sess *, char *, ns_esc_whence);
static int ns_mov_screen_disp(_ns_sess *, int, int);
static _ns_sess *ns_dst_sess(_ns_sess **);

#ifdef NS_HAVE_SCREEN
static int ns_inp_tab(void *, char *, size_t, size_t);
#endif



/****************************************************************************
           _     _     _ _          _                       
 _ __ ___ (_) __| | __| | | ___    | | __ _ _   _  ___ _ __ 
| '_ ` _ \| |/ _` |/ _` | |/ _ \   | |/ _` | | | |/ _ \ '__|
| | | | | | | (_| | (_| | |  __/   | | (_| | |_| |  __/ |   
|_| |_| |_|_|\__,_|\__,_|_|\___|   |_|\__,_|\__, |\___|_|   
                                            |___/

central abstraction layer

  this abstracts the front-end (terminal emulator) against the back-end
  (local or remote terminal server), and the back-end against the front-end:

  - front-end hands us an URL we attach to (without knowing about the backend)
  - CAL receives messages from back-end and calls the external function (efun)
    the front-end registered for this event
  - CAL functions are called from the front-end and send data fitting the
    session-type to the backend
*/



/* test if we have a valid callback for function-type "e".
  !p  a variable of the "_ns_efuns *" type.  will contain a pointer to
      an efun struct containing a function pointer to the requested function
      if such a struct exists, or NULL, if it doesn't exist
   s  a variable of the "_ns_sess *" type, or NULL (see ns_get_efuns())
   d  a variable of the "_nd_disp *" type, or NULL (see ns_get_efuns())
   e  the name of an element of "_ns_efuns"
  !<- conditional execution of next (compound-) statement (which would
      normally be (p)->(e)(...), the call of the function e).
 */
#define NS_EFUN_EXISTS(p,s,d,e) (((p)=ns_get_efuns((s),(d)))&&((p)->e))



/***************************************************************************/
/* constructors/destructors */
/****************************/



/* ns_free
   free a string (or whatever) */

static void
*
ns_free(char **x)
{
    if (x && !*x) {
        FREE(*x);
        *x = NULL;
    }
    return NULL;
}



/* ns_new_hop.  create and initialize a hop struct.
   lp    local port.  if 0:  if otherwise matching hop exists, reuse that.
                             otherwise, find the first FREE (as in, not used
                             by us) port, starting with NS_MIN_PORT.
   fw    firewall machine.  numeric or symbolic.
   fp    foreign port. if 0: default to SSH port.
   delay wait n seconds for tunnel to come up before trying to use it
   s     the session to add the hop to
   <-    a matching (existing or newly created) hop structure, or NULL */

static _ns_hop *
ns_new_hop(int lp, char *fw, int fp, int delay, _ns_sess * s)
{
    _ns_hop *h = ha;

    if (!fw || !*fw)
        return NULL;

    if (!fp)
        fp = ns_get_ssh_port(); /* remote port defaults to SSH */

    if (s) {
        /* see if we already have a matching hop. */
        while (h && !(((h->localport == lp) || (!lp)) && (!strcmp(h->fw, fw)) && (h->fwport == fp) && (h->sess->port == s->port)
                      && (!strcmp(h->sess->host, s->host))))
            h = h->next;

        if (h) {
            if (delay)
                h->delay = delay;       /* may change delay! */
            h->refcount++;
            return h;
        }
    }

    h = MALLOC(sizeof(_ns_hop));
    if (h) {
        bzero(h, sizeof(_ns_hop));
        if ((h->fw = STRDUP(fw))) {
            if (!lp) {
                int tmp_sock;

                tmp_sock = socket(PF_INET, SOCK_STREAM, 6);
                if (tmp_sock > 0) {
                    struct sockaddr_in addr;

                    addr.sin_family = AF_INET;
                    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
                    for (lp = NS_MIN_PORT; (lp > 0) && (lp < NS_MAX_PORT); lp++) {
                        addr.sin_port = htons(lp);

                        if (bind(tmp_sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in))) {
                            D_ESCREEN(("Unable to bind socket %d to 127.0.0.1:%hd -- %s\n", tmp_sock, lp, strerror(errno)));
                        } else if (listen(tmp_sock, 1)) {
                            D_ESCREEN(("Unable to listen on port %hd -- %s\n", lp, strerror(errno)));
                        } else {
                            /* We can listen on this port.  Use it! */
                            /* FIXME:  Minor race condition between port selection and ssh call. */
                            D_ESCREEN(("Got available listening port %d.\n", lp));
                            break;
                        }
                    }
                    if ((lp < 0) || (lp == NS_MAX_PORT)) {
                        /* We're going to fail anyway, so just throw something in. */
                        lp = NS_MIN_PORT + (random() % (NS_MAX_PORT - NS_MIN_PORT));
                        BOUND(lp, NS_MIN_PORT, NS_MAX_PORT);
                        D_ESCREEN(("Chose random listening port %d.\n", lp));
                    }
                    close(tmp_sock);
                }
            }
            h->delay = (delay ? delay : NS_TUNNEL_DELAY);
            h->localport = lp;
            h->fwport = fp;
            h->refcount++;
            h->next = ha;
            h->sess = s;
            ha = h;
        } else {
            FREE(h);
            return NULL;
        }
    }

    return h;
}



/* ns_dst_hop.  deref (and, where necessary, release) a hop struct.
   if sp is provided, additional integrity magic will take place.
   ss  hop to deref/FREE
   sp  session that the hop used to belong to (NULL for none (as if))
   <-  NULL */

static _ns_hop *
ns_dst_hop(_ns_hop ** ss, _ns_sess * sp)
{
    if (ss && *ss) {
        _ns_hop *s = *ss;

        if (s->refcount <= 0) {
            D_ESCREEN(("ns_dst_hop: leak alert -- trying to double-FREE hop...\n"));
            return NULL;
        }

        if (!--(s->refcount)) { /* was last ref to hop => FREE hop */
            if (s->fw)
                FREE(s->fw);
            if (ha == s)        /* delist */
                ha = s->next;
            else {
                _ns_hop *h = ha;

                while (h && h->next != s)
                    h = h->next;
                if (h)
                    h->next = s->next;
            }
            FREE(s);
        } else if (sp && sp->hop == s) {
            /* hop shouldn't point back at a session that just dereffed it
               as it's probably about to die. fix the back ref to a session
               that's actually valid. */
            _ns_sess *p = sa;

            while (p && ((p == sp) || (p->port != sp->port) || (strcmp(p->host, sp->host))))
                p = p->next;
            if (!p)
                ns_desc_hop(s,
                            NS_PREFIX
                            "ns_dst_sess: Leak alert -- found a hop that is only\n referenced once, but has a refcount > 1. Hop data follow");
            else
                s->sess = p;
        }
        *ss = NULL;
    }
    return NULL;
}



_ns_efuns *
ns_new_efuns(void)
{
    _ns_efuns *s = MALLOC(sizeof(_ns_efuns));

    if (s) {
        bzero(s, sizeof(_ns_efuns));
    }
    return s;
}

static _ns_efuns *
ns_ref_efuns(_ns_efuns ** ss)
{
    if (ss && *ss) {
        (*ss)->refcount++;
        return *ss;
    }
    return NULL;
}

_ns_efuns *
ns_dst_efuns(_ns_efuns ** ss)
{
    if (ss && *ss) {
        _ns_efuns *s = *ss;

        *ss = NULL;
        if (!--(s->refcount)) {
            FREE(s);
        }
    }
    return NULL;
}



static _ns_disp *
ns_new_disp(void)
{
    _ns_disp *s = MALLOC(sizeof(_ns_disp));

    if (s) {
        bzero(s, sizeof(_ns_disp));
    }
    return s;
}

static _ns_disp *
ns_dst_disp(_ns_disp ** ss)
{
    if (ss && *ss) {
        _ns_disp *s = *ss;

        if (s->name)
            FREE(s->name);
        if (s->efuns)
            ns_dst_efuns(&(s->efuns));
        if (s->child)           /* nested screen? */
            ns_dst_sess(&(s->child));
        *ss = NULL;
        FREE(s);
    }
    return NULL;
}

static _ns_disp *
ns_dst_dsps(_ns_disp ** ss)
{
    if (ss && *ss) {
        _ns_disp *s = *ss, *t;

        *ss = NULL;
        do {
            t = s->next;
            ns_dst_disp(&s);
            s = t;
        } while (s);
    }
    return NULL;
}



_ns_sess *
ns_1st_sess(void)
{
    return sa;
}



static _ns_sess *
ns_new_sess(void)
{
    _ns_sess *s = MALLOC(sizeof(_ns_sess));

    if (s) {
        bzero(s, sizeof(_ns_sess));
        s->escape = NS_SCREEN_ESCAPE;   /* default setup for the screen program */
        s->literal = NS_SCREEN_LITERAL; /* set even ifndef NS_HAVE_SCREEN */
        s->dsbb = NS_SCREEN_DEFSBB;
        s->delay = NS_INIT_DELAY;
        s->fd = -1;
        s->disp = -1;
        s->port = -1;
        if (sa) {               /* add to end of list */
            _ns_sess *r = sa;

            while (r->next)
                r = r->next;
            r->next = s;
        } else
            sa = s;
    }
    return s;
}

static _ns_sess *
ns_dst_sess(_ns_sess ** ss)
{
    if (ss && *ss) {
        _ns_sess *s = *ss;

        ns_dst_dsps(&(s->dsps));
        if (s->hop)
            ns_dst_hop(&(s->hop), s);
        if (s->host)
            FREE(s->host);
        if (s->user)
            FREE(s->user);
        if (s->pass)
            FREE(s->pass);
#ifdef NS_HAVE_TWIN
        if (s->twin_str)
            FREE(s->twin_str);
#endif
        if (s->efuns)
            ns_dst_efuns(&(s->efuns));
        if (s->prvs)
            s->prvs->next = s->next;
        else
            sa = s->next;       /* align anchor */
        if (s->next)
            s->next->prvs = s->prvs;
        *ss = NULL;
        FREE(s);
    }
    return NULL;
}




/***************************************************************************/
/* display-list handling */
/*************************/



/* ns_magic_disp
   if we pass one or both of session and display info to this, both
   elements will be set up with sensible data afterwards.  shouldn't
   fail unless the lists are major corrupted or no info is supplied.
   s  a session pointer's address
   d  a display pointer's address
   <- NS_SUCC if we could set up the info */

int
ns_magic_disp(_ns_sess ** s, _ns_disp ** d)
{
    if (!d)
        return NS_FAIL;

    if (*d) {
        (*d)->sess->curr = *d;
        if (s && !*s)
            (*s) = (*d)->sess;
#ifdef NS_PARANOID
        else if (s && *s != (*d)->sess) {
            D_ESCREEN(("ns_magic_disp: was given a disp and a session that do not belong (\n"));
            return NS_FAIL;
        }
#endif
        return NS_SUCC;
    } else if (s && *s) {
        if (!(*s)->curr) {
            if (((*s)->curr = (*s)->dsps))
                return NS_SUCC;
        } else
            return NS_SUCC;
    }
    return NS_FAIL;
}



/* we need a certain display struct (index n in session s).
   give it to us if it exists.
   s  the session the display should be in
   n  the index of the display (>=0).  displays in a session are sorted
      by index, but may be sparse (0, 1, 3, 7)
   <- the requested display */

static _ns_disp *
disp_fetch(_ns_sess * s, int n)
{
    _ns_disp *e = NULL, *c;

    for (c = s->dsps; c && (c->index < n); c = c->next)
        e = c;
    if (c && (c->index == n))   /* found it */
        return c;
    return NULL;
}



/* we need a certain display struct (index n in session s).
   give it to us.  if you can't find it, make one up and insert it into
   the list.
   s  the session the display should be in
   n  the index of the display (>=0).  displays in a session are sorted
      by index, but may be sparse (0, 1, 3, 7)
   <- the requested display */

static _ns_disp *
disp_fetch_or_make(_ns_sess * s, int n)
{
    _ns_disp *d, *e = NULL, *c;

    for (c = s->dsps; c && (c->index < n); c = c->next)
        e = c;

    if (c && (c->index == n))   /* found it */
        return c;

    if (!(d = ns_new_disp()))   /* not there, create new */
        return NULL;            /* can't create, fail */

    d->index = n;

    if ((d->next = c))          /* if not last element... */
        c->prvs = d;
    if ((d->prvs = e))          /* if not first element */
        e->next = d;
    else                        /* make first */
        s->dsps = d;

    d->sess = s;                /* note session on display */

    if (!d->sess->curr)         /* note as current on session if first display */
        d->sess->curr = d;

    return d;
}



/* get element number from screen-index (latter is sparse, former ain't)
   screen   the session in question
   n        the index screen gave us (sparse)
   <-       the real index (element number in our list of displays) */

int
disp_get_real_by_screen(_ns_sess * screen, int n)
{
    _ns_disp *d2 = screen->dsps;
    int r = 0;

    while (d2 && d2->index != n) {
        d2 = d2->next;
        r++;
    }
#ifdef NS_PARANOID
    if (!d2)
        return -1;
#endif
    return r;
}



/* get screen-index from element number (former is sparse, latter ain't)
   screen   the session in question
   n        the real index (element number in our list of displays)
   <-       the index screen knows (sparse) */

int
disp_get_screen_by_real(_ns_sess * screen, int r)
{
    _ns_disp *d2 = screen->dsps;

    while (d2 && (r-- > 0))
        d2 = d2->next;
#ifdef NS_PARANOID
    if (!d2)
        return -1;
#endif
    return d2->index;
}



/* remove a display from the internal list and release its struct and data
   disp  the display in question */

static void
disp_kill(_ns_disp * d3)
{
    if (d3->prvs) {
        d3->prvs->next = d3->next;
        if (d3->sess->curr == d3)
            d3->sess->curr = d3->prvs;
    } else {
        d3->sess->dsps = d3->next;
        if (d3->sess->curr == d3)
            d3->sess->curr = d3->next;
    }
    if (d3->next)
        d3->next->prvs = d3->prvs;
    ns_dst_disp(&d3);
}



/***************************************************************************/
/* attach/detach */
/*****************/



/* ns_sess_init
   init an opened session (transmit .screenrc, or whatever)
   sess  the session
   <-    error code */

int
ns_sess_init(_ns_sess * sess)
{
#ifdef NS_HAVE_SCREEN
    if ((sess->backend == NS_MODE_NEGOTIATE) || (sess->backend == NS_MODE_SCREEN)) {
        (void) ns_parse_screenrc(sess, sess->sysrc, NS_ESC_SYSSCREENRC);
        return ns_parse_screenrc(sess, sess->home, NS_ESC_SCREENRC);
    }
#endif
    return NS_SUCC;
}



/* return port number for service TWIN.
   <-  a port number -- 7754 in all likelihood. */

int
ns_get_twin_port(void)
{
#ifdef NS_HAVE_TWIN
    static int port = 0;
    struct servent *srv;

    if (port)
        return port;
    /* (fixme) replace with getservbyname_r on systems that have it */
    srv = getservbyname("twin", "tcp");
    return (port = (srv ? ntohs(srv->s_port) : TW_INET_PORT));
#else
    return -1;
#endif
}



/* return port number for service SSH (secure shell).
   <-  a port number -- 22 in all likelihood. */

int
ns_get_ssh_port(void)
{
    static int port = 0;
    struct servent *srv;

    if (port)
        return port;
    /* (fixme) replace with getservbyname_r on systems that have it */
    srv = getservbyname("ssh", "tcp");
    return (port = (srv ? ntohs(srv->s_port) : NS_DFLT_SSH_PORT));
}




/* ns_parse_hop
   parse a hop-string into a hop-struct
   h:   one of NULL lclport:fw:fwport fw:fwport lclport:fw
        if set, describes how to tunnel through a fw to access an URL
        describing a target behind said firewall
   <-   a hop struct, or NULL
*/

static _ns_hop *
ns_parse_hop(_ns_sess * s, char *h)
{
    char *p = h, *e;
    int f = 0, v, lp = 0, fp = 0, delay = 0;

    if (!h || !*h)
        return NULL;

    if ((e = strrchr(h, ','))) {
        *(e++) = '\0';
        if (*e)
            delay = atoi(e);
    }

    while (*p && *p != ':')
        if (!isdigit(*(p++)))
            f = 1;

    if (!*p)                    /* fw only */
        return ns_new_hop(lp, h, fp, delay, s);

    if (!f) {                   /* lp:fw... */
        if (!(v = atoi(h)))
            return NULL;
        lp = v;
        e = ++p;
        while (*e && *e != ':')
            e++;
        if (*e) {
            *(e++) = '\0';
            if (!(v = atoi(e)))
                return NULL;
            fp = v;
        }
    } else {                    /* fw:fp */
        *(p++) = '\0';
        if (!(v = atoi(p)))
            return NULL;
        fp = v;
        p = h;
    }
    return ns_new_hop(lp, p, fp, delay, s);
}



/* ns_desc_string
   c        the string
   doc      context-info
   !stdout  the string, in human-readable form */

static void
ns_desc_string(char *c, char *doc)
{
    char *p = c;
    char buff[1024], *pbuff;
    size_t len, n;

    len = sizeof(buff);
    pbuff = buff;

    if (doc) {
        n = snprintf(pbuff, len, "%s: ", doc);
        len -= n;
        pbuff += n;
    }

    if (!c) {
        snprintf(pbuff, len, "NULL\n");
        D_ESCREEN(("%s", buff));
        return;
    } else if (!*c) {
        snprintf(pbuff, len, "empty\n");
        D_ESCREEN(("%s", buff));
        return;
    }

    while (*p) {
        if (*p < ' ') {
            snprintf(pbuff, len, "^%c", *p + 'A' - 1);
            pbuff += 2;
            len -= 2;
        } else {
            snprintf(pbuff++, len--, "%c", *p);
        }
        p++;
    }

    D_ESCREEN(("%s\n", buff));

    return;
}



/* ns_desc_twin
   print status of a twin session
   sess    the session
   doc     info about the context
   !stderr  info about the twin session */

void
ns_desc_twin(_ns_sess * sess, char *doc)
{
#ifdef HAVE_TWIN
    if (!sess) {
        D_ESCREEN(("%s: ns_desc_twin called with broken pointer!\n", doc ? doc : ""));
        return;
    }
    D_ESCREEN(("%s: twin status (%s) is %d-%s, %d-%s\n", doc, sess->twin_str,
               Tw_Errno(sess->twin),
               Tw_StrError(sess->twin, Tw_Errno(sess->twin)), Tw_ErrnoDetail(sess->twin), Tw_StrErrorDetail(sess->twin,
                                                                                                            Tw_Errno(sess->twin),
                                                                                                            Tw_ErrnoDetail(sess->
                                                                                                                           twin))));
#else
    USE_VAR(sess);
    USE_VAR(doc);
#endif
}



/* ns_desc_hop
   print basic info about a hop (tunnel, firewall).  mostly for debugging.
   hop:    a hop struct as generated by (eg) ns_attach_by_URL()
   doc:    info about the context
 ! stderr: info about the hop */

static void
ns_desc_hop(_ns_hop * h, char *doc)
{
    if (!h) {
        D_ESCREEN(("%s: ns_desc_hop called with broken pointer!\n", doc ? doc : ""));
        return;
    }

    if (doc)
        D_ESCREEN(("%s:\n", doc));

    D_ESCREEN(("tunnel from localhost:%d to %s:%d to %s:%d is %s.  (delay %d, %d ref%s)\n",
               h->localport, h->fw, h->fwport, h->sess->host, h->sess->port, h->established ? "up" : "down", h->delay, h->refcount,
               h->refcount == 1 ? "" : "s"));
}



/* ns_desc_sess
   print basic info about a session.  mostly for debugging.
   sess:   a session struct as generated by (eg) ns_attach_by_URL()
   doc:    info about the context
 ! stderr: info about the session */

static void
ns_desc_sess(_ns_sess * sess, char *doc)
{
    if (!sess) {
        D_ESCREEN(("%s: ns_desc_sess called with broken pointer!\n", doc ? doc : ""));
        return;
    }
    if (sess->where == NS_LCL)
        D_ESCREEN(("%s: (efuns@%p)\t (user %s) local %s", doc, sess->efuns, sess->user, sess->proto));
    else {
        D_ESCREEN(("%s: (efuns@%p)\t %s://%s%s%s@%s",
                   doc, sess->efuns, sess->proto ? sess->proto : "???", sess->user, sess->pass ? ":" : "",
                   sess->pass ? sess->pass : "", sess->host));
        if (sess->port != NS_DFLT_SSH_PORT)
            D_ESCREEN((":%s", sess->port));
    }
    D_ESCREEN(("%c%s\n", sess->where == NS_LCL ? ' ' : '/', sess->rsrc));
    if (sess->hop)
        ns_desc_hop(sess->hop, NULL);
    if (sess->sysrc)
        D_ESCREEN(("%s: searching for sysrc in %s\n", doc, sess->sysrc));
    if (sess->home)
        D_ESCREEN(("%s: searching for usrrc in %s\n", doc, sess->home));
    D_ESCREEN(("%s: escapes set to ^%c-%c\n", doc, sess->escape + 'A' - 1, sess->literal));
#ifdef NS_HAVE_TWIN
    D_ESCREEN(("%s: twin %s at %p\n", doc, sess->twin_str ? sess->twin_str : "NULL", sess->twin));
#endif
}



/* run a command. uses the terminal's internal run facility.
   converts system/"char *" to exec/"arg **".
   efuns: struct of callbacks into the terminal program.
          ns_run() will fail if no callback to the terminal's "run program"
          (exec) facility is provided.
   cmd:   a string to exec
   <-     whatever the callback returns.  In Eterm, it's a file-descriptor.  */

int
ns_run(_ns_efuns * efuns, char *cmd)
{
    char **args = NULL;
    char *p = cmd;
    int c, n = 0, s = 0;

    if (!efuns || !efuns->execute)
        goto fail;

    if (cmd && *cmd) {          /* count args (if any) */
        D_ESCREEN(("ns_run: executing \"%s\"...\n", cmd));
        do {
            n++;
            while (*p && *p != ' ') {
                if (*p == '\"') {
                    do {
                        p++;
                        if (s)
                            s = 0;
                        else if (*p == '\\')
                            s = 1;
                        else if (*p == '\"')
                            s = 2;
                    }
                    while (*p && s != 2);
                }
                p++;
            }
            while (*p == ' ')
                p++;
        }
        while (*p);

        if (!(args = MALLOC((n + 2) * sizeof(char *))))
            goto fail;

        for (p = cmd, c = 0; c < n; c++) {
            args[c] = p;
            while (*p && *p != ' ') {
                if (*p == '\"') {       /* leave quoting stuff together as one arg */
                    args[c] = &p[1];    /* but remove the quote signs */
                    do {
                        p++;
                        if (s)
                            s = 0;
                        else if (*p == '\\')
                            s = 1;
                        else if (*p == '\"')
                            s = 2;
                    }
                    while (*p && s != 2);
                    *p = '\0';
                }
                p++;
            }
            while (*p == ' ')
                *(p++) = '\0';
        }
        args[c++] = NULL;
    }

    n = efuns->execute(NULL, args);
    if (args)
        FREE(args);
    return n;

  fail:
    return NS_FAIL;
}



/* create a call line. used in ns_attach_ssh/lcl
   tmpl   the template. should contain one %s
   dflt   the default value
   opt    the user-supplied value (or NULL)
   <-     a new MALLOC'd string (or NULL) */

static char *
ns_make_call_el(char *tmpl, char *dflt, char *opt)
{
    int l, r;
    char *p;

    if (tmpl && dflt && *tmpl && strstr(tmpl, "%s")) {
        l = strlen(tmpl) + (opt ? strlen(opt) : strlen(dflt)) - 1L;
        if ((p = MALLOC(l))) {
            r = snprintf(p, l, tmpl, opt ? opt : dflt);
            if ((r >= 0) && (r < l)) {
                return p;
            }
            FREE(p);
        }
    }
    return NULL;
}



static char *
ns_make_call(_ns_sess * sess)
{
    char *call, *tmp = NULL, *screen = NULL, *scream = NULL, *screem = NULL, *twin;

#ifdef NS_HAVE_TWIN
    if (sess->backend == NS_MODE_TWIN) {
        int r, l;

        tmp = sess->rsrc ? STRDUP(sess->rsrc) : ns_make_call_el(NS_TWIN_OPTS, ":0", sess->twin_str);
        l = 1 + strlen(NS_TWIN_CALL) + 2 * strlen(tmp);
        if ((twin = MALLOC(l))) {
            r = snprintf(twin, l, NS_TWIN_CALL, tmp ? tmp : "", tmp ? tmp : "");
#  ifdef NS_PARANOID
            if ((r < 0) || (r > l))
                ns_free(&twin);
#  endif
        }
        ns_free(&tmp);
        return twin;
    }
#else
    USE_VAR(twin);
#endif
    /* unless decidedly in other mode... */
    if (sess->backend != NS_MODE_SCREEN)
        tmp = scream = ns_make_call_el(NS_SCREAM_CALL, NS_SCREAM_OPTS, sess->rsrc);
#ifdef NS_HAVE_SCREEN
    if (sess->backend != NS_MODE_SCREAM)
        tmp = screen = ns_make_call_el(NS_SCREEN_CALL, NS_SCREEN_OPTS, sess->rsrc);
#endif
    if (sess->backend == NS_MODE_NEGOTIATE) {
        int r, l = strlen(NS_SCREEM_CALL) + (scream ? strlen(scream) : 1) + (screen ? strlen(screen) : 1) - 3;

        if ((screem = MALLOC(l))) {
            r = snprintf(screem, l, NS_SCREEM_CALL, scream ? scream : ":", screen ? screen : ":");
#ifdef NS_PARANOID
            if ((r < 0) || (r > l))
                ns_free(&screem);
#endif
        }
        tmp = screem;
    }
    call = ns_make_call_el(NS_WRAP_CALL, tmp, NULL);
    ns_free(&screen);
    ns_free(&scream);
    ns_free(&screem);
    return call;
}



/* attach a local session (using screen/scream)
   sp  the session
   <-  NS_FAIL, or the result of ns_run() */

static int
ns_attach_lcl(_ns_sess ** sp)
{
    _ns_sess *sess;
    char *call;
    int ret = -1;

    if (!sp || !*sp)
        return ret;

    sess = *sp;

    if ((call = ns_make_call(sess))) {
        char *c2 = ns_make_call_el("/bin/sh -c \"%s\"", call, NULL);

        ns_free(&call);
        if (c2) {
            ret = ns_run(sess->efuns, c2);
            ns_free(&c2);
        }
    }
    return ret;
}


static int
ns_wait_for_socket(int port)
{
    int tmp_sock, ret = -1;
    time_t start_time;

    D_ESCREEN(("Waiting for forwarder to begin listening on port %d.\n", port));
    tmp_sock = socket(PF_INET, SOCK_STREAM, 6);
    start_time = time(NULL);
    if (tmp_sock >= 0) {
        struct sockaddr_in addr;
        char timeout = 0;

        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        addr.sin_port = htons(port);

        do {
            usleep(100);
            if ((time(NULL) - start_time) >= NS_TUNNEL_DELAY) {
                timeout = 1;
                break;
            }
        } while (connect(tmp_sock, (struct sockaddr *) &addr, sizeof(struct sockaddr_in)));
        if (timeout) {
            D_ESCREEN((" -> Unable to connect; timeout after %d seconds.\n", NS_TUNNEL_DELAY));
            sleep(1);
            return -1;
        } else {
            ret = time(NULL) - start_time;
            D_ESCREEN((" -> Connected after %d seconds.\n", ret));
        }
        close(tmp_sock);
    }
    return ret;
}

/* attach a remote session (using screen/scream via ssh)
   sp  the session
   <-  -1, or the result of ns_run() */

static int
ns_attach_ssh(_ns_sess ** sp)
{
    _ns_sess *sess;
    char cmd[NS_MAXCMD + 1];
    char esc[] = " -e^\0\0\0";
    char *call, *p;
    int ret;

    if (!sp || !*sp)
        return NS_FAIL;

    sess = *sp;

    p = &esc[3];                /* escapes for screen and compatibles */
    if (sess->escape < ' ') {
        *p++ = '^';
        *p++ = sess->escape + 'A' - 1;
    } else
        *p++ = sess->escape;    /* this should never happen */
    if (sess->literal < ' ') {  /* this should never happen */
        *p++ = '^';
        *p++ = sess->literal + 'A' - 1;
    } else
        *p++ = sess->literal;

    call = ns_make_call(sess);

    if (sess->hop) {
        if (sess->hop->established == NS_HOP_DOWN) {    /* the nightmare foe */
            _ns_efuns *efuns = NULL;

            ret = snprintf(cmd, NS_MAXCMD, "%s %s -p %d -L %d:%s:%d %s@%s",
                           NS_SSH_CALL, NS_SSH_TUNNEL_OPTS, sess->hop->fwport, sess->hop->localport, sess->host, sess->port,
                           sess->user, sess->hop->fw);
            if (ret < 0 || ret > NS_MAXCMD)
                return NS_FAIL;
            D_ESCREEN(("Spawning forwarder:  %s\n", cmd));
            ns_run(sess->efuns, cmd);
            if (NS_EFUN_EXISTS(efuns, sess, NULL, inp_text)) {
                char tmp_buff[] = "Waiting for forwarder...";

                efuns->inp_text((void *) 1, sess->fd, tmp_buff);
            }
            if ((ns_wait_for_socket(sess->hop->localport) < 0) && efuns) {
                char tmp_buff[] = "...timed out.";

                efuns->inp_text((void *) 1, sess->fd, tmp_buff);
            }
        }
        ret = snprintf(cmd, NS_MAXCMD, "%s %s -p %d %s@localhost \"%s%s\"",
                       NS_SSH_CALL, NS_SSH_OPTS, sess->hop->localport, sess->user, call, ((sess->backend == NS_MODE_SCREEN)
                                                                                          || (sess->backend ==
                                                                                              NS_MODE_NEGOTIATE)) ? esc : "");
        D_ESCREEN(("Spawning screen session:  %s\n", cmd));
    } else {
        ret =
            snprintf(cmd, NS_MAXCMD, "%s %s -p %d %s@%s \"%s%s\"", NS_SSH_CALL, NS_SSH_OPTS, sess->port, sess->user, sess->host,
                     call, ((sess->backend == NS_MODE_SCREEN) || (sess->backend == NS_MODE_NEGOTIATE)) ? esc : "");
        D_ESCREEN(("Spawning screen session:  %s\n", cmd));
    }
    ns_free(&call);

    return (ret < 0 || ret > NS_MAXCMD) ? NS_FAIL : ns_run(sess->efuns, cmd);
}



/* ns_attach_by_sess
   attach/create a scream/screen session, locally or over the net.
   sess:   a session struct as generated by (eg) ns_attach_by_URL()
 ! err:    if non-NULL, variable pointed at will contain an error status
   <-      the requested session, or NULL in case of failure.
           a session thus returned must be detached/closed later.
*/

_ns_sess *
ns_attach_by_sess(_ns_sess ** sp, int *err)
{
    _ns_sess *sess;
    int err_dummy;

    if (!err)
        err = &err_dummy;
    *err = NS_INVALID_SESS;

    if (!sp || !*sp)
        return NULL;
    sess = *sp;

    ns_desc_sess(sess, "ns_attach_by_sess()");

    (void) ns_sess_init(sess);

    switch (sess->where) {
        case NS_LCL:
            sess->fd = ns_attach_lcl(&sess);
            break;
        case NS_SU:            /* (fixme) uses ssh, should use su */
            /* local session, but for a different uid. */
            /* FALL-THROUGH */
        case NS_SSH:
            if (!sess->delay) {
                sess->delay = NS_INIT_DELAY ? NS_INIT_DELAY : 1;
            }
            sess->fd = ns_attach_ssh(&sess);
            break;
        default:
            *err = NS_UNKNOWN_LOC;
            goto fail;
    }

    D_ESCREEN(("ns_attach_by_sess: screen session-fd is %d, ^%c-%c\n", sess->fd, sess->escape + 'A' - 1, sess->literal));

    return sess;

  fail:
    return ns_dst_sess(sp);
}



/* ns_attach_by_URL
   parse URL into sess struct (with sensible defaults), then pick up/create
   said session using ns_attach_by_sess()
   url:    URL to create/pick up a session at.
           proto://user:password@host.domain:port  (all parts optional)
           NULL/empty string equivalent to
           screen://current_user@localhost/-xRR
   hop:    one of NULL lclport:fw:fwport fw:fwport lclport:fw
           if set, describes how to tunnel through a fw to access an URL
           describing a target behind said firewall
   ef:     a struct containing callbacks into client (resize scrollbars etc.)
           while setting those callbacks is optional; omitting the struct
           itself seems unwise.
 ! err:    if non-NULL, variable pointed at will contain an error status
   xd:     pointer to extra-data the terminal app wants to associate with
           a session, or NULL
   <-      the requested session, or NULL in case of failure.
           a session thus returned must be detached/closed later.
*/

_ns_sess *
ns_attach_by_URL(char *url, char *hop, _ns_efuns ** ef, int *err, void *xd)
{
    int err_dummy;
    char *p, *d = NULL;
    _ns_sess *sess = ns_new_sess();
    struct passwd *pwe = getpwuid(getuid());

    if (!err)
        err = &err_dummy;
    *err = NS_OOM;

    if (!sess) {
        D_ESCREEN(("ns_attach_by_URL: no session...\n"));
        return NULL;
    }

    D_ESCREEN(("ns_attach_by_URL(%s,%s,%p,%p,%p)\n", url, hop, *ef, err, xd));

    if (url && strlen(url)) {
        char *q;

        if (!(d = STRDUP(url)))
            goto fail;

        if ((q = strstr(d, "://"))) {   /* protocol, if any */
            *q = '\0';
            if (!(sess->proto = STRDUP(d)))
                goto fail;
            q += 3;
        } else
            q = d;

        if ((p = strchr(q, '@'))) {     /* user, if any */
            char *r;

            if (p != q) {       /* ignore empty user */
                *p = '\0';
                if ((r = strchr(q, ':'))) {     /* password, if any */
                    *(r++) = '\0';
                    if (!(sess->pass = STRDUP(r)))      /* password may be empty string! */
                        goto fail;
                }
                sess->user = STRDUP(q);
            }
            q = p + 1;
        }

        if ((p = strchr(q, '/'))) {     /* rsrc, possibly url-encoded */
            *(p++) = '\0';
            if (strlen(p)) {
                char *r = p;
                int f;

                while (*r) {
                    if (*r == '+')
                        *(r++) = ' ';
                    else if ((*r == '%') && (strlen(r) > 2)) {
                        long v;
                        char *e;
                        char b[3];

                        b[0] = r[1];
                        b[1] = r[2];
                        b[2] = '\0';
                        v = strtol(b, &e, 16);
                        if (!*e) {
                            *(r++) = (char) (v & 0xff);
                            memmove(r, &r[2], strlen(&r[2]));
                        }
                    } else
                        r++;
                }
                r = p;
                f = 0;
                while (*r) {
                    if (*r == ' ') {    /* Padding between arguments */
                        while (*r == ' ')
                            r++;
                    } else {
                        if (*r == '-') {
#  ifdef NS_HAVE_TWIN
                            if (!strncmp(NS_TWIN_PARA, &r[1], strlen(NS_TWIN_PARA))) {
                                char *y = strchr(r, '@');

                                if (y && *++y) {
                                    char *z = strchr(y, ':');

                                    if (z) {
                                        *z++ = '\0';
                                        if (*z)
                                            sess->disp = atoi(z);
                                        if (sess->disp < 0 || sess->disp > NS_MAX_PORT)
                                            sess->disp = 0;
                                    }
                                    if (strlen(y))
                                        sess->host = STRDUP(y);
                                }
                            } else
#  endif
#  ifdef NS_HAVE_SCREEN
                            if (*(++r) == 'e') {        /* set escape */
                                char x = 0, y = 0;

                                while (*(++r) == ' ');
                                if ((x = ns_parse_esc(&r)) && (y = ns_parse_esc(&r))) {
                                    sess->escape = x;
                                    sess->literal = y;
                                    sess->escdef = NS_ESC_CMDLINE;
                                }
                            } else if (*r == 'c') {     /* alt screenrc */
                                char *rc, *rx;

                                while (*(++r) == ' ');
                                if ((rx = strchr(r, ' ')))
                                    *rx = '\0';
                                if (*r != '/')
                                    D_ESCREEN(("URL: path for screen's option -c should be absolute (%s)\n", r));
                                if ((rc = STRDUP(r))) {
                                    if (sess->home)     /* this should never happen */
                                        FREE(sess->home);
                                    D_ESCREEN(("URL: searching for rc in %s\n", rc));
                                    sess->home = rc;
                                }
                                if (rx) {
                                    r = rx;
                                    *rx = ' ';
                                }
                            } else
#  endif
                            {
                                NOP;
                            }
                            while (*r && (f || *r != ' ')) {
                                if (*r == '\"')
                                    f = 1 - f;
                                r++;
                            }
                        }
                        while (*r && *r != ' ') /* proceed to space */
                            r++;
                    }
                }

                if (!(sess->rsrc = STRDUP(p)))
                    goto fail;
            }
        }

        if ((p = strchr(q, ':'))) {     /* port, if any */
            *(p++) = '\0';
            if (!*p || !(sess->port = atoi(p)) || sess->port > NS_MAX_PORT) {
                *err = NS_MALFORMED_URL;
                D_ESCREEN(("malformed URL...\n"));
                goto fail;
            }
        }

        if (strlen(q) && !(sess->host = STRDUP(q)))     /* host, if any */
            goto fail;

        FREE(d);
    }

    sess->where = NS_SSH;

    if (!sess->user) {          /* default user (current user) */
        if (!pwe) {
            *err = NS_UNKNOWN_USER;
            D_ESCREEN(("unknown user...\n"));
            goto fail;
        }
        if (!(sess->user = STRDUP(pwe->pw_name)))
            goto fail;
    } else if ((sess->host && strcmp(sess->host, "localhost") && strcmp(sess->host, "127.0.0.1")) || (sess->port > 0)) {
        pwe = NULL;
    } else if (!pwe || strcmp(pwe->pw_name, sess->user)) {      /* user!=current_user */
        sess->where = NS_SU;
        if (!(pwe = getpwnam(sess->user))) {
            *err = NS_UNKNOWN_USER;
            D_ESCREEN(("unknown user...\n"));
            goto fail;
        }
    } else {
        *err = NS_UNKNOWN_USER;
        D_ESCREEN(("unknown user...\n"));
        goto fail;
    }


#ifdef NS_HAVE_SCREEN
    if (getenv("SYSSCREENRC")) {        /* $SYSSCREENRC */
        if (!(sess->sysrc = STRDUP(getenv("SCREENRC"))))
            goto fail;
    } else {
        char *loc[] = {
            "/usr/local/etc/screenrc",  /* official */
            "/etc/screenrc",    /* actual (on SuSE) */
            "/usr/etc/screenrc",
            "/opt/etc/screenrc",
            "/etc/screen/screenrc"
        };
        int n, nloc = sizeof(loc) / sizeof(char *);

        for (n = 0; n < nloc; n++)
            if (!access(loc[n], R_OK)) {
                if (!(sess->sysrc = STRDUP(loc[n])))
                    goto fail;
                n = nloc;
            }
    }

    if (getenv("SCREENRC")) {   /* $SCREENRC */
        sess->home = STRDUP(getenv("SCREENRC"));
    } else if (pwe && !sess->home) {    /* ~/.screenrc */
        if ((sess->home = MALLOC(strlen(pwe->pw_dir) + strlen(NS_SCREEN_RC) + 2)))
            sprintf(sess->home, "%s/%s", pwe->pw_dir, NS_SCREEN_RC);
    }
#endif

    sess->backend = NS_MODE_NEGOTIATE;
    if (sess->proto) {
#warning compiling in libscream
#ifdef NS_HAVE_SCREEN
        if (!strcmp(sess->proto, "screen")) {
            D_ESCREEN(("Using session protocol \"%s\"\n", sess->proto));
            sess->backend = NS_MODE_SCREEN;
        } else
#warning compiling in support for GNU screen
#endif
#ifdef NS_HAVE_TWIN
#warning compiling in support for twin
        if (!strcmp(sess->proto, "twin")) {
            char *twd = getenv("TWDISPLAY");

            D_ESCREEN(("Using session protocol \"%s\"\n", sess->proto));
            sess->backend = NS_MODE_TWIN;

            /* fall back on TWDISPLAY env var only if host not set yet */
            if (twd && (!sess->host || !strlen(sess->host) || !strcmp(sess->host, "localhost"))) {
                char *twdisp = strrchr(twd, ':');

                if (twdisp) {
                    *twdisp++ = '\0';
                    if (*twdisp && sess->disp < 0)      /* fall back on TWDISPLAY display */
                        sess->disp = atoi(twdisp);
                }               /* fall back on TWDISPLAY host */
                if (((!sess->host) || (!strlen(sess->host))) && strlen(twd) && strcmp(twd, "localhost"))
                    sess->host = STRDUP(twd);
            }
            /* this is ugly, but does the intuitive thing.
             * if you specifically want to connect a twin to a non-twin
             * port < 20, make it unambiguous by using the syntax
             * twin://host.dom:port/-twin@:disp  or
             * twin://:port/-twin@host.dom:disp  */
            else if (sess->host && sess->disp < 0 && sess->port >= 0 && sess->port < 20) {
                sess->disp = sess->port;
                sess->port = -1;
            }
        } else
#endif
        if (!strcmp(sess->proto, "scream")) {
            D_ESCREEN(("Using session protocol \"%s\"\n", sess->proto));
            sess->backend = NS_MODE_SCREAM;
        } else {
            *err = NS_UNKNOWN_PROTO;
            D_ESCREEN(("unknown protocol %s...\n", sess->proto));
            fprintf(stderr, "protocol \"%s\" not known...\n", sess->proto);
            goto fail;
        }
    } else {
        D_ESCREEN(("No session protocol specified.\n"));
    }

    if ((sess->disp < 0) || (sess->disp > NS_MAX_PORT))
        sess->disp = 0;

#ifdef NS_HAVE_TWIN
    /* do this *before* host-fallback */
    if (sess->twin_str)
        FREE(sess->twin_str);

    if (sess->twin_str = MALLOC((sess->host ? strlen(sess->host) : 0) + 7))
        sprintf(sess->twin_str, "%s:%d", (sess->host ? sess->host : ""), sess->disp);
#endif

    if (!sess->host) {          /* no host */
        if (!(sess->host = STRDUP("localhost")))
            goto fail;
        if (sess->port <= 0) {  /* no host/port */
            sess->where = NS_LCL;
        }
    } else if ((p = strchr(sess->host, '/')))   /* have host */
        *p = '\0';

    if (sess->port <= 0) {      /* no port -> default port (SSH) */
        if (sess->backend == NS_MODE_TWIN)
            sess->port = ns_get_twin_port();
        else
            sess->port = ns_get_ssh_port();
    }

    if (!sess->efuns && ef && *ef) {
        sess->efuns = ns_ref_efuns(ef);
    }

    sess->userdef = xd;

    if (hop && strlen(hop)) {
        sess->hop = ns_parse_hop(sess, hop);
        if (sess->hop
            && (!strcmp(sess->host, sess->hop->fw) || !strcmp(sess->host, "localhost") || !strcmp(sess->host, "127.0.0.1")))
            D_ESCREEN(("ns_attach_by_URL: routing in circles...\n"));
    }

    *err = NS_SUCC;

    return ns_attach_by_sess(&sess, err);

  fail:
    if (d)
        FREE(d);

    D_ESCREEN(("ns_attach_by_URL: fail...\n"));

    return ns_dst_sess(&sess);
}



/* detach a session and release its memory
   sess  the session
   <-    error code */
int
ns_detach(_ns_sess ** sess)
{
    ns_desc_sess(*sess, "ns_detach");
#ifdef NS_HAVE_TWIN
    if (((*sess)->backend == NS_MODE_TWIN) && (*sess)->twin) {
        Tw_Flush((*sess)->twin);
        Tw_Close((*sess)->twin);
    }
#endif
    (void) ns_dst_sess(sess);
    return NS_SUCC;
}



/****************************************************************************
 ____             _                        _ 
| __ )  __ _  ___| | __      ___ _ __   __| |
|  _ \ / _` |/ __| |/ /____ / _ \ '_ \ / _` |
| |_) | (_| | (__|   <_____|  __/ | | | (_| |
|____/ \__,_|\___|_|\_\     \___|_| |_|\__,_|
                                             

backend abstraction (utils)

   this abstracts the backend against the frontend; the terminal-emulator
   calls these functions without knowing what the backend is. */



/***************************************************************************/
/* display foo */
/***************/



/* toggle last two displays */
int
ns_tog_disp(_ns_sess * s)
{
    if (!s)
        return NS_FAIL;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
/*        return ns_screen_command(s, "\x01\x01"); */
          return ns_statement(s, "other");
          break;
#endif
        default:
            return NS_FAIL;
    }
}

/* go to display #d */
int
ns_go2_disp(_ns_sess * s, int d)
{
    if (!s)
        return NS_FAIL;
    if (s->curr && s->curr->index == d)
        return NS_SUCC;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
          {
/*            char b[] = "\x01_";
              b[1] = '0' + d;
              return ns_screen_command(s, b); */
              char b[] = "select _";
              b[7] = '0' + d;
              return ns_statement(s, b);
          }
          break;
#endif
#ifdef NS_HAVE_TWIN
        case NS_MODE_TWIN:
            {
                tscreen ts = Tw_FirstScreen(s->twin);

                printf("screen: %p\n", ts);
                while (d-- && ts)
                    ts = Tw_NextObj(s->twin, ts);
                if (ts) {
                    Tw_RaiseScreen(s->twin, ts);
                    return NS_SUCC;
                }
            }
            break;
#endif
        default:
            return NS_FAIL;
    }
}

/* toggle monitor mode for disp (if possible). -1 for current disp */
int
ns_mon_disp(_ns_sess * s, int no, int quiet)
{
    if (!s)
        return NS_FAIL;

    D_ESCREEN(("toggling monitoring for display %d\n", no));

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
          if (no >= 0)
              ns_go2_disp(s, no);
          if (quiet == NS_MON_TOGGLE_QUIET)
              s->flags |= NS_SESS_NO_MON_MSG;
          else
              s->flags &= (~NS_SESS_NO_MON_MSG);
/*        return ns_screen_command(s, "\x01M"); */
          return ns_statement(s, "monitor");
          break;
#endif
    }
    return NS_FAIL;
}

/* scrollback buffer mode (if any) */
int
ns_sbb_disp(_ns_sess * s, int no)
{
    if (!s)
        return NS_FAIL;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
          ns_go2_disp(s, no);
/*        return ns_screen_command(s, "\x01\x1b"); */
          return ns_statement(s, "copy");
          break;
#endif
        default:
            return NS_FAIL;
    }
}

/* go to display #+-d */
int
ns_rel_disp(_ns_sess * s, int d)
{
    _ns_disp *x;

    if (!s)
        return NS_FAIL;
    if (!d)
        return NS_SUCC;

    if (!s->curr) {
        if (!(s->curr = s->dsps)) {
            return NS_FAIL;
        }
    }

    x = s->curr;

    if (d < 0) {
        _ns_disp *l;

        for (l = s->dsps; l->next; l = l->next);

        while (d++) {
            if (!(x = x->prvs))
                x = l;
        }
    } else {
        while (d--) {
            if (!(x = x->next))
                x = s->dsps;
        }
    }
    return ns_go2_disp(s, x->index);
}



/* add a client display and a tab.
   s       the session to add to
   after   add after this display (0..n).  -1 to add before disp 0.
   name    NULL: ask. "": let backend choose name.  else:  set name.  */

int
ns_add_disp(_ns_sess * s, int after, char *name)
{
    int ret = NS_FAIL;

    if (!s) {
        return NS_FAIL;
    }

    D_ESCREEN(("ns_add_disp: add %s after #%d\n", name, after));

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
          if (after >= 0)
              ns_go2_disp(s, after);
/*        if (ns_screen_command(s, "\x01\x03") == NS_SUCC) { */
          ret = ns_statement(s, "screen");
          if (ret == NS_SUCC) {
              D_ESCREEN(("Sent \"screen\" command, now renaming tab.\n"));
              if (!name || strlen(name)) {
                  ns_ren_disp(s, -2, name);
              }
              ret = ns_mon_disp(s, -2, NS_MON_TOGGLE_QUIET);
          } else {
              D_ESCREEN(("ns_statement(screen) returned %d\n", ret));
          }
          break;
#endif
#ifdef NS_HAVE_TWIN
        case NS_MODE_TWIN:
            ret = ns_twin_control(s, "twin", TW_MSG_CONTROL_OPEN);
            printf("ns_add_disp: twin add window after %d -> %d\n", after, ret);
            break;
#endif
    }
    return ret;
}



/* move client display #fm to display slot #to */
int
ns_mov_disp(_ns_sess * s, int fm, int to)
{
    _ns_disp *d;

    if (!s) {
        return NS_FAIL;
    }

    if (fm == to)
        return NS_SUCC;

    if ((fm < 0) || (to < 0))
        return NS_FAIL;

    if (!(d = s->dsps))         /* this should never happen */
        return NS_FAIL;

    fm = disp_get_screen_by_real(s, fm);
    to = disp_get_screen_by_real(s, to);

    if (fm == to)               /* ??? */
        return NS_SUCC;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
        case NS_MODE_SCREEN:
            D_ESCREEN(("ns_mov_disp: move #%d to #%d\n", fm, to));
            ns_mov_screen_disp(s, fm, to);
            break;
#endif
    }
    return NS_FAIL;
}

/* resize display #d to w*h */
int
ns_rsz_disp(_ns_sess * s, int d, int w, int h)
{
    USE_VAR(d);
    USE_VAR(w);
    USE_VAR(h);

    if (!s) {
        return NS_FAIL;
    }

    switch (s->backend) {
        default:
            return NS_FAIL;
    }
}

/* remove display #d */
int
ns_rem_disp(_ns_sess * s, int d, int ask)
{
    char *i = NULL;
    int ret = NS_FAIL;

    if (!s) {
        return NS_FAIL;
    }

    if (!s->curr) {
        if (!(s->curr = s->dsps))
            return NS_FAIL;
    }

    if (d < 0) {
        d = s->curr->index;
    }

    if (ask) {
        (void) ns_inp_dial(s, "Really delete this display?", 1, &i, NULL);
        if (!i || !*i)
            return NS_FAIL;
    }

    if (*i == 'y' || *i == 'Y') {
        switch (s->backend) {
#ifdef NS_HAVE_SCREEN
          case NS_MODE_SCREEN:
              ns_go2_disp(s, d);
/*            ret = ns_screen_command(s, "\x01ky\r"); */
              if ((ret = ns_statement(s, "kill")) == NS_SUCC)
                  ret = ns_screen_command(s, "y\r");
              break;
#endif
        }
    }

    if (i)
        FREE(i);

    return ret;
}

/* rename display
   s    session display is in
   d       -1   current display
           -2   display was just created and is not in list yet
          >=0   index of the display
   name   NULL  ask for name
         !NULL  the name
   <-   error code */

int
ns_ren_disp(_ns_sess * s, int d, char *name)
{
    char *i = NULL, *n;
    size_t l = 0;
    int ret = NS_FAIL;

    if (!s) {
        return NS_FAIL;
    }

    D_ESCREEN(("Renaming display %d to %s\n", d, ((name) ? (name) : ("dialog box input"))));
    if (!s->curr) {
        if (!(s->curr = s->dsps))
            return NS_FAIL;
    }

    if (d == -1)
        d = s->curr->index;

    if (!name || !*name) {      /* ask */
        if (d == -2)
            l = 32;             /* dirty, but effective */
        else {
            i = s->curr->name;
            l = strlen(i);
        }
        D_ESCREEN(("Invoking input dialog; i == %s, l == %lu\n", NONULL(i), l));
        (void) ns_inp_dial(s, "Enter a new name for the current display", 12, &i, NULL);
        D_ESCREEN((" -> Back, new name is:  \"%s\"\n", NONULL(i)));
        if (!i || !*i)
            return NS_FAIL;
    }

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
        case NS_MODE_SCREEN:
            if ((n = MALLOC(strlen(i ? i : name) + l + 1))) {
                if (d >= 0)
                    ns_go2_disp(s, d);
                strcpy(&n[l], i ? i : name);    /* copy new name */
                while (l)       /* prepend backspaces */
                    n[--l] = '\x08';
                ret = ns_screen_xcommand(s, 'A', n);    /* rename */
                FREE(n);
            }
            break;
#endif
    }

    return ret;
}

/* log activity in display #d to file "logfile" */
int
ns_log_disp(_ns_sess * s, int d, char *logfile)
{
    USE_VAR(d);
    USE_VAR(logfile);

    if (!s) {
        return NS_FAIL;
    }

    switch (s->backend) {
        default:
            return NS_FAIL;
    }
}



/***************************************************************************/
/* region/window foo */
/*********************/


int
ns_tog_region(_ns_sess * s, _ns_disp * d)
{
    return ns_magic_disp(&s, &d);
}

int
ns_go2_region(_ns_sess * s, _ns_disp * d, int n)
{
    USE_VAR(n);
    return ns_magic_disp(&s, &d);
}



int
ns_rel_region(_ns_sess * s, _ns_disp * d, int n)
{
    int ret = NS_FAIL;

    if (!n)
        return NS_SUCC;

    if (ns_magic_disp(&s, &d) == NS_FAIL)
        return NS_FAIL;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
          if (n < 0)
              return NS_FAIL;
          do {
/*            ret = ns_screen_command(s, "\x01\x09"); */
              ret = ns_statement(s, "focus");
          } while (--n && (ret == NS_SUCC));
          break;
#endif
    }
    return ret;
}



int
ns_add_region(_ns_sess * s, _ns_disp * d, int after, char *name)
{
    int ret = NS_FAIL;

    USE_VAR(after);
    USE_VAR(name);

    if (ns_magic_disp(&s, &d) == NS_FAIL)
        return NS_FAIL;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
/*        ret = ns_screen_command(s, "\x01S"); */
          ret = ns_statement(s, "split");
          break;
#endif
    }
    return ret;
}



int
ns_rsz_region(_ns_sess * s, _ns_disp * d, int r, int w, int h)
{
    USE_VAR(r);
    USE_VAR(w);
    USE_VAR(h);

    return ns_magic_disp(&s, &d);
}

int
ns_rem_region(_ns_sess * s, _ns_disp * d, int r, int ask)
{
    int ret = NS_FAIL;

    USE_VAR(r);
    USE_VAR(ask);

    if (ns_magic_disp(&s, &d) == NS_FAIL)
        return NS_FAIL;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
/*        ret = ns_screen_command(s, "\x01X"); */
          ret = ns_statement(s, "remove");
          break;
#endif
    }
    return ret;
}



int
ns_one_region(_ns_sess * s, _ns_disp * d, int r)
{
    int ret = NS_FAIL;

    USE_VAR(r);

    if (ns_magic_disp(&s, &d) == NS_FAIL)
        return NS_FAIL;

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
      case NS_MODE_SCREEN:
/*        ret = ns_screen_command(s, "\x01Q"); */
          ret = ns_statement(s, "only");
          break;
#endif
    }
    return ret;
}



int
ns_mov_region(_ns_sess * s, _ns_disp * d, int fm, int to)
{
    USE_VAR(fm);
    USE_VAR(to);
    return ns_magic_disp(&s, &d);
}

int
ns_ren_region(_ns_sess * s, _ns_disp * d, int r, char *name)
{
    USE_VAR(r);
    USE_VAR(name);
    return ns_magic_disp(&s, &d);
}

int
ns_log_region(_ns_sess * s, _ns_disp * d, int r, char *logfile)
{
    USE_VAR(r);
    USE_VAR(logfile);
    return ns_magic_disp(&s, &d);
}

int
ns_mon_region(_ns_sess * s, _ns_disp * d, int r)
{
    USE_VAR(r);
    return ns_magic_disp(&s, &d);
}

int
ns_sbb_region(_ns_sess * s, _ns_disp * d, int r)
{
    USE_VAR(r);
    return ns_magic_disp(&s, &d);
}



/***************************************************************************/
/* session foo */
/***************/



/* scroll horizontally to column x (dummy) */
int
ns_scroll2x(_ns_sess * s, int x)
{
    USE_VAR(x);

    if (!s) {
        return NS_FAIL;
    }

    return NS_FAIL;
}

/* scroll vertically so line y of the scrollback buffer is the top line */
int
ns_scroll2y(_ns_sess * s, int y)
{
    USE_VAR(y);

    if (!s) {
        return NS_FAIL;
    }

    return NS_FAIL;
}

/* force an update of the status line */
int
ns_upd_stat(_ns_sess * s)
{
    D_ESCREEN(("Forcing update of status line for session 0x%p.\n", s));
    if (!s) {
        return NS_FAIL;
    }

    switch (s->backend) {
        /* FIXME:  Causes other problems.  case NS_MODE_NEGOTIATE:*/
#ifdef NS_HAVE_SCREEN
        case NS_MODE_SCREEN:
            D_ESCREEN(("Calling ns_screen_command(0x%p, %s)\n", s, NS_SCREEN_UPDATE));
            return ns_screen_command(s, NS_SCREEN_UPDATE);
#endif
        default:
            D_ESCREEN(("Failed; backend set to %d\n", s->backend));
            return NS_FAIL;
    }
}

/* send a statement to screen.  ("^A:" is automatically prefixed)
   the command is parsed before execution (so if e.g. the command changes
   the hotkey, we'll know).
   s  the session
   c  the statement.  if none is given, a dialog is opened.
   <- an error-code */
int
ns_statement(_ns_sess * s, char *c)
{
    int ret = NS_FAIL;
    char *i = NULL;
    char x, y;

    if (!s) {
        return NS_FAIL;
    }

    y = x = s->escape;

    if (!c || !*c) {
        (void) ns_inp_dial(s, "Enter a command to send to the text-window manager", 64, &i,
#ifdef NS_HAVE_SCREEN
                           ns_inp_tab
#else
                           NULL
#endif
            );
        if (!i || !*i)
            return NS_FAIL;
    }

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
        case NS_MODE_SCREEN:
            if ((ret = ns_parse_screen_cmd(s, i ? i : c, NS_ESC_INTERACTIVE)) == NS_SUCC) {
                if (s->escape != x) {
                    y = s->escape;
                    s->escape = x;
                }
                ret = ns_screen_xcommand(s, NS_SCREEN_CMD, i ? i : c);
                D_ESCREEN(("ns_screen_xcommand(%10p, NS_SCREEN_CMD, %s) returned %d.\n",
                           s, NONULL(((i) ? (i) : (c))), ret));
                s->escape = y;
            } else if (ret == NS_NOT_ALLOWED) {
                ns_inp_dial(s, "Sorry, David, I cannot allow that.", 0, NULL, NULL);
            }
            break;
#endif
        default:
            ret = NS_FAIL;
    }

    if (i)
        FREE(i);

    D_ESCREEN(("Returning %d\n", ret));
    return ret;
}

int
ns_reset(_ns_sess * s, int type)
{
    USE_VAR(type);

    if (!s) {
        return NS_FAIL;
    }

    switch (s->backend) {
#ifdef NS_HAVE_SCREEN
        case NS_MODE_SCREEN:
            return ns_screen_command(s, NS_SCREEN_INIT);
#endif
        default:
            return NS_FAIL;
    }
}

char *
ns_get_url(_ns_sess * s, int d)
{
    int r, l;
    char *u;
    char esc[] = "^_\0";
    char lit[] = "^_\0";

    USE_VAR(d);

    if (!s) {
        return NULL;
    }

    l = ((s->proto) ? strlen(s->proto) + 3 : 0) + strlen(s->user) + 1 + strlen(s->host) + 1 + 5 + 1 +
        ((s->rsrc) ? strlen(s->rsrc) : 0) + 7 + (s->name ? strlen(s->name) + 4 : 0) + 1;

    if ((u = MALLOC(l + 1))) {
        if (!s->escape) {
            esc[0] = '\0';
        } else if (s->escape < ' ') {
            esc[1] = s->escape + 'A' - 1;
        } else {
            esc[0] = s->escape;
            esc[1] = '\0';
        }
        if (!s->literal) {
            lit[0] = '\0';
        } else if (s->literal < ' ') {
            lit[1] = s->literal + 'A' - 1;
        } else {
            lit[0] = s->literal;
            lit[1] = '\0';
        }
        r = snprintf(u, l, "%s%s%s@%s:%d/%s%s%s%s%s%s", s->proto ? s->proto : "", s->proto ? "://" : "", s->user, s->host, s->port,
                     ((s->rsrc) ? s->rsrc : ""), ((s->escape) ? "+-e" : ""), esc, ((s->escape) ? lit : ""),
                     ((s->name) ? "+-x+" : ""), ((s->name) ? s->name : ""));
        D_ESCREEN(("ns_get_url: URL is %s\n", u));
        if ((r >= 0) && (r < l)) {
            return u;
        }
        FREE(u);
    }

    return NULL;
}



/****************************************************************************
 _            _                                 _  __ _      
| |___      _(_)_ __        ___ _ __   ___  ___(_)/ _(_) ___ 
| __\ \ /\ / / | '_ \ _____/ __| '_ \ / _ \/ __| | |_| |/ __|
| |_ \ V  V /| | | | |_____\__ \ |_) |  __/ (__| |  _| | (__ 
 \__| \_/\_/ |_|_| |_|     |___/ .__/ \___|\___|_|_| |_|\___|
                               |_|
twin-sepcific routines

  these routines handle a specific backend, the "twin" program. */


#ifdef NS_HAVE_TWIN

int
ns_twin_command(_ns_sess * sess, udat type, byte * port, udat cmd, byte * data)
{
    udat l = 0;
    tmsgport msgport;
    tmsg msg;
    uldat err;
    byte ret;

    if (data)
        l = strlen(data);

    if (port) {
        if ((msgport = Tw_FindMsgPort(sess->twin, TW_NOID, strlen(port), port))) {
            if (type == TW_MSG_USER_CONTROL) {
                tevent_control EventC;

                if ((msg = Tw_CreateMsg(sess->twin, TW_MSG_USER_CONTROL, l + TW_SIZEOF_TEVENT_CONTROL))) {
                    EventC = &msg->Event.EventControl;
                    EventC->W = TW_NOID;
                    EventC->Code = cmd;
                    EventC->Len = l;
                    EventC->X = EventC->Y = 0;

                    if (l)
                        memcpy(EventC->Data, data, l);

                    if ((ret = Tw_SendMsg(sess->twin, msgport, msg))) {
                        printf("controlMsg <- %d\n", ret);
                        return NS_SUCC;
                    }
                }
            } else {
                tevent_clientmsg EventC;

                if ((msg = Tw_CreateMsg(sess->twin, TW_MSG_USER_CLIENTMSG, l + TW_SIZEOF_TEVENT_CLIENTMSG))) {
                    EventC->W = TW_NOID;
                    EventC->Code = cmd;
                    EventC->Len = l;
                    if (l)
                        memcpy(EventC->Data.b, data, l);
                    if ((ret = Tw_SendMsg(sess->twin, msgport, msg))) {
                        printf("clientMsg <- %d\n", ret);
                        return NS_SUCC;
                    }
                }
            }
        } else {
            D_ESCREEN(("msgport \"%s\" not found\n", port));
            return NS_FAIL;
        }
    } else {
        D_ESCREEN(("no msgport given\n"));
        return NS_FAIL;
    }

    err = TwErrno;
    D_ESCREEN(("libTw error: %s%s\n", TwStrError(err), TwStrErrorDetail(err, TwErrnoDetail)));
    return NS_FAIL;
}

#endif

/****************************************************************************
                                                        _  __ _      
 ___  ___ _ __ ___  ___ _ __        ___ _ __   ___  ___(_)/ _(_) ___ 
/ __|/ __| '__/ _ \/ _ \ '_ \ _____/ __| '_ \ / _ \/ __| | |_| |/ __|
\__ \ (__| | |  __/  __/ | | |_____\__ \ |_) |  __/ (__| |  _| | (__ 
|___/\___|_|  \___|\___|_| |_|     |___/ .__/ \___|\___|_|_| |_|\___|
                                       |_|                           
screen-specific routines

   these routines handle a specific backend, the GNU "screen" program. */



#ifdef NS_HAVE_SCREEN

/* ns_swp_screen_disp - swap screen displays
   s   session
   fm  from (old index) 
   to  to   (new index)
   <-  error code */

static int
ns_swp_screen_disp(_ns_sess * s, int fm, int to)
{
/*  char *t2 = "\x01:number %d\r"; */
    char *t2 = "number %d";
    char b[NS_MAXCMD + 1];
    int l;
    _ns_disp *d, *d2, *n;

#  ifdef NS_PARANOID
    if ((fm > 9999) || (to > 9999))
        return NS_FAIL;
#  endif

    if (!s->curr || s->curr->index != fm) {     /* switch to source disp if necessary */
/*      char *t1 = "\x01'%d\r"; */
        char *t1 = "select %d";

        if (!(s->curr = disp_fetch(s, fm))) {
            return NS_FAIL;
        }

        l = snprintf(b, NS_MAXCMD, t1, fm);
#  ifdef NS_PARANOID
        if ((l <= 0) || (l > NS_MAXCMD)) {
            return NS_FAIL;
        }
#  endif

/*      (void) ns_screen_command(s, b); */
        (void) ns_statement(s, b);
    }

    l = snprintf(b, NS_MAXCMD, t2, to);
#  ifdef NS_PARANOID
    if ((l <= 0) || (l > NS_MAXCMD)) {
        return NS_FAIL;
    }
#  endif

/*  (void) ns_screen_command(s, b); */
    (void) ns_statement(s, b);

    d2 = disp_fetch(s, to);

    s->curr->index = to;

    if (d2)                     /* target did exist => screen swapped them, so adjust tgt index */
        d2->index = fm;

    d = s->dsps;
    while (d) {
        if ((n = d->next) && (d->index > n->index)) {   /* must... sort... */
            for (d2 = n; d2->next && (d2->index <= d->index); d2 = d2->next);

            if (d->prvs)        /* remove offender from list */
                d->prvs->next = d->next;
            else
                s->dsps = d->next;
            if (d->next)
                d->next->prvs = d->prvs;

            d->prvs = d2;
            if ((d->next = d2->next))
                d2->next->prvs = d;
            d2->next = d;

            d = s->dsps;
        } else
            d = d->next;
    }

    return NS_SUCC;
}


/* ns_mov_screen_disp - move a screen display to a new position
   this does some magic to implement an "insert" using screen's "swap"
   s   session
   fm  from (old index) 
   to  to   (new index)
   <-  error code */

static int
ns_mov_screen_disp(_ns_sess * s, int fm, int to)
{
    _ns_efuns *efuns;
    _ns_disp *d, *d2 = NULL;
    int n = 1;

#  ifdef NS_PARANOID
    if (!(d = s->dsps))         /* this should never happen */
        return NS_FAIL;
#  endif

    while (d && d->next) {
        n++;
        if (d->index == to)
            d2 = d;
        d = d->next;
    }

    if (d2) {                   /* target exist, do the whole enchilada */
        if ((d2->prvs) && (d2->prvs->index == fm)) {    /* special case: swap */
            ns_swp_screen_disp(s, fm, to);
        } else {
            while (d && (d->index >= to)) {
                ns_swp_screen_disp(s, d->index, d->index + 1);
                d = d->prvs;
            }

            ns_swp_screen_disp(s, fm + ((fm > to) ? 1 : 0), to);

            /* done. now unsparse. */
            if (to > fm) {      /* moved right */
                d = s->dsps;
                while (d->index <= fm)
                    d = d->next;
                while (d) {
                    ns_swp_screen_disp(s, d->index, d->index - 1);
                    d = d->next;
                }
            }
        }
    } else if (d->index == to) {        /* kinda ugly : ( */
        if ((to - fm) == 1) {   /* swap last two */
            ns_swp_screen_disp(s, fm, to);
        } else {                /* move before last */
            while (d && (d->index >= to)) {     /* renumber */
                ns_swp_screen_disp(s, d->index, d->index + 1);
                d = d->prvs;
            }

            ns_swp_screen_disp(s, fm, to);

            /* done. now unsparse. */
            d = s->dsps;
            while (d->index <= fm)
                d = d->next;
            while (d) {
                ns_swp_screen_disp(s, d->index, d->index - 1);
                d = d->next;
            }
        }
    } else {                    /* no target, simple renumber */
        ns_swp_screen_disp(s, fm, to);
    }

    s->curr = NULL;
    ns_dst_dsps(&(s->dsps));

    if (NS_EFUN_EXISTS(efuns, s, NULL, expire_buttons))
        efuns->expire_buttons(s->userdef, n);

    ns_upd_stat(s);
    return NS_SUCC;
}



/* send a command string to a session, using the appropriate escape-char
   sess  the session
   cmd   the command string.  escapes must be coded as NS_SCREEN_ESCAPE;
         this routine will convert the string to use the escapes actually
         used in the session
   <-    error code */

int
ns_screen_command(_ns_sess * sess, char *cmd)
{
    _ns_efuns *efuns;
    char *c;
    int ret = NS_SUCC;

    D_ESCREEN(("Sending command \"%s\"\n", NONULL(cmd)));
    if (!cmd || !*cmd) {
        return NS_FAIL;
    }

    if (NS_EFUN_EXISTS(efuns, sess, NULL, inp_text)) {
        if ((c = STRDUP(cmd))) {
            char *p;

            for (p = c; *p; p++) {
                if (*p == NS_SCREEN_ESCAPE) {
                    *p = sess->escape;
                }
            }
            ns_desc_string(c, "ns_screen_command: xlated string");
            D_ESCREEN(("Calling inp_text(NULL, %d, %s) with ret == %d\n", sess->fd, NONULL(c), ret));
            efuns->inp_text(NULL, sess->fd, c);
            FREE(c);
        } else {
            /* out of memory */
            ret = NS_OOM;
        }
    } else {
        ret = NS_EFUN_NOT_SET;
        D_ESCREEN(("ns_screen_command: sess->efuns->inp_text not set!\n"));
    }
    D_ESCREEN(("Returning %d\n", ret));
    return ret;
}



/* send a single command string to screen, adding the equiv of ^A:
   s     the session
   cmd   the command string
   <-    error code */

int
ns_screen_xcommand(_ns_sess * s, char prefix, char *cmd)
{
    char *i;
    int ret = NS_OOM;

    if ((i = MALLOC(strlen(cmd) + 4))) {
        size_t l = strlen(cmd) + 2;

        strcpy(&i[2], cmd);
        i[0] = s->escape;
        i[1] = prefix;
        i[l] = '\n';
        i[++l] = '\0';
        ret = ns_screen_command(s, i);
        FREE(i);
    }
    D_ESCREEN(("Returning %d\n", ret));
    return ret;
}



/* tab completion for screen-commands
  !b  current entry (changes)
   l  number of characters to compare in current entry
   m  maximum number of characters in entry (size of input buffer)
   <- error code */

static int
ns_inp_tab(void *xd, char *b, size_t l, size_t m)
{
    char *sc[] = { "acladd", "addacl", "aclchg", "chacl", "acldel", "aclgrp",
        "aclumask", "umask", "activity",
        "allpartial", "at", "attrcolor", "autonuke", "bce",
        "bell_msg", "bind", "bindkey", "break", "breaktype",
        "bufferfile", "c1", "caption", "charset", "chdir",
        "clear", "compacthist", "console", "copy",
        "crlf", "debug", "defc1", "defautonuke", "defbce",
        "defbreaktype", "defcharset", "defflow", "defgr",
        "defencoding", "deflog", "deflogin", "defmode",
        "defmonitor", "defobuflimit", "defscrollback",
        "defshell", "defsilence", "defslowpast", "defutf8",
        "defwrap", "defwritelock", "defzombie", "detach",
        "dinfo", "displays", "digraph", "dumptermcap",
        "escape", "eval", "exec", "fit", "flow", "focus", "gr",
        "hardcopy", "hardcopy_append", "hardcopydir",
        "height", "help", "history", "ignorecase", "encoding",
        "kill", "license", "lockscreen", "log", "logfile",
        "login", "logtstamp", "mapdefault", "mapnotnext",
        "maptimeout", "markkeys", "meta", "monitor",
        "multiuser", "nethack", "next", "nonblock", "number",
        "obuflimit", "only", "other", "partial", "password",
        "paste", "pastefont", "pow_break", "pow_detach",
        "prev", "printcmd", "process", "quit", "readbuf",
        "readreg", "redisplay", "remove", "removebuf", "reset",
        "resize", "screen", "scrollback", "select",
        "sessionname", "setenv", "setsid", "shell",
        "shelltitle", "silence", "silencewait", "sleep",
        "slowpast", "source", "sorendition", "split", "stuff",
        "su", "suspend", "term", "termcap", "terminfo",
        "termcapinfo", "unsetenv", "utf8", "vbell",
        "vbell_msg", "vbellwait", "verbose", "version",
        "width", "windowlist", "windows", "wrap", "writebuf",
        "writelock", "xoff", "xon", "zombie"
    };

    _ns_efuns *efuns;
    _ns_sess *s = (_ns_sess *) xd;
    int nsc = sizeof(sc) / sizeof(char *);

    if (NS_EFUN_EXISTS(efuns, s, NULL, inp_tab))
        return efuns->inp_tab((void *) s, sc, nsc, b, l, m) < 0 ? NS_FAIL : NS_SUCC;

    D_ESCREEN(("ns_screen_command: sess->efuns->inp_tab not set!\n"));
    return NS_EFUN_NOT_SET;
}



/* parse argument to screen's "escape" statement.
   x   points to the char to process
       screen-manual says this can be one of x ^X \123 or \\ \^ ...
  !x   the pointer is advanced to the next segment (from esc to literal etc.)
   <-  return as char ('\0' -> fail) */

char
ns_parse_esc(char **x)
{
    char r = '\0';

    if (**x == '\\') {
        (*x)++;
        r = **x;
        if (r >= '0' && r <= '7') {     /* octal, otherwise literal */
            char b[4] = "\0\0\0";
            char *e = *x;
            size_t l = 0;

            while ((*e >= '0' && *e <= '7') && (l < 3)) {       /* can't use endptr here : ( */
                e++;
                l++;
            }
            *x = &e[-1];
            while (--l)
                b[l] = *(--e);
            r = (char) strtol(b, &e, 8);
        }
    } else if (**x == '^') {
        (*x)++;
        r = **x;
        if (r >= 'A' && r <= 'Z')
            r = 1 + r - 'A';
        else if (r >= 'a' && r <= 'z')
            r = 1 + r - 'a';
        else
            r = '\0';
    } /* malformed */
    else
        r = **x;

    if (**x)
        (*x)++;
    return r;
}



/* ns_parse_screen_cmd
   parse a command the user intends to send to the screen program,
   either via .screenrc or using ^A:
   s       the affected (current) session.  s->current should be set.
   p       the command
   whence  which parsing stage (screenrc, interactive, ...)
   <-  error code */

int
ns_parse_screen_cmd(_ns_sess * s, char *p, ns_esc_whence whence)
{
    char *p2;
    long v1 = -1;

    if (!p || !*p)
        return NS_FAIL;

    if ((p2 = strchr(p, ' '))) {        /* first argument */
        char *e;

        while (isspace(*p2))
            p2++;
        v1 = strtol(p2, &e, 0); /* magic conversion mode */
        if ((p2 == e) || (v1 < 0))
            v1 = -1;
    }
#define IS_CMD(b) (strncasecmp(p,b,strlen(b))==0)
    if (!p2) {
        D_ESCREEN(("screenrc: ignoring  \"%s\" without an argument...\n", p));
        /* must return success so it's fowarded to screen in interactive mode.
           that way, the user can read the original reply instead of a fake
           one from us. */
        return NS_SUCC;
    } else if (IS_CMD("defescape"))
        D_ESCREEN(("screenrc: ignoring  \"defescape\", did you mean \"escape\"?\n"));
    else if (IS_CMD("defhstatus") || IS_CMD("hardstatus") || IS_CMD("echo") || IS_CMD("colon") || IS_CMD("wall") ||
#ifdef NS_PARANOID
             IS_CMD("nethack") ||
#endif
             IS_CMD("info") || IS_CMD("time") || IS_CMD("title") || IS_CMD("lastmsg") || IS_CMD("msgwait") || IS_CMD("msgminwait")) {
        D_ESCREEN(("screenrc: ignoring  \"%s\", not applicable...\n", p));
        return NS_NOT_ALLOWED;
    } else if (IS_CMD("escape")) {
        char x = 0, y = 0;

        if ((x = ns_parse_esc(&p2)) && (y = ns_parse_esc(&p2))) {
            if (s->escdef == NS_ESC_CMDLINE) {
                D_ESCREEN(("screenrc: ignoring  \"escape\"; overridden on command-line...\n", x, y));
                return NS_NOT_ALLOWED;
            } else {
                s->escape = x;
                s->literal = y;
                s->escdef = whence;
                return NS_SUCC;
            }
        } else
            D_ESCREEN(("screenrc: ignoring  \"escape\" because of invalid arguments %o %o...\n", x, y));
    } else if (IS_CMD("defscrollback")) {
        if (v1 < NS_SCREEN_DEFSBB)
            D_ESCREEN(("screenrc: ignoring  \"%s\" for value < %d...\n", p, NS_SCREEN_DEFSBB));
        else {
            s->dsbb = v1;
            return NS_SUCC;
        }
    } else if (IS_CMD("scrollback")) {
        if (v1 < NS_SCREEN_DEFSBB)
            D_ESCREEN(("screenrc: ignoring  \"%s\" for value < %d...\n", p, NS_SCREEN_DEFSBB));
        else {
            if (!s->curr)
                s->curr = s->dsps;
            if (!s->curr)
                D_ESCREEN(("screenrc: ignoring  \"%s\", cannot determine current display!?...\n", p));
            else
                s->curr->sbb = v1;
            return NS_SUCC;
        }
    } else {
        D_ESCREEN(("screenrc: bored now \"%s\"\n", p));
        return NS_SUCC;
    }
    return NS_FAIL;
}



/* ns_parse_screen_key
   parse and forward a screen-hotkey
   s    the session to forward to
   c    the character following the escape-char.  (when we got here,
        we already got (and threw out) a screen-escape, so we'll have
        to also send one if we ever forward c to the screen program.
   <-   error code */

int
ns_parse_screen_key(_ns_sess * s, char c)
{
    int ret = NS_SUCC;
    char b[3];

    b[0] = s->escape;
    b[1] = c;
    b[2] = '\0';

    if (c < 27)
        D_ESCREEN(("screen_key: ^%c-^%c %d\n", s->escape + 'A' - 1, c + 'A' - 1, c));
    else
        D_ESCREEN(("screen_key: ^%c-%c %d\n", s->escape + 'A' - 1, c, c));

    switch (c) {
        case NS_SCREEN_CMD:    /* send command (statement) to screen server */
            ns_statement(s, NULL);
            break;
        case NS_SCREEN_RENAME: /* rename current display */
            ret = ns_ren_disp(s, -1, NULL);
            break;
        case NS_SCREEN_KILL:
            ret = ns_rem_disp(s, -1, TRUE);
            break;
        default:
            ret = ns_screen_command(s, b);
    }

    return ret;
}



/* ns_parse_screen_interactive
   parse a whole string that may contain screen-escapes that should be
   handled interactively (that should open dialog boxes etc.).
   this will normally be called by menus, buttons etc. that want to send
   input without generating X events for the keystrokes (real keystrokes
   do not come through here; the keyboard-handler should call
   ns_parse_screen_key() directly when it sees the session's escape-char).
   s   the session in question
   c   the string to parse
   <-  error code */

int
ns_parse_screen_interactive(_ns_sess * sess, char *c)
{
    char *s, *p, *o;

    if (!c || !*c)
        return NS_FAIL;
#ifdef NS_PARANOID
    if (!(s = o = STRDUP(c)))
        return NS_FAIL;
#else
    s = c;
#endif

    p = s;

    while ((p = strchr(s, NS_SCREEN_ESCAPE))) {
        *p = '\0';
        (void) ns_screen_command(sess, s);
        *p = NS_SCREEN_ESCAPE;
        if (*(++p))
            ns_parse_screen_key(sess, *(p++));
        s = p;
    }
    (void) ns_screen_command(sess, s);

#ifdef NS_PARANOID
    FREE(o);
#endif

    return NS_SUCC;
}



/* ns_weird_screen -- damage control
   screen        the offending session
   doc           string specifying the context
  !stderr        a description of the problem
  !err_inhibit   the problem-type is marked so we don't rewarn.
   <-            error code (always NS_FAIL) */

static int
ns_screen_weird(_ns_sess * screen, long type, char *doc)
{
    if (!(err_inhibit & type)) {
        err_inhibit |= type;
        ns_desc_sess(screen, "ns_screen_weird");
        fprintf(stderr, "parse_screen: %s (%ld) screen sent weird stuff.\n"
                "This should never happen. It is assumed that you use a\n"
                "rather unusual configuration for \"screen\".   Please\n"
                "send the result of 'screen --version' to <scream@azundris.com>\n"
                "(together with your ~/.screenrc and /etc/screenrc if present).\n"
                "If at all possible, please also run 'Eterm -e screen' and make\n"
                "a screenshot of the offending window (and the window only, the\n"
                "beauty of your desktop is not relevant to this investigation. : ).\n", doc, type);
    }
    (void) ns_upd_stat(screen);
    return NS_FAIL;
}



/* ns_parse_screenrc -- read the user's screenrc (if we can find it),
   parse it (we need to know if she changes the escapes etc.), and
   send it to the actually screen
   s       the session
   fn      name of the file in question
   whence  which screenrc are we in?
   <-      error code */

static int
ns_parse_screenrc(_ns_sess * s, char *fn, ns_esc_whence whence)
{
    int fd = -1;
    char *rc = NULL;

    if (fn) {
        struct stat st;
        ssize_t rd = 0;

        if ((fd = open(fn, 0)) >= 0) {
            if (!fstat(fd, &st)) {
                if ((rc = MALLOC(st.st_size + 1))) {
                    char *p;

                    while (((rd = read(fd, rc, st.st_size)) < 0) && (errno == EINTR));
                    if (rd < 0)
                        goto fail;
                    rc[rd] = '\0';

                    p = rc;
                    while (*p) {
                        char *p2 = p, *n;
                        int f = 0;

                        while (*p2 && *p2 != '\n' && *p2 != '\r')       /* find EOL */
                            p2++;
                        n = p2;
                        while (*n == '\r' || *n == '\n')        /* delete EOL */
                            *(n++) = '\0';
                        while (isspace(*p))
                            p++;

                        p2 = p; /* on first non-white */
                        while (*p2) {
                            if (*p2 == '\\') {
                                p2++;
                                if (*p2)        /* sanity check */
                                    p2++;
                            } else {
                                if (*p2 == '\"')
                                    f = 1 - f;
                                if (!f && *p2 == '#')   /* comment, kill to EOL */
                                    *p2 = '\0';
                                else
                                    p2++;
                            }
                        }

                        if (strlen(p))  /* any commands in line? */
                            ns_parse_screen_cmd(s, p, whence);
                        p = n;  /* done, next line */
                    }
                    FREE(rc);
                    close(fd);
                    return NS_SUCC;
                }
            }
        }
    }

  fail:
    if (fd >= 0)
        close(fd);
    if (rc)
        FREE(rc);
    return NS_FAIL;
}




/* parse a message (not a display-list) set by the "screen" program
   screen   the session associated with that instance of screen,
            as returned by ns_attach_by_URL() and related.
            the session must contain a valid struct of callbacks (efuns),
            as certain functionalities ("add a tab", "show status message")
            may be called from here.
   p        the offending message-line
  !         mode of operation may be modified using screen->flags
   <-       returns an error code. */

static int
ns_parse_screen_msg(_ns_sess * screen, char *p)
{
    _ns_efuns *efuns;
    char *p2;
    char vdate[33], vtype[3], vrem[17], win[64];
    int ma, mi, mu, ret = NS_SUCC, type, n;
    size_t l;

    if (!p)
        return NS_FAIL;

    if (*p == ':')
        p++;
    while (isspace(*p))
        p++;

    D_ESCREEN(("got \"%s\"\n", p));

    type = (strlen(p) > 1) ? NS_SCREEN_STATUS : NS_SCREEN_ST_CLR;

    if (type == NS_SCREEN_ST_CLR) {
        if (NS_EFUN_EXISTS(efuns, screen, NULL, err_msg))
            ret = efuns->err_msg(NULL, type, "");
    }
    /* a screen display can disappear because the program in it dies, or
       because we explicitly ask screen to kill the display.  in the latter
       case, screen messages upon success.  rather than explicitly killing
       the disp-struct here, we force a status-line update instead (in which
       the status-line checker will notice the disp has gone, and delete it
       from the struct-list).  this way, we won't need to duplicate the
       delete-logic here. */
    else if (!strncmp(p, "Window ", strlen("Window ")) && (p2 = strrchr(p, ' ')) && !strcmp(p2, " killed.")) {
        ret = ns_upd_stat(screen);
        p = NULL;
    } else if (!strncmp(p, NS_SCREEN_SESS_T, strlen(NS_SCREEN_SESS_T))) {
        if (screen->name) {
            FREE(screen->name);
        }
        if ((screen->name = STRDUP(&p[strlen(NS_SCREEN_SESS_T)]))) {
            size_t lsn = strlen(screen->name);

            if (lsn) {
                screen->name[--lsn] = '\0';
            }
            D_ESCREEN(("ns_parse_screen_msg: session is \"%s\"\n", screen->name));
        }
        p = NULL;
    } else if (!strcmp(p, "New screen...") ||
               !strncmp(p, "msgwait", strlen("msgwait")) ||
               !strncmp(p, "msgminwait", strlen("msgminwait")) ||
               !strcmp(p, "Press ^@ to destroy or ^@ to resurrect window")
               || !strcmp(p, "Aborted because of window size change.")) {
        p = NULL;
    } else if ((screen->flags & NS_SESS_NO_MON_MSG) &&
             ((sscanf(p, "Window %d (%s) is now being monitored for all activity.", &n, win) == 2) ||
              (sscanf(p, "Window %d (%s) is no longer being monitored for activity.", &n, win) == 2))) {
        D_ESCREEN(("activity toggled quietly for window %d-%s\n", n, win));
        p = NULL;
        screen->flags = (screen->flags & ~NS_SESS_NO_MON_MSG);  /* reset mute flag */
    } else if (!strncmp(p, NS_SCREEN_ACT_T, l = strlen(NS_SCREEN_ACT_T))) {
        if (NS_EFUN_EXISTS(efuns, screen, NULL, upd_disp)) {
            int inx, button;
            _ns_disp *d;

            p += l;
            inx = atoi(p);
            button = disp_get_real_by_screen(screen, inx);
            if ((d = disp_fetch(screen, inx))) {
                D_ESCREEN(("activity in window %d-%s (button %d)\n", inx, d->name, button));
                d->flags |= NS_SCREAM_ACT;
                efuns->upd_disp(screen->userdef, button, d->flags, NULL);
            } else {
                D_ESCREEN(("activity in unknown window %d (button %d)...\n", inx, button));
            }
        }
        p = NULL;
    } else if (sscanf(p, NS_SCREEN_VERSION_T, vtype, &ma, &mi, &mu, vrem, vdate) == 6) {
        if (!strcmp("en", vtype))
            screen->backend = NS_MODE_SCREEN;
        else if (!strcmp("am", vtype))
            screen->backend = NS_MODE_SCREAM;
        p = NULL;
        D_ESCREEN(("ns_parse_screen_msg: scre%s %d.%2d.%2d %s a/o %s -> mode %d\n", vtype, ma, mi, mu, vrem, vdate,
                   screen->backend));
    } else if (!strcmp(p, NS_SCREEN_NO_DEBUG)) {
        p = "debug info was not compiled into \"screen\"...";
    } else if (!strncmp(p, NS_SCREEN_DK_CMD_T, strlen(NS_SCREEN_DK_CMD_T))) {
        p[strlen(p) - 1] = '\0';
        p2 = &p[strlen(NS_SCREEN_DK_CMD_T)];
        p = "unknown screen statement ignored";
    }
    if (p) {                    /* status. send to status-line or dialog or whatever */
        if (NS_EFUN_EXISTS(efuns, screen, NULL, err_msg)) {
            ret = efuns->err_msg(NULL, type, p);
        }
    }
    return ret;
}



/* parse the "hardstatus"-line of screens.
   this is, and unfortunately has to be, a ton of heuristics.
   I'm pretty sure there will be (esoteric) situations that are not handled
   (correctly) by this code, particularly in connection with more sessions
   than can be enumerated in the status-line (we do have workarounds for
   that case, they're just not very well tested yet).
   do not touch this unless you are absolutely sure you know what you're
   doing.   2002/05/01  Azundris  <scream@azundris.com>

   screen   the session associated with that instance of screen,
            as returned by ns_attach_by_URL() and related.
            the session must contain a valid struct of callbacks (efuns),
            as certain functionalities ("add a tab", "show status message")
            may be called from here.
   force    the terminal wants us to update.  if it doesn't, we may see
            fit to do so anyway in certain cases.
   width    the terminal's width in columns (ie that of the status line)
   p        the pointer to the status line.  may point into the terminal's
            line buffer if that holds plain text data (not interleaved with
            colour- and boldness-data)
   <-       returns an error code. */

int
ns_parse_screen(_ns_sess * screen, int force, int width, char *p)
{
    char *p4, *p3, *p2;         /* pointers for parser magic */
    static const char *p5 = NS_SCREEN_FLAGS;
    static int l = sizeof(NS_SCREEN_FLAGS);
    size_t status_blanks = 0;   /* status-bar overflow? */
    int ret = NS_SUCC, tmp, parsed,     /* no of *visible* elements in status line */
        n,                      /* screen's index (immutable, sparse) */
        r;                      /* real index (r'th element) */
    _ns_efuns *efuns;
    _ns_disp *disp = NULL, *d2 = NULL;

    if (!screen || !p || !width)
        return NS_FAIL;

    if (!force && screen->timestamp)
        return NS_SUCC;

    D_ESCREEN(("ns_parse_screen(0x%08x, %d, %d, \"%s\")\n", screen, force, width,
               safe_print_string(p, width)));
    if ((p = STRDUP(p))) {
        _ns_parse pd[NS_MAX_DISPS];

        p2 = &p[width - 1];
        if (*p2 == ' ') {
            while (p2 > p && *p2 == ' ') {
                status_blanks++;
                *(p2--) = '\0';
            }                   /* p2 now points behind last item */
        } else {
            *p2 = 0;            /* make darn sure it's NUL-terminated */
        }

        D_ESCREEN(("parse_screen: screen sends \"%s\" (%d)\n", p, strlen(p)));

        if (strlen(p) < 2) {    /* special case: display 0 */
            disp = screen->dsps;        /* might not get a status-line in d0! */
            if (disp && !(disp->flags & NS_SCREAM_CURR)) {      /* flags need updating */
                disp->flags |= NS_SCREAM_CURR;  /* set flag to avoid calling inp_text */
                disp->flags &= ~NS_SCREAM_ACT;
                while (disp->next) {
                    disp->flags &= ~NS_SCREAM_CURR;
                    disp = disp->next;
                }
                ret = ns_upd_stat(screen);
                D_ESCREEN(("parse_screen: qeueing update\n"));
            } /* more than once */
            else if (!screen->timestamp) {
                /* send init string the first time around, just to be on
                   the safe side.  we could send it before entering this
                   function for the first time, but that would break if
                   escapes or screenrc were set from the
                   command-line. don't ask. */

                D_ESCREEN(("parse_screen: preparing screen...\n"));

                if (screen->delay > 0) {
                    screen->timestamp = time(NULL) + screen->delay;
                    if (NS_EFUN_EXISTS(efuns, screen, NULL, waitstate)) {
                        ret = efuns->waitstate(NULL, screen->delay * 1000);
                    }
                    if (screen->where == NS_LCL) {
                        D_ESCREEN(("Sending update request.\n"));
                        ns_upd_stat(screen);
                    } else {
                        D_ESCREEN(("Not sending update request (%d).\n", screen->where));
                    }
                } else {
                    if (screen->where == NS_LCL) {
                        D_ESCREEN(("Sending init request.\n"));
                        (void) ns_screen_command(screen, NS_SCREEN_INIT);
                    } else {
                        D_ESCREEN(("Not sending init request (%d).\n", screen->where));
                    }
                    screen->timestamp = 1;
                }
            } else if ((screen->timestamp > 1) && (time(NULL) >= screen->timestamp)) {
                (void) ns_screen_command(screen, NS_SCREEN_INIT);
                screen->timestamp = 1;
                D_ESCREEN(("parse_screen: resetting screen...\n"));
            } else {
                D_ESCREEN(("parse_screen: nothing to do in exception, updating anyways...\n"));
                ret = ns_upd_stat(screen);
            }
            FREE(p);
            return ret;
        } else if (screen->backend == NS_MODE_NEGOTIATE) {
            /* I can't believe we haven't decided on a backend yet!  Ask! */
            (void) ns_screen_command(screen, NS_SCREEN_VERSION);
            (void) ns_screen_command(screen, NS_SCREEN_SESSION);
            screen->timestamp = time(NULL);
        }

        p3 = p;
        while (isspace(*p3))    /* skip left padding */
            p3++;

        if (isdigit(*p3)) {     /* list of displays */
            parsed = r = 0;
            do {
                n = atoi(p3);
                pd[parsed].name = NULL;
                pd[parsed].screen = n;
                pd[parsed].real = r++;

                while (isdigit(*p3))    /* skip index */
                    p3++;

                pd[parsed].flags = 0;   /* get and skip flags */
                while (*p3 && *p3 != ' ') {
                    for (n = 0; n < l; n++) {
                        if (*p3 == p5[n]) {
                            pd[parsed].flags |= (1 << n);
                            break;
                        }
                    }
                    p3++;
                }

                if (*p3 == ' ') {       /* skip space, read name */
                    *(p3++) = '\0';
                    p4 = p3;
                    while (p3[0] && p3[1] && (p3[0] != ' ' || p3[1] != ' '))
                        p3++;
                    if (p3[0] == ' ') {
                        *(p3++) = '\0';
                        while (isspace(*p3))
                            p3++;
                    }
                    pd[parsed++].name = p4;
                    if (parsed >= NS_MAX_DISPS)
                        p3 = &p3[strlen(p3)];
                } /* out of mem => skip remainder */
                else
                    p3 = &p3[strlen(p3)];       /* weirdness  => skip remainder */
            } while (*p3);

            for (r = 0; r < parsed; r++) {
                n = pd[r].screen;
                disp = disp_fetch(screen, n);

                if (pd[r].flags & NS_SCREAM_CURR) {
                    pd[r].flags &= ~NS_SCREAM_ACT;
                }

                if (!disp) {    /* new display */
                    if (!(disp = disp_fetch_or_make(screen, n)) || !(disp->name = STRDUP(pd[r].name))) {
                        D_ESCREEN(("parse_screen: out of memory in new_display(%d)\n", n));
                        ret = NS_FAIL;
                    } else {
                        if (NS_EFUN_EXISTS(efuns, screen, NULL, ins_disp))
                            ret = efuns->ins_disp(screen->userdef, pd[r].real - 1, pd[r].screen, disp->name);
                    }
                } else {
                    int fl = (disp->flags & ~NS_SCREAM_CURR) | (pd[r].flags & NS_SCREAM_MASK);

                    if ((fl & (NS_SCREAM_CURR | NS_SCREAM_ACT)) == (NS_SCREAM_CURR | NS_SCREAM_ACT))
                        fl &= ~NS_SCREAM_ACT;

                    if ((tmp = strcmp(disp->name, pd[r].name)) ||       /* upd display */
                        (disp->flags != fl)) {
                        if (tmp) {
                            /* Don't free disp->name; ns_inp_dial() already did! */
                            if (!(disp->name = STRDUP(pd[r].name))) {
                                FREE(p);
                                return NS_FAIL;
                            }
                        }
                        if (disp->flags != fl) {
                            if (pd[r].flags & NS_SCREAM_CURR)
                                disp->sess->curr = disp;
                            disp->flags = fl;
                        } else
                            fl = -1;
                        if (NS_EFUN_EXISTS(efuns, screen, NULL, upd_disp))
                            ret = efuns->upd_disp(screen->userdef, r, fl, (!tmp) ? NULL : disp->name);
                    }
                }

                /* remove any displays from list that have disappeared
                   from the middle of the status-line */
                if (!d2 || d2->next != disp) {  /* remove expired displays */
                    _ns_disp *d3 = disp->prvs, *d4;

                    while (d3 && d3 != d2) {
                        D_ESCREEN(("parse_screen: remove expired middle %d \"%s\"...\n", d3->index, d3->name));
                        d4 = d3->prvs;
                        if (NS_EFUN_EXISTS(efuns, screen, NULL, del_disp))
                            ret = efuns->del_disp(screen->userdef, disp_get_real_by_screen(screen, d3->index));
                        disp_kill(d3);
                        d3 = d4;
                    }
                }
                d2 = disp;
            }



#ifdef NS_PARANOID
            if (!r) {
                D_ESCREEN(("parse_screen: no elements parsed (!r)...\n"));
                FREE(p);
                return ns_screen_weird(screen, NS_ERR_WEIRDSCREEN, "no elements parsed (!r)...");
            } else
#endif
                /* kill overhang (o/t right) if status-line isn't side-scrolling
                   (as it will if not all the disp names fit in the status-line) */
            if (disp->next && status_blanks > (strlen(disp->next->name) + 6)) {
                _ns_disp *d3 = disp;

                for (disp = disp->next; disp;) {
                    D_ESCREEN(("parse_screen: remove expired right %d \"%s\"...\n", disp->index, disp->name));
                    d2 = disp;
                    if (d2->sess->curr == d2)
                        d2->sess->curr = d3;
                    disp = disp->next;
                    if (NS_EFUN_EXISTS(efuns, screen, NULL, del_disp))
                        ret = efuns->del_disp(screen->userdef, disp_get_real_by_screen(screen, d2->index));
                    disp_kill(d2);
                }
                d3->next = NULL;
            }
        }

        else                    /* not a list of displays, but a message. handle separately. */
            ret = ns_parse_screen_msg(screen, p);

        FREE(p);                /* release our (modified) copy of the status-line */
    }

    return ret;
}

#endif



/****************************************************************************
 _____                _                       _ 
|  ___| __ ___  _ __ | |_       ___ _ __   __| |
| |_ | '__/ _ \| '_ \| __|____ / _ \ '_ \ / _` |
|  _|| | | (_) | | | | ||_____|  __/ | | | (_| |
|_|  |_|  \___/|_| |_|\__|     \___|_| |_|\__,_|
                                                

frontend abstraction (callbacks for messages to the client)

   this abstracts the frontend against the backend; the abstraction-layer
   (libscream) calls these in response to message from the backend (screen,
   or whatever) without really knowing what terminal-emulator (Eterm,
   konsole, multi-gnome-terminal, ...) the frontend is. */



/* function that moves horizontal scrollbar to x/1000 % of width */
void
ns_register_ssx(_ns_efuns * efuns, int (*set_scroll_x) (void *, int))
{
    efuns->set_scroll_x = set_scroll_x;
}

/* function that moves vertical scrollbar to y/1000 % of height */
void
ns_register_ssy(_ns_efuns * efuns, int (*set_scroll_y) (void *, int))
{
    efuns->set_scroll_y = set_scroll_y;
}

/* function that sets horizontal scrollbar to w/1000 % of width */
void
ns_register_ssw(_ns_efuns * efuns, int (*set_scroll_w) (void *, int))
{
    efuns->set_scroll_w = set_scroll_w;
}

/* function that sets vertical scrollbar to h/1000 % of height */
void
ns_register_ssh(_ns_efuns * efuns, int (*set_scroll_h) (void *, int))
{
    efuns->set_scroll_h = set_scroll_h;
}

/* function that redraws the terminal */
void
ns_register_red(_ns_efuns * efuns, int (*redraw) (void *))
{
    efuns->redraw = redraw;
}


/* function that redraw part of the terminal */
void
ns_register_rda(_ns_efuns * efuns, int (*redraw_xywh) (void *, int, int, int, int))
{
    efuns->redraw_xywh = redraw_xywh;
}

/* function that expires buttons */
void
ns_register_exb(_ns_efuns * efuns, int (*expire_buttons) (void *, int))
{
    efuns->expire_buttons = expire_buttons;
}

/* function to call when a new client was added ("add tab").
   after denotes the index of the button after which this one should
   be inserted (0..n, 0 denoting "make it the first button") */
void
ns_register_ins(_ns_efuns * efuns, int (*ins_disp) (void *, int, int, char *))
{
    efuns->ins_disp = ins_disp;
}

/* function to call when a client was closed ("remove tab") */
void
ns_register_del(_ns_efuns * efuns, int (*del_disp) (void *, int))
{
    efuns->del_disp = del_disp;
}

/* function to call when a client's title was changed ("update tab") */
void
ns_register_upd(_ns_efuns * efuns, int (*upd_disp) (void *, int, int, char *))
{
    efuns->upd_disp = upd_disp;
}

/* function to pass status lines to */
void
ns_register_err(_ns_efuns * efuns, int (*err_msg) (void *, int, char *))
{
    efuns->err_msg = err_msg;
}

/* function that will execute client programs (in pseudo-terminal et al) */
void
ns_register_exe(_ns_efuns * efuns, int (*execute) (void *, char **))
{
    efuns->execute = execute;
}

/* function that will hand text as input to the client */
void
ns_register_txt(_ns_efuns * efuns, int (*inp_text) (void *, int, char *))
{
    efuns->inp_text = inp_text;
}



/* function that will open a dialog */
void
ns_register_inp(_ns_efuns * efuns, int (*inp_dial) (void *, char *, int, char **, int (*)(void *, char *, size_t, size_t)))
{
    efuns->inp_dial = inp_dial;
}



/* function that will handle tab-completion in a dialog */
void
ns_register_tab(_ns_efuns * efuns, int (*inp_tab) (void *, char *[], int, char *, size_t, size_t))
{
    efuns->inp_tab = inp_tab;
}



/* function that will do whatever while waiting */
void
ns_register_fun(_ns_efuns * efuns, int (*inp_fun) (void *, int))
{
    efuns->waitstate = inp_fun;
}



/* get callbacks.  at least one of session and display must be non-NULL.
   s  session, or NULL. if NULL, will be initialized from d->sess
   d  display, or NULL. if NULL, will be initialized from s->curr.
                        if set, will override session callbacks;
                        note that NULL pointers in d->efuns *will*
                        override (disable) non-NULL pointers in s->efuns!
   <- callback-struct */

_ns_efuns *
ns_get_efuns(_ns_sess * s, _ns_disp * d)
{
    if (!s) {
        if (!d || !d->sess)
            return NULL;
        else
            s = d->sess;
    }
    if (!d)
        d = s->curr;
    if (d && d->efuns)
        return d->efuns;
    else
        return s->efuns;
}



/* ns_inp_dial
   open a dialog (wrapp around efuns->inp_dial)
   s        the session
  !retstr   where we'll store a pointer to the result (the user's input)
   prompt   the prompt to appear in the dialog box
   <-       msg */


int
ns_inp_dial(_ns_sess * s, char *prompt, int maxlen, char **retstr, int (*inp_tab) (void *, char *, size_t, size_t))
{
    _ns_efuns *efuns;
    int ret = NS_SUCC;

    if (NS_EFUN_EXISTS(efuns, s, NULL, inp_dial)) {
        (void) efuns->inp_dial((void *) s, prompt, maxlen, retstr, inp_tab);
    } else {
        ret = NS_EFUN_NOT_SET;
        D_ESCREEN(("ns_inp_dial: sess->efuns->inp_dial not set!\n"));
    }
    return ret;
}



/***************************************************************************/
