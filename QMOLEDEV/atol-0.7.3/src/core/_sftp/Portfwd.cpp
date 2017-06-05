////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif


struct pfwd_queue {
    struct pfwd_queue *next;
    char *buf;
};

struct PFwdPrivate {
    struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */
    void *c;			       /* (channel) data used by ssh.c */
    Socket s;
    char hostname[128];
    int throttled, throttle_override;
    int port;
    int ready;
    struct pfwd_queue *waiting;
};

void pfd_close(CSshSession &session, Socket s);


static int pfd_closing(CSshSession &session, Plug plug, char *error_msg, int error_code,
		       int calling_back)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;

    /*
     * We have no way to communicate down the forwarded connection,
     * so if an error occurred on the socket, we just ignore it
     * and treat it like a proper close.
     */
    sshfwd_close(session, (struct ssh_channel *)pr->c);
    pfd_close(session, pr->s);
    return 1;
}

static int pfd_receive(CSshSession &session, Plug plug, int urgent, char *data, int len)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;
    if (pr->ready) {
	if (sshfwd_write(session, (struct ssh_channel *)pr->c, data, len) > 0) {
	    pr->throttled = 1;
	    sk_set_frozen(session, pr->s, 1);
	}
    }
    return 1;
}

static void pfd_sent(CSshSession &session, Plug plug, int bufsize)
{
    struct PFwdPrivate *pr = (struct PFwdPrivate *) plug;

    sshfwd_unthrottle(session, (struct ssh_channel *)pr->c, bufsize);
}

/*
 * Called when receiving a PORT OPEN from the server
 */
char *pfd_newconnect(CSshSession &session, Socket *s, char *hostname, int port, void *c)
{
    static struct plug_function_table fn_table = {
		pfd_closing,
		pfd_receive,
		pfd_sent,
		NULL
    };

    SockAddr addr;
    char *err, *dummy_realhost;
    struct PFwdPrivate *pr;

    /*
     * Try to find host.
     */
    addr = sk_namelookup(hostname, &dummy_realhost);
    if ((err = sk_addr_error(addr)))
	return err;

    /*
     * Open socket.
     */
    pr = (struct PFwdPrivate *) smalloc(sizeof(struct PFwdPrivate));
    pr->fn = &fn_table;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 1;
    pr->c = c;

    pr->s = *s = sk_new(session, addr, port, 0, 1, 0, (Plug) pr);
    if ((err = sk_socket_error(session, *s))) {
		sfree(pr);
		return err;
    }

    sk_set_private_ptr(session, *s, pr);
    sk_addr_free(addr);
    return NULL;
}

/*
 called when someone connects to the local port
 */

static int pfd_accepting(CSshSession &session, Plug p, void *sock)
{
    static struct plug_function_table fn_table = {
		pfd_closing,
		pfd_receive,
		pfd_sent,
		NULL
    };
    struct PFwdPrivate *pr, *org;
    Socket s;
    char *err;

    org = (struct PFwdPrivate *)p;
    pr = (struct PFwdPrivate *) smalloc(sizeof(struct PFwdPrivate));
    pr->fn = &fn_table;

    pr->c = NULL;

    pr->s = s = sk_register(session, sock, (Plug) pr);
    if ((err = sk_socket_error(session, s))) {
		sfree(pr);
		return err != NULL;
    }

    pr->c = new_sock_channel(session, s);

    strcpy(pr->hostname, org->hostname);
    pr->port = org->port;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 0;
    pr->waiting = NULL;

    sk_set_private_ptr(session, s, pr);

    if (pr->c == NULL) {
	sfree(pr);
	return 1;
    } else {
		/* asks to forward to the specified host/port for this */
		ssh_send_port_open(session, pr->c, pr->hostname, pr->port, "forwarding");
    }

    return 0;
}


/* Add a new forwarding from port -> desthost:destport
 sets up a listener on the local machine on port
 */
char *pfd_addforward(CSshSession &session, char *desthost, int destport, int port)
{
    static struct plug_function_table fn_table = {
	pfd_closing,
	pfd_receive,		       /* should not happen... */
	pfd_sent,		       /* also should not happen */
	pfd_accepting
    };

    char *err;
    struct PFwdPrivate *pr;
    Socket s;

    /*
     * Open socket.
     */
    pr = (struct PFwdPrivate *) smalloc(sizeof(struct PFwdPrivate));
    pr->fn = &fn_table;
    pr->c = NULL;
    strcpy(pr->hostname, desthost);
    pr->port = destport;
    pr->throttled = pr->throttle_override = 0;
    pr->ready = 0;
    pr->waiting = NULL;

    pr->s = s = sk_newlistener(session, port, (Plug) pr, !cfg.lport_acceptall);
    if ((err = sk_socket_error(session, s))) {
		sfree(pr);
		return err;
    }

    sk_set_private_ptr(session, s, pr);

    return NULL;
}

void pfd_close(CSshSession &session,Socket s)
{
    struct PFwdPrivate *pr;

    if (!s)
	return;

    pr = (struct PFwdPrivate *) sk_get_private_ptr(session, s);

    sfree(pr);

    sk_close(session, s);
}

void pfd_unthrottle(CSshSession &session, Socket s)
{
    struct PFwdPrivate *pr;
    if (!s)
		return;
    pr = (struct PFwdPrivate *) sk_get_private_ptr(session, s);

    pr->throttled = 0;
    sk_set_frozen(session, s, pr->throttled || pr->throttle_override);
}

void pfd_override_throttle(CSshSession &session, Socket s, int enable)
{
    struct PFwdPrivate *pr;
    if (!s)
		return;
    pr = (struct PFwdPrivate *) sk_get_private_ptr(session, s);

    pr->throttle_override = enable;
    sk_set_frozen(session, s, pr->throttled || pr->throttle_override);
}

/*
 * Called to send data down the raw connection.
 */
int pfd_send(CSshSession &session, Socket s, char *data, int len)
{
    if (s == NULL)
	return 0;
    return sk_write(session, s, data, len);
}


void pfd_confirm(CSshSession &session, Socket s)
{
    struct PFwdPrivate *pr;

    if (s == NULL)
	return;

    pr = (struct PFwdPrivate *) sk_get_private_ptr(session, s);
    pr->ready = 1;
    sk_set_frozen(session, s, 0);
    sk_write(session, s, NULL, 0);
}
