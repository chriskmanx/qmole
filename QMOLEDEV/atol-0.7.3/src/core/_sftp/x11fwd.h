////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _X11FWD_H_INCLUDED
#define _X11FWD_H_INCLUDED

char *x11_init(CSshSession &session, Socket *, char *, void *);
void x11_close(CSshSession &session, Socket);
int x11_send(CSshSession &session, Socket, char *, int);
void x11_invent_auth(char *, int, char *, int);
void x11_unthrottle(CSshSession &session, Socket s);
void x11_override_throttle(CSshSession &session, Socket s, int enable);

#endif //_X11FWD_H_INCLUDED