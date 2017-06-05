////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef BACKEND_H
#define BACKEND_H

typedef enum {
    TS_AYT, TS_BRK, TS_SYNCH, TS_EC, TS_EL, TS_GA, TS_NOP, TS_ABORT,
    TS_AO, TS_IP, TS_SUSP, TS_EOR, TS_EOF, TS_LECHO, TS_RECHO, TS_PING,
    TS_EOL
} Telnet_Special;

typedef struct socket_function_table **Socket;

class CSshSession;

typedef struct {
    char *(*init) (CSshSession &session, char *host, int port, char **realhost, int nodelay);
    // back->send() returns the current amount of buffered data.
    int (*send) (CSshSession &session, char *buf, int len);
    // back->sendbuffer() does the same thing but without attempting a send
    int (*sendbuffer) (CSshSession &session);
    void (*size) (CSshSession &session);
    void (*special) (CSshSession &session, Telnet_Special code);
    Socket(*socket) (CSshSession &session);
    int (*exitcode) (CSshSession &session);
    int (*sendok) (CSshSession &session);
    int (*ldisc) (CSshSession &session, int);
    // 
    //  back->unthrottle() tells the back end that the front end
    //  buffer is clearing.
    // 
    void (*unthrottle) (CSshSession &session, int);
    int default_port;
} Backend;


#endif //BACKEND_H
