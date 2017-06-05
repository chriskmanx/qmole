/*
 * orbhttp.c: hacked up 
 * nanohttp.c: minimalist HTTP GET implementation to fetch external subsets.
 *             focuses on size, streamability, reentrancy and portability
 *
 * This is clearly not a general purpose HTTP implementation
 * If you look for one, check:
 *         http://www.w3.org/Library/
 *
 * See Copyright for the status of this software.
 *
 * Daniel.Veillard@w3.org
 */
 
/* TODO add compression support, Send the Accept- , and decompress on the
        fly with ZLIB if found at compile-time */

#include "config.h"
#include <glib.h>

#include <stdio.h>
#include <string.h>

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#ifdef HAVE_WINSOCK2_H
#  include <winsock2.h>
#  ifndef _WINSOCKAPI_
#    define _WINSOCKAPI_
#  endif
#  define read(s, b, l) recv(s, b, l, 0)
#  define write(s, b, l) send(s, b, l, 0)
#  define EINPROGRESS WSAEINPROGRESS
#  define close closesocket
#else /* !HAVE_WINSOCK2_H */
#  ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#  endif
#  ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#  endif
#  ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#  endif
#  include <netdb.h>
#endif /* !HAVE_WINSOCK2_H */

#include <fcntl.h> 
#include <errno.h>
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
#endif

#define CHECK_URI(str) \
(!strncmp(str, "IOR:", strlen("IOR:")) \
 || !strncmp(str, "iiop://", strlen("iiop://")) \
 || !strncmp(str, "iioploc://", strlen("iioploc://")))

static void	orbHTTPInit		(void);
static void	orbHTTPScanProxy	(const char *URL);
static void *	orbHTTPOpen		(const char *URL);
static int	orbHTTPRead		(void *ctx,
				 void *dest,
				 int len);
static void	orbHTTPClose	(void *ctx);

#include "orbhttp.h"

#ifdef STANDALONE
#define DEBUG_HTTP
#endif

#define ORB_HTTP_MAX_REDIR	10

#define ORB_HTTP_CHUNK	4096
#define ORB_TEMP_BUF_SIZE 4096

#define ORB_HTTP_CLOSED	0
#define ORB_HTTP_WRITE	1
#define ORB_HTTP_READ	2
#define ORB_HTTP_NONE	4

typedef struct orbHTTPCtxt {
    char *protocol;	/* the protocol name */
    char *hostname;	/* the host name */
    int port;		/* the port */
    char *path;		/* the path within the URL */
    int fd;		/* the file descriptor for the socket */
    int state;		/* WRITE / READ / CLOSED */
    char *out;		/* buffer sent (zero terminated) */
    char *outptr;	/* index within the buffer sent */
    char *in;		/* the receiving buffer */
    char *content;	/* the start of the content */
    char *inptr;	/* the next byte to read from network */
    char *inrptr;	/* the next byte to give back to the client */
    int inlen;		/* len of the input buffer */
    int last;		/* return code for last operation */
    int returnValue;	/* the protocol return value */
    char *contentType;	/* the MIME type for the input */
    char *location;	/* the new URL in case of redirect */
} orbHTTPCtxt, *orbHTTPCtxtPtr;

static int initialized = 0;
static char *proxy = NULL;	/* the proxy name if any */
static int proxyPort;	/* the proxy port if any */

/**
 * orbHTTPInit:
 *
 * Initialize the HTTP protocol layer.
 * Currently it just checks for proxy informations
 */

static void
orbHTTPInit (void)
{
	const char *env;

	if (initialized)
		return;

	if (!proxy) {
		proxyPort = 80;
		env = g_getenv ("no_proxy");
		if (env)
			goto done;
		if ((env = g_getenv ("http_proxy")) ||
		    (env = g_getenv ("HTTP_PROXY")))
			orbHTTPScanProxy (env);
	}
 done:
	initialized = 1;
}

/**
 * orbHTTPScanURL:
 * @ctxt:  an HTTP context
 * @URL:  The URL used to initialize the context
 *
 * (Re)Initialize an HTTP context by parsing the URL and finding
 * the protocol host port and path it indicates.
 */

static void
orbHTTPScanURL (orbHTTPCtxtPtr ctxt, const char *URL)
{
    const char *cur = URL;
    char buf[ORB_TEMP_BUF_SIZE];
    int index = 0;
    int port = 0;

    if (ctxt->protocol != NULL) { 
        g_free(ctxt->protocol);
	ctxt->protocol = NULL;
    }
    if (ctxt->hostname != NULL) { 
        g_free(ctxt->hostname);
	ctxt->hostname = NULL;
    }
    if (ctxt->path != NULL) { 
        g_free(ctxt->path);
	ctxt->path = NULL;
    }
    if (URL == NULL) return;
    buf[index] = 0;
    while (*cur != 0) {
        if ((cur[0] == ':') && (cur[1] == '/') && (cur[2] == '/')) {
	    buf[index] = 0;
	    ctxt->protocol = g_strdup(buf);
	    index = 0;
            cur += 3;
	    break;
	}
	buf[index++] = *cur++;
    }
    if (*cur == 0) return;

    buf[index] = 0;
    while (1) {
        if (cur[0] == ':') {
	    buf[index] = 0;
	    ctxt->hostname = g_strdup(buf);
	    index = 0;
	    cur += 1;
	    while ((*cur >= '0') && (*cur <= '9')) {
	        port *= 10;
		port += *cur - '0';
		cur++;
	    }
	    if (port != 0) ctxt->port = port;
	    while ((cur[0] != '/') && (*cur != 0)) 
	        cur++;
	    break;
	}
        if ((*cur == '/') || (*cur == 0)) {
	    buf[index] = 0;
	    ctxt->hostname = g_strdup(buf);
	    index = 0;
	    break;
	}
	buf[index++] = *cur++;
    }
    if (*cur == 0) 
        ctxt->path = g_strdup("/");
    else {
        index = 0;
        buf[index] = 0;
	while (*cur != 0)
	    buf[index++] = *cur++;
	buf[index] = 0;
	ctxt->path = g_strdup(buf);
    }	
}

/**
 * orbHTTPScanProxy:
 * @URL:  The proxy URL used to initialize the proxy context
 *
 * (Re)Initialize the HTTP Proxy context by parsing the URL and finding
 * the protocol host port it indicates.
 * Should be like http://myproxy/ or http://myproxy:3128/
 * A NULL URL cleans up proxy informations.
 */

static void
orbHTTPScanProxy (const char *URL)
{
	const char *cur = URL;
	char buf[ORB_TEMP_BUF_SIZE];
	int index;
	int port = 0;

	g_free (proxy);
	proxy = NULL;

	if (proxyPort != 0) 
		proxyPort = 0;

#ifdef DEBUG_HTTP
	if (!URL)
		printf ("Removing HTTP proxy info\n");
	else
		printf ("Using HTTP proxy %s\n", URL);
#endif
	if (!URL)
		return;

	index = 0;
	buf [index] = 0;
	while (*cur != '\0') {
		if ((cur[0] == ':') && (cur[1] == '/') && (cur[2] == '/')) {
			buf[index] = 0;
			index = 0;
			cur += 3;
			break;
		}
		buf[index++] = *cur++;
	}
	if (*cur == '\0')
		return;

	buf[index] = 0;
	while (1) {
		if (cur[0] == ':') {
			buf[index] = 0;
			proxy = g_strdup(buf);
			index = 0;
			cur += 1;
			while ((*cur >= '0') && (*cur <= '9')) {
				port *= 10;
				port += *cur - '0';
				cur++;
			}
			if (port != 0) proxyPort = port;
			while ((cur[0] != '/') && (*cur != 0)) 
				cur++;
			break;
		}
		if ((*cur == '/') || (*cur == '\0')) {
			buf[index] = 0;
			proxy = g_strdup(buf);
			index = 0;
			break;
		}
		buf[index++] = *cur++;
	}
}

/**
 * orbHTTPNewCtxt:
 * @URL:  The URL used to initialize the context
 *
 * Allocate and initialize a new HTTP context.
 *
 * Returns an HTTP context or NULL in case of error.
 */
static orbHTTPCtxtPtr
orbHTTPNewCtxt (const char *URL)
{
	orbHTTPCtxtPtr ret;

	ret = g_new (orbHTTPCtxt, 1);

	memset (ret, 0, sizeof(orbHTTPCtxt));
	ret->port = 80;
	ret->returnValue = 0;

	orbHTTPScanURL (ret, URL);

	return ret;
}

/**
 * orbHTTPFreeCtxt:
 * @ctxt:  an HTTP context
 *
 * Frees the context after closing the connection.
 */

static void
orbHTTPFreeCtxt(orbHTTPCtxtPtr ctxt)
{
    if (ctxt == NULL) return;
    if (ctxt->hostname != NULL) g_free(ctxt->hostname);
    if (ctxt->protocol != NULL) g_free(ctxt->protocol);
    if (ctxt->path != NULL) g_free(ctxt->path);
    if (ctxt->out != NULL) g_free(ctxt->out);
    if (ctxt->in != NULL) g_free(ctxt->in);
    if (ctxt->contentType != NULL) g_free(ctxt->contentType);
    if (ctxt->location != NULL) g_free(ctxt->location);
    ctxt->state = ORB_HTTP_NONE;
    if (ctxt->fd >= 0) close(ctxt->fd);
    ctxt->fd = -1;
    g_free(ctxt);
}

/**
 * orbHTTPSend:
 * @ctxt:  an HTTP context
 *
 * Send the input needed to initiate the processing on the server side
 */
static void
orbHTTPSend (orbHTTPCtxtPtr ctxt)
{
	if (ctxt->state & ORB_HTTP_WRITE)
		ctxt->last = write (
			ctxt->fd, ctxt->outptr, strlen (ctxt->outptr));
}

/**
 * orbHTTPRecv:
 * @ctxt:  an HTTP context
 *
 * Read information coming from the HTTP connection.
 * This is a blocking call (but it blocks in select(), not read()).
 *
 * Returns the number of byte read or -1 in case of error.
 */
static int
orbHTTPRecv (orbHTTPCtxtPtr ctxt)
{
    fd_set rfd;
    struct timeval tv;

#define ORB_HTTP_DYN_BUFSIZE 65000

    while (ctxt->state & ORB_HTTP_READ) {
	if (ctxt->in == NULL) {
	    ctxt->in = (char *) g_malloc(ORB_HTTP_DYN_BUFSIZE * sizeof(char));
	    ctxt->inlen = ORB_HTTP_DYN_BUFSIZE;
	    ctxt->inptr = ctxt->content = ctxt->inrptr = ctxt->in;
	}
	if (ctxt->inrptr > ctxt->in + ORB_HTTP_CHUNK) {
	    int delta = ctxt->inrptr - ctxt->in;
	    int len = ctxt->inptr - ctxt->inrptr;
	    
	    memmove(ctxt->in, ctxt->inrptr, len);
	    ctxt->inrptr -= delta;
	    ctxt->content -= delta;
	    ctxt->inptr -= delta;
	}
        if ((ctxt->in + ctxt->inlen) < (ctxt->inptr + ORB_HTTP_CHUNK)) {
	    int d_inptr = ctxt->inptr - ctxt->in;
	    int d_content = ctxt->content - ctxt->in;
	    int d_inrptr = ctxt->inrptr - ctxt->in;

	    ctxt->inlen *= 2;
            ctxt->in = (char *) g_realloc(ctxt->in, ctxt->inlen);
            ctxt->inptr = ctxt->in + d_inptr;
            ctxt->content = ctxt->in + d_content;
            ctxt->inrptr = ctxt->in + d_inrptr;
	}
	ctxt->last = read(ctxt->fd, ctxt->inptr, ORB_HTTP_CHUNK);
	if (ctxt->last > 0) {
	    ctxt->inptr += ctxt->last;
	    return(ctxt->last);
	}
	if (ctxt->last == 0) {
	    return(0);
	}
#ifdef EWOULDBLOCK
	if ((ctxt->last == -1) && (errno != EWOULDBLOCK)) {
	    return(0);
	}
#endif
	tv.tv_sec=10;
	tv.tv_usec=0;
	FD_ZERO(&rfd);
	FD_SET(ctxt->fd, &rfd);
	
	if(select(ctxt->fd+1, &rfd, NULL, NULL, &tv)<1)
		return(0);
    }
    return(0);
}

/**
 * orbHTTPReadLine:
 * @ctxt:  an HTTP context
 *
 * Read one line in the HTTP server output, usually for extracting
 * the HTTP protocol informations from the answer header.
 *
 * Returns a newly allocated string with a copy of the line, or NULL
 *         which indicate the end of the input.
 */

static char *
orbHTTPReadLine(orbHTTPCtxtPtr ctxt) {
    char buf[ORB_TEMP_BUF_SIZE];
    char *bp=buf;
    
    while(bp - buf < (ORB_TEMP_BUF_SIZE-1)) {
	if(ctxt->inrptr == ctxt->inptr) {
	    if (orbHTTPRecv(ctxt) == 0) {
		if (bp == buf)
		    return(NULL);
		else
		    *bp = 0;
		return(g_strdup(buf));
	    }
	}
	*bp = *ctxt->inrptr++;
	if(*bp == '\n') {
	    *bp = 0;
	    return(g_strdup(buf));
	}
	if(*bp != '\r')
	    bp++;
    }
    buf[ORB_TEMP_BUF_SIZE-1] = 0;
    return(g_strdup(buf));
}


/**
 * orbHTTPScanAnswer:
 * @ctxt:  an HTTP context
 * @line:  an HTTP header line
 *
 * Try to extract useful informations from the server answer.
 * We currently parse and process:
 *  - The HTTP revision/ return code
 *  - The Content-Type
 *  - The Location for redirrect processing.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

static void
orbHTTPScanAnswer(orbHTTPCtxtPtr ctxt, const char *line) {
    const char *cur = line;

    if (line == NULL) return;

    if (!strncmp(line, "HTTP/", 5)) {
        int version = 0;
	int ret = 0;

	cur += 5;
	while ((*cur >= '0') && (*cur <= '9')) {
	    version *= 10;
	    version += *cur - '0';
	    cur++;
	}
	if (*cur == '.') {
	    cur++;
	    if ((*cur >= '0') && (*cur <= '9')) {
		version *= 10;
		version += *cur - '0';
		cur++;
	    }
	    while ((*cur >= '0') && (*cur <= '9'))
		cur++;
	} else
	    version *= 10;
	if ((*cur != ' ') && (*cur != '\t')) return;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	if ((*cur < '0') || (*cur > '9')) return;
	while ((*cur >= '0') && (*cur <= '9')) {
	    ret *= 10;
	    ret += *cur - '0';
	    cur++;
	}
	if ((*cur != 0) && (*cur != ' ') && (*cur != '\t')) return;
	ctxt->returnValue = ret;
    } else if (!g_ascii_strncasecmp(line, "content-type:", 13)
	       || !g_ascii_strncasecmp(line, "contenttype:", 12)) {
        cur = strchr(cur, ':') + 1;
	if (ctxt->contentType != NULL) return;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	ctxt->contentType = g_strdup(cur);
    } else if (!g_ascii_strncasecmp(line, "Location:", 9)) {
        cur += 9;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	if (ctxt->location != NULL)
	    g_free(ctxt->location);
	ctxt->location = g_strdup(cur);
    }
}

/**
 * orbHTTPConnectAttempt:
 * @ia:  an internet adress structure
 * @port:  the port number
 *
 * Attempt a connection to the given IP:port endpoint. It forces
 * non-blocking semantic on the socket, and allow 60 seconds for
 * the host to answer.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

static int
orbHTTPConnectAttempt(struct in_addr ia, int port)
{
    int s=socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    struct sockaddr_in sin;
    fd_set wfd;
    struct timeval tv;
    int status;
    
    if(s==-1) {
#ifdef DEBUG_HTTP
	perror("socket");
#endif
	return(-1);
    }
    
#ifdef _WINSOCKAPI_
    {
	int rv = 0 ;
	u_long one = 1;

	status = ioctlsocket(s, FIONBIO, &one) == SOCKET_ERROR ? -1 : 0;
    }
#else /* _WINSOCKAPI_ */
#if defined(VMS)
    {
	int enable = 1;
	status = IOCTL(s, FIONBIO, &enable);
    }
#else /* VMS */
    if((status = fcntl(s, F_GETFL, 0)) != -1) {
#ifdef O_NONBLOCK
	status |= O_NONBLOCK;
#else /* O_NONBLOCK */
#ifdef F_NDELAY
	status |= F_NDELAY;
#endif /* F_NDELAY */
#endif /* !O_NONBLOCK */
	status = fcntl(s, F_SETFL, status);
    }
    if(status < 0) {
#ifdef DEBUG_HTTP
	perror("nonblocking");
#endif
	close(s);
	return(-1);
    }
#endif /* !VMS */
#endif /* !_WINSOCKAPI_ */


    sin.sin_family = AF_INET;	
    sin.sin_addr   = ia;
    sin.sin_port   = htons(port);
    
    if((connect(s, (struct sockaddr *)&sin, sizeof(sin))==-1) &&
       (errno != EINPROGRESS)) {
	perror("connect");
	close(s);
	return(-1);
    }	
    
    tv.tv_sec = 60;		/* We use 60 second timeouts for now */
    tv.tv_usec = 0;
    
    FD_ZERO(&wfd);
    FD_SET(s, &wfd);
    
    switch(select(s+1, NULL, &wfd, NULL, &tv))
    {
	case 0:
	    /* Time out */
	    close(s);
	    return(-1);
	case -1:
	    /* Ermm.. ?? */
#ifdef DEBUG_HTTP
	    perror("select");
#endif
	    close(s);
	    return(-1);
    }

    if ( FD_ISSET(s, &wfd) ) {
	unsigned int len; /* was socklen_t barfed on some systems :-( */
	len = sizeof(status);
	if (getsockopt(s, SOL_SOCKET, SO_ERROR, &status, &len) < 0 ) {
	    /* Solaris error code */
	    return (-1);
	}
	if ( status ) {
	    close (s);
	    errno = status;
	    return (-1);
	}
    } else {
	/* pbm */
	return (-1);
    }
    
    return(s);
}
 
/**
 * orbHTTPConnectHost:
 * @host:  the host name
 * @port:  the port number
 *
 * Attempt a connection to the given host:port endpoint. It tries
 * the multiple IP provided by the DNS if available.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

static int
orbHTTPConnectHost(const char *host, int port)
{
    struct hostent *h;
    int i;
    int s;
    
    h=gethostbyname(host);
    if(h==NULL)
    {
#ifdef DEBUG_HTTP
	fprintf(stderr,"unable to resolve '%s'.\n", host);
#endif
	return(-1);
    }
    
    for(i=0; h->h_addr_list[i]; i++)
    {
	struct in_addr ia;
	memcpy(&ia, h->h_addr_list[i],4);
	s = orbHTTPConnectAttempt(ia, port);
	if(s != -1)
	    return(s);
    }

#ifdef DEBUG_HTTP
    fprintf(stderr, "unable to connect to '%s'.\n", host);
#endif
    return(-1);
}


/**
 * orbHTTPOpen:
 * @URL:  The URL to load
 * @contentType:  if available the Content-Type information will be
 *                returned at that location
 *
 * This function try to open a connection to the indicated resource
 * via HTTP GET.
 *
 * Returns NULL in case of failure, otherwise a request handler.
 *     The contentType, if provided must be freed by the caller
 */

static void*
orbHTTPOpen(const char *URL) {
    orbHTTPCtxtPtr ctxt;
    char buf[ORB_TEMP_BUF_SIZE];
    int ret;
    char *p;
    int head;
    int nbRedirects = 0;
    char *redirURL = NULL;
    
    orbHTTPInit();

retry:
    if (redirURL == NULL)
      ctxt = orbHTTPNewCtxt(URL);
    else if(CHECK_URI(redirURL))
      {
	ctxt = orbHTTPNewCtxt("");
	ctxt->location = redirURL;
	return ctxt;
      }
    else
      {
	ctxt = orbHTTPNewCtxt(redirURL);
	g_free(redirURL);
	redirURL = NULL;
      }

    if ((ctxt->protocol == NULL) || (strcmp(ctxt->protocol, "http"))) {
        orbHTTPFreeCtxt(ctxt);
	if (redirURL != NULL) g_free(redirURL);
        return(NULL);
    }
    if (ctxt->hostname == NULL) {
        orbHTTPFreeCtxt(ctxt);
        return(NULL);
    }
    if (proxy)
	ret = orbHTTPConnectHost(proxy, proxyPort);
    else
	ret = orbHTTPConnectHost(ctxt->hostname, ctxt->port);
    if (ret < 0) {
        orbHTTPFreeCtxt(ctxt);
        return(NULL);
    }
    ctxt->fd = ret;
    if (proxy) {
	if (ctxt->port != 80)
	    g_snprintf(buf, sizeof(buf),
		     "GET http://%s:%d%s HTTP/1.0\r\nHost: %s\r\n\r\n",
		 ctxt->hostname, ctxt->port, ctxt->path, ctxt->hostname);
	else 
	    g_snprintf(buf, sizeof(buf),"GET http://%s%s HTTP/1.0\r\nHost: %s\r\n\r\n",
		 ctxt->hostname, ctxt->path, ctxt->hostname);
#ifdef DEBUG_HTTP
	if (ctxt->port != 80)
	    printf("-> Proxy GET http://%s:%d%s HTTP/1.0\n-> Host: %s\n\n",
	           ctxt->hostname, ctxt->port, ctxt->path, ctxt->hostname);
	else
	    printf("-> Proxy GET http://%s%s HTTP/1.0\n-> Host: %s\n\n",
	           ctxt->hostname, ctxt->path, ctxt->hostname);
#endif
    } else {
	g_snprintf(buf, sizeof(buf),"GET %s HTTP/1.0\r\nHost: %s\r\n\r\n",
		 ctxt->path, ctxt->hostname);
#ifdef DEBUG_HTTP
	printf("-> GET %s HTTP/1.0\n-> Host: %s\n\n",
	       ctxt->path, ctxt->hostname);
#endif
    }
    ctxt->outptr = ctxt->out = g_strdup(buf);
    ctxt->state = ORB_HTTP_WRITE;
    orbHTTPSend(ctxt);
    ctxt->state = ORB_HTTP_READ;
    head = 1;

    while ((p = orbHTTPReadLine(ctxt)) != NULL) {
        if (head && (*p == 0)) {
	    head = 0;
	    ctxt->content = ctxt->inrptr;
	    g_free(p);
	    break;
	}
	orbHTTPScanAnswer(ctxt, p);

#ifdef DEBUG_HTTP
	if (p != NULL) printf("<- %s\n", p);
#endif
        if (p != NULL) g_free(p);
    }

    if ((ctxt->location != NULL) && (ctxt->returnValue >= 300) &&
        (ctxt->returnValue < 400)) {
#ifdef DEBUG_HTTP
	printf("\nRedirect to: %s\n", ctxt->location);
#endif
	while (orbHTTPRecv(ctxt)) ;
        if (nbRedirects < ORB_HTTP_MAX_REDIR) {
	    nbRedirects++;
	    redirURL = g_strdup(ctxt->location);
	    orbHTTPFreeCtxt(ctxt);
	    goto retry;
	}
	orbHTTPFreeCtxt(ctxt);
#ifdef DEBUG_HTTP
	printf("Too many redirrects, aborting ...\n");
#endif
	return(NULL);

    }

#ifdef DEBUG_HTTP
    if (ctxt->contentType != NULL)
	printf("\nCode %d, content-type '%s'\n\n",
	       ctxt->returnValue, ctxt->contentType);
    else
	printf("\nCode %d, no content-type\n\n",
	       ctxt->returnValue);
#endif

    return((void *) ctxt);
}

/**
 * orbHTTPRead:
 * @ctx:  the HTTP context
 * @dest:  a buffer
 * @len:  the buffer length
 *
 * This function tries to read @len bytes from the existing HTTP connection
 * and saves them in @dest. This is a blocking call.
 *
 * Returns the number of byte read. 0 is an indication of an end of connection.
 *         -1 indicates a parameter error.
 */
static int
orbHTTPRead(void *ctx, void *dest, int len) {
    orbHTTPCtxtPtr ctxt = (orbHTTPCtxtPtr) ctx;

    if (ctx == NULL) return(-1);
    if (dest == NULL) return(-1);
    if (len <= 0) return(0);

    while (ctxt->inptr - ctxt->inrptr < len) {
        if (orbHTTPRecv(ctxt) == 0) break;
    }
    if (ctxt->inptr - ctxt->inrptr < len)
        len = ctxt->inptr - ctxt->inrptr;
    memcpy(dest, ctxt->inrptr, len);
    ctxt->inrptr += len;
    return(len);
}

/**
 * orbHTTPClose:
 * @ctx:  the HTTP context
 *
 * This function closes an HTTP context, it ends up the connection and
 * free all data related to it.
 */
static void
orbHTTPClose(void *ctx) {
    orbHTTPCtxtPtr ctxt = (orbHTTPCtxtPtr) ctx;

    if (ctx == NULL) return;

    orbHTTPFreeCtxt(ctxt);
}

#ifndef DEBUG_HTTP
#define DEBUG_HTTP
#endif

char *
orb_http_resolve(const char *URL)
{
    orbHTTPCtxtPtr ctxt;
    char buf[ORB_TEMP_BUF_SIZE];
    int len_read, len;
    char *content_type, *retval = NULL;

    content_type = NULL;
    ctxt = orbHTTPOpen(URL);
    if (ctxt == NULL)
      return NULL;

    if(ctxt->location
       && CHECK_URI(ctxt->location)) /* Was a redirect to an IOR */
      retval = g_strdup(ctxt->location);
    else
      {
	len_read = 0;
	do {
	  len = orbHTTPRead(ctxt, buf + len_read, sizeof(buf) - len_read);
	  len_read += len;
	} while(len > 0);
	len_read += len;
	buf[len_read] = '\0';
	g_strstrip(buf);
	if(CHECK_URI(buf))
	  retval = g_strdup(buf);
      }

    orbHTTPClose(ctxt);

    return retval;
}
