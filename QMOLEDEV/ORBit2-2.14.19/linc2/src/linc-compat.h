/*
 * Herein lies a set of private ugly portability
 * hacks for the mind-numbingly broken Unix like
 * things that exist out there.
 */
#ifndef LINK_HACKS_H
#define LINK_HACKS_H

#include <sys/types.h>

#ifdef HAVE_WINSOCK2_H
#  include <winsock2.h>
#  include <ws2tcpip.h>
#else
#  ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#  endif
#  ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#  endif
#  ifdef HAVE_NETDB_H
#    include <netdb.h>
#  endif
#  ifdef HAVE_NETINET_TCP_H
#    include <netinet/tcp.h>
#  endif
#  ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#  endif
#  ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#  endif
#  ifdef HAVE_ARPA_NAMESER_H
#    include <arpa/nameser.h>
#  endif
#  ifdef HAVE_RESOLV_H
#    include <resolv.h>
#  endif
#endif

#include <sys/stat.h>
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#ifdef HAVE_UTIME_H
#  include <utime.h>
#else
#  ifdef HAVE_SYS_UTIME_H
#    include <sys/utime.h>
#  endif
#endif
#include <errno.h>
#include <string.h>

#ifdef HAVE_LINUX_IRDA_H
#  include <asm/types.h>
#  include <linux/irda.h>
#endif

#ifndef MAXHOSTNAMELEN
#  define MAXHOSTNAMELEN 255
#endif

#ifdef HAVE_WINSOCK2_H
   /* Define fake errno values for socket stuff. These aren't defined
    * in the Microsoft C library. Use the WSAE* error codes as such,
    * as they don't overlap with errno values. Define only those that
    * actually are used by the linc2 code.
    */
#  ifndef ECONNREFUSED
#    define ECONNREFUSED WSAECONNREFUSED
#  endif
#  ifndef EADDRINUSE
#    define EADDRINUSE WSAEADDRINUSE
#  endif
#  ifndef EINPROGRESS
#    define EINPROGRESS WSAEINPROGRESS
#  endif

   /* Undefine address families that aren't really present, as the
    * linc code checks for these macros being defined when it
    * determines whether they are supported. (It would be more correct
    * to check at run-time.)
    */
#  undef AF_UNIX		/* No Unix domain sockets */
#  undef AF_INET6		/* Doesn't have sockaddr_in6 */

#endif

#if !defined (NI_MAXSERV) || !defined (NI_MAXHOST)
#  include <sys/param.h>
#endif

#if !defined (NI_MAXHOST)
#  define NI_MAXHOST MAXHOSTNAMELEN
#endif

#if !defined (NI_MAXSERV)
#  define NI_MAXSERV 64
#endif

#if !defined (INADDR_NONE)
#  define INADDR_NONE (-1)
#endif

#if !defined (UNIX_PATH_MAX)
/* UNP: 14.2 - Posix.1g at least 100 bytes */
#  define LINK_UNIX_PATH_MAX 100
#else
#  define LINK_UNIX_PATH_MAX UNIX_PATH_MAX
#endif

const char *link_strerror (int);

#endif /* LINK_HACKS_H */
