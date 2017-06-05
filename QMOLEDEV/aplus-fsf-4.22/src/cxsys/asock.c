/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/development.h>
#include <stdio.h>
#include <signal.h>
#include <netdb.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#if defined(_AIX) || defined(__osf__) || defined(linux)
#include <sys/ioctl.h>
#include <sys/select.h>
#else
#include <sys/filio.h>
#endif

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>

static struct timeval timeout = { 0, 0 };

/*
 * Handler for SIGPIPE
 */
static void sigpipehandler(i)
int i;
{
  return;	/* Do nothing */
}

/*
 * Opens a socket, binds it to a specified port and enables connections
 * Returns the file descriptor of the listening socket or -1 on error
 */
I socklisten(I port)
{
  int	sock;
  struct sockaddr_in sin;
  int toggle;

  /* Ignore signals from closing of sockets */
  aplus_signal(SIGPIPE, sigpipehandler);

  /* Open a socket */
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    (void)perror("socket");
    return -1;
  }

  /* make it ok to reuse the address */
  toggle = 1;
  if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
	  (char *)(&toggle), sizeof(toggle)) < 0)
  {
    (void)perror("setsockopt");
  }

  /* Bind the socket to a port */
  sin.sin_port = htons((u_short)port);
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = INADDR_ANY;
  if (bind(sock, (struct sockaddr *)&sin, sizeof(sin))) {
    (void)perror("bind");
    (void)close(sock);
    return -1;
  }

  /* Listen at the port for connections */
  if (listen(sock, 5)) {
    perror("listen");
    (void)close(sock);
    return -1;
  }
  return (I)sock;
}

/*
 * Accepts connections to the specified listening socket.
 * If waitflag is 1 then sockaccept() blocks until a connection is made
 * otherwise it returns immediately.  If a connection is made then
 * the file descriptor of the connected socket is returned.  If no
 * connection was pending and waiflag is 0 then a -2 is returned.
 * A -1 is returned on error.
 */
I sockaccept(I sock, I waitflag)
{
  int newsock;
  fd_set afd;
  int toggle;

  FD_ZERO(&afd);
  FD_SET(sock, &afd);
  if (!waitflag) {
    if (select(FD_SETSIZE, &afd, NULL, NULL, &timeout) < 0) {
      (void)perror("select");
      return -1;
    }
  }
  if (FD_ISSET((int)sock, &afd)) {
    if ((newsock = accept((int)sock, NULL, NULL)) == -1) {
      (void)perror("accept");
      return -1;
    }
    toggle = 1;
    if (setsockopt(newsock, SOL_SOCKET, SO_KEEPALIVE,
	           (char *)(&toggle), sizeof(toggle)) < 0)
    {
      (void)perror("setsockopt");
    }
  } else {
    return newsock = -2;
  }
  return (I)newsock;
}

/*
 * Make socket blocking or non-blocking
 * blockflag = 1
 */
I sockblock(I sock, I blockflag)
{
  int bf = blockflag ? 0 : 1;
  return ioctl((int)sock, FIONBIO, &bf);
}

/*
 * Connect to a port on a host where there is expected to be a listening
 * server.  Returns the file descriptor of the socket or -1 on failure.
 */
I sockconnect(char *host, I port)
{
  int sock;
  struct sockaddr_in server;
  int toggle;
  struct hostent *hp=0;
#if defined(APLUS_THREAD_SAFE_FUNCTIONS)
  struct hostent hostentStruct;
  char charBuf[1024];
  int err;
#endif

  /* Open a socket */
  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    (void)perror("socket");
    return -1;
  }

  /* Connect to a server socket */
  APLUS_GETHOSTBYNAME(host,&hostentStruct,charBuf,1024,&err,hp);
  if (hp==0)
    {
      (void)perror("gethostbyname");
      (void)close(sock);
      return -1;
    }
  (void)bcopy((char *)hp->h_addr, (char *)&server.sin_addr, hp->h_length);
  server.sin_port = htons((u_short)port);
  server.sin_family = AF_INET;
  if (connect(sock, (struct sockaddr *)&server, sizeof(server))) {
    (void)perror("connect");
    (void)close(sock);
    return -1;
  }
  toggle = 1;
  if (setsockopt(sock, SOL_SOCKET, SO_KEEPALIVE,
		(char *)(&toggle), sizeof(toggle)) < 0)
  {
    (void)perror("setsockopt");
  }
  return (I)sock;
}

I awrite(I fd, A a)
{
  int cnt;
  I c;
  char *cp = (char *)a;
  int bytes;

  switch (a->t) {
  case Ct: bytes = AH + a->n*sizeof(C) + 1; break;
  case It: bytes = AH + a->n*sizeof(I); break;
  case Ft: bytes = AH + a->n*sizeof(F); break;
  default: bytes = 0; break;
  };

  if (c = a->c) a->c = 0;
  while (bytes > 0) {
    if ((cnt = write((int)fd, cp, bytes)) == -1) {
      if (errno != EWOULDBLOCK) {
	if (c) a->c = c;
	return -1;	/* ERROR */
      } else {
	if (c) a->c = c;
	return -2;	/* WOULD BLOCK */
      }
    } else {
      cp += cnt;
      bytes -= cnt;
    }
  }
  if (c) a->c = c;
  return 0;		/* OK */
}

I aread(I fd, I waitflag)
{
  A z;
  struct a a;
  fd_set rfd;
  char *cp = (char *)&a;
  int bytes;
  int cnt;

  FD_ZERO(&rfd);
  FD_SET((int)fd, &rfd);
  if (!waitflag) {
    if (select(FD_SETSIZE, &rfd, NULL, NULL, &timeout) < 0) {
      (void)perror("select");
      return gz();
    }
  }
  if (FD_ISSET((int)fd, &rfd)) {
    bytes = AH;
    while (bytes > 0) {
      if ((cnt = read((int)fd, cp, bytes)) == -1) {
	if (errno != EWOULDBLOCK) return gz();
      } else if (cnt == 0) {	/* EOF */
	return gz();
      } else {
	cp += cnt;
	bytes -= cnt;
      }
    }
    if ((z = ga(a.t, a.r, a.n, a.d)) != NULL) {
      switch (a.t) {
      case Ct: bytes = a.n*sizeof(C) + 1; break;
      case It: bytes = a.n*sizeof(I); break;
      case Ft: bytes = a.n*sizeof(F); break;
      default: bytes = 0; break;
      }
      cp = (char *)z->p;
      while (bytes > 0) {
	if ((cnt = read((int)fd, cp, bytes)) == -1) {
	  if (errno != EWOULDBLOCK) {
	    dc(z);
	    return gz();
	  }
	} else if (cnt == 0) {	/* EOF */
	  dc(z);
	  return gz();
	} else {
	  cp += cnt;
	  bytes -= cnt;
	}
      }
    }
  } else {
    return gz();
  }
  return (I)z;
}

I areadstat(I fd, I waitflag, A stat)
{
  A z;
  struct a a;
  fd_set rfd;
  char *cp = (char *)&a;
  int bytes;
  int cnt;

  FD_ZERO(&rfd);
  FD_SET((int)fd, &rfd);
  if (!waitflag) {
    if (select(FD_SETSIZE, &rfd, NULL, NULL, &timeout) < 0) {
      (void)perror("select");
      *stat->p = -1;
      return gz();
    }
  }
  if (FD_ISSET((int)fd, &rfd)) {
    *stat->p = 0;
    bytes = AH;
    while (bytes > 0) {
      if ((cnt = read((int)fd, cp, bytes)) == -1) {
	if (errno != EWOULDBLOCK) {
	  *stat->p = -1;
	  return gz();
	}
      } else if (cnt == 0) {	/* EOF */
	*stat->p = -1;
	return gz();
      } else {
	cp += cnt;
	bytes -= cnt;
      }
    }
    if ((z = ga(a.t, a.r, a.n, a.d)) != NULL) {
      switch (a.t) {
      case Ct: bytes = a.n*sizeof(C) + 1; break;
      case It: bytes = a.n*sizeof(I); break;
      case Ft: bytes = a.n*sizeof(F); break;
      default: bytes = 0; break;
      }
      cp = (char *)z->p;
      while (bytes > 0) {
	if ((cnt = read((int)fd, cp, bytes)) == -1) {
	  if (errno != EWOULDBLOCK) {
	    dc(z);
	    *stat->p = -1;
	    return gz();
	  }
	} else if (cnt == 0) {	/* EOF */
	  dc(z);
	  *stat->p = -1;
	  return gz();
	} else {
	  cp += cnt;
	  bytes -= cnt;
	}
      }
    }
  } else {
    *stat->p = -2;	/* WOULD BLOCK */
    return gz();
  }
  return (I)z;
}

I areadwait(I fd, I sec, I usec)
{
  struct timeval timeout;
  A z;
  struct a a;
  fd_set rfd;
  char *cp = (char *)&a;
  int bytes;
  int cnt;
  int num;

  timeout.tv_sec = sec;
  timeout.tv_usec = usec;

  FD_ZERO(&rfd);
  FD_SET((int)fd, &rfd);
  if ((num = select(FD_SETSIZE, &rfd, NULL, NULL, &timeout)) < 0) {
    if (errno != EINTR)
      (void)perror("select");
    return gz();
  }
  if (num == 0) return gz();
  if (FD_ISSET((int)fd, &rfd)) {
    bytes = AH;
    while (bytes > 0) {
      if ((cnt = read((int)fd, cp, bytes)) == -1) {
	if (errno != EWOULDBLOCK) {
	  if (bytes != AH) {
	    (void)perror("areadwait: corrupted");
	    close(fd);
	  }
	  return gz();
	}
      } else if (cnt == 0) {	/* EOF */
	return gz();
      } else {
	cp += cnt;
	bytes -= cnt;
      }
    }
    if ((z = ga(a.t, a.r, a.n, a.d)) != NULL) {
      switch (a.t) {
      case Ct: bytes = a.n*sizeof(C) + 1; break;
      case It: bytes = a.n*sizeof(I); break;
      case Ft: bytes = a.n*sizeof(F); break;
      default: bytes = 0; break;
      }
      cp = (char *)z->p;
      while (bytes > 0) {
	if ((cnt = read((int)fd, cp, bytes)) == -1) {
	  if (errno != EWOULDBLOCK) {
	    (void)perror("areadwait: corrupted");
	    close(fd);
	    dc(z);
	    return gz();
	  }
	} else if (cnt == 0) {	/* EOF */
	  dc(z);
	  return gz();
	} else {
	  cp += cnt;
	  bytes -= cnt;
	}
      }
    } else {
      (void)perror("areadwait: corrupted");
      close(fd);
      return gz();
    }
  } else {
    return gz();
  }
  return (I)z;
}


void asockInstall()
{
  CX saveCx=Cx;
  Cx=cx("sys");

  install(socklisten, "socklisten", IV,1,IV,0,0,0,0,0,0,0);
  install(sockaccept, "sockaccept", IV,2,IV,IV,0,0,0,0,0,0);
  install(sockblock, "sockblock", IV,2,IV,IV,0,0,0,0,0,0);
  install(sockconnect, "sockconnect", IV,2,CP,IV,0,0,0,0,0,0);
  install(awrite, "awrite", IV,2,IV,A_,0,0,0,0,0,0);
  install(aread, "aread", A_,2,IV,IV,0,0,0,0,0,0);
  install(areadstat, "areadstat", A_,3,IV,IV,A_,0,0,0,0,0);
  install(areadwait, "areadwait", A_,3,IV,IV,IV,0,0,0,0,0);

  Cx = saveCx;
  return;
}
