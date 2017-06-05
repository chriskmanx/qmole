/*
 * linc-compat.c: This file is part of the linc library.
 *
 * Authors:
 *    Tor Lillqvist  (tml@novell.com)
 *
 * Copyright 2005, Novell, Inc.
 */
#include "config.h"

#include <linc/linc.h>

#include "linc-compat.h"
#include "linc-debug.h"

#ifdef G_OS_WIN32

/* Map some WinSock error codes to errno values. Only those that
 * correspond to real errno values that the linc2 source code checks
 * for are mapped. They should obviously not include those errno
 * values that don't exist in the Microsoft C library, and which are
 * defined as the corresponding WSAE* value in linc-compat.h
 */
void
link_map_winsock_error_to_errno (void)
{
	int wsa_error = WSAGetLastError ();
	d_printf ("WSAGetLastError: %d\n", wsa_error);
	errno = wsa_error;
	switch (errno) {
	case WSAEBADF:
		errno = EBADF; break;
	case WSAEWOULDBLOCK:
		errno = EAGAIN; break;
	}
}

#endif

int
link_pipe (int *handles)
{
#ifndef G_OS_WIN32

  return pipe (handles);

#else

  SOCKET temp, temp2, socket1 = -1, socket2 = -1;
  struct sockaddr_in saddr;
  int len;
  u_long arg;
  fd_set read_set, write_set;
  struct timeval tv;

  temp = socket (AF_INET, SOCK_STREAM, 0);
  if (temp == INVALID_SOCKET)
    {
      link_map_winsock_error_to_errno ();
      goto out0;
    }
  
  arg = 1;
  if (ioctlsocket (temp, FIONBIO, &arg) == SOCKET_ERROR)
    {
      link_map_winsock_error_to_errno ();
      goto out0;
    }

  memset (&saddr, 0, sizeof (saddr));
  saddr.sin_family = AF_INET;
  saddr.sin_port = 0;
  saddr.sin_addr.s_addr = htonl (INADDR_LOOPBACK);

  if (bind (temp, (struct sockaddr *)&saddr, sizeof (saddr)))
    {
      link_map_winsock_error_to_errno ();
      goto out0;
    }

  if (listen (temp, 1) == SOCKET_ERROR)
    {
      link_map_winsock_error_to_errno ();
      goto out0;
    }

  len = sizeof (saddr);
  if (getsockname (temp, (struct sockaddr *)&saddr, &len))
    {
      link_map_winsock_error_to_errno ();
      goto out0;
    }

  socket1 = socket (AF_INET, SOCK_STREAM, 0);
  if (socket1 == INVALID_SOCKET)
    {
      link_map_winsock_error_to_errno ();
      goto out0;
    }
  
  if (!DuplicateHandle (GetCurrentProcess (), (HANDLE) socket1,
			GetCurrentProcess (), (LPHANDLE) &temp2,
			0, FALSE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE)) {
    goto out1;
  }
  socket1 = temp2;

  arg = 1;
  if (ioctlsocket (socket1, FIONBIO, &arg) == SOCKET_ERROR)
    { 
      link_map_winsock_error_to_errno ();
      goto out1;
    }

  if (connect (socket1, (struct sockaddr  *)&saddr, len) != SOCKET_ERROR ||
      WSAGetLastError () != WSAEWOULDBLOCK)
    {
      link_map_winsock_error_to_errno ();
      goto out1;
    }

  FD_ZERO (&read_set);
  FD_SET (temp, &read_set);
  
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  if (select (0, &read_set, NULL, NULL, NULL) == SOCKET_ERROR)
    {
      link_map_winsock_error_to_errno ();
      goto out1;
    }

  if (!FD_ISSET (temp, &read_set))
    {
      errno = WSAECONNREFUSED;	/* Oh well, whatever */
      goto out1;
    }

  socket2 = accept (temp, (struct sockaddr *) &saddr, &len);
  if (socket2 == INVALID_SOCKET)
    {
      link_map_winsock_error_to_errno ();
      goto out1;
    }

  FD_ZERO (&write_set);
  FD_SET (socket1, &write_set);

  tv.tv_sec = 0;
  tv.tv_usec = 0;

  if (select (0, NULL, &write_set, NULL, NULL) == SOCKET_ERROR)
    {
      link_map_winsock_error_to_errno ();
      goto out2;
    }

  if (!FD_ISSET (socket1, &write_set))
    {
      errno = WSAECONNREFUSED;
      goto out2;
    }

  if (!DuplicateHandle (GetCurrentProcess (), (HANDLE) socket2,
			GetCurrentProcess (), (LPHANDLE) &temp2,
			0, FALSE, DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE)) {
    goto out2;
  }
  socket2 = temp2;

  arg = 0;
  if (ioctlsocket (socket1, FIONBIO, &arg) == SOCKET_ERROR)
    {
      link_map_winsock_error_to_errno ();
      goto out2;
    }

  arg = 0;
  if (ioctlsocket (socket2, FIONBIO, &arg) == SOCKET_ERROR)
    {
      link_map_winsock_error_to_errno ();
      goto out2;
    }
  
  handles[0] = socket1;
  handles[1] = socket2;

  d_printf ("socketpair %d <-> %d\n", socket1, socket2);
  
  closesocket (temp);

  return 0;

 out2:
  closesocket (socket2);
 out1:
  closesocket (socket1);
 out0:
  closesocket (temp);

  return -1;

#endif
}

const char *
link_strerror (int number)
{
	switch (number) {
#ifdef HAVE_WINSOCK2_H
	case WSAEOPNOTSUPP:
	  return "Operation not supported on transport endpoint";
	case WSAEPFNOSUPPORT:
	  return "Protocol family not supported";
	case WSAECONNRESET:
	  return "Connection reset by peer";
	case WSAENOBUFS:
	  return "No buffer space available";
	case WSAEAFNOSUPPORT:
	  return "Address family not supported by protocol family";
	case WSAENOTSOCK:
	  return "Socket operation on non-socket";
	case WSAENOPROTOOPT:
	  return "Protocol not available";
	case WSAESHUTDOWN:
	  return "Can't send after socket shutdown";
	case WSAECONNREFUSED:
	  return "Connection refused";
	case WSAEADDRINUSE:
	  return "Address already in use";
	case WSAECONNABORTED:
	  return "Connection aborted";
	case WSAENETUNREACH:
	  return "Network is unreachable";
	case WSAENETDOWN:
	  return "Network interface is not configured";
	case WSAETIMEDOUT:
	  return "Connection timed out";
	case WSAEHOSTDOWN:
	  return "Host is down";
	case WSAEHOSTUNREACH:
	  return "Host is unreachable";
	case WSAEINPROGRESS:
	  return "Connection already in progress";
	case WSAEALREADY:
	  return "Socket already connected";
	case WSAEPROTONOSUPPORT:
	  return "Unknown protocol";
	case WSAESOCKTNOSUPPORT:
	  return "Socket type not supported";
	case WSAEADDRNOTAVAIL:
	  return "Address not available";
	case WSAEISCONN:
	  return "Socket is already connected";
	case WSAENOTCONN:
	  return "Socket is not connected";
#endif
	default:
		return g_strerror (number);
	}
}
