/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/*********************************************************************\
* Source File:		sockforkexec.c
* Functions:		sockforkexec() - Open bidirectional pipe, fork
*			and exec
* Author:		Charles A. Ocheret
* Creation Date:	Sat Jan 21 13:52:42 EST 1989
\*********************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <errno.h>



static void handler(fd)
int fd;
{
  char buf[256];
#ifndef HAVE_STRERROR
  extern char *sys_errlist[];
  sprintf(buf, "handler: <%d>: %s\n", errno, sys_errlist[errno]);
#else
  sprintf(buf, "handler: <%d>: %s\n", errno, strerror(errno));
#endif
  (void)write(fd, buf, strlen(buf));
}

int sfe(name, argv)
char *name;
char *argv[];
{
  int fildes[2];
  
  if (socketpair(PF_UNIX, SOCK_STREAM, 0, fildes) == -1) {
    fildes[0] = -1;
  } else {
    /* Fork new process */
#if defined(__sgi)
    switch (fork()) {
#else
    switch (vfork()) {
#endif
    case -1:		/* Error */
      fildes[0] = -1;
      (void)close(fildes[1]);
      break;
    case 0:		/* Child process */
      /* Close unneeded file descriptor */
      (void)close(fildes[0]);

      /* Make socket the stdin of the child process */
      (void)close(0);
      if (dup(fildes[1]) != 0) {
/*	handler(fildes[1]); */
	_exit(1);
      }

      /* Make socket the stdout of the child process */
      (void)close(1);
      if (dup(fildes[1]) != 1) {
/*	handler(fildes[1]); */
	_exit(1);
      }
      (void)close(fildes[1]);

      /* Exec the new program */
      (void)execvp(name, argv);

      /* Failed to exec */
      handler(1);
      _exit(1);
      break;
    default:		/* Parent process */
      (void)close(fildes[1]);
      break;
    }
  }
  return fildes[0];
}


void sfeInstall()
{
  CX saveCx=Cx;
  Cx=cx("sys");
  
  install((PFI)sfe, "sfe", IV,2,CP,IP,0,0,0,0,0,0);
  
  Cx = saveCx;
  return;
}
