/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-unix-credentials.c - write and read unix credentials on socket

   Copyright (C) 2003 Red Hat, Inc

   Gnome keyring is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.
  
   Gnome keyring is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
  
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Alexander Larsson <alexl@redhat.com>
   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "egg-unix-credentials.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#if defined(HAVE_GETPEERUCRED)
#include <ucred.h>
#endif

int
egg_unix_credentials_read (int sock, pid_t *pid, uid_t *uid)
{
	struct msghdr msg;
	struct iovec iov;
	char buf;
	int ret;
	
#if defined(HAVE_CMSGCRED) || defined(LOCAL_CREDS)
	/* Prefer CMSGCRED over LOCAL_CREDS because the former provides the
	 * remote PID. */
#if defined(HAVE_CMSGCRED)
	struct cmsgcred *cred;
#else /* defined(LOCAL_CREDS) */
	struct sockcred *cred;
#endif
	union {
		struct cmsghdr hdr;
		char cred[CMSG_SPACE (sizeof *cred)];
	} cmsg;
#endif
	
	*pid = 0;
	*uid = 0;
	
	/* If LOCAL_CREDS are used in this platform, they have already been
	 * initialized by init_connection prior to sending of the credentials
	 * byte we receive below. */
	
	iov.iov_base = &buf;
	iov.iov_len = 1;
	
	memset (&msg, 0, sizeof (msg));
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;
	
#if defined(HAVE_CMSGCRED) || defined(LOCAL_CREDS)
	memset (&cmsg, 0, sizeof (cmsg));
	msg.msg_control = (caddr_t) &cmsg;
	msg.msg_controllen = CMSG_SPACE(sizeof *cred);
#endif

 again:
	ret = recvmsg (sock, &msg, 0);

 	if (ret < 0) {
		if (errno == EINTR)
			goto again;
		return -1;
		
	} else if (ret == 0) {
		/* Disconnected */
		return -1;
	}
	
	if (buf != '\0') {
		fprintf (stderr, "credentials byte was not nul\n");
		return -1;
	}

#if defined(HAVE_CMSGCRED) || defined(LOCAL_CREDS)
	if (cmsg.hdr.cmsg_len < CMSG_LEN (sizeof *cred) ||
	    cmsg.hdr.cmsg_type != SCM_CREDS) {
		fprintf (stderr, "message from recvmsg() was not SCM_CREDS\n");
		return -1;
	}
#endif

	{
#ifdef SO_PEERCRED
		struct ucred cr;   
		socklen_t cr_len = sizeof (cr);
		
		if (getsockopt (sock, SOL_SOCKET, SO_PEERCRED, &cr, &cr_len) == 0 &&
		    cr_len == sizeof (cr)) {
			*pid = cr.pid;
			*uid = cr.uid;
		} else {
			fprintf (stderr, "failed to getsockopt() credentials, returned len %d/%d\n",
				     cr_len, (int) sizeof (cr));
			return -1;
		}
#elif defined(HAVE_CMSGCRED)
		cred = (struct cmsgcred *) CMSG_DATA (&cmsg.hdr);
		*pid = cred->cmcred_pid;
		*uid = cred->cmcred_euid;
#elif defined(LOCAL_CREDS)
		cred = (struct sockcred *) CMSG_DATA (&cmsg.hdr);
		*pid = 0;
		*uid = cred->sc_euid;
		set_local_creds(sock, 0);
#elif defined(HAVE_GETPEEREID) /* OpenBSD */
		uid_t euid;
		gid_t egid;
		*pid = 0;

		if (getpeereid (sock, &euid, &egid) == 0) {
			*uid = euid;
		} else {
			fprintf (stderr, "getpeereid() failed: %s\n", strerror (errno)); 
			return -1;
		}
#elif defined(HAVE_GETPEERUCRED)
		ucred_t *uc = NULL;

		if (getpeerucred (sock, &uc) == 0) {
			*pid = ucred_getpid (uc);
			*uid = ucred_geteuid (uc);
			ucred_free (uc);
		} else {
			fprintf (stderr, "getpeerucred() failed: %s\n", strerror (errno));
			return -1;
		}
#else /* !SO_PEERCRED && !HAVE_CMSGCRED */
		fprintf (stderr, "socket credentials not supported on this OS\n");
		return -1;
#endif
	}

	return 0;
}

int
egg_unix_credentials_write (int socket)
{
	char buf;
	int bytes_written;
#if defined(HAVE_CMSGCRED) && (!defined(LOCAL_CREDS) || defined(__FreeBSD__))
	union {
		struct cmsghdr hdr;
		char cred[CMSG_SPACE (sizeof (struct cmsgcred))];
	} cmsg;
	struct iovec iov;
	struct msghdr msg;
#endif

	buf = 0;

#if defined(HAVE_CMSGCRED) && (!defined(LOCAL_CREDS) || defined(__FreeBSD__))
	iov.iov_base = &buf;
	iov.iov_len = 1;

	memset (&msg, 0, sizeof (msg));
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;

	msg.msg_control = (caddr_t) &cmsg;
	msg.msg_controllen = CMSG_SPACE (sizeof (struct cmsgcred));
	memset (&cmsg, 0, sizeof (cmsg));
	cmsg.hdr.cmsg_len = CMSG_LEN (sizeof (struct cmsgcred));
	cmsg.hdr.cmsg_level = SOL_SOCKET;
	cmsg.hdr.cmsg_type = SCM_CREDS;
#endif

again:

#if defined(HAVE_CMSGCRED) && (!defined(LOCAL_CREDS) || defined(__FreeBSD__))
	bytes_written = sendmsg (socket, &msg, 0);
#else
	bytes_written = write (socket, &buf, 1);
#endif

	if (bytes_written < 0 && errno == EINTR)
		goto again;

	if (bytes_written <= 0)
		return -1;
		
	return 0;
}

int
egg_unix_credentials_setup (int sock)
{
	int retval = 0;
#if defined(LOCAL_CREDS) && !defined(HAVE_CMSGCRED)
	int val = 1;
	if (setsockopt (sock, 0, LOCAL_CREDS, &val, sizeof (val)) < 0) {
		fprintf (stderr, "unable to set LOCAL_CREDS socket option on fd %d\n", fd);
		retval = -1;
	}
#endif
	return retval;
}

char*
egg_unix_credentials_executable (pid_t pid)
{
	char *result = NULL;

	/* Try and figure out the path from the pid */
#if defined(__linux__) || defined(__FreeBSD__)
	char path[1024];
	char buffer[64];
	int count;

#if defined(__linux__)
	snprintf (buffer, sizeof (buffer), "/proc/%d/exe", (int)pid);
#elif defined(__FreeBSD__)
	snprintf (buffer, sizeof (buffer), "/proc/%d/file", (int)pid);
#endif

	count = readlink (buffer, path, sizeof (path));
	if (count < 0)
		fprintf (stderr, "readlink failed for file: %s", buffer);
	else
		result = strndup (path, count);
#endif

	return result;
}
