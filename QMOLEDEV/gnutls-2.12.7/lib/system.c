/*
 * Copyright (C) 2010 Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

#include <system.h>
#include <gnutls_int.h>
#include <gnutls_errors.h>

#include <errno.h>

#ifdef _WIN32
#include <windows.h>

#else
#ifdef HAVE_PTHREAD_LOCKS
#include <pthread.h>
#endif
#endif

/* We need to disable gnulib's replacement wrappers to get native
   Windows interfaces. */
#undef recv
#undef send

/* System specific function wrappers.
 */

/* wrappers for write() and writev()
 */
#ifdef _WIN32

int
system_errno (gnutls_transport_ptr p)
{
  int tmperr = WSAGetLastError ();
  int ret = 0;
  switch (tmperr)
    {
    case WSAEWOULDBLOCK:
      ret = EAGAIN;
      break;
    case WSAEINTR:
      ret = EINTR;
      break;
    default:
      ret = EIO;
      break;
    }
  WSASetLastError (tmperr);

  return ret;
}

ssize_t
system_write (gnutls_transport_ptr ptr, const void *data, size_t data_size)
{
  return send (GNUTLS_POINTER_TO_INT (ptr), data, data_size, 0);
}
#else /* POSIX */
int
system_errno (gnutls_transport_ptr ptr)
{
  return errno;
}

ssize_t
system_writev (gnutls_transport_ptr ptr, const giovec_t * iovec,
               int iovec_cnt)
{
  return writev (GNUTLS_POINTER_TO_INT (ptr), (struct iovec *) iovec,
                 iovec_cnt);

}
#endif

ssize_t
system_read (gnutls_transport_ptr ptr, void *data, size_t data_size)
{
  return recv (GNUTLS_POINTER_TO_INT (ptr), data, data_size, 0);
}

ssize_t
system_read_peek (gnutls_transport_ptr ptr, void *data, size_t data_size)
{
  return recv (GNUTLS_POINTER_TO_INT (ptr), data, data_size, MSG_PEEK);
}

/* Thread stuff */

#ifdef HAVE_WIN32_LOCKS


/* FIXME: win32 locks are untested */
static int
gnutls_system_mutex_init (void **priv)
{
  CRITICAL_SECTION *lock = malloc (sizeof (CRITICAL_SECTION));

  if (lock == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  InitializeCriticalSection (lock);

  *priv = lock;

  return 0;
}

static int
gnutls_system_mutex_deinit (void **priv)
{
  DeleteCriticalSection ((CRITICAL_SECTION *) * priv);
  free (*priv);

  return 0;
}

static int
gnutls_system_mutex_lock (void **priv)
{
  EnterCriticalSection ((CRITICAL_SECTION *) * priv);
  return 0;
}

static int
gnutls_system_mutex_unlock (void **priv)
{
  LeaveCriticalSection ((CRITICAL_SECTION *) * priv);
  return 0;
}

int
_gnutls_atfork (void (*prepare) (void), void (*parent) (void),
                void (*child) (void))
{
  return 0;
}


#endif /* WIN32_LOCKS */

#ifdef HAVE_PTHREAD_LOCKS

static int
gnutls_system_mutex_init (void **priv)
{
  pthread_mutex_t *lock = malloc (sizeof (pthread_mutex_t));
  int ret;

  if (lock == NULL)
    return GNUTLS_E_MEMORY_ERROR;

  ret = pthread_mutex_init (lock, NULL);
  if (ret)
    {
      free (lock);
      gnutls_assert ();
      return GNUTLS_E_LOCKING_ERROR;
    }

  *priv = lock;

  return 0;
}

static int
gnutls_system_mutex_deinit (void **priv)
{
  pthread_mutex_destroy ((pthread_mutex_t *) * priv);
  free (*priv);
  return 0;
}

static int
gnutls_system_mutex_lock (void **priv)
{
  if (pthread_mutex_lock ((pthread_mutex_t *) * priv))
    {
      gnutls_assert ();
      return GNUTLS_E_LOCKING_ERROR;
    }

  return 0;
}

static int
gnutls_system_mutex_unlock (void **priv)
{
  if (pthread_mutex_unlock ((pthread_mutex_t *) * priv))
    {
      gnutls_assert ();
      return GNUTLS_E_LOCKING_ERROR;
    }

  return 0;
}

int
_gnutls_atfork (void (*prepare) (void), void (*parent) (void),
                void (*child) (void))
{
  return pthread_atfork (prepare, parent, child);
}

#endif /* PTHREAD_LOCKS */

#ifdef HAVE_NO_LOCKS

static int
gnutls_system_mutex_init (void **priv)
{
  return 0;
}

static int
gnutls_system_mutex_deinit (void **priv)
{
  return 0;
}

static int
gnutls_system_mutex_lock (void **priv)
{
  return 0;
}

static int
gnutls_system_mutex_unlock (void **priv)
{
  return 0;
}

int
_gnutls_atfork (void (*prepare) (void), void (*parent) (void),
                void (*child) (void))
{
  return 0;
}

#endif /* NO_LOCKS */

gnutls_time_func gnutls_time = time;
mutex_init_func gnutls_mutex_init = gnutls_system_mutex_init;
mutex_deinit_func gnutls_mutex_deinit = gnutls_system_mutex_deinit;
mutex_lock_func gnutls_mutex_lock = gnutls_system_mutex_lock;
mutex_unlock_func gnutls_mutex_unlock = gnutls_system_mutex_unlock;
