#ifndef SYSTEM_H
#define SYSTEM_H

#include <gnutls_int.h>

#ifndef _WIN32
#include <sys/uio.h>            /* for writev */
#endif

int system_errno (gnutls_transport_ptr);

#ifdef _WIN32
ssize_t system_write (gnutls_transport_ptr ptr, const void *data,
                      size_t data_size);
#else
#define HAVE_WRITEV
ssize_t system_writev (gnutls_transport_ptr ptr, const giovec_t * iovec,
                       int iovec_cnt);
#endif
ssize_t system_read (gnutls_transport_ptr ptr, void *data, size_t data_size);
ssize_t system_read_peek (gnutls_transport_ptr ptr, void *data,
                          size_t data_size);

#ifdef _WIN32
#define HAVE_WIN32_LOCKS
#else
#ifdef HAVE_LIBPTHREAD
#define HAVE_PTHREAD_LOCKS
#else
#define HAVE_NO_LOCKS
#endif
#endif

int _gnutls_atfork (void (*prepare) (void), void (*parent) (void),
                    void (*child) (void));
extern gnutls_time_func gnutls_time;

#endif /* SYSTEM_H */
