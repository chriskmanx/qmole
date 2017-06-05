#ifndef GNUTLS_LOCKS_H
#define GNUTLS_LOCKS_H

#include <gnutls/gnutls.h>
#include <gnutls_int.h>

extern mutex_init_func gnutls_mutex_init;
extern mutex_deinit_func gnutls_mutex_deinit;
extern mutex_lock_func gnutls_mutex_lock;
extern mutex_unlock_func gnutls_mutex_unlock;

#endif
