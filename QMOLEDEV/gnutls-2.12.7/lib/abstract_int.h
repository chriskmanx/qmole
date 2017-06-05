#ifndef _ABSTRACT_INT_H
# define _ABSTRACT_INT_H

#include <gnutls/abstract.h>

int _gnutls_privkey_get_public_mpis (gnutls_privkey_t key,
                                     bigint_t * params, int *params_size);

#endif
