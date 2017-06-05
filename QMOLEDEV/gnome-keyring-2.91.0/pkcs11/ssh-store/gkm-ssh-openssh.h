#ifndef GKM_SSHOPENSSH_H_
#define GKM_SSHOPENSSH_H_

#include <glib.h>

#include <gcrypt.h>

#include "gkm/gkm-data-types.h"

GkmDataResult         gkm_ssh_openssh_parse_public_key                   (const guchar *data,
                                                                          gsize n_data,
                                                                          gcry_sexp_t *sexp,
                                                                          gchar **comment);

GkmDataResult         gkm_ssh_openssh_parse_private_key                  (const guchar *data,
                                                                          gsize n_data,
                                                                          const gchar *password,
                                                                          gssize n_password,
                                                                          gcry_sexp_t *sexp);

gchar*                gkm_ssh_openssh_digest_private_key                 (const guchar *data,
                                                                          gsize n_data);

#endif /* GKM_SSHOPENSSH_H_ */
