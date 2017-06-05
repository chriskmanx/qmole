/*
 * Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software
 * Foundation, Inc.
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

/* Here lies the code of the gnutls_*_set_priority() functions.
 */

#include "gnutls_int.h"
#include "gnutls_algorithms.h"
#include "gnutls_errors.h"
#include <gnutls_num.h>

static void
break_comma_list (char *etag,
                  char **broken_etag, int *elements, int max_elements,
                  char sep);

/**
 * gnutls_cipher_set_priority:
 * @session: is a #gnutls_session_t structure.
 * @list: is a 0 terminated list of gnutls_cipher_algorithm_t elements.
 *
 * Sets the priority on the ciphers supported by gnutls.  Priority is
 * higher for elements specified before others.  After specifying the
 * ciphers you want, you must append a 0.  Note that the priority is
 * set on the client. The server does not use the algorithm's
 * priority except for disabling algorithms that were not specified.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_cipher_set_priority (gnutls_session_t session, const int *list)
{
  int num = 0, i;

  while (list[num] != 0)
    num++;
  if (num > MAX_ALGOS)
    num = MAX_ALGOS;
  session->internals.priorities.cipher.algorithms = num;

  for (i = 0; i < num; i++)
    {
      session->internals.priorities.cipher.priority[i] = list[i];
    }

  return 0;
}

typedef void (bulk_rmadd_func) (priority_st * priority_list, const int *);

inline static void
_set_priority (priority_st * st, const int *list)
{
  int num = 0, i;

  while (list[num] != 0)
    num++;
  if (num > MAX_ALGOS)
    num = MAX_ALGOS;
  st->algorithms = num;

  for (i = 0; i < num; i++)
    {
      st->priority[i] = list[i];
    }

  return;
}

static void
_clear_priorities (priority_st * st, const int *list)
{
  memset(st, 0, sizeof(*st));  
}

/**
 * gnutls_kx_set_priority:
 * @session: is a #gnutls_session_t structure.
 * @list: is a 0 terminated list of gnutls_kx_algorithm_t elements.
 *
 * Sets the priority on the key exchange algorithms supported by
 * gnutls.  Priority is higher for elements specified before others.
 * After specifying the algorithms you want, you must append a 0.
 * Note that the priority is set on the client. The server does not
 * use the algorithm's priority except for disabling algorithms that
 * were not specified.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_kx_set_priority (gnutls_session_t session, const int *list)
{
  _set_priority (&session->internals.priorities.kx, list);
  return 0;
}

/**
 * gnutls_mac_set_priority:
 * @session: is a #gnutls_session_t structure.
 * @list: is a 0 terminated list of gnutls_mac_algorithm_t elements.
 *
 * Sets the priority on the mac algorithms supported by gnutls.
 * Priority is higher for elements specified before others.  After
 * specifying the algorithms you want, you must append a 0.  Note
 * that the priority is set on the client. The server does not use
 * the algorithm's priority except for disabling algorithms that were
 * not specified.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_mac_set_priority (gnutls_session_t session, const int *list)
{
  _set_priority (&session->internals.priorities.mac, list);
  return 0;
}

/**
 * gnutls_compression_set_priority:
 * @session: is a #gnutls_session_t structure.
 * @list: is a 0 terminated list of gnutls_compression_method_t elements.
 *
 * Sets the priority on the compression algorithms supported by
 * gnutls.  Priority is higher for elements specified before others.
 * After specifying the algorithms you want, you must append a 0.
 * Note that the priority is set on the client. The server does not
 * use the algorithm's priority except for disabling algorithms that
 * were not specified.
 *
 * TLS 1.0 does not define any compression algorithms except
 * NULL. Other compression algorithms are to be considered as gnutls
 * extensions.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_compression_set_priority (gnutls_session_t session, const int *list)
{
  _set_priority (&session->internals.priorities.compression, list);
  return 0;
}

/**
 * gnutls_protocol_set_priority:
 * @session: is a #gnutls_session_t structure.
 * @list: is a 0 terminated list of gnutls_protocol_t elements.
 *
 * Sets the priority on the protocol versions supported by gnutls.
 * This function actually enables or disables protocols. Newer protocol
 * versions always have highest priority.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_protocol_set_priority (gnutls_session_t session, const int *list)
{
  _set_priority (&session->internals.priorities.protocol, list);

  /* set the current version to the first in the chain.
   * This will be overridden later.
   */
  if (list)
    _gnutls_set_current_version (session, list[0]);

  return 0;
}

/**
 * gnutls_certificate_type_set_priority:
 * @session: is a #gnutls_session_t structure.
 * @list: is a 0 terminated list of gnutls_certificate_type_t elements.
 *
 * Sets the priority on the certificate types supported by gnutls.
 * Priority is higher for elements specified before others.
 * After specifying the types you want, you must append a 0.
 * Note that the certificate type priority is set on the client.
 * The server does not use the cert type priority except for disabling
 * types that were not specified.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_certificate_type_set_priority (gnutls_session_t session,
                                      const int *list)
{
#ifdef ENABLE_OPENPGP
  _set_priority (&session->internals.priorities.cert_type, list);
  return 0;
#else

  return GNUTLS_E_UNIMPLEMENTED_FEATURE;

#endif
}

static const int protocol_priority[] = {
  GNUTLS_TLS1_2,
  GNUTLS_TLS1_1,
  GNUTLS_TLS1_0,
  GNUTLS_SSL3,
  0
};

static const int kx_priority_performance[] = {
  GNUTLS_KX_RSA,
  GNUTLS_KX_DHE_RSA,
  GNUTLS_KX_DHE_DSS,
  /* GNUTLS_KX_ANON_DH: Man-in-the-middle prone, don't add!
   * GNUTLS_KX_RSA_EXPORT: Deprecated, don't add!
   */
  0
};

static const int kx_priority_export[] = {
  GNUTLS_KX_RSA,
  GNUTLS_KX_DHE_RSA,
  GNUTLS_KX_DHE_DSS,
  GNUTLS_KX_RSA_EXPORT,
  0
};

static const int kx_priority_secure[] = {
  /* The ciphersuites that offer forward secrecy take
   * precendance
   */
  GNUTLS_KX_DHE_RSA,
  GNUTLS_KX_DHE_DSS,
  GNUTLS_KX_RSA,
  /* GNUTLS_KX_ANON_DH: Man-in-the-middle prone, don't add!
   * GNUTLS_KX_RSA_EXPORT: Deprecated, don't add!
   */
  0
};

static const int cipher_priority_performance[] = {
  GNUTLS_CIPHER_ARCFOUR_128,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_128_CBC,
#endif
  GNUTLS_CIPHER_AES_128_CBC,
  GNUTLS_CIPHER_3DES_CBC,
  GNUTLS_CIPHER_AES_256_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_256_CBC,
#endif
  /* GNUTLS_CIPHER_ARCFOUR_40: Insecure, don't add! */
  0
};

static const int cipher_priority_normal[] = {
  GNUTLS_CIPHER_AES_128_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_128_CBC,
#endif
  GNUTLS_CIPHER_AES_256_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_256_CBC,
#endif
  GNUTLS_CIPHER_3DES_CBC,
  GNUTLS_CIPHER_ARCFOUR_128,
  /* GNUTLS_CIPHER_ARCFOUR_40: Insecure, don't add! */
  0
};

static const int cipher_priority_secure128[] = {
  GNUTLS_CIPHER_AES_128_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_128_CBC,
#endif
  GNUTLS_CIPHER_3DES_CBC,
  GNUTLS_CIPHER_ARCFOUR_128,
  /* GNUTLS_CIPHER_ARCFOUR_40: Insecure, don't add! */
  0
};


static const int cipher_priority_secure256[] = {
  GNUTLS_CIPHER_AES_256_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_256_CBC,
#endif
  GNUTLS_CIPHER_AES_128_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_128_CBC,
#endif
  GNUTLS_CIPHER_3DES_CBC,
  GNUTLS_CIPHER_ARCFOUR_128,
  /* GNUTLS_CIPHER_ARCFOUR_40: Insecure, don't add! */
  0
};

/* The same as cipher_priority_security_normal + arcfour-40. */
static const int cipher_priority_export[] = {
  GNUTLS_CIPHER_AES_128_CBC,
  GNUTLS_CIPHER_AES_256_CBC,
#ifdef	ENABLE_CAMELLIA
  GNUTLS_CIPHER_CAMELLIA_128_CBC,
  GNUTLS_CIPHER_CAMELLIA_256_CBC,
#endif
  GNUTLS_CIPHER_3DES_CBC,
  GNUTLS_CIPHER_ARCFOUR_128,
  GNUTLS_CIPHER_ARCFOUR_40,
  0
};

static const int comp_priority[] = {
  /* compression should be explicitely requested to be enabled */
  GNUTLS_COMP_NULL,
  0
};

static const int sign_priority_default[] = {
  GNUTLS_SIGN_DSA_SHA224,
  GNUTLS_SIGN_DSA_SHA256,
  GNUTLS_SIGN_RSA_SHA256,
  GNUTLS_SIGN_RSA_SHA384,
  GNUTLS_SIGN_RSA_SHA512,
  GNUTLS_SIGN_RSA_SHA1,
  GNUTLS_SIGN_DSA_SHA1,
  0
};

static const int sign_priority_secure128[] = {
  GNUTLS_SIGN_RSA_SHA256,
  GNUTLS_SIGN_RSA_SHA384,
  GNUTLS_SIGN_RSA_SHA512,
  GNUTLS_SIGN_DSA_SHA1,
  0
};

static const int sign_priority_secure256[] = {
  GNUTLS_SIGN_RSA_SHA512,
  0
};

static const int mac_priority_normal[] = {
  GNUTLS_MAC_SHA1,
  GNUTLS_MAC_SHA256,
  GNUTLS_MAC_MD5,
  0
};


static const int mac_priority_secure[] = {
  GNUTLS_MAC_SHA256,
  GNUTLS_MAC_SHA1,
  0
};

static int cert_type_priority[] = {
  GNUTLS_CRT_X509,
  GNUTLS_CRT_OPENPGP,
  0
};

typedef void (rmadd_func) (priority_st * priority_list, unsigned int alg);

static void
prio_remove (priority_st * priority_list, unsigned int algo)
{
  int i = 0;
  int pos = -1;                 /* the position of the cipher to remove */

  while (priority_list->priority[i] != 0)
    {
      if (priority_list->priority[i] == algo)
        pos = i;
      i++;
    }

  if (pos >= 0)
    {
      priority_list->priority[pos] = priority_list->priority[i - 1];
      priority_list->priority[i - 1] = 0;
      priority_list->algorithms--;
    }

  return;
}

static void
prio_add (priority_st * priority_list, unsigned int algo)
{
  register int i = 0;
  while (priority_list->priority[i] != 0)
    {
      if (algo == priority_list->priority[i])
        return;                 /* if it exists */
      i++;
    }

  if (i < MAX_ALGOS)
    {
      priority_list->priority[i] = algo;
      priority_list->algorithms++;
    }

  return;
}


/**
 * gnutls_priority_set:
 * @session: is a #gnutls_session_t structure.
 * @priority: is a #gnutls_priority_t structure.
 *
 * Sets the priorities to use on the ciphers, key exchange methods,
 * macs and compression methods.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_priority_set (gnutls_session_t session, gnutls_priority_t priority)
{
  if (priority == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_NO_CIPHER_SUITES;
    }

  memcpy (&session->internals.priorities, priority,
          sizeof (struct gnutls_priority_st));

  /* set the current version to the first in the chain.
   * This will be overridden later.
   */
  if (session->internals.priorities.protocol.algorithms > 0)
    _gnutls_set_current_version (session,
                                 session->internals.priorities.protocol.
                                 priority[0]);

  return 0;
}


#define MAX_ELEMENTS 48

/**
 * gnutls_priority_init:
 * @priority_cache: is a #gnutls_prioritity_t structure.
 * @priorities: is a string describing priorities
 * @err_pos: In case of an error this will have the position in the string the error occured
 *
 * Sets priorities for the ciphers, key exchange methods, macs and
 * compression methods.
 *
 * The #priorities option allows you to specify a colon
 * separated list of the cipher priorities to enable.
 *
 * Common keywords: Some keywords are defined to provide quick access
 * to common preferences.
 *
 * "PERFORMANCE" means all the "secure" ciphersuites are enabled,
 * limited to 128 bit ciphers and sorted by terms of speed
 * performance.
 *
 * "NORMAL" means all "secure" ciphersuites. The 256-bit ciphers are
 * included as a fallback only.  The ciphers are sorted by security
 * margin.
 *
 * "SECURE128" means all "secure" ciphersuites with ciphers up to 128
 * bits, sorted by security margin.
 *
 * "SECURE256" means all "secure" ciphersuites including the 256 bit
 * ciphers, sorted by security margin.
 *
 * "EXPORT" means all ciphersuites are enabled, including the
 * low-security 40 bit ciphers.
 *
 * "NONE" means nothing is enabled.  This disables even protocols and
 * compression methods.
 *
 * Special keywords:
 * "!" or "-" appended with an algorithm will remove this algorithm.
 *
 * "+" appended with an algorithm will add this algorithm.
 *
 * Check the GnuTLS manual section "Priority strings" for detailed
 * information.
 *
 * Examples:
 *
 * "NONE:+VERS-TLS-ALL:+MAC-ALL:+RSA:+AES-128-CBC:+SIGN-ALL:+COMP-NULL"
 *
 * "NORMAL:-ARCFOUR-128" means normal ciphers except for ARCFOUR-128.
 *
 * "SECURE:-VERS-SSL3.0:+COMP-DEFLATE" means that only secure ciphers are
 * enabled, SSL3.0 is disabled, and libz compression enabled.
 *
 * "NONE:+VERS-TLS-ALL:+AES-128-CBC:+RSA:+SHA1:+COMP-NULL:+SIGN-RSA-SHA1", 
 *
 * "NORMAL:%COMPAT" is the most compatible mode.
 *
 * Returns: On syntax error %GNUTLS_E_INVALID_REQUEST is returned,
 * %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_priority_init (gnutls_priority_t * priority_cache,
                      const char *priorities, const char **err_pos)
{
  char *broken_list[MAX_ELEMENTS];
  int broken_list_size = 0, i = 0, j;
  char *darg = NULL;
  int algo;
  rmadd_func *fn;
  bulk_rmadd_func *bulk_fn;

  *priority_cache = gnutls_calloc (1, sizeof (struct gnutls_priority_st));
  if (*priority_cache == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_MEMORY_ERROR;
    }

  /* for now unsafe renegotiation is default on everyone. To be removed
   * when we make it the default.
   */
  (*priority_cache)->sr = SR_PARTIAL;
  (*priority_cache)->ssl3_record_version = 1;

  if (priorities == NULL)
    priorities = "NORMAL";

  darg = gnutls_strdup (priorities);
  if (darg == NULL)
    {
      gnutls_assert ();
      goto error;
    }

  break_comma_list (darg, broken_list, &broken_list_size, MAX_ELEMENTS, ':');
  /* This is our default set of protocol version, certificate types and
   * compression methods.
   */
  if (strcasecmp (broken_list[0], "NONE") != 0)
    {
      _set_priority (&(*priority_cache)->protocol, protocol_priority);
      _set_priority (&(*priority_cache)->compression, comp_priority);
      _set_priority (&(*priority_cache)->cert_type, cert_type_priority);
      _set_priority (&(*priority_cache)->sign_algo, sign_priority_default);
      i = 0;
    }
  else
    {
      i = 1;
    }

  for (; i < broken_list_size; i++)
    {
      if (strcasecmp (broken_list[i], "PERFORMANCE") == 0)
        {
          _set_priority (&(*priority_cache)->cipher,
                         cipher_priority_performance);
          _set_priority (&(*priority_cache)->kx, kx_priority_performance);
          _set_priority (&(*priority_cache)->mac, mac_priority_normal);
          _set_priority (&(*priority_cache)->sign_algo,
                         sign_priority_default);
        }
      else if (strcasecmp (broken_list[i], "NORMAL") == 0)
        {
          _set_priority (&(*priority_cache)->cipher, cipher_priority_normal);
          _set_priority (&(*priority_cache)->kx, kx_priority_secure);
          _set_priority (&(*priority_cache)->mac, mac_priority_normal);
          _set_priority (&(*priority_cache)->sign_algo,
                         sign_priority_default);
        }
      else if (strcasecmp (broken_list[i], "SECURE256") == 0
               || strcasecmp (broken_list[i], "SECURE") == 0)
        {
          _set_priority (&(*priority_cache)->cipher,
                         cipher_priority_secure256);
          _set_priority (&(*priority_cache)->kx, kx_priority_secure);
          _set_priority (&(*priority_cache)->mac, mac_priority_secure);
          _set_priority (&(*priority_cache)->sign_algo,
                         sign_priority_secure256);
        }
      else if (strcasecmp (broken_list[i], "SECURE128") == 0)
        {
          _set_priority (&(*priority_cache)->cipher,
                         cipher_priority_secure128);
          _set_priority (&(*priority_cache)->kx, kx_priority_secure);
          _set_priority (&(*priority_cache)->mac, mac_priority_secure);
          _set_priority (&(*priority_cache)->sign_algo,
                         sign_priority_secure128);
        }
      else if (strcasecmp (broken_list[i], "EXPORT") == 0)
        {
          _set_priority (&(*priority_cache)->cipher, cipher_priority_export);
          _set_priority (&(*priority_cache)->kx, kx_priority_export);
          _set_priority (&(*priority_cache)->mac, mac_priority_secure);
          _set_priority (&(*priority_cache)->sign_algo,
                         sign_priority_default);
        }                       /* now check if the element is something like -ALGO */
      else if (broken_list[i][0] == '!' || broken_list[i][0] == '+'
               || broken_list[i][0] == '-')
        {
          if (broken_list[i][0] == '+')
            {
              fn = prio_add;
              bulk_fn = _set_priority;
            }
          else
            {
              fn = prio_remove;
              bulk_fn = _clear_priorities;
            }

          if ((algo =
               gnutls_mac_get_id (&broken_list[i][1])) != GNUTLS_MAC_UNKNOWN)
            fn (&(*priority_cache)->mac, algo);
          else if ((algo = gnutls_cipher_get_id (&broken_list[i][1])) !=
                   GNUTLS_CIPHER_UNKNOWN)
            fn (&(*priority_cache)->cipher, algo);
          else if ((algo = gnutls_kx_get_id (&broken_list[i][1])) !=
                   GNUTLS_KX_UNKNOWN)
            fn (&(*priority_cache)->kx, algo);
          else if (strncasecmp (&broken_list[i][1], "VERS-", 5) == 0)
            {
              if (strncasecmp (&broken_list[i][1], "VERS-TLS-ALL", 12) == 0)
                {
                  bulk_fn (&(*priority_cache)->protocol,
                                 protocol_priority);
                }
              else
                {
                  if ((algo =
                       gnutls_protocol_get_id (&broken_list[i][6])) !=
                      GNUTLS_VERSION_UNKNOWN)
                    fn (&(*priority_cache)->protocol, algo);
                  else
                    goto error;

                }
            }                   /* now check if the element is something like -ALGO */
          else if (strncasecmp (&broken_list[i][1], "COMP-", 5) == 0)
            {
              if (strncasecmp (&broken_list[i][1], "COMP-ALL", 8) == 0)
                {
                  bulk_fn (&(*priority_cache)->compression,
                                 comp_priority);
                }
              else
                {
                  if ((algo =
                       gnutls_compression_get_id (&broken_list[i][6])) !=
                      GNUTLS_COMP_UNKNOWN)
                    fn (&(*priority_cache)->compression, algo);
                  else
                    goto error;
                }
            }                   /* now check if the element is something like -ALGO */
          else if (strncasecmp (&broken_list[i][1], "CTYPE-", 6) == 0)
            {
              if (strncasecmp (&broken_list[i][1], "CTYPE-ALL", 9) == 0)
                {
                  bulk_fn (&(*priority_cache)->cert_type,
                                 cert_type_priority);
                }
              else
                {
                  if ((algo =
                       gnutls_certificate_type_get_id (&broken_list[i][7])) !=
                      GNUTLS_CRT_UNKNOWN)
                    fn (&(*priority_cache)->cert_type, algo);
                  else
                    goto error;
                }
            }                   /* now check if the element is something like -ALGO */
          else if (strncasecmp (&broken_list[i][1], "SIGN-", 5) == 0)
            {
              if (strncasecmp (&broken_list[i][1], "SIGN-ALL", 8) == 0)
                {
                  bulk_fn (&(*priority_cache)->sign_algo,
                                 sign_priority_default);
                }
              else
                {
                  if ((algo =
                       gnutls_sign_get_id (&broken_list[i][6])) !=
                      GNUTLS_SIGN_UNKNOWN)
                    fn (&(*priority_cache)->sign_algo, algo);
                  else
                    goto error;
                }
            }
          else if (strncasecmp (&broken_list[i][1], "MAC-ALL", 7) == 0)
            {
                  bulk_fn (&(*priority_cache)->mac,
                                mac_priority_secure);
            }
          else if (strncasecmp (&broken_list[i][1], "CIPHER-ALL", 7) == 0)
            {
                  bulk_fn (&(*priority_cache)->cipher,
                                cipher_priority_normal);
            }
          else
            goto error;
        }
      else if (broken_list[i][0] == '%')
        {
          if (strcasecmp (&broken_list[i][1], "COMPAT") == 0)
            {
              (*priority_cache)->no_padding = 1;
              (*priority_cache)->allow_large_records = 1;
            }
          else if (strcasecmp (&broken_list[i][1],
                               "VERIFY_ALLOW_SIGN_RSA_MD5") == 0)
            {
              prio_add (&(*priority_cache)->sign_algo, GNUTLS_SIGN_RSA_MD5);
              (*priority_cache)->additional_verify_flags |=
                GNUTLS_VERIFY_ALLOW_SIGN_RSA_MD5;
            }
          else if (strcasecmp (&broken_list[i][1],
                               "SSL3_RECORD_VERSION") == 0)
            (*priority_cache)->ssl3_record_version = 1;
          else if (strcasecmp (&broken_list[i][1],
                               "LATEST_RECORD_VERSION") == 0)
            (*priority_cache)->ssl3_record_version = 0;
          else if (strcasecmp (&broken_list[i][1],
                               "VERIFY_ALLOW_X509_V1_CA_CRT") == 0)
            (*priority_cache)->additional_verify_flags |=
              GNUTLS_VERIFY_ALLOW_X509_V1_CA_CRT;
          else if (strcasecmp (&broken_list[i][1],
                               "UNSAFE_RENEGOTIATION") == 0)
            {
              (*priority_cache)->sr = SR_UNSAFE;
            }
          else if (strcasecmp (&broken_list[i][1], "SAFE_RENEGOTIATION") == 0)
            {
              (*priority_cache)->sr = SR_SAFE;
            }
          else if (strcasecmp (&broken_list[i][1],
                               "PARTIAL_RENEGOTIATION") == 0)
            {
              (*priority_cache)->sr = SR_PARTIAL;
            }
          else if (strcasecmp (&broken_list[i][1],
                               "DISABLE_SAFE_RENEGOTIATION") == 0)
            {
              (*priority_cache)->sr = SR_DISABLED;
            }
          else
            goto error;
        }
      else
        goto error;
    }

  gnutls_free (darg);
  return 0;

error:
  if (err_pos != NULL && i < broken_list_size)
    {
      *err_pos = priorities;
      for (j = 0; j < i; j++)
        {
          (*err_pos) += strlen (broken_list[j]) + 1;
        }
    }
  gnutls_free (darg);
  gnutls_free (*priority_cache);

  return GNUTLS_E_INVALID_REQUEST;

}

/**
 * gnutls_priority_deinit:
 * @priority_cache: is a #gnutls_prioritity_t structure.
 *
 * Deinitializes the priority cache.
 **/
void
gnutls_priority_deinit (gnutls_priority_t priority_cache)
{
  gnutls_free (priority_cache);
}


/**
 * gnutls_priority_set_direct:
 * @session: is a #gnutls_session_t structure.
 * @priorities: is a string describing priorities
 * @err_pos: In case of an error this will have the position in the string the error occured
 *
 * Sets the priorities to use on the ciphers, key exchange methods,
 * macs and compression methods.  This function avoids keeping a
 * priority cache and is used to directly set string priorities to a
 * TLS session.  For documentation check the gnutls_priority_init().
 *
 * Returns: On syntax error %GNUTLS_E_INVALID_REQUEST is returned,
 * %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_priority_set_direct (gnutls_session_t session,
                            const char *priorities, const char **err_pos)
{
  gnutls_priority_t prio;
  int ret;

  ret = gnutls_priority_init (&prio, priorities, err_pos);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = gnutls_priority_set (session, prio);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  gnutls_priority_deinit (prio);

  return 0;
}

/* Breaks a list of "xxx", "yyy", to a character array, of
 * MAX_COMMA_SEP_ELEMENTS size; Note that the given string is modified.
  */
static void
break_comma_list (char *etag,
                  char **broken_etag, int *elements, int max_elements,
                  char sep)
{
  char *p = etag;
  if (sep == 0)
    sep = ',';

  *elements = 0;

  do
    {
      broken_etag[*elements] = p;

      (*elements)++;

      p = strchr (p, sep);
      if (p)
        {
          *p = 0;
          p++;                  /* move to next entry and skip white
                                 * space.
                                 */
          while (*p == ' ')
            p++;
        }
    }
  while (p != NULL && *elements < max_elements);
}

/**
 * gnutls_set_default_priority:
 * @session: is a #gnutls_session_t structure.
 *
 * Sets some default priority on the ciphers, key exchange methods,
 * macs and compression methods.
 *
 * This is the same as calling:
 *
 * gnutls_priority_set_direct (session, "NORMAL", NULL);
 *
 * This function is kept around for backwards compatibility, but
 * because of its wide use it is still fully supported.  If you wish
 * to allow users to provide a string that specify which ciphers to
 * use (which is recommended), you should use
 * gnutls_priority_set_direct() or gnutls_priority_set() instead.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_set_default_priority (gnutls_session_t session)
{
  return gnutls_priority_set_direct (session, "NORMAL", NULL);
}

/**
 * gnutls_set_default_export_priority:
 * @session: is a #gnutls_session_t structure.
 *
 * Sets some default priority on the ciphers, key exchange methods, macs
 * and compression methods.  This function also includes weak algorithms.
 *
 * This is the same as calling:
 *
 * gnutls_priority_set_direct (session, "EXPORT", NULL);
 *
 * This function is kept around for backwards compatibility, but
 * because of its wide use it is still fully supported.  If you wish
 * to allow users to provide a string that specify which ciphers to
 * use (which is recommended), you should use
 * gnutls_priority_set_direct() or gnutls_priority_set() instead.
 *
 * Returns: %GNUTLS_E_SUCCESS on success, or an error code.
 **/
int
gnutls_set_default_export_priority (gnutls_session_t session)
{
  return gnutls_priority_set_direct (session, "EXPORT", NULL);
}
