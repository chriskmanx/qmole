/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
 * 2009, 2010 Free Software Foundation, Inc.
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

#include <gnutls_int.h>
#include "gnutls_errors.h"
#include <libtasn1.h>
#ifdef STDC_HEADERS
#include <stdarg.h>
#endif

/* I18n of error codes. */
#include "gettext.h"
#define _(String) dgettext (PACKAGE, String)
#define N_(String) gettext_noop (String)

#define ERROR_ENTRY(desc, name, fatal) \
	{ desc, #name, name, fatal}

struct gnutls_error_entry
{
  const char *desc;
  const char *_name;
  int number;
  int fatal;                    /* whether this error is fatal and the session for handshake 
                                 * should be terminated.
                                 */
};
typedef struct gnutls_error_entry gnutls_error_entry;

static const gnutls_error_entry error_algorithms[] = {
  /* "Short Description", Error code define, critical (0,1) -- 1 in most cases */
  ERROR_ENTRY (N_("Success."), GNUTLS_E_SUCCESS, 0),
  ERROR_ENTRY (N_("Could not negotiate a supported cipher suite."),
               GNUTLS_E_UNKNOWN_CIPHER_SUITE, 1),
  ERROR_ENTRY (N_("The cipher type is unsupported."),
               GNUTLS_E_UNKNOWN_CIPHER_TYPE, 1),
  ERROR_ENTRY (N_("The certificate and the given key do not match."),
               GNUTLS_E_CERTIFICATE_KEY_MISMATCH, 1),
  ERROR_ENTRY (N_("Could not negotiate a supported compression method."),
               GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM, 1),
  ERROR_ENTRY (N_("An unknown public key algorithm was encountered."),
               GNUTLS_E_UNKNOWN_PK_ALGORITHM, 1),

  ERROR_ENTRY (N_("An algorithm that is not enabled was negotiated."),
               GNUTLS_E_UNWANTED_ALGORITHM, 1),
  ERROR_ENTRY (N_("A large TLS record packet was received."),
               GNUTLS_E_LARGE_PACKET, 1),
  ERROR_ENTRY (N_("A record packet with illegal version was received."),
               GNUTLS_E_UNSUPPORTED_VERSION_PACKET, 1),
  ERROR_ENTRY (N_
               ("The Diffie-Hellman prime sent by the server is not acceptable (not long enough)."),
               GNUTLS_E_DH_PRIME_UNACCEPTABLE, 1),
  ERROR_ENTRY (N_("A TLS packet with unexpected length was received."),
               GNUTLS_E_UNEXPECTED_PACKET_LENGTH, 1),
  ERROR_ENTRY (N_
               ("The specified session has been invalidated for some reason."),
               GNUTLS_E_INVALID_SESSION, 1),

  ERROR_ENTRY (N_("GnuTLS internal error."), GNUTLS_E_INTERNAL_ERROR, 1),
  ERROR_ENTRY (N_("An illegal TLS extension was received."),
               GNUTLS_E_RECEIVED_ILLEGAL_EXTENSION, 1),
  ERROR_ENTRY (N_("A TLS fatal alert has been received."),
               GNUTLS_E_FATAL_ALERT_RECEIVED, 1),
  ERROR_ENTRY (N_("An unexpected TLS packet was received."),
               GNUTLS_E_UNEXPECTED_PACKET, 1),
  ERROR_ENTRY (N_("A TLS warning alert has been received."),
               GNUTLS_E_WARNING_ALERT_RECEIVED, 0),
  ERROR_ENTRY (N_
               ("An error was encountered at the TLS Finished packet calculation."),
               GNUTLS_E_ERROR_IN_FINISHED_PACKET, 1),
  ERROR_ENTRY (N_("The peer did not send any certificate."),
               GNUTLS_E_NO_CERTIFICATE_FOUND, 1),
  ERROR_ENTRY (N_("The given DSA key is incompatible with the selected TLS protocol."),
               GNUTLS_E_INCOMPAT_DSA_KEY_WITH_TLS_PROTOCOL, 1),

  ERROR_ENTRY (N_("There is already a crypto algorithm with lower priority."),
               GNUTLS_E_CRYPTO_ALREADY_REGISTERED, 1),

  ERROR_ENTRY (N_("No temporary RSA parameters were found."),
               GNUTLS_E_NO_TEMPORARY_RSA_PARAMS, 1),
  ERROR_ENTRY (N_("No temporary DH parameters were found."),
               GNUTLS_E_NO_TEMPORARY_DH_PARAMS, 1),
  ERROR_ENTRY (N_("An unexpected TLS handshake packet was received."),
               GNUTLS_E_UNEXPECTED_HANDSHAKE_PACKET, 1),
  ERROR_ENTRY (N_("The scanning of a large integer has failed."),
               GNUTLS_E_MPI_SCAN_FAILED, 1),
  ERROR_ENTRY (N_("Could not export a large integer."),
               GNUTLS_E_MPI_PRINT_FAILED, 1),
  ERROR_ENTRY (N_("Decryption has failed."), GNUTLS_E_DECRYPTION_FAILED, 1),
  ERROR_ENTRY (N_("Encryption has failed."), GNUTLS_E_ENCRYPTION_FAILED, 1),
  ERROR_ENTRY (N_("Public key decryption has failed."),
               GNUTLS_E_PK_DECRYPTION_FAILED, 1),
  ERROR_ENTRY (N_("Public key encryption has failed."),
               GNUTLS_E_PK_ENCRYPTION_FAILED, 1),
  ERROR_ENTRY (N_("Public key signing has failed."), GNUTLS_E_PK_SIGN_FAILED,
               1),
  ERROR_ENTRY (N_("Public key signature verification has failed."),
               GNUTLS_E_PK_SIG_VERIFY_FAILED, 1),
  ERROR_ENTRY (N_("Decompression of the TLS record packet has failed."),
               GNUTLS_E_DECOMPRESSION_FAILED, 1),
  ERROR_ENTRY (N_("Compression of the TLS record packet has failed."),
               GNUTLS_E_COMPRESSION_FAILED, 1),

  ERROR_ENTRY (N_("Internal error in memory allocation."),
               GNUTLS_E_MEMORY_ERROR, 1),
  ERROR_ENTRY (N_("An unimplemented or disabled feature has been requested."),
               GNUTLS_E_UNIMPLEMENTED_FEATURE, 1),
  ERROR_ENTRY (N_("Insufficient credentials for that request."),
               GNUTLS_E_INSUFFICIENT_CREDENTIALS, 1),
  ERROR_ENTRY (N_("Error in password file."), GNUTLS_E_SRP_PWD_ERROR, 1),
  ERROR_ENTRY (N_("Wrong padding in PKCS1 packet."), GNUTLS_E_PKCS1_WRONG_PAD,
               1),
  ERROR_ENTRY (N_("The requested session has expired."), GNUTLS_E_EXPIRED, 1),
  ERROR_ENTRY (N_("Hashing has failed."), GNUTLS_E_HASH_FAILED, 1),
  ERROR_ENTRY (N_("Base64 decoding error."), GNUTLS_E_BASE64_DECODING_ERROR,
               1),
  ERROR_ENTRY (N_("Base64 unexpected header error."),
               GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR,
               1),
  ERROR_ENTRY (N_("Base64 encoding error."), GNUTLS_E_BASE64_ENCODING_ERROR,
               1),
  ERROR_ENTRY (N_("Parsing error in password file."),
               GNUTLS_E_SRP_PWD_PARSING_ERROR, 1),
  ERROR_ENTRY (N_("The requested data were not available."),
               GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE, 1),
  ERROR_ENTRY (N_("Error in the pull function."), GNUTLS_E_PULL_ERROR, 1),
  ERROR_ENTRY (N_("Error in the push function."), GNUTLS_E_PUSH_ERROR, 1),
  ERROR_ENTRY (N_
               ("The upper limit of record packet sequence numbers has been reached. Wow!"),
               GNUTLS_E_RECORD_LIMIT_REACHED, 1),
  ERROR_ENTRY (N_("Error in the certificate."), GNUTLS_E_CERTIFICATE_ERROR,
               1),
  ERROR_ENTRY (N_("Unknown Subject Alternative name in X.509 certificate."),
               GNUTLS_E_X509_UNKNOWN_SAN, 1),

  ERROR_ENTRY (N_("Unsupported critical extension in X.509 certificate."),
               GNUTLS_E_X509_UNSUPPORTED_CRITICAL_EXTENSION, 1),
  ERROR_ENTRY (N_("Key usage violation in certificate has been detected."),
               GNUTLS_E_KEY_USAGE_VIOLATION, 1),
  ERROR_ENTRY (N_("Resource temporarily unavailable, try again."),
               GNUTLS_E_AGAIN, 0),
  ERROR_ENTRY (N_("Function was interrupted."), GNUTLS_E_INTERRUPTED, 0),
  ERROR_ENTRY (N_("Rehandshake was requested by the peer."),
               GNUTLS_E_REHANDSHAKE, 0),
  ERROR_ENTRY (N_
               ("TLS Application data were received, while expecting handshake data."),
               GNUTLS_E_GOT_APPLICATION_DATA, 1),
  ERROR_ENTRY (N_("Error in Database backend."), GNUTLS_E_DB_ERROR, 1),
  ERROR_ENTRY (N_("The certificate type is not supported."),
               GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE, 1),
  ERROR_ENTRY (N_("The given memory buffer is too short to hold parameters."),
               GNUTLS_E_SHORT_MEMORY_BUFFER, 1),
  ERROR_ENTRY (N_("The request is invalid."), GNUTLS_E_INVALID_REQUEST, 1),
  ERROR_ENTRY (N_("An illegal parameter has been received."),
               GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER, 1),
  ERROR_ENTRY (N_("Error while reading file."), GNUTLS_E_FILE_ERROR, 1),

  ERROR_ENTRY (N_("ASN1 parser: Element was not found."),
               GNUTLS_E_ASN1_ELEMENT_NOT_FOUND, 1),
  ERROR_ENTRY (N_("ASN1 parser: Identifier was not found"),
               GNUTLS_E_ASN1_IDENTIFIER_NOT_FOUND, 1),
  ERROR_ENTRY (N_("ASN1 parser: Error in DER parsing."),
               GNUTLS_E_ASN1_DER_ERROR, 1),
  ERROR_ENTRY (N_("ASN1 parser: Value was not found."),
               GNUTLS_E_ASN1_VALUE_NOT_FOUND, 1),
  ERROR_ENTRY (N_("ASN1 parser: Generic parsing error."),
               GNUTLS_E_ASN1_GENERIC_ERROR, 1),
  ERROR_ENTRY (N_("ASN1 parser: Value is not valid."),
               GNUTLS_E_ASN1_VALUE_NOT_VALID, 1),
  ERROR_ENTRY (N_("ASN1 parser: Error in TAG."), GNUTLS_E_ASN1_TAG_ERROR, 1),
  ERROR_ENTRY (N_("ASN1 parser: error in implicit tag"),
               GNUTLS_E_ASN1_TAG_IMPLICIT, 1),
  ERROR_ENTRY (N_("ASN1 parser: Error in type 'ANY'."),
               GNUTLS_E_ASN1_TYPE_ANY_ERROR, 1),
  ERROR_ENTRY (N_("ASN1 parser: Syntax error."), GNUTLS_E_ASN1_SYNTAX_ERROR,
               1),
  ERROR_ENTRY (N_("ASN1 parser: Overflow in DER parsing."),
               GNUTLS_E_ASN1_DER_OVERFLOW, 1),

  ERROR_ENTRY (N_("Too many empty record packets have been received."),
               GNUTLS_E_TOO_MANY_EMPTY_PACKETS, 1),
  ERROR_ENTRY (N_("The initialization of GnuTLS-extra has failed."),
               GNUTLS_E_INIT_LIBEXTRA, 1),
  ERROR_ENTRY (N_
               ("The GnuTLS library version does not match the GnuTLS-extra library version."),
               GNUTLS_E_LIBRARY_VERSION_MISMATCH, 1),
  ERROR_ENTRY (N_("The gcrypt library version is too old."),
               GNUTLS_E_INCOMPATIBLE_GCRYPT_LIBRARY, 1),

  ERROR_ENTRY (N_("The tasn1 library version is too old."),
               GNUTLS_E_INCOMPATIBLE_LIBTASN1_LIBRARY, 1),
  ERROR_ENTRY (N_("The OpenPGP User ID is revoked."),
               GNUTLS_E_OPENPGP_UID_REVOKED, 1),
  ERROR_ENTRY (N_("The OpenPGP key has not a preferred key set."),
               GNUTLS_E_OPENPGP_PREFERRED_KEY_ERROR, 1),
  ERROR_ENTRY (N_("Error loading the keyring."),
               GNUTLS_E_OPENPGP_KEYRING_ERROR, 1),
  ERROR_ENTRY (N_("The initialization of crypto backend has failed."),
               GNUTLS_E_CRYPTO_INIT_FAILED, 1),
  ERROR_ENTRY (N_("The initialization of LZO has failed."),
               GNUTLS_E_LZO_INIT_FAILED, 1),
  ERROR_ENTRY (N_("No supported compression algorithms have been found."),
               GNUTLS_E_NO_COMPRESSION_ALGORITHMS, 1),
  ERROR_ENTRY (N_("No supported cipher suites have been found."),
               GNUTLS_E_NO_CIPHER_SUITES, 1),
  ERROR_ENTRY (N_("Could not get OpenPGP key."),
               GNUTLS_E_OPENPGP_GETKEY_FAILED, 1),
  ERROR_ENTRY (N_("Could not find OpenPGP subkey."),
               GNUTLS_E_OPENPGP_SUBKEY_ERROR, 1),
  ERROR_ENTRY (N_("Safe renegotiation failed."),
               GNUTLS_E_SAFE_RENEGOTIATION_FAILED, 1),
  ERROR_ENTRY (N_("Unsafe renegotiation denied."),
               GNUTLS_E_UNSAFE_RENEGOTIATION_DENIED, 1),

  ERROR_ENTRY (N_("The SRP username supplied is illegal."),
               GNUTLS_E_ILLEGAL_SRP_USERNAME, 1),
  ERROR_ENTRY (N_("The SRP username supplied is unknown."),
               GNUTLS_E_UNKNOWN_SRP_USERNAME, 1),

  ERROR_ENTRY (N_("The OpenPGP fingerprint is not supported."),
               GNUTLS_E_OPENPGP_FINGERPRINT_UNSUPPORTED, 1),
  ERROR_ENTRY (N_("The signature algorithm is not supported."),
               GNUTLS_E_UNSUPPORTED_SIGNATURE_ALGORITHM, 1),
  ERROR_ENTRY (N_("The certificate has unsupported attributes."),
               GNUTLS_E_X509_UNSUPPORTED_ATTRIBUTE, 1),
  ERROR_ENTRY (N_("The OID is not supported."), GNUTLS_E_X509_UNSUPPORTED_OID,
               1),
  ERROR_ENTRY (N_("The hash algorithm is unknown."),
               GNUTLS_E_UNKNOWN_HASH_ALGORITHM, 1),
  ERROR_ENTRY (N_("The PKCS structure's content type is unknown."),
               GNUTLS_E_UNKNOWN_PKCS_CONTENT_TYPE, 1),
  ERROR_ENTRY (N_("The PKCS structure's bag type is unknown."),
               GNUTLS_E_UNKNOWN_PKCS_BAG_TYPE, 1),
  ERROR_ENTRY (N_("The given password contains invalid characters."),
               GNUTLS_E_INVALID_PASSWORD, 1),
  ERROR_ENTRY (N_("The Message Authentication Code verification failed."),
               GNUTLS_E_MAC_VERIFY_FAILED, 1),
  ERROR_ENTRY (N_("Some constraint limits were reached."),
               GNUTLS_E_CONSTRAINT_ERROR, 1),
  ERROR_ENTRY (N_("Failed to acquire random data."), GNUTLS_E_RANDOM_FAILED,
               1),

  ERROR_ENTRY (N_("Received a TLS/IA Intermediate Phase Finished message"),
               GNUTLS_E_WARNING_IA_IPHF_RECEIVED, 0),
  ERROR_ENTRY (N_("Received a TLS/IA Final Phase Finished message"),
               GNUTLS_E_WARNING_IA_FPHF_RECEIVED, 0),
  ERROR_ENTRY (N_("Verifying TLS/IA phase checksum failed"),
               GNUTLS_E_IA_VERIFY_FAILED, 1),

  ERROR_ENTRY (N_("The specified algorithm or protocol is unknown."),
               GNUTLS_E_UNKNOWN_ALGORITHM, 1),

  ERROR_ENTRY (N_("The handshake data size is too large (DoS?), "
                  "check gnutls_handshake_set_max_packet_length()."),
               GNUTLS_E_HANDSHAKE_TOO_LARGE, 1),

  ERROR_ENTRY (N_("Error opening /dev/crypto"),
               GNUTLS_E_CRYPTODEV_DEVICE_ERROR, 1),

  ERROR_ENTRY (N_("Error interfacing with /dev/crypto"),
               GNUTLS_E_CRYPTODEV_IOCTL_ERROR, 1),

  ERROR_ENTRY (N_("Channel binding data not available"),
               GNUTLS_E_CHANNEL_BINDING_NOT_AVAILABLE, 1),

  ERROR_ENTRY (N_("PKCS #11 error."),
               GNUTLS_E_PKCS11_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 initialization error."),
               GNUTLS_E_PKCS11_LOAD_ERROR, 1),
  ERROR_ENTRY (N_("Error in parsing."),
               GNUTLS_E_PARSING_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in PIN."),
               GNUTLS_E_PKCS11_PIN_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 PIN should be saved."),
               GNUTLS_E_PKCS11_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in slot"),
               GNUTLS_E_PKCS11_SLOT_ERROR, 1),
  ERROR_ENTRY (N_("Thread locking error"),
               GNUTLS_E_LOCKING_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in attribute"),
               GNUTLS_E_PKCS11_ATTRIBUTE_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in device"),
               GNUTLS_E_PKCS11_DEVICE_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in data"),
               GNUTLS_E_PKCS11_DATA_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 unsupported feature"),
               GNUTLS_E_PKCS11_UNSUPPORTED_FEATURE_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in key"),
               GNUTLS_E_PKCS11_KEY_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 PIN expired"),
               GNUTLS_E_PKCS11_PIN_EXPIRED, 1),
  ERROR_ENTRY (N_("PKCS #11 PIN locked"),
               GNUTLS_E_PKCS11_PIN_LOCKED, 1),
  ERROR_ENTRY (N_("PKCS #11 error in session"),
               GNUTLS_E_PKCS11_SESSION_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in signature"),
               GNUTLS_E_PKCS11_SIGNATURE_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 error in token"),
               GNUTLS_E_PKCS11_TOKEN_ERROR, 1),
  ERROR_ENTRY (N_("PKCS #11 user error"),
               GNUTLS_E_PKCS11_USER_ERROR, 1),
  {NULL, NULL, 0, 0}
};

/**
 * gnutls_error_is_fatal:
 * @error: is a GnuTLS error code, a negative value
 *
 * If a GnuTLS function returns a negative value you may feed that
 * value to this function to see if the error condition is fatal.
 *
 * Note that you may want to check the error code manually, since some
 * non-fatal errors to the protocol may be fatal for you program.
 *
 * This function is only useful if you are dealing with errors from
 * the record layer or the handshake layer.
 *
 * Returns: 1 if the error code is fatal, for positive @error values,
 *   0 is returned.  For unknown @error values, -1 is returned.
 **/
int
gnutls_error_is_fatal (int error)
{
  int ret = 1;
  const gnutls_error_entry *p;

  /* Input sanitzation.  Positive values are not errors at all, and
     definitely not fatal. */
  if (error > 0)
    return 0;

  for (p = error_algorithms; p->desc != NULL; p++)
    {
      if (p->number == error)
        {
          ret = p->fatal;
          break;
        }
    }

  return ret;
}

/**
 * gnutls_perror:
 * @error: is a GnuTLS error code, a negative value
 *
 * This function is like perror(). The only difference is that it
 * accepts an error number returned by a gnutls function.
 **/
void
gnutls_perror (int error)
{
  fprintf (stderr, "GnuTLS error: %s\n", gnutls_strerror (error));
}


/**
 * gnutls_strerror:
 * @error: is a GnuTLS error code, a negative value
 *
 * This function is similar to strerror.  The difference is that it
 * accepts an error number returned by a gnutls function; In case of
 * an unknown error a descriptive string is sent instead of %NULL.
 *
 * Error codes are always a negative value.
 *
 * Returns: A string explaining the GnuTLS error message.
 **/
const char *
gnutls_strerror (int error)
{
  const char *ret = NULL;
  const gnutls_error_entry *p;

  for (p = error_algorithms; p->desc != NULL; p++)
    {
      if (p->number == error)
        {
          ret = p->desc;
          break;
        }
    }

  /* avoid prefix */
  if (ret == NULL)
    return _("(unknown error code)");

  return _(ret);
}

/**
 * gnutls_strerror_name:
 * @error: is an error returned by a gnutls function.
 *
 * Return the GnuTLS error code define as a string.  For example,
 * gnutls_strerror_name (GNUTLS_E_DH_PRIME_UNACCEPTABLE) will return
 * the string "GNUTLS_E_DH_PRIME_UNACCEPTABLE".
 *
 * Returns: A string corresponding to the symbol name of the error
 * code.
 *
 * Since: 2.6.0
 **/
const char *
gnutls_strerror_name (int error)
{
  const char *ret = NULL;
  const gnutls_error_entry *p;

  for (p = error_algorithms; p->desc != NULL; p++)
    {
      if (p->number == error)
        {
          ret = p->_name;
          break;
        }
    }

  return ret;
}

int
_gnutls_asn2err (int asn_err)
{
  switch (asn_err)
    {
    case ASN1_FILE_NOT_FOUND:
      return GNUTLS_E_FILE_ERROR;
    case ASN1_ELEMENT_NOT_FOUND:
      return GNUTLS_E_ASN1_ELEMENT_NOT_FOUND;
    case ASN1_IDENTIFIER_NOT_FOUND:
      return GNUTLS_E_ASN1_IDENTIFIER_NOT_FOUND;
    case ASN1_DER_ERROR:
      return GNUTLS_E_ASN1_DER_ERROR;
    case ASN1_VALUE_NOT_FOUND:
      return GNUTLS_E_ASN1_VALUE_NOT_FOUND;
    case ASN1_GENERIC_ERROR:
      return GNUTLS_E_ASN1_GENERIC_ERROR;
    case ASN1_VALUE_NOT_VALID:
      return GNUTLS_E_ASN1_VALUE_NOT_VALID;
    case ASN1_TAG_ERROR:
      return GNUTLS_E_ASN1_TAG_ERROR;
    case ASN1_TAG_IMPLICIT:
      return GNUTLS_E_ASN1_TAG_IMPLICIT;
    case ASN1_ERROR_TYPE_ANY:
      return GNUTLS_E_ASN1_TYPE_ANY_ERROR;
    case ASN1_SYNTAX_ERROR:
      return GNUTLS_E_ASN1_SYNTAX_ERROR;
    case ASN1_MEM_ERROR:
      return GNUTLS_E_SHORT_MEMORY_BUFFER;
    case ASN1_MEM_ALLOC_ERROR:
      return GNUTLS_E_MEMORY_ERROR;
    case ASN1_DER_OVERFLOW:
      return GNUTLS_E_ASN1_DER_OVERFLOW;
    default:
      return GNUTLS_E_ASN1_GENERIC_ERROR;
    }
}

void
_gnutls_mpi_log (const char *prefix, bigint_t a)
{
  size_t binlen = 0;
  void *binbuf;
  size_t hexlen;
  char *hexbuf;
  int res;

  res = _gnutls_mpi_print (a, NULL, &binlen);
  if (res != 0)
    {
      gnutls_assert ();
      _gnutls_hard_log ("MPI: can't print value (%d/%d)\n", res,
                        (int) binlen);
      return;
    }

  if (binlen > 1024 * 1024)
    {
      gnutls_assert ();
      _gnutls_hard_log ("MPI: too large mpi (%d)\n", (int) binlen);
      return;
    }

  binbuf = gnutls_malloc (binlen);
  if (!binbuf)
    {
      gnutls_assert ();
      _gnutls_hard_log ("MPI: out of memory (%d)\n", (int) binlen);
      return;
    }

  res = _gnutls_mpi_print (a, binbuf, &binlen);
  if (res != 0)
    {
      gnutls_assert ();
      _gnutls_hard_log ("MPI: can't print value (%d/%d)\n", res,
                        (int) binlen);
      gnutls_free (binbuf);
      return;
    }

  hexlen = 2 * binlen + 1;
  hexbuf = gnutls_malloc (hexlen);

  if (!hexbuf)
    {
      gnutls_assert ();
      _gnutls_hard_log ("MPI: out of memory (hex %d)\n", (int) hexlen);
      gnutls_free (binbuf);
      return;
    }

  _gnutls_bin2hex (binbuf, binlen, hexbuf, hexlen, NULL);

  _gnutls_hard_log ("MPI: length: %d\n\t%s%s\n", (int) binlen, prefix,
                    hexbuf);

  gnutls_free (hexbuf);
  gnutls_free (binbuf);
}

/* this function will output a message using the
 * caller provided function
 */
void
_gnutls_log (int level, const char *fmt, ...)
{
  va_list args;
  char *str;
  int ret;

  if (_gnutls_log_func == NULL)
    return;

  va_start (args, fmt);
  ret = vasprintf (&str, fmt, args);
  va_end (args);

  if (ret >= 0)
    {
      _gnutls_log_func (level, str);
      free (str);
    }
}

#ifndef DEBUG
#ifndef C99_MACROS

/* Without C99 macros these functions have to
 * be called. This may affect performance.
 */
void
_gnutls_null_log (void *x, ...)
{
  return;
}

#endif /* C99_MACROS */
#endif /* DEBUG */
