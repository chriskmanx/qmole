/* 
   pakchois PKCS#11 interface -- error mapping
   Copyright (C) 2008, Joe Orton <joe@manyfish.co.uk>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA
*/

/*
  This code is directly derived from the scute.org PKCS#11 cryptoki
  interface, which is:

   Copyright 2006, 2007 g10 Code GmbH
   Copyright 2006 Andreas Jellinghaus

   This file is free software; as a special exception the author gives
   unlimited permission to copy and/or distribute it, with or without
   modifications, as long as this notice is preserved.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY, to the extent permitted by law; without even
   the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.
*/

#include "config.h"

#include "pakchois.h"

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(x) dgettext(PACKAGE_NAME, x)
#else
#define _(x) x
#endif

const char *
pakchois_error (ck_rv_t rv)
{
  if (rv >= CKR_VENDOR_DEFINED)
    {
      return _("Vendor defined error");
    }

  switch (rv)
    {
    case CKR_OK:
      return _("OK");
    case CKR_CANCEL:
      return _("Cancel");
    case CKR_HOST_MEMORY:
      return _("Host memory");
    case CKR_SLOT_ID_INVALID:
      return _("Slot id invalid");
    case CKR_GENERAL_ERROR:
      return _("General error");
    case CKR_FUNCTION_FAILED:
      return _("Function failed");
    case CKR_ARGUMENTS_BAD:
      return _("Arguments bad");
    case CKR_NO_EVENT:
      return _("No event");
    case CKR_NEED_TO_CREATE_THREADS:
      return _("Need to create threads");
    case CKR_CANT_LOCK:
      return _("Can't lock");
    case CKR_ATTRIBUTE_READ_ONLY:
      return _("Attribute read only");
    case CKR_ATTRIBUTE_SENSITIVE:
      return _("Attribute sensitive");
    case CKR_ATTRIBUTE_TYPE_INVALID:
      return _("Attribute type invalid");
    case CKR_ATTRIBUTE_VALUE_INVALID:
      return _("Attribute value invalid");
    case CKR_DATA_INVALID:
      return _("Data invalid");
    case CKR_DATA_LEN_RANGE:
      return _("Data len range");
    case CKR_DEVICE_ERROR:
      return _("Device error");
    case CKR_DEVICE_MEMORY:
      return _("Device memory");
    case CKR_DEVICE_REMOVED:
      return _("Device removed");
    case CKR_ENCRYPTED_DATA_INVALID:
      return _("Encrypted data invalid");
    case CKR_ENCRYPTED_DATA_LEN_RANGE:
      return _("Encrypted data len range");
    case CKR_FUNCTION_CANCELED:
      return _("Function canceled");
    case CKR_FUNCTION_NOT_PARALLEL:
      return _("Function not parallel");
    case CKR_FUNCTION_NOT_SUPPORTED:
      return _("Function not supported");
    case CKR_KEY_HANDLE_INVALID:
      return _("Key handle invalid");
    case CKR_KEY_SIZE_RANGE:
      return _("Key size range");
    case CKR_KEY_TYPE_INCONSISTENT:
      return _("Key type inconsistent");
    case CKR_KEY_NOT_NEEDED:
      return _("Key not needed");
    case CKR_KEY_CHANGED:
      return _("Key changed");
    case CKR_KEY_NEEDED:
      return _("Key needed");
    case CKR_KEY_INDIGESTIBLE:
      return _("Key indigestible");
    case CKR_KEY_FUNCTION_NOT_PERMITTED:
      return _("Key function not permitted");
    case CKR_KEY_NOT_WRAPPABLE:
      return _("Key not wrappable");
    case CKR_KEY_UNEXTRACTABLE:
      return _("Key unextractable");
    case CKR_MECHANISM_INVALID:
      return _("Mechanism invalid");
    case CKR_MECHANISM_PARAM_INVALID:
      return _("Mechanism param invalid");
    case CKR_OBJECT_HANDLE_INVALID:
      return _("Object handle invalid");
    case CKR_OPERATION_ACTIVE:
      return _("Operation active");
    case CKR_OPERATION_NOT_INITIALIZED:
      return _("Operation not initialized");
    case CKR_PIN_INCORRECT:
      return _("PIN incorrect");
    case CKR_PIN_INVALID:
      return _("PIN invalid");
    case CKR_PIN_LEN_RANGE:
      return _("PIN len range");
    case CKR_PIN_EXPIRED:
      return _("PIN expired");
    case CKR_PIN_LOCKED:
      return _("PIN locked");
    case CKR_SESSION_CLOSED:
      return _("Session closed");
    case CKR_SESSION_COUNT:
      return _("Session count");
    case CKR_SESSION_HANDLE_INVALID:
      return _("Session handle invalid");
    case CKR_SESSION_PARALLEL_NOT_SUPPORTED:
      return _("Session parallel not supported");
    case CKR_SESSION_READ_ONLY:
      return _("Session read only");
    case CKR_SESSION_EXISTS:
      return _("Session exists");
    case CKR_SESSION_READ_ONLY_EXISTS:
      return _("Session read only exists");
    case CKR_SESSION_READ_WRITE_SO_EXISTS:
      return _("Session read write so exists");
    case CKR_SIGNATURE_INVALID:
      return _("Signature invalid");
    case CKR_SIGNATURE_LEN_RANGE:
      return _("Signature length range");
    case CKR_TEMPLATE_INCOMPLETE:
      return _("Template incomplete");
    case CKR_TEMPLATE_INCONSISTENT:
      return _("Template inconsistent");
    case CKR_TOKEN_NOT_PRESENT:
      return _("Token not present");
    case CKR_TOKEN_NOT_RECOGNIZED:
      return _("Token not recognized");
    case CKR_TOKEN_WRITE_PROTECTED:
      return _("Token write protected");
    case CKR_UNWRAPPING_KEY_HANDLE_INVALID:
      return _("Unwrapping key handle invalid");
    case CKR_UNWRAPPING_KEY_SIZE_RANGE:
      return _("Unwrapping key size range");
    case CKR_UNWRAPPING_KEY_TYPE_INCONSISTENT:
      return _("Unwrapping key type inconsistent");
    case CKR_USER_ALREADY_LOGGED_IN:
      return _("User already logged in");
    case CKR_USER_NOT_LOGGED_IN:
      return _("User not logged in");
    case CKR_USER_PIN_NOT_INITIALIZED:
      return _("User PIN not initialized");
    case CKR_USER_TYPE_INVALID:
      return _("User type invalid");
    case CKR_USER_ANOTHER_ALREADY_LOGGED_IN:
      return _("Another user already logged in");
    case CKR_USER_TOO_MANY_TYPES:
      return _("User too many types");
    case CKR_WRAPPED_KEY_INVALID:
      return _("Wrapped key invalid");
    case CKR_WRAPPED_KEY_LEN_RANGE:
      return _("Wrapped key length range");
    case CKR_WRAPPING_KEY_HANDLE_INVALID:
      return _("Wrapping key handle invalid");
    case CKR_WRAPPING_KEY_SIZE_RANGE:
      return _("Wrapping key size range");
    case CKR_WRAPPING_KEY_TYPE_INCONSISTENT:
      return _("Wrapping key type inconsistent");
    case CKR_RANDOM_SEED_NOT_SUPPORTED:
      return _("Random seed not supported");
    case CKR_RANDOM_NO_RNG:
      return _("Random no rng");
    case CKR_DOMAIN_PARAMS_INVALID:
      return _("Domain params invalid");
    case CKR_BUFFER_TOO_SMALL:
      return _("Buffer too small");
    case CKR_SAVED_STATE_INVALID:
      return _("Saved state invalid");
    case CKR_INFORMATION_SENSITIVE:
      return _("Information sensitive");
    case CKR_STATE_UNSAVEABLE:
      return _("State unsaveable");
    case CKR_CRYPTOKI_NOT_INITIALIZED:
      return _("Cryptoki not initialized");
    case CKR_CRYPTOKI_ALREADY_INITIALIZED:
      return _("Cryptoki already initialized");
    case CKR_MUTEX_BAD:
      return _("Mutex bad");
    case CKR_MUTEX_NOT_LOCKED:
      return _("Mutex not locked");
    case CKR_FUNCTION_REJECTED:
      return _("Function rejected");
    default:
      break;
    }

  return _("Unknown error");
}
