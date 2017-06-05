/* pkcs11x.h
   Copyright 2010 Collabora Ltd

   This file is free software; as a special exception the author gives
   unlimited permission to copy and/or distribute it, with or without
   modifications, as long as this notice is preserved.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY, to the extent permitted by law; without even
   the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  */


#ifndef PKCS11X_H
#define PKCS11X_H

#include "pkcs11.h"

#define CKA_XDG   (CKA_VENDOR_DEFINED | 0x58444700UL /* XDG0 */ )
#define CKO_XDG   (CKA_VENDOR_DEFINED | 0x58444700UL /* XDG0 */ )

/* -------------------------------------------------------------------
 * TRUST ASSERTIONS
 */

#define CKO_X_TRUST_ASSERTION                    (CKO_XDG + 100)

#define CKA_X_ASSERTION_TYPE                     (CKA_XDG + 1)

#define CKA_X_CERTIFICATE_VALUE                  (CKA_XDG + 2)

#define CKA_X_PURPOSE                            (CKA_XDG + 3)

#define CKA_X_PEER                               (CKA_XDG + 4)

typedef CK_ULONG CK_X_ASSERTION_TYPE;

#define CKT_X_DISTRUSTED_CERTIFICATE             1UL

#define CKT_X_PINNED_CERTIFICATE                 2UL

#define CKT_X_ANCHORED_CERTIFICATE               3UL

#endif /* PKCS11X_H */
