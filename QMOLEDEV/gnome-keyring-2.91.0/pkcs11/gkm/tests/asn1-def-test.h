#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <libtasn1.h>

const ASN1_ARRAY_TYPE test_asn1_tab[] = {
  { "TEST", 536872976, NULL },
  { NULL, 1073741836, NULL },
  { "TestIntegers", 1610612741, NULL },
  { "mpi", 3, NULL },
  { "TestData", 536870917, NULL },
  { "data", 1073741831, NULL },
  { "boolean", 536903684, NULL },
  { NULL, 131081, NULL },
  { NULL, 0, NULL }
};
