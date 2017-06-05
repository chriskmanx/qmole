#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <libtasn1.h>

const ASN1_ARRAY_TYPE test_asn1_tab[] = {
  { "TESTS", 536872976, NULL },
  { NULL, 1073741836, NULL },
  { "TestInteger", 1073741827, NULL },
  { "TestBoolean", 1073741828, NULL },
  { "TestOctetString", 1073741831, NULL },
  { "TestGeneralized", 1082130449, NULL },
  { "TestImplicit", 1610620935, NULL },
  { NULL, 4104, "5"},
  { "TestExplicit", 1610620935, NULL },
  { NULL, 2056, "5"},
  { "TestBitString", 1073741830, NULL },
  { "TestIntegers", 1610612741, NULL },
  { "uint1", 1073741827, NULL },
  { "uint2", 1073741827, NULL },
  { "uint3", 3, NULL },
  { "TestData", 1610612741, NULL },
  { "data", 7, NULL },
  { "TestBooleanSeq", 1610612741, NULL },
  { "boolean", 536903684, NULL },
  { NULL, 131081, NULL },
  { "TestOid", 1610612741, NULL },
  { "oid", 12, NULL },
  { "TestAnySeq", 536870917, NULL },
  { "contents", 13, NULL },
  { NULL, 0, NULL }
};
