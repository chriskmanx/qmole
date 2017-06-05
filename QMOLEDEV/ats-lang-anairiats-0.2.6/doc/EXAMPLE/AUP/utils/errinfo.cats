#ifndef ERRINFO_SATS
#define ERRINFO_SATS
#include <errno.h>
typedef struct {
  char *errinfo_loc ;
  int errinfo_errno ;
  char *errinfo_errstr ;
} ats_errinfo_type ;
#endif // end of [ERRINFO_SATS]

