#if !defined(_HASH_C_H)
#define _HASH_C_H

#include "unix32_c.h"

#define HASH_BUFFER_LEN 131072
unsigned char hash_buffer[HASH_BUFFER_LEN];

#define ML_HASH(HASH_NAME,HASH_CONTEXT,HASH_INIT,HASH_APPEND,HASH_FINISH) \
value HASH_NAME##_unsafe64_fd (value digest_v, value fd_v, value pos_v, value len_v) \
{ \
  OS_FD fd = Fd_val(fd_v); \
  off_t pos = Int64_val(pos_v); \
  off_t len = Int64_val(len_v); \
  unsigned char *digest = String_val(digest_v); \
  HASH_CONTEXT context; \
  int nread; \
 \
  HASH_INIT (&context); \
  os_lseek(fd, pos, SEEK_SET); \
 \
  while (len!=0){ \
    int max_nread = HASH_BUFFER_LEN > len ? len : HASH_BUFFER_LEN; \
 \
    nread = os_read (fd, hash_buffer, max_nread); \
 \
    if(nread < 0) { \
      unix_error(errno, "md4_safe_fd: Read", Nothing); \
    } \
 \
    if(nread == 0){ \
      HASH_FINISH (&context, digest); \
 \
      return Val_unit; \
    } \
 \
    HASH_APPEND (&context, hash_buffer, nread); \
    len -= nread; \
  } \
  HASH_FINISH (&context, digest); \
 \
  return Val_unit; \
} \
\
value HASH_NAME##_unsafe_string(value digest_v, value string_v, value len_v) \
{ \
  unsigned char *digest = String_val(digest_v); \
  unsigned char *string = String_val(string_v); \
  long len = Long_val(len_v); \
  HASH_CONTEXT context; \
 \
  HASH_INIT (&context); \
  HASH_APPEND (&context, string, len); \
  HASH_FINISH (&context, digest); \
  \
  return Val_unit; \
} \
 \
value HASH_NAME##_unsafe_file (value digest_v, value filename_v, value file_size) \
{ \
  char *filename  = String_val(filename_v); \
  unsigned char *digest = String_val(digest_v); \
  FILE *file; \
  HASH_CONTEXT context; \
  int len; \
 \
  if ((file = fopen (filename, "rb")) == NULL) \
    raise_not_found(); \
 \
  else { \
    HASH_INIT (&context); \
    while ((len = fread (hash_buffer, 1, HASH_BUFFER_LEN, file)) >0) \
      HASH_APPEND (&context, hash_buffer, len); \
    HASH_FINISH (&context, digest); \
 \
    fclose (file); \
  } \
  return Val_unit; \
} \
\
value HASH_NAME##_init_file(value unit){\
  return (value) NULL; \
} \
\
value HASH_NAME##_update_file(value context_v, value s_v, value pos_v, value len_v){\
  return Val_unit; \
} \
\
value HASH_NAME##_finish_file(value digest_v, value context_v){\
  return Val_unit; \
}\


#define ML_HASH2(HASH_NAME,HASH_CONTEXT,HASH_INIT,HASH_APPEND,HASH_FINISH) \
 \
value HASH_NAME##_context_size_ml(value unit)  \
{  \
  return Val_int(sizeof(HASH_CONTEXT));  \
}  \
  \
value HASH_NAME##_context_init_ml(value ctx_v)  \
{  \
  HASH_CONTEXT *ctx = (HASH_CONTEXT*) ctx_v;  \
  HASH_INIT(ctx);  \
  return Val_unit;  \
}  \
  \
value HASH_NAME##_context_append_ml(value ctx_v,   \
  value str_v, value pos_v, value len_v)  \
{  \
  HASH_CONTEXT *ctx = (HASH_CONTEXT*) ctx_v;  \
  unsigned char *str = String_val(str_v);  \
  long int pos = Long_val(pos_v);  \
  long int len = Long_val(len_v);  \
  \
  HASH_APPEND( ctx, str + pos, len );  \
  return Val_unit;  \
}  \
  \
value HASH_NAME##_context_finish_ml(value digest_v, value ctx_v)  \
{  \
  unsigned char *digest = String_val(digest_v);  \
  HASH_CONTEXT *ctx = (HASH_CONTEXT*) ctx_v;  \
  \
  HASH_FINISH (ctx, digest);  \
  return Val_unit;  \
}  \



#endif
