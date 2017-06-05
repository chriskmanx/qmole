/* Internal key index structure. */
struct key_idx_s
{
  off_t offset;
  u32 keyid[2];
  byte fpr[KEY_FPR_LEN];
};
typedef struct key_idx_s *key_idx_t;

/* Internal key cache to associate a key with an file offset. */
struct key_table_s
{
  struct key_table_s *next;
  off_t offset;
};
typedef struct key_table_s *key_table_t;

typedef struct cdk_keydb_search_s
{
  off_t off;                    /* last file offset */
  union
  {
    char *pattern;              /* A search is performed by pattern. */
    u32 keyid[2];               /* A search by keyid. */
    byte fpr[KEY_FPR_LEN];      /* A search by fingerprint. */
  } u;
  int type;
  struct key_table_s *cache;
  size_t ncache;
  unsigned int no_cache:1;      /* disable the index cache. */

  cdk_stream_t idx;
  char *idx_name;               /* name of the index file or NULL. */

} cdk_keydb_search_s;

/* Internal key database handle. */
struct cdk_keydb_hd_s
{
  int type;                     /* type of the key db handle. */
  int fp_ref;                   /* 1=means it is a reference and shall not be closed. */
  cdk_stream_t fp;
  char *name;                   /* name of the underlying file or NULL. */
  unsigned int secret:1;        /* contain secret keys. */
  unsigned int isopen:1;        /* the underlying stream is opened. */

  /* structure to store some stats about the keydb. */
  struct
  {
    size_t new_keys;            /* amount of new keys that were imported. */
  } stats;
};
