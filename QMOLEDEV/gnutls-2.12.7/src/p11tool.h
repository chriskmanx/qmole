#ifndef P11TOOL_H
#define P11TOOL_H

#include "certtool-common.h"

void pkcs11_list (FILE * outfile, const char *url, int type,
                  unsigned int login, unsigned int detailed,
                  common_info_st *);
void pkcs11_mechanism_list (FILE * outfile, const char *url,
                            unsigned int login, common_info_st *);
void pkcs11_export (FILE * outfile, const char *pkcs11_url,
                    unsigned int login, common_info_st *);
void pkcs11_token_list (FILE * outfile, unsigned int detailed,
                        common_info_st *);
void pkcs11_write (FILE * outfile, const char *pkcs11_url, const char *label,
                   int trusted, unsigned int login, common_info_st *);
void pkcs11_delete (FILE * outfile, const char *pkcs11_url, int batch,
                    unsigned int login, common_info_st *);
void pkcs11_init (FILE * outfile, const char *pkcs11_url, const char *label,
                  common_info_st *);

#define PKCS11_TYPE_CRT_ALL 1
#define PKCS11_TYPE_TRUSTED 2
#define PKCS11_TYPE_PK 3
#define PKCS11_TYPE_ALL 4
#define PKCS11_TYPE_PRIVKEY 5


enum
{
  ACTION_PKCS11_LIST,
  ACTION_PKCS11_TOKENS,
  ACTION_PKCS11_EXPORT_URL,
  ACTION_PKCS11_WRITE_URL,
  ACTION_PKCS11_DELETE_URL,
  ACTION_PKCS11_TOKEN_INIT,
  ACTION_PKCS11_MECHANISMS,
};

#endif
