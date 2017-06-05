////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _RSA_H_INCLUDED
#define _RSA_H_INCLUDED

#include "sshbn.h"

struct RSAKey {
    int bits;
    int bytes;
#ifdef MSCRYPTOAPI
    unsigned long exponent;
    unsigned char *modulus;
#else
    Bignum modulus;
    Bignum exponent;
    Bignum private_exponent;
    Bignum p;
    Bignum q;
    Bignum iqmp;
#endif
    char *comment;
};

int makekey(unsigned char *data, struct RSAKey *result,
	    unsigned char **keystr, int order);
int makeprivate(unsigned char *data, struct RSAKey *result);
void rsaencrypt(unsigned char *data, int length, struct RSAKey *key);
Bignum rsadecrypt(Bignum input, struct RSAKey *key);
void rsasign(unsigned char *data, int length, struct RSAKey *key);
void rsasanitise(struct RSAKey *key);
int rsastr_len(struct RSAKey *key);
void rsastr_fmt(char *str, struct RSAKey *key);
void rsa_fingerprint(char *str, int len, struct RSAKey *key);
int rsa_verify(struct RSAKey *key);
unsigned char *rsa_public_blob(struct RSAKey *key, int *len);
int rsa_public_blob_len(void *data);
void freersakey(struct RSAKey *key);

int loadrsakey(char *filename, struct RSAKey *key, char *passphrase);
int rsakey_encrypted(char *filename, char **comment);
int rsakey_pubblob(char *filename, void **blob, int *bloblen);
int saversakey(char *filename, struct RSAKey *key, char *passphrase);


#endif //_RSA_H_INCLUDED

