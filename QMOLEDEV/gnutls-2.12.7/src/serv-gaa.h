
#line 104 "gaa.skel"
/* GAA HEADER */
#ifndef GAA_HEADER_POKY
#define GAA_HEADER_POKY

typedef struct _gaainfo gaainfo;

struct _gaainfo
{
#line 91 "serv.gaa"
	char *priorities;
#line 88 "serv.gaa"
	char *opaque_prf_input;
#line 85 "serv.gaa"
	char *srp_passwd_conf;
#line 82 "serv.gaa"
	char *srp_passwd;
#line 79 "serv.gaa"
	char *psk_hint;
#line 76 "serv.gaa"
	char *psk_passwd;
#line 73 "serv.gaa"
	int disable_client_cert;
#line 70 "serv.gaa"
	int require_cert;
#line 67 "serv.gaa"
	char *x509_dsacertfile;
#line 64 "serv.gaa"
	char *x509_dsakeyfile;
#line 61 "serv.gaa"
	char *x509_certfile;
#line 58 "serv.gaa"
	char *x509_keyfile;
#line 55 "serv.gaa"
	char *pgp_subkey;
#line 52 "serv.gaa"
	char *pgp_certfile;
#line 49 "serv.gaa"
	char *pgp_keyfile;
#line 46 "serv.gaa"
	char *pgp_keyring;
#line 43 "serv.gaa"
	char *x509_crlfile;
#line 40 "serv.gaa"
	char *x509_cafile;
#line 37 "serv.gaa"
	int fmtder;
#line 34 "serv.gaa"
	char *dh_params_file;
#line 30 "serv.gaa"
	int http;
#line 27 "serv.gaa"
	int noticket;
#line 24 "serv.gaa"
	int nodb;
#line 21 "serv.gaa"
	int quiet;
#line 18 "serv.gaa"
	int port;
#line 15 "serv.gaa"
	int generate;
#line 12 "serv.gaa"
	int debug;

#line 114 "gaa.skel"
};

#ifdef __cplusplus
extern "C"
{
#endif

    int gaa(int argc, char *argv[], gaainfo *gaaval);

    void gaa_help(void);
    
    int gaa_file(const char *name, gaainfo *gaaval);
    
#ifdef __cplusplus
}
#endif


#endif
