
#line 104 "gaa.skel"
/* GAA HEADER */
#ifndef GAA_HEADER_POKY
#define GAA_HEADER_POKY

typedef struct _gaainfo gaainfo;

struct _gaainfo
{
#line 103 "cli.gaa"
	char *rest_args;
#line 95 "cli.gaa"
	int insecure;
#line 92 "cli.gaa"
	char *port;
#line 89 "cli.gaa"
	char *opaque_prf_input;
#line 86 "cli.gaa"
	char *psk_key;
#line 83 "cli.gaa"
	char *psk_username;
#line 80 "cli.gaa"
	char *srp_passwd;
#line 77 "cli.gaa"
	char *srp_username;
#line 74 "cli.gaa"
	char *x509_certfile;
#line 71 "cli.gaa"
	char *x509_keyfile;
#line 68 "cli.gaa"
	char *pgp_subkey;
#line 65 "cli.gaa"
	char *pgp_certfile;
#line 62 "cli.gaa"
	char *pgp_keyring;
#line 59 "cli.gaa"
	char *pgp_keyfile;
#line 56 "cli.gaa"
	char *x509_crlfile;
#line 53 "cli.gaa"
	char *x509_cafile;
#line 50 "cli.gaa"
	char *priorities;
#line 47 "cli.gaa"
	int verbose;
#line 44 "cli.gaa"
	int record_size;
#line 41 "cli.gaa"
	int print_cert;
#line 38 "cli.gaa"
	int disable_extensions;
#line 35 "cli.gaa"
	int fingerprint;
#line 32 "cli.gaa"
	int fmtder;
#line 29 "cli.gaa"
	int crlf;
#line 26 "cli.gaa"
	int starttls;
#line 23 "cli.gaa"
	int noticket;
#line 20 "cli.gaa"
	int rehandshake;
#line 17 "cli.gaa"
	int resume;
#line 14 "cli.gaa"
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
