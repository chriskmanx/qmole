
#line 104 "gaa.skel"
/* GAA HEADER */
#ifndef GAA_HEADER_POKY
#define GAA_HEADER_POKY

typedef struct _gaainfo gaainfo;

struct _gaainfo
{
#line 80 "p11tool.gaa"
	int debug;
#line 75 "p11tool.gaa"
	char *outfile;
#line 72 "p11tool.gaa"
	int action;
#line 71 "p11tool.gaa"
	char* pkcs11_provider;
#line 67 "p11tool.gaa"
	int incert_format;
#line 64 "p11tool.gaa"
	int pkcs8;
#line 61 "p11tool.gaa"
	char *cert;
#line 58 "p11tool.gaa"
	char *pubkey;
#line 55 "p11tool.gaa"
	char *privkey;
#line 52 "p11tool.gaa"
	char* secret_key;
#line 48 "p11tool.gaa"
	int pkcs11_detailed_url;
#line 45 "p11tool.gaa"
	int pkcs11_login;
#line 42 "p11tool.gaa"
	int pkcs11_trusted;
#line 35 "p11tool.gaa"
	char* pkcs11_label;
#line 24 "p11tool.gaa"
	int pkcs11_type;
#line 21 "p11tool.gaa"
	char* pkcs11_url;

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
