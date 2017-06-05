#define PORT 5556
#define SERVER "127.0.0.1"

#include <gnutls/gnutls.h>

#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <netinet/in.h>
#include <signal.h>
#ifdef _WIN32
#include <io.h>
#include <winbase.h>
#endif

#ifndef __attribute__
#if __GNUC__ < 2 || (__GNUC__ == 2 && __GNUC_MINOR__ < 5)
#define __attribute__(Spec)     /* empty */
#endif
#endif

/* the number of elements in the priority structures.
 */
#define PRI_MAX 16

extern const char str_unknown[];

int print_info (gnutls_session_t state, const char *hostname, int insecure);
void print_cert_info (gnutls_session_t state, const char *hostname,
                      int insecure);
void print_list (int verbose);

const char *raw_to_string (const unsigned char *raw, size_t raw_size);
int service_to_port (const char *service);
void pkcs11_common (void);
void sockets_init (void);
