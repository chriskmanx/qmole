#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "libsoup/soup-types.h"

void test_init    (int argc, char **argv, GOptionEntry *entries);
void test_cleanup (void);

extern int debug_level, errors;
void debug_printf (int level, const char *format, ...);

#ifdef HAVE_APACHE
void apache_init    (void);
void apache_cleanup (void);
#endif

SoupSession *soup_test_session_new (GType type, ...);
SoupServer  *soup_test_server_new  (gboolean in_own_thread);
