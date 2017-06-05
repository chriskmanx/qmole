#ifndef I18N_H
#define I18N_H

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif

#if ENABLE_NLS
#include <libintl.h>
#define _(String) gettext (String)
#define gettext_noop(String) String
#define N_(String) gettext_noop (String)
#else
#define _(String) (String)
#define N_(String) (String)
#endif

#endif
