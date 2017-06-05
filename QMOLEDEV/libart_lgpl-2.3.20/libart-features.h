#ifndef LIBART_FEATURES_H
#define LIBART_FEATURES_H 1

#define LIBART_MAJOR_VERSION (2)
#define LIBART_MINOR_VERSION (3)
#define LIBART_MICRO_VERSION (20)
#define LIBART_VERSION "2.3.20"

#ifdef _WIN32
#  ifdef LIBART_COMPILATION
#    define LIBART_VAR __declspec(dllexport)
#  else
#    define LIBART_VAR extern __declspec(dllimport)
#  endif
#else
#  define LIBART_VAR extern
#endif

LIBART_VAR const unsigned int libart_major_version, libart_minor_version, libart_micro_version;
LIBART_VAR const char *libart_version;

void libart_preinit(void *app, void *modinfo);
void libart_postinit(void *app, void *modinfo);
#endif
