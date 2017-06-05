#ifndef DLOPEN_H
#define DLOPEN_H

#include "config.h"

#ifdef _WIN32

#define RTLD_LOCAL 0
#define RTLD_NOW 1

void *dlopen (const char *filename, int flag);
void *dlsym (void *handle, const char *symbol);
int dlclose (void *handle);

#else

#include <dlfcn.h>

#endif

#endif
