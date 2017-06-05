#ifndef STARTUPNOTIFICATION_H
#define STARTUPNOTIFICATION_H

#include <fx.h>

#ifdef STARTUP_NOTIFICATION

#include "libsn/sn.h"

void startup_completed (void);
Time gettimestamp (void);
int runcmd(FXString, FXString, FXString, FXString, FXbool, FXString);

#else

#include <X11/Xlib.h>

int runcmd(FXString, FXString, FXString);

#endif

#endif

