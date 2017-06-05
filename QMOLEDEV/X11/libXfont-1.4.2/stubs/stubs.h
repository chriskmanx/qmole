/* $XFree86: xc/lib/font/stubs/stubs.h,v 1.2 1999/08/21 13:48:07 dawes Exp $ */

/* This directory includes dummy entry for bdftopcf and mkfontdir */

#include <stdio.h>
#include "fntfilst.h"
#include "font.h"
#include "fontstruct.h"

#ifndef True
#define True (-1)
#endif
#ifndef False
#define False (0)
#endif

extern FontPtr find_old_font ( FSID id );
extern FontResolutionPtr GetClientResolutions ( int *num );
extern int GetDefaultPointSize ( void );
extern int set_font_authorizations ( char **authorizations, 
				     int *authlen, 
				     ClientPtr client );
extern unsigned long GetTimeInMillis (void);
extern void ErrorF(char *f);
extern void FatalError(char *f);


/* end of file */
