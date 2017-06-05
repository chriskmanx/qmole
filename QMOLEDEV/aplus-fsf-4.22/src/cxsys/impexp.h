#ifndef included_cxsys_impexp_h
#define included_cxsys_impexp_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif
extern char *ExportAObject(A, char *, I, I *);
extern I ExportAObjectSizePass(A, const char *,const I, I*, I*);
extern I ExportAObjectFillPass(A, const char *,const I, I, char *);
extern A AExportAObject(A, char *, I, I *);
extern A ImportAObject(char *, I, char *);
#ifdef __cplusplus
}
#endif

#endif

