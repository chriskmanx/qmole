/*
  ImageMagick Error Methods.
*/
#ifndef _ERROR_H
#define _ERROR_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/*
  Error define definitions.
*/
#if defined(sun) && !defined(SVR4)
#if !defined(strerror)
#define strerror(n) \
  (((n) >= 0 && (n) < sys_nerr) ? sys_errlist[n] : "unknown error")

extern char
  *sys_errlist[];

extern int
  sys_nerr;
#endif
#endif

/*
  Error typedef declarations.
*/
typedef void
  (*ErrorHandler)(const unsigned int,const char *,const char *);

/*
  Error declarations.
*/
extern Export ErrorHandler
  SetErrorHandler(ErrorHandler),
  SetWarningHandler(ErrorHandler);

extern Export void
  MagickError(const unsigned int,const char *,const char *),
  MagickWarning(const unsigned int,const char *,const char *);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
