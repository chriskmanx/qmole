/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <stdio.h>
#if defined(__cplusplus)
#include <strings.h>
#else
#include <string.h>
#endif
#if !defined(__cfront)
#include <stdarg.h>
#endif
#include <errno.h>
#include <sys/time.h>
#include <time.h>

typedef void (*VPF) ();

static FILE *_Warnfp = (FILE *)2;

static void
_DefaultWarnFunc(char *s)
{
  fputs(s, _Warnfp);

/******************************************/
/* Lexa redefines fflush in stdio.h      */
/* which results in an unresolved symbol */
#if defined(__edgfe) && defined(fflush)
#undef fflush
#endif
/******************************************/

  fflush(_Warnfp);
}

static VPF _WarnFunc = _DefaultWarnFunc;

#define	MAXERRSIZE	BUFSIZ

/* char _ErrBuf[2 * MAXERRSIZE]; not thread safe moved into functions*/

#if defined(__sun__)
extern int sys_nerr;
#endif

/* preprocess the format string */
static char *
fixit(int errnum, char *s, char r[])
{
  char *z, *p = r, *cp = s, *str, ctimebuf[60];
  static struct timeval tp;
  static struct timezone tzp;

  while (cp && *cp != (char) NULL) {
    if (p == &r[MAXERRSIZE - 1])
      return ("bark!\n");
    if (*cp == '%')
      switch (*(cp + 1)) {

	/* quoted `%' */
      case '%':
	*p++ = *cp++;
	*p++ = *cp++;
	break;

	/* error string a la syslog(3) */
      case 'm':
	if (errnum < 1 || errnum > sys_nerr)
	  str = "unknown error";
	else
#ifdef HAVE_STRERROR
          str = strerror(errnum);
#else
	  str = sys_errlist[errnum];
#endif
	for (z = str; *z != (char) NULL &&
	     p < &r[MAXERRSIZE - 1]; *p++ = *z++);
	cp += 2;
	break;

	/* timestamp */
      case 't':
	if (gettimeofday(&tp, &tzp) < 0)
	  str = "(time?) ";
	else {
#if defined(__SUNPRO_C)
          str = ctime_r(&tp.tv_sec,ctimebuf,sizeof(ctimebuf));
#else
          /* For linux ctimebuf must be at least 26 */
          str = ctime_r(&tp.tv_sec,ctimebuf);
#endif
	  str[19] = '\0';
	}
	for (z = str + 4; *z != (char) NULL &&
	     p < &r[MAXERRSIZE - 1]; *p++ = *z++);
	cp += 2;
	break;

	/* normal for _doprnt */
      default:
	*p++ = *cp++;
	break;
      }
    else
      *p++ = *cp++;
  }
  *p = (char) NULL;
  return (r);
}

void 
vWarn(char *fmt, va_list ap)
{
  char r[MAXERRSIZE];
  char _ErrBuf[2 * MAXERRSIZE];
  if (_WarnFunc != (VPF) NULL) {
    int errnum = errno;
    fmt = fixit(errnum, fmt,r);
    (void) vsprintf(_ErrBuf, fmt, ap);
    (_WarnFunc) (_ErrBuf);
  }
}

void
Warn(char *fmt,...)
{
  char r[MAXERRSIZE];
  char _ErrBuf[2 * MAXERRSIZE];
  if (_WarnFunc != (VPF) NULL) {
    int errnum = errno;
    va_list ap;

    fmt = fixit(errnum, fmt,r);
    va_start(ap, fmt);
    (void) vsprintf(_ErrBuf, fmt, ap);
    va_end(ap);
    (_WarnFunc) (_ErrBuf);
  }
  return;
}

void
Abort(char *fmt,...)
{
  char r[MAXERRSIZE];
  char _ErrBuf[2 * MAXERRSIZE];
  int errnum = errno;
  va_list ap;

  fmt = fixit(errnum, fmt,r);
  va_start(ap, fmt);
  (void) vsprintf(_ErrBuf, fmt, ap);
  va_end(ap);
  if (_WarnFunc != (VPF) NULL)
    (_WarnFunc) (_ErrBuf);
  else
    _DefaultWarnFunc(_ErrBuf);
  abort();
  _exit(1);
}

void
Panic(char *fmt,...)
{
  char r[MAXERRSIZE];
  char _ErrBuf[2 * MAXERRSIZE];
  int errnum = errno;
  va_list ap;

  fmt = fixit(errnum, fmt, r);
  va_start(ap, fmt);
  (void) vsprintf(_ErrBuf, fmt, ap);
  va_end(ap);
  if (_WarnFunc != (VPF) NULL)
    (_WarnFunc) (_ErrBuf);
  else
    _DefaultWarnFunc(_ErrBuf);
  exit(1);
}

void
Exit(int exitcode, char *fmt,...)
{
  char r[MAXERRSIZE];
  char _ErrBuf[2 * MAXERRSIZE];
  int errnum = errno;
  va_list ap;

  fmt = fixit(errnum, fmt, r);
  va_start(ap, fmt);
  (void) vsprintf(_ErrBuf, fmt, ap);
  va_end(ap);
  if (_WarnFunc != (VPF) NULL)
    (_WarnFunc) (_ErrBuf);
  else
    _DefaultWarnFunc(_ErrBuf);
  exit(exitcode);
}

void
SetWarn(char *file)
{
  static char fnc[] = "SetWarn";
  FILE *fp;

  if ((fp = fopen(file, "w")) == (FILE *) NULL)
    Warn("%s fopen(%s): %m\n", fnc, file);
  else
    _Warnfp = fp;
  (void) setbuf(fp, (char *) NULL);
  return;
}

void
SetWarnFP(FILE * fp)
{
  _Warnfp = fp;
  return;
}

void
SetWarnFunc(VPF f)
{
  _WarnFunc = f;
  return;
}
