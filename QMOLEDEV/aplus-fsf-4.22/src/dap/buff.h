#ifndef included_dap_buff_h
#define included_dap_buff_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/development.h>


/* external macro declarations */
#define BUFFAT(np)		((struct buff *)((np)->d))

/* external struct, union, typedef and enum declarations */
struct buff
{
	int inc; /* This member is going away */
	int ref;
	char *min;
	char *get;
	char *put;
	char *max;
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct 	buff *buffalloc(void);
  extern struct 	buff *buffalloc_r(void);
  extern void		buffclear(struct buff *p);
  extern struct buff *	bufffrag(struct buff *p, int frag);
  extern void		bufffree(struct buff *p);
  extern int		buffgetc(struct buff *p);
  extern void		buffgprintf(struct buff *p, int size, char *fmt, ...);
  extern void		buffputc(struct buff *p, char c);
  extern int		buffputc_r(struct buff *p, char c);
  extern void		buffputdouble(struct buff *p, double d);
  extern void		buffputint(struct buff *p, int d);
  extern void		buffputlong(struct buff *p, long d);
  extern void		buffputs(struct buff *p, char *s);
  extern void		buffputshort(struct buff *p, short d);
  extern int		buffread(struct buff *p, int fd, int len);
  extern struct buff *	buffref(struct buff *p);
  extern void		buffroom(struct buff *p, int room);
  extern int		buffroom_r(struct buff *p, int room);
  extern void		buffstuff(struct buff *p, char *value, int size);
  extern int		buffstuff_r(struct buff *p, char *value, int size);
  extern void		bufftrim(struct buff *p);
  extern int		buffwrite(struct buff *p, int fd, int len);
  extern void		buffzero(struct buff *p, int size);
# ifdef __cplusplus
}
# endif
#else
  extern struct 	buff *buffalloc();
  extern struct 	buff *buffalloc_r();
  extern void		buffclear();
  extern struct buff *	bufffrag();
  extern void		bufffree();
  extern int		buffgetc();
  extern void		buffgprintf();
  extern void		buffputc();
  extern int		buffputc_r();
  extern void		buffputdouble();
  extern void		buffputint();
  extern void		buffputlong();
  extern void		buffputs();
  extern void		buffputshort();
  extern int		buffread();
  extern struct buff *	buffref();
  extern void		buffroom();
  extern int		buffroom_r();
  extern void		buffstuff();
  extern int		buffstuff_r();
  extern void		bufftrim();
  extern int		buffwrite();
  extern void		buffzero();
#endif

#endif








