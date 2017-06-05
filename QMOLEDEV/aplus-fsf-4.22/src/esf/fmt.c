/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/development.h>
#include <stdio.h>
#if !defined(__cfront)
#include <stdarg.h>
#endif
#include <ctype.h>
#include <math.h>
#include <string.h>
#if defined(__NetBSD__) || defined(__FreeBSD) || defined (__APPLE__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include <a/ik.h>
#include <a/x.h>
#include <a/fir.h>

#define DATAINC 100

/* Token types */
#define T_EOF 0
#define T_NUM 1
#define T_STRING 2
#define T_EDIT 3
#define T_MODIFIER 4
#define T_PERIOD 5
#define T_COMMA 6
#define T_LPAREN 7
#define T_RPAREN 8

#define TOKENTYPE(t) ( ((T_EOF      == t) ? "End of file" : \
		        (T_NUM      == t) ? "Number" :      \
		        (T_STRING   == t) ? "String" :      \
		        (T_EDIT     == t) ? "Edit" :        \
		        (T_MODIFIER == t) ? "Modifier" :    \
		        (T_PERIOD   == t) ? "Period" :      \
		        (T_COMMA    == t) ? "Comma" :       \
		        (T_LPAREN   == t) ? "Left paren" :  \
		        (T_RPAREN   == t) ? "Right Paren" : "unknown" ))

#define RAWBUFSIZE    1024
#define COOKEDBUFSIZE  128
#define FINALBUFSIZE   256
#define ERRBUFSIZE      80
#define EXPSIZE         30

static char *EndOfObj;
#define TooBig(x) ( (x) > EndOfObj ) ? 1 : 0

static char errmsg_fmt[ERRBUFSIZE + 1];
static char raw[RAWBUFSIZE + 1];

static struct token {
  int type;	/* Type of token */
  int ivalue;	/* Integer token value */
  char *svalue;	/* String token value */
  unsigned char *orig;		/* pointer to the original string */
  unsigned char *current;      	/* pointer to the current position */
  unsigned char *last;		/* end of the last token parsed */

} tok;

struct format {
  int type;		/* -1=undefined, 0=loop, I, F, E, A, X, T, ... */
  int rcount;		/* repeat count if loop */
  int width;
  int precision;
  int scale;		/* If != 0, scale number by 10 ** scale */
  double fscale;	/* If scale number by this (fscale=10**scale) */
  char blank_if_zero;	/* Blank field if value is 0 */
  char zerofill;	/* Fill in leading 0s */
  char comma;		/* Insert commas in data */
  char leftj;		/* Left justify field */
  char *text;		/* String for Text format */
  char *gpattern;	/* Pattern for G format */
  char *nldecor;	/* Negative left decoration */
  char *nrdecor;	/* Negative right decoration */
  char *odecor;		/* Format 0s as text  :NOTEIT. not documented */
  char *pldecor;	/* Negative left decoration */
  char *prdecor;	/* Positive right decoration */
  char *backfill;	/* Background fill string */
  char *symbpairs;	/* Standard symbol substitution pairs */
  char *symgpairs;	/* "g" format symbol substitution pairs */
  struct format *loop;	/* Sub loop */
  struct format *next;	/* Next node in this loop */
};

struct column {
  int type;	/* Data type of column */
  int rows;	/* Number of rows in column */
  int cols;	/* Distance between elements in column (# of cols in matrix) */
  I *p;		/* Data for the column */
};

static char empty[] = "\0";
static struct column *dataList;
static int dataAllocated;


/* dbg_tfmt controls the behavior of FWarn() calls.  Its values are:
    0 = no Warn messages
    1 = Additional messages to indicate position in format string
*/

extern I dbg_tfmt;

void FWarn(int scb, C *fmt, ...)
 {
  if (dbg_tfmt)
   {
    va_list ap;

    va_start(ap, fmt);
    Warn("\343 _fmt: ", ap);
    Warn(fmt, ap);
    if ( scb == 1 )
     {
      /* print buffer information */
      Warn("\343 _fmt: %s\n",tok.orig);
      Warn("\343 _fmt: %*s%*s\n",
	   (tok.last-tok.orig)+1,"^",
	   (tok.current-tok.last)-1,"^");
     }
    va_end(ap);
   }
 }

/*
 * Allocate and initialize a format structure
 */
static struct format *formalloc()
{
  return (struct format *)calloc(1, sizeof(struct format));
}

/*
 * Free space allocated for the formats
 */
static void freeforms(fmtlist)
  struct format *fmtlist;
{
  struct format *p, *next;
  
  for (p = fmtlist; p != NULL; p = next) {
    if (p->type == 0) {
      /* This is a loop */
      freeforms(p->loop);
    } else {
      if (p->text)      (void)free((char *)p->text);
      if (p->gpattern)  (void)free((char *)p->gpattern);
      if (p->nldecor)   (void)free((char *)p->nldecor);
      if (p->nrdecor)   (void)free((char *)p->nrdecor);
      if (p->pldecor)   (void)free((char *)p->pldecor);
      if (p->prdecor)   (void)free((char *)p->prdecor);
      if (p->odecor)    (void)free((char *)p->odecor);
      if (p->backfill)  (void)free((char *)p->backfill);
      if (p->symbpairs) (void)free((char *)p->symbpairs);
      if (p->symgpairs) (void)free((char *)p->symgpairs);
    }
    next = p->next;
    (void)free((char *)p);
  }
}

/*
 * Reverse a string in place 
 */
static void reverse(s)
  char *s;
{
  int c, i, j;
  for ( i=0, j=strlen((DEV_STRARG)s)-1; i<j; i++, j--) {
    c = s[i];
    s[i] = s[j];
    s[j] = c;
  }
}

/*
 * Make a list of all simple arrays' columns in the data (depth first)
 */
static int datafind(indx, maxrow, d)
  int *indx;
  int *maxrow;
  A d;
{
  int i, rc = 0;
  register int rows;
  register int cols;
  register I *p = d->p;
  register F *dp = (F *)d->p;
  register C *cp = (C *)d->p;
  
  if (d->t == Et && !QS(d->p[0])) {
    /* Array is enclosed and not symbols so descend and process subarrays */
    for (i = 0; i < d->n; i++) {
      if (QA(d->p[i])) {
	rc = datafind(indx, maxrow, ((A*)d->p)[i]);
      }
    }
  } else {
    switch (d->r) {
    case 0: rows = 1; cols = 1; break;
    case 1: rows = d->n; cols = 1; break;
    case 2: rows = d->d[0]; cols = d->d[1]; break;
    default:
      for (rows = 1, i = 0; i < d->r - 1; i++) {
	rows *= d->d[i];
      }
      cols = d->d[d->r - 1];
      break;
    }
    for (i = 0; i < cols; i++) {
      if (dataAllocated <= *indx) {
	dataAllocated += DATAINC;
	dataList = (struct column *)realloc(dataList,
					    sizeof(*dataList) * dataAllocated);
      }
      dataList[*indx].rows = rows;
      dataList[*indx].cols = cols;
      switch (dataList[*indx].type = d->t) {
      case Ct: dataList[(*indx)++].p = (I*)(cp++); break;
      case Ft: dataList[(*indx)++].p = (I*)(dp++); break;
      case Et:		                                	/* symbol */
      case It: dataList[(*indx)++].p = p++; break;
      default: rc = ERR_TYPE; break;
      }
    }
    *maxrow = MAX(*maxrow, rows);
  }
  return rc;
}

/*
 * Retrieve tokens from a format string
 */
static int gettoken(f)
  unsigned char *f;
{
  static unsigned char *p=NULL;         	/* Empty string */
  static unsigned char *start=NULL;	/* Use for text fields */
  unsigned char uc;
  int i, neg = 0;
  register int num;
  static unsigned char stringstart[] = "<\244\332\241\314\354/";
  static unsigned char stringend[] = ">\246\330\241\314\354/";
  static unsigned char edits[] = "AEFGITXaefgitx";
  static unsigned char modifiers[] = "BCKLMNOPQRSZbcklmnopqrsz";
  
  /* The first call to gettoken is with f pointing to the string to tokenize.
   * Subsequent calls should be with a NULL pointer so that the tokenizing
   * process may continue from where it left off
   */
  if ( f ) tok.orig = tok.current = tok.last = p = f; /* Initial call */
  if ( p == NULL ) { 
    FWarn(0, "Format string missing\n"); 
    return ERR_DOMAIN; 
  }
  
  while (ISspace(*p)) p++;	/* Skip leading white space */
  if ( !*p ) { tok.type = T_EOF; tok.current = p; return 0; } /* At End */
  
  /* Is next token a number */
  if (ISdigit(*p) || *p == '-' || *p == (unsigned char)'\242') {
    if (*p == '-' || *p == (unsigned char)'\242') {
      neg = 1;
      p++;
      if (!ISdigit(*p)) {	/* need a digit after the sign */
	tok.current = p;
	FWarn(1,"Bad number specification\n");
	return ERR_DOMAIN; 
      }
    }
    for (num = *p++ - '0'; ISdigit(*p); p++) {num *= 10; num += *p - '0';}
    tok.type = T_NUM;
    tok.ivalue = neg ? -num : num;
    tok.current = p;
    return 0;
  }
  
  /*
   * Check for text string - Delimiter pairs are:
   *	< >
   *	¤ ¦     \244 and \245
   *	Ú Ø	\332 and \330
   *	¡ ¡	\241
   *	Ì Ì	\314
   *	ì ì	\354
   *	/ /
   */
  for ( i=0; i<strlen((DEV_STRARG)stringstart); i++) { 
    /* Get text strings w/o delimmiters */
    if ( stringstart[i] == *p ) {
      start = ++p;		/* beginning of text */
      for(; *p && stringend[i]!=*p; p++); /* Scan for matching end character */
      if ( !*p ){
	tok.current = p;
        FWarn(1, "Missing text ending character '%c'\n",stringend[i]);
	return ERR_DOMAIN; 
      }
      tok.type = T_STRING;
      uc = *p; 
      *p = '\0'; 
      tok.svalue = strdup((char *)start); 
      *p = uc;
      p++;
      tok.current = p;
      return 0;
    }
  }
  
  if (strchr((DEV_STRARG)edits, (int)*p)) {
    tok.type = T_EDIT;
    tok.ivalue = (int)*p++;
    if (ISlower(tok.ivalue)) tok.ivalue = toupper(tok.ivalue);
    tok.current = p;
    return 0;
  }
  if (strchr((DEV_STRARG)modifiers, (int)*p)) {
    tok.type = T_MODIFIER;
    tok.ivalue = (int)*p++;
    if (ISlower(tok.ivalue)) tok.ivalue = toupper(tok.ivalue);
    tok.current = p;
    return 0;
  }
  
  switch ( *p ) {
    case '.': tok.type = T_PERIOD; p++; tok.current = p; return 0;
    case ',': tok.type = T_COMMA;  p++; tok.current = p; return 0;
    case '(': tok.type = T_LPAREN; p++; tok.current = p; return 0;
    case ')': tok.type = T_RPAREN; p++; tok.current = p; return 0;
  default:
    tok.current = p+1;
    FWarn(1,"Garbage character, '%c', in format string\n",(int)*p);
    return ERR_DOMAIN; 
  }
}

/*
 * Parse modfier expressions
 */
static int parsemodifiers(f)
  struct format *f;
{
  int c, rc = 0;
  
  while (tok.type == T_MODIFIER) {
    switch (c = tok.ivalue) {
    case 'B': f->blank_if_zero = 1; break;
    case 'C': f->comma = 1; break;
    case 'K':
      if ( (rc = gettoken((unsigned char *)0)) || tok.type != T_NUM) {
	if (rc) return rc;
	FWarn(1,"K modifier requires a scale factor\n");
	return ERR_DOMAIN;
      }
      f->scale = tok.ivalue;
      f->fscale = pow(10.0, (double)f->scale);
      break;
    case 'L': f->leftj = 1; break;
    case 'Z': f->zerofill = 1; break;
    default:
      if (( rc = gettoken((unsigned char *)0)) || tok.type != T_STRING ) {
	if (rc) return rc;
	FWarn(1,"%c modifier requires text string\n",c);
	return ERR_DOMAIN;
      }
      if ( strlen((DEV_STRARG)tok.svalue)==0 && (c=='R'||c=='S') ) {
	FWarn(1,"%c modifier a text length greater than zero\n",c);
	return ERR_DOMAIN;
      }
      switch (c) {
      case 'M': f->nldecor = tok.svalue; break;
      case 'N': f->nrdecor = tok.svalue; break;
      case 'O': f->odecor = tok.svalue; break;
      case 'P': f->pldecor = tok.svalue; break;
      case 'Q': f->prdecor = tok.svalue; break;
      case 'R': f->backfill = tok.svalue; break;
      case 'S':
	f->symbpairs = tok.svalue;
	if (strlen((DEV_STRARG)tok.svalue) & 1) {
	  FWarn(1,"S<...> must have even number of symbols\n");
	  return ERR_DOMAIN;
	}
	break;
      }
      break;
    }
    if (rc = gettoken((unsigned char *)0)) return rc;
  }
  return 0;
}

/*
 * Parse edit specifiers
 */
static int parseedit(f)
  struct format *f;
{
  int rc = 0, i;
  /* Set type, width, and precision fields as appropriate for edit type */
  switch (f->type = tok.ivalue) {
  case 'A':  case 'I':  case 'T': /* char: int: absolute tab: */
    if ( rc = gettoken((unsigned char *)0) ) return rc;
    if (tok.type != T_NUM || tok.ivalue <= 0) {
      FWarn(1,"A number greater than zero must follow format %c\n", f->type);
      return ERR_DOMAIN;
    }
    f->width = tok.ivalue;
    break;
  case 'X':			/* Relative tab negative value is OK */
    if ( rc = gettoken((unsigned char *)0) ) return rc;
    if (tok.type != T_NUM) {
      FWarn(1,"A number must follow format %c\n", f->type);
      return ERR_DOMAIN;
    }
    f->width = tok.ivalue;
    break;
  case 'E':  case 'F':		/* exponential: fixed point: */
    if ( rc = gettoken((unsigned char *)0) ) return rc;
    
    if ( tok.type != T_NUM || tok.ivalue <= 0) {
      FWarn(1,"A number greater than 0 must follow format %c\n", f->type);
      return ERR_DOMAIN;
    }
    
    f->width = tok.ivalue;
    if ( rc = gettoken((unsigned char *)0) ) return rc;
    if ( tok.type != T_PERIOD) {
      FWarn(1,"%c format is missing a '.'\n", f->type);
      return ERR_DOMAIN;
    } 
    
    if ( rc = gettoken((unsigned char *)0) ) return rc;

    if ( f->type == 'F' && (tok.type != T_NUM || tok.ivalue < 0)) {
      FWarn(1, "%c format requires non-negative precision\n", f->type); 
      return ERR_DOMAIN;
    }

    if ( f->type == 'E' && (tok.type != T_NUM || tok.ivalue <= 0)) {
      FWarn(1, "%c format significant digits greater than 0\n", f->type); 
      return ERR_DOMAIN;
    }

    if ( f->type != 'E' && tok.ivalue == 0) {
      FWarn(1,"Warning format %c has zero significant digits\n", f->type);
    }
    
    f->precision = tok.ivalue;

    if ( f->precision >= f->width ) {
      FWarn(1,"Field width must be greater than %s\n",
	      (f->type == 'F') ? "precision" : "significant digits");
      return ERR_DOMAIN;
    } 

    break;
  case 'G':			/* picture format */
    if ( rc = gettoken((unsigned char *)0) ) return rc;
    if ( tok.type != T_STRING ) {
      FWarn(1,"%c format requires a pattern\n", f->type);
      return ERR_DOMAIN;
    }
    f->gpattern = tok.svalue;
    f->width = strlen((DEV_STRARG)tok.svalue);
    if (f->symbpairs) {		/* separate out any formating characters */
      f->symgpairs = strdup(f->symbpairs);
      for ( i=0; i < strlen((DEV_STRARG)f->symbpairs); i +=2) {
	if ( (f->symbpairs[i] == '9') || (toupper(f->symbpairs[i]) == 'Z')) {
	  f->symbpairs[i] = f->symbpairs[i+1] = ' '; /* remove from std */
	  f->symgpairs[i] = toupper(f->symgpairs[i]); /* Make sure contol UC */
	} else {
	  f->symgpairs[i] = f->symgpairs[i+1] = ' ';
	}
      }
    }
    /* a few warnings messages */
    if ( f->blank_if_zero ) 
      FWarn(1,"'b' qualifier ignored with G format\n");

    if ( f->comma ) 
      FWarn(1,"'c' qualifier ignored with G format\n");

    if ( f->zerofill ) 
      FWarn(1,"'z' qualifier ignored with G format\n");

    if ( f->leftj ) 
      FWarn(1,"'l' qualifier ignored with G format\n");

    if ( f->nrdecor || f->prdecor ) 
      FWarn(1,"Right decorators ignored with G format\n");

    if ( f->nldecor && ( strlen((DEV_STRARG)f->nldecor) > 1) )
	FWarn(1, "Negative/Left %s chars ignored\n", &(f->nldecor[1]) );

    if ( f->pldecor && ( strlen((DEV_STRARG)f->pldecor) > 1) )
	FWarn(1, "Positive/Left %s chars ignored\n", &(f->pldecor[1]) );
    break;
  }
  return gettoken((unsigned char *)0);
}

/*
 * Parse the format string
 */
static int parseform( fmtlist )
  struct format **fmtlist;
{
  int state = 0;
  int repeat=0;
  int rc = 0;
  struct format *head, *frm, *savf = NULL;
  
  if ((frm = head = formalloc()) == NULL) {
    fmtlist = NULL;
    return ERR_DOMAIN;
  }
  for (;;) {
    switch (state) {
    case 0: /* Start state */
      switch (tok.type) {
      case T_NUM:
	if ( 1 > (repeat = tok.ivalue) ) {
	  FWarn(1,"Repetition factor should be greater than 0\n");
	  state = rc = -1;
	  break;
	}
	state = (rc = gettoken((unsigned char *)0)) ? -1 : 1;
	break;
      case T_LPAREN:
	repeat = 1;
	state = (rc = gettoken((unsigned char *)0)) ? -1 : 2;
	break;
      case T_MODIFIER:
	state = 3;
	break;
      case T_EDIT:
	state = 4;
	break;
      case T_STRING:
	state = 5;
	break;
      default:
	FWarn(1,"Format string parse error\n");
	state = rc = -1;
	break;
      }
      break;
    case 1: /* A repeat count has been encountered */
      switch (tok.type) {
      case T_LPAREN:
	state = (rc = gettoken((unsigned char *)0)) ? -1 : 2;
	break;
      case T_MODIFIER:
	frm->type = 0;	/* Loop */
	frm->rcount = repeat;
	savf = frm;
	state = ((savf->loop = frm = formalloc()) == NULL) ? -1 : 3;
	break;
      case T_EDIT:
	frm->type = 0;	/* Loop */
	frm->rcount = repeat;
	savf = frm;
	state = ((savf->loop = frm = formalloc()) == NULL) ? -1 : 4;
	break;
      case T_STRING:
	frm->type = 0;	/* Loop */
	frm->rcount = repeat;
	savf = frm;
	state = ((savf->loop = frm = formalloc()) == NULL) ? -1 : 5;
	break;
      default:
	FWarn(1,"Format string parse error\n");
	state = rc = -1;
	break;
      }
      break;
    case 2:
      /*
       * Opening parentheses have been encountered.  Recursively
       * call parseform() to parse the expression within the parens.
       */
      frm->type = 0;	/* Loop */
      frm->rcount = repeat;
      if ( rc = parseform( &(frm->loop) )  ) {
	state = -1;
	break;
      }
      if (tok.type != T_RPAREN) {
	FWarn(1,"Missing right paren ')'\n");
	state = rc = -1;
	break;
      }
      state = (rc = gettoken((unsigned char *)0)) ? -1 : 6;
      break;
    case 3: /* A modifier has been seen */
      if ( rc = parsemodifiers(frm) ) {
	state = -1;
      } else if (tok.type == T_EDIT) {
	state = 4;
      } else if (tok.type == T_STRING) {
	state = 5;
      } else {
	FWarn(1,"Missing edit specification\n");
	state = rc = -1;
      }
      break;
    case 4: /* An edit specifier has been seen */
      if (rc = parseedit(frm)) {
	state = rc = -1;
      } else {
	if ( tok.type==T_COMMA || tok.type==T_EOF || tok.type==T_RPAREN ) {
	  state = 6;
	} else {
	  FWarn(1,"Format phrases must be delimited\n");
	  state = rc = -1;
	}
      }
      break;
    case 5: /* A string has been seen */
      frm->type = '<';	/* String */
      frm->text = tok.svalue;
      frm->width = strlen((DEV_STRARG)tok.svalue);
      if ( (rc = gettoken((unsigned char *)0)) ) {
	state = -1;
      } else {
	if ( tok.type==T_COMMA || tok.type==T_EOF || tok.type==T_RPAREN ) {
	  state = 6;
	} else {
	  FWarn(1,"<text> must be delimited\n");
	  state = rc = -1;
	}
      }
      break;
    case 6:
      if (tok.type == T_COMMA) {
	if (rc = gettoken((unsigned char *)0)) {
	  state = -1;
	  break;
	}
	if (savf) {
	  frm = savf;
	  savf = NULL;
	}
	frm = frm->next = formalloc();
	state = 0;
	tok.last = tok.current;	/* progress pointers into format phrase */
      } else {
	*fmtlist = head;	/* Return from recursive call */
	return 0;
      }
      break;
    default:
      freeforms(head);
      if (rc) {
	return ERR_DOMAIN;
      } else {
	FWarn(1,"Format string parse error\n");
	return ERR_DOMAIN;
      }
    }
  }
}

/*
 * Traverse format list to determine size of output
 */
static void trav1(f, dcols, pcols, ncol)
  struct format *f;	/* Next node of format tree to process */
  int *dcols;		/* Total number data columns left to format */
  int *pcols;		/* Number of print columns required */
  int *ncol;		/* Next column where printing will take place */
{
  int i;
  
  
  do {
    switch (f->type) {
    case 0:	/* LOOP */
      for (i = 0; i < f->rcount; i++) {
	if (!*dcols) return;
	trav1(f->loop, dcols, pcols, ncol);
      }
      break;
    case '<':	/* String */
      *ncol += f->width;
      *pcols = MAX(*pcols, *ncol);
      break;
    case 'X':
      if ( (f->width+*ncol) < 0) {
	(void) FWarn(0,"Rel tab modified (%d->%d) to keep position >= zero\n",
		       f->width, (f->width=-*ncol));
      }
      *ncol += f->width;
      *pcols = MAX(*pcols, *ncol);
      break;
    case 'T':
      *ncol = f->width + 1;	/* :NOTEIT. next field is p+1 see trav2 */
      *pcols = MAX(*pcols, *ncol);
      break;
    default:
      if (!*dcols) return;
      *ncol += f->width;
      *pcols = MAX(*pcols, *ncol);
      (*dcols)--;
      break;
    }
  } while (f = f->next);
}

/*
 * Determine number of printed columns in output
 */
static int printcols(fmtlist, dcols, pcols)
  struct format *fmtlist;
  int dcols;
  int *pcols;
{
  int ncol = 0, rc=0;
  int save_dcols;
  *pcols = 0;
  /* to repeat on dcols where: (# of format phrases) < dcols  */
  while (dcols) {
    save_dcols=dcols;
    trav1(fmtlist, &dcols, pcols, &ncol); /* note pcols is a (int *) */
    if ( dcols == save_dcols ) {
      FWarn(0,"Missing format phrases for data\n");
      return ERR_DOMAIN;
    }
  }
  return rc;
}

/*
 * Put literal text into the output
 */
static int textformat(cp, f, rows, cols)
  char *cp;
  struct format *f;
  int rows, cols;
{
  int i;
  int rc = 0;

  if ( TooBig(cp + f->width + cols*(rows-1)) ) return ERR_DOMAIN;

  for (i = 0; i < rows; i++) {
    (void)bcopy(f->text, cp, f->width);
    cp += cols;
  }
  return rc;
}

/*
 * Fill an output cell with a repeated background pattern
 */
static int backfill(cp, f)
  char *cp;
  struct format *f;
{
  int j, k;
  int rc = 0;
  if (f->backfill) {
    if ( TooBig(cp + f->width ) ) return ERR_DOMAIN;
    for (j = k = 0; j < f->width; j++) {
      if ( cp[j] == ' ' ) cp[j] = f->backfill[k++];
      if (!f->backfill[k]) k = 0; /* Backfill string short */
    }
  }
  return rc;
}

/*
 * Put commas into a numeric string
 */
static void docomma(o,i,w,l)
  char *o;			/* cooked buffer */
  char *i;			/* raw buffer    */
  int   w;			/* width         */
  int   l;			/* leading comma */
{
  int k,j;
  char *omax=o+COOKEDBUFSIZE;
  
  if (l&&!(w%3)) *o++=',';	
  for(k=0,j=(w-1)%3+1;k<w&&o<omax;k++,j--) {
    if(j==0) {
      *o++=',';j=3;
    }
    *o++=i[k];
  }
  while(i[k]&&o<omax) *o++=i[k++];
  *o='\0';
}

/*
 * Substitue specific characters in a string
 */
static void symbpairs(cp, sp)
  char *cp, *sp;
{
  /* Following line dedicated to atw */
  C *p;if(sp)for(;*cp;cp++)for(p=sp;*p;p+=2)if(*p==*cp){*cp=p[1];break;}
}

/*
 * Set Error Character in the field
 */

static void errorSet(cp, default_char, f)
  char *cp, *default_char;
  struct format *f;
{
  char err_char[2];
  *err_char = *default_char;
  symbpairs(err_char, f->symbpairs); /* change default error symbol */
  (void)memset(cp, *err_char, f->width);
  return;
}

static int aformat(cp, f, d, pcols)
  char *cp;
  struct format *f;
  struct column *d;
  int pcols;
{
  int i, len;
  char *pp;
  int rc = 0;

  if ( TooBig( cp + f->width + pcols * (d->rows-1) ) ) return ERR_DOMAIN;

  switch (d->type) {
  case It:
    for (i = 0; i < d->rows; i++, d->p += d->cols, cp += pcols) {
      if (rc = backfill(cp, f)) return rc;
      if (f->leftj) *cp = (char)*d->p;
      else cp[f->width-1] = (char)*d->p;
    }
    break;
  case Ct:
    for (pp=(char *)d->p,i = 0;i < d->rows; i++, pp += d->cols, cp += pcols) {
      if (rc = backfill(cp, f)) return rc;
      if (f->leftj) *cp = *pp;
      else cp[f->width-1] = *pp;
    }
    break;
  case Et:
    for (i = 0; i < d->rows; i++, d->p += d->cols, cp += pcols) {
      if (!QS(*d->p) || 
	  (len = strlen((DEV_STRARG)(pp = XS(*d->p)->n))) > f->width) {
	errorSet(cp, "*", f);
	continue;
      }
      if (rc = backfill(cp, f)) return rc;
      if (f->leftj) (void)bcopy(pp, cp, len);
      else (void)bcopy(pp,cp+f->width-len,len);
    }
    break;
  default:
    for (i = 0;i < d->rows; i++, cp += pcols) {
      errorSet(cp, "*", f);
    }
    break;
  }
  return rc;
}

#ifdef FUNCNOTUSED
static int roundnearest(d)
  double d;
{
  return (d < 0.0) ? (int)(d - .5) : (int)(d + .5);
}
#endif

static int iformat(cp, f, d, pcols)
  char *cp;
  struct format *f;
  struct column *d;
  int pcols;
{
  int i, width, leading_comma=0;
  double val;
  char cooked[COOKEDBUFSIZE + 1], final[FINALBUFSIZE + 1], *p;
  int rc = 0;

  if ( TooBig( cp + f->width + pcols * (d->rows-1) ) ) return ERR_DOMAIN;

  for (i = 0; i < d->rows; i++, cp += pcols) {
    char *prefix = empty, *suffix = empty;
    
    if (rc = backfill(cp, f)) return rc;
    switch (d->type) {		/* Only It and Ft var valid data types */
    case It: val = (double)*d->p; d->p += d->cols; break;
    case Ft: val = *(F*)d->p; d->p = (I*)((F*)d->p + d->cols); break;
    default: errorSet(cp, "?", f); d->p += d->cols; continue;
    }
    if (val == 0.0) {
      if (f->blank_if_zero) continue;
      val = 0.0;	/* Take care of problems like -0.0 */
    }
    if (f->scale) val *= f->fscale; /* scale value if needed */
    if (val < 0.0) {		    /* handle decorators for negatives */
      if (f->nldecor) prefix = f->nldecor; 
      else prefix = APL ? "\242": "-";
      if (f->nrdecor) suffix = f->nrdecor;
      val = -val;
    } else if (val >= 0.0) {
      if (f->pldecor) prefix = f->pldecor;
      if (f->prdecor) suffix = f->prdecor;
    }
    /* Compute width available for integer part */
    width = f->width - strlen((DEV_STRARG)prefix) - strlen((DEV_STRARG)suffix);
    if (f->comma) {
      leading_comma = f->zerofill && !(f->leftj || (width % 4));
      width -= width / 4;
    }
    if (width > 0) {
      if (f->zerofill && !f->leftj) {
	sprintf(raw, "%0*.0f", width, val);
      } else {
	sprintf(raw, "%.0f", val);
      }
    } else {
      errorSet(cp, "*", f);
      continue;
    }
    
    {				/* reckeck for zero evaluation up to '.' */
      int i;
      for (i=0; raw[i] && raw[i] != '.' ; i++) 
	if ( ISdigit(raw[i]) && raw[i] != '0' ) break;
      if ( (!raw[i]) || (raw[i] == '.') ) { 
	/* If got to end must all be zeros */
	if (f->blank_if_zero) continue;
	val = 0.0;	/* Take care of problems like -0.0 */
      }
    }
    

    if (f->comma) {
      width = (p = (char *)strchr((DEV_STRARG)raw, '.')) ? p - raw : 
	strlen((DEV_STRARG)raw);
      docomma(cooked, raw, width, leading_comma);
    } else {
      (void)strncpy(cooked, raw, COOKEDBUFSIZE);
    }
    if (val == 0.0 && f->odecor) (void)strncpy(final, f->odecor, FINALBUFSIZE);
    else sprintf(final, "%s%s%s", prefix, cooked, suffix);
    symbpairs(final, f->symbpairs);
    width = strlen((DEV_STRARG)final);
    if (width > f->width) errorSet(cp, "*", f);
    else if (f->leftj) strncpy(cp, final, width);
    else strncpy(cp+f->width-width, final, width);
  }
  return rc;
}

static int eformat(cp, f, d, pcols)
  char *cp;
  struct format *f;
  struct column *d;
  int pcols;
{
  int i, width, decpt, sign;
  I *tmp_p;
  double val;
  char cooked[COOKEDBUFSIZE + 1], expnt[EXPSIZE+1], final[FINALBUFSIZE + 1];
  int rc = 0;
  int largest=1;
  char expFmtStr[10];
  int InfOrNaN=0;

  if ( TooBig( cp + f->width + pcols * (d->rows-1) ) ) return ERR_DOMAIN;
  
  if ( f->leftj ) {
    sprintf(expFmtStr,"E%%d\0");
  } else { /* scan data to get largest exponent */
    tmp_p = d->p;
    for ( i=0; i < d->rows; i++ ) { 
      switch ( d->type ) {
      case It: val = (double)*tmp_p; tmp_p += d->cols; break;
      case Ft: val = *(F*)tmp_p; tmp_p = (I*)((F*)tmp_p + d->cols); break;
      default: tmp_p += d->cols; continue;  
      }
#if defined(_AIX) || defined(__sgi) || defined(linux) | defined(__FreeBSD__) || defined(__NetBSD__) || (__APPLE__)
      ecvt(val, f->precision, &decpt, &sign);
#else
      econvert(val, f->precision, &decpt, &sign, raw);
#endif
      sprintf(expnt, "%d", --decpt);
      largest = MAX(largest, strlen((DEV_STRARG)expnt) );
    }
    sprintf(expFmtStr,"E%%%dd\0", largest );
  }

  for (i = 0; i < d->rows; i++, cp += pcols) {
    char *prefix = empty, *suffix = empty;
    
    if (rc = backfill(cp, f)) return rc;
    switch (d->type) {
    case It: val = (double)*d->p; d->p += d->cols; break;
    case Ft: val = *(F*)d->p; d->p = (I*)((F*)d->p + d->cols); break;
    default: errorSet(cp, "?", f); d->p += d->cols; continue;
    }
    if (val == 0.0) {
      if (f->blank_if_zero) continue;
      val = 0.0;	/* Take care of problems like -0.0 */
    }
    if (f->scale) val *= f->fscale;
    if (val < 0.0) {
      if (f->nldecor) prefix = f->nldecor;
      else prefix = APL ? "\242": "-";
      if (f->nrdecor) suffix = f->nrdecor;
      val = -val;
    } else if (val >= 0.0) {
      if (f->pldecor) prefix = f->pldecor;
      if (f->prdecor) suffix = f->prdecor;
    }
    width = f->width - strlen((DEV_STRARG)prefix) - strlen((DEV_STRARG)suffix);
#if defined(_AIX) || defined(__sgi) || defined(linux) | defined(__FreeBSD__) || defined(__NetBSD__) || (__APPLE__)
    if (width > 0) {
      (void)strncpy(raw, (char *)ecvt(val, f->precision, &decpt, &sign), RAWBUFSIZE);
#else
    if (width > 0 && econvert(val, f->precision, &decpt, &sign, raw)) {
#endif
      if ( (!strcmp((DEV_STRARG)raw,(DEV_STRARG)"Inf")) || 
	   (!strcmp((DEV_STRARG)raw,(DEV_STRARG)"INF")) ||
	   (!strcmp((DEV_STRARG)raw,(DEV_STRARG)"NaN")) ) 
	InfOrNaN=1;                    
      else InfOrNaN=0; 

      if(!InfOrNaN){
	if ( '0' == *raw ) {	/* recheck if conversion returned a zero  */
	  if (f->blank_if_zero) continue;
	  val = 0.0;	/* Take care of problems like -0.0 */
	}
	
	sprintf(expnt, expFmtStr, --decpt);
	
	if (decpt < 0 && APL) {	/* Insert negative next to exponent */
	  int i;
	  for (i=1; expnt[i]; i++) 
	    if ( expnt[i] == '-') {expnt[i] = '\242'; break; }
	}
	
	if (f->precision <= 0) (void)strncpy(cooked, expnt, COOKEDBUFSIZE);
	else if (f->precision == 1) sprintf(cooked, "%c%s", raw[0], expnt);
	else {
	  cooked[0]=raw[0]; cooked[1]='.'; cooked[2]='\0';
	  strncat(cooked,raw+1,COOKEDBUFSIZE-2);
	  strncat(cooked,expnt,COOKEDBUFSIZE-strlen((DEV_STRARG)cooked));
	}
      } else { (void)strncpy(cooked, raw, COOKEDBUFSIZE); }
    } else {
      errorSet(cp, "*", f);
      continue;
    }
    if (val == 0.0 && f->odecor) (void)strncpy(final, f->odecor, FINALBUFSIZE);
    else sprintf(final, "%s%s%s", prefix, cooked, suffix);
    if (!InfOrNaN) symbpairs(final, f->symbpairs);
    width = strlen((DEV_STRARG)final);
    if (width > f->width) errorSet(cp, "*", f);
    else if (f->leftj) strncpy(cp, final, width);
    else strncpy(cp+f->width-width, final, width);
  }
  return rc;
}

static int fformat(cp, f, d, pcols)
  char *cp;
  struct format *f;
  struct column *d;
  int pcols;
{
  int i, width, leading_comma=0;
  double val;
  char cooked[COOKEDBUFSIZE + 1], final[FINALBUFSIZE + 1], *p;
  int rc = 0;
  int InfOrNaN=0;

  if ( TooBig( cp + f->width + pcols * (d->rows-1) ) ) return ERR_DOMAIN;

  for (i = 0; i < d->rows; i++, cp += pcols) {
    char *prefix = empty, *suffix = empty;
    if (rc = backfill(cp, f)) return rc;
    switch (d->type) {
    case It: val = (double)*d->p; d->p += d->cols; break;
    case Ft: val = *(F*)d->p; d->p = (I*)((F*)d->p + d->cols); break;
    default: errorSet(cp, "?", f); d->p += d->cols; continue;
    }
    if (val == 0.0) {
      if (f->blank_if_zero) continue;
      val = 0.0;	/* Take care of problems like -0.0 */
    }
    if (f->scale) val *= f->fscale;
    if (val < 0.0) {
      if (f->nldecor) prefix = f->nldecor;
      else prefix = APL ? "\242": "-";
      if (f->nrdecor) suffix = f->nrdecor;
      val = -val;
    } else if (val >= 0.0) {
      if (f->pldecor) prefix = f->pldecor;
      if (f->prdecor) suffix = f->prdecor;
    }
    /* Compute width available for integer part */
    width = f->width - f->precision - 1 - strlen((DEV_STRARG)prefix) - strlen((DEV_STRARG)suffix);
    if (f->comma) {
      leading_comma = f->zerofill && !(f->leftj || (width % 4));
      width -= width / 4;
    }
    width += f->precision + 1;
    if (width > 0) {
      if (f->zerofill && !f->leftj) {
	sprintf(raw, "%#0*.*f", width, f->precision, val);
      } else {
	sprintf(raw, "%#.*f", f->precision, val);
      }
    } else {
      errorSet(cp, "*", f);
      continue;
    }
    
    {				/* reckeck for zero evaluation */
      int i;
      for (i=0; raw[i]; i++) if ( ISdigit(raw[i]) && raw[i] != '0' ) break;
      if ( !raw[i] ) {		/* If got to end must all be zeros */
	if (f->blank_if_zero) continue;
	val = 0.0;	/* Take care of problems like -0.0 */
      }
    }

    if ( (!strcmp((DEV_STRARG)raw,(DEV_STRARG)"Inf")) || 
         (!strcmp((DEV_STRARG)raw,(DEV_STRARG)"INF")) ||
	 (!strcmp((DEV_STRARG)raw,(DEV_STRARG)"NaN"))) 
      InfOrNaN=1; 
    else InfOrNaN=0;                   

    if (f->comma && !InfOrNaN) {
      width = (p = (char *)strchr((DEV_STRARG)raw, '.')) ? p - raw : strlen((DEV_STRARG)raw);
      docomma(cooked, raw, width, leading_comma);
    } else {
      (void)strncpy(cooked, raw, COOKEDBUFSIZE);
    }
    if (val == 0.0 && f->odecor) (void)strncpy(final, f->odecor, FINALBUFSIZE);
    else sprintf(final, "%s%s%s", prefix, cooked, suffix);
    if(!InfOrNaN) symbpairs(final, f->symbpairs);
    width = strlen((DEV_STRARG)final);

    if ( (1 == (width - f->width)) && ('0'==*final) ){ /* shift if only 1 lead 0 */
      int i;
      for(i=1; i<width; i++) final[i-1] = final[i];
      width--;
    }

      
    if (width > f->width) errorSet(cp, "*", f);
    else if (f->leftj) strncpy(cp, final, width);
    else strncpy(cp+f->width-width, final, width);
  }
  return rc;
}

#define DIGIT    0
#define NOZERO   1
#define NONSIG   2
#define NUMSYMS  3

static int gformat(cp, f, d, pcols)
  char *cp;
  struct format *f;
  struct column *d;
  int pcols;
{

  int i, j, width;
  double val;
  char final[FINALBUFSIZE + 1];
  char *num_p = NULL, *fmt_p = NULL;
  int rc = 0;
  char ok, prefix_set = 0, trailing_zero;
  char stdsym[NUMSYMS + 1];

  stdsym[DIGIT]='9'; stdsym[NOZERO]='Z'; stdsym[NONSIG]='_';
  stdsym[NUMSYMS]='\0';

  if ( TooBig( cp + f->width + pcols * (d->rows-1) ) ) return ERR_DOMAIN;
  
  for (i = 0; i < d->rows; i++, cp += pcols) {
    char *prefix;
    prefix = empty;

    
    switch (d->type) {		/* Only It and Ft var valid data types */
    case It: val = (double)*d->p; d->p += d->cols; break;
    case Ft: val = *(F*)d->p; d->p = (I*)((F*)d->p + d->cols); break;
    default: errorSet(cp, "?", f); d->p += d->cols; continue;
    }
    if (val == 0.0) {
      val = 0.0;	/* Take care of problems like -0.0 */
    }
    if (f->scale) val *= f->fscale; /* scale value if needed */
    if (val < 0.0) {		    /* handle decorators for negatives */
      prefix_set = 1;
      if (f->nldecor) prefix = f->nldecor; 
      else prefix = APL ? "\242": "-";
      val = -val;
    } else if (val >= 0.0) {
      if (f->pldecor) { 
	prefix_set = 1; prefix = f->pldecor; 
      } else {
	prefix_set=0;
      }
    }
    /* Compute width available for integer part */
    width = f->width - strlen((DEV_STRARG)prefix);
    if (width > 0) {
      if (f->zerofill && !f->leftj) {
	sprintf(raw, "%0*.0f", width, val);
      } else {
	sprintf(raw, "%.0f", val);
      }
    } else {
      errorSet(cp, "*", f);
      continue;
    }

    symbpairs(stdsym, f->symgpairs); /* allow substitution for controls */
    
    strncpy(final,f->gpattern,FINALBUFSIZE);

    reverse(raw); reverse(final);
    ok = 1; trailing_zero = 1;	/* reset indicator flags */
    for( fmt_p = (char *)final, num_p = (char *)raw; *fmt_p && ok; fmt_p++) {
      if (        *fmt_p  == stdsym[DIGIT] ||  
	  toupper(*fmt_p) == stdsym[DIGIT] ) {
	if ( ! *num_p ) { /* nothing left  */
	  *fmt_p = '0'; 
	  if ( trailing_zero ) trailing_zero = 0;
	  continue; 
	}   
	if ( ! ISdigit(*num_p) ) { ok = 0; continue; } /* error: must be a number */
	if ( trailing_zero ) trailing_zero = 0;
	*fmt_p = *num_p;
	num_p++;
      } else if (        *fmt_p  == stdsym[NOZERO] || 
		 toupper(*fmt_p) == stdsym[NOZERO] ) {

	if ( ! *num_p ) {	/* nothing left */
	  *fmt_p = (prefix_set) ? *prefix : ' '; 
	  if (prefix_set) prefix_set = 0; /* set only one */
	  continue; 
	}    
	if ( ! ISdigit(*num_p) ) { ok = 0; continue; } /* error: must be a number */
        if (trailing_zero) trailing_zero = (*num_p == '0') ? 1 : 0; 
	if (trailing_zero) {
          *fmt_p = ' ';
	  trailing_zero = 1;
        } else {
	  *fmt_p = *num_p;
        }
	num_p++;
      }
    }
    reverse ( final );
    
    if ( ok && !*num_p && !prefix_set ) {
      /* remove decorators not surrounded by digits   */
      /* by moving a 3 element window though pattern  */
      /* g<refund zzz,zz9.99> _fmt 12 123 123456      */
      /*   refund         12                          */
      /*   refund       1.23                          */
      /*   refund   1,234.56                          */
      for ( j=2; j < f->width; j++){
	if ( (!ISdigit(final[j-2])) || (!ISdigit(final[j])) ) {
	   
	  if ( (toupper(f->gpattern[j-2]) == stdsym[DIGIT] || 
		toupper(f->gpattern[j-2]) == stdsym[NOZERO]) && 
	       (toupper(f->gpattern[j])   == stdsym[DIGIT] || 
		toupper(f->gpattern[j])   == stdsym[NOZERO]) &&
	       (toupper(f->gpattern[j-1]) != stdsym[DIGIT] &&
		toupper(f->gpattern[j-1]) != stdsym[NOZERO]) ){
	    final[j-1] = ' ';	/* remove decorator */
	  }

	}
      }
      symbpairs(final, f->symbpairs);
      strncpy(cp, final, f->width);
      if (rc = backfill(cp, f)) return rc;
    } else {
      errorSet(cp, "*", f);
    }
  }
  return rc;
}

static int trav2(f, d, rows, i, dcols, ncol, pcols, buf)
  struct format *f;
  struct column *d;
  int rows, *i, dcols, *ncol, pcols;
  char *buf;
{
  int j;
  static char *cp = NULL;
  int rc = 0;
  
  cp = buf + *ncol;
  do {
    switch (f->type) {
    case 0:	/* LOOP */
      for (j = 0; j < f->rcount; j++) {
	if (*i >= dcols) return rc;
	if (rc = trav2(f->loop, d, rows, i, dcols, ncol, pcols, buf)) return rc;
      }
      break;
    case '<':	/* String */
      if (rc = textformat(cp, f, rows, pcols)) return rc;
      cp += f->width;
      break;
    case 'X':
      cp += f->width;
      break;
    case 'T':
      cp = buf + f->width + 1;
      break;
    case 'A':
      if (*i >= dcols) return rc;
      if (rc = aformat(cp, f, d+*i, pcols)) return rc;
      cp += f->width;
      (*i)++;
      break;
    case 'I':
      if (*i >= dcols) return rc;
      if (rc = iformat(cp, f, d+*i, pcols)) return rc;
      cp += f->width;
      (*i)++;
      break;
    case 'E':
      if (*i >= dcols) return rc;
      if (rc = eformat(cp, f, d+*i, pcols)) return rc;
      cp += f->width;
      (*i)++;
      break;
    case 'F':
      if (*i >= dcols) return rc;
      if (rc = fformat(cp, f, d+*i, pcols)) return rc;
      cp += f->width;
      (*i)++;
      break;
    case 'G':
      if (*i >= dcols) return rc;
      if (rc = gformat(cp, f, d+*i, pcols)) return rc;
      cp += f->width;
      (*i)++;
      break;
    }
    *ncol = cp - buf;
  } while (f = f->next);
  return rc;
}

static int dofmt(fmtlist, rows, dcols, pcols, buf)
  struct format *fmtlist;
  int rows, dcols, pcols;
  char *buf;
{
  int i = 0;
  int ncol = 0;
  int rc = 0;

  while (i < dcols) {
    if (rc=trav2(fmtlist, dataList, rows, &i, dcols, &ncol, pcols, buf)) {
      if ( rc )
	FWarn(0,"Output A+ object allocation error\n"); 
      return ERR_DOMAIN;
    }
  }
  return rc;
}

/*
 * Format arrays - QUAD FMT imitation
 */
ENTRYPOINT
A ep_fmt(f, d)
unsigned char *f;
A d;
{
  A a;
  I dims[2];
  int maxrows = 0;		/* maximum rows */
  int dcols = 0;		/* number of data columns  */
  int pcols;			/* number of print columns */
  int rc = 0;			/* general return code */
  struct format *fmtlist;	/* tree of format phrases */
  
  if ( 2>strlen((DEV_STRARG)f) ) {
    FWarn(0,"Format phase too short\n");
    ERROUT(ERR_DOMAIN);
  }
  NDC1(d);
  /* Make a list of primitive objects (depth first) */
  dataList = (struct column *)malloc(DATAINC * sizeof(*dataList));
  dataAllocated = DATAINC;
  if ( rc = datafind(&dcols, &maxrows, d) ) {
    if (dataList) (void)free((char *)dataList);
    ERROUT(rc);
  }

  /* Parse the format specification */
  fmtlist = NULL;
  if ( (rc = gettoken(f)) || (rc = parseform( &fmtlist )) ) {
    freeforms(fmtlist);
    if (dataList) (void)free((char *)dataList); 
    if ( rc == ERR_MESSAGE ) ERRMSG(errmsg_fmt) else ERROUT(rc);
  } else {			/* OK print warning if not at end */
    if (tok.type != T_EOF) {
      (void)FWarn(0,"Extra characters at end of format ignored\n");
    }
  }
  
  /* Determine and allocate space needed to hold formatted output */
  if ( rc = printcols(fmtlist, dcols, &pcols) ){
    freeforms(fmtlist);
    if (dataList) (void)free((char *)dataList); 
    if ( rc == ERR_MESSAGE ) ERRMSG(errmsg_fmt) else ERROUT(rc);    
  }
  dims[0] = maxrows;
  dims[1] = pcols;
  if (!(a = (A)ga(Ct,2,pcols*maxrows,dims))) {
    freeforms(fmtlist);
    if (dataList) (void)free((char *)dataList);
    return NULL;
  }
  (void)memset((char *)a->p, ' ', pcols*maxrows);
  EndOfObj = (pcols*maxrows) + (char *)a->p; /* save addr for data boundry */
  
  /* Format the data */
  if (rc = dofmt(fmtlist, maxrows, dcols, pcols, (char *)a->p)) {
    if ( rc == ERR_MESSAGE ) ERRMSG(errmsg_fmt) else ERROUT(rc);
  }
  
  freeforms(fmtlist);
  if (dataList) (void)free((char *)dataList);
  return a;
}

void fmtInstall()
{
  install((PFI)ep_fmt,"_fmt", A_, 2, CP, A_,0,0,0,0,0,0);
  return;
}

#if defined(__FreeBSD__) || defined(__NetBSD__)
/* ecvt.c  --  an implementation of ecvt()</h1>
Choose your browser's option to Save As... to download this code example. Send
the program to your AS/400 using FTP or similar method and include it in your source
code with the line below.
#include "ecvt.c"
This program was developed on a V3R7 system.  There is a notable difference between
this version of ecvt() and that of other platforms.  This relates to the use
of _INF, INFINITY, and NAN macros.  None of these macros exist in AS/400 header files,
so as such, are not directly supported.  However, the HUGE_VAL macro is supported for
the incoming Value parameter.  To simulate the action of ecvt() on other platforms,
the HUGE_VAL macro is translated to return "INF" as the resultant string of ecvt().
NAN is supported using another workaround, the isnan() workaround.

This code fragment that is furnished by IBM is a simple example to provide an
illustration. This example has not been thoroughly tested under all conditions.
IBM, therefore, cannot guarantee or imply reliability, serviceability, or function
of this program. All code contained herein is provided to you "AS IS". THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
EXPRESSLY DISCLAIMED.

*/

#include <stdio.h>
#include <string.h>


#define QPIDECVTBUFSIZE 128

/* typedef struct { */
/*     unsigned long lobits; */
/*     unsigned long hibits; */
/*     } BITMASK; */

/* typedef union { */
/*     BITMASK bm; */
/*     double  dv; */
/*     } OVERLAY; */

/* #define EXPBITS  0x7FF00000 */
/* #define ALLZEROS 0x00000000 */
/* #define CHKEXP   0x000FFFFF */

/****** prototypes ******/
/* int isnan(double fres); */
/* int finite(double fres); */
char* ecvt(double Value, int NumberOfDigits, int* DecimalPointer, int* Sign);
/************************/

char* QPIDecvt_buffer=NULL;



char* ecvt(double Value, int NumberOfDigits, int* DecimalPointer, int* Sign)
{
  char buf[QPIDECVTBUFSIZE];
  char spfbuf[10];
  int idx=0;
  int idx2;
  int aft=0;

  if (QPIDecvt_buffer == NULL) QPIDecvt_buffer = (char*)malloc(QPIDECVTBUFSIZE);

  if (!finite(Value))
     {
     *DecimalPointer=0;
     if (Value < 0) *Sign = 1; else *Sign = 0;
     strcpy(QPIDecvt_buffer,"INF");
     return QPIDecvt_buffer;
     }
  if (isnan(Value))
     {
     *Sign=0;
     *DecimalPointer=0;
     strcpy(QPIDecvt_buffer,"NAN");
     return QPIDecvt_buffer;
     }

  *Sign = 0;
  sprintf(buf, "%#+1.*E",NumberOfDigits-1,Value);
  printf("   buf '%s'\n", buf);

  switch (buf[idx])
     {
     case '-':
        *Sign = 1;
        aft++;
        break;
     case '+':
        aft++;
        break;
     }

  QPIDecvt_buffer[0] = buf[1];

  for (idx=1; idx < NumberOfDigits; idx++)
     QPIDecvt_buffer[idx] = buf[idx+2];

  QPIDecvt_buffer[NumberOfDigits] = '\0';
  sscanf(buf+NumberOfDigits+4, "%d", DecimalPointer);
  (*DecimalPointer) ++;
  return QPIDecvt_buffer;
}



#if 0
/* just call the libc libm versions for isnan and finite */ 
int isnan( double fres ) {

    OVERLAY value;

    value.dv = fres;
    if ( ( (value.bm.lobits & EXPBITS) == EXPBITS )  &&
                 ( ( (value.bm.lobits & CHKEXP)  != ALLZEROS ) ||
                 ( value.bm.hibits != ALLZEROS ) ) )
        return( 1 ); /* Yes, it is not a number NaN */
    else
                return( 0 ); /* it is a number */
}



int finite( double fres ) {

    OVERLAY value;

    value.dv = fres;
    if ( ( (value.bm.lobits & EXPBITS) == EXPBITS )  &&
		 ( (value.bm.lobits & CHKEXP)  == ALLZEROS ) &&
		 ( value.bm.hibits == ALLZEROS ) )
        return( 0 ); /* No, it is not a finite number */
    else
		return( 1 ); /* it is a finite number */
}
#endif
#endif
