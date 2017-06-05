/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/development.h>
#include <stdio.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/arthur.h>
#include <dap/balloc.h>
#include "cxc.h"

static char *alignment_origin();
static A retrieve_field();
static int deposit_field();
static int field_to_index();


#define MODRNDUP(v,m) (((v)+(m))-(1+((((v)+(m))-1)%(m))))
#define INDEX_TO_OFFSET(s, i) (((A*)s->p)[3]->p[i])
#define INDEX_TO_TYPE(s, i) (((A*)s->p)[4]->p[i])
#define INDEX_TO_LENGTH(s, i) (((A*)s->p)[1]->p[i])

/* Type Codes */
#define TC_CHAR 1
#define TC_U_CHAR 2
#define TC_SHORT 3
#define TC_U_SHORT 4
#define TC_INT 5
#define TC_U_INT 6
#define TC_LONG 7
#define TC_U_LONG 8
#define TC_STRUCT1 9
#define TC_STRUCT2 10
#define TC_STRUCT4 11
#define TC_STRUCT8 12
#define TC_POINTER 13
#define TC_FLOAT 14
#define TC_DOUBLE 15

/* Type Alignments */
#ifdef mc68000
#define AL_CHAR 1
#define AL_U_CHAR 1
#define AL_SHORT 2
#define AL_U_SHORT 2
#define AL_INT 2
#define AL_U_INT 2
#define AL_LONG 2
#define AL_U_LONG 2
#define AL_STRUCT1 1
#define AL_STRUCT2 2
#define AL_STRUCT4 4
#define AL_STRUCT8 8
#define AL_POINTER 2
#define AL_FLOAT 2
#define AL_DOUBLE 2
#define STRUCT_PAD_TO 2
#else
#define AL_CHAR sizeof(char)
#define AL_U_CHAR sizeof(char)
#define AL_SHORT sizeof(short)
#define AL_U_SHORT sizeof(short)
#define AL_INT sizeof(int)
#define AL_U_INT sizeof(int)
#define AL_LONG sizeof(long)
#define AL_U_LONG sizeof(long)
#define AL_STRUCT1 1
#define AL_STRUCT2 2
#define AL_STRUCT4 4
#define AL_STRUCT8 8
#define AL_POINTER sizeof(I *)
#define AL_FLOAT sizeof(float)
#define AL_DOUBLE sizeof(double)
#define STRUCT_PAD_TO 1
#endif
static struct typeinfo {
  char *name;
  int alignment;
  int typecode;
  int size;
} typeinfo[] = {
  { "char", AL_CHAR, TC_CHAR, sizeof(char) },
  { "u_char", AL_U_CHAR, TC_U_CHAR, sizeof(unsigned char) },
  { "short", AL_SHORT, TC_SHORT, sizeof(short) },
  { "u_short", AL_U_SHORT, TC_U_SHORT, sizeof(unsigned short) },
  { "int", AL_INT, TC_INT, sizeof(int) },
  { "u_int", AL_U_INT, TC_U_INT, sizeof(unsigned int) },
  { "long", AL_LONG, TC_LONG, sizeof(long) },
  { "u_long", AL_U_LONG, TC_U_LONG, sizeof(unsigned long) },
  { "struct1", AL_STRUCT1, TC_STRUCT1, sizeof(char) },
  { "struct2", AL_STRUCT2, TC_STRUCT2, sizeof(char) },
  { "struct4", AL_STRUCT4, TC_STRUCT4, sizeof(char) },
  { "struct8", AL_STRUCT8, TC_STRUCT8, sizeof(char) },
  { "pointer", AL_POINTER, TC_POINTER, sizeof(void *) },
  { "float", AL_FLOAT, TC_FLOAT, sizeof(float) },
  { "double", AL_DOUBLE, TC_DOUBLE, sizeof(double) }
};

#define NUM_TYPES (sizeof(typeinfo)/sizeof(struct typeinfo))

/*
 * Define a structure type - Do correct architecture alignment
 */
I structdef(fields, lengths, types)
  A fields;	/* Symbols specifying field names */
  A lengths;	/* Number of elements of type in field */
  A types;	/* Symbols specifying field types */
{
  A s;		/* Resulting structure template */
  A offsets;	/* Offsets of fields in structure */
  A typecodes;	/* Type codes of fields in structure */
  int next_pos;	/* Next byte position available in structure */
  int n;	/* Number of fields */
  int i;	/* Loop index */

  /* More robust argument checks can be added later.  For example,
   * two fields with the same name should not be allowed within a
   * given structure
   */

  /* Check the lengths of the arguments */
  n = fields->n;
  if (n != lengths->n || n != types->n) {
    q = ERR_LENGTH;
    return 0;
  }

  /* Check the types of the arguments */
  if (fields->t != Et || lengths->t != It || types->t != Et) {
    q = ERR_TYPE;
    return 0;
  }

  /* Allocate vector to contain offsets and packed array size */
  if (!(offsets = gv(It, n + 1))) return 0;	/* WS FULL */

  /* Allocate vector to contain field type codes and packed array alignment */
  if (!(typecodes = gv(It, n + 1))) {
    dc(offsets);
    return 0;					/* WS FULL */
  }
  next_pos = 0;
  typecodes->p[n] = 0;
  for (i = 0; i < n; i++) {
    int j, k;
    char *tp = XS(types->p[i])->n;
    for (j = 0; j < NUM_TYPES; j++) {
      if (!strcmp(tp, typeinfo[j].name)) break;
    }
    if (j == NUM_TYPES) {
      /* Invalid type specified */
      q = ERR_DOMAIN;
      dc(typecodes);
      dc(offsets);
      return 0;
    } else {
      k = typeinfo[j].alignment;
      offsets->p[i] = ((next_pos + k - 1) / k) * k;
      typecodes->p[i] = typeinfo[j].typecode;
      next_pos = offsets->p[i] + lengths->p[i] * typeinfo[j].size;
      typecodes->p[n] = MAX(typecodes->p[n], k);
    }
#ifdef DEBUG
    (void)printf("field = '%s',length = %d,type = '%s',o = %d,t = %d\n",
		 XS(fields->p[i])->n, lengths->p[i],
		 XS(types->p[i])->n, offsets->p[i], typecodes->p[i]);
#endif /* DEBUG */
  }

  /* Store size of structure at end of offsets vector */
  offsets->p[i] = MODRNDUP(next_pos, typecodes->p[n]);
  offsets->p[i] = MODRNDUP(offsets->p[i], STRUCT_PAD_TO);

  /* Create general array to contain fields, lengths, types, and offsets
   * and type codes
   */
  if (!(s = gv(Et, 5))) {
    dc(offsets);
    return 0;	/* WS FULL */
  }
  ((A*)s->p)[0] = fields; ((A*)s->p)[1] = lengths; ((A*)s->p)[2] = types;
  ((A*)s->p)[3] = offsets; ((A*)s->p)[4] = typecodes;

  /* Bump reference counts */
  ic(fields); ic(lengths); ic(types);
  return (I)s;
}

/*
 * Return size of structure in bytes
 */
A structsize(s)
  A s;
{
  A a = ((A*)(s->p))[3];
  return gi(a->p[a->n-1]);
}

/*
 * Returns an array large enough to represent
 * the structure defined by s.  This means there must
 * be enough space in the array to place a properly aligned
 * structure.
 */
I structcreate(s)
  A s;
{
  A a = ((A*)(s->p))[3];
  A b = ((A*)(s->p))[4];
  I n = a->n - 1;

  /* Compute structure size plus padding to guarantee room for alignment */
  n = (a->p[n] + b->p[n] + sizeof(I) - 2) / sizeof(I);
  a = gv(It, n);
  zr(a);		/* zero out the array */
  return (I)a;
}

/*
 * Return value of fields of a in a general array.  If only 1 field
 * is specified, then the result is simple.
 */
I structget(s, a, fields)
  A s;
  A a;
  A fields;
{
  A z;
  int i;

  if (fields->n == 1) {
    z = retrieve_field(s, a, *(S*)fields->p);
  } else {
    if (z = gv(Et, fields->n)) {
      zr(z);
      for (i = 0; i < fields->n; i++) {
	if (!(((A*)(z->p))[i] = retrieve_field(s, a, ((S*)fields->p)[i]))) {
	  dc(z);
	  z = (A)0;
	}
      }
    }
  }
  return (I)z;
}

/*
 * Set the values of fields.  If multiple fields, values must be
 * in a general array.  If only 1 field is specified,
 * then the value is simple.
 */
I structset(s, a, fields, values)
  A s;
  A a;
  A fields;
  A values;
{
  int i;
  int deposit_field();

  if (fields->n == 1) {
    if (deposit_field(s, a, *(S*)fields->p, values)) {
      return 0;
    }
  } else {
    for (i = 0; i < fields->n; i++) {
      if (deposit_field(s, a, ((S*)fields->p)[i], ((A*)values->p)[i])) {
        return 0;
      }
    }
  }
  ic(a);
  return (I)a;
}

static A retrieve_field(s, a, f)
  A s;
  A a;
  S f;
{
  int i, j, type;
  char *cp, *dp;
  A r;

  i = field_to_index(s, f);
  if (i == -1) return (A)0;
  cp = alignment_origin(s, a) + INDEX_TO_OFFSET(s, i);
  j = INDEX_TO_LENGTH(s, i);
  type = INDEX_TO_TYPE(s, i);
  switch (type) {
    /* XXX - should I handle TC_CHAR and TC_U_CHAR here? */
  case TC_FLOAT:
  case TC_DOUBLE:
    if (j > 1) r = gv(Ft, j); else r = gs(Ft);
    break;
  default:
    if (j > 1) r = gv(It, j); else r = gs(It);
    break;
  }
  switch (type) {
  case TC_CHAR:
    for (i = 0; i < j; i++) r->p[i] = ((char *)cp)[i];
    break;
  case TC_U_CHAR:
    for (i = 0; i < j; i++) r->p[i] = ((unsigned char *)cp)[i];
    break;
  case TC_SHORT:
    for (i = 0; i < j; i++) r->p[i] = ((short *)cp)[i];
    break;
  case TC_U_SHORT:
    for (i = 0; i < j; i++) r->p[i] = ((unsigned short *)cp)[i];
    break;
  case TC_INT:
    for (i = 0; i < j; i++) r->p[i] = ((int *)cp)[i];
    break;
  case TC_U_INT:
    for (i = 0; i < j; i++) r->p[i] = ((unsigned int *)cp)[i];
    break;
  case TC_LONG:
    for (i = 0; i < j; i++) r->p[i] = ((long *)cp)[i];
    break;
  case TC_U_LONG:
    for (i = 0; i < j; i++) r->p[i] = ((unsigned long *)cp)[i];
    break;
  case TC_STRUCT1:
    dc(r);
    r = gv(It, (j + AL_STRUCT1 + sizeof(I) - 2) / sizeof(I));
    dp = (char *)(((long)r->p + AL_STRUCT1 - 1) & ~(AL_STRUCT1 - 1));
    for (i = 0; i < j; i++) dp[i] = cp[i];
    break;
  case TC_STRUCT2:
    dc(r);
    r = gv(It, (j + AL_STRUCT2 + sizeof(I) - 2) / sizeof(I));
    dp = (char *)(((long)r->p + AL_STRUCT2 - 1) & ~(AL_STRUCT2 - 1));
    for (i = 0; i < j; i++) dp[i] = cp[i];
    break;
  case TC_STRUCT4:
    dc(r);
    r = gv(It, (j + AL_STRUCT4 + sizeof(I) - 2) / sizeof(I));
    dp = (char *)(((long)r->p + AL_STRUCT4 - 1) & ~(AL_STRUCT4 - 1));
    for (i = 0; i < j; i++) dp[i] = cp[i];
    break;
  case TC_STRUCT8:
    dc(r);
    r = gv(It, (j + AL_STRUCT8 + sizeof(I) - 2) / sizeof(I));
    dp = (char *)(((long)r->p + AL_STRUCT8 - 1) & ~(AL_STRUCT8 - 1));
    for (i = 0; i < j; i++) dp[i] = cp[i];
    break;
  case TC_POINTER:
    for (i = 0; i < j; i++) r->p[i] = ((int *)cp)[i];
    break;
  case TC_FLOAT:
    r->t = Ft;
    for (i = 0; i < j; i++) ((double *)r->p)[i] = ((float *)cp)[i];
    break;
  case TC_DOUBLE:
    r->t = Ft;
    for (i = 0; i < j; i++) ((double *)r->p)[i] = ((double *)cp)[i];
    break;
  default:
    dc(r);
    r = (A)0;
  }
  return r;
}

static int deposit_field(s, a, f, v)
  A s;
  A a;
  S f;
  A v;
{
  int field_to_index();
  int i, j, type;
  char *cp, *dp;

  i = field_to_index(s, f);
  if (i == -1) return -1;
  cp = alignment_origin(s, a) + INDEX_TO_OFFSET(s, i);
  j = INDEX_TO_LENGTH(s, i);
  type = INDEX_TO_TYPE(s, i);
  switch (type) {
  case TC_CHAR:
    for (i = 0; i < j; i++) ((char *)cp)[i] = (char)v->p[i];
    break;
  case TC_U_CHAR:
    for (i = 0; i < j; i++) ((unsigned char *)cp)[i] = (unsigned char)v->p[i];
    break;
  case TC_SHORT:
    for (i = 0; i < j; i++) ((short *)cp)[i] = (short)v->p[i];
    break;
  case TC_U_SHORT:
    for (i = 0; i < j; i++) ((unsigned short *)cp)[i]=(unsigned short)v->p[i];
    break;
  case TC_INT:
    for (i = 0; i < j; i++) ((int *)cp)[i] = (int)v->p[i];
    break;
  case TC_U_INT:
    for (i = 0; i < j; i++) ((unsigned int *)cp)[i] = (unsigned int)v->p[i];
    break;
  case TC_LONG:
    for (i = 0; i < j; i++) ((long *)cp)[i] = (long)v->p[i];
    break;
  case TC_U_LONG:
    for (i = 0; i < j; i++) ((unsigned long *)cp)[i] = (unsigned long)v->p[i];
    break;
/* these were ints */
  case TC_STRUCT1:
    dp = (char *)(((long)v->p + AL_STRUCT1 - 1) & ~(AL_STRUCT1 - 1));
    for (i = 0; i < j; i++) cp[i] = dp[i];
    break;
  case TC_STRUCT2:
    dp = (char *)(((long)v->p + AL_STRUCT2 - 1) & ~(AL_STRUCT2 - 1));
    for (i = 0; i < j; i++) cp[i] = dp[i];
    break;
  case TC_STRUCT4:
    dp = (char *)(((long)v->p + AL_STRUCT4 - 1) & ~(AL_STRUCT4 - 1));
    for (i = 0; i < j; i++) cp[i] = dp[i];
    break;
  case TC_STRUCT8:
    dp = (char *)(((long)v->p + AL_STRUCT8 - 1) & ~(AL_STRUCT8 - 1));
    for (i = 0; i < j; i++) cp[i] = dp[i];
    break;
  case TC_POINTER:
    for (i = 0; i < j; i++) ((int *)cp)[i] = v->p[i];
    break;
  case TC_FLOAT:
    for (i = 0; i < j; i++) ((float *)cp)[i] = (float)((double *)v->p)[i];
    break;
  case TC_DOUBLE:
    for (i = 0; i < j; i++) ((double *)cp)[i] = ((double *)v->p)[i];
    break;
  default:
    return -1;	/* Failure */
  }
  return 0;		/* Success */
}

static int field_to_index(s, f)
  A s;
  S f;
{
  int i;
  S *fields = (S*)(*(A*)s->p)->p;

  for (i = 0; i < (*(A*)s->p)->n; i++) {
    if (fields[i] == f) {
#ifdef DEBUG
      (void)printf("field_to_index: field <%s>, index %d\n", XS(f)->n, i);
#endif /* DEBUG */
      return i;
    }
  }
  q = ERR_DOMAIN;
  return -1;
}

static char *alignment_origin(s, a)
  A s;
  A a;
{
  A z = ((A*)s->p)[4];
  long u = (long)a->p;
  long v = z->p[z->n - 1] - 1;	/* Alignment factor */
  /* The alignment is assumed to be a power of 2 */
  v = (u + v) & (~v);
#ifdef DEBUG
  (void)printf("alignment_origin: u %d, v %d, u - v %d\n", u, v, u-v);
#endif /* DEBUG */
  return (char *)v;
}

/*
 * Return a pointer (in an integer) to the beginning of the data in a struct
 */
I pointer(s, a)
A s;
A a;
{
  return (I)gi((I)alignment_origin(s, a));
}

I ptr(a) A a; {return (I)a->p;}

/*
 * Return a packed array which is a copy of the structure
 * pointed to by p. Structure is defined by s.
 */
I struct_pointed_to_by(s, p)
  A s;
  A p;
{
  int n, i;
  char *cp;
  char *pp = (char *)p->p[0];
  A a = ((A*)(s->p))[3];
  n = a->p[a->n - 1];
  if (a = (A)structcreate(s)) {
    cp = alignment_origin(s, a);
    for (i = 0; i < n; i++) cp[i] = pp[i];
  }
  return (I)a;
}

/*
 * Return (double value of) float pointed to by p
 */
I float_pointed_to_by(p)
  A p;
{
  A z;

  if (z = gs(Ft)) {
    *(F *)z->p = (F)*(float *)(p->p[0]);
  }
  return (I)z;
}

/*
 * Return double pointed to by p
 */
I double_pointed_to_by(p)
  A p;
{
  A z;

  if (z = gs(Ft)) {
    *(F *)z->p = *(F *)(p->p[0]);
  }
  return (I)z;
}

/*
 * Return char pointed to by p
 */
A char_pointed_to_by(p)
  A p;
{
  return gi((I)*(char *)(p->p[0]));
}

/*
 * Return int pointed to by p
 */
A int_pointed_to_by(p)
  A p;
{
  return gi(*(I *)(p->p[0]));
}

/*
 * Return short pointed to by p
 */
A short_pointed_to_by(p)
  A p;
{
  return gi((I)*(short *)(p->p[0]));
}

/*
 * Return string pointed to by p
 */
I string_pointed_to_by(p)
  A p;
{
  return (I)gsv(0, (char *)(p->p[0]));
}

I structtype(s)
  A s;
{
  extern S si();
  A z = ((A*)s->p)[4];
  int alignment = z->p[z->n - 1];
  S sym;

  switch (alignment) {
  case AL_STRUCT1: sym = si("struct1"); break;
  case AL_STRUCT2: sym = si("struct2"); break;
  case AL_STRUCT4: sym = si("struct4"); break;
  case AL_STRUCT8: sym = si("struct8"); break;
  default:
    sym = (S)0;
    break;
  }
  if (sym) {
    z = gs(Et);
    *z->p = MS(sym);
  } else {
    z = (A)0;
  }
  return (I)z;
}

/*
 * Display the contents of a structure in a and
 * described by s in a reasonable fashion
 */
void structprint(s, a)
  A a;
  A s;
{
  int i;
  A z;
  A fields = ((A*)s->p)[0];
  /* A lengths = ((A*)s->p)[1]; */
  A types = ((A*)s->p)[2];
  /* A offsets = ((A*)s->p)[3]; */
  A typecodes = ((A*)s->p)[4];

  /* For each field */
  for (i = 0; i < ((A*)s->p)[0]->n; i++) {
    switch (typecodes->p[i]) {
    case TC_CHAR:
    case TC_U_CHAR:
    case TC_SHORT:
    case TC_U_SHORT:
    case TC_INT:
    case TC_U_INT:
    case TC_LONG:
    case TC_U_LONG:
    case TC_POINTER:
    case TC_FLOAT:
    case TC_DOUBLE:
      z = retrieve_field(s, a, ((S*)fields->p)[i]);
      (void)printf("%s:(%s):", XS(fields->p[i])->n, XS(types->p[i])->n);
      pa((V)z);
      (void)putchar('\n');
      dc(z);
      break;
    case TC_STRUCT1:
    case TC_STRUCT2:
    case TC_STRUCT4:
    case TC_STRUCT8:
      (void)printf("%s:(%s): ----\n",
		   XS(fields->p[i])->n, XS(types->p[i])->n);
      break;
    }
  }
}

void place_ints_at(ai, p)
  A	ai;
  I	*p;
{
  int i;
  for (i = 0; i < ai->n; i++)
    *p++ = ai->p[i];
}

void place_floats_at(af, p)
  A	af;
  F	*p;
{
  int i;
  for (i = 0; i < af->n; i++)
    *p++ = ((F*)af->p)[i];
}

void place_chars_at(ac, p)
  A	ac;
  C	*p;
{
  int i;
  for (i = 0; i < ac->n; i++)
    *p++ = ((C*)ac->p)[i];
}

A AHeader(a)
  A	a;
{
  A	r, d;
  I	i;
  static char *types[] = { "It","Ft","Ct","", "Et","","","","Xt","Xt+1","Xt+2","Xt+3" };
  r = (A)gv(Et,5);
  ((A*)r->p)[0] = gi(a->c);
  ((A*)r->p)[1] = (A)gsv(0, types[a->t]);
  ((A*)r->p)[2] = gi(a->r);
  ((A*)r->p)[3] = gi(a->n);
  ((A*)r->p)[4] = d = gv(It, a->r);
  for (i = 0; i < a->r; i++)
    d->p[i] = a->d[i];
  R r;
}

Z I DetermineLength(a)
  A	a;
{
  int	i;
  I	length, stat;

  /*
   * for It, Ft, and Ct write out
   *	1 char = type information ('I', 'F', 'C')
   *	1 char = rank
   *	1 I    = length (times reduce rho)
   *	rank I = dimensions
   *	data
   * for Et write out
   *	1 char = type information ('I', 'F', 'C')
   *	1 char = rank
   *	1 I    = length (times reduce rho)
   *	rank I = dimensions
   * for Symbols write out
   *	1 char = type
   *	1 I    = string length
   *    symbol text
   */
  switch (a->t) {
  case It:
    length = 2*sizeof(char) + (1 + a->r)*sizeof(I) + a->n*sizeof(I);
    break;
  case Ft:
    length = 2*sizeof(char) + (1 + a->r)*sizeof(I) + a->n*sizeof(F);
    break;
  case Ct:
    length = 2*sizeof(char) + (1 + a->r)*sizeof(I) + a->n*sizeof(C);
    break;
  case Et:
    length = 2*sizeof(char) + (1 + a->r)*sizeof(I);
    for (stat = i = 0; i < a->n && stat != -1; i++) {
      if (QA(((A*)a->p)[i]))
	length += stat = DetermineLength(((A*)a->p)[i]);
      else if (QS(((A*)a->p)[i]))
	length += (1 + strlen(XS(((A*)a->p)[i])->n))*sizeof(char) + sizeof(I);
      else {
	q = ERR_TYPE;
	length = stat = -1;
      }
    }
    break;
  default:
    R (I)(-1);
  }
  R length;
}

Z int PumpData(start, end, a)
  char	*start, *end;
  A	a;
{
  char	*cp;
  I	i, len;

  cp = start;
  switch (a->t) {
  case It:
    if (end - start < 2*sizeof(char) + (1 + a->r)*sizeof(I) + a->n*sizeof(I)) {
      q = ERR_LENGTH;
      len = -1;
    } else {
      *cp++ = 'I';
      *cp++ = (char)a->r;
      (void)bcopy(&a->n, cp, sizeof(I)); cp += sizeof(I);
      (void)bcopy(a->d, cp, (i = a->r * sizeof(I))); cp += i;
      (void)bcopy(a->p, cp, (i = a->n * sizeof(I))); cp += i;
      len = cp - start;
    }
    break;
  case Ft:
    if (end - start < 2*sizeof(char) + (1 + a->r)*sizeof(I) + a->n*sizeof(F)) {
      q = ERR_LENGTH;
      len = -1;
    } else {
      *cp++ = 'F';
      *cp++ = (char)a->r;
      (void)bcopy(&a->n, cp, sizeof(I)); cp += sizeof(I);
      (void)bcopy(a->d, cp, (i = a->r * sizeof(I))); cp += i;
      (void)bcopy(a->p, cp, (i = a->n * sizeof(F))); cp += i;
      len = cp - start;
    }
    break;
  case Ct:
    if (end - start < 2*sizeof(char) + (1 + a->r)*sizeof(I) + a->n*sizeof(C)) {
      q = ERR_LENGTH;
      len = -1;
    } else {
      *cp++ = 'C';
      *cp++ = (char)a->r;
      (void)bcopy(&a->n, cp, sizeof(I)); cp += sizeof(I);
      (void)bcopy(a->d, cp, (i = a->r * sizeof(I))); cp += i;
      (void)bcopy(a->p, cp, (i = a->n * sizeof(C))); cp += i;
      len = cp - start;
    }
    break;
  case Et:
    if (end - start < 2*sizeof(char) + (1 + a->r)*sizeof(I)) {
      q = ERR_LENGTH;
      len = -1;
    } else {
      *cp++ = 'E';
      *cp++ = (char)a->r;
      (void)bcopy(&a->n, cp, sizeof(I)); cp += sizeof(I);
      (void)bcopy(a->d, cp, (i = a->r * sizeof(I))); cp += i;
      for (len = i = 0; i < a->n && len != -1; i++) {
	if (QA(((A*)a->p)[i]))
	  cp += (len = PumpData(cp, end, ((A*)a->p)[i]));
	else if (QS(((A*)a->p)[i])) {
	  len = strlen(XS(((A*)a->p)[i])->n) * sizeof(C);
	  if ((end - cp) < (len + sizeof(C) + sizeof(I))) {
	    q = ERR_LENGTH;
	    len = -1;
	  } else {
	    *cp++ = 'S';
	    (void)bcopy(&len, cp, sizeof(I)); cp += sizeof(I);
	    (void)bcopy(XS(((A*)a->p)[i])->n, cp, len); cp += len;
	  }
	} else {
	  q = ERR_TYPE;
	  len = -1;
	}
      }
      len = cp - start;
    }
    break;
  default:
    len = -1;
    break;
  }
  R len;
}

A stuff(a)
  A	a;
{
  A	r;
  I	length;
  char	*start, *end;

  if ((length = DetermineLength(a)) == -1)
    R 0;

  /* Add room for header info */
  r = gv(Ct, length + sizeof(I));
  start = (char *)r->p;
  end = (char *)(r->p + length + sizeof(I));

  (void)bcopy(&length, start, sizeof(I));
  start += sizeof(I);
  if (PumpData(start, end, a) == -1) {
    dc(r);
    r = (A)gz();
  }
  R r;
}

Z A SuckData(start, end)
  char	**start, *end;
{
  char	*cp;
  char	type;
  int	i;
  I	d[MAXR+1], rank, length;
  A	r=0, s;

  if (*start == end) {
    q = ERR_LENGTH;
    R (A)0;
  }
  cp = *start;
  type = *cp++;
  switch (type) {
  case 'I':
  case 'F':
  case 'C':
  case 'E':
    if (end - cp < sizeof(char) + sizeof(I) ||
	(rank = (I)(unsigned char)*cp++) > MAXR) {
      q = ERR_LENGTH;
      R (A)0;
    }
    (void)bcopy(cp, &length, sizeof(I)); cp += sizeof(I);
    if (end - cp < rank * sizeof(I)) {
      q = ERR_LENGTH;
      R (A)0;
    }
    (void)bcopy(cp, d, (i = rank * sizeof(I))); cp += i;
    switch (type) {
    case 'I':
      r = ga(It, rank, length, d);
      (void)bcopy(cp, r->p, (i = length * sizeof(I))); cp += i;
      break;
    case 'F':
      r = ga(Ft, rank, length, d);
      (void)bcopy(cp, r->p, (i = length * sizeof(F))); cp += i;
      break;
    case 'C':
      r = ga(Ct, rank, length, d);
      (void)bcopy(cp, r->p, (i = length * sizeof(C))); cp += i;
      break;
    case 'E':
      r = ga(Et, rank, length, d);
      for (i = 0; i < length; i++) {
	if ((r->p[i] = (I)SuckData(&cp, end)) == (I)0)
	  R (A)0;
      }
      break;
    }
    break;
  case 'S':
    if (end - cp < sizeof(I)) {
      q = ERR_LENGTH;
      R (A)0;
    }
    (void)bcopy(cp, &length, sizeof(I)); cp += sizeof(I);
    if (end - cp < length * sizeof(C)) {
      q = ERR_LENGTH;
      R (A)0;
    }
    s = gv(Ct, length);
    (void)bcopy(cp, s->p, (i = length * sizeof(C))); cp += i;
    r = (A)MS(si((char *)s->p));
    dc(s);
    break;
  default:
    q = ERR_TYPE;
    R 0;
  }
  *start = cp;
  R r;
}

A unstuff(a)
  A	a;
{
  I	length;
  char	*start, *end;

  if (a->t != Ct) {
    q = ERR_TYPE;
    R (A)0;
  } if (a->n < 4) {
    q = ERR_LENGTH;
    R (A)0;
  }
  (void)bcopy(a->p, &length, sizeof(I));
  start = ((char *)a->p) + sizeof(I);
  end = ((char *)a->p) + a->n;
  if (end - start != length) {
    q = ERR_LENGTH;
    R (A)0;
  }
  R SuckData(&start, end);
}

PointerTable *AllocPointerTable()
{
  PointerTable *p;

  p = (PointerTable *)balloc(sizeof(*p));
  p->length = 0;
  p->ptr = NULL;
  R p;
}

void FreePointerTable(p)
  PointerTable *p;
{
  bfree((char *)p->ptr);
  bfree((char *)p);
}

char *AToString(a)
  A	a;
{
  char	*string;
  switch (a->t) {
  case Ct: string = (C *)a->p; break;
  case It: string = (C *)*a->p; break;
  case Et:
    if (a->n == 0)
      string = (char *)0;
    else if (!QS(*a->p)) {
      q = ERR_TYPE;
      string = (char *)(-1);
    } else
      string = XS(*a->p)->n;
    break;
  default:
    if (a->n == 0)
      string = (char *)0;
    else {
      q = ERR_TYPE;
      string = (char *)(-1);
    }
  }
  R string;
}

void *FetchPointer(table, index)
  PointerTable	*table;
  I		index;
{
  void *ptr;

  if (table == NULL ||
      index < 0 ||
      index >= table->length ||
      (ptr = table->ptr[index]) == (void *)(-1))
    ptr = (void *)(-1);
  R ptr;
}

int FetchIndex(table, ptr)
  PointerTable	*table;
  void		*ptr;
{
  int	i;
  
  if (table != NULL)
    for (i = 0; i < table->length; i++)
      if (table->ptr[i] == ptr)
	R i;
  R -1;
}

void RemovePointer(table, index)
  PointerTable	*table;
  I		index;
{
  if (table != NULL && index >= 0 && index < table->length)
    table->ptr[index] = (void *)(-1);
}

I InternPointer(table, ptr)
  PointerTable	*table;
  void		*ptr;
{
  I	i;

  if (table == NULL)
    i = (-1);
  else {
    /* If pointer is already installed just return it */
    for (i = 0; i < table->length; i++)
      if (table->ptr[i] == ptr)
	return i;
    for (i = 0; i < table->length; i++)
      if (table->ptr[i] == (void *)(-1)) break;
    if (i == table->length) {
      table->length++;
      table->ptr = (void **)brealloc((char *)table->ptr,
				     table->length*sizeof(void *));
    }
    table->ptr[i] = ptr;
  }
  R i;
}

void InitStructureTable(sTable)
  StructureTable	*sTable;
{
  int	i;
  for (i = 0; sTable[i].string != (char *)0; i++)
    sTable[i].msymbol = MS(si(sTable[i].string));
}

void InitEnumTable(enumTable)
  EnumTable	*enumTable;
{
  int i;

  for (i = 0; enumTable[i].string != (char *)0; i++)
    enumTable[i].msymbol = MS(si(enumTable[i].string));
}

void InitMaskTable(maskTable)
  MaskTable	*maskTable;
{
  int i;

  for (i = 0; maskTable[i].string != (char *)0; i++)
    maskTable[i].msymbol = MS(si(maskTable[i].string));
}

A EnumToSymbol(enumTable, value)
  EnumTable	*enumTable;
  unsigned long	value;
{
  int 	i;
  A	r;

  /* Initialize enumTable if needed */
  if (enumTable[0].msymbol == 0)
    InitEnumTable(enumTable);

  for (i = 0; enumTable[i].string != (char *)0; i++)
    if (enumTable[i].value == value) {
      r = gs(Et);
      *r->p = enumTable[i].msymbol;
      R r;
    }
  R (A)gz();
}

A MaskToSymbols(maskTable, mask)
  MaskTable	*maskTable;
  unsigned long	mask;
{
  int 	i, n;
  A	r;

  /* Initialize maskTable if needed */
  if (maskTable[0].msymbol == 0)
    InitMaskTable(maskTable);

  for (n = i = 0; maskTable[i].string != (char *)0; i++) {
    if ((mask & maskTable[i].mask) == maskTable[i].mask) {
      maskTable[i].found = 1;
      n++;
    } else
      maskTable[i].found = 0;
  }
  if (n) {
    r = gv(Et, n);
    for (n = i = 0; maskTable[i].string != (char *)0; i++)
      if (maskTable[i].found)
	r->p[n++] = maskTable[i].msymbol;
  } else
    r = (A)gz();
  R r;
}

int SymbolToEnum(enumTable, a, valuep)
  EnumTable	*enumTable;
  A		a;
  unsigned long	*valuep;
{
  int	i;

  if (a->t == It && a->n == 1) {
    *valuep = (unsigned long)a->p[0];
    R 0;
  }
  if (a->t != Et || (a->n >= 1 && !QS(*a->p)))
    R -1;

  /* Initialize enumTable if needed */
  if (enumTable[0].msymbol == 0)
    InitEnumTable(enumTable);

  for (i = 0; enumTable[i].string != 0; i++)
    if (enumTable[i].msymbol == *a->p) {
      *valuep = enumTable[i].value;
      R 0;
    }
  R -1;
}

int SymbolsToMask(maskTable, a, maskp)
  MaskTable	*maskTable;
  A		a;
  unsigned long	*maskp;
{
  I	msymbol;
  int	i, j;

  if (a->t == It && a->n == 1) {
    *maskp = (unsigned long)a->p[0];
    R 0;
  }
  if (a->t != Et)
    R -1;

  /* Initialize maskTable if needed */
  if (maskTable[0].msymbol == 0)
    InitMaskTable(maskTable);

  *maskp = 0;
  for (i = 0; i < a->n; i++) {
    if (!QS(a->p[i]))
      R -1;
    msymbol = a->p[i];
    for (j = 0; maskTable[j].string != (char *)0; j++)
      if (msymbol == maskTable[j].msymbol) {
	*maskp |= maskTable[j].mask;
	break;
      }
    if (maskTable[j].string == (char *)0)
      R -1;
  }
  R 0;
}

int AToStructure(sTable, a, valuemaskp, sp)
  StructureTable	*sTable;
  A			a;
  unsigned long		*valuemaskp;
  char			*sp;
{
  A		k, v, p;
  int		h, i, j;
  unsigned long	valuemask, value, mask;

  if (a->t != Et) {
    q = ERR_TYPE;
    R 1;
  } else if (a->n != 2) {
    q = ERR_LENGTH;
    R 1;
  }
  k = ((A*)a->p)[0];
  v = ((A*)a->p)[1];
  if (k->t != Et || v->t != Et) {
    q = ERR_TYPE;
    R 1;
  } else if (k->n != v->n) {
    q = ERR_LENGTH;
    R 1;
  }
  if (sTable[0].msymbol == 0)
    InitStructureTable(sTable);
  valuemask = 0;
  for (i = 0; i < k->n; i++) {
    if (!QS(k->p[i])) {q = ERR_TYPE; R 1;}
    for (j = 0; sTable[j].string != (char *)0; j++) {
      if (sTable[j].msymbol == k->p[i]) {
	p = ((A*)v->p)[i];
	switch (sTable[j].type) {
	case ST_CHAR:
	  if (p->t != It) {q = ERR_TYPE; R 1;}
	  *((char *)(sp+sTable[j].offset)) = (char)*p->p;
	  break;
	case ST_BOOL:
	case ST_INT:
	  if (p->t != It) {q = ERR_TYPE; R 1;}
	  *((int *)(sp+sTable[j].offset)) = (int)*p->p;
	  break;
	case ST_INTENUM:
	  if (SymbolToEnum((EnumTable *)sTable[j].table, p, &value)) R 1;
	  *((int *)(sp+sTable[j].offset)) = (int)value;
	  break;
	case ST_LONGMASK:
	  if (SymbolsToMask((MaskTable *)sTable[j].table, p, &mask)) R 1;
	  *((long *)(sp+sTable[j].offset)) = (long)mask;
	  break;
	case ST_ULONG:
	  if (p->t != It) {q = ERR_TYPE; R 1;}
	  *((unsigned long *)(sp+sTable[j].offset)) = (unsigned long)*p->p;
	  break;
	case ST_MULTIINT:
	  if (p->t != It) {q = ERR_TYPE; R 1;}
	  if (p->n != (I)sTable[j].table) {q = ERR_LENGTH; R 1;}
	  for (h = 0; h < (long)sTable[j].table; h++)
	    *(h+(int *)(sp+sTable[j].offset)) = (int)p->p[h];
	  break;
	case ST_VOID:
	default:
	  break;
	}
	valuemask |= sTable[j].mask;
	break;
      }	/* if */
    } /* for j */
    if (sTable[j].string == (char *)0) {
      (void)fprintf(stderr, "Warning: Unknown member `%s in\n",
		    XS(k->p[i])->n);
      (void)pa((V)a);
      fputc('\n', stdout);
    }
  } /* for i */
  *valuemaskp = valuemask;
  R 0;
}


void cInstall()
{
  CX saveCx=Cx;
  Cx=cx("c"); 

  install((PFI)structdef,            "structdef",            A_,3,A_,A_,A_, 0,0,0,0,0);
  install((PFI)structsize,           "structsize",           A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)structcreate,         "structcreate",         A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)structget,            "structget",            A_,3,A_,A_,A_, 0,0,0,0,0);
  install((PFI)structset,            "structset",            A_,4,A_,A_,A_,A_,0,0,0,0);
  install((PFI)pointer,              "pointer",              A_,2,A_,A_, 0, 0,0,0,0,0);
  install((PFI)ptr,                  "ptr",                  IV,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)struct_pointed_to_by, "struct_pointed_to_by", A_,2,A_,A_, 0, 0,0,0,0,0);
  install((PFI)double_pointed_to_by, "double_pointed_to_by", A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)float_pointed_to_by,  "float_pointed_to_by" , A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)char_pointed_to_by,   "char_pointed_to_by",   A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)int_pointed_to_by,    "int_pointed_to_by",    A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)short_pointed_to_by,  "short_pointed_to_by",  A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)string_pointed_to_by, "string_pointed_to_by", A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)structtype,            "structtype",           A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)structprint, 	      "structprint",          V_,2,A_,A_, 0, 0,0,0,0,0);
  install((PFI)place_ints_at, 	      "place_ints_at",        V_,2,IA,IV, 0, 0,0,0,0,0);
  install((PFI)place_floats_at,       "place_floats_at",      V_,2,FA,IV, 0, 0,0,0,0,0);
  install((PFI)place_chars_at, 	      "place_chars_at",       V_,2,CA,IV, 0, 0,0,0,0,0);
  install((PFI)stuff, 		      "stuff",                A_,1,A_, 0, 0, 0,0,0,0,0);
  install((PFI)unstuff, 	      "unstuff",              A_,1,CA, 0, 0, 0,0,0,0,0);
  install((PFI)AHeader, 	      "AHeader",              A_,1,A_, 0, 0, 0,0,0,0,0);
  Cx=saveCx;

  cformInstall();
  R;
}
