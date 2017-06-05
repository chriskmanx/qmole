#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#define PANGO_ENABLE_BACKEND /* to access PangoFcFont.font_pattern */

#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>
#include <zlib.h>
#include <string.h>
#include <locale.h>
#include <pango/pango.h>
#include <pango/pangofc-font.h>
#include <pango/pangoft2.h>
#include <fontconfig/fontconfig.h>
#include <ft2build.h>
#include FT_FREETYPE_H

#define NO_MAPPERS
#define NO_TYPE3
#define NO_TYPE42
#include "ttsubset/sft.h"

#include "xournal.h"
#include "xo-support.h"
#include "xo-misc.h"
#include "xo-paint.h"
#include "xo-print.h"
#include "xo-file.h"

#define RGBA_RED(rgba) (((rgba>>24)&0xff)/255.0)
#define RGBA_GREEN(rgba) (((rgba>>16)&0xff)/255.0)
#define RGBA_BLUE(rgba) (((rgba>>8)&0xff)/255.0)
#define RGBA_ALPHA(rgba) (((rgba>>0)&0xff)/255.0)
#define RGBA_RGB(rgba) RGBA_RED(rgba), RGBA_GREEN(rgba), RGBA_BLUE(rgba)

/*********** Printing to PDF ************/

gboolean ispdfspace(char c)
{
  return (c==0 || c==9 || c==10 || c==12 || c==13 || c==' ');
}

gboolean ispdfdelim(char c)
{
  return (c=='(' || c==')' || c=='<' || c=='>' || c=='[' || c==']' ||
          c=='{' || c=='}' || c=='/' || c=='%');
}

void skipspace(char **p, char *eof)
{
  while (ispdfspace(**p) || **p=='%') {
    if (**p=='%') while (*p!=eof && **p!=10 && **p!=13) (*p)++;
    if (*p==eof) return;
    (*p)++;
  }
}

void free_pdfobj(struct PdfObj *obj)
{
  int i;
  
  if (obj==NULL) return;
  if ((obj->type == PDFTYPE_STRING || obj->type == PDFTYPE_NAME ||
      obj->type == PDFTYPE_STREAM) && obj->str!=NULL)
    g_free(obj->str);
  if ((obj->type == PDFTYPE_ARRAY || obj->type == PDFTYPE_DICT ||
      obj->type == PDFTYPE_STREAM) && obj->num>0) {
    for (i=0; i<obj->num; i++)
      free_pdfobj(obj->elts[i]);
    g_free(obj->elts);
  }
  if ((obj->type == PDFTYPE_DICT || obj->type == PDFTYPE_STREAM) && obj->num>0) {
    for (i=0; i<obj->num; i++)
      g_free(obj->names[i]);
    g_free(obj->names);
  }
  g_free(obj);
}

struct PdfObj *dup_pdfobj(struct PdfObj *obj)
{
  struct PdfObj *dup;
  int i;
  
  if (obj==NULL) return NULL;
  dup = g_memdup(obj, sizeof(struct PdfObj));
  if ((obj->type == PDFTYPE_STRING || obj->type == PDFTYPE_NAME ||
      obj->type == PDFTYPE_STREAM) && obj->str!=NULL) {
    if (obj->type == PDFTYPE_NAME) obj->len = strlen(obj->str);
    dup->str = g_memdup(obj->str, obj->len+1);
  }
  if ((obj->type == PDFTYPE_ARRAY || obj->type == PDFTYPE_DICT ||
      obj->type == PDFTYPE_STREAM) && obj->num>0) {
    dup->elts = g_malloc(obj->num*sizeof(struct PdfObj *));
    for (i=0; i<obj->num; i++)
      dup->elts[i] = dup_pdfobj(obj->elts[i]);
  }
  if ((obj->type == PDFTYPE_DICT || obj->type == PDFTYPE_STREAM) && obj->num>0) {
    dup->names = g_malloc(obj->num*sizeof(char *));
    for (i=0; i<obj->num; i++)
      dup->names[i] = g_strdup(obj->names[i]);
  }
  return dup;
}

void show_pdfobj(struct PdfObj *obj, GString *str)
{
  int i;
  if (obj==NULL) return;
  switch(obj->type) {
    case PDFTYPE_CST:
      if (obj->intval==1) g_string_append(str, "true");
      if (obj->intval==0) g_string_append(str, "false");
      if (obj->intval==-1) g_string_append(str, "null");
      break;
    case PDFTYPE_INT:
      g_string_append_printf(str, "%d", obj->intval);
      break;
    case PDFTYPE_REAL:
      g_string_append_printf(str, "%f", obj->realval);
      break;
    case PDFTYPE_STRING:
      g_string_append_len(str, obj->str, obj->len);
      break;
    case PDFTYPE_NAME:
      g_string_append(str, obj->str);
      break;
    case PDFTYPE_ARRAY:
      g_string_append_c(str, '[');
      for (i=0;i<obj->num;i++) {
        if (i) g_string_append_c(str, ' ');
        show_pdfobj(obj->elts[i], str);
      }
      g_string_append_c(str, ']');
      break;
    case PDFTYPE_DICT:
      g_string_append(str, "<<");
      for (i=0;i<obj->num;i++) {
        g_string_append_printf(str, " %s ", obj->names[i]); 
        show_pdfobj(obj->elts[i], str);
      }
      g_string_append(str, " >>");
      break;
    case PDFTYPE_REF:
      g_string_append_printf(str, "%d %d R", obj->intval, obj->num);
      break;
  }
}

void DEBUG_PRINTOBJ(struct PdfObj *obj)
{
  GString *s = g_string_new("");
  show_pdfobj(obj, s);
  puts(s->str);
  g_string_free(s, TRUE);
}

// parse a PDF object; returns NULL if fails
// THIS PARSER DOES NOT RECOGNIZE STREAMS YET

struct PdfObj *parse_pdf_object(char **ptr, char *eof)
{
  struct PdfObj *obj, *elt;
  char *p, *q, *r, *eltname;
  int stack;

  obj = g_malloc(sizeof(struct PdfObj));
  p = *ptr;
  skipspace(&p, eof);
  if (p==eof) { g_free(obj); return NULL; }
  
  // maybe a constant
  if (!strncmp(p, "true", 4)) {
    obj->type = PDFTYPE_CST;
    obj->intval = 1;
    *ptr = p+4;
    return obj;
  }
  if (!strncmp(p, "false", 5)) {
    obj->type = PDFTYPE_CST;
    obj->intval = 0;
    *ptr = p+5;
    return obj;
  }
  if (!strncmp(p, "null", 4)) {
    obj->type = PDFTYPE_CST;
    obj->intval = -1;
    *ptr = p+4;
    return obj;
  }

  // or a number ?
  obj->intval = strtol(p, &q, 10);
  *ptr = q;
  if (q!=p) {
    if (*q == '.') {
      obj->type = PDFTYPE_REAL;
      obj->realval = g_ascii_strtod(p, ptr);
      return obj;
    }
    if (ispdfspace(*q)) {
      // check for indirect reference
      skipspace(&q, eof);
      obj->num = strtol(q, &r, 10);
      if (r!=q) {
        skipspace(&r, eof);
        if (*r=='R') {
          *ptr = r+1;
          obj->type = PDFTYPE_REF;
          return obj;
        }
      }
    }
    obj->type = PDFTYPE_INT;
    return obj;
  }

  // a string ?
  if (*p=='(') {
    q=p+1; stack=1;
    while (stack>0 && q!=eof) {
      if (*q=='(') stack++;
      if (*q==')') stack--;
      if (*q=='\\') q++;
      if (q!=eof) q++;
    }
    if (q==eof) { g_free(obj); return NULL; }
    obj->type = PDFTYPE_STRING;
    obj->len = q-p;
    obj->str = g_malloc(obj->len+1);
    obj->str[obj->len] = 0;
    g_memmove(obj->str, p, obj->len);
    *ptr = q;
    return obj;
  }  
  if (*p=='<' && p[1]!='<') {
    q=p+1;
    while (*q!='>' && q!=eof) q++;
    if (q==eof) { g_free(obj); return NULL; }
    q++;
    obj->type = PDFTYPE_STRING;
    obj->len = q-p;
    obj->str = g_malloc(obj->len+1);
    obj->str[obj->len] = 0;
    g_memmove(obj->str, p, obj->len);
    *ptr = q;
    return obj;
  }
  
  // a name ?
  if (*p=='/') {
    q=p+1;
    while (!ispdfspace(*q) && !ispdfdelim(*q)) q++;
    obj->type = PDFTYPE_NAME;
    obj->str = g_strndup(p, q-p);
    *ptr = q;
    return obj;
  }

  // an array ?
  if (*p=='[') {
    obj->type = PDFTYPE_ARRAY;
    obj->num = 0;
    obj->elts = NULL;
    q=p+1; skipspace(&q, eof);
    while (*q!=']') {
      elt = parse_pdf_object(&q, eof);
      if (elt==NULL) { free_pdfobj(obj); return NULL; }
      obj->num++;
      obj->elts = g_realloc(obj->elts, obj->num*sizeof(struct PdfObj *));
      obj->elts[obj->num-1] = elt;
      skipspace(&q, eof);
    }
    *ptr = q+1;
    return obj;
  }

  // a dictionary ?
  if (*p=='<' && p[1]=='<') {
    obj->type = PDFTYPE_DICT;
    obj->num = 0;
    obj->elts = NULL;
    obj->names = NULL;
    q=p+2; skipspace(&q, eof);
    while (*q!='>' || q[1]!='>') {
      if (*q!='/') { free_pdfobj(obj); return NULL; }
      r=q+1;
      while (!ispdfspace(*r) && !ispdfdelim(*r)) r++;
      eltname = g_strndup(q, r-q);
      q=r; skipspace(&q, eof);
      elt = parse_pdf_object(&q, eof);
      if (elt==NULL) { g_free(eltname); free_pdfobj(obj); return NULL; }
      obj->num++;
      obj->elts = g_realloc(obj->elts, obj->num*sizeof(struct PdfObj *));
      obj->names = g_realloc(obj->names, obj->num*sizeof(char *));
      obj->elts[obj->num-1] = elt;
      obj->names[obj->num-1] = eltname;
      skipspace(&q, eof);
    }
    *ptr = q+2;
    return obj;
  }

  // DOES NOT RECOGNIZE STREAMS YET (handle as subcase of dictionary)
  
  g_free(obj);
  return NULL;
}

struct PdfObj *get_dict_entry(struct PdfObj *dict, char *name)
{
  int i;
  
  if (dict==NULL) return NULL;
  if (dict->type != PDFTYPE_DICT) return NULL;
  for (i=0; i<dict->num; i++) 
    if (!strcmp(dict->names[i], name)) return dict->elts[i];
  return NULL;
}

struct PdfObj *get_pdfobj(GString *pdfbuf, struct XrefTable *xref, struct PdfObj *obj)
{
  char *p, *eof;
  int offs, n;

  if (obj==NULL) return NULL;
  if (obj->type!=PDFTYPE_REF) return dup_pdfobj(obj);
  if (obj->intval>xref->last) return NULL;
  offs = xref->data[obj->intval];
  if (offs<=0 || offs >= pdfbuf->len) return NULL;

  p = pdfbuf->str + offs;
  eof = pdfbuf->str + pdfbuf->len;
  n = strtol(p, &p, 10);
  if (n!=obj->intval) return NULL;
  skipspace(&p, eof);
  n = strtol(p, &p, 10);
  skipspace(&p, eof);
  if (strncmp(p, "obj", 3)) return NULL;
  p+=3;
  return parse_pdf_object(&p, eof);
}

// read the xref table of a PDF file in memory, and return the trailerdict

struct PdfObj *parse_xref_table(GString *pdfbuf, struct XrefTable *xref, int offs)
{
  char *p, *eof;
  struct PdfObj *trailerdict, *obj;
  int start, len, i;
  
  if (strncmp(pdfbuf->str+offs, "xref", 4)) return NULL;
  p = strstr(pdfbuf->str+offs, "trailer");
  eof = pdfbuf->str + pdfbuf->len;
  if (p==NULL) return NULL;
  p+=8;
  trailerdict = parse_pdf_object(&p, eof);
  obj = get_dict_entry(trailerdict, "/Size");
  if (obj!=NULL && obj->type == PDFTYPE_INT && obj->intval-1>xref->last)
    make_xref(xref, obj->intval-1, 0);
  obj = get_dict_entry(trailerdict, "/Prev");
  if (obj!=NULL && obj->type == PDFTYPE_INT && obj->intval>0 && obj->intval!=offs) {
    // recurse into older xref table
    obj = parse_xref_table(pdfbuf, xref, obj->intval);
    free_pdfobj(obj);
  }
  p = pdfbuf->str+offs+4;
  skipspace(&p, eof);
  if (*p<'0' || *p>'9') { free_pdfobj(trailerdict); return NULL; }
  while (*p>='0' && *p<='9') {
    start = strtol(p, &p, 10);
    skipspace(&p, eof);
    len = strtol(p, &p, 10);
    skipspace(&p, eof);
    if (len <= 0 || 20*len > eof-p) break;
    if (start+len-1 > xref->last) make_xref(xref, start+len-1, 0);
    for (i=start; i<start+len; i++) {
      xref->data[i] = strtol(p, NULL, 10);
      p+=20;
    }
    skipspace(&p, eof);
  }
  if (*p!='t') { free_pdfobj(trailerdict); return NULL; }
  return trailerdict;
}

// parse the page tree

int pdf_getpageinfo(GString *pdfbuf, struct XrefTable *xref, 
                struct PdfObj *pgtree, int nmax, struct PdfPageDesc *pages)
{
  struct PdfObj *obj, *kid;
  int i, count, j;
  
  obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/Type"));
  if (obj == NULL || obj->type != PDFTYPE_NAME)
    return 0;
  if (!strcmp(obj->str, "/Page")) {
    free_pdfobj(obj);
    pages->contents = dup_pdfobj(get_dict_entry(pgtree, "/Contents"));
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/Resources"));
    if (obj!=NULL) {
      free_pdfobj(pages->resources);
      pages->resources = obj;
    }
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/MediaBox"));
    if (obj!=NULL) {
      free_pdfobj(pages->mediabox);
      pages->mediabox = obj;
    }
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/Rotate"));
    if (obj!=NULL && obj->type == PDFTYPE_INT)
      pages->rotate = obj->intval;
    free_pdfobj(obj);
    return 1;
  }
  else if (!strcmp(obj->str, "/Pages")) {
    free_pdfobj(obj);
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/Count"));
    if (obj!=NULL && obj->type == PDFTYPE_INT && 
        obj->intval>0 && obj->intval<=nmax) count = obj->intval;
    else count = 0;
    free_pdfobj(obj);
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/Resources"));
    if (obj!=NULL)
      for (i=0; i<count; i++) {
        free_pdfobj(pages[i].resources);
        pages[i].resources = dup_pdfobj(obj);
      }
    free_pdfobj(obj);
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/MediaBox"));
    if (obj!=NULL)
      for (i=0; i<count; i++) {
        free_pdfobj(pages[i].mediabox);
        pages[i].mediabox = dup_pdfobj(obj);
      }
    free_pdfobj(obj);
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/Rotate"));
    if (obj!=NULL && obj->type == PDFTYPE_INT)
      for (i=0; i<count; i++)
        pages[i].rotate = obj->intval;
    free_pdfobj(obj);
    obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pgtree, "/Kids"));
    if (obj!=NULL && obj->type == PDFTYPE_ARRAY) {
      for (i=0; i<obj->num; i++) {
        kid = get_pdfobj(pdfbuf, xref, obj->elts[i]);
        if (kid!=NULL) {
          j = pdf_getpageinfo(pdfbuf, xref, kid, nmax, pages);
          nmax -= j;
          pages += j;
          free_pdfobj(kid);
        }
      }
    }
    free_pdfobj(obj);
    return count;
  }
  return 0;
}

// parse a PDF file in memory

gboolean pdf_parse_info(GString *pdfbuf, struct PdfInfo *pdfinfo, struct XrefTable *xref)
{
  char *p;
  int offs;
  struct PdfObj *obj, *pages;

  xref->n_alloc = xref->last = 0;
  xref->data = NULL;
  p = pdfbuf->str + pdfbuf->len-1;
  
  while (*p!='s' && p!=pdfbuf->str) p--;
  if (strncmp(p, "startxref", 9)) return FALSE; // fail
  p+=9;
  while (ispdfspace(*p) && p!=pdfbuf->str+pdfbuf->len) p++;
  offs = strtol(p, NULL, 10);
  if (offs <= 0 || offs > pdfbuf->len) return FALSE; // fail
  pdfinfo->startxref = offs;
  
  pdfinfo->trailerdict = parse_xref_table(pdfbuf, xref, offs);
  if (pdfinfo->trailerdict == NULL) return FALSE; // fail
  
  obj = get_pdfobj(pdfbuf, xref,
     get_dict_entry(pdfinfo->trailerdict, "/Root"));
  if (obj == NULL)
    { free_pdfobj(pdfinfo->trailerdict); return FALSE; }
  pages = get_pdfobj(pdfbuf, xref, get_dict_entry(obj, "/Pages"));
  free_pdfobj(obj);
  if (pages == NULL)
    { free_pdfobj(pdfinfo->trailerdict); return FALSE; }
  obj = get_pdfobj(pdfbuf, xref, get_dict_entry(pages, "/Count"));
  if (obj == NULL || obj->type != PDFTYPE_INT || obj->intval<=0) 
    { free_pdfobj(pdfinfo->trailerdict); free_pdfobj(pages); 
      free_pdfobj(obj); return FALSE; }
  pdfinfo->npages = obj->intval;
  free_pdfobj(obj);
  
  pdfinfo->pages = g_malloc0(pdfinfo->npages*sizeof(struct PdfPageDesc));
  pdf_getpageinfo(pdfbuf, xref, pages, pdfinfo->npages, pdfinfo->pages);
  free_pdfobj(pages);
  
  return TRUE;
}

// add an entry to the xref table

void make_xref(struct XrefTable *xref, int nobj, int offset)
{
  if (xref->n_alloc <= nobj) {
    xref->n_alloc = nobj + 10;
    xref->data = g_realloc(xref->data, xref->n_alloc*sizeof(int));
  }
  if (xref->last < nobj) xref->last = nobj;
  xref->data[nobj] = offset;
}

// a wrapper for deflate

GString *do_deflate(char *in, int len)
{
  GString *out;
  z_stream zs;
  
  zs.zalloc = Z_NULL;
  zs.zfree = Z_NULL;
  deflateInit(&zs, Z_DEFAULT_COMPRESSION);
  zs.next_in = (Bytef *)in;
  zs.avail_in = len;
  zs.avail_out = deflateBound(&zs, len);
  out = g_string_sized_new(zs.avail_out);
  zs.next_out = (Bytef *)out->str;
  deflate(&zs, Z_FINISH);
  out->len = zs.total_out;
  deflateEnd(&zs);
  return out;
}

// prefix to scale the original page

GString *make_pdfprefix(struct PdfPageDesc *pgdesc, double width, double height)
{
  GString *str;
  double v[4], t, xscl, yscl;
  int i;

  // push 3 times in case code to be annotated has unbalanced q/Q (B of A., ...)
  str = g_string_new("q q q "); 
  if (pgdesc->rotate == 90) {
    g_string_append_printf(str, "0 -1 1 0 0 %.2f cm ", height);
    t = height; height = width; width = t;
  }
  if (pgdesc->rotate == 270) {
    g_string_append_printf(str, "0 1 -1 0 %.2f 0 cm ", width);
    t = height; height = width; width = t;
  }
  if (pgdesc->rotate == 180) {
    g_string_append_printf(str, "-1 0 0 -1 %.2f %.2f cm ", width, height);
  }
  if (pgdesc->mediabox==NULL || pgdesc->mediabox->type != PDFTYPE_ARRAY ||
      pgdesc->mediabox->num != 4) return str;
  for (i=0; i<4; i++) {
    if (pgdesc->mediabox->elts[i]->type == PDFTYPE_INT)
      v[i] = pgdesc->mediabox->elts[i]->intval;
    else if (pgdesc->mediabox->elts[i]->type == PDFTYPE_REAL)
      v[i] = pgdesc->mediabox->elts[i]->realval;
    else return str;
  }
  if (v[0]>v[2]) { t = v[0]; v[0] = v[2]; v[2] = t; }
  if (v[1]>v[3]) { t = v[1]; v[1] = v[3]; v[3] = t; }
  if (v[2]-v[0] < 1. || v[3]-v[1] < 1.) return str;
  xscl = width/(v[2]-v[0]);
  yscl = height/(v[3]-v[1]);
  g_string_append_printf(str, "%.4f 0 0 %.4f %.2f %.2f cm ",
    xscl, yscl, -v[0]*xscl, -v[1]*yscl);
  return str;
}

// add an entry to a subentry of a directory

struct PdfObj *mk_pdfname(char *name)
{
  struct PdfObj *obj;
  
  obj = g_malloc(sizeof(struct PdfObj));
  obj->type = PDFTYPE_NAME;
  obj->str = g_strdup(name);
  return obj;
}

struct PdfObj *mk_pdfref(int num)
{
  struct PdfObj *obj;
  
  obj = g_malloc(sizeof(struct PdfObj));
  obj->type = PDFTYPE_REF;
  obj->intval = num;
  obj->num = 0;
  return obj;
}

gboolean iseq_obj(struct PdfObj *a, struct PdfObj *b)
{
  if (a==NULL || b==NULL) return (a==b);
  if (a->type!=b->type) return FALSE;
  if (a->type == PDFTYPE_CST || a->type == PDFTYPE_INT)
    return (a->intval == b->intval);
  if (a->type == PDFTYPE_REAL)
    return (a->realval == b->realval);
  if (a->type == PDFTYPE_NAME)
    return !strcmp(a->str, b->str);
  if (a->type == PDFTYPE_REF)
    return (a->intval == b->intval && a->num == b->num);
  return FALSE;
}

void add_dict_subentry(GString *pdfbuf, struct XrefTable *xref,
   struct PdfObj *obj, char *section, int type, char *name, struct PdfObj *entry)
{
  struct PdfObj *sec;
  int i, subpos;
  
  subpos = -1;
  for (i=0; i<obj->num; i++) 
    if (!strcmp(obj->names[i], section)) subpos = i;
  if (subpos == -1) {
    subpos = obj->num;
    obj->num++;
    obj->elts = g_realloc(obj->elts, obj->num*sizeof(struct PdfObj*));
    obj->names = g_realloc(obj->names, obj->num*sizeof(char *));
    obj->names[subpos] = g_strdup(section);
    obj->elts[subpos] = NULL;
  }
  if (obj->elts[subpos]!=NULL && obj->elts[subpos]->type==PDFTYPE_REF) {
    sec = get_pdfobj(pdfbuf, xref, obj->elts[subpos]);
    free_pdfobj(obj->elts[subpos]);
    obj->elts[subpos] = sec;
  }
  if (obj->elts[subpos]!=NULL && obj->elts[subpos]->type!=type)
    { free_pdfobj(obj->elts[subpos]); obj->elts[subpos] = NULL; }
  if (obj->elts[subpos] == NULL) {
    obj->elts[subpos] = sec = g_malloc(sizeof(struct PdfObj));
    sec->type = type;
    sec->num = 0;
    sec->elts = NULL;
    sec->names = NULL;
  }
  sec = obj->elts[subpos];

  subpos = -1;
  if (type==PDFTYPE_DICT) {
    for (i=0; i<sec->num; i++) 
      if (!strcmp(sec->names[i], name)) subpos = i;
    if (subpos == -1) {
      subpos = sec->num;
      sec->num++;
      sec->elts = g_realloc(sec->elts, sec->num*sizeof(struct PdfObj*));
      sec->names = g_realloc(sec->names, sec->num*sizeof(char *));
      sec->names[subpos] = g_strdup(name);
      sec->elts[subpos] = NULL;
    }
    free_pdfobj(sec->elts[subpos]);
    sec->elts[subpos] = entry;
  } 
  if (type==PDFTYPE_ARRAY) {
    for (i=0; i<sec->num; i++)
      if (iseq_obj(sec->elts[i], entry)) subpos = i;
    if (subpos == -1) {
      subpos = sec->num;
      sec->num++;
      sec->elts = g_realloc(sec->elts, sec->num*sizeof(struct PdfObj*));
      sec->elts[subpos] = entry;
    }
    else free_pdfobj(entry);
  }
}

// draw a page's background

void pdf_draw_solid_background(struct Page *pg, GString *str)
{
  double x, y;

  g_string_append_printf(str, 
    "%.2f %.2f %.2f rg 0 0 %.2f %.2f re f ",
    RGBA_RGB(pg->bg->color_rgba), pg->width, pg->height);
  if (!ui.print_ruling) return;
  if (pg->bg->ruling == RULING_NONE) return;
  g_string_append_printf(str,
    "%.2f %.2f %.2f RG %.2f w ",
    RGBA_RGB(RULING_COLOR), RULING_THICKNESS);
  if (pg->bg->ruling == RULING_GRAPH) {
    for (x=RULING_GRAPHSPACING; x<pg->width-1; x+=RULING_GRAPHSPACING)
      g_string_append_printf(str, "%.2f 0 m %.2f %.2f l S ",
        x, x, pg->height);
    for (y=RULING_GRAPHSPACING; y<pg->height-1; y+=RULING_GRAPHSPACING)
      g_string_append_printf(str, "0 %.2f m %.2f %.2f l S ",
        y, pg->width, y);
    return;
  }
  for (y=RULING_TOPMARGIN; y<pg->height-1; y+=RULING_SPACING)
    g_string_append_printf(str, "0 %.2f m %.2f %.2f l S ",
      y, pg->width, y);
  if (pg->bg->ruling == RULING_LINED)
    g_string_append_printf(str, 
      "%.2f %.2f %.2f RG %.2f 0 m %.2f %.2f l S ",
      RGBA_RGB(RULING_MARGIN_COLOR), 
      RULING_LEFTMARGIN, RULING_LEFTMARGIN, pg->height);
}

int pdf_draw_bitmap_background(struct Page *pg, GString *str, 
                                struct XrefTable *xref, GString *pdfbuf)
{
  BgPdfPage *pgpdf;
  GdkPixbuf *pix;
  GString *zpix;
  PopplerPage *pdfpage;
  char *buf, *p1, *p2;
  int height, width, stride, x, y, chan;
  double pgheight, pgwidth;
  
  if (pg->bg->type == BG_PDF) {
    if (!bgpdf.document) return -1;
    pdfpage = poppler_document_get_page(bgpdf.document, pg->bg->file_page_seq-1);
    if (!pdfpage) return -1;
    poppler_page_get_size(pdfpage, &pgwidth, &pgheight);
    width = (int) (PDFTOPPM_PRINTING_DPI * pgwidth/72.0);
    height = (int) (PDFTOPPM_PRINTING_DPI * pgheight/72.0);
    pix = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
    poppler_page_render_to_pixbuf(
       pdfpage, 0, 0, width, height, PDFTOPPM_PRINTING_DPI/72.0, 0, pix);
    g_object_unref(pdfpage);
  }
  else pix = g_object_ref(pg->bg->pixbuf);
  
  if (gdk_pixbuf_get_bits_per_sample(pix) != 8 ||
      gdk_pixbuf_get_colorspace(pix) != GDK_COLORSPACE_RGB)
    { g_object_unref(pix); return -1; }
                    
  width = gdk_pixbuf_get_width(pix);
  height = gdk_pixbuf_get_height(pix);
  stride = gdk_pixbuf_get_rowstride(pix);
  chan = gdk_pixbuf_get_n_channels(pix);
  if (chan!=3 && chan!=4) { g_object_unref(pix); return -1; }

  g_string_append_printf(str, "q %.2f 0 0 %.2f 0 %.2f cm /ImBg Do Q ",
    pg->width, -pg->height, pg->height);
  
  p2 = buf = (char *)g_malloc(3*width*height);
  for (y=0; y<height; y++) {
    p1 = (char *)gdk_pixbuf_get_pixels(pix)+stride*y;
    for (x=0; x<width; x++) {
      *(p2++)=*(p1++); *(p2++)=*(p1++); *(p2++)=*(p1++);
      if (chan==4) p1++;
    }
  }
  zpix = do_deflate(buf, 3*width*height);
  g_free(buf);
  g_object_unref(pix);

  make_xref(xref, xref->last+1, pdfbuf->len);
  g_string_append_printf(pdfbuf, 
    "%d 0 obj\n<< /Length %d /Filter /FlateDecode /Type /Xobject "
    "/Subtype /Image /Width %d /Height %d /ColorSpace /DeviceRGB "
    "/BitsPerComponent 8 >> stream\n",
    xref->last, zpix->len, width, height);
  g_string_append_len(pdfbuf, zpix->str, zpix->len);
  g_string_free(zpix, TRUE);
  g_string_append(pdfbuf, "endstream\nendobj\n");
 
  return xref->last;
}

// manipulate Pdf fonts

struct PdfFont *new_pdffont(struct XrefTable *xref, GList **fonts,
   char *filename, int font_id, FT_Face face, int glyph_page)
{
  GList *list;
  struct PdfFont *font;
  int i;
  const char *s;
  
  for (list = *fonts; list!=NULL; list = list->next) {
    font = (struct PdfFont *)list->data;
    if (!strcmp(font->filename, filename) && font->font_id == font_id 
        && font->glyph_page == glyph_page)
          { font->used_in_this_page = TRUE; return font; }
  }
  font = g_malloc(sizeof(struct PdfFont));
  *fonts = g_list_append(*fonts, font);
  font->n_obj = xref->last+1;
  make_xref(xref, xref->last+1, 0); // will give it a value later
  font->filename = g_strdup(filename);
  font->font_id = font_id;
  font->glyph_page = glyph_page;
  font->used_in_this_page = TRUE;
  font->num_glyphs_used = 0;
  for (i=0; i<256; i++) {
    font->glyphmap[i] = -1;
    font->advance[i] = 0;
    font->glyphpsnames[i] = NULL;
  }
  font->glyphmap[0] = 0;
  // fill in info from the FT_Face
  font->is_truetype = FT_IS_SFNT(face);
  font->nglyphs = face->num_glyphs;
  font->ft2ps = 1000.0 / face->units_per_EM;
  font->ascender = (int)(font->ft2ps*face->ascender);
  font->descender = (int)(font->ft2ps*face->descender);
  if (face->bbox.xMin < -100000 || face->bbox.xMin > 100000) font->xmin = 0;
  else font->xmin = (int)(font->ft2ps*face->bbox.xMin);
  if (face->bbox.xMax < -100000 || face->bbox.xMax > 100000) font->xmax = 0;
  else font->xmax = (int)(font->ft2ps*face->bbox.xMax);
  if (face->bbox.yMin < -100000 || face->bbox.yMin > 100000) font->ymin = 0;
  else font->ymin = (int)(font->ft2ps*face->bbox.yMin);
  if (face->bbox.yMax < -100000 || face->bbox.yMax > 100000) font->ymax = 0;
  else font->ymax = (int)(font->ft2ps*face->bbox.yMax);
  if (font->is_truetype) font->flags = 4; // symbolic
  else {
    font->flags = 4; // symbolic
    if (FT_IS_FIXED_WIDTH(face)) font->flags |= 1;
    if (face->style_flags & FT_STYLE_FLAG_ITALIC) font->flags |= 64;
  }
  s = FT_Get_Postscript_Name(face);
  if (s==NULL) s = "Noname";
  if (glyph_page) font->fontname = g_strdup_printf("%s_%03d", s, glyph_page);
  else font->fontname = g_strdup(s);
  return font;
}

#define pfb_get_length(x) (((x)[3]<<24) + ((x)[2]<<16) + ((x)[1]<<8) + (x)[0])
#define T1_SEGMENT_1_END "currentfile eexec"
#define T1_SEGMENT_3_END "cleartomark"

void embed_pdffont(GString *pdfbuf, struct XrefTable *xref, struct PdfFont *font)
{
  // this code inspired by libgnomeprint
  gboolean fallback, is_binary;
  guchar encoding[256];
  gushort glyphs[256];
  int i, j, num, len1, len2;
  gsize len;
  TrueTypeFont *ttfnt;
  char *seg1, *seg2;
  char *fontdata, *p;
  char prefix[8];
  int nobj_fontprog, nobj_descr, lastchar;
  
  fallback = FALSE;
  // embed the font file: TrueType case
  if (font->is_truetype) {
    glyphs[0] = encoding[0] = 0;
    num = 1;
    for (i=1; i<=255; i++) 
      if (font->glyphmap[i]>=0) {
        font->glyphmap[i] = num;
        glyphs[num] = 255*font->glyph_page+i-1;
        encoding[num] = i;
        num++;
      }
    font->num_glyphs_used = num-1;
    if (OpenTTFont(font->filename, 0, &ttfnt) == SF_OK) {
      if (CreateTTFromTTGlyphs_tomemory(ttfnt, (guint8**)&fontdata, &len, glyphs, encoding, num, 
                   0, NULL, TTCF_AutoName | TTCF_IncludeOS2) == SF_OK) {
        make_xref(xref, xref->last+1, pdfbuf->len);
        nobj_fontprog = xref->last;
        g_string_append_printf(pdfbuf, 
          "%d 0 obj\n<< /Length %d /Length1 %d >> stream\n",
          nobj_fontprog, (int)len, (int)len);
        g_string_append_len(pdfbuf, fontdata, len);
        g_string_append(pdfbuf, "endstream\nendobj\n");
        g_free(fontdata);
      }
      else fallback = TRUE;
      CloseTTFont(ttfnt);
    } 
    else fallback = TRUE;
  } else {
  // embed the font file: Type1 case
    if (g_file_get_contents(font->filename, &fontdata, &len, NULL) && len>=8) {
      if (fontdata[0]==(char)0x80 && fontdata[1]==(char)0x01) {
        is_binary = TRUE;
        len1 = pfb_get_length((unsigned char *)fontdata+2);
        if (fontdata[len1+6]!=(char)0x80 || fontdata[len1+7]!=(char)0x02) fallback = TRUE;
        else {
          len2 = pfb_get_length((unsigned char *)fontdata+len1+8);
          if (fontdata[len1+len2+12]!=(char)0x80 || fontdata[len1+len2+13]!=(char)0x01)
            fallback = TRUE;
        }
      }
      else if (!strncmp(fontdata, "%!PS", 4)) {
        is_binary = FALSE;
        p = strstr(fontdata, T1_SEGMENT_1_END) + strlen(T1_SEGMENT_1_END);
        if (p==NULL) fallback = TRUE;
        else {
          if (*p=='\n' || *p=='\r') p++;
          if (*p=='\n' || *p=='\r') p++;
          len1 = p-fontdata;
          p = g_strrstr_len(fontdata, len, T1_SEGMENT_3_END);
          if (p==NULL) fallback = TRUE;
          else {
            // rewind 512 zeros
            i = 512; p--;
            while (i>0 && p!=fontdata && (*p=='0' || *p=='\r' || *p=='\n')) {
              if (*p=='0') i--;
              p--;
            }
            while (p!=fontdata && (*p=='\r' || *p=='\n')) p--;
            p++;
            if (i>0) fallback = TRUE;
            else len2 = p-fontdata-len1;
          }
        }
      }
      else fallback = TRUE;
      if (!fallback) {
        if (is_binary) {
          seg1 = fontdata+6;
          seg2 = fontdata + len1 + 12;
        } else {
          seg1 = fontdata;
          seg2 = g_malloc(len2/2);
          j=0;
          p = fontdata+len1;
          while (p+1 < fontdata+len1+len2) {
            if (*p==' '||*p=='\t'||*p=='\n'||*p=='\r') { p++; continue; }
            if (p[0]>'9') { p[0]|=0x20; p[0]-=39; }
            if (p[1]>'9') { p[1]|=0x20; p[1]-=39; }
            seg2[j++] = ((p[0]-'0')<<4) + (p[1]-'0');
            p+=2;
          }
          len2 = j;
        }
        make_xref(xref, xref->last+1, pdfbuf->len);
        nobj_fontprog = xref->last;
        g_string_append_printf(pdfbuf, 
          "%d 0 obj\n<< /Length %d /Length1 %d /Length2 %d /Length3 0 >> stream\n",
          nobj_fontprog, len1+len2, len1, len2);
        g_string_append_len(pdfbuf, seg1, len1);
        g_string_append_len(pdfbuf, seg2, len2);
        g_string_append(pdfbuf, "endstream\nendobj\n");
        if (!is_binary) g_free(seg2);
      }
      g_free(fontdata);
    }
    else fallback = TRUE;
  }
  
  // next, the font descriptor
  if (!fallback) {
    make_xref(xref, xref->last+1, pdfbuf->len);
    nobj_descr = xref->last;
    g_string_append_printf(pdfbuf,
      "%d 0 obj\n<< /Type /FontDescriptor /FontName /%s /Flags %d "
      "/FontBBox [%d %d %d %d] /ItalicAngle 0 /Ascent %d "
      "/Descent %d /CapHeight %d /StemV 100 /%s %d 0 R >> endobj\n",
      nobj_descr, font->fontname, font->flags, 
      font->xmin, font->ymin, font->xmax, font->ymax, 
      font->ascender, -font->descender, font->ascender, 
      font->is_truetype ? "FontFile2":"FontFile",
      nobj_fontprog);
  }
  
  // finally, the font itself
  /* Note: in Type1 case, font->glyphmap maps charcodes to glyph no's
     in TrueType case, encoding lists the used charcodes by index,
                       glyphs   list the used glyph no's by index
                       font->glyphmap maps charcodes to indices        */
  xref->data[font->n_obj] = pdfbuf->len;
  if (font->is_truetype) lastchar = encoding[font->num_glyphs_used];
  else lastchar = font->num_glyphs_used;
  if (fallback) {
    font->is_truetype = FALSE;
    g_free(font->fontname);
    font->fontname = g_strdup("Helvetica");
  }
  prefix[0]=0;
  if (font->is_truetype) {
    num = font->glyph_page;
    for (i=0; i<6; i++) { prefix[i] = 'A'+(num%26); num/=26; }
    prefix[6]='+'; prefix[7]=0;
  }
  g_string_append_printf(pdfbuf,
    "%d 0 obj\n<< /Type /Font /Subtype /%s /BaseFont /%s%s /Name /F%d ",
    font->n_obj, font->is_truetype?"TrueType":"Type1",
    prefix, font->fontname, font->n_obj);
  if (!fallback) {
    g_string_append_printf(pdfbuf,
      "/FontDescriptor %d 0 R /FirstChar 0 /LastChar %d /Widths [",
      nobj_descr, lastchar);
    for (i=0; i<=lastchar; i++)
      g_string_append_printf(pdfbuf, "%d ", font->advance[i]);
    g_string_append(pdfbuf, "] ");
  }
  if (!font->is_truetype) { /* encoding */
    g_string_append(pdfbuf, "/Encoding << /Type /Encoding "
      "/BaseEncoding /MacRomanEncoding /Differences [1 ");
    for (i=1; i<=lastchar; i++) {
      g_string_append_printf(pdfbuf, "/%s ", font->glyphpsnames[i]);
      g_free(font->glyphpsnames[i]);
    }
    g_string_append(pdfbuf, "] >> ");
  }
  g_string_append(pdfbuf, ">> endobj\n");
}

// draw a page's graphics

void pdf_draw_page(struct Page *pg, GString *str, gboolean *use_hiliter, 
                  struct XrefTable *xref, GList **pdffonts)
{
  GList *layerlist, *itemlist, *tmplist;
  struct Layer *l;
  struct Item *item;
  guint old_rgba, old_text_rgba;
  double old_thickness;
  double *pt;
  int i, j;
  PangoFontDescription *font_desc;
  PangoContext *context;
  PangoLayout *layout;
  PangoLayoutIter *iter;
  PangoRectangle logical_rect;
  PangoLayoutRun *run;
  PangoFcFont *fcfont;
  PangoFontMap *fontmap;
  FcPattern *pattern;
  int baseline, advance;
  int glyph_no, glyph_page, current_page;
  char *filename;
  char tmpstr[200];
  int font_id;
  FT_Face ftface;
  struct PdfFont *cur_font;
  gboolean in_string;
  
  old_rgba = old_text_rgba = 0x12345678;    // not any values we use, so we'll reset them
  old_thickness = 0.0;
  for (tmplist = *pdffonts; tmplist!=NULL; tmplist = tmplist->next) {
    cur_font = (struct PdfFont *)tmplist->data;
    cur_font->used_in_this_page = FALSE;
  }

  for (layerlist = pg->layers; layerlist!=NULL; layerlist = layerlist->next) {
    l = (struct Layer *)layerlist->data;
    for (itemlist = l->items; itemlist!=NULL; itemlist = itemlist->next) {
      item = (struct Item *)itemlist->data;
      if (item->type == ITEM_STROKE) {
        if ((item->brush.color_rgba & ~0xff) != old_rgba)
          g_string_append_printf(str, "%.2f %.2f %.2f RG ",
            RGBA_RGB(item->brush.color_rgba));
        if (item->brush.thickness != old_thickness)
          g_string_append_printf(str, "%.2f w ", item->brush.thickness);
        if ((item->brush.color_rgba & 0xf0) != 0xf0) { // transparent
          g_string_append(str, "q /XoHi gs ");
          *use_hiliter = TRUE;
        }
        old_rgba = item->brush.color_rgba & ~0xff;
        old_thickness = item->brush.thickness;
        pt = item->path->coords;
        if (!item->brush.variable_width) {
          g_string_append_printf(str, "%.2f %.2f m ", pt[0], pt[1]);
          for (i=1, pt+=2; i<item->path->num_points; i++, pt+=2)
            g_string_append_printf(str, "%.2f %.2f l ", pt[0], pt[1]);
          g_string_append_printf(str,"S\n");
          old_thickness = item->brush.thickness;
        } else {
          for (i=0; i<item->path->num_points-1; i++, pt+=2)
            g_string_append_printf(str, "%.2f w %.2f %.2f m %.2f %.2f l S\n", 
               item->widths[i], pt[0], pt[1], pt[2], pt[3]);
          old_thickness = 0.0;
        }
        if ((item->brush.color_rgba & 0xf0) != 0xf0) // undo transparent
          g_string_append(str, "Q ");
      }
      else if (item->type == ITEM_TEXT) {
        if ((item->brush.color_rgba & ~0xff) != old_text_rgba)
          g_string_append_printf(str, "%.2f %.2f %.2f rg ",
            RGBA_RGB(item->brush.color_rgba));
        old_text_rgba = item->brush.color_rgba & ~0xff;
        fontmap = pango_ft2_font_map_new();
        pango_ft2_font_map_set_resolution(PANGO_FT2_FONT_MAP (fontmap), 72, 72);
        context = pango_context_new();
        pango_context_set_font_map(context, fontmap);
        layout = pango_layout_new(context);
        g_object_unref(fontmap);
        g_object_unref(context);
        font_desc = pango_font_description_from_string(item->font_name);
        pango_font_description_set_absolute_size(font_desc,
          item->font_size*PANGO_SCALE);
        pango_layout_set_font_description(layout, font_desc);
        pango_font_description_free(font_desc);
        pango_layout_set_text(layout, item->text, -1);
        // this code inspired by the code in libgnomeprint
        iter = pango_layout_get_iter(layout);
        do {
          run = pango_layout_iter_get_run(iter);
          if (run==NULL) continue;
          pango_layout_iter_get_run_extents (iter, NULL, &logical_rect);
          baseline = pango_layout_iter_get_baseline (iter);
          if (!PANGO_IS_FC_FONT(run->item->analysis.font)) continue;
          fcfont = PANGO_FC_FONT(run->item->analysis.font);
          pattern = fcfont->font_pattern;
          if (FcPatternGetString(pattern, FC_FILE, 0, (unsigned char **)&filename) != FcResultMatch ||
              FcPatternGetInteger(pattern, FC_INDEX, 0, &font_id) != FcResultMatch)
                continue;
          ftface = pango_fc_font_lock_face(fcfont);
          current_page = -1;
          cur_font = NULL;
          g_string_append_printf(str, "BT %.2f 0 0 %.2f %.2f %.2f Tm ",
            item->font_size, -item->font_size,
            item->bbox.left + (gdouble) logical_rect.x/PANGO_SCALE,
            item->bbox.top + (gdouble) baseline/PANGO_SCALE);
          in_string = FALSE;
          for (i=0; i<run->glyphs->num_glyphs; i++) {
            glyph_no = run->glyphs->glyphs[i].glyph;
            if (FT_IS_SFNT(ftface)) glyph_page = glyph_no/255;
            else glyph_page = 0;
            if (glyph_page != current_page) {
              cur_font = new_pdffont(xref, pdffonts, filename, font_id,
                 ftface, glyph_page);
              if (in_string) g_string_append(str, ") Tj ");
              in_string = FALSE;
              g_string_append_printf(str, "/F%d 1 Tf ", cur_font->n_obj);
            }
            current_page = glyph_page;
            FT_Load_Glyph(ftface, glyph_no, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING | FT_LOAD_NO_BITMAP | FT_LOAD_IGNORE_TRANSFORM);
            advance = (int)(ftface->glyph->metrics.horiAdvance * cur_font->ft2ps + 0.5);
            if (!in_string) g_string_append_c(str, '(');
            in_string = TRUE;
            if (cur_font->is_truetype) {
              if (glyph_no) glyph_no = (glyph_no%255)+1;
              cur_font->glyphmap[glyph_no] = glyph_no;
            }
            else {
              for (j=1; j<=cur_font->num_glyphs_used; j++)
                if (cur_font->glyphmap[j] == glyph_no) break;
              if (j==256) j=0; // font is full, what do we do?
              if (j>cur_font->num_glyphs_used) {
                cur_font->glyphmap[j] = glyph_no; 
                cur_font->num_glyphs_used++;
                if (FT_Get_Glyph_Name(ftface, glyph_no, tmpstr, 200) == FT_Err_Ok)
                  cur_font->glyphpsnames[j] = g_strdup(tmpstr);
                else cur_font->glyphpsnames[j] = g_strdup(".notdef");
              }
              glyph_no = j;
            }
            cur_font->advance[glyph_no] = advance;
            if (glyph_no=='\\' || glyph_no == '(' || glyph_no == ')' || glyph_no == 10 || glyph_no == 13) 
              g_string_append_c(str, '\\');
            if (glyph_no==10) g_string_append_c(str,'n');
            else if (glyph_no==13) g_string_append_c(str,'r');
            else g_string_append_c(str, glyph_no);
          }
          if (in_string) g_string_append(str, ") Tj ");
          g_string_append(str, "ET ");
          pango_fc_font_unlock_face(fcfont);
        } while (pango_layout_iter_next_run(iter));
        pango_layout_iter_free(iter);
        g_object_unref(layout);
      }
    }
  }
}

// main printing function

/* we use the following object numbers, starting with n_obj_catalog:
    0 the document catalog
    1 the page tree
    2 the GS for the hiliters
    3 ... the page objects
*/

gboolean print_to_pdf(char *filename)
{
  FILE *f;
  GString *pdfbuf, *pgstrm, *zpgstrm, *tmpstr;
  int n_obj_catalog, n_obj_pages_offs, n_page, n_obj_bgpix, n_obj_prefix;
  int i, startxref;
  struct XrefTable xref;
  GList *pglist;
  struct Page *pg;
  gboolean annot, uses_pdf;
  gboolean use_hiliter;
  struct PdfInfo pdfinfo;
  struct PdfObj *obj;
  GList *pdffonts, *list;
  struct PdfFont *font;
  char *tmpbuf;
  
  f = fopen(filename, "w");
  if (f == NULL) return FALSE;
  setlocale(LC_NUMERIC, "C");
  annot = FALSE;
  xref.data = NULL;
  uses_pdf = FALSE;
  pdffonts = NULL;
  for (pglist = journal.pages; pglist!=NULL; pglist = pglist->next) {
    pg = (struct Page *)pglist->data;
    if (pg->bg->type == BG_PDF) uses_pdf = TRUE;
  }
  
  if (uses_pdf && bgpdf.status != STATUS_NOT_INIT && 
      bgpdf.file_contents!=NULL && !strncmp(bgpdf.file_contents, "%PDF-1.", 7)) {
    // parse the existing PDF file
    pdfbuf = g_string_new_len(bgpdf.file_contents, bgpdf.file_length);
    if (pdfbuf->str[7]<'4') pdfbuf->str[7] = '4'; // upgrade to 1.4
    annot = pdf_parse_info(pdfbuf, &pdfinfo, &xref);
    if (!annot) {
      g_string_free(pdfbuf, TRUE);
      if (xref.data != NULL) g_free(xref.data);
    }
  }

  if (!annot) {
    pdfbuf = g_string_new("%PDF-1.4\n%\370\357\365\362\n");
    xref.n_alloc = xref.last = 0;
    xref.data = NULL;
  }
    
  // catalog and page tree
  n_obj_catalog = xref.last+1;
  n_obj_pages_offs = xref.last+4;
  make_xref(&xref, n_obj_catalog, pdfbuf->len);
  g_string_append_printf(pdfbuf, 
    "%d 0 obj\n<< /Type /Catalog /Pages %d 0 R >> endobj\n",
     n_obj_catalog, n_obj_catalog+1);
  make_xref(&xref, n_obj_catalog+1, pdfbuf->len);
  g_string_append_printf(pdfbuf,
    "%d 0 obj\n<< /Type /Pages /Kids [", n_obj_catalog+1);
  for (i=0;i<journal.npages;i++)
    g_string_append_printf(pdfbuf, "%d 0 R ", n_obj_pages_offs+i);
  g_string_append_printf(pdfbuf, "] /Count %d >> endobj\n", journal.npages);
  make_xref(&xref, n_obj_catalog+2, pdfbuf->len);
  g_string_append_printf(pdfbuf, 
    "%d 0 obj\n<< /Type /ExtGState /CA %.2f >> endobj\n",
     n_obj_catalog+2, ui.hiliter_opacity);
  xref.last = n_obj_pages_offs + journal.npages-1;
  
  for (pglist = journal.pages, n_page = 0; pglist!=NULL;
       pglist = pglist->next, n_page++) {
    pg = (struct Page *)pglist->data;
    
    // draw the background and page into pgstrm
    pgstrm = g_string_new("");
    g_string_printf(pgstrm, "q 1 0 0 -1 0 %.2f cm 1 J 1 j ", pg->height);
    n_obj_bgpix = -1;
    n_obj_prefix = -1;
    if (pg->bg->type == BG_SOLID)
      pdf_draw_solid_background(pg, pgstrm);
    else if (pg->bg->type == BG_PDF && annot && 
             pdfinfo.pages[pg->bg->file_page_seq-1].contents!=NULL) {
      make_xref(&xref, xref.last+1, pdfbuf->len);
      n_obj_prefix = xref.last;
      tmpstr = make_pdfprefix(pdfinfo.pages+(pg->bg->file_page_seq-1),
                              pg->width, pg->height);
      g_string_append_printf(pdfbuf,
        "%d 0 obj\n<< /Length %d >> stream\n%s\nendstream\nendobj\n",
        n_obj_prefix, tmpstr->len, tmpstr->str);
      g_string_free(tmpstr, TRUE);
      g_string_prepend(pgstrm, "Q Q Q ");
    }
    else if (pg->bg->type == BG_PIXMAP || pg->bg->type == BG_PDF)
      n_obj_bgpix = pdf_draw_bitmap_background(pg, pgstrm, &xref, pdfbuf);
    // draw the page contents
    use_hiliter = FALSE;
    pdf_draw_page(pg, pgstrm, &use_hiliter, &xref, &pdffonts);
    g_string_append_printf(pgstrm, "Q\n");
    
    // deflate pgstrm and write it
    zpgstrm = do_deflate(pgstrm->str, pgstrm->len);
    g_string_free(pgstrm, TRUE);
    
    make_xref(&xref, xref.last+1, pdfbuf->len);
    g_string_append_printf(pdfbuf, 
      "%d 0 obj\n<< /Length %d /Filter /FlateDecode>> stream\n",
      xref.last, zpgstrm->len);
    g_string_append_len(pdfbuf, zpgstrm->str, zpgstrm->len);
    g_string_free(zpgstrm, TRUE);
    g_string_append(pdfbuf, "endstream\nendobj\n");
    
    // write the page object
    
    make_xref(&xref, n_obj_pages_offs+n_page, pdfbuf->len);
    g_string_append_printf(pdfbuf, 
      "%d 0 obj\n<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %.2f %.2f] ",
      n_obj_pages_offs+n_page, n_obj_catalog+1, pg->width, pg->height);
    if (n_obj_prefix>0) {
      obj = get_pdfobj(pdfbuf, &xref, pdfinfo.pages[pg->bg->file_page_seq-1].contents);
      if (obj->type != PDFTYPE_ARRAY) {
        free_pdfobj(obj);
        obj = dup_pdfobj(pdfinfo.pages[pg->bg->file_page_seq-1].contents);
      }
      g_string_append_printf(pdfbuf, "/Contents [%d 0 R ", n_obj_prefix);
      if (obj->type == PDFTYPE_REF) 
        g_string_append_printf(pdfbuf, "%d %d R ", obj->intval, obj->num);
      if (obj->type == PDFTYPE_ARRAY) {
        for (i=0; i<obj->num; i++) {
          show_pdfobj(obj->elts[i], pdfbuf);
          g_string_append_c(pdfbuf, ' ');
        }
      }
      free_pdfobj(obj);
      g_string_append_printf(pdfbuf, "%d 0 R] ", xref.last);
    }
    else g_string_append_printf(pdfbuf, "/Contents %d 0 R ", xref.last);
    g_string_append(pdfbuf, "/Resources ");

    if (n_obj_prefix>0)
      obj = dup_pdfobj(pdfinfo.pages[pg->bg->file_page_seq-1].resources);
    else obj = NULL;
    if (obj!=NULL && obj->type!=PDFTYPE_DICT)
      { free_pdfobj(obj); obj=NULL; }
    if (obj==NULL) {
      obj = g_malloc(sizeof(struct PdfObj));
      obj->type = PDFTYPE_DICT;
      obj->num = 0;
      obj->elts = NULL;
      obj->names = NULL;
    }
    add_dict_subentry(pdfbuf, &xref,
        obj, "/ProcSet", PDFTYPE_ARRAY, NULL, mk_pdfname("/PDF"));
    if (n_obj_bgpix>0)
      add_dict_subentry(pdfbuf, &xref,
        obj, "/ProcSet", PDFTYPE_ARRAY, NULL, mk_pdfname("/ImageC"));
    if (use_hiliter)
      add_dict_subentry(pdfbuf, &xref,
        obj, "/ExtGState", PDFTYPE_DICT, "/XoHi", mk_pdfref(n_obj_catalog+2));
    if (n_obj_bgpix>0)
      add_dict_subentry(pdfbuf, &xref,
        obj, "/XObject", PDFTYPE_DICT, "/ImBg", mk_pdfref(n_obj_bgpix));
    for (list=pdffonts; list!=NULL; list = list->next) {
      font = (struct PdfFont *)list->data;
      if (font->used_in_this_page) {
        add_dict_subentry(pdfbuf, &xref,
          obj, "/ProcSet", PDFTYPE_ARRAY, NULL, mk_pdfname("/Text"));
        tmpbuf = g_strdup_printf("/F%d", font->n_obj);
        add_dict_subentry(pdfbuf, &xref,
          obj, "/Font", PDFTYPE_DICT, tmpbuf, mk_pdfref(font->n_obj));
        g_free(tmpbuf);
      }
    }
    show_pdfobj(obj, pdfbuf);
    free_pdfobj(obj);
    g_string_append(pdfbuf, " >> endobj\n");
  }
  
  // after the pages, we insert fonts
  for (list = pdffonts; list!=NULL; list = list->next) {
    font = (struct PdfFont *)list->data;
    embed_pdffont(pdfbuf, &xref, font);
    g_free(font->filename);
    g_free(font->fontname);
    g_free(font);
  }
  g_list_free(pdffonts);
  
  // PDF trailer
  startxref = pdfbuf->len;
  if (annot) g_string_append_printf(pdfbuf,
        "xref\n%d %d\n", n_obj_catalog, xref.last-n_obj_catalog+1);
  else g_string_append_printf(pdfbuf, 
        "xref\n0 %d\n0000000000 65535 f \n", xref.last+1);
  for (i=n_obj_catalog; i<=xref.last; i++)
    g_string_append_printf(pdfbuf, "%010d 00000 n \n", xref.data[i]);
  g_string_append_printf(pdfbuf, 
    "trailer\n<< /Size %d /Root %d 0 R ", xref.last+1, n_obj_catalog);
  if (annot) {
    g_string_append_printf(pdfbuf, "/Prev %d ", pdfinfo.startxref);
    // keeping encryption info somehow doesn't work.
    // xournal can't annotate encrypted PDFs anyway...
/*    
    obj = get_dict_entry(pdfinfo.trailerdict, "/Encrypt");
    if (obj!=NULL) {
      g_string_append_printf(pdfbuf, "/Encrypt ");
      show_pdfobj(obj, pdfbuf);
    } 
*/
  }
  g_string_append_printf(pdfbuf, 
    ">>\nstartxref\n%d\n%%%%EOF\n", startxref);
  
  g_free(xref.data);
  if (annot) {
    free_pdfobj(pdfinfo.trailerdict);
    if (pdfinfo.pages!=NULL)
      for (i=0; i<pdfinfo.npages; i++) {
        free_pdfobj(pdfinfo.pages[i].resources);
        free_pdfobj(pdfinfo.pages[i].mediabox);
        free_pdfobj(pdfinfo.pages[i].contents);
      }
  }
  
  setlocale(LC_NUMERIC, "");
  if (fwrite(pdfbuf->str, 1, pdfbuf->len, f) < pdfbuf->len) {
    fclose(f);
    g_string_free(pdfbuf, TRUE);
    return FALSE;
  }
  fclose(f);
  g_string_free(pdfbuf, TRUE);
  return TRUE;
}

/*********** Printing via gtk-print **********/

#if GTK_CHECK_VERSION(2, 10, 0)

// does the same job as update_canvas_bg(), but to a print context

void print_background(cairo_t *cr, struct Page *pg)
{
  double x, y;
  GdkPixbuf *pix;
  BgPdfPage *pgpdf;
  PopplerPage *pdfpage;
  cairo_surface_t *cr_pixbuf;
  double pgwidth, pgheight;

  if (pg->bg->type == BG_SOLID) {
    cairo_set_source_rgb(cr, RGBA_RGB(pg->bg->color_rgba));
    cairo_rectangle(cr, 0, 0, pg->width, pg->height);
    cairo_fill(cr);
    if (!ui.print_ruling) return;
    if (pg->bg->ruling == RULING_NONE) return;
    cairo_set_source_rgb(cr, RGBA_RGB(RULING_COLOR));
    cairo_set_line_width(cr, RULING_THICKNESS);

    if (pg->bg->ruling == RULING_GRAPH) {
      for (x=RULING_GRAPHSPACING; x<pg->width-1; x+=RULING_GRAPHSPACING)
        { cairo_move_to(cr, x, 0); cairo_line_to(cr, x, pg->height); }
      for (y=RULING_GRAPHSPACING; y<pg->height-1; y+=RULING_GRAPHSPACING)
        { cairo_move_to(cr, 0, y); cairo_line_to(cr, pg->width, y); }
      cairo_stroke(cr);
      return;
    }
    
    for (y=RULING_TOPMARGIN; y<pg->height-1; y+=RULING_SPACING)
      { cairo_move_to(cr, 0, y); cairo_line_to(cr, pg->width, y); }
    cairo_stroke(cr);
    if (pg->bg->ruling == RULING_LINED) {
      cairo_set_source_rgb(cr, RGBA_RGB(RULING_MARGIN_COLOR));
      cairo_move_to(cr, RULING_LEFTMARGIN, 0);
      cairo_line_to(cr, RULING_LEFTMARGIN, pg->height);
      cairo_stroke(cr);
    }
    return;
  }
  else
  if (pg->bg->type == BG_PDF) {
    if (!bgpdf.document) return;
    pdfpage = poppler_document_get_page(bgpdf.document, pg->bg->file_page_seq-1);
    if (!pdfpage) return;
    poppler_page_get_size(pdfpage, &pgwidth, &pgheight);
    cairo_save(cr);
    cairo_scale(cr, pg->width/pgwidth, pg->height/pgheight);
    poppler_page_render(pdfpage, cr);
    cairo_restore(cr);
    g_object_unref(pdfpage);
  }
  else 
  if (pg->bg->type == BG_PIXMAP) {
    cairo_save(cr);
    cairo_scale(cr, pg->width/gdk_pixbuf_get_width(pg->bg->pixbuf),
                    pg->height/gdk_pixbuf_get_height(pg->bg->pixbuf));
    gdk_cairo_set_source_pixbuf(cr, pg->bg->pixbuf, 0, 0);
    cairo_rectangle(cr, 0, 0, gdk_pixbuf_get_width(pg->bg->pixbuf), gdk_pixbuf_get_height(pg->bg->pixbuf));
    cairo_fill(cr);
    cairo_restore(cr);
  }
}

void print_job_render_page(GtkPrintOperation *print, GtkPrintContext *context, gint pageno, gpointer user_data)
{
  cairo_t *cr;
  gdouble width, height, scale;
  struct Page *pg;
  guint old_rgba;
  double old_thickness;
  GList *layerlist, *itemlist;
  struct Layer *l;
  struct Item *item;
  int i;
  double *pt;
  PangoFontDescription *font_desc;
  PangoLayout *layout;
        
  pg = (struct Page *)g_list_nth_data(journal.pages, pageno);
  cr = gtk_print_context_get_cairo_context(context);
  width = gtk_print_context_get_width(context);
  height = gtk_print_context_get_height(context);
  scale = MIN(width/pg->width, height/pg->height);
  
  cairo_translate(cr, (width-scale*pg->width)/2, (height-scale*pg->height)/2);
  cairo_scale(cr, scale, scale);
  cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND);
  cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
  
  print_background(cr, pg);

  old_rgba = predef_colors_rgba[COLOR_BLACK];
  cairo_set_source_rgb(cr, 0, 0, 0);
  old_thickness = 0.0;

  for (layerlist = pg->layers; layerlist!=NULL; layerlist = layerlist->next) {
    l = (struct Layer *)layerlist->data;
    for (itemlist = l->items; itemlist!=NULL; itemlist = itemlist->next) {
      item = (struct Item *)itemlist->data;
      if (item->type == ITEM_STROKE || item->type == ITEM_TEXT) {
        if (item->brush.color_rgba != old_rgba)
          cairo_set_source_rgba(cr, RGBA_RGB(item->brush.color_rgba),
                                    RGBA_ALPHA(item->brush.color_rgba));
        old_rgba = item->brush.color_rgba;
      }
      if (item->type == ITEM_STROKE) {    
        if (item->brush.thickness != old_thickness)
          cairo_set_line_width(cr, item->brush.thickness);
        pt = item->path->coords;
        if (!item->brush.variable_width) {
          cairo_move_to(cr, pt[0], pt[1]);
          for (i=1, pt+=2; i<item->path->num_points; i++, pt+=2)
            cairo_line_to(cr, pt[0], pt[1]);
          cairo_stroke(cr);
          old_thickness = item->brush.thickness;
        } else {
          for (i=0; i<item->path->num_points-1; i++, pt+=2) {
            cairo_move_to(cr, pt[0], pt[1]);
            cairo_set_line_width(cr, item->widths[i]);
            cairo_line_to(cr, pt[2], pt[3]);
            cairo_stroke(cr);
          }
          old_thickness = 0.0;
        }
      }
      if (item->type == ITEM_TEXT) {
        layout = gtk_print_context_create_pango_layout(context);
        font_desc = pango_font_description_from_string(item->font_name);
        if (item->font_size)
          pango_font_description_set_absolute_size(font_desc,
            item->font_size*PANGO_SCALE);
        pango_layout_set_font_description(layout, font_desc);
        pango_font_description_free(font_desc);
        pango_layout_set_text(layout, item->text, -1);
        cairo_move_to(cr, item->bbox.left, item->bbox.top);
        pango_cairo_show_layout(cr, layout);
        g_object_unref(layout);
      }
    }
  }
}

#endif
