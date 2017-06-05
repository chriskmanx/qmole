/* various utility functions */

#include <config.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <regex.h>
#include <zlib.h>

#ifdef HAVE_LIBBZ2
#include <bzlib.h>
#endif
#include <glib.h>

#include "data.h"
#include "utils.h"

void strip_spaces( char *str )
{
  int len;

  len = strlen( str );
  if (len < 1)
    return;

  while (*str == ' ')
    {
      len = strlen( str );
      memmove(str, str+1, len);
    }

  len = strlen( str );
  while (*(str+len) == '\n')
    {
      *(str+len) = '\000';
      len = strlen( str );
    }
	  
#if 0
  q = str + strlen( str );
  for (p=str; *p == ' ' || *p == '\t'; p++);
  for (r=q-1; *r == ' ' || *r == '\t'; r--);
  len = r - p + 1;
  memmove( str, p, len );
  *(str+len)='\000';
#endif
}

void strip_newlines( char *str )
{
  char *p;
  int len;

  len = strlen( str );
  if (len < 2)
    return;

  for (p=str; len > 0; len--)
    if ( *p == '\n' )
      {
	if (len > 1)
	  memmove(p , p+1, len );
	else
	  *p = '\000';
      }
    else
      p++;
}

void strip_dupspaces( char *str )
{
  char *p;
  int len;

  len = strlen( str ) - 1;
  if (len < 2)
    return;

  for (p=str+1; len > 0; len--)
    if ( *p == ' ' && *(p-1) == ' ')
      {  
	if (len > 1)
	  memmove(p , p+1, len );
	else
	  *p = '\000';
      }
    else
      p++;
}

void convert_newlines_to_spaces( char *str )
{
  char *p;
  int len;

  /* trim head and trailing newlines */
  while (*str == '\n')
    {
      len = strlen( str );
      memmove(str, str+1, len);
    }

  len = strlen( str );
  while (*(str+len) == '\n')
    {
      *(str+len) = '\000';
      len = strlen( str );
    }

  len = strlen( str );
  for (p=str; len > 0; len--, p++)
    if ( *p == '\n' )
      *p = ' ';
}

char * escape_html_chars( char *str )
{
  char *p;
  char *q;
  char tmp[2];
  int len;

  if (!str)
    return NULL;

  len = strlen( str );
#if 0
  if (len <2)
    fprintf(stderr,"escaped less < 2 chars= |%s|\n",str);

#endif

  q = (char *) g_malloc( len * 5 + 4);
  *q = '\000';
  *(tmp+1) = '\000';
  for (p=str; *p; p++)
    if ( *p == '>' )
      strcat(q, "&gt;");
    else if ( *p == '<' )
      strcat(q, "&lt;");
    else if ( *p == '&' )
      strcat(q, "&amp;");
    else
      {
	*tmp = *p;
	strcat(q, tmp);
      }
  return q;
}


char *get_line_from_contents( char *ptr, int size )
{
  char *eoln;
  char *line;
  int linelen;

  eoln = memchr(ptr, '\n', size);
  if (eoln == NULL)
    return NULL;
  
  linelen = (eoln-ptr);
  line = (char *) g_malloc( linelen+1 );
  memcpy(line, ptr, linelen);
  *(line+linelen) = '\000';
  return line;
}
  
/* copies from string str up to any character in chr */
/* if chr doesn't exist, return NULL */
/* otherwise return location of match */
/* allocates a new string if anything copied */
char *copy_to_anychar( char *str, char *chr, char **copy  )
{
  int len;
  char *match;

  match = strpbrk(str, chr);
  if (!match)
    return NULL;

  len = match - str;
  *copy = (char *) g_malloc( len+1 );
  *(*copy+len) = '\000';
  memcpy(*copy, str, len );
  return match;
}

/* allocates a new node */
NODE *alloc_node()
{
  NODE * tmp;

  tmp = (NODE *) g_malloc( sizeof(NODE) );

  if (tmp)
    {
      tmp->nodename=NULL;
      tmp->filename=NULL;
      tmp->contents=NULL;
      tmp->contlen=0;
      tmp->next=NULL;
      tmp->prev=NULL;
      tmp->up=NULL;
      tmp->menu=NULL;
      tmp->menu_start=NULL;
    }
  return tmp;
}

/* deallocates a new node */
void free_node(NODE *tmp)
{
  if (tmp)
    {
      if (tmp->nodename)
	g_free(tmp->nodename);
      if (tmp->filename)
	g_free(tmp->filename);
      if (tmp->contents)
	g_free(tmp->contents);
    }
  g_free(tmp);
}

#if 0
void map_spaces_to_underscores( char *str )
{
  char *p;
  
  for (p=str; *p; p++)
    if (*p == ' ')
      *p = '_';
}
#endif

void map_spaces_to_underscores( char *str )
{
  char *p;

  for (p=str; *p; p++)
    switch (*p)
      {
      case ' ':
      case '\n':
      case '\t':
      case '`':
      case '\'':
      case '/': 
      case '\\':
      case '"':
      case '.':
      case '!':
	*p = '_';
	break;
      }
}


  
/* reduce infofile filename to basename alone */
void fixup_info_filename( char *file )
{
  char *ptr1;
  char tail[] = ".info";

  if (strlen(file) < 6)
    return;

  ptr1 = strrchr( file, '.' );
  if (!ptr1)
    return;

  if (!strncmp(ptr1, tail, strlen(tail)))
    *ptr1 = '\000';

}

ReadBuf * readbuf_open(const char *name) {
  ReadBuf *f;

#ifdef HAVE_LIBBZ2  
  if (strlen(name) > 4 && strcmp(name+strlen(name)-4,".bz2") == 0) {
    f = g_new0 (ReadBuf, 1);
    f->buffer = g_malloc(READ_BUF_SIZE);
    f->bzfile = bzopen(name, "r");
    if(!f->bzfile) {
      free(f);
      return NULL;
    }
    
    f->type = BZIP2;
    f->eof = FALSE;
  }
  else 
#endif
    {
      f = g_new0 (ReadBuf, 1);
      f->buffer = g_malloc(READ_BUF_SIZE);
      f->gzfile = gzopen(name, "r");     
      if (f->gzfile == NULL) {
	free(f);
	return NULL;
      }
      else {
	f->type = GZIP;
	f->eof = FALSE;
      }
    }
  return f;
}

void readbuf_close(ReadBuf *f) 
{
  switch (f->type) {
  case GZIP:
    gzclose(f->gzfile);
    break;
#ifdef HAVE_LIBBZ2
  case BZIP2:
    bzclose(f->bzfile);
    break;
#endif
  }
  g_free (f->buffer);
  g_free (f);
  }

int readbuf_eof(ReadBuf *f)
{
  return f->eof;
}

static int
readbuf_getc (ReadBuf *rb)
{
	if (rb->eof)
		return EOF;

	if (rb->size == 0 ||
	    rb->pos == rb->size) {
		int bytes_read;
		
		switch (rb->type) {
		case GZIP:
		  bytes_read = gzread(rb->gzfile,rb->buffer,READ_BUF_SIZE);
		  break;
#ifdef HAVE_LIBBZ2
		case BZIP2:
		  bytes_read = bzread(rb->bzfile,rb->buffer,READ_BUF_SIZE);
		  break;
#endif
		}

		if (bytes_read == 0) {
			rb->eof = TRUE;
			return EOF;
		}

		rb->size = bytes_read;
		rb->pos = 0;

	}

	return (guchar) rb->buffer[rb->pos++];
}

char *
readbuf_gets (ReadBuf *rb, char *buf, gsize bufsize)
{
	int c;
	gsize pos;

	g_return_val_if_fail (buf != NULL, NULL);
	g_return_val_if_fail (rb != NULL, NULL);

	pos = 0;
	buf[0] = '\0';

	do {
		c = readbuf_getc (rb);
		if (c == EOF || c == '\n')
			break;
		buf[pos++] = c;
	} while (pos < bufsize-1);

	if (c == EOF && pos == 0)
		return NULL;

	if (c == '\n')
	  buf[pos++] = '\n';

	buf[pos++] = '\0';

	return buf;
}

