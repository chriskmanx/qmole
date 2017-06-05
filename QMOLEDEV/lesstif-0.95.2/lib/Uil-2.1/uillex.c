/**
 *
 * $Id: uillex.c,v 1.1 2004/08/28 19:22:42 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */ 

static const char rcsid[] = "$Id: uillex.c,v 1.1 2004/08/28 19:22:42 dannybackx Exp $";

#include <LTconfig.h>

/* Enable this for verbose lexing */
/* #define DEBUG */

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <XmI/UilI.h>
#include <uil/UilSymGl.h>

#include <XmI/DebugUtil.h>


extern void
_uilmsg(Uil_parse_data *pd, int severity, const char *fmt, ...)
{
    va_list arg_list;
 
    va_start(arg_list, fmt);
    vfprintf(stderr, fmt, arg_list);
    va_end(arg_list);
}


static unsigned char
hashval(const char *data, int len)
{
    int i;
    unsigned char hash = 0;

    for (i = 0; i < len; i++)
    {
	hash += data[i] * i;
    }

    hash %= sym_k_hash_table_limit;

    return hash;
}


static sym_entry *
add_symbol(Uil_parse_data *pd, char *data, int len, int type)
{
    int hash = hashval(data, len);
    sym_entry *e, **lst;

    e = (sym_entry *)XtMalloc(sizeof(sym_entry));

    for (lst = &pd->hash[hash]; *lst != NULL; lst = &(*lst)->next)
	;

    *lst = e;

    e->next = NULL;
    e->type = type;
    e->text = XtMalloc(len);
    e->len = len;
    memcpy(e->text, data, len);
    e->e.entry = NULL;

    return e;
}


static sym_entry *
find_symbol(Uil_parse_data *pd, char *data, int len)
{
    int hash = hashval(data, len);
    sym_entry *lst;

    for (lst = pd->hash[hash]; lst != NULL; lst = lst->next)
    {
	if (lst->len == len && memcmp(lst->text, data, len) == 0)
	{
	    return lst;
	}
    }

    return NULL;
}


static void
add_file(Uil_parse_data *pd, char *fn)
{
    int i;

    if (pd->files == NULL)
    {
	pd->max_files = 10;

	pd->files = (char **)XtCalloc(pd->max_files, sizeof(char *));

	pd->num_files = 0;

	pd->cur_file_name_idx = 0;
    }
    else if (pd->num_files == pd->max_files - 1)
    {
	pd->max_files <<= 1;

	pd->files = (char **)XtRealloc((char *)pd->files,
				       pd->max_files * sizeof(char *));
    }

    for (i = 0; i < pd->num_files; i++)
    {
	if (strcmp(fn, pd->files[i]) == 0)
	{
	    pd->cur_file_name_idx = i;

	    return;
	}
    }

    pd->files[pd->num_files] = XtNewString(fn);
    pd->cur_file_name_idx = pd->num_files;
    pd->num_files++;
}

static void
remove_file(Uil_parse_data *pd)
{
    if (pd->cur_file >= 0)
    {
	close(pd->cur_file);
	pd->cur_file--;
    }
}


static int
get_file(Uil_parse_data *pd, char *fn)
{
    int ret, i;

    TRACE(("Trying to open %s\n", fn));

    if ((ret = open(fn, O_RDONLY)) >= 0)
    {
	add_file(pd, fn);

	pd->cur_file++;
	pd->file_data[pd->cur_file].fd = ret;
	pd->file_data[pd->cur_file].pbuf[PARSE_BUF_SIZE/2 - 1] = EOF;
	pd->file_data[pd->cur_file].pbuf[PARSE_BUF_SIZE - 1] = EOF;
	pd->file_data[pd->cur_file].lexeme = pd->file_data[pd->cur_file].pbuf;
	pd->file_data[pd->cur_file].forward =
	    pd->file_data[pd->cur_file].pbuf + PARSE_BUF_SIZE - 2;

	TRACE(("Opened file: cur_file is %d: fd %d\n",
		pd->cur_file, pd->file_data[pd->cur_file].fd));

	return ret;
    }

    for (i = 0; i < (int)pd->command->include_dir_count; i++)
    {
	char buf[PATHLENMAX];

	strcpy(buf, pd->command->include_dir[i]);
	if (buf[strlen(buf)] != '/')
	{
	    strcat(buf, "/");
	}

	strcat(buf, fn);

	TRACE(("Trying to open %s\n", fn));

	if ((ret = open(buf, O_RDONLY)) > 0)
	{
	    add_file(pd, buf);

	    pd->cur_file++;
	    pd->file_data[pd->cur_file].fd = ret;
	    pd->file_data[pd->cur_file].pbuf[PARSE_BUF_SIZE/2 - 1] = EOF;
	    pd->file_data[pd->cur_file].pbuf[PARSE_BUF_SIZE - 1] = EOF;
	    pd->file_data[pd->cur_file].lexeme =
		pd->file_data[pd->cur_file].pbuf;
	    pd->file_data[pd->cur_file].forward =
		pd->file_data[pd->cur_file].pbuf + PARSE_BUF_SIZE - 2;

	    return ret;
	}
    }

    return -ENOENT;
}


extern Uil_parse_data *
_uil_parse_init(Uil_command_type *cmd, Uil_compile_desc_type *desc)
{
    Uil_parse_data *pd;
    int fd, i;

    pd = (Uil_parse_data *)XtCalloc(1, sizeof(Uil_parse_data));

    pd->command = cmd;
    pd->desc = desc;
    pd->root = NULL;
    pd->hash = (sym_entry **)XtCalloc(sym_k_hash_table_limit,
				      sizeof(sym_entry *));

    pd->cur_file = -1;
    if ((fd = get_file(pd, cmd->source_file)) < 0)
    {
	_uilmsg(pd, Uil_k_severe_status,
	       "Can't open source file: %s\n", cmd->source_file);

	_uil_parse_destroy(pd);

	return NULL;
    }

    pd->include_depth = 0;

    pd->num_lines = 0;
    pd->max_lines = 100;
    pd->lines =
	(src_source_record_type *)XtCalloc(pd->max_lines,
					   sizeof(src_source_record_type));

    for (i = 0; i < key_k_keyword_count; i++)
    {
	sym_entry *s;

	s = add_symbol(pd, key_table[i].at_name, key_table[i].b_length,
		       KEYWORD);

	s->e.keyword = &key_table[i];
    }

    return pd;
}


extern void
_uil_parse_destroy(Uil_parse_data *pd)
{
    int i;

    if (pd->lines)
    {
	XtFree((char *)pd->lines);
    }

    if (pd->lines)
    {
	XtFree((char *)pd->lines);
    }

    if (pd->hash)
    {
	XtFree((char *)pd->hash);
    }

    if (pd->files)
    {
	for (i = 0; i < pd->num_files; i++)
	{
	    XtFree(pd->files[i]);
	}
	XtFree((char *)pd->files);
    }

    XtFree((char *)pd);
}


static char
input(Uil_parse_data *pd)
{
    int ret;
    struct _file_data *cf = &pd->file_data[pd->cur_file];

    cf->forward++;
    cf->colno++;
    if (*cf->forward == (char)EOF)
    {
	if (cf->forward - cf->pbuf == PARSE_BUF_SIZE - 1)
	{
	    if ((ret = read(cf->fd, cf->pbuf,
			    PARSE_BUF_SIZE / 2 - 1)) < 0)
	    {
		_uilmsg(pd, Uil_k_severe_status,
		       "Error reading source file: %s fd %d ret %d errno %d\n",
		       pd->files[pd->cur_file_name_idx],
		       pd->file_data[pd->cur_file].fd, ret, errno);

		return EOF;
	    }
	    else if (ret < PARSE_BUF_SIZE / 2 - 1)
	    {
		cf->forward = cf->pbuf;
		cf->pbuf[ret] = EOF;

		if (*cf->forward == '\n')
		{
		    cf->lineno++;
		    cf->colno = 0;
		}

		return *cf->forward;
	    }
	    else
	    {
		cf->forward = cf->pbuf;

		if (*cf->forward == '\n')
		{
		    cf->lineno++;
		    cf->colno = 0;
		}

		return *cf->forward;
	    }
	}
	else if (cf->forward - cf->pbuf == PARSE_BUF_SIZE / 2 - 1)
	{
	    if ((ret = read(cf->fd, cf->pbuf + PARSE_BUF_SIZE / 2,
			    PARSE_BUF_SIZE / 2 - 1)) < 0)
	    {
		_uilmsg(pd, Uil_k_severe_status,
		       "Error reading source file: %s fd %d ret %d errno %d\n",
		       pd->files[pd->cur_file_name_idx],
		       pd->file_data[pd->cur_file].fd, ret, errno);

		return EOF;
	    }
	    else if (ret < PARSE_BUF_SIZE / 2 - 1)
	    {
		cf->forward = cf->pbuf + PARSE_BUF_SIZE / 2;
		cf->forward[ret] = EOF;

		if (*cf->forward == '\n')
		{
		    cf->lineno++;
		    cf->colno = 0;
		}

		return *cf->forward;
	    }
	    else
	    {
		cf->forward = cf->pbuf + PARSE_BUF_SIZE / 2;

		if (*cf->forward == '\n')
		{
		    cf->lineno++;
		    cf->colno = 0;
		}

		return *cf->forward;
	    }
	}
	else
	{
	    return EOF;
	}
    }

    if (*cf->forward == '\n')
    {
	cf->lineno++;
	cf->colno = 0;
    }

    return *cf->forward;
}


static void
unput(Uil_parse_data *pd)
{
    struct _file_data *cf = &pd->file_data[pd->cur_file];

    if (*cf->forward == '\n')
    {
	cf->lineno--;
    }

    if (cf->forward == cf->pbuf)
    {
	cf->forward = cf->pbuf + PARSE_BUF_SIZE - 2;
    }
    else if (cf->forward == cf->pbuf + PARSE_BUF_SIZE / 2)
    {
	cf->forward = cf->pbuf + PARSE_BUF_SIZE / 2 - 2;
    }
    else
    {
	cf->forward--;
    }
}


extern int
_uil_next_lexeme(Uil_parse_data *pd)
{
    char ch, ch2, *ptr;
    int have_point;
    sym_entry *e;

    pd->file_data[pd->cur_file].lexeme = pd->file_data[pd->cur_file].forward;
    for (;;)
    {
	ch = input(pd);

	switch (ch)
	{
	case ' ':
	case '\t':
	case '\v':
	case '\r':
	case '\b':
	case '\n':
	    /* skip it */
	    pd->file_data[pd->cur_file].lexeme =
		pd->file_data[pd->cur_file].forward;
	    break;

	case '!':
	    while ((ch = input(pd)) != '\n' && ch != (char)EOF)
		;

	    pd->file_data[pd->cur_file].lexeme =
		pd->file_data[pd->cur_file].forward;

	    TRACE(("Bang comment: lineno: %d\n",
		   pd->file_data[pd->cur_file].lineno));

	    if (ch != '\n')
	    {
		return EOF;
	    }
	    break;

	case '/':
	    ch2 = input(pd);
	    if (ch2 == '*')
	    {
		while ((ch2 = input(pd)) != (char)EOF)
		{
		    if (ch2 == '*')
		    {
			if ((ch = input(pd)) == '/')
			{
			    pd->file_data[pd->cur_file].lexeme =
				pd->file_data[pd->cur_file].forward;

			    break;
			}
		    }
		}

		TRACE(("C comment: lineno: %d\n",
		       pd->file_data[pd->cur_file].lineno));

		if (ch == (char)EOF)
		{
		    return EOF;
		}

		pd->file_data[pd->cur_file].lexeme =
		    pd->file_data[pd->cur_file].forward;
	    }
	    else
	    {
		unput(pd);

		return ch;
	    }
	    break;

	case '\'':
	case '"':
	    ch2 = ch;
	    ptr = pd->curtok;
	    while ((ch = input(pd)) != (char)EOF)
	    {
		if (ch == '\'' || ch == '"')
		{
		    if (ch2 != '\\')
		    {
			break;
		    }
		}

		*ptr = ch;
		ptr++;
		ch2 = ch;
	    }

	    if (ch == (char)EOF)
	    {
		unput(pd);
	    }

	    *ptr = 0;

	    TRACE(("got STRING: %s\n", pd->curtok));

	    pd->value.string = XtNewString(pd->curtok);

	    return STRING;

	case '*':
	case '+':
	case '-':
	case '=':
	    TRACE(("got OPERATOR: %c\n", ch));
	    return ch;
	    break;

	case '(':
	case ')':
	case '{':
	case '}':
	case ':':
	case ';':
	case ',':
	    TRACE(("got PUNCTUATION: %c\n", ch));
	    return ch;
	    break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    /* get number */
	    have_point = False;
	    ptr = pd->curtok;
	    *ptr = ch;
	    ptr++;
	    while ((ch = input(pd)) != (char)EOF)
	    {
		if (!isdigit(ch) || ch != '.')
		{
		    break;
		}

		if (ch == '.' && have_point)
		{
		    break;
		}
		else if (ch == '.')
		{
		    have_point = True;
		}

		*ptr = ch;
		ptr++;
	    }

	    unput(pd);

	    *ptr = 0;

	    TRACE(("got NUMBER: %s\n", pd->curtok));

	    if (have_point)
	    {
		pd->value.real = atof(pd->curtok);
		return REAL;
	    }
	    else
	    {
		pd->value.integer = atoi(pd->curtok);
		return INT;
	    }

	case EOF:
	    TRACE(("EOF\n"));
	    TRACE(("CURRENT FILE: %s line: %d\n",
		   pd->files[pd->cur_file_name_idx],
		   pd->file_data[pd->cur_file].lineno));

	    remove_file(pd);

	    if (pd->cur_file < 0)
	    {
		return EOF;
	    }

	    break;

	default:
	    if (isalpha(ch))
	    {
		ptr = pd->curtok;
		*ptr = ch;
		ptr++;
		while ((ch = input(pd)) != (char)EOF)
		{
		    if (!isalpha(ch) && !isdigit(ch) && ch != '_')
		    {
			break;
		    }
		    *ptr = ch;
		    ptr++;
		}

		unput(pd);

		*ptr = 0;

		TRACE(("got SYMBOL: %s\n", pd->curtok));

		if ((e = find_symbol(pd, pd->curtok, strlen(pd->curtok)))
		    != NULL)
		{
		    pd->value.sym = e;

		    return e->type;
		}

		pd->value.sym = add_symbol(pd, pd->curtok, strlen(pd->curtok),
					   SYMBOL);

		return SYMBOL;
	    }
	    else
	    {
		_uilmsg(pd, Uil_k_severe_status,
		       "Invalid lexical component: '%c' : %02x %s %d\n",
		       ch, (unsigned char)ch,
		       pd->files[pd->cur_file_name_idx],
		       pd->file_data[pd->cur_file].lineno);

		return EOF;
	    }

	    break;
	}
    }
}
