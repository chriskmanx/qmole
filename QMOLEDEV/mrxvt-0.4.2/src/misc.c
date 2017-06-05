/*--------------------------------*-C-*---------------------------------*
 * File:	misc.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1996        mj olesen <olesen@me.QueensU.CA>
 * Copyright (c) 1997,1998   Oezguer Kesim <kesim@math.fu-berlin.de>
 * Copyright (c) 1998-2000   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: misc.c,v 1.19 2005/03/21 00:42:08 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL 1
#else 
#define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif


/* EXTPROTO */
char*
rxvt_r_basename(const char *str)
{
    char*	base = STRRCHR(str, '/');

    return (char *)(base ? base + 1 : str);
}


/*
 * Print an error message
 */
/* EXTPROTO */
void
rxvt_print_error(const char *fmt,...)
{
    va_list         arg_ptr;

    va_start(arg_ptr, fmt);
    fprintf(stderr, APL_NAME ": ");
    vfprintf(stderr, fmt, arg_ptr);
    fprintf(stderr, "\n");
    va_end(arg_ptr);
}


/*
 * check that the first characters of S1 match S2
 *
 * No Match
 *      return: 0
 * Match
 *      return: STRLEN (S2)
 */
/* EXTPROTO */
int
rxvt_str_match(const char *s1, const char *s2)
{
    int             n = STRLEN(s2);

    return ((STRNCMP(s1, s2, n) == 0) ? n : 0);
}


/* EXTPROTO */
const char*
rxvt_str_skip_space(const char *str)
{
    if (str)
	while (*str && isspace((int) *str))
	    str++;
    return str;
}


/*
 * remove leading/trailing space and strip-off leading/trailing quotes.
 * in place.
 */
/* EXTPROTO */
char*
rxvt_str_trim(char *str)
{
    char           *r, *s;
    int             n;

    if (!str || !*str)		/* shortcut */
		return str;

	/* skip leading spaces */
    for (s = str; *s && isspace((int) *s); s++)
		;
	/* goto end of string */
    for (n = 0, r = s; *r++; n++)
		;
    r -= 2;
	/* dump return */
    if (n > 0 && *r == '\n')
		n--, r--;
	/* backtrack along trailing spaces */
    for (; n > 0 && isspace((int) *r); r--, n--)
		;
	/* skip matching leading/trailing quotes */
    if (*s == '"' && *r == '"' && n > 1) {
		s++;
		n -= 2;
    }
	/* copy back over: forwards copy */
    for (r = str; n; n--)
		*r++ = *s++;
    *r = '\0';

    return str;
}


/*
 * in-place interpretation of string:
 *
 *      backslash-escaped:      "\a\b\E\e\n\r\t", "\octal"
 *      Ctrl chars:     ^@ .. ^_, ^?
 *
 *      Emacs-style:    "M-" prefix
 *
 * Also,
 *      "M-x" prefixed strings, append "\r" if needed
 *      "\E]" prefixed strings (XTerm escape sequence) append ST if needed
 *
 * returns the converted string length
 */
/* EXTPROTO */
int
rxvt_str_escaped(char *str)
{
    char            ch, *s, *d;
    int             i, num, append = 0;

    if (!str || !*str)
	return 0;

    d = s = str;

    if (*s == 'M' && s[1] == '-') {
		/* Emacs convenience, replace leading `M-..' with `\E..' */
		*d++ = C0_ESC;
		s += 2;
		if (toupper((int) *s) == 'X')
		    /* append carriage-return for `M-xcommand' */
		    for (*d++ = 'x', append = '\r', s++;
				isspace((int) *s);
				s++)
				;
    }
    for (; (ch = *s++);) {
		if (ch == '\\') {
		    ch = *s++;
		    if (ch >= '0' && ch <= '7') {	/* octal */
				num = ch - '0';
				for (i = 0; i < 2; i++, s++) {
				    ch = *s;
				    if (ch < '0' || ch > '7')
						break;
				    num = num * 8 + ch - '0';
				}
				ch = (char)num;
		    }
			else if (ch == 'a')
				ch = C0_BEL;	/* bell */
			else if (ch == 'b')
				ch = C0_BS;	/* backspace */
			else if (ch == 'E' || ch == 'e')
				ch = C0_ESC;	/* escape */
			else if (ch == 'n')
				ch = '\n';	/* newline */
			else if (ch == 'r')
				ch = '\r';	/* carriage-return */
			else if (ch == 't')
				ch = C0_HT;	/* tab */
		}
		else if (ch == '^') {
		    ch = *s++;
		    ch = toupper((int) ch);
		    ch = (ch == '?' ? 127 : (ch - '@'));
		}
		*d++ = ch;
    }

	/* ESC] is an XTerm escape sequence, must be terminated */
    if (*str == '\0' && str[1] == C0_ESC && str[2] == ']')
		append = CHAR_ST;

	/* add trailing character as required */
    if (append && d[-1] != append)
		*d++ = append;
    *d = '\0';

    return (d - str);
}


/*
 * Split a comma-separated string into an array, stripping leading and
 * trailing spaces (and paired quotes) from each entry.  Empty strings
 * are properly returned
 * Caller should free each entry and array when done
 */
/* EXTPROTO */
char**
rxvt_splitcommastring(const char *cs)
{
    int             l, n, p;
    const char     *s, *t;
    char          **ret;

    if ((s = cs) == NULL)
	s = "";

    for (n = 1, t = s; *t; t++)
		if (*t == ',')
		    n++;
	assert (n >= 0 && n+1 > 0);	/* possible integer overflow? */
    ret = rxvt_malloc((n + 1) * sizeof(char *));
    ret[n] = NULL;

    for (l = 0, t = s; l < n; l++) {
		for ( ; *t && *t != ','; t++) ;
			p = t - s;
		ret[l] = rxvt_malloc(p + 1);
		STRNCPY(ret[l], s, p);
		ret[l][p] = '\0';
		rxvt_str_trim(ret[l]);
		s = ++t;
    }
    return ret;
}


/*----------------------------------------------------------------------*
 * file searching
 */

/* #define DEBUG_SEARCH_PATH */

#if defined (BACKGROUND_IMAGE) || (MENUBAR_MAX)
/*
 * search for FILE in the current working directory, and within the
 * colon-delimited PATHLIST, adding the file extension EXT if required.
 *
 * FILE is either semi-colon or zero terminated
 */
/* INTPROTO */
char           *
rxvt_File_search_path(const char *pathlist, const char *file, const char *ext)
{
    int             maxpath, len;
    const char     *p, *path;
    char            name[256];

    if (!access(file, R_OK))	/* found (plain name) in current directory */
	return STRDUP(file);

/* semi-colon delimited */
    if ((p = STRCHR(file, ';')))
	len = (p - file);
    else
	len = STRLEN(file);

#ifdef DEBUG_SEARCH_PATH
    getcwd(name, sizeof(name));
    fprintf(stderr, "pwd: \"%s\"\n", name);
    fprintf(stderr, "find: \"%.*s\"\n", len, file);
#endif

/* leave room for an extra '/' and trailing '\0' */
    maxpath = sizeof(name) - (len + (ext ? STRLEN(ext) : 0) + 2);
    if (maxpath <= 0)
	return NULL;

/* check if we can find it now */
    STRNCPY(name, file, len);
    name[len] = '\0';

    if (!access(name, R_OK))
	return STRDUP(name);
    if (ext) {
	STRCAT(name, ext);
	if (!access(name, R_OK))
	    return STRDUP(name);
    }
    for (path = pathlist; path != NULL && *path != '\0'; path = p) {
	int             n;

	/* colon delimited */
	if ((p = STRCHR(path, ':')) == NULL)
	    p = STRCHR(path, '\0');

	n = (p - path);
	if (*p != '\0')
	    p++;

	if (n > 0 && n <= maxpath) {
	    STRNCPY(name, path, n);
	    if (name[n - 1] != '/')
		name[n++] = '/';
	    name[n] = '\0';
	    STRNCAT(name, file, len);

	    if (!access(name, R_OK))
		return STRDUP(name);
	    if (ext) {
		STRCAT(name, ext);
		if (!access(name, R_OK))
		    return STRDUP(name);
	    }
	}
    }
    return NULL;
}


/* EXTPROTO */
char           *
rxvt_File_find(const char *file, const char *ext, const char *path)
{
    char           *f;

    if (file == NULL || *file == '\0')
	return NULL;

/* search environment variables here too */
    if ((f = rxvt_File_search_path(path, file, ext)) == NULL)
#ifdef PATH_ENV
	if ((f = rxvt_File_search_path(getenv(PATH_ENV), file, ext)) == NULL)
#endif
	    f = rxvt_File_search_path(getenv("PATH"), file, ext);

#ifdef DEBUG_SEARCH_PATH
    if (f)
	fprintf(stderr, "found: \"%s\"\n", f);
#endif

    return f;
}
#endif				/* defined (BACKGROUND_IMAGE) || (MENUBAR_MAX) */


/*----------------------------------------------------------------------*
 * miscellaneous drawing routines
 */

# define CHOOSE_GC_FG(DISP, MYGC, PIXCOL)	\
	XSetForeground ((DISP), (MYGC), (PIXCOL))

/*
 * Draw top/left and bottom/right border shadows around windows
 */
#if defined(RXVT_SCROLLBAR) || defined(HAVE_MENUBAR)
/* EXTPROTO */
void
rxvt_draw_shadow (Display *Xdisplay, Window win, GC gc, unsigned long topShadow, unsigned long botShadow, int x, int y, int w, int h)
{
    int             shadow;

    shadow = (w == 0 || h == 0) ? 1 : SHADOW;
    w += x - 1;
    h += y - 1;
    for (; shadow-- > 0; x++, y++, w--, h--) {
		CHOOSE_GC_FG(Xdisplay, gc, topShadow);
		XDrawLine(Xdisplay, win, gc, x, y, w, y);
		XDrawLine(Xdisplay, win, gc, x, y, x, h);

		CHOOSE_GC_FG(Xdisplay, gc, botShadow);
		XDrawLine(Xdisplay, win, gc, w, h, w, y + 1);
		XDrawLine(Xdisplay, win, gc, w, h, x + 1, h);
    }
}
#endif


/* button shapes */
#ifdef HAVE_MENUBAR
/* EXTPROTO */
void
rxvt_draw_triangle (Display *Xdisplay, Window win, GC gc, unsigned long topShadow, unsigned long botShadow, int x, int y, int w, int type)
{
    switch (type) {
    case 'r':			/* right triangle */
		CHOOSE_GC_FG(Xdisplay, gc, topShadow);
		XDrawLine(Xdisplay, win, gc, x, y, x, y + w);
		XDrawLine(Xdisplay, win, gc, x, y, x + w, y + w / 2);

		CHOOSE_GC_FG(Xdisplay, gc, botShadow);
		XDrawLine(Xdisplay, win, gc, x, y + w, x + w, y + w / 2);
		break;

    case 'l':			/* left triangle */
		CHOOSE_GC_FG(Xdisplay, gc, botShadow);
		XDrawLine(Xdisplay, win, gc, x + w, y + w, x + w, y);
		XDrawLine(Xdisplay, win, gc, x + w, y + w, x, y + w / 2);

		CHOOSE_GC_FG(Xdisplay, gc, topShadow);
		XDrawLine(Xdisplay, win, gc, x, y + w / 2, x + w, y);
		break;

    case 'd':			/* down triangle */
		CHOOSE_GC_FG(Xdisplay, gc, topShadow);
		XDrawLine(Xdisplay, win, gc, x, y, x + w / 2, y + w);
		XDrawLine(Xdisplay, win, gc, x, y, x + w, y);

		CHOOSE_GC_FG(Xdisplay, gc, botShadow);
		XDrawLine(Xdisplay, win, gc, x + w, y, x + w / 2, y + w);
		break;

    case 'u':			/* up triangle */
		CHOOSE_GC_FG(Xdisplay, gc, botShadow);
		XDrawLine(Xdisplay, win, gc, x + w, y + w, x + w / 2, y);
		XDrawLine(Xdisplay, win, gc, x + w, y + w, x, y + w);

		CHOOSE_GC_FG(Xdisplay, gc, topShadow);
		XDrawLine(Xdisplay, win, gc, x, y + w, x + w / 2, y);
		break;
    }
}
#endif
/*----------------------- end-of-file (C source) -----------------------*/
