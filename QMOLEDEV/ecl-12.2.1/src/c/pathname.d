/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    pathname.d -- Pathnames.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/*
	O.S. DEPENDENT

	This file contains those functions that interpret namestrings.
*/

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>

typedef int (*delim_fn)(int);

/*
 * Translates a string into the host's preferred case.
 * See CLHS 19.2.2.1.2.2 Common Case in Pathname Components.
 */
/* We use UN*X conventions, so lower case is default.
 * However, this really should be conditionalised to the OS type,
 * and it should translate to _opposite_ of the local case.
 */

static cl_object
normalize_case(cl_object path, cl_object cas)
{
        if (cas == @':local') {
                if (path->pathname.logical)
                        return @':upcase';
                return @':downcase';
        } else if (cas == @':common' || cas == @':downcase' || cas == @':upcase') {
                return cas;
        } else {
                FEerror("Not a valid pathname case :~%~A", 1, cas);
        }
}

static bool
in_local_case_p(cl_object str, cl_object cas)
{
        if (cas == @':downcase')
                return ecl_string_case(str) < 0;
        return 1;
}

static bool
in_antilocal_case_p(cl_object str, cl_object cas)
{
        if (cas == @':downcase')
                return ecl_string_case(str) > 0;
        return 0;
}

static cl_object
ensure_local_case(cl_object str, cl_object cas)
{
        if (cas == @':downcase')
                return str;
        return cl_string_upcase(1, str);
}

static cl_object
to_local_case(cl_object str, cl_object cas)
{
        if (cas == @':downcase')
                return cl_string_downcase(1, str);
        return cl_string_upcase(1, str);
}

static cl_object
host_case(cl_object host)
{
        if (Null(host))
                return @':local';
        if (ecl_logical_hostname_p(host))
                return @':upcase';
        return @':downcase';
}

static cl_object
to_antilocal_case(cl_object str, cl_object cas)
{
        if (cas == @':downcase')
                return cl_string_upcase(1, str);
        return cl_string_upcase(1, str);
}

static cl_object
translate_from_common(cl_object str, cl_object tocase)
{
	int string_case = ecl_string_case(str);
	if (string_case > 0) { /* ALL_UPPER */
		return to_local_case(str, tocase);
	} else if (string_case < 0) { /* ALL_LOWER */
		return to_antilocal_case(str, tocase);
	} else { /* Mixed case goes unchanged */
		return str;
	}
}

static cl_object
translate_to_common(cl_object str, cl_object fromcase)
{
	if (in_local_case_p(str, fromcase)) {
		return cl_string_upcase(1, str);
	} else if (in_antilocal_case_p(str, fromcase)) {
		return cl_string_downcase(1, str);
	} else {
		return str;
	}
}

static cl_object
translate_component_case(cl_object str, cl_object fromcase, cl_object tocase)
{
        /* Pathnames may contain some other objects, such as symbols,
         * numbers, etc, which need not be translated */
        if (str == OBJNULL) {
                return str;
        } else if (!ECL_BASE_STRING_P(str)) {
#ifdef ECL_UNICODE
                if (ECL_EXTENDED_STRING_P(str) && ecl_fits_in_base_string(str)) {
                        str = si_coerce_to_base_string(str);
                        return translate_component_case(str, fromcase, tocase);
                }
#endif
                return str;
        } else if (tocase == fromcase) {
                return str;
        } else if (tocase == @':common') {
		return translate_to_common(str, fromcase);
	} else if (fromcase == @':common') {
		return translate_from_common(str, tocase);
	} else {
                str = translate_to_common(str, fromcase);
                return translate_from_common(str, tocase);
        }
}

static cl_object
translate_list_case(cl_object list, cl_object fromcase, cl_object tocase)
{
	/* If the argument is really a list, translate all strings in it and
	 * return this new list, else assume it is a string and translate it.
	 */
	if (!CONSP(list)) {
		return translate_component_case(list, fromcase, tocase);
	} else {
		cl_object l;
		list = cl_copy_list(list);
		for (l = list; !ecl_endp(l); l = CDR(l)) {
			/* It is safe to pass anything to translate_component_case,
			 * because it will only transform strings, leaving other
			 * object (such as symbols) unchanged.*/
			cl_object name = ECL_CONS_CAR(l);
                        name = ECL_LISTP(name)?
                                translate_list_case(name, fromcase, tocase) :
                                translate_component_case(name, fromcase, tocase);
			ECL_RPLACA(l, name);
		}
		return list;
	}
}

static void
push_substring(cl_object buffer, cl_object string, cl_index start, cl_index end)
{
	string = cl_string(string);
	while (start < end) {
		ecl_string_push_extend(buffer, ecl_char(string, start));
		start++;
	}
}

static void
push_string(cl_object buffer, cl_object string)
{
	push_substring(buffer, string, 0, ecl_length(string));
}

static cl_object
destructively_check_directory(cl_object directory, bool logical)
{
	/* This function performs two tasks
	 * 1) It ensures that the list is a valid directory list
	 * 2) It ensures that all strings in the list are valid C strings without fill pointer
	 *    All strings are copied, thus avoiding problems with the user modifying the
	 *    list that was passed to MAKE-PATHNAME.
         * 3) Redundant :back are removed.
	 */
	/* INV: directory is always a list */
	cl_object ptr;
	int i;

	if (!LISTP(directory))
		return @':error';
	if (Null(directory))
		return directory;
	if (ECL_CONS_CAR(directory) != @':absolute' &&
	    ECL_CONS_CAR(directory) != @':relative')
		return @':error';
 BEGIN:
	for (i=0, ptr=directory; CONSP(ptr); ptr = ECL_CONS_CDR(ptr), i++) {
		cl_object item = ECL_CONS_CAR(ptr);
		if (item == @':back') {
			if (i == 0)
				return @':error';
			item = ecl_nth(i-1, directory);
			if (item == @':absolute' || item == @':wild-inferiors')
				return @':error';
			if (i >= 2) {
                                cl_object next = ECL_CONS_CDR(ptr);
                                ptr = ecl_nthcdr(i-2, directory);
				ECL_RPLACD(ptr, next);
                                i--;
                        }
		} else if (item == @':up') {
			if (i == 0)
				return @':error';
			item = ecl_nth(i-1, directory);
			if (item == @':absolute' || item == @':wild-inferiors')
				return @':error';
		} else if (item == @':relative' || item == @':absolute') {
			if (i > 0)
				return @':error';
		} else if (ecl_stringp(item)) {
			cl_index l = ecl_length(item);
#ifdef ECL_UNICODE
			if (ecl_fits_in_base_string(item)) {
				item = si_copy_to_simple_base_string(item);
			} else
#endif
				item = cl_copy_seq(item);
			ECL_RPLACA(ptr, item);
			if (logical)
				continue;
			if (l && ecl_char(item,0) == '.') {
				if (l == 1) {
					/* Single dot */
					if (i == 0)
						return @':error';
					ECL_RPLACD(ecl_nthcdr(--i, directory),
						   ECL_CONS_CDR(ptr));
				} else if (l == 2 && ecl_char(item,1) == '.') {
					ECL_RPLACA(ptr, @':up');
					goto BEGIN;
				}
			}
		} else if (item != @':wild' && item != @':wild-inferiors') {
			return @':error';
		}
	}
	return directory;
}

cl_object
ecl_make_pathname(cl_object host, cl_object device, cl_object directory,
		  cl_object name, cl_object type, cl_object version,
                  cl_object fromcase)
{
	cl_object x, p, component;
        cl_object (*translator)(cl_object);

	p = ecl_alloc_object(t_pathname);
	if (ecl_stringp(host))
		p->pathname.logical = ecl_logical_hostname_p(host);
	else if (host == Cnil)
		p->pathname.logical = FALSE;
	else {
		x = directory;
		component = @':host';
		goto ERROR;
	}
	if (device != Cnil && device != @':unspecific' &&
	    !(!p->pathname.logical && ecl_stringp(device))) {
		x = device;
		component = @':device';
		goto ERROR;
	}
	if (name != Cnil && name != @':wild' && !ecl_stringp(name)) {
		x = name;
		component = @':name';
		goto ERROR;
	}
	if (type != Cnil && type != @':unspecific' && type != @':wild' && !ecl_stringp(type)) {
		x = type;
		component = @':type';
		goto ERROR;
	}
	if (version != @':unspecific' && version != @':newest' &&
	    version != @':wild' && version != Cnil && !FIXNUMP(version))
	{
		x = version;
		component = @':version';
	ERROR:	FEerror("~s is not a valid pathname-~a component", 2, x, component);
	}
	switch (type_of(directory)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		directory = cl_list(2, @':absolute', directory);
		break;
	case t_symbol:
		if (directory == @':wild') {
			directory = cl_list(2, @':absolute', @':wild-inferiors');
			break;
		}
		x = directory;
		component = @':directory';
		goto ERROR;
	case t_list:
		directory = cl_copy_list(directory);
		break;
	default:
		x = directory;
		component = @':directory';
		goto ERROR;
	}
        p->pathname.host = host;
        {
                cl_object tocase = normalize_case(p, @':local');
                if (p->pathname.logical)
                        fromcase = @':common';
                else
                        fromcase = normalize_case(p, fromcase);
                p->pathname.host =
                        translate_component_case(host, fromcase, tocase);
                p->pathname.device =
                        translate_component_case(device, fromcase, tocase);
                p->pathname.directory =
			directory =
                        translate_list_case(directory, fromcase, tocase);
                p->pathname.name =
                        translate_component_case(name, fromcase, tocase);
                p->pathname.type =
                        translate_component_case(type, fromcase, tocase);
                p->pathname.version = version;
        }
	directory = destructively_check_directory(directory, p->pathname.logical);
        unlikely_if (directory == @':error') {
		cl_error(3, @'file-error', @':pathname', p);
	}
	p->pathname.directory = directory;
	return(p);
}

static cl_object
tilde_expand(cl_object pathname)
{
	/*
	 * If the pathname is a physical one, without hostname, without device
	 * and the first element is either a tilde '~' or '~' followed by
	 * a user name, we merge the user homedir pathname with this one.
	 */
	cl_object directory, head;
	if (pathname->pathname.logical || pathname->pathname.host != Cnil
	    || pathname->pathname.device != Cnil) {
		return pathname;
	}
	directory = pathname->pathname.directory;
	if (!CONSP(directory) || ECL_CONS_CAR(directory) != @':relative'
	    || ECL_CONS_CDR(directory) == Cnil) {
		return pathname;
	}
	head = CADR(directory);
	if (ecl_stringp(head) && ecl_length(head) > 0 && 
	    ecl_char(head,0) == '~') {
		/* Remove the tilde component */
		ECL_RPLACD(directory, CDDR(directory));
		pathname = cl_merge_pathnames(2, pathname,
					      ecl_homedir_pathname(head));
	}
	return pathname;
}

#define WORD_INCLUDE_DELIM 1
#define WORD_ALLOW_ASTERISK  2
#define WORD_EMPTY_IS_NIL 4
#define WORD_LOGICAL 8
#define WORD_SEARCH_LAST_DOT 16
#define WORD_ALLOW_LEADING_DOT 32
#define WORD_DISALLOW_SLASH 64
#define WORD_DISALLOW_SEMICOLON 128

static cl_object
make_one(cl_object s, cl_index start, cl_index end)
{
	return cl_subseq(3, s, MAKE_FIXNUM(start), MAKE_FIXNUM(end));
}

static int is_colon(int c) { return c == ':'; }
static int is_slash(int c) { return IS_DIR_SEPARATOR(c); }
static int is_semicolon(int c) { return c == ';'; }
static int is_dot(int c) { return c == '.'; }
static int is_null(int c) { return c == '\0'; }

/*
 * Parses a word from string `S' until either:
 *	1) character `DELIM' is found
 *	2) end of string is reached
 *	3) a non valid character is found
 * Output is either
 *	1) :error in case (3) above
 *	2) :wild, :wild-inferiors, :up
 *	3) "" or Cnil when word has no elements
 *	5) A non empty string
 */
static cl_object
parse_word(cl_object s, delim_fn delim, int flags, cl_index start,
	   cl_index end, cl_index *end_of_word)
{
	cl_index i, j, last_delim = end;
	bool wild_inferiors = FALSE;

	i = j = start;
	for (; i < end; i++) {
		bool valid_char;
		cl_index c = ecl_char(s, i);
		if (delim(c)) {
			if ((i == start) && (flags & WORD_ALLOW_LEADING_DOT)) {
				/* Leading dot is included */
				continue;
			}
			last_delim = i;
			if (!(flags & WORD_SEARCH_LAST_DOT)) {
				break;
			}
		}
		if (c == '*') {
			if (!(flags & WORD_ALLOW_ASTERISK))
				valid_char = FALSE; /* Asterisks not allowed in this word */
			else {
				wild_inferiors = (i > start && ecl_char(s, i-1) == '*');
				valid_char = TRUE; /* single "*" */
			}
		} else if (c == ';' && (flags & WORD_DISALLOW_SEMICOLON)) {
			valid_char = 0;
		} else if (c == '/' && (flags & WORD_DISALLOW_SLASH)) {
			valid_char = 0;
		} else {
			valid_char = c != 0;
		}
		if (!valid_char) {
			*end_of_word = start;
			return @':error';
		}
	}
	if (i > last_delim) {
		/* Go back to the position of the last delimiter */
		i = last_delim;
	}
	if (i < end) {
		*end_of_word = i+1;
	} else {
		*end_of_word = end;
		/* We have reached the end of the string without finding
		   the proper delimiter */
		if (flags & WORD_INCLUDE_DELIM) {
			*end_of_word = start;
			return Cnil;
		}
	}
	switch(i-j) {
	case 0:
		if (flags & WORD_EMPTY_IS_NIL)
			return Cnil;
		return cl_core.null_string;
	case 1:
		if (ecl_char(s,j) == '*')
			return @':wild';
		break;
	case 2: {
		cl_index c0 = ecl_char(s,j);
		cl_index c1 = ecl_char(s,j+1);
		if (c0 == '*' && c1 == '*')
			return @':wild-inferiors';
		if (!(flags & WORD_LOGICAL) && c0 == '.' && c1 == '.')
			return @':up';
		break;
	}
	default:
		if (wild_inferiors)	/* '**' surrounded by other characters */
			return @':error';
	}
	return make_one(s, j, i);
}

/*
 * Parses a logical or physical directory tree. Output is always a
 * list of valid directory components, which may be just NIL.
 *
 * INV: When parsing of directory components has failed, a valid list
 * is also returned, and it will be later in the parsing of
 * pathname-name or pathname-type when the same error is detected.
 */

static cl_object
parse_directories(cl_object s, int flags, cl_index start, cl_index end,
		  cl_index *end_of_dir)
{
	cl_index i, j;
	cl_object path = Cnil;
	delim_fn delim = (flags & WORD_LOGICAL) ? is_semicolon : is_slash;

	flags |= WORD_INCLUDE_DELIM | WORD_ALLOW_ASTERISK;
	*end_of_dir = start;
	for (i = j = start; i < end; j = i) {
		cl_object part = parse_word(s, delim, flags, j, end, &i);
		if (part == @':error' || part == Cnil)
			break;
		if (part == cl_core.null_string) {  /* "/", ";" */
			if (j != start) {
				if (flags & WORD_LOGICAL)
					return @':error';
				*end_of_dir = i;
				continue;
			}
			part = (flags & WORD_LOGICAL) ? @':relative' : @':absolute';
		}
		*end_of_dir = i;
		path = ecl_cons(part, path);
	}
	return cl_nreverse(path);
}

bool
ecl_logical_hostname_p(cl_object host)
{
	if (!ecl_stringp(host))
		return FALSE;
	return !Null(@assoc(4, host, cl_core.pathname_translations, @':test', @'string-equal'));
}

/*
 * Parses a lisp namestring until the whole substring is parsed or an
 * error is found. It returns a valid pathname or NIL, plus the place
 * where parsing ended in *END_OF_PARSING.
 *
 * The rules are as follows:
 *
 * 1) If a hostname is supplied it determines whether the namestring
 *    will be parsed as logical or as physical.
 *
 * 2) If no hostname is supplied, first it tries parsing using logical
 *    pathname rules and, if no logical hostname is found, then it
 *    tries the physical pathname format.
 *
 * 3) Logical pathname syntax:
 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
 *
 * 4) Physical pathname syntax:
 *	[device:][[//hostname]/][directory-component/]*[pathname-name][.pathname-type]
 *
 *	logical-hostname, device, hostname = word
 *	logical-directory-component = word | wildcard-word
 *	directory-component = word | wildcard-word | '..' | '.'
 *	pathname-name, pathname-type = word | wildcard-word | ""
 *
 */
cl_object
ecl_parse_namestring(cl_object s, cl_index start, cl_index end, cl_index *ep,
		     cl_object default_host)
{
	cl_object host, device, path, name, type, aux, version;
	bool logical;

	if (start == end) {
		host = device = path = name = type = aux = version = @'nil';
		logical = 0;
		goto make_it;
	}
	/* We first try parsing as logical-pathname. In case of
	 * failure, physical-pathname parsing is performed only when
	 * there is no supplied *logical* host name. All other failures
	 * result in Cnil as output.
	 */
	host = parse_word(s, is_colon, WORD_LOGICAL | WORD_INCLUDE_DELIM |
			  WORD_DISALLOW_SEMICOLON, start, end, ep);
	if (default_host != Cnil) {
		if (host == Cnil || host == @':error')
			host = default_host;
	}
	if (!ecl_logical_hostname_p(host))
		goto physical;
	/*
	 * Logical pathname format:
	 *	[logical-hostname:][;][logical-directory-component;][pathname-name][.pathname-type]
	 */
	logical = TRUE;
	device = @':unspecific';
	path = parse_directories(s, WORD_LOGICAL, *ep, end, ep);
	if (CONSP(path)) {
		if (ECL_CONS_CAR(path) != @':relative' &&
		    ECL_CONS_CAR(path) != @':absolute')
			path = CONS(@':absolute', path);
		path = destructively_check_directory(path, TRUE);
	} else {
		path = CONS(@':absolute', path);
	}
	if (path == @':error')
		return Cnil;
	name = parse_word(s, is_dot, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			  WORD_EMPTY_IS_NIL, *ep, end, ep);
	if (name == @':error')
		return Cnil;
	type = Cnil;
	version = Cnil;
	if (*ep == start || ecl_char(s, *ep-1) != '.')
		goto make_it;
	type = parse_word(s, is_dot, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			  WORD_EMPTY_IS_NIL, *ep, end, ep);
	if (type == @':error')
		return Cnil;
	if (*ep == start || ecl_char(s, *ep-1) != '.')
		goto make_it;
	aux = parse_word(s, is_null, WORD_LOGICAL | WORD_ALLOW_ASTERISK |
			 WORD_EMPTY_IS_NIL, *ep, end, ep);
	if (aux == @':error') {
		return Cnil;
	} else if (SYMBOLP(aux)) {
		version = aux;
	} else {
		version = cl_parse_integer(3, aux, @':junk-allowed', Ct);
		if (cl_integerp(version) != Cnil && ecl_plusp(version) &&
		    fix(VALUES(1)) == ecl_length(aux))
			;
		else if (cl_string_equal(2, aux, @':newest') != Cnil)
			version = @':newest';
		else
			return Cnil;
	}
	goto make_it;
 physical:
	/*
	 * Physical pathname format:
	 *	[[device:[//hostname]]/][directory-component/]*[pathname-name][.pathname-type]
	 */
	logical = FALSE;
	/* We only parse a hostname when the device was present. This
	 * requisite is a bit stupid and only applies to the Unix port,
	 * where "//home/" is equivalent to "/home" However, in Windows
	 * we need "//FOO/" to be separately handled, for it is a shared
	 * resource.
	 */
#if defined(ECL_MS_WINDOWS_HOST)
	if ((start+1 <= end) && is_slash(ecl_char(s, start))) {
		device = Cnil;
		goto maybe_parse_host;
	}
#endif
	device = parse_word(s, is_colon, WORD_INCLUDE_DELIM | WORD_EMPTY_IS_NIL |
			    WORD_DISALLOW_SLASH, start, end, ep);
	if (device == @':error' || device == Cnil) {
		device = Cnil;
		host = Cnil;
		goto done_device_and_host;
	}
	if (!ecl_stringp(device)) {
		return Cnil;
	}
 maybe_parse_host:
	/* Files have no effective device. */
	if (@string-equal(2, device, @':file') == Ct)
		device = Cnil;
	start = *ep;
	host = Cnil;
	if ((start+2) <= end && is_slash(ecl_char(s, start)) &&
	    is_slash(ecl_char(s, start+1)))
	{
		host = parse_word(s, is_slash, WORD_EMPTY_IS_NIL,
				  start+2, end, ep);
		if (host == @':error') {
			host = Cnil;
		} else if (host != Cnil) {
			if (!ecl_stringp(host))
				return Cnil;
			start = *ep;
			if (is_slash(ecl_char(s,--start)))
				*ep = start;
		}
	}
	if (ecl_length(device) == 0)
		device = Cnil;
 done_device_and_host:
	path = parse_directories(s, 0, *ep, end, ep);
	if (CONSP(path)) {
		if (ECL_CONS_CAR(path) != @':relative' &&
		    ECL_CONS_CAR(path) != @':absolute')
			path = CONS(@':relative', path);
		path = destructively_check_directory(path, FALSE);
	}
	if (path == @':error')
		return Cnil;
	start = *ep;
	name = parse_word(s, is_dot,
			  WORD_ALLOW_LEADING_DOT | WORD_SEARCH_LAST_DOT |
			  WORD_ALLOW_ASTERISK | WORD_EMPTY_IS_NIL,
			  start, end, ep);
	if (name == @':error')
		return Cnil;
	if ((*ep - start) <= 1 || ecl_char(s, *ep-1) != '.') {
		type = Cnil;
	} else {
		type = parse_word(s, is_null, WORD_ALLOW_ASTERISK, *ep, end, ep);
		if (type == @':error')
			return Cnil;
	}
	version = (name != Cnil || type != Cnil) ? @':newest' : Cnil;
 make_it:
	if (*ep >= end) *ep = end;
	path = ecl_make_pathname(host, device, path, name, type, version,
                                 @':local');
	path->pathname.logical = logical;
	return tilde_expand(path);
}

cl_object
si_default_pathname_defaults(void)
{
	/* This routine outputs the value of *default-pathname-defaults*
	 * coerced to type PATHNAME. Special care is taken so that we do
	 * not enter an infinite loop when using PARSE-NAMESTRING, because
	 * this routine might itself try to use the value of this variable. */
	cl_object path = ecl_symbol_value(@'*default-pathname-defaults*');
	unlikely_if (!ECL_PATHNAMEP(path)) {
		const cl_env_ptr the_env = ecl_process_env();
		ecl_bds_bind(the_env, @'*default-pathname-defaults*', si_getcwd(0));
                FEwrong_type_key_arg(@[pathname], @[*default-pathname-defaults*],
                                     path, @'pathname');
	}
	@(return path)
}

cl_object
cl_pathname(cl_object x)
{
L:
	switch (type_of(x)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		x = cl_parse_namestring(1, x);
	case t_pathname:
		break;
	case t_stream:
		switch ((enum ecl_smmode)x->stream.mode) {
		case smm_input:
		case smm_output:
		case smm_probe:
		case smm_io:
		case smm_input_file:
		case smm_output_file:
		case smm_io_file:
			x = IO_STREAM_FILENAME(x);
			goto L;
		case smm_synonym:
			x = SYNONYM_STREAM_STREAM(x);
			goto L;
		default:
			;/* Fall through to error message */
		}
	default: {
                const char *type = "(OR FILE-STREAM STRING PATHNAME)";
                FEwrong_type_only_arg(@[pathname], x, ecl_read_from_cstring(type));
        }
	}
	@(return x)
}

cl_object
cl_logical_pathname(cl_object x)
{
	x = cl_pathname(x);
	if (!x->pathname.logical) {
		cl_error(9, @'simple-type-error', @':format-control',
			 make_constant_base_string("~S cannot be coerced to a logical pathname."),
			 @':format-arguments', cl_list(1, x),
			 @':expected-type', @'logical-pathname',
			 @':datum', x);
	}
	@(return x);
}

/* FIXME! WILD-PATHNAME-P is missing! */
@(defun wild-pathname-p (pathname &optional component)
	bool checked = 0;
@
	pathname = cl_pathname(pathname);
	if (component == Cnil || component == @':host') {
		if (pathname->pathname.host == @':wild')
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':device') {
		if (pathname->pathname.device == @':wild')
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':version') {
		if (pathname->pathname.version == @':wild')
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':name') {
		cl_object name = pathname->pathname.name;
		if (name != Cnil &&
		    (name == @':wild' || (!SYMBOLP(name) && ecl_member_char('*', name))))
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':type') {
		cl_object name = pathname->pathname.type;
		if (name != Cnil &&
		    (name == @':wild' || (!SYMBOLP(name) && ecl_member_char('*', name))))
			@(return Ct);
		checked = 1;
	}
	if (component == Cnil || component == @':directory') {
		cl_object list = pathname->pathname.directory;
		checked = 1;
		loop_for_on_unsafe(list) {
			cl_object name = ECL_CONS_CAR(list);
			if (name != Cnil &&
			    (name == @':wild' || name == @':wild-inferiors' ||
			     (!SYMBOLP(name) && ecl_member_char('*', name))))
			{
				@(return Ct)
			}
		} end_loop_for_on_unsafe(list);
	}
	if (checked == 0) {
		FEerror("~A is not a valid pathname component", 1, component);
	}
	@(return Cnil)
@)

/*
 * coerce_to_file_pathname(P) converts P to a physical pathname,
 * for a file which is accesible in our filesystem.
 * INV: Wildcards are allowed.
 * INV: A fresh new copy of the pathname is created.
 * INV: The pathname is absolute.
 */
cl_object
coerce_to_file_pathname(cl_object pathname)
{
	pathname = coerce_to_physical_pathname(pathname);
	pathname = cl_merge_pathnames(1, pathname);
#if 0
#if !defined(cygwin) && !defined(ECL_MS_WINDOWS_HOST)
	if (pathname->pathname.device != Cnil)
		FEerror("Device ~S not yet supported.", 1,
			pathname->pathname.device);
	if (pathname->pathname.host != Cnil)
		FEerror("Access to remote files not yet supported.", 0);
#endif
#endif
	if (pathname->pathname.directory == Cnil ||
	    ECL_CONS_CAR(pathname->pathname.directory) == @':relative') {
		pathname = cl_merge_pathnames(2, pathname, si_getcwd(0));
	}
	return pathname;
}

/*
 * coerce_to_physical_pathname(P) converts P to a physical pathname,
 * performing the appropiate transformation if P was a logical pathname.
 */
cl_object
coerce_to_physical_pathname(cl_object x)
{
	x = cl_pathname(x);
	if (x->pathname.logical)
		return cl_translate_logical_pathname(1, x);
	return x;
}

/*
 * si_coerce_to_filename(P) converts P to a physical pathname and then to
 * a namestring. The output must always be a new simple-string which can
 * be used by the C library.
 * INV: No wildcards are allowed.
 */
cl_object
si_coerce_to_filename(cl_object pathname_orig)
{
	cl_object namestring, pathname;

	/* We always go through the pathname representation and thus
	 * cl_namestring() always outputs a fresh new string */
	pathname = coerce_to_file_pathname(pathname_orig);
	if (cl_wild_pathname_p(1,pathname) != Cnil)
		cl_error(3, @'file-error', @':pathname', pathname_orig);
	namestring = ecl_namestring(pathname,
                                    ECL_NAMESTRING_TRUNCATE_IF_ERROR |
                                    ECL_NAMESTRING_FORCE_BASE_STRING);
	if (namestring == Cnil) {
		FEerror("Pathname without a physical namestring:"
                        "~% :HOST ~A"
                        "~% :DEVICE ~A"
                        "~% :DIRECTORY ~A"
                        "~% :NAME ~A"
                        "~% :TYPE ~A"
                        "~% :VERSION ~A",
			6, pathname_orig->pathname.host,
                        pathname_orig->pathname.device,
                        pathname_orig->pathname.directory,
                        pathname_orig->pathname.name,
                        pathname_orig->pathname.type,
                        pathname_orig->pathname.version);
	}
	if (cl_core.path_max != -1 &&
	    ecl_length(namestring) >= cl_core.path_max - 16)
		FEerror("Too long filename: ~S.", 1, namestring);
	return namestring;
}

#define default_device(host) Cnil

cl_object
ecl_merge_pathnames(cl_object path, cl_object defaults, cl_object default_version)
{
	cl_object host, device, directory, name, type, version;
        cl_object tocase;

	defaults = cl_pathname(defaults);
	path = cl_parse_namestring(1, path, Cnil, defaults);
	if (Null(host = path->pathname.host))
		host = defaults->pathname.host;
        tocase = host_case(host);
	if (Null(path->pathname.device)) {
		if (Null(path->pathname.host))
			device = cl_pathname_device(3, defaults, @':case', tocase);
		else if (path->pathname.host == defaults->pathname.host)
			device = defaults->pathname.device;
		else
			device = default_device(path->pathname.host);
	} else {
		device = path->pathname.device;
        }
	if (Null(path->pathname.directory)) {
                directory = cl_pathname_directory(3, defaults, @':case', tocase);
        } else if (ECL_CONS_CAR(path->pathname.directory) == @':absolute') {
		directory = path->pathname.directory;
        } else if (!Null(defaults->pathname.directory)) {
		directory = ecl_append(cl_pathname_directory(3, defaults,
                                                             @':case', tocase),
                                       CDR(path->pathname.directory));
                /* Eliminate redundant :back */
                directory = destructively_check_directory(directory, 1);
        } else {
		directory = path->pathname.directory;
        }
	if (Null(name = path->pathname.name)) {
		name = cl_pathname_name(3, defaults, @':case', tocase);
        }
	if (Null(type = path->pathname.type)) {
		type = cl_pathname_type(3, defaults, @':case', tocase);
        }
	version = path->pathname.version;
	if (Null(path->pathname.name)) {
		if (Null(version))
			version = defaults->pathname.version;
	}
	if (Null(version)) {
		version = default_version;
	}
        if (default_version == @':default') {
                if (Null(name) && Null(type)) {
                        version = Cnil;
                } else {
                        version = @':newest';
                }
        }
	/*
		In this implementation, version is not considered
	*/
	defaults = ecl_make_pathname(host, device, directory, name,
                                     type, version, tocase);
	return defaults;
}

/*
	ecl_namestring(x, flag) converts a pathname to a namestring.
	if flag is true, then the pathname may be coerced to the requirements
	of the filesystem, removing fields that have no meaning (such as
	version, or type, etc); otherwise, when it is not possible to
	produce a readable representation of the pathname, NIL is returned.
*/
cl_object
ecl_namestring(cl_object x, int flags)
{
	bool logical;
	cl_object l, y;
	cl_object buffer, host;
        bool truncate_if_unreadable = flags & ECL_NAMESTRING_TRUNCATE_IF_ERROR;

	x = cl_pathname(x);

	/* INV: Pathnames can only be created by mergin, parsing namestrings
	 * or using ecl_make_pathname(). In all of these cases ECL will complain
	 * at creation time if the pathname has wrong components.
	 */
	buffer = ecl_make_string_output_stream(128, 1);
	logical = x->pathname.logical;
	host = x->pathname.host;
	if (logical) {
		if ((y = x->pathname.device) != @':unspecific' &&
		    truncate_if_unreadable)
			return Cnil;
		if (host != Cnil) {
			si_do_write_sequence(host, buffer, MAKE_FIXNUM(0), Cnil);
			writestr_stream(":", buffer);
		}
	} else {
		if ((y = x->pathname.device) != Cnil) {
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
			writestr_stream(":", buffer);
		}
		if (host != Cnil) {
#if !defined(ECL_MS_WINDOWS_HOST)
			if (y == Cnil) {
				writestr_stream("file:", buffer);
			}
#endif
			writestr_stream("//", buffer);
			si_do_write_sequence(host, buffer, MAKE_FIXNUM(0), Cnil);
		}
	}
	l = x->pathname.directory;
	if (ecl_endp(l))
		goto NO_DIRECTORY;
	y = ECL_CONS_CAR(l);
	if (y == @':relative') {
		if (logical)
			ecl_write_char(';', buffer);
	} else {
		if (!logical)
			ecl_write_char(DIR_SEPARATOR, buffer);
	}
	l = ECL_CONS_CDR(l);
	loop_for_in(l) {
		y = ECL_CONS_CAR(l);
		if (y == @':up') {
			writestr_stream("..", buffer);
		} else if (y == @':wild') {
			writestr_stream("*", buffer);
		} else if (y == @':wild-inferiors') {
			writestr_stream("**", buffer);
		} else if (y != @':back') {
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
		} else {
			/* Directory :back has no namestring representation */
			return Cnil;
		}
		ecl_write_char(logical? ';' : DIR_SEPARATOR, buffer);
	} end_loop_for_in;
NO_DIRECTORY:
	if (ecl_file_position(buffer) == MAKE_FIXNUM(0)) {
		if ((ecl_stringp(x->pathname.name) &&
		     ecl_member_char(':', x->pathname.name)) ||
		    (ecl_stringp(x->pathname.type) &&
		     ecl_member_char(':', x->pathname.type)))
			writestr_stream(":", buffer);
	}
	y = x->pathname.name;
	if (y != Cnil) {
		if (y == @':wild') {
			writestr_stream("*", buffer);
		} else {
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
		}
	} else if (!logical && !Null(x->pathname.type)) {
                /* #P".txt" is :NAME = ".txt" :TYPE = NIL and
                   hence :NAME = NIL and :TYPE != NIL does not have
                   a printed representation */
                return Cnil;
        }
	y = x->pathname.type;
        if (y == @':unspecific') {
                return Cnil;
        } else if (y != Cnil) {
		if (y == @':wild') {
			writestr_stream(".*", buffer);
		} else {
			writestr_stream(".", buffer);
			si_do_write_sequence(y, buffer, MAKE_FIXNUM(0), Cnil);
		}
	}
	y = x->pathname.version;
	if (logical) {
		if (y != Cnil) {
			writestr_stream(".", buffer);
			if (y == @':wild') {
				writestr_stream("*", buffer);
			} else if (y == @':newest') {
				si_do_write_sequence(ecl_symbol_name(y), buffer,
						     MAKE_FIXNUM(0), Cnil);
			} else {
				/* Since the printer is not reentrant,
				 * we cannot use cl_write and friends.
				 */
				int n = fix(y), i;
				char b[FIXNUM_BITS/2];
				for (i = 0; n; i++) {
					b[i] = n%10 + '0';
					n = n/10;
				}
				if (i == 0)
					b[i++] = '0';
				while (i--) {
					ecl_write_char(b[i], buffer);
				}
			}
		}
	} else if (!truncate_if_unreadable) {
		/* Namestrings of physical pathnames have restrictions... */
		if (Null(x->pathname.name) && Null(x->pathname.type)) {
			/* Directories cannot have a version number */
			if (y != Cnil)
				return Cnil;
		} else if (y != @':newest') {
			/* Filenames have an implicit version :newest */
			return Cnil;
		}
	}
        buffer = cl_get_output_stream_string(buffer);
#ifdef ECL_UNICODE
	if (ECL_EXTENDED_STRING_P(buffer) &&
            (flags & ECL_NAMESTRING_FORCE_BASE_STRING)) {
		unlikely_if (!ecl_fits_in_base_string(buffer))
			FEerror("The filesystem does not accept filenames "
                                "with extended characters: ~S",
				1, buffer);
		buffer = si_copy_to_simple_base_string(buffer);
	}
#endif
	return buffer;
}

cl_object
cl_namestring(cl_object x)
{
	@(return ecl_namestring(x, ECL_NAMESTRING_TRUNCATE_IF_ERROR))
}

@(defun parse_namestring (thing
	&o host (defaults si_default_pathname_defaults())
	&k (start MAKE_FIXNUM(0)) end junk_allowed
	&a output)
@
	if (host != Cnil) {
		host = cl_string(host);
	}
	if (!ecl_stringp(thing)) {
		output = cl_pathname(thing);
	} else {
		cl_object default_host = host;
                cl_index_pair p;
                cl_index ee;
		if (default_host == Cnil && defaults != Cnil) {
			defaults = cl_pathname(defaults);
			default_host = defaults->pathname.host;
		}
#ifdef ECL_UNICODE
		thing = si_coerce_to_base_string(thing);
#endif
		p = ecl_vector_start_end(@[parse-namestring], thing, start, end);
		output = ecl_parse_namestring(thing, p.start, p.end, &ee, default_host);
		start = MAKE_FIXNUM(ee);
		if (output == Cnil || ee != p.end) {
			if (Null(junk_allowed)) {
				FEparse_error("Cannot parse the namestring ~S~%"
					      "from ~S to ~S.", Cnil,
					      3, thing, start, end);
			}
			goto OUTPUT;
		}
	}
	if (host != Cnil && !ecl_equal(output->pathname.host, host)) {
		FEerror("The pathname ~S does not contain the required host ~S.",
			2, thing, host);
	}
  OUTPUT:
	@(return output start)
@)

@(defun merge_pathnames (path
	&o (defaults si_default_pathname_defaults())
 	   (default_version @':newest'))
@
	path = cl_pathname(path);
	defaults = cl_pathname(defaults);
	@(return ecl_merge_pathnames(path, defaults, default_version))
@)

@(defun make_pathname (&key (host Cnil hostp) (device Cnil devicep)
		            (directory Cnil directoryp)
			    (name Cnil namep) (type Cnil typep) (version Cnil versionp)
		            ((:case scase) @':local')
		            defaults
		       &aux x)
@
	if (Null(defaults)) {
		defaults = si_default_pathname_defaults();
		defaults = ecl_make_pathname(defaults->pathname.host,
					     Cnil, Cnil, Cnil, Cnil, Cnil,
                                             @':local');
	} else {
		defaults = cl_pathname(defaults);
	}
	if (!hostp) host = defaults->pathname.host;
	x = ecl_make_pathname(host, device, directory, name, type, version, scase);
	if (!devicep) x->pathname.device = defaults->pathname.device;
	if (!directoryp) x->pathname.directory = defaults->pathname.directory;
	if (!namep) x->pathname.name = defaults->pathname.name;
	if (!typep) x->pathname.type = defaults->pathname.type;
	if (!versionp) x->pathname.version = defaults->pathname.version;

	@(return x)
@)

cl_object
cl_pathnamep(cl_object pname)
{
	@(return (ECL_PATHNAMEP(pname) ? Ct : Cnil))
}

cl_object
si_logical_pathname_p(cl_object pname)
{
	@(return ((ECL_PATHNAMEP(pname) && pname->pathname.logical)?
		  Ct : Cnil))
}

@(defun pathname_host (pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
	@(return translate_component_case(pname->pathname.host,
                                          normalize_case(pname, @':local'),
                                          normalize_case(pname, scase)))
@)

@(defun pathname_device (pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
	@(return translate_component_case(pname->pathname.device,
                                          normalize_case(pname, @':local'),
                                          normalize_case(pname, scase)))
@)

@(defun pathname_directory (pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
        @(return translate_list_case(pname->pathname.directory,
                                     normalize_case(pname, @':local'),
                                     normalize_case(pname, scase)))
@)

@(defun pathname_name(pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
	@(return translate_component_case(pname->pathname.name,
                                          normalize_case(pname, @':local'),
                                          normalize_case(pname, scase)))
@)

@(defun pathname_type(pname &key ((:case scase) @':local'))
@
	pname = cl_pathname(pname);
        @(return translate_component_case(pname->pathname.type,
                                          normalize_case(pname, @':local'),
                                          normalize_case(pname, scase)))
@)

cl_object
cl_pathname_version(cl_object pname)
{
	pname = cl_pathname(pname);
	@(return  pname->pathname.version)
}

cl_object
cl_file_namestring(cl_object pname)
{
	pname = cl_pathname(pname);
	@(return ecl_namestring(ecl_make_pathname(Cnil, Cnil, Cnil,
						  pname->pathname.name,
						  pname->pathname.type,
						  pname->pathname.version,
                                                  @':local'),
				ECL_NAMESTRING_TRUNCATE_IF_ERROR))
}

cl_object
cl_directory_namestring(cl_object pname)
{
	pname = cl_pathname(pname);
	@(return ecl_namestring(ecl_make_pathname(Cnil, Cnil,
						  pname->pathname.directory,
						  Cnil, Cnil, Cnil,
                                                  @':local'),
				ECL_NAMESTRING_TRUNCATE_IF_ERROR))
}

cl_object
cl_host_namestring(cl_object pname)
{
	pname = cl_pathname(pname);
	pname = pname->pathname.host;
	if (Null(pname) || pname == @':wild')
		pname = cl_core.null_string;
	@(return pname)
}

#define EN_MATCH(p1,p2,el) (ecl_equalp(p1->pathname.el, p2->pathname.el)? Cnil : p1->pathname.el)

@(defun enough_namestring (path
	&o (defaults si_default_pathname_defaults()))
	cl_object newpath, pathdir, defaultdir, fname;
@
	defaults = cl_pathname(defaults);
	path = cl_pathname(path);
	pathdir = path->pathname.directory;
	defaultdir = defaults->pathname.directory;
	if (Null(pathdir)) {
		pathdir = ecl_list1(@':relative');
	} else if (Null(defaultdir)) {
		/* The defaults pathname does not have a directory. */
	} else if (ECL_CONS_CAR(pathdir) == @':relative') {
		/* The pathname is relative to the default one one, so we just output the
		   original one */
	} else {
		/* The new pathname is an absolute one. We compare it with the defaults
		   and if they have some common elements, we just output the remaining ones. */
		cl_object dir_begin = funcall(5, @'mismatch', pathdir, defaultdir,
					      @':test', @'equal');
		if (dir_begin == Cnil) {
			pathdir = Cnil;
		} else if (dir_begin == cl_length(defaultdir)) {
			pathdir = funcall(3, @'subseq', pathdir, dir_begin);
			pathdir = CONS(@':relative', pathdir);
		}
	}
	fname = EN_MATCH(path, defaults, name);
	if (fname == Cnil) fname = path->pathname.name;
	/* Create a path with all elements that do not match the default */
	newpath
	= ecl_make_pathname(EN_MATCH(path, defaults, host),
			    EN_MATCH(path, defaults, device),
			    pathdir, fname,
			    EN_MATCH(path, defaults, type),
			    EN_MATCH(path, defaults, version),
                            @':local');
	newpath->pathname.logical = path->pathname.logical;
	@(return ecl_namestring(newpath, ECL_NAMESTRING_TRUNCATE_IF_ERROR))
@)
#undef EN_MATCH

/* --------------- PATHNAME MATCHING ------------------ */

/*
 * Take two C strings and check if the first (s) one matches against
 * the pattern given by the second one (p). The pattern is that of a
 * Unix shell except for brackets and curly braces
 */
bool
ecl_string_match(cl_object s, cl_index j, cl_index ls,
                 cl_object p, cl_index i, cl_index lp)
{
	while (i < lp) {
		cl_index cp = ecl_char(p, i);
                switch (cp) {
                case '*': {
			/* An asterisk in the pattern matches any
			 * number of characters. We try the shortest
			 * sequence that matches. */
			cl_index cn = 0, next;
			for (next = i+1;
			     next < lp && ((cn = ecl_char(p, next)) == '*');
			     next++)
				;
			if (next == lp) {
				return TRUE;
			}
			while (j < ls) {
				if (ecl_string_match(s, j, ls, p, next, lp)) {
					return TRUE;
				}
				j++;
			}
			return FALSE;
                        break;
		}
                case '?':
                        /* Match any character */
                        if (j > ls) return FALSE;
                        i++; j++;
                        break;
                case '\\':
                        /* Interpret a pattern character literally.
                           Trailing slash is interpreted as a slash. */
                        if (++i >= lp) i--;
                default:
                        if ((j >= ls) || (cp != ecl_char(s, j))) {
                                /* Either there are no characters left in "s"
                                 * or the next character does not match. */
                                return FALSE;
                        }
                        i++; j++;
                }
	}
        /* At the end all characters should have been matched */
	return (j >= ls);
}

static bool
path_item_match(cl_object a, cl_object mask) {
	if (mask == @':wild')
		return TRUE;
	/* If a component in the tested path is a wildcard field, this
	   can only be matched by the same wildcard field in the mask */
	if (!ecl_stringp(a) || mask == Cnil)
		return (a == mask);
	if (!ecl_stringp(mask))
		FEerror("~S is not supported as mask for pathname-match-p", 1, mask);
	return ecl_string_match(a, 0, ecl_length(a),
                                mask, 0, ecl_length(mask));
}

static bool
path_list_match(cl_object a, cl_object mask) {
	cl_object item_mask;
	while (!ecl_endp(mask)) {
		item_mask = CAR(mask);
		mask = CDR(mask);
		if (item_mask == @':wild-inferiors') {
			if (ecl_endp(mask))
				return TRUE;
			while (!ecl_endp(a)) {
				if (path_list_match(a, mask))
					return TRUE;
				a = CDR(a);
			}
			return FALSE;
		} else if (ecl_endp(a)) {
			/* A NIL directory should match against :absolute
			   or :relative, in order to perform suitable translations. */
			if (item_mask != @':absolute' && item_mask != @':relative')
				return FALSE;
		} else if (!path_item_match(CAR(a), item_mask)) {
			return FALSE;
		} else {
			a = CDR(a);
		}
	}
	if (!ecl_endp(a))
		return FALSE;
	return TRUE;
}

cl_object
cl_pathname_match_p(cl_object path, cl_object mask)
{
	cl_object output = Cnil;
	path = cl_pathname(path);
	mask = cl_pathname(mask);
	if (path->pathname.logical != mask->pathname.logical)
		goto OUTPUT;
#if 0
	/* INV: This was checked in the calling routine */
	if (!path_item_match(path->pathname.host, mask->pathname.host))
		goto OUTPUT;
#endif
	/* Missing components default to :WILD */
	if (!Null(mask->pathname.directory) &&
	    !path_list_match(path->pathname.directory, mask->pathname.directory))
		goto OUTPUT;
	if (!path_item_match(path->pathname.name, mask->pathname.name))
		goto OUTPUT;
	if (!path_item_match(path->pathname.type, mask->pathname.type))
		goto OUTPUT;
	if (Null(mask->pathname.version) ||
	    path_item_match(path->pathname.version, mask->pathname.version))
		output = Ct;
 OUTPUT:
	@(return output)
}

/* --------------- PATHNAME TRANSLATIONS ------------------ */

static cl_object
coerce_to_from_pathname(cl_object x, cl_object host)
{
	switch (type_of(x)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		x = cl_parse_namestring(2, x, host);
	case t_pathname:
		if (x->pathname.logical)
			return x;
	default:
		FEerror("~S is not a valid from-pathname translation", 1, x);
	}
}

@(defun si::pathname-translations (host &optional (set OBJNULL))
	cl_index parsed_len, len;
	cl_object pair, l;
@
	/* Check that host is a valid host name */
        if (ecl_unlikely(!ECL_STRINGP(host)))
                FEwrong_type_nth_arg(@[si::pathname-translations], 1, host, @[string]);
	host = cl_string_upcase(1, host);
	len = ecl_length(host);
	parse_word(host, is_null, WORD_LOGICAL, 0, len, &parsed_len);
	if (parsed_len < len) {
		FEerror("Wrong host syntax ~S", 1, host);
	}
	/* Find its translation list */
	pair = @assoc(4, host, cl_core.pathname_translations, @':test', @'string-equal');
	if (set == OBJNULL) {
		@(return ((pair == Cnil)? Cnil : CADR(pair)));
	}
	/* Set the new translation list */
        if (ecl_unlikely(!LISTP(set))) {
                FEwrong_type_nth_arg(@[si::pathname-translations], 2, set, @[list]);
        }
	if (pair == Cnil) {
		pair = CONS(host, CONS(Cnil, Cnil));
		cl_core.pathname_translations = CONS(pair, cl_core.pathname_translations);
	}
	for (l = set, set = Cnil; !ecl_endp(l); l = CDR(l)) {
		cl_object item = CAR(l);
		cl_object from = coerce_to_from_pathname(cl_car(item), host);
		cl_object to = cl_pathname(cl_cadr(item));
		set = CONS(CONS(from, CONS(to, Cnil)), set);
	}
	set = cl_nreverse(set);
	ECL_RPLACA(ECL_CONS_CDR(pair), set);
	@(return set)
@)

static cl_object
find_wilds(cl_object l, cl_object source, cl_object match)
{
	cl_index i, j, k, ls, lm;

	if (match == @':wild')
		return ecl_list1(source);
	if (!ecl_stringp(match) || !ecl_stringp(source)) {
		if (match != source)
			return @':error';
		return l;
	}
	ls = ecl_length(source);
	lm = ecl_length(match);
	for(i = j = 0; i < ls && j < lm; ) {
		cl_index pattern_char = ecl_char(match,j);
		if (pattern_char == '*') {
			for (j++, k = i;
			     k < ls && ecl_char(source,k) != pattern_char;
			     k++)
				;
			l = CONS(make_one(source, i, k), l);
			i = k;
			continue;
		}
		if (ecl_char(source,i) != pattern_char)
			return @':error';
		i++, j++;
	}
	if (i < ls || j < lm)
		return @':error';
	return l;
}

static cl_object
find_list_wilds(cl_object a, cl_object mask)
{
	cl_object l = Cnil, l2;

	while (!ecl_endp(mask)) {
		cl_object item_mask = CAR(mask);
		mask = CDR(mask);
		if (item_mask == @':wild-inferiors') {
			l2 = Cnil;
			while (!path_list_match(a, mask)) {
				if (ecl_endp(a))
					return @':error';
				l2 = CONS(CAR(a),l2);
				a = CDR(a);
			}
			l = CONS(l2, l);
		} else if (ecl_endp(a)) {
			/* A NIL directory should match against :absolute
			   or :relative, in order to perform suitable translations. */
			if (item_mask != @':absolute' && item_mask != @':relative')
				return @':error';
		} else {
			l2 = find_wilds(l, CAR(a), item_mask);
			if (l == @':error')
				return @':error';
			if (!Null(l2))
				l = CONS(l2, l);
			a = CDR(a);
		}
	}
	return @nreverse(l);
}

static cl_object
copy_wildcards(cl_object *wilds_list, cl_object pattern)
{
	cl_index i, l, j;
	bool new_string;
	cl_object wilds = *wilds_list, token;

	if (pattern == @':wild') {
		if (ecl_endp(wilds))
			return @':error';
		pattern = CAR(wilds);
		*wilds_list = CDR(wilds);
		return pattern;
	}
	if (pattern == @':wild-inferiors')
		return @':error';
	if (!ecl_stringp(pattern))
		return pattern;

	new_string = FALSE;
	l = ecl_length(pattern);
	token = si_get_buffer_string();
	for (j = i = 0; i < l; ) {
		cl_index c = ecl_char(pattern, i);
		if (c != '*') {
			i++;
			continue;
		}
		if (i != j) {
			push_substring(token, pattern, j, i);
		}
		new_string = TRUE;
		if (ecl_endp(wilds)) {
			return @':error';
		}
		push_string(token, CAR(wilds));
		wilds = CDR(wilds);
		j = i++;
	}
	/* Only create a new string when needed */
	if (new_string) {
		if (ecl_fits_in_base_string(token)) {
			pattern = si_copy_to_simple_base_string(token);
		} else {
			pattern = cl_copy_seq(token);
		}
	}
	si_put_buffer_string(token);
	*wilds_list = wilds;
	return pattern;
}

static cl_object
copy_list_wildcards(cl_object *wilds, cl_object to)
{
	cl_object l = Cnil;

	while (!ecl_endp(to)) {
		cl_object d, mask = CAR(to);
		if (mask == @':wild-inferiors') {
			cl_object list = *wilds;
			if (ecl_endp(list))
				return @':error';
			else {
				cl_object dirlist = CAR(list);
				if (CONSP(dirlist))
					l = ecl_append(CAR(list), l);
				else if (!Null(CAR(list)))
					return @':error';
			}
			*wilds = CDR(list);
		} else {
			d = copy_wildcards(wilds, CAR(to));
			if (d == @':error')
				return d;
			l = CONS(d, l);
		}
		to = CDR(to);
	}
	if (CONSP(l))
		l = @nreverse(l);
	return l;
}

@(defun translate-pathname (source from to &key ((:case scase) @':local'))
	cl_object wilds, d;
	cl_object host, device, directory, name, type, version;
	cl_object fromcase, tocase;
@
	/* The pathname from which we get the data */
	source = cl_pathname(source);
	/* The mask applied to the source pathname */
	from = cl_pathname(from);
	fromcase = normalize_case(from, @':local');
	/* The pattern which says what the output should look like */
	to = cl_pathname(to);
	tocase = normalize_case(to, @':local');

	if (source->pathname.logical != from->pathname.logical)
		goto error;

	/* Match host names */
	if (cl_string_equal(2, source->pathname.host, from->pathname.host) == Cnil)
		goto error;
	host = to->pathname.host;

	/* Logical pathnames do not have devices. We just overwrite it. */
	device = to->pathname.device;

	/* Match directories */
	wilds = find_list_wilds(source->pathname.directory,
				from->pathname.directory);
	if (wilds == @':error')	goto error;
	if (Null(to->pathname.directory)) {
                /* Missing components are replaced */
                d = translate_list_case(from->pathname.directory, fromcase, tocase);
        } else {
                wilds = translate_list_case(wilds, fromcase, tocase);
                d = copy_list_wildcards(&wilds, to->pathname.directory);
                if (d == @':error') goto error;
                if (wilds != Cnil) goto error2;
        }
	directory = d;

	/* Match name */
	wilds = find_wilds(Cnil, source->pathname.name, from->pathname.name);
	if (wilds == @':error') goto error2;
	if (Null(to->pathname.name)) {
                d = translate_component_case(from->pathname.name, fromcase, tocase);
        } else {
                wilds = translate_list_case(wilds, fromcase, tocase);
                d = copy_wildcards(&wilds, to->pathname.name);
                if (d == @':error') goto error;
                if (wilds != Cnil) goto error2;
        }
	name = d;

	/* Match type */
	wilds = find_wilds(Cnil, source->pathname.type, from->pathname.type);
	if (wilds == @':error') goto error2;
	if (Null(to->pathname.type)) {
                d = translate_component_case(from->pathname.type, fromcase, tocase);
        } else {
                wilds = translate_list_case(wilds, fromcase, tocase);
                d = copy_wildcards(&wilds, to->pathname.type);
                if (d == @':error') goto error;
                if (wilds != Cnil) goto error2;
        }
	type = d;

	/* Match version */
	version = to->pathname.version;
	if (from->pathname.version == @':wild') {
		if (to->pathname.version == @':wild') {
			version = source->pathname.version;
		}
	}
	return ecl_make_pathname(host, device, directory, name, type,
                                 version, tocase);
 error:
	FEerror("~S is not a specialization of path ~S", 2, source, from);
 error2:
	FEerror("Number of wildcards in ~S do not match  ~S", 2, from, to);
@)

@(defun translate-logical-pathname (source &key)
	cl_object l, pair;
	cl_object pathname;
@
	pathname = cl_pathname(source);
 begin:
	if (!pathname->pathname.logical) {
		@(return pathname)
	}
	l = @si::pathname-translations(1, pathname->pathname.host);
	for(; !ecl_endp(l); l = CDR(l)) {
		pair = CAR(l);
		if (!Null(cl_pathname_match_p(pathname, CAR(pair)))) {
			pathname = cl_translate_pathname(3, pathname,
                                                         CAR(pair),
							 CADR(pair));
			goto begin;
		}
	}
	FEerror("~S admits no logical pathname translations", 1, pathname);
@)
