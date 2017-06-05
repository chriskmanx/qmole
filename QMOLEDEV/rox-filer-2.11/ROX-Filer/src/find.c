/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* find.c - processes the find conditions
 *
 * A Condition is a tree structure. Each node has a test() fn which
 * can be used to see whether the current file matches, and a free() fn
 * which frees it. Both will recurse down the tree as needed.
 */

#include "config.h"

#include <string.h>
#include <fnmatch.h>
#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#include "global.h"

#include "main.h"
#include "find.h"

typedef struct _Eval Eval;

/* Static prototypes */
static FindCondition *parse_expression(const gchar **expression);
static FindCondition *parse_case(const gchar **expression);
static FindCondition *parse_system(const gchar **expression);
static FindCondition *parse_condition(const gchar **expression);
static FindCondition *parse_match(const gchar **expression);
static FindCondition *parse_comparison(const gchar **expression);
static FindCondition *parse_dash(const gchar **expression);
static FindCondition *parse_is(const gchar **expression);
static Eval *parse_eval(const gchar **expression);
static Eval *parse_variable(const gchar **expression);

static gboolean match(const gchar **expression, const gchar *word);

typedef enum {
	IS_DIR,
	IS_REG,
	IS_LNK,
	IS_FIFO,
	IS_SOCK,
	IS_CHR,
	IS_BLK,
	IS_DEV,
	IS_DOOR,
	IS_SUID,
	IS_SGID,
	IS_STICKY,
	IS_READABLE,
	IS_WRITEABLE,
	IS_EXEC,
	IS_EMPTY,
	IS_MINE,
} IsTest;

typedef enum {
	COMP_LT,
	COMP_LE,
	COMP_EQ,
	COMP_NE,
	COMP_GE,
	COMP_GT,
} CompType;

typedef enum {
	V_ATIME,
	V_CTIME,
	V_MTIME,
	V_SIZE,
	V_INODE,
	V_NLINKS,
	V_UID,
	V_GID,
	V_BLOCKS,
} VarType;

enum
{
	FLAG_AGO 	= 1 << 0,
	FLAG_HENCE 	= 1 << 1,
};

typedef double (*EvalCalc)(Eval *eval, FindInfo *info);
typedef void (*EvalFree)(Eval *eval);

struct _FindCondition
{
	FindTest	test;
	FindFree	free;
	/* These next three depend on the first two... */
	gpointer	data1;
	gpointer	data2;
	gint		value;
};

struct _Eval
{
	EvalCalc	calc;
	EvalFree	free;
	gpointer	data1;
	gpointer	data2;
};

#define EAT ((*expression)++)
#define NEXT (**expression)
#define SKIP while (NEXT == ' ' || NEXT == '\t') EAT
#define MATCH(word) (match(expression, word))

#ifndef S_ISVTX
# define S_ISVTX 0x0001000
#endif

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* Take a string and parse it, returning a condition object which
 * can be passed to find_test_condition() later. NULL if the string
 * is not a valid expression.
 */
FindCondition *find_compile(const gchar *string)
{
	FindCondition 	*cond;
	const gchar	**expression = &string;

	g_return_val_if_fail(string != NULL, NULL);

	cond = parse_expression(expression);
	if (!cond)
		return NULL;

	SKIP;
	if (NEXT != '\0')
	{
		cond->free(cond);
		cond = NULL;
	}

	return cond;
}

gboolean find_test_condition(FindCondition *condition, FindInfo *info)
{
	g_return_val_if_fail(condition != NULL, FALSE);
	g_return_val_if_fail(info != NULL, FALSE);

	return condition->test(condition, info);
}

void find_condition_free(FindCondition *condition)
{
	if (condition)
		condition->free(condition);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* Call this when you've just eaten '('. Returns the string upto the
 * matching ')' (and eats that bracket), or NULL on failure.
 * Brackets within the string may be quoted or escaped.
 */
static gchar *get_bracketed_string(const gchar **expression)
{
	GString		*str;
	int		opens = 1;
	gchar		quote = '\0';

	str = g_string_new(NULL);

	while (NEXT != '\0')
	{
		gchar	c = NEXT;

		EAT;

		if (quote == '\0')
		{
			if (c == '\'' || c == '"')
				quote = c;	/* Start quoted section */
			else if (c == '\\' && (NEXT == '(' || NEXT == ')'))
			{
				c = NEXT;
				EAT;
			}
			else if (c == ')')
			{
				opens--;
				if (opens < 1)
				{
					gchar	*retval = str->str;

					g_string_free(str, FALSE);
					return retval;
				}
			}
			else if (c == '(')
				opens++;
		}
		else if (c == quote)
			quote = '\0';		/* End quoted section */
		else if (c == '\\' && NEXT == quote)
		{
			g_string_append_c(str, c);
			c = NEXT;
			EAT;
		}

		g_string_append_c(str, c);
	}
	
	g_string_free(str, TRUE);

	return NULL;
}


/*				TESTING CODE				*/

static gboolean test_prune(FindCondition *condition, FindInfo *info)
{
	info->prune = TRUE;
	return FALSE;
}

static gboolean test_leaf(FindCondition *condition, FindInfo *info)
{
	return fnmatch(condition->data1, info->leaf, 0) == 0;
}

static gboolean test_path(FindCondition *condition, FindInfo *info)
{
	return fnmatch(condition->data1, info->fullpath, FNM_PATHNAME) == 0;
}

static gboolean test_system(FindCondition *condition, FindInfo *info)
{
	gchar	*command = (gchar *) condition->data1;
	gchar	*start = command;
	GString	*to_sys = NULL;
	gchar	*perc;
	int	retcode;

	to_sys = g_string_new(NULL);

	while ((perc = strchr(command, '%')))
	{
		if (perc > start && perc[-1] == '\\')
			perc--;

		while (command < perc)
			g_string_append_c(to_sys, *(command++));
		
		if (*perc == '%')
			g_string_append(to_sys, info->fullpath);
		else
		{
			g_string_append_c(to_sys, '%');
			perc++;
		}
		
		command = perc + 1;
	}

	g_string_append(to_sys, command);

	retcode = system(to_sys->str);

	g_string_free(to_sys, TRUE);
	
	return retcode == 0;
}

static gboolean test_OR(FindCondition *condition, FindInfo *info)
{
	FindCondition	*first = (FindCondition *) condition->data1;
	FindCondition	*second = (FindCondition *) condition->data2;

	return first->test(first, info) || second->test(second, info);
}

static gboolean test_AND(FindCondition *condition, FindInfo *info)
{
	FindCondition	*first = (FindCondition *) condition->data1;
	FindCondition	*second = (FindCondition *) condition->data2;

	return first->test(first, info) && second->test(second, info);
}

static gboolean test_neg(FindCondition *condition, FindInfo *info)
{
	FindCondition	*first = (FindCondition *) condition->data1;

	return !first->test(first, info);
}

static gboolean test_is(FindCondition *condition, FindInfo *info)
{
	mode_t	mode = info->stats.st_mode;

	switch ((IsTest) condition->value)
	{
		case IS_DIR:
			return S_ISDIR(mode);
		case IS_REG:
			return S_ISREG(mode);
		case IS_LNK:
			return S_ISLNK(mode);
		case IS_FIFO:
			return S_ISFIFO(mode);
		case IS_SOCK:
			return S_ISSOCK(mode);
		case IS_CHR:
			return S_ISCHR(mode);
		case IS_BLK:
			return S_ISBLK(mode);
		case IS_DEV:
			return S_ISCHR(mode)
			    || S_ISBLK(mode);
		case IS_DOOR:
			return S_ISDOOR(mode);
		case IS_SUID:
			return (mode & S_ISUID) != 0;
		case IS_SGID:
			return (mode & S_ISGID) != 0;
		case IS_STICKY:
			return (mode & S_ISVTX) != 0;
		/* NOTE: access() uses uid, not euid. Shouldn't matter? */
		case IS_READABLE:
			return access(info->fullpath, R_OK) == 0;
		case IS_WRITEABLE:
			return access(info->fullpath, W_OK) == 0;
		case IS_EXEC:
			return access(info->fullpath, X_OK) == 0;
		case IS_EMPTY:
			return info->stats.st_size == 0;
		case IS_MINE:
			return info->stats.st_uid == euid;
	}

	return FALSE;
}

static gboolean test_comp(FindCondition *condition, FindInfo *info)
{
	Eval	*first = (Eval *) condition->data1;
	Eval	*second = (Eval *) condition->data2;
	double	a, b;

	a = first->calc(first, info);
	b = second->calc(second, info);

	switch ((CompType) condition->value)
	{
		case COMP_LT:
			return a < b;
		case COMP_LE:
			return a <= b;
		case COMP_EQ:
			return a == b;
		case COMP_NE:
			return a != b;
		case COMP_GE:
			return a >= b;
		case COMP_GT:
			return a > b;
	}

	return FALSE;
}

/*				FREEING CODE				*/

/* Frees the structure and g_free()s both data items (NULL is OK) */
static void free_simple(FindCondition *condition)
{
	g_return_if_fail(condition != NULL);

	g_free(condition->data1);
	g_free(condition->data2);
	g_free(condition);
}

/* Treats data1 and data2 as conditions (or NULL) and frees recursively */
static void free_branch(FindCondition *condition)
{
	FindCondition	*first = (FindCondition *) condition->data1;
	FindCondition	*second = (FindCondition *) condition->data2;

	if (first)
		first->free(first);
	if (second)
		second->free(second);
	g_free(condition);
}

/* Treats data1 and data2 as evals (or NULL) and frees recursively */
static void free_comp(FindCondition *condition)
{
	Eval	*first = (Eval *) condition->data1;
	Eval	*second = (Eval *) condition->data2;

	if (first)
		first->free(first);
	if (second)
		second->free(second);
	g_free(condition);
}

/* 				PARSING CODE				*/

/* These all work in the same way - you give them an expression and the
 * parse as much as they can, returning a condition for everything that
 * was parsed and updating the expression pointer to the first unknown
 * token. NULL indicates an error.
 */


/* An expression is a series of comma-separated cases, any of which
 * may match.
 */
static FindCondition *parse_expression(const gchar **expression)
{
	FindCondition	*first, *second, *cond;

	first = parse_case(expression);
	if (!first)
		return NULL;

	SKIP;
	if (NEXT != ',')
		return first;
	EAT;

	second = parse_expression(expression);
	if (!second)
	{
		first->free(first);
		return NULL;
	}

	cond = g_new(FindCondition, 1);
	cond->test = &test_OR;
	cond->free = &free_branch;
	cond->data1 = first;
	cond->data2 = second;

	return cond;
}

static FindCondition *parse_case(const gchar **expression)
{
	FindCondition	*first, *second, *cond;

	first = parse_condition(expression);
	if (!first)
		return NULL;

	SKIP;
	if (NEXT == '\0' || NEXT == ',' || NEXT == ')')
		return first;

	(void) MATCH(_("And"));

	second = parse_case(expression);
	if (!second)
	{
		first->free(first);
		return NULL;
	}

	cond = g_new(FindCondition, 1);
	cond->test = &test_AND;
	cond->free = &free_branch;
	cond->data1 = first;
	cond->data2 = second;

	return cond;
}

static FindCondition *parse_condition(const gchar **expression)
{
	FindCondition	*cond = NULL;

	SKIP;

	if (NEXT == '!' || MATCH(_("Not")))
	{
		FindCondition *operand;
		
		EAT;

		operand = parse_condition(expression);
		if (!operand)
			return NULL;
		cond = g_new(FindCondition, 1);
		cond->test = test_neg;
		cond->free = free_branch;
		cond->data1 = operand;
		cond->data2 = NULL;
		return cond;
	}

	if (NEXT == '(')
	{
		FindCondition *subcond;

		EAT;

		subcond = parse_expression(expression);
		if (!subcond)
			return NULL;
		SKIP;
		if (NEXT != ')')
		{
			subcond->free(subcond);
			return NULL;
		}

		EAT;
		return subcond;
	}
	
	if (NEXT == '\'')
	{
		EAT;
		return parse_match(expression);
	}

	if (MATCH(_("system")))
	{
		SKIP;
		if (NEXT != '(')
			return NULL;
		EAT;
		return parse_system(expression);
	}
	else if (MATCH(_("prune")))
	{
		cond = g_new(FindCondition, 1);
		cond->test = test_prune;
		cond->free = (FindFree) g_free;
		cond->data1 = NULL;
		cond->data2 = NULL;

		return cond;
	}

	cond = parse_dash(expression);
	if (cond)
		return cond;

	cond = parse_is(expression);
	if (cond)
		return cond;

	return parse_comparison(expression);
}

/* Call this when you've just eaten 'system(' */
static FindCondition *parse_system(const gchar **expression)
{	
	FindCondition	*cond = NULL;
	gchar		*command_string;

	command_string = get_bracketed_string(expression);
	if (!command_string)
		return NULL;

	cond = g_new(FindCondition, 1);
	cond->test = test_system;
	cond->free = &free_simple;
	cond->data1 = command_string;
	cond->data2 = NULL;

	return cond;
}

static FindCondition *parse_comparison(const gchar **expression)
{
	FindCondition	*cond = NULL;
	Eval		*first;
	Eval		*second;
	CompType	comp;

	SKIP;

	first = parse_eval(expression);
	if (!first)
		return NULL;
	
	SKIP;
	if (NEXT == '=')
	{
		comp = COMP_EQ;
		EAT;
	}
	else if (NEXT == '>')
	{
		EAT;
		if (NEXT == '=')
		{
			EAT;
			comp = COMP_GE;
		}
		else
			comp = COMP_GT;
	}
	else if (NEXT == '<')
	{
		EAT;
		if (NEXT == '=')
		{
			EAT;
			comp = COMP_LE;
		}
		else
			comp = COMP_LT;
	}
	else if (NEXT == '!' && (*expression)[1] == '=')
	{
		EAT;
		EAT;
		comp = COMP_NE;
	}
	else if (MATCH(_("After")))
		comp = COMP_GT;
	else if (MATCH(_("Before")))
		comp = COMP_LT;
	else
		return NULL;

	SKIP;
	second = parse_eval(expression);
	if (!second)
	{
		first->free(first);
		return NULL;
	}

	cond = g_new(FindCondition, 1);
	cond->test = &test_comp;
	cond->free = (FindFree) &free_comp;
	cond->data1 = first;
	cond->data2 = second;
	cond->value = comp;

	return cond;
}

static FindCondition *parse_dash(const gchar **expression)
{
	const gchar *exp = *expression;
	FindCondition *cond, *retval = NULL;
	IsTest	 test;
	int i = 1;

	if (NEXT != '-')
		return NULL;

	while (exp[i] && !g_ascii_isspace(exp[i]))
	{
		switch (exp[i])
		{
			case 'f': test = IS_REG; break;
			case 'l': test = IS_LNK; break;
			case 'd': test = IS_DIR; break;
			case 'b': test = IS_BLK; break;
			case 'c': test = IS_CHR; break;
			case 'D': test = IS_DEV; break;
			case 'p': test = IS_FIFO; break;
			case 'S': test = IS_SOCK; break;
			case 'O': test = IS_DOOR; break;
			case 'u': test = IS_SUID; break;
			case 'g': test = IS_SGID; break;
			case 'k': test = IS_STICKY; break;
			case 'r': test = IS_READABLE; break;
			case 'w': test = IS_WRITEABLE; break;
			case 'x': test = IS_EXEC; break;
			case 'o': test = IS_MINE; break;
			case 'z': test = IS_EMPTY; break;
			default:
				  find_condition_free(retval);
				  return NULL;
		}
		i++;

		cond = g_new(FindCondition, 1);
		cond->test = &test_is;
		cond->free = (FindFree) &g_free;
		cond->data1 = NULL;
		cond->data2 = NULL;
		cond->value = test;

		if (retval)
		{
			FindCondition *new;

			new = g_new(FindCondition, 1);
			new->test = &test_AND;
			new->free = &free_branch;
			new->data1 = retval;
			new->data2 = cond;

			retval = new;
		}
		else
			retval = cond;
	}

	(*expression) += i;

	return retval;
}

/* Returns NULL if expression is not an is-expression */
static FindCondition *parse_is(const gchar **expression)
{
	FindCondition	*cond;
	IsTest		test;

	if (MATCH(_("IsReg")))
		test = IS_REG;
	else if (MATCH(_("IsLink")))
		test = IS_LNK;
	else if (MATCH(_("IsDir")))
		test = IS_DIR;
	else if (MATCH(_("IsChar")))
		test = IS_CHR;
	else if (MATCH(_("IsBlock")))
		test = IS_BLK;
	else if (MATCH(_("IsDev")))
		test = IS_DEV;
	else if (MATCH(_("IsPipe")))
		test = IS_FIFO;
	else if (MATCH(_("IsSocket")))
		test = IS_SOCK;
	else if (MATCH(_("IsDoor")))
		test = IS_DOOR;
	else if (MATCH(_("IsSUID")))
		test = IS_SUID;
	else if (MATCH(_("IsSGID")))
		test = IS_SGID;
	else if (MATCH(_("IsSticky")))
		test = IS_STICKY;
	else if (MATCH(_("IsReadable")))
		test = IS_READABLE;
	else if (MATCH(_("IsWriteable")))
		test = IS_WRITEABLE;
	else if (MATCH(_("IsExecutable")))
		test = IS_EXEC;
	else if (MATCH(_("IsEmpty")))
		test = IS_EMPTY;
	else if (MATCH(_("IsMine")))
		test = IS_MINE;
	else
		return NULL;

	cond = g_new(FindCondition, 1);
	cond->test = &test_is;
	cond->free = (FindFree) &g_free;
	cond->data1 = NULL;
	cond->data2 = NULL;
	cond->value = test;

	return cond;
}

/* Call this just after reading a ' */
static FindCondition *parse_match(const gchar **expression)
{
	FindCondition	*cond = NULL;
	GString		*str;
	FindTest	test = &test_leaf;
	str = g_string_new(NULL);

	while (NEXT != '\'')
	{
		gchar	c = NEXT;

		if (c == '\0')
			goto out;
		EAT;
		
		if (c == '\\' && NEXT == '\'')
		{
			c = NEXT;
			EAT;
		}

		if (c == '/')
			test = &test_path;

		g_string_append_c(str, c);
	}
	EAT;
	
	cond = g_new(FindCondition, 1);
	cond->test = test;
	cond->free = &free_simple;
	cond->data1 = str->str;
	cond->data2 = NULL;

out:
	g_string_free(str, cond ? FALSE : TRUE);

	return cond;
}

/*			NUMERIC EXPRESSIONS				*/

/*	CALCULATIONS	*/

static double get_constant(Eval *eval, FindInfo *info)
{
	double	value = *((double *) (eval->data1));
	gint	flags = GPOINTER_TO_INT(eval->data2);

	if (flags & FLAG_AGO)
		value = info->now - value;
	else if (flags & FLAG_HENCE)
		value = info->now + value;
	
	return value;
}

static double get_var(Eval *eval, FindInfo *info)
{
	switch ((VarType) eval->data1)
	{
		case V_ATIME:
			return info->stats.st_atime;
		case V_CTIME:
			return info->stats.st_ctime;
		case V_MTIME:
			return info->stats.st_mtime;
		case V_SIZE:
			return info->stats.st_size;
		case V_INODE:
			return info->stats.st_ino;
		case V_NLINKS:
			return info->stats.st_nlink;
		case V_UID:
			return info->stats.st_uid;
		case V_GID:
			return info->stats.st_gid;
		case V_BLOCKS:
			return info->stats.st_blocks;
	}

	return 0;
}

/*	FREEING		*/

static void free_constant(Eval *eval)
{
	g_free(eval->data1);
	g_free(eval);
}

/*	PARSING		*/

/* Parse something that evaluates to a number.
 * This function tries to get a constant - if it fails then it tries
 * interpreting the next token as a variable.
 */
static Eval *parse_eval(const gchar **expression)
{
	const char *start;
	char	*end;
	double	value;
	Eval	*eval;
	gint	flags = 0;
	
	SKIP;
	start = *expression;
	value = strtol(start, &end, 0);

	if (end == start)
	{
		if (MATCH(_("Now")))
		{
			value = 0;
			flags |= FLAG_HENCE;
		}
		else
			return parse_variable(expression);
	}
	else
		*expression = end;

	SKIP;

	if (MATCH(_("Byte")) || MATCH(_("Bytes")))
		;
	else if (MATCH(_("Kb")) || MATCH(_("K")))
		value *= 1<<10;
	else if (MATCH(_("Mb")) || MATCH(_("M")))
		value *= 1<<20;
	else if (MATCH(_("Gb")) || MATCH(_("G")))
		value *= 1<<30;
	else if (MATCH(_("Sec")) || MATCH(_("Secs")))
		;
	else if (MATCH(_("Min")) || MATCH(_("Mins")))
		value *= 60;
	else if (MATCH(_("Hour")) || MATCH(_("Hours")))
		value *= 60 * 60;
	else if (MATCH(_("Day")) || MATCH(_("Days")))
		value *= 60 * 60 * 24;
	else if (MATCH(_("Week")) || MATCH(_("Weeks")))
		value *= 60 * 60 * 24 * 7;
	else if (MATCH(_("Year")) || MATCH(_("Years")))
		value *= 60 * 60 * 24 * 7 * 365.25;

	eval = g_new(Eval, 1);
	eval->calc = &get_constant;
	eval->free = &free_constant;
	eval->data1 = g_memdup(&value, sizeof(value));

	SKIP;
	if (MATCH(_("Ago")))
		flags |= FLAG_AGO;
	else if (MATCH(_("Hence")))
		flags |= FLAG_HENCE;

	eval->data2 = GINT_TO_POINTER(flags);

	return eval;
}

static Eval *parse_variable(const gchar **expression)
{
	Eval	*eval;
	VarType	var;

	SKIP;

	if (MATCH(_("atime")))
		var = V_ATIME;
	else if (MATCH(_("ctime")))
		var = V_CTIME;
	else if (MATCH(_("mtime")))
		var = V_MTIME;
	else if (MATCH(_("size")))
		var = V_SIZE;
	else if (MATCH(_("inode")))
		var = V_INODE;
	else if (MATCH(_("nlinks")))
		var = V_NLINKS;
	else if (MATCH(_("uid")))
		var = V_UID;
	else if (MATCH(_("gid")))
		var = V_GID;
	else if (MATCH(_("blocks")))
		var = V_BLOCKS;
	else
		return NULL;

	eval = g_new(Eval, 1);
	eval->calc = &get_var;
	eval->free = (EvalFree) &g_free;
	eval->data1 = (gpointer) var;
	eval->data2 = NULL;

	return eval;
}

static gboolean match(const gchar **expression, const gchar *word)
{
	int	len;

	len = strlen(word);
	if (g_strncasecmp(*expression, word, len))
		return FALSE;

	if (g_ascii_isalpha(*(*expression + len)))
		return FALSE;

	(*expression) += len;

	return TRUE;
}
