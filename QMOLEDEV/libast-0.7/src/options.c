/*
 * Copyright (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/**
 * @file options.c
 * Command Line Option Parser Source File
 *
 * This file contains the functions which comprise the command line
 * option parser.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.18 $
 * $Date: 2004/10/26 18:01:53 $
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: options.c,v 1.18 2004/10/26 18:01:53 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/*@{*/
/**
 * @name Internal Parser Macros
 * Macros to simply certain parser operations.
 *
 * This group of macros is used internally by the command line option
 * parser.  They are not available for use by client programs and are
 * documented here solely for completeness and clarity.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink
 * @ingroup DOXGRP_OPT
 */

/** Next argument.  Proceed to parsing the next argument in the argv[] list. */
#define NEXT_ARG()       D_OPTIONS(("NEXT_ARG()\n")); i++; opt = SPIF_CHARPTR(argv[i]); continue
/** Next letter.  Proceed to the next letter in a bundled option series. */
#define NEXT_LETTER()    D_OPTIONS(("NEXT_LETTER(%s)\n", opt)); if (*(opt + 1)) {opt++;} else {NEXT_ARG();} continue
/** Next loop.  Proceed to the next parsing stage (letter or word). */
#define NEXT_LOOP()      D_OPTIONS(("NEXT_LOOP()\n")); if (islong || val_ptr) {NEXT_ARG();} else {NEXT_LETTER();} NOP
/** Option parse test.  Returns true IFF the option should be parsed on this pass. */
#define SHOULD_PARSE(j)  ((SPIFOPT_FLAGS_IS_SET(SPIFOPT_SETTING_PREPARSE) && SPIFOPT_OPT_IS_PREPARSE(j)) \
                           || (!SPIFOPT_FLAGS_IS_SET(SPIFOPT_SETTING_PREPARSE) && !SPIFOPT_OPT_IS_PREPARSE(j)))
/*@}*/

/**
 * Internal option parser settings.
 *
 * This variable holds the actual structure containing the settings
 * for the option parser.  It should never be accessed directly, but
 * rather through use of the Option Parser Settings Macros.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink
 * @ingroup DOXGRP_OPT
 */
spifopt_settings_t spifopt_settings = { NULL, 0, 0, 0, 0, 0, NULL };

/**
 * Option-type-to-string translator.
 *
 * This function is used internally by spifopt_usage() to convert an
 * option's numerical type to a short string representing that type of
 * option.
 *
 * @param type The numeric option type (SPIFOPT_OPT_FLAGS()).
 * @return     A 6-char-max string describing the option type.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink
 * @ingroup DOXGRP_OPT
 */
static spif_charptr_t 
get_option_type_string(spif_uint16_t type)
{
    switch (type) {
        case SPIFOPT_FLAG_BOOLEAN: return SPIF_CHARPTR("(bool)"); break;
        case SPIFOPT_FLAG_INTEGER: return SPIF_CHARPTR("(int)"); break;
        case SPIFOPT_FLAG_ARGLIST: return SPIF_CHARPTR("(strs)"); break;
        default: return SPIF_CHARPTR("(str)");
    }
    ASSERT_NOTREACHED_RVAL(NULL);
}

/**
 * Built-in default function for displaying help information.
 *
 * This is the default "help handler" function.  It displays a list of
 * long and short options along with the description of each.  It also
 * prints out a brief type identifier as produced by
 * get_option_type_string().
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, SPIFOPT_HELPHANDLER_SET(), spifopt_helphandler_t
 * @ingroup DOXGRP_MEM
 */
void
spifopt_usage(void)
{
    spif_uint16_t i, col, l_long = 0, l_desc = 0;

    /* Find the longest long option and the longest description. */
    for (i = 0; i < SPIFOPT_NUMOPTS_GET(); i++) {
        MAX_IT(l_long, strlen(SPIFOPT_OPT_LONG(i)));
        MAX_IT(l_desc, strlen(SPIFOPT_OPT_DESC(i)));
    }
    l_long += 2;  /* Add 2 for the "--" */
    l_desc += 7;  /* Add 7 for the type and a space */

    printf("%s %s\n", libast_program_name, libast_program_version);
    printf("Usage:\n\n");
    printf("POSIX ");

    for (col = 0; col < (l_long - 3) / 2; col++) printf(" ");
    printf("GNU");
    for (col = 0; col < (l_long - 3) / 2; col++) printf(" ");
    if (!(l_long % 2)) {
        printf(" ");
    }
    printf("  ");

    for (col = 0; col < (l_desc - 11) / 2; col++) printf(" ");
    printf("Description");
    for (col = 0; col < (l_desc - 11) / 2; col++) printf(" ");
    if (!(l_desc % 2)) {
        printf(" ");
    }

    printf("\n");
    printf("----- ");

    for (col = 0; col < l_long; col++) printf("-");
    printf("  ");
    for (col = 0; col < l_desc; col++) printf("-");
    printf("\n");

    for (i = 0, l_long -= 2; i < SPIFOPT_NUMOPTS_GET(); i++) {
        if (SPIFOPT_OPT_SHORT(i)) {
            printf(" -%c   ", SPIFOPT_OPT_SHORT(i));
        } else {
            printf("      ");
        }
        printf("--%s", SPIFOPT_OPT_LONG(i));
        for (col = strlen(SPIF_CAST_C(char *) SPIFOPT_OPT_LONG(i)); col < l_long; col++) {
            printf(" ");
        }
        printf("  %-6s %s\n", get_option_type_string(SPIFOPT_OPT_TYPE(i)), SPIFOPT_OPT_DESC(i));
    }
    exit(EXIT_FAILURE);
}

/**
 * Find matching long option.
 *
 * This function searches the option list for a long option which
 * matches the given string.
 *
 * @param opt The long option string to match against.
 * @return    The index of the matching option, or -1 if not found.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, is_valid_option()
 * @ingroup DOXGRP_OPT
 */
static spif_int32_t
find_long_option(spif_charptr_t opt)
{
    spif_int32_t j;

    D_OPTIONS(("opt == \"%s\"\n", NONULL(opt)));
    /* Check to see if we have a long option that matches this. */
    for (j = 0; j < SPIFOPT_NUMOPTS_GET(); j++) {
        size_t l;

        l = strlen(SPIF_CHARPTR_C(SPIFOPT_OPT_LONG(j)));
        /* Look for matches to the part before the =, if any. */
        if (!strncasecmp(SPIF_CHARPTR_C(SPIFOPT_OPT_LONG(j)), SPIF_CHARPTR_C(opt), l)
            && (opt[l] == '=' || !opt[l])) {
            /* Got one. */
            D_OPTIONS(("Match found at %d:  %s == %s\n", j, SPIFOPT_OPT_LONG(j), opt));
            return j;
        }
    }
    /* No matching long option found.  Report an error and
       continue with the next arg. */
    libast_print_error("Unrecognized long option --%s\n", opt);
    CHECK_BAD();
    return ((spif_int32_t) -1);
}

/**
 * Find matching short option.
 *
 * This function searches the option list for a short option which
 * matches the given character.
 *
 * @param opt The short option character to match against.
 * @return    The index of the matching option, or -1 if not found.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, is_valid_option()
 * @ingroup DOXGRP_OPT
 */
static spif_int32_t
find_short_option(char opt)
{
    spif_int32_t j;

    D_OPTIONS(("opt == \"%c\"\n", opt));
    for (j = 0; j < SPIFOPT_NUMOPTS_GET(); j++) {
        if (SPIFOPT_OPT_SHORT(j) == opt) {
            D_OPTIONS(("Match found at %d:  %c == %c\n", j, SPIFOPT_OPT_SHORT(j), opt));
            return j;
        }
    }
    libast_print_error("unrecognized option -%c\n", opt);
    CHECK_BAD();
    return ((spif_int32_t) -1);
}

/**
 * Find the value for a long option.
 *
 * This function looks for and returns the value associated with a
 * long option, or NULL if one is not found.
 *
 * @param arg      The argument containing the long option.
 * @param next_arg The next word on the command line.
 * @param hasequal Address of a toggle variable to return whether or
 *                 not the value was appended to the option with an
 *                 equals sign ('=').
 * @return         The option's value string, or NULL if no value is
 *                 found.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse()
 * @ingroup DOXGRP_OPT
 */
static spif_charptr_t 
find_value_long(spif_charptr_t arg, spif_charptr_t next_arg, spif_charptr_t hasequal)
{
    spif_charptr_t val_ptr;

    if ((val_ptr = SPIF_CHARPTR(strchr(SPIF_CHARPTR_C(arg), '='))) != NULL) {
        val_ptr++;
        *hasequal = 1;
    } else {
        if (next_arg) {
            val_ptr = next_arg;
        }
        *hasequal = 0;
    }
    D_OPTIONS(("hasequal == %d  val_ptr == %10.8p \"%s\"\n", *hasequal, val_ptr, NONULL(val_ptr)));
    return val_ptr;
}

/**
 * Find the value for a short option.
 *
 * This function looks for and returns the value associated with a
 * short option, or NULL if one is not found.
 *
 * @param arg      The argument containing the short option.
 * @param next_arg The next word on the command line.
 * @return         The option's value string, or NULL if no value is
 *                 found.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse()
 * @ingroup DOXGRP_OPT
 */
static spif_charptr_t 
find_value_short(spif_charptr_t arg, spif_charptr_t next_arg)
{
    spif_charptr_t val_ptr = NULL;

    if (arg[1]) {
        val_ptr = arg + 1;
    } else if (next_arg != NULL) {
        val_ptr = next_arg;
    }
    D_OPTIONS(("val_ptr == %10.8p \"%s\"\n", val_ptr, NONULL(val_ptr)));
    return val_ptr;
}

/**
 * Test for a valid boolean value.
 *
 * This function compares the given value pointer to the possible
 * values for a boolean option.
 *
 * @param val_ptr The value to be tested.
 * @return        TRUE if boolean, FALSE if not.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse()
 * @ingroup DOXGRP_OPT
 */
static spif_bool_t
is_boolean_value(spif_charptr_t val_ptr)
{
    if (!(val_ptr) || !(*val_ptr)) {
        return FALSE;
    }
    return ((BOOL_OPT_ISTRUE(val_ptr) || BOOL_OPT_ISFALSE(val_ptr)) ? (TRUE) : (FALSE));
}

/**
 * Check for a match to the current option.
 *
 * This function does some initial parsing, then calls the appropriate
 * sub-function to look for matches.
 *
 * @param opt The argument string.
 * @return    TRUE if a match is found, FALSE otherwise.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse(), find_long_option(),
 *      find_short_option()
 * @ingroup DOXGRP_OPT
 */
static spif_bool_t
is_valid_option(spif_charptr_t opt)
{
    REQUIRE_RVAL(opt != NULL, FALSE);

    if (*opt != '-') {
        return FALSE;
    }
    opt++;
    if (*opt == '-') {
        opt++;
        if (find_long_option(opt) >= 0) {
            return TRUE;
        }
    } else {
        if (find_short_option(*opt) >= 0) {
            return TRUE;
        }
    }
    return FALSE;
}

/**
 * Handle a boolean option.
 *
 * This function is reponsible for taking the proper action for a
 * boolean option.  It sets the appropriate bitfield on the
 * appropriate variable as defined by the settings for that option.
 *
 * @param n       The index for the option within the option list.
 * @param val_ptr A pointer (possibly NULL) to the value specified
 *                with the option.
 * @param islong  0 if the option was short, non-zero otherwise.
 * @return    TRUE if a match is found, FALSE otherwise.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse(), find_long_option(),
 *      find_short_option()
 * @ingroup DOXGRP_OPT
 */
static spif_bool_t
handle_boolean(spif_int32_t n, spif_charptr_t val_ptr, unsigned char islong)
{
    D_OPTIONS(("Boolean option detected\n"));
    if (val_ptr && islong) {
        /* There's a value, so let's see what it is. */
        if (BOOL_OPT_ISTRUE(val_ptr)) {
            if (SHOULD_PARSE(n)) {
                D_OPTIONS(("\"%s\" == TRUE\n", val_ptr));
                *((unsigned long *) SPIFOPT_OPT_VALUE(n)) |= SPIFOPT_OPT_MASK(n);
            }
        } else if (BOOL_OPT_ISFALSE(val_ptr)) {
            if (SHOULD_PARSE(n)) {
                D_OPTIONS(("\"%s\" == FALSE\n", val_ptr));
                *((unsigned long *) SPIFOPT_OPT_VALUE(n)) &= ~SPIFOPT_OPT_MASK(n);
            }
        } else {
            if (SHOULD_PARSE(n)) {
                D_OPTIONS(("Forcing option --%s to TRUE\n", SPIFOPT_OPT_LONG(n)));
                *((unsigned long *) SPIFOPT_OPT_VALUE(n)) |= SPIFOPT_OPT_MASK(n);
            }
            return FALSE;
        }
    } else {
        if (SHOULD_PARSE(n)) {
            /* No value, or it was a short option, so pretend it was true. */
            if (islong) {
                D_OPTIONS(("Forcing option --%s to TRUE\n", SPIFOPT_OPT_LONG(n)));
            } else {
                val_ptr = NULL;
                D_OPTIONS(("Forcing option -%c to TRUE\n", SPIFOPT_OPT_SHORT(n)));
            }
            *((unsigned long *) SPIFOPT_OPT_VALUE(n)) |= SPIFOPT_OPT_MASK(n);
        }
    }
    return TRUE;
}

/**
 * Handle an integer option.
 *
 * This function is responsible for taking the appropriate action when
 * an integer option is encountered.  The variable whose address was
 * given to the option structure is assigned the value of the option.
 *
 * @param n       The index of the option.
 * @param val_ptr The value passed to the option.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse(), find_long_option(),
 *      find_short_option()
 * @ingroup DOXGRP_OPT
 */
static void
handle_integer(spif_int32_t n, spif_charptr_t val_ptr)
{
    D_OPTIONS(("Integer option detected\n"));
    *((int *) SPIFOPT_OPT_VALUE(n)) = strtol(SPIF_CHARPTR_C(val_ptr), (char **) NULL, 0);
}

/**
 * Handle a string option.
 *
 * This function is responsible for taking the appropriate action when
 * a string option is encountered.  The variable whose address was
 * given to the option structure is assigned the value of the option.
 *
 * @param n       The index of the option.
 * @param val_ptr The value passed to the option.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse(), find_long_option(),
 *      find_short_option()
 * @ingroup DOXGRP_OPT
 */
static void
handle_string(spif_int32_t n, spif_charptr_t val_ptr)
{
    D_OPTIONS(("String option detected\n"));
    *((const char **) SPIFOPT_OPT_VALUE(n)) = SPIF_CAST_C(char *) STRDUP(val_ptr);
}


/**
 * Handle an argument list option.
 *
 * This function is responsible for taking the appropriate action when
 * an argument list option is encountered.  An array of arguments is
 * created at the specified address.  There can be only one of these.
 *
 * @param n        The index of the option.
 * @param val_ptr  The value passed to the option.
 * @param hasequal TRUE if the long option used '=', FALSE otherwise.
 * @param i        The index of the current option within argc[].
 * @param argc     The argument count.
 * @param argv     The argument list.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink, spifopt_parse(), find_long_option(),
 *      find_short_option()
 * @ingroup DOXGRP_OPT
 */
static void
handle_arglist(spif_int32_t n, spif_charptr_t val_ptr, unsigned char hasequal,
               spif_int32_t i, int argc, char *argv[])
{
    spif_charptr_t *tmp;
    register unsigned short k;

    D_OPTIONS(("Argument list option detected\n"));
    if (hasequal) {
        /* There's an equals sign, so just parse the rest of this option into words. */
        tmp = SPIF_CAST_PTR(charptr) MALLOC(sizeof(spif_charptr_t) * (spiftool_num_words(val_ptr) + 1));

        for (k = 0; val_ptr; k++) {
            tmp[k] = spiftool_get_word(1, val_ptr);
            val_ptr = spiftool_get_pword(2, val_ptr);
            D_OPTIONS(("tmp[%d] == %s\n", k, tmp[k]));
        }
        tmp[k] = SPIF_NULL_TYPE(charptr);
        *(SPIF_CAST_C(spif_charptr_t **) SPIFOPT_OPT_VALUE(n)) = tmp;
    } else {
        unsigned short len = argc - i;

        /* No equals sign, so use the rest of the command line and break. */
        tmp = SPIF_CAST_PTR(charptr) MALLOC(sizeof(spif_charptr_t ) * (argc - i + 1));

        for (k = 0; k < len; k++) {
            tmp[k] = SPIF_CAST(charptr) STRDUP(argv[k + i]);
            D_OPTIONS(("tmp[%d] == %s\n", k, tmp[k]));
            argv[k + i] = NULL;
        }
        tmp[k] = SPIF_NULL_TYPE(charptr);
        *(SPIF_CAST_C(spif_charptr_t **) SPIFOPT_OPT_VALUE(n)) = tmp;
    }
}

/**
 * Parse the command line arguments for options.
 *
 * This function iterates through the command line arguments looking
 * for options which have been defined.  Each option encountered is
 * handled according to its type.
 *
 * @param argc The number of arguments.
 * @param argv The array of argument strings.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink
 * @ingroup DOXGRP_OPT
 */
void
spifopt_parse(int argc, char *argv[])
{
    spif_int32_t i, j;
    spif_charptr_t opt;

    REQUIRE(argc > 1);
    REQUIRE(argv != NULL);

    /* Process each command line arg one-by-one. */
    for (i = 1, opt = SPIF_CHARPTR(argv[1]); i < argc; ) {
        spif_charptr_t val_ptr = NULL;
        spif_char_t islong = 0, hasequal = 0;

        D_OPTIONS(("argv[%d] == \"%s\", opt == \"%s\"\n", i, argv[i], opt));

        if (SPIF_PTR_ISNULL(opt)) {
            /* NEXT_ARG(); */
            break;
        } else if (opt == SPIF_CHARPTR(argv[i])) {
            /* If it's not an option, skip it. */
            if (*opt != '-') {
                NEXT_ARG();
            } else {
                opt++;
            }
        }

        /* If the second character is also a hyphen, it's a long option. */
        if (*opt == '-') {
            islong = 1;
            /* Skip the leading "--" */
            opt++;
            D_OPTIONS(("Long option detected\n"));
            if ((j = find_long_option(opt)) == -1) {
                NEXT_ARG();
            }
        } else {
            if ((j = find_short_option(*opt)) == -1) {
                NEXT_LETTER();
            }
        }
        if (!SPIFOPT_FLAGS_IS_SET(SPIFOPT_SETTING_PREPARSE)) {
            argv[i] = NULL;
        }

        /* If a value was passed to this option, set val_ptr to point to it. */
        if (islong) {
            val_ptr = find_value_long(SPIF_CHARPTR(opt), SPIF_CHARPTR(argv[i + 1]), &hasequal);
        } else {
            val_ptr = find_value_short(opt, SPIF_CHARPTR(argv[i + 1]));
        }

        /* Boolean options may or may not have a value... */
        if (val_ptr) {
            if (SPIFOPT_OPT_IS_BOOLEAN(j) && !is_boolean_value(val_ptr)) {
                val_ptr = NULL;
            } else if (SPIFOPT_OPT_IS_ABSTRACT(j) && is_valid_option(val_ptr)) {
                val_ptr = NULL;
            }
        }
        if (val_ptr) {
            if (val_ptr == SPIF_CHARPTR(argv[i + 1])) {
                i++;
                opt += strlen(SPIF_CHARPTR_C(opt));
            }
        }

        /* If this option is deprecated, print a warning before continuing. */
        if (SPIFOPT_OPT_IS_DEPRECATED(j)) {
            spif_str_t warn;

            warn = spif_str_new_from_buff(SPIF_CHARPTR("The "), 128);
            if (SPIFOPT_OPT_SHORT(j)) {
                spif_str_append_char(warn, '-');
                spif_str_append_char(warn, SPIFOPT_OPT_SHORT(j));
                spif_str_append_from_ptr(warn, SPIF_CHARPTR(" / --"));
            } else {
                spif_str_append_from_ptr(warn, SPIF_CHARPTR("--"));
            }
            spif_str_append_from_ptr(warn, SPIFOPT_OPT_LONG(j));
            spif_str_append_from_ptr(warn, SPIF_CHARPTR(" option is deprecated and should not be used.\n"));
            libast_print_warning(SPIF_CHARPTR_C(SPIF_STR_STR(warn)));
            spif_str_del(warn);
        }

        /* Make sure that options which require a parameter have them. */
        if (SPIFOPT_OPT_NEEDS_VALUE(j)) {
            if (val_ptr == NULL) {
                if (islong) {
                    libast_print_error("long option --%s requires a%s value\n", SPIFOPT_OPT_LONG(j),
                                (SPIFOPT_OPT_IS_INTEGER(j)
                                 ? ("n integer")
                                 : (SPIFOPT_OPT_IS_STRING(j)
                                    ? " string"
                                    : (SPIFOPT_OPT_IS_ARGLIST(j)
                                       ? "n argument list"
                                       : ""))));
                } else {
                    libast_print_error("option -%c requires a%s value\n", SPIFOPT_OPT_SHORT(j),
                                (SPIFOPT_OPT_IS_INTEGER(j)
                                 ? ("n integer")
                                 : (SPIFOPT_OPT_IS_STRING(j)
                                    ? " string"
                                    : (SPIFOPT_OPT_IS_ARGLIST(j)
                                       ? "n argument list"
                                       : ""))));
                }
                CHECK_BAD();
                continue;
            }
            /* Also make sure we know what to do with the value. */
            if (SPIFOPT_OPT_VALUE(j) == NULL) {
                NEXT_LOOP();
            }
        } else if (SPIFOPT_OPT_IS_ABSTRACT(j) && SPIFOPT_OPT_VALUE(j) == NULL) {
            /* Also make sure that abstract options have a function pointer. */
            NEXT_LOOP();
        }

        if (SPIFOPT_OPT_IS_BOOLEAN(j)) {
            if (!handle_boolean(j, val_ptr, islong)) {
                i--;
            }
        } else if (SPIFOPT_OPT_IS_STRING(j)) {
            if (SHOULD_PARSE(j)) {
                handle_string(j, val_ptr);
            }
        } else if (SPIFOPT_OPT_IS_INTEGER(j)) {
            if (SHOULD_PARSE(j)) {
                handle_integer(j, val_ptr);
            }
        } else if (SPIFOPT_OPT_IS_ARGLIST(j)) {
            if (SHOULD_PARSE(j)) {
                handle_arglist(j, val_ptr, hasequal, i, argc, argv);
            }
            if (!hasequal) {
                break;
            }
        } else if (SPIFOPT_OPT_IS_ABSTRACT(j)) {
            if (SHOULD_PARSE(j)) {
                D_OPTIONS(("Abstract option detected\n"));
                ((spifopt_abstract_handler_t) SPIFOPT_OPT_VALUE(j))(val_ptr);
            }
        }
        if (!SPIFOPT_FLAGS_IS_SET(SPIFOPT_SETTING_PREPARSE)) {
            argv[i] = NULL;
        }
        NEXT_LOOP();
    }

    if (SPIFOPT_FLAGS_IS_SET(SPIFOPT_SETTING_PREPARSE)) {
        SPIFOPT_FLAGS_CLEAR(SPIFOPT_SETTING_PREPARSE);
    } else {
        for (i = 1, j = 1; i < argc; i++) {
            if (argv[i]) {
                argv[j] = argv[i];
                j++;
            }
        }
        if (j > 1) {
            argv[j] = NULL;
        }
    }
}

/**
 * @defgroup DOXGRP_OPT Command Line Option Parser
 *
 * This group of functions/defines/macros comprises the command line
 * option parser.
 *
 *
 * A small sample program demonstrating some of these routines can be
 * found @link opt_example.c here @endlink.
 */

/**
 * @example opt_example.c
 * Example code for using the options parser.
 *
 */
