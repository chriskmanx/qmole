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
 * @file libast_internal.h
 * LibAST header file for internal-use-only stuff.
 *
 * This file contains all macros, structure definitions, etc. which
 * are restricted to internal LibAST use only.
 *
 * @author Michael Jennings <mej@eterm.org>
 * @version $Revision: 1.21 $
 * @date $Date: 2004/07/21 22:17:49 $
 */

#ifndef _LIBAST_INTERNAL_H_
#define _LIBAST_INTERNAL_H_

/* This GNU goop has to go before the system headers */
#ifdef __GNUC__
# ifndef __USE_GNU
#  define __USE_GNU
# endif
# ifndef _GNU_SOURCE
#  define _GNU_SOURCE
# endif
# ifndef _BSD_SOURCE
#  define _BSD_SOURCE
# endif
#endif

#include "config.h"
#include "libast.h"

#ifdef HAVE_STDARG_H
# include <stdarg.h>
#endif

/******************************** MSGS GOOP ***********************************/
extern spif_charptr_t libast_program_name, libast_program_version;



/********************************* MEM GOOP ***********************************/
/**
 * Filename length limit.
 *
 * This is used to limit the maximum length of a source filename.
 * When tracking memory allocation, the filename and line number for
 * each MALLOC()/REALLOC()/CALLOC()/FREE() call is recorded.  A small
 * buffer of static length is used to speed things up.
 *
 * @see MALLOC(), REALLOC(), CALLOC(), FREE()
 * @ingroup DOXGRP_MEM
 */
#define LIBAST_FNAME_LEN  20

/**
 * Pointer tracking structure.
 *
 * This structure is used by LibAST's memory management system to keep
 * track of what pointers have been allocated, where they were
 * allocated, and how much space was requested.
 *
 * @see MALLOC(), REALLOC(), CALLOC(), FREE()
 * @ingroup DOXGRP_MEM
 */
typedef struct ptr_t_struct {
    /** The allocated pointer.  The allocated pointer. */
    void *ptr;
    /** The pointer's size, in bytes.  The pointer's size, in bytes. */
    size_t size;
    /** Filename.  The file which last (re)allocated the pointer. */
    spif_char_t file[LIBAST_FNAME_LEN + 1];
    /** Line number.  The line number where the pointer was last (re)allocated. */
    spif_uint32_t line;
} ptr_t;
/**
 * Pointer list structure.
 *
 * This structure is used by LibAST's memory management system to hold
 * the list of pointers being tracked.  This list is maintained as an
 * array for simplicity.
 *
 * @see MALLOC(), REALLOC(), CALLOC(), FREE(), ptr_t_struct
 * @ingroup DOXGRP_MEM
 */
typedef struct memrec_t_struct {
    /** Pointer count.  The number of pointers being tracked. */
    size_t cnt;
    /** Pointer list.  The list of tracked pointers. */
    ptr_t *ptrs;
} memrec_t;



/******************************** CONF GOOP ***********************************/
/**
 * Convert context name to ID.
 *
 * This macro converts a context name as read from a config file into
 * the corresponding ID number.  If the context name is not found, an
 * error message is printed, and the ID number of 0 (the "null"
 * context) is returned.
 *
 * @bug This probably should not be a macro.
 * @bug The @a i parameter really isn't needed.
 *
 * @param the_id The return value -- the context ID.
 * @param n      The name of the context.
 * @param i      An arbitrary counter variable.
 *
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink
 * @ingroup DOXGRP_CONF_CTX
 */
#define ctx_name_to_id(the_id, n, i) do { \
                                       for ((i)=0; (i) <= ctx_idx; (i)++) { \
                                         if (!strcasecmp(SPIF_CAST_C(char *) (n), \
                                                         SPIF_CAST_C(char *) context[(i)].name)) { \
                                           (the_id) = (i); \
                                           break; \
                                         } \
                                       } \
                                       if ((i) > ctx_idx) { \
                                         libast_print_error("Parsing file %s, line %lu:  No such context \"%s\"\n", \
                                                            file_peek_path(), file_peek_line(), (n)); \
                                         (the_id) = 0; \
                                       } \
                                     } while (0)
/**
 * Convert context ID to name.
 *
 * This macro converts a context ID to its name.
 *
 * @param id The context ID number.
 * @return   The name of the context.
 *
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink
 * @ingroup DOXGRP_CONF_CTX
 */
#define ctx_id_to_name(id)         (context[(id)].name)
/**
 * Convert context ID to handler.
 *
 * This macro returns the function pointer (a ctx_handler_t)
 * corresponding to the assigned handler function for the given
 * context.
 *
 * @param id The context ID number.
 * @return   The name of the context.
 *
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink
 * @ingroup DOXGRP_CONF_CTX
 */
#define ctx_id_to_func(id)         (context[(id)].handler)

/*@{*/
/**
 * @name Internal Context State Stack Manipulation Macros
 * Facilitate use of the context state stack.
 *
 * These macros provide a function-style interface to the CSS.
 *
 * @bug All this goop will be replaced with a spif object class.
 * @ingroup DOXGRP_CONF_CTX
 */

/**
 * Pushes a context onto the stack.  Pushes a context onto the stack.
 *
 * @param ctx The context ID for the new context.
 */
#define ctx_push(ctx)              spifconf_register_context_state(ctx)
/**
 * Pops a context structure off the stack.  Pops a context structure
 * off the stack.
 */
#define ctx_pop()                  (ctx_state_idx--)
/**
 * Returns the context structure atop the stack.  Returns the context
 * structure atop the stack.
 *
 * @return The current context.
 */
#define ctx_peek()                 (ctx_state[ctx_state_idx])
/**
 * Returns the context ID from the context structure atop the stack.
 * Returns the context ID from the context structure atop the stack.
 *
 * @return The current context ID.
 */
#define ctx_peek_id()              (ctx_state[ctx_state_idx].ctx_id)
/**
 * Returns the context state from the context structure atop the
 * stack.  Returns the context state from the context structure atop
 * the stack.
 *
 * @return The current context state.
 */
#define ctx_peek_state()           (ctx_state[ctx_state_idx].state)
/**
 * Returns the context ID from the context structure one level below
 * the top of the stack.  Returns the context ID from the context
 * structure one level below the top of the stack.
 *
 * @return The previous context ID.
 */
#define ctx_peek_last_id()         (ctx_state[(ctx_state_idx?ctx_state_idx-1:0)].ctx_id)
/**
 * Returns the context state from the context structure one level
 * below the top of the stack.  Returns the context state from the
 * context structure one level below the top of the stack.
 *
 * @return The previous context state.
 */
#define ctx_peek_last_state()      (ctx_state[(ctx_state_idx?ctx_state_idx-1:0)].state)
/**
 * Sets the current context state.  Sets the current context state.
 *
 * @param q The new state for the current context.
 */
#define ctx_poke_state(q)          ((ctx_state[ctx_state_idx].state) = (q))
/**
 * Gets the current depth of the context stack.  Gets the current
 * depth of the context stack.
 *
 * @return The current context stack depth.
 */
#define ctx_get_depth()            (ctx_state_idx)
/**
 * Convenience macro for beginning a new context.
 *
 * This macro simplifies the beginning of a new context.  The name
 * read from the config file is turned into a context ID which is then
 * registered with the parser.  The context handler for the new
 * context is called with CONF_BEGIN_STRING.  The returned state is
 * saved in the context structure atop the stack.
 *
 * @param idx The word number of the context name.
 */
#define ctx_begin(idx)             do { \
                                     spif_charptr_t name; \
                                     name = spiftool_get_word(idx, buff); \
                                     ctx_name_to_id(id, name, i); \
                                     ctx_push(id); \
                                     state = (*ctx_id_to_func(id))(SPIF_CAST(charptr) SPIFCONF_BEGIN_STRING, \
                                                                   ctx_peek_last_state()); \
                                     ctx_poke_state(state); \
                                     FREE(name); \
                                   } while (0)
/**
 * Convenience macro for ending a context.
 *
 * This macro simplifies the ending of a context.  The context handler
 * for the context is called with SPIFCONF_END_STRING.  The old context is
 * then popped off the stack, and the returned state is saved for the
 * parent context.
 *
 */
#define ctx_end()                  do { \
                                     if (ctx_get_depth()) { \
                                       state = (*ctx_id_to_func(id))(SPIF_CAST(charptr) SPIFCONF_END_STRING, \
                                                                     ctx_peek_state()); \
                                       ctx_poke_state(NULL); \
                                       ctx_pop(); \
                                       id = ctx_peek_id(); \
                                       ctx_poke_state(state); \
                                       file_poke_skip(0); \
                                     } \
                                   } while (0)
/*@}*/

/**
 * Context structure.
 *
 * This structure is used to hold context names and their respective
 * handlers (#ctx_handler_t).
 *
 * @bug This needs to be turned into a spif object class.
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink
 * @ingroup DOXGRP_CONF_CTX
 */
typedef struct ctx_t_struct {
    /**
     * Context name.
     *
     * The string representation of the context name.  The word
     * immediately after the keyword @c begin is compared with this
     * value to find the handler for the requested context.  This
     * comparison is case-insensitive.
     */
    spif_charptr_t name;
    /**
     * Context handler.
     *
     * Pointer to the function which will handle this context.
     * Context handlers must accept two parameters, a spif_charptr_t 
     * containing either the config file line or a begin/end magic
     * string, and a void * containing state information; they must
     * return a void * which will be passed to the next invocation of
     * the handler as the aforementioned state information parameter.
     */
    ctx_handler_t handler;
} ctx_t;

/**
 * Context state structure.
 *
 * This structure is used as part of the context stack to track the
 * current context and its state information.
 *
 * @bug This needs to be turned into a spif object class.
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink
 * @ingroup DOXGRP_CONF_CTX
 */
typedef struct ctx_state_t_struct {
    /**
     * Context ID.
     *
     * The ID number of the context.
     */
    unsigned char ctx_id;
    /**
     * Context state.
     *
     * The state for the context.  This holds the state variable in
     * between calls to the handler (ctx_t_struct#handler).
     */
    void *state;
} ctx_state_t;

/**
 * Built-in config file function.
 *
 * This structure holds the name and a pointer to the handler for
 * built-in config file functions (%get(), %random(), etc.).
 *
 * @bug This needs to be turned into a spif object class.
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink
 * @ingroup DOXGRP_CONF_CTX
 */
typedef struct spifconf_func_t_struct {
    /**
     * Function name.
     *
     * The string representation of the built-in function name, not
     * including the leading percent sign ('%').
     */
    spif_charptr_t name;
    /**
     * Function handler pointer.
     *
     * Pointer to the handler for the built-in function.
     */
    spifconf_func_ptr_t ptr;
} spifconf_func_t;

/**
 * Linked list for user-defined config file variables.
 *
 * This structure holds the name and value for user-defined config
 * file variables set/retrieved using the %get() and %put() built-in
 * functions.
 *
 * @bug This needs to be turned into a spif object class.
 * @see @link DOXGRP_CONF_CTX Context Handling @endlink, builtin_get(), builtin_put()
 * @ingroup DOXGRP_CONF_CTX
 */
typedef struct spifconf_var_t_struct {
    /**
     * Variable name.
     *
     * The string representation of the variable name.  Variable names
     * ARE case-sensitive!
     */
    spif_charptr_t var;
    /**
     * Variable value.
     *
     * The value of the user-defined variable.  The value must be a
     * text string (obviously, since the config files are text-based.
     */
    spif_charptr_t value;
    /**
     * Linked list pointer.
     *
     * Pointer to the next variable in the list.
     */
    struct spifconf_var_t_struct *next;
} spifconf_var_t;



/******************************* OPTIONS GOOP **********************************/

/**
 * Increment and check bad option counter.
 *
 * This is a convenience macro which is invoked each time an option
 * parsing error is encountered.  This macro increments the bad option
 * count within the parser and checks to see if the count has now
 * exceeded the threshold.  If so, the registered help handler
 * (spifopt_settings_t_struct#help_handler) via the
 * #SPIFOPT_HELPHANDLER() macro.  If not, an error message is printed
 * noting that behavior may be abnormal but that parsing will
 * continue.
 *
 * @see @link DOXGRP_OPT Command Line Option Parser @endlink
 * @ingroup DOXGRP_OPT
 */
#define CHECK_BAD()  do { \
                       SPIFOPT_BADOPTS_SET(SPIFOPT_BADOPTS_GET() + 1); \
                       if (SPIFOPT_BADOPTS_GET() > SPIFOPT_ALLOWBAD_GET()) { \
                         libast_print_error("Error threshold exceeded, giving up.\n"); \
                         SPIFOPT_HELPHANDLER(); \
                       } else { \
                         libast_print_error("Attempting to continue, but strange things may happen.\n"); \
                       } \
                     } while(0)


#endif /* _LIBAST_INTERNAL_H_ */
