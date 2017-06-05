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
 * @file debug.c
 * Debugging Subsystem Source File
 *
 * This file contains all non-cpp-based debugging functionality.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.14 $
 * $Date: 2004/12/15 00:00:22 $
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: debug.c,v 1.14 2004/12/15 00:00:22 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libast_internal.h"

/**
 * Debug level.
 *
 * This variable holds the current debug level.
 *
 * @internal
 * @see DEBUG_LEVEL
 * @ingroup DOXGRP_DEBUG
 */
unsigned int libast_debug_level = 0;
/**
 * Debug flags.
 *
 * This variable holds the current debug flags.  Currently unused.
 *
 * @internal
 * @see DEBUG_FLAGS
 * @ingroup DOXGRP_DEBUG
 */
unsigned long libast_debug_flags = 0;

/**
 * @defgroup DOXGRP_DEBUG Debugging Subsystem
 *
 * This group of functions/defines/macros implements the debugging
 * subsystem within LibAST.
 *
 * LibAST provides a flexible level-based debugging mechanism to
 * client programs, supporting both compile-time and run-time methods
 * for including or suppressing debugging output by increasing or
 * decreasing the "debug level," i.e. the verbosity, of the system.
 *
 * The current level of debugging output is controlled by the
 * DEBUG_LEVEL variable; to alter it, simply assign a new integer
 * value (from 0 to 9, inclusive) to this variable.
 *
 * LibAST provides debugging for several of its own subsystems and
 * allows client applications to use the same debugging mechanisms by
 * defining its own categories. To use the LibAST debugging system,
 * you will need one #define and one macro for each subsystem or
 * debugging category your program will have.
 *
 * For example, let's say your code contains some socket code.  To
 * create a debugging entity for that code, simply add a #define which
 * sets the debug level for it, and a macro to map output messages to
 * the DPRINTF*() macro for that debug level.  If you wanted
 * socket-related debugging information at debug level 2, you'd use
 * something like these:
 *
 * @code
 * #define DEBUG_SOCKETS  2
 * #define D_SOCKETS(x)   DPRINTF2(x)
 * @endcode
 *
 * That's all there is to it!  Now, anywhere you want socket-related
 * debugging output, just use the D_SOCKETS() macro like so:
 *
 * @code
 *     D_SOCKETS(("Connecting to %s...\n", hostname));
 * @endcode
 *
 * Be sure to use TWO sets of parentheses, not just one.  Don't worry;
 * you'll get used to it! :-)
 *
 * That's all there is to it!
 *
 * If you'd like to see a sample program which uses this system, click
 * @link debug_example.c here @endlink.
 */
/**
 * @example debug_example.c
 * Example code for using the debugging subsystem.
 *
 * This is a trivial code example that uses LibAST-style debugging.
 * The program takes a number, a symbol, and another number on the
 * command line to perform simple arithmetic.  Specify the -d option
 * to enable the debugging output.
 *
 * With debugging output turned off (no -d option), you should see
 * something like this:
 *
 * @code
 * $ ./debug_example 4 + 4
 * 4 + 4 = 8
 * $
 * @endcode
 *
 * With debugging output turned on, you should see something like
 * this:
 *
 * @code
 * $ ./debug_example -d 10 - 14
 * [1045855757] debug_example.c |   35: main(): Debugging is now on.
 * [1045855757] debug_example.c |   48: main(): Number 1 is 10
 * [1045855757] debug_example.c |   43: main(): Got operation '-'.
 * [1045855757] debug_example.c |   51: main(): Number 2 is 14
 * 10 - 14 = -4
 * $
 * @endcode
 *
 * As you can see, the debugging output contains UNIX-style time_t
 * timestamps, source file names, line numbers, and function names for
 * each debugging statement.
 *
 * Here's the complete source code:
 */

/* Documentation for the main Doxygen-generated page is here, for no
 * other reason than this file lacks actual code. :-)
 */

/**
 * @mainpage LibAST 0.6.1 Documentation
 *
 * This document describes the various features and capabilities
 * offered by LibAST, the Library of Assorted Spiffy Things.  As its
 * name suggests, LibAST is a collection of various functions, macros,
 * etc. which do all sorts of spiffy stuff.  By reading through all
 * the following pages of goop, hopefully you'll learn about the
 * miscellaneous mounds of spiffitude which await you in LibAST!
 *
 * Choose a section from the list below, or pick one of the quick
 * links at the top of the page.  If you prefer tree-style navigation,
 * there is a hidden frame on the left-hand side of your browser
 * window.  You can expand this frame to find the document tree
 * (requires JavaScript!).
 *
 * @section topiclist Topic List
 *
 * -# @link DOXGRP_TYPES   Portable Data Types @endlink
 * -# @link DOXGRP_DEBUG   Debugging Subsystem @endlink
 * -# @link DOXGRP_MEM     Memory Management Subsystem @endlink
 * -# @link DOXGRP_STRINGS String Utility Routines @endlink
 * -# @link DOXGRP_CONF    Configuration File Parser @endlink
 *   -# @link DOXGRP_CONF_FSS    File State Stack @endlink
 *   -# @link DOXGRP_CONF_CTX    Context Handling @endlink
 * -# @link DOXGRP_OPT     Command Line Option Parser @endlink
 * -# @link DOXGRP_OBJ     Object Infrastructure @endlink
 *
 */
