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
 * @file mem.c
 * Memory Management Subsystem Source File
 *
 * This file contains the memory management subsystem.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.25 $
 * $Date: 2005/12/22 23:28:55 $
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: mem.c,v 1.25 2005/12/22 23:28:55 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libast_internal.h"

static void memrec_add_var(memrec_t *, const spif_charptr_t, unsigned long, void *, size_t);
static ptr_t *memrec_find_var(memrec_t *, const void *);
static void memrec_rem_var(memrec_t *, const spif_charptr_t, const spif_charptr_t, unsigned long, const void *);
static void memrec_chg_var(memrec_t *, const spif_charptr_t, const spif_charptr_t, unsigned long, const void *, void *, size_t);
static void memrec_dump_pointers(memrec_t *);
static void memrec_dump_resources(memrec_t *);

#if MALLOC_CALL_DEBUG
/*@{*/
/**
 * @name Memory Management Call Tracking
 * Count calls to memory management functions.
 *
 * This group of variables is used to count calls to the memory
 * management functions.  Call counting is controlled by the
 * #MALLOC_CALL_DEBUG symbol, and is off by default.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */

/** Count calls to MALLOC().  Count calls to MALLOC(). */
static int malloc_count = 0;
/** Count calls to CALLOC().  Count calls to CALLOC(). */
static int calloc_count = 0;
/** Count calls to REALLOC().  Count calls to REALLOC(). */
static int realloc_count = 0;
/** Count calls to FREE().  Count calls to FREE(). */
static int free_count = 0;
/*@}*/
#endif

/**
 * Allocated pointers.
 *
 * This structure keeps track of the pointer array which represents
 * pointers allocated via the memory management interface.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, memrec_t_struct
 * @ingroup DOXGRP_MEM
 */
static memrec_t malloc_rec;
/**
 * Allocated pixmaps.
 *
 * This structure keeps track of the pixmap array which represents
 * pixmaps allocated via the memory management interface.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, memrec_t_struct
 * @ingroup DOXGRP_MEM
 */
static memrec_t pixmap_rec;
/**
 * Allocated GC's.
 *
 * This structure keeps track of the GC array which represents
 * X11 Graphics Context objects, or GC's, allocated via the memory
 * management interface.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, memrec_t_struct
 * @ingroup DOXGRP_MEM
 */
static memrec_t gc_rec;

/**
 * Initialize memory management system.
 *
 * A call to this function must occur before any other part of the
 * memory management system is used.  This function initializes the
 * pointer lists.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink
 * @ingroup DOXGRP_MEM
 */
void
spifmem_init(void)
{
    D_MEM(("Constructing memory allocation records\n"));
    malloc_rec.ptrs = (ptr_t *) malloc(sizeof(ptr_t));
    pixmap_rec.ptrs = (ptr_t *) malloc(sizeof(ptr_t));
    gc_rec.ptrs = (ptr_t *) malloc(sizeof(ptr_t));
}

/**
 * Add a variable to a record set.
 *
 * This is the static, internal-use-only function that does the actual
 * work of recording information on a variable to be tracked.  This
 * information includes file and line number information and is stored
 * as a #ptr_t.
 *
 * @param memrec   Address of the #memrec_t we're adding to.
 * @param filename The filename where the variable was allocated.
 * @param line     The line number of @a filename where the variable
 *                 was allocated.
 * @param ptr      The allocated variable.
 * @param size     The number of bytes requested.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, MALLOC(), libast_malloc()
 * @ingroup DOXGRP_MEM
 */
static void
memrec_add_var(memrec_t *memrec, const spif_charptr_t filename, unsigned long line, void *ptr, size_t size)
{
    register ptr_t *p;

    ASSERT(memrec != NULL);
    memrec->cnt++;
    if ((memrec->ptrs = (ptr_t *) realloc(memrec->ptrs, sizeof(ptr_t) * memrec->cnt)) == NULL) {
        D_MEM(("Unable to reallocate pointer list -- %s\n", strerror(errno)));
    }
    p = memrec->ptrs + memrec->cnt - 1;
    D_MEM(("Adding variable (%10p, %lu bytes) from %s:%lu.\n", ptr, size, filename, line));
    D_MEM(("Storing as pointer #%lu at %10p (from %10p).\n", memrec->cnt, p, memrec->ptrs));
    p->ptr = ptr;
    p->size = size;
    spiftool_safe_strncpy(p->file, SPIF_CONST_CAST(charptr) filename, LIBAST_FNAME_LEN);
    p->file[LIBAST_FNAME_LEN] = 0;
    p->line = line;
}

/**
 * Find a variable within a record set.
 *
 * This function searches through the pointer list of the specified
 * @a memrec object for a given pointer.
 *
 * @param memrec Address of the #memrec_t we're searching.
 * @param ptr    The value of the requested pointer.
 * @return       A pointer to the #ptr_t object within @a memrec
 *               that matches @a ptr, or NULL if not found.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, MALLOC(), libast_malloc()
 * @ingroup DOXGRP_MEM
 */
static ptr_t *
memrec_find_var(memrec_t *memrec, const void *ptr)
{
    register ptr_t *p;
    register unsigned long i;

    ASSERT_RVAL(memrec != NULL, NULL);
    REQUIRE_RVAL(ptr != NULL, NULL);

    for (i = 0, p = memrec->ptrs; i < memrec->cnt; i++, p++) {
        if (p->ptr == ptr) {
            D_MEM(("Found pointer #%lu stored at %10p (from %10p)\n", i + 1, p, memrec->ptrs));
            return p;
        }
    }
    return NULL;
}

/**
 * Remove a variable from a record set.
 *
 * This is the static, internal-use-only function that does the actual
 * work of freeing recorded information for a deleted pointer.
 *
 * @param memrec   Address of the #memrec_t we're removing from.
 * @param var      The variable name being freed (for diagnostic
 *                 purposes only).
 * @param filename The filename where the variable was freed.
 * @param line     The line number of @a filename where the variable
 *                 was freed.
 * @param ptr      The freed variable.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, FREE(), libast_free()
 * @ingroup DOXGRP_MEM
 */
static void
memrec_rem_var(memrec_t *memrec, const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, const void *ptr)
{
    register ptr_t *p;

    ASSERT(memrec != NULL);
    USE_VAR(var);
    USE_VAR(filename);
    USE_VAR(line);

    if ((p = memrec_find_var(memrec, ptr)) == NULL) {
        D_MEM(("ERROR:  File %s, line %d attempted to free variable %s (%10p) which was not allocated with MALLOC/REALLOC\n",
               filename, line, var, ptr));
        return;
    }
    D_MEM(("Removing variable %s (%10p) of size %lu\n", var, ptr, p->size));
    if ((--memrec->cnt) > 0) {
        memmove(p, p + 1, sizeof(ptr_t) * (memrec->cnt - (p - memrec->ptrs)));
        memrec->ptrs = (ptr_t *) realloc(memrec->ptrs, sizeof(ptr_t) * memrec->cnt);
    }
}

/**
 * Resize a variable in a record set.
 *
 * This is the static, internal-use-only function that does the actual
 * work of altering information on a tracked variable.
 *
 * @param memrec   Address of the #memrec_t we're modifying.
 * @param var      The variable name being resized (for diagnostic
 *                 purposes only).
 * @param filename The filename where the variable was resized.
 * @param line     The line number of @a filename where the variable
 *                 was resized.
 * @param oldp     The old value of the pointer.
 * @param newp     The new value of the pointer.
 * @param size     The new size in bytes.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, REALLOC(), libast_realloc()
 * @ingroup DOXGRP_MEM
 */
static void
memrec_chg_var(memrec_t *memrec, const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, const void *oldp, void *newp, size_t size)
{
    register ptr_t *p;

    ASSERT(memrec != NULL);
    USE_VAR(var);

    if ((p = memrec_find_var(memrec, oldp)) == NULL) {
        D_MEM(("ERROR:  File %s, line %d attempted to realloc variable %s (%10p) which was not allocated with MALLOC/REALLOC\n", filename,
               line, var, oldp));
        return;
    }
    D_MEM(("Changing variable %s (%10p, %lu -> %10p, %lu)\n", var, oldp, p->size, newp, size));
    p->ptr = newp;
    p->size = size;
    spiftool_safe_strncpy(p->file, SPIF_CONST_CAST(charptr) filename, LIBAST_FNAME_LEN);
    p->line = line;
}

/**
 * Dump listing of tracked pointers.
 *
 * This function dumps a listing of all pointers in @a memrec along
 * with the filename, line number, address, size, and contents for
 * each.  Contents are displayed in both hex and ASCII, the latter
 * having non-printable characters replaced with periods ('.').
 *
 * @param memrec Address of the #memrec_t we're dumping.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, MALLOC_DUMP(), libast_dump_mem_tables()
 * @ingroup DOXGRP_MEM
 */
static void
memrec_dump_pointers(memrec_t *memrec)
{
    register ptr_t *p;
    unsigned long i, j, k, l, total = 0;
    unsigned long len;
    spif_char_t buff[9];

    ASSERT(memrec != NULL);
    fprintf(LIBAST_DEBUG_FD, "PTR:  %lu pointers stored.\n", SPIF_CAST_C(unsigned long) memrec->cnt);
    fprintf(LIBAST_DEBUG_FD,
            "PTR:   Pointer |       Filename       |  Line  |  Address |  Size  | Offset  | 00 01 02 03 04 05 06 07 |  ASCII  \n");
    fprintf(LIBAST_DEBUG_FD,
            "PTR:  ---------+----------------------+--------+----------+--------+---------+-------------------------+---------\n");
    fflush(LIBAST_DEBUG_FD);
    len = sizeof(ptr_t) * memrec->cnt;
    memset(buff, 0, sizeof(buff));

    /* First, dump the contents of the memrec->ptrs[] array. */
    for (p = memrec->ptrs, j = 0; j < len; j += 8) {
        fprintf(LIBAST_DEBUG_FD, "PTR:   %07lu | %20s | %6lu | %10p | %06lu | %07x | ",
                (unsigned long) 0, "", (unsigned long) 0,
                SPIF_CAST(ptr) memrec->ptrs,
                (unsigned long) (sizeof(ptr_t) * memrec->cnt), (unsigned int) j);
        /* l is the number of characters we're going to output */
        l = ((len - j < 8) ? (len - j) : (8));
        /* Copy l bytes (up to 8) from memrec->ptrs[] (p) to buffer */
        memcpy(buff, ((spif_charptr_t) p) + j, l);
        buff[l] = 0;
        for (k = 0; k < l; k++) {
            fprintf(LIBAST_DEBUG_FD, "%02x ", buff[k]);
        }
        /* If we printed less than 8 bytes worth, pad with 3 spaces per byte */
        for (; k < 8; k++) {
            fprintf(LIBAST_DEBUG_FD, "   ");
        }
        /* Finally, print the printable ASCII string for those l bytes */
        fprintf(LIBAST_DEBUG_FD, "| %-8s\n", spiftool_safe_str((spif_charptr_t) buff, l));
        /* Flush after every line in case we crash */
        fflush(LIBAST_DEBUG_FD);
    }

    /* Now print out each pointer and its contents. */
    for (i = 0; i < memrec->cnt; p++, i++) {
        /* Add this pointer's size to our total */
        total += p->size;
        for (j = 0; j < p->size; j += 8) {
            fprintf(LIBAST_DEBUG_FD, "PTR:   %07lu | %20s | %6lu | %10p | %06lu | %07x | ",
                    i + 1, NONULL(p->file), SPIF_CAST_C(unsigned long) p->line,
                    p->ptr, SPIF_CAST_C(unsigned long) p->size, SPIF_CAST_C(unsigned) j);
            /* l is the number of characters we're going to output */
            l = ((p->size - j < 8) ? (p->size - j) : (8));
            /* Copy l bytes (up to 8) from p->ptr to buffer */
            memcpy(buff, ((spif_charptr_t) p->ptr) + j, l);
            buff[l] = 0;
            for (k = 0; k < l; k++) {
                fprintf(LIBAST_DEBUG_FD, "%02x ", buff[k]);
            }
            /* If we printed less than 8 bytes worth, pad with 3 spaces per byte */
            for (; k < 8; k++) {
                fprintf(LIBAST_DEBUG_FD, "   ");
            }
            /* Finally, print the printable ASCII string for those l bytes */
            fprintf(LIBAST_DEBUG_FD, "| %-8s\n", spiftool_safe_str(buff, l));
            /* Flush after every line in case we crash */
            fflush(LIBAST_DEBUG_FD);
        }
    }
    fprintf(LIBAST_DEBUG_FD, "PTR:  Total allocated memory: %10lu bytes\n", total);
    fflush(LIBAST_DEBUG_FD);
}

/**
 * Dump listing of tracked resources.
 *
 * This function is very similar to memrec_dump_pointers() but is
 * intended for use with non-pointer data.
 *
 * @param memrec Address of the #memrec_t we're dumping.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, MALLOC_DUMP(), libast_dump_mem_tables(),
 *      memrec_dump_pointers()
 * @ingroup DOXGRP_MEM
 */
static void
memrec_dump_resources(memrec_t *memrec)
{
    register ptr_t *p;
    unsigned long i, total;
    unsigned long len;

    ASSERT(memrec != NULL);
    len = memrec->cnt;
    fprintf(LIBAST_DEBUG_FD, "RES:  %lu resources stored.\n",
            SPIF_CAST_C(unsigned long) memrec->cnt);
    fprintf(LIBAST_DEBUG_FD, "RES:   Index | Resource ID |       Filename       |  Line  |  Size  \n");
    fprintf(LIBAST_DEBUG_FD, "RES:  -------+-------------+----------------------+--------+--------\n");
    fflush(LIBAST_DEBUG_FD);

    for (p = memrec->ptrs, i = 0, total = 0; i < len; i++, p++) {
        total += p->size;
        fprintf(LIBAST_DEBUG_FD, "RES:   %5lu |  0x%08lx | %20s | %6lu | %6lu\n",
                i, SPIF_CAST_C(unsigned long) p->ptr, NONULL(p->file),
                SPIF_CAST_C(unsigned long) p->line,
                SPIF_CAST_C(unsigned long) p->size);
        /* Flush after every line in case we crash */
        fflush(LIBAST_DEBUG_FD);
    }
    fprintf(LIBAST_DEBUG_FD, "RES:  Total size: %lu bytes\n", SPIF_CAST_C(unsigned long) total);
    fflush(LIBAST_DEBUG_FD);
}

/******************** MEMORY ALLOCATION INTERFACE ********************/

/**
 * LibAST implementation of malloc().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * MALLOC() macro are routed here.  The macro allows filename and line
 * number information to be provided thanks to the __FILE__ and
 * __LINE__ symbols pre-defined by cpp.
 *
 * @param filename The filename where the variable is being
 *                 allocated.
 * @param line     The line number of @a filename where the variable
 *                 is being allocated.
 * @param size     The requested size in bytes (as passed to MALLOC()).
 * @return         A pointer to the newly-allocated memory.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, MALLOC()
 * @ingroup DOXGRP_MEM
 */
void *
spifmem_malloc(const spif_charptr_t filename, unsigned long line, size_t size)
{
    void *temp;

#if MALLOC_CALL_DEBUG
    ++malloc_count;
    if (!(malloc_count % MALLOC_MOD)) {
        fprintf(LIBAST_DEBUG_FD, "Calls to malloc(): %d\n", malloc_count);
    }
#endif

    D_MEM(("%lu bytes requested at %s:%lu\n", size, NONULL(filename), line));

    temp = (void *) malloc(size);
    ASSERT_RVAL(!SPIF_PTR_ISNULL(temp), SPIF_NULL_TYPE(ptr));
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        memrec_add_var(&malloc_rec, SPIF_CAST(charptr) NONULL(filename), line, temp, size);
    }
    return (temp);
}

/**
 * LibAST implementation of realloc().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * REALLOC() macro are routed here.  The macro allows variable name,
 * filename, and line number information to be provided.
 *
 * @param var      The variable name being resized.
 * @param filename The filename where the variable is being
 *                 reallocated.
 * @param line     The line number of @a filename where the variable
 *                 is being reallocated.
 * @param ptr      The old value of the pointer being resized.
 * @param size     The new requested size in bytes (as passed to
 *                 REALLOC()). 
 * @return         The new value (possibly moved) of the pointer.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, REALLOC()
 * @ingroup DOXGRP_MEM
 */
void *
spifmem_realloc(const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, void *ptr, size_t size)
{
    void *temp;

#if MALLOC_CALL_DEBUG
    ++realloc_count;
    if (!(realloc_count % REALLOC_MOD)) {
        D_MEM(("Calls to realloc(): %d\n", realloc_count));
    }
#endif

    D_MEM(("Variable %s (%10p -> %lu) at %s:%lu\n", var, ptr, (unsigned long) size, NONULL(filename), line));
    if (ptr == NULL) {
        temp = (void *) spifmem_malloc(filename, line, size);
    } else if (size == 0) {
        spifmem_free(var, filename, line, ptr);
        temp = NULL;
    } else {
        temp = (void *) realloc(ptr, size);
        ASSERT_RVAL(!SPIF_PTR_ISNULL(temp), SPIF_NULL_TYPE(ptr));
        if (DEBUG_LEVEL >= DEBUG_MEM) {
            memrec_chg_var(&malloc_rec, var, SPIF_CAST(charptr) NONULL(filename), line, ptr, temp, size);
        }
    }
    return (temp);
}

/**
 * LibAST implementation of calloc().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * CALLOC() macro are routed here.  The macro allows filename and line
 * number information to be provided thanks to the __FILE__ and
 * __LINE__ symbols pre-defined by cpp.
 *
 * @param filename The filename where the variable is being
 *                 allocated.
 * @param line     The line number of @a filename where the variable
 *                 is being allocated.
 * @param count    The number of objects being allocated.
 * @param size     The size in bytes of each object (as passed to
 *                 CALLOC()).
 * @return         A pointer to the newly-allocated, zeroed memory.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, CALLOC()
 * @ingroup DOXGRP_MEM
 */
void *
spifmem_calloc(const spif_charptr_t filename, unsigned long line, size_t count, size_t size)
{
    void *temp;

#if MALLOC_CALL_DEBUG
    ++calloc_count;
    if (!(calloc_count % CALLOC_MOD)) {
        fprintf(LIBAST_DEBUG_FD, "Calls to calloc(): %d\n", calloc_count);
    }
#endif

    D_MEM(("%lu units of %lu bytes each requested at %s:%lu\n", count, size, NONULL(filename), line));
    temp = (void *) calloc(count, size);
    ASSERT_RVAL(!SPIF_PTR_ISNULL(temp), SPIF_NULL_TYPE(ptr));
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        memrec_add_var(&malloc_rec, SPIF_CAST(charptr) NONULL(filename), line, temp, size * count);
    }
    return (temp);
}

/**
 * LibAST implementation of free().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * FREE() macro are routed here.  The macro allows variable name,
 * filename, and line number information to be provided.
 *
 * @param var      The variable name being freed.
 * @param filename The filename where the variable is being freed.
 * @param line     The line number of @a filename where the variable
 *                 is being freed.
 * @param ptr      The pointer being freed (as passed to FREE()).
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, FREE()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_free(const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, void *ptr)
{
#if MALLOC_CALL_DEBUG
    ++free_count;
    if (!(free_count % FREE_MOD)) {
        fprintf(LIBAST_DEBUG_FD, "Calls to free(): %d\n", free_count);
    }
#endif

    D_MEM(("Variable %s (%10p) at %s:%lu\n", var, ptr, NONULL(filename), line));
    if (ptr) {
        if (DEBUG_LEVEL >= DEBUG_MEM) {
            memrec_rem_var(&malloc_rec, var, SPIF_CAST(charptr) NONULL(filename), line, ptr);
        }
        free(ptr);
    } else {
        D_MEM(("ERROR:  Caught attempt to free NULL pointer\n"));
    }
}

/**
 * LibAST implementation of strdup().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * STRDUP() macro are routed here.  The macro allows variable name,
 * filename, and line number information to be provided.
 *
 * @param var      The variable name being duplicated.
 * @param filename The filename where the variable is being duplicated.
 * @param line     The line number of @a filename where the variable
 *                 is being duplicated.
 * @param str      The string being duplicated (as passed to STRDUP()).
 * @return         A pointer to a newly-allocated copy of @a str.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, STRDUP()
 * @ingroup DOXGRP_MEM
 */
spif_charptr_t 
spifmem_strdup(const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, const spif_charptr_t str)
{
    register spif_charptr_t newstr;
    register size_t len;

    ASSERT_RVAL(!SPIF_PTR_ISNULL(str), SPIF_NULL_TYPE_C(spif_charptr_t));
    USE_VAR(var);
    D_MEM(("Variable %s (%10p) at %s:%lu\n", var, str, NONULL(filename), line));

    len = strlen(SPIF_CAST_C(char *) str) + 1;      /* Copy NUL byte also */
    newstr = SPIF_CAST(charptr) spifmem_malloc(SPIF_CAST(charptr) NONULL(filename), line, len);
    ASSERT_RVAL(!SPIF_PTR_ISNULL(newstr), SPIF_NULL_TYPE(ptr));
    strcpy(SPIF_CAST_C(char *) newstr, SPIF_CAST_C(char *) str);
    return (newstr);
}

/**
 * Dump listing of tracked pointers.
 *
 * This function simply calls memrec_dump_pointers() and passes in the
 * address of the #malloc_rec variable.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, MALLOC_DUMP(), memrec_dump_pointers()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_dump_mem_tables(void)
{
    fprintf(LIBAST_DEBUG_FD, "Dumping memory allocation table:\n");
    memrec_dump_pointers(&malloc_rec);
}

#if LIBAST_X11_SUPPORT

/******************** PIXMAP ALLOCATION INTERFACE ********************/

/**
 * LibAST implementation of XCreatePixmap().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * X_CREATE_PIXMAP() macro are routed here.  The macro allows filename
 * and line number information to be provided thanks to the __FILE__
 * and __LINE__ symbols pre-defined by cpp.
 *
 * @param filename The filename where the pixmap is being created.
 * @param line     The line number of @a filename where the pixmap is
 *                 being created.
 * @param d        The Display for the new pixmap.
 * @param win      The Drawable for the new pixmap.
 * @param w        Width of the pixmap, in pixels.
 * @param h        Height of the pixmap, in pixels.
 * @param depth    The color depth for the new pixmap.
 * @return         A newly-created Pixmap.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, X_CREATE_PIXMAP()
 * @ingroup DOXGRP_MEM
 */
Pixmap
spifmem_x_create_pixmap(const spif_charptr_t filename, unsigned long line, Display * d, Drawable win, unsigned int w, unsigned int h,
                       unsigned int depth)
{
    Pixmap p;

    p = XCreatePixmap(d, win, w, h, depth);
    D_MEM(("Created %ux%u pixmap 0x%08x of depth %u for window 0x%08x at %s:%lu\n", w, h, p, depth, win, NONULL(filename), line));
    ASSERT_RVAL(p != None, None);
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        memrec_add_var(&pixmap_rec, SPIF_CAST(charptr) NONULL(filename), line, (void *) p, w * h * (depth / 8));
    }
    return (p);
}

/**
 * LibAST implementation of XFreePixmap().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * X_FREE_PIXMAP() macro are routed here.  The macro allows variable
 * name, filename, and line number information to be provided.
 *
 * @param var      The variable name of the pixmap being freed.
 * @param filename The filename where the pixmap is being freed.
 * @param line     The line number of @a filename where the pixmap is
 *                 being freed.
 * @param d        The Display for the pixmap.
 * @param p        The Pixmap to be freed.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, X_FREE_PIXMAP()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_x_free_pixmap(const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, Display * d, Pixmap p)
{
    D_MEM(("Freeing pixmap %s (0x%08x) at %s:%lu\n", var, p, NONULL(filename), line));
    if (p) {
        if (DEBUG_LEVEL >= DEBUG_MEM) {
            memrec_rem_var(&pixmap_rec, var, SPIF_CAST(charptr) NONULL(filename), line, (void *) p);
        }
        XFreePixmap(d, p);
    } else {
        D_MEM(("ERROR:  Caught attempt to free NULL pixmap\n"));
    }
}

# if LIBAST_IMLIB2_SUPPORT
/**
 * Register a pixmap for tracking.
 *
 * Imlib has its own mechanism for creating pixmaps internally.  In
 * order to keep track of these pixmaps, they must be registered with
 * LibAST using this function (via the IMLIB_REGISTER_PIXMAP() macro).
 *
 * @param var      The variable name of the pixmap being registered.
 * @param filename The filename where the pixmap is being registered.
 * @param line     The line number of @a filename where the pixmap is
 *                 being registered.
 * @param p        The Pixmap being registered.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, IMLIB_REGISTER_PIXMAP()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_imlib_register_pixmap(const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, Pixmap p)
{
    USE_VAR(var);
    D_MEM(("Registering pixmap %s (0x%08x) created by Imlib2 at %s:%lu\n", var, p, NONULL(filename), line));
    if (p) {
        if (DEBUG_LEVEL >= DEBUG_MEM) {
            if (!memrec_find_var(&pixmap_rec, (void *) p)) {
                memrec_add_var(&pixmap_rec, SPIF_CAST(charptr) NONULL(filename), line, (void *) p, 1);
            } else {
                D_MEM(("Pixmap 0x%08x already registered.\n"));
            }
        }
    } else {
        D_MEM(("ERROR:  Refusing to register a NULL pixmap\n"));
    }
}

/**
 * Free a pixmap created by Imlib.
 *
 * Imlib has its own mechanism for freeing pixmaps, and their
 * associated mask (if any), internally.  All pixmaps created by Imlib
 * must also be freed by Imlib.  It is safe, albeit a bit slower, to
 * free all pixmaps via Imlib, regardless of how they were created.
 *
 * @param var      The variable name of the pixmap being freed.
 * @param filename The filename where the pixmap is being freed.
 * @param line     The line number of @a filename where the pixmap is
 *                 being freed.
 * @param p        The Pixmap being freed.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, IMLIB_FREE_PIXMAP()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_imlib_free_pixmap(const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, Pixmap p)
{
    D_MEM(("Freeing pixmap %s (0x%08x) at %s:%lu using Imlib2\n", var, p, NONULL(filename), line));
    if (p) {
        if (DEBUG_LEVEL >= DEBUG_MEM) {
            memrec_rem_var(&pixmap_rec, var, SPIF_CAST(charptr) NONULL(filename), line, (void *) p);
        }
        imlib_free_pixmap_and_mask(p);
    } else {
        D_MEM(("ERROR:  Caught attempt to free NULL pixmap\n"));
    }
}
# endif

/**
 * Dump listing of tracked pixmaps.
 *
 * This function simply calls memrec_dump_resources() and passes in
 * the address of the #pixmap_rec variable.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, PIXMAP_DUMP(), memrec_dump_resources()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_dump_pixmap_tables(void)
{
    fprintf(LIBAST_DEBUG_FD, "Dumping X11 Pixmap allocation table:\n");
    memrec_dump_resources(&pixmap_rec);
}



/********************** GC ALLOCATION INTERFACE **********************/

/**
 * LibAST implementation of XCreateGC().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * X_CREATE_GC() macro are routed here.  The macro allows filename
 * and line number information to be provided thanks to the __FILE__
 * and __LINE__ symbols pre-defined by cpp.
 *
 * @param filename The filename where the GC is being created.
 * @param line     The line number of @a filename where the GC is
 *                 being created.
 * @param d        The Display for the new GC.
 * @param win      The Drawable for the new GC.
 * @param mask     Bitwise OR of zero or more GC flags.
 * @param gcv      Pointer to XGCValues structure.
 * @return         A newly-created GC.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, X_CREATE_GC()
 * @ingroup DOXGRP_MEM
 */
GC
spifmem_x_create_gc(const spif_charptr_t filename, unsigned long line, Display * d, Drawable win, unsigned long mask, XGCValues * gcv)
{
    GC gc;

    D_MEM(("Creating gc for window 0x%08x at %s:%lu\n", win, NONULL(filename), line));

    gc = XCreateGC(d, win, mask, gcv);
    ASSERT_RVAL(gc != None, None);
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        memrec_add_var(&gc_rec, SPIF_CAST(charptr) NONULL(filename), line, (void *) gc, sizeof(XGCValues));
    }
    return (gc);
}

/**
 * LibAST implementation of XFreeGC().
 *
 * When memory debugging is active (via #DEBUG_MEM), all calls to the
 * X_FREE_GC() macro are routed here.  The macro allows variable
 * name, filename, and line number information to be provided.
 *
 * @param var      The variable name of the GC being freed.
 * @param filename The filename where the GC is being freed.
 * @param line     The line number of @a filename where the GC is
 *                 being freed.
 * @param d        The Display for the GC.
 * @param gc       The GC to be freed.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, X_FREE_GC()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_x_free_gc(const spif_charptr_t var, const spif_charptr_t filename, unsigned long line, Display * d, GC gc)
{
    D_MEM(("spifmem_x_free_gc() called for variable %s (0x%08x) at %s:%lu\n", var, gc, NONULL(filename), line));
    if (gc) {
        if (DEBUG_LEVEL >= DEBUG_MEM) {
            memrec_rem_var(&gc_rec, var, SPIF_CAST(charptr) NONULL(filename), line, (void *) gc);
        }
        XFreeGC(d, gc);
    } else {
        D_MEM(("ERROR:  Caught attempt to free NULL GC\n"));
    }
}

/**
 * Dump listing of tracked GC's.
 *
 * This function simply calls memrec_dump_resources() and passes in
 * the address of the #gc_rec variable.
 *
 * @see @link DOXGRP_MEM Memory Management Subsystem @endlink, GC_DUMP(), memrec_dump_resources()
 * @ingroup DOXGRP_MEM
 */
void
spifmem_dump_gc_tables(void)
{
    fprintf(LIBAST_DEBUG_FD, "Dumping X11 GC allocation table:\n");
    memrec_dump_resources(&gc_rec);
}
#endif

/**
 * Free an array of pointers.
 *
 * This really doesn't relate to the memory management subsystem, per
 * se.  It is simply a convenience function which simplifies the
 * freeing of pointer arrays.  The first @a count pointers in the
 * array are freed, after which the array itself is freed.  If
 * @a count is 0, the array must be NULL-terminated.  All pointers up
 * to the first NULL pointer encountered will be freed.
 *
 * @param list  The pointer array to be freed.  This variable's value
 *              MUST NOT be used after being passed to this function.
 * @param count The number of pointers in the array, or 0 for a
 *              NULL-terminated array.
 *
 * @ingroup DOXGRP_MEM
 */
void
spiftool_free_array(void *list, size_t count)
{
    register size_t i;
    void **l = (void **) list;

    REQUIRE(list != NULL);

    if (count == 0) {
        count = (size_t) (-1);
    }
    for (i = 0; i < count && l[i]; i++) {
        FREE(l[i]);
    }
    FREE(list);
}

/**
 * @defgroup DOXGRP_MEM Memory Management Subsystem
 *
 * This group of functions/defines/macros implements the memory
 * management subsystem within LibAST.
 *
 * LibAST provides a robust mechanism for tracking memory allocations
 * and deallocations.  This system employs macro-based wrappers
 * around the standard libc malloc/realloc/calloc/free routines, other
 * libc fare such as strdup(), Xlib GC and Pixmap create/free
 * routines, and even Imlib2's own pixmap functions.
 *
 * To take advantage of this system, simply substitute the macro
 * versions in place of the standard versions throughout your code
 * (e.g., use MALLOC() instead of malloc(), X_FREE_GC() instead of
 * XFreeGC(), etc.).  If DEBUG is set to a value higher than
 * DEBUG_MEM, the LibAST-custom versions of these functions will be
 * used.  Of course, if memory debugging has not been requested, the
 * original libc/XLib/Imlib2 versions will be used instead, so that
 * you only incur the debugging overhead when you want it.
 *
 * LibAST has also been designed to work effectively with Gray
 * Watson's excellent malloc-debugging library, dmalloc
 * (http://dmalloc.com/), either instead of or in addition to its own
 * memory tracking routines.  Unlike LibAST, dmalloc supplements
 * memory allocation tracking with fence-post checking, freed pointer
 * reuse detection, and other very handy features.
 *
 * A small sample program demonstrating use of LibAST's memory
 * management system can be found
 * @link mem_example.c here @endlink.
 */

/**
 * @example mem_example.c
 * Example code for using the memory management subsystem.
 *
 * This small program demonstrates how to use LibAST's built-in memory
 * management system as well as a few of the errors it can catch for
 * you.
 *
 * The following shows output similar to what you can expect to
 * receive if you build and run this program:
 *
 * @code
 * $ ./mem_example 
 * [1045859036]        mem.c |  246: spifmem_malloc(): 500 bytes requested at mem_example.c:27
 * [1045859036]        mem.c |   74: memrec_add_var(): Adding variable (0x8049a20, 500 bytes) from mem_example.c:27.
 * [1045859036]        mem.c |   75: memrec_add_var(): Storing as pointer #1 at 0x8049c18 (from 0x8049c18).
 * [1045859036]        mem.c |  329: spifmem_strdup(): Variable pointer (0x8049a20) at mem_example.c:36
 * [1045859036]        mem.c |  246: spifmem_malloc(): 16 bytes requested at mem_example.c:36
 * [1045859036]        mem.c |   74: memrec_add_var(): Adding variable (0x8049c40, 16 bytes) from mem_example.c:36.
 * [1045859036]        mem.c |   75: memrec_add_var(): Storing as pointer #2 at 0x8049c7c (from 0x8049c58).
 * [1045859036]        mem.c |  312: spifmem_free(): Variable dup (0x8049c40) at mem_example.c:39
 * [1045859036]        mem.c |   94: memrec_find_var(): Found pointer #2 stored at 0x8049c7c (from 0x8049c58)
 * [1045859036]        mem.c |  113: memrec_rem_var(): Removing variable dup (0x8049c40) of size 16
 * [1045859036]        mem.c |  312: spifmem_free(): Variable dup (   (nil)) at mem_example.c:43
 * [1045859036]        mem.c |  319: spifmem_free(): ERROR:  Caught attempt to free NULL pointer
 * [1045859036]        mem.c |  268: spifmem_realloc(): Variable pointer (0x8049a20 -> 1000) at mem_example.c:46
 * [1045859036]        mem.c |   94: memrec_find_var(): Found pointer #1 stored at 0x8049c58 (from 0x8049c58)
 * [1045859036]        mem.c |  132: memrec_chg_var(): Changing variable pointer (0x8049a20, 500 -> 0x8049c80, 1000)
 * Dumping memory allocation table:
 * PTR:  1 pointers stored.
 * PTR:   Pointer |       Filename       |  Line  |  Address |  Size  | Offset  | 00 01 02 03 04 05 06 07 |  ASCII  
 * PTR:  ---------+----------------------+--------+----------+--------+---------+-------------------------+---------
 * PTR:   0000000 |                      |      0 | 0x8049c58 | 000036 | 0000000 | 80 9c 04 08 e8 03 00 00 | €œ..è...
 * PTR:   0000000 |                      |      0 | 0x8049c58 | 000036 | 0000008 | 6d 65 6d 5f 65 78 61 6d | mem_exam
 * PTR:   0000000 |                      |      0 | 0x8049c58 | 000036 | 0000010 | 70 6c 65 2e 63 00 00 00 | ple.c...
 * PTR:   0000000 |                      |      0 | 0x8049c58 | 000036 | 0000018 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000000 |                      |      0 | 0x8049c58 | 000036 | 0000020 | 2e 00 00 00             | ....    
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000000 | 54 68 69 73 20 69 73 20 | This is 
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000008 | 61 20 74 65 73 74 2e 00 | a test..
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000010 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000018 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000020 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000028 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000030 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000038 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000040 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000048 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000050 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000058 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000060 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000068 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000070 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000078 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000080 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000088 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000090 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000098 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000a0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000a8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000b0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000b8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000c0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000c8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000d0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000d8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000e0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000e8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000f0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00000f8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000100 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000108 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000110 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000118 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000120 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000128 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000130 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000138 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000140 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000148 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000150 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000158 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000160 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000168 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000170 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000178 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000180 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000188 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000190 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000198 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001a0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001a8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001b0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001b8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001c0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001c8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001d0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001d8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001e0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001e8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001f0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00001f8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000200 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000208 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000210 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000218 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000220 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000228 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000230 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000238 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000240 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000248 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000250 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000258 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000260 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000268 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000270 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000278 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000280 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000288 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000290 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000298 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002a0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002a8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002b0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002b8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002c0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002c8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002d0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002d8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002e0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002e8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002f0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00002f8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000300 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000308 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000310 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000318 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000320 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000328 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000330 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000338 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000340 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000348 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000350 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000358 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000360 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000368 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000370 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000378 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000380 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000388 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000390 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 0000398 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003a0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003a8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003b0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003b8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003c0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003c8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003d0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003d8 | 00 00 00 00 00 00 00 00 | ........
 * PTR:   0000001 |        mem_example.c |     46 | 0x8049c80 | 001000 | 00003e0 | 00 00 00 00 00 00 00 00 | ........
 * PTR:  Total allocated memory:       1000 bytes
 * @endcode
 *
 * Here is the source code:
 */
