(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010
//
(* ****** ****** *)

//
// HX-2010-02-28:
// In any acconts, [ghash] is a terrible package for ATS to incorporate.
// It is simply a _big_ mess, and I tried my best to make some sense out
// of this mess!
//
// HX-2010-03-01:
// I suggest that you don't use this package unless you _really_ know what
// you are doing, especially, if you want to turn on GCATS!
//

(* ****** ****** *)

%{#
#include "contrib/glib/CATS/glib/ghash.cats"
%} // end of [%{#]

(* ****** ****** *)

//
// HX-2010-02-27: only need for individual testing
// staload "contrib/glib/SATS/gtypes.sats"
//

(* ****** ****** *)
//
// HX-2010-02-24: [GHashTable_ref] is refcounted
//
absviewtype GHashTable_ref
  (key:type, itm: type, l:addr) = $extype"GHashTable_ref"
viewtypedef GHashTable_ref
  (key:type, itm: type) = [l:addr] GHashTable_ref (key, itm, l)
// end of [GHashTable_ref]

(* ****** ****** *)

fun g_hash_table_new
  {key,itm:type} (
  hash_func: GHashFunc key, key_equal_func: GEqualFunc key
) : GHashTable_ref (key,itm) = "mac#atslib_g_hash_table_new"
// end of [g_hash_table_new]

fun g_hash_table_destroy
  {key,itm:type} (
  hash_table: GHashTable_ref (key,itm)
) : void = "mac#atslib_g_hash_table_destroy"
// end of [g_hash_table_destroy]

(* ****** ****** *)

fun g_hash_table_insert
  {key,itm:type} {l:addr} (
  hash_table: !GHashTable_ref (key, itm, l), key: key, itm: itm
) : void
  = "mac#atsctrb_g_hash_table_insert"
// end of [g_hash_table_insert]

(* ****** ****** *)

fun g_hash_table_size
  {key,itm:type} {l:addr} (
  hash_table: !GHashTable_ref (key, itm, l)
) : guint
  = "mac#atsctrb_g_hash_table_size"
// end of [g_hash_table_size]

(* ****** ****** *)

fun g_hash_table_lookup
  {key,itm:type} {l:addr} (
  hash_table: !GHashTable_ref (key, itm, l), key: key
) : [lp:addr] (
  {lp <> null} (!ptr lp >> itm) -<prf> void | ptr lp
) = "mac#atslib_g_hash_table_lookup"
// end of [g_hash_table_lookup]

(* ****** ****** *)

fun g_str_hash (str: string): guint = "mac#atsctrb_g_str_hash"

(* ****** ****** *)

(* end of [ghash.sats] *)

////

/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef __G_HASH_H__
#define __G_HASH_H__

#include <glib/gtypes.h>

G_BEGIN_DECLS

typedef struct _GHashTable  GHashTable;

typedef gboolean  (*GHRFunc)  (gpointer  key,
                               gpointer  value,
                               gpointer  user_data);

/* Hash tables
 */
GHashTable* g_hash_table_new		   (GHashFunc	    hash_func,
					    GEqualFunc	    key_equal_func);
GHashTable* g_hash_table_new_full      	   (GHashFunc	    hash_func,
					    GEqualFunc	    key_equal_func,
					    GDestroyNotify  key_destroy_func,
					    GDestroyNotify  value_destroy_func);
void	    g_hash_table_destroy	   (GHashTable	   *hash_table);
void	    g_hash_table_insert		   (GHashTable	   *hash_table,
					    gpointer	    key,
					    gpointer	    value);
void        g_hash_table_replace           (GHashTable     *hash_table,
					    gpointer	    key,
					    gpointer	    value);
gboolean    g_hash_table_remove		   (GHashTable	   *hash_table,
					    gconstpointer   key);
void        g_hash_table_remove_all        (GHashTable     *hash_table);
gboolean    g_hash_table_steal             (GHashTable     *hash_table,
					    gconstpointer   key);
void        g_hash_table_steal_all         (GHashTable     *hash_table);
gpointer    g_hash_table_lookup		   (GHashTable	   *hash_table,
					    gconstpointer   key);
gboolean    g_hash_table_lookup_extended   (GHashTable	   *hash_table,
					    gconstpointer   lookup_key,
					    gpointer	   *orig_key,
					    gpointer	   *value);
void	    g_hash_table_foreach	   (GHashTable	   *hash_table,
					    GHFunc	    func,
					    gpointer	    user_data);
gpointer    g_hash_table_find	           (GHashTable	   *hash_table,
					    GHRFunc	    predicate,
					    gpointer	    user_data);
guint	    g_hash_table_foreach_remove	   (GHashTable	   *hash_table,
					    GHRFunc	    func,
					    gpointer	    user_data);
guint	    g_hash_table_foreach_steal	   (GHashTable	   *hash_table,
					    GHRFunc	    func,
					    gpointer	    user_data);
guint	    g_hash_table_size		   (GHashTable	   *hash_table);

/* keeping hash tables alive */
GHashTable* g_hash_table_ref   		   (GHashTable 	   *hash_table);
void        g_hash_table_unref             (GHashTable     *hash_table);

#ifndef G_DISABLE_DEPRECATED

/* The following two functions are deprecated and will be removed in
 * the next major release. They do no good. */
#define g_hash_table_freeze(hash_table) ((void)0)
#define g_hash_table_thaw(hash_table) ((void)0)

#endif /* G_DISABLE_DEPRECATED */

/* Hash Functions
 */
gboolean g_str_equal (gconstpointer  v1,
                      gconstpointer  v2);
guint    g_str_hash  (gconstpointer  v);

gboolean g_int_equal (gconstpointer  v1,
                      gconstpointer  v2);
guint    g_int_hash  (gconstpointer  v);

/* This "hash" function will just return the key's address as an
 * unsigned integer. Useful for hashing on plain addresses or
 * simple integer values.
 * Passing NULL into g_hash_table_new() as GHashFunc has the
 * same effect as passing g_direct_hash().
 */
guint    g_direct_hash  (gconstpointer  v) G_GNUC_CONST;
gboolean g_direct_equal (gconstpointer  v1,
                         gconstpointer  v2) G_GNUC_CONST;

G_END_DECLS

#endif /* __G_HASH_H__ */

