/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
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
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include "gconf-listeners.h"
#include "gconf.h"

#include <string.h>
#include <unistd.h>

/* #define DEBUG_LISTENERS 1 */
#ifdef DEBUG_LISTENERS
#include <stdio.h>
#endif

/* FIXME get rid of this stupid thing */
struct _GConfListeners {
  gpointer dummy;
};

/* 24 bits are the array index; 8 bits are just to reduce the
   chance of duplicate connection IDs. If CnxnID was just an
   array index, recycling the IDs would make it likely that
   a broken client would remove another client's connections.
   The unique-izing 8 bits make this much less likely.
   Yeah just using a hash table instead of an array would work too since
   we wouldn't need to fool with recycling indices to avoid sparseness */
#define CNXN_ID_INDEX(cid) (cid & 0xFFFFFF)

typedef struct _Listener Listener;

struct _Listener {
  guint cnxn;
  guint refcount : 24;
  guint removed : 1; /* has been removed */
  gpointer listener_data;
  GFreeFunc destroy_notify;
};

/* LTable is GConfListenersPrivate, but shorter */

typedef struct _LTable LTable;

struct _LTable {
  GNode* tree; /* Represents the configuration "filesystem" namespace. 
                *  Kept sorted. 
                */
  GPtrArray* listeners; /* Listeners are also kept in a flat array here, indexed by connection number */
  guint active_listeners; /* count of "alive" listeners */

  /* 0 is an error value */
  guint next_cnxn;
  
  /* Connection array indexes to be recycled */
  GSList* removed_indices;
};

typedef struct _LTableEntry LTableEntry;

struct _LTableEntry {
  gchar* name; /* The name of this "directory" */
  GList* listeners; /* Each listener listening *exactly* here. You probably 
                        want to notify all listeners *below* this node as well. 
                     */
  gchar *full_name; /* fully-qualified name */
};

static LTable* ltable_new(void);
static void    ltable_insert(LTable* ltable,
                             const gchar* where,
                             Listener* listener);
static void    ltable_remove(LTable* ltable,
                             guint cnxn);
static void    ltable_destroy(LTable* ltable);
static void    ltable_notify(LTable* ltable,
                             const gchar* key,
                             GConfListenersCallback callback,
                             gpointer user_data);

static guint   ltable_next_cnxn(LTable* ltable);
static void    ltable_foreach  (LTable* ltable,
                                GConfListenersForeach callback,
                                gpointer user_data);
static gboolean ltable_get_data (LTable *ltable,
                                 guint cnxn_id,
                                 gpointer *listener_data_p,
                                 const gchar  **location_p);
static void    ltable_remove_if  (LTable* ltable,
                                  GConfListenersPredicate predicate,
                                  gpointer user_data);

#ifdef DEBUG_LISTENERS
static void    ltable_spew(LTable* ltable);
#endif

static LTableEntry* ltable_entry_new(gchar **pathv,
				     int     end);
static void         ltable_entry_destroy(LTableEntry* entry);

static Listener* listener_new   (guint      cnxn_id,
                                 gpointer   listener_data,
                                 GFreeFunc  destroy_notify);
static void      listener_unref (Listener  *l);
static void      listener_ref   (Listener  *l);

/*
 * Public API
 */ 

GConfListeners*
gconf_listeners_new     (void)
{
  LTable* lt;

  lt = ltable_new();

  return (GConfListeners*)lt;
}

void
gconf_listeners_free (GConfListeners* listeners)
{
  LTable* lt = (LTable*)listeners;

  ltable_destroy(lt);
}

guint
gconf_listeners_add     (GConfListeners* listeners,
                         const gchar* listen_point,
                         gpointer listener_data,
                         GFreeFunc destroy_notify)
{
  LTable* lt = (LTable*)listeners;
  Listener* l;

  l = listener_new(ltable_next_cnxn(lt),
                   listener_data,
                   destroy_notify);
  
  ltable_insert(lt, listen_point, l);

  return l->cnxn;
}

void
gconf_listeners_remove  (GConfListeners* listeners,
                         guint cnxn_id)
{
  LTable* lt = (LTable*)listeners;

  /* Silently fail, since this can easily happen (e.g. adding the
   * notify fails)
   */
  if (cnxn_id == 0)
    return;
  
  ltable_remove(lt, cnxn_id);
}

void
gconf_listeners_notify  (GConfListeners* listeners,
                         const gchar* all_above,
                         GConfListenersCallback callback,
                         gpointer user_data)
{
  LTable* lt = (LTable*)listeners;

  ltable_notify(lt, all_above, callback, user_data);
}

guint
gconf_listeners_count   (GConfListeners* listeners)
{
  LTable* lt = (LTable*)listeners;

  return lt->active_listeners;
}

void
gconf_listeners_foreach (GConfListeners* listeners,
                         GConfListenersForeach callback,
                         gpointer user_data)
{
  LTable* lt = (LTable*)listeners;
  
  ltable_foreach (lt, callback, user_data);
}

gboolean
gconf_listeners_get_data (GConfListeners* listeners,
                          guint cnxn_id,
                          gpointer *listener_data_p,
                          const gchar **location_p)
{
  LTable* lt = (LTable*)listeners;

  return ltable_get_data (lt, cnxn_id, listener_data_p, location_p);
}

void
gconf_listeners_remove_if (GConfListeners         *listeners,
                           GConfListenersPredicate predicate,
                           gpointer                user_data)
{
  LTable* lt = (LTable*)listeners;

  ltable_remove_if (lt, predicate, user_data);
}

/*
 * LTable impl
 */

static Listener* 
listener_new(guint cnxn_id, gpointer listener_data, GFreeFunc destroy_notify)
{
  Listener* l;

  l = g_new0(Listener, 1);

  l->refcount = 1;
  l->removed = FALSE;
  l->listener_data = listener_data;
  l->cnxn = cnxn_id;  
  l->destroy_notify = destroy_notify;

  return l;
}

static void      
listener_unref (Listener* l)
{
  l->refcount -= 1;
  if (l->refcount == 0)
    {
      (*l->destroy_notify)(l->listener_data);
      g_free(l);
    }
}

static void
listener_ref (Listener  *l)
{
  l->refcount += 1;
}

static LTable* 
ltable_new(void)
{
  LTable* lt;

  lt = g_new0(LTable, 1);

  lt->listeners = g_ptr_array_new();

  /* Set initial size; note that GPtrArray's are initialized
     to 0-filled */
  g_ptr_array_set_size(lt->listeners, 5);
  
  lt->active_listeners = 0;

  lt->removed_indices = NULL;

  lt->next_cnxn = 1; /* 0 is invalid */
  
  return lt;
}

static guint
ltable_next_cnxn(LTable* lt)
{
  static guchar start = 0;
  static guchar uniqueness = 0;
  guint uniqueness_shifted;

  if (start == 0)
    {
      /* Don't start uniqueness at the same place every time */
      start = getpid () % 256;
      if (start == 0)
        start = 1;

      uniqueness = start;
    }
  
  /* this overflows and starts over occasionally */
  ++uniqueness;

  uniqueness_shifted = uniqueness;
  uniqueness_shifted = uniqueness_shifted << 24;
  
  if (lt->removed_indices != NULL)
    {
      guint retval = GPOINTER_TO_UINT(lt->removed_indices->data);

      lt->removed_indices = g_slist_remove(lt->removed_indices, lt->removed_indices->data);

      /* set the uniqueness bits */
      retval |= uniqueness_shifted;
      
      return retval;
    }
  else
    {
      /* make sure we fit in 24 bits */
      g_assert(lt->next_cnxn <= 0xFFFFFF);

      lt->next_cnxn += 1;
      return (lt->next_cnxn - 1) | uniqueness_shifted;
    }
}

static void
ltable_insert(LTable* lt, const gchar* where, Listener* l)
{
  gchar** dirnames;
  guint i;
  GNode* cur;
  GNode* found = NULL;
  LTableEntry* lte;
  const gchar* noroot_where = where + 1;

  g_return_if_fail(gconf_valid_key(where, NULL));
  
  if (lt->tree == NULL)
    {
      lte = ltable_entry_new(NULL, 0);
      
      lt->tree = g_node_new(lte);

      lte = NULL; /* paranoia */
    }
  
  /* Add to the tree */
  dirnames = g_strsplit(noroot_where, "/", -1);
  
  cur = lt->tree;
  i = 0;
  while (dirnames[i])
    {
      LTableEntry* ne;
      GNode* across;

      /* Find this dirname on this level, or add it. */
      g_assert (cur != NULL);        

      found = NULL;

      across = cur->children;

      while (across != NULL)
        {
          int cmp;

          lte = across->data;

          cmp = strcmp(lte->name, dirnames[i]);

          if (cmp == 0)
            {
              found = across;
              break;
            }
          else if (cmp > 0)
            {
              /* Past it */
              break;
            }
          else 
            {
              across = g_node_next_sibling(across);
            }
        }

      if (found == NULL)
        {
          ne = ltable_entry_new(dirnames, i);
              
          if (across != NULL) /* Across is at the one past */
            found = g_node_insert_data_before(cur, across, ne);
          else                /* Never went past, append - could speed this up by saving last visited */
            found = g_node_append_data(cur, ne);
        }

      g_assert(found != NULL);

      cur = found;

      ++i;
    }

  /* cur is still the root node ("/") if where was "/" since nothing
     was returned from g_strsplit */
  lte = cur->data;

  lte->listeners = g_list_prepend(lte->listeners, l);

  g_strfreev(dirnames);

  /* Add tree node to the flat table */
  g_ptr_array_set_size(lt->listeners, MAX(CNXN_ID_INDEX(lt->next_cnxn), CNXN_ID_INDEX(l->cnxn)));
  g_ptr_array_index(lt->listeners, CNXN_ID_INDEX(l->cnxn)) = cur;

  lt->active_listeners += 1;

#ifdef DEBUG_LISTENERS
  g_print ("Added %u at %s, spewing:\n",
	   l->cnxn, where);
  ltable_spew(lt);
#endif
}

static void    
ltable_remove(LTable* lt, guint cnxn)
{
  LTableEntry* lte;
  GList* tmp;
  GNode* node;
  guint index = CNXN_ID_INDEX(cnxn);

  g_return_if_fail(index < lt->listeners->len);
  
  if (index >= lt->listeners->len) /* robust even with checks off */
    return;
  
  /* Lookup in the flat table */
  node = g_ptr_array_index(lt->listeners, index);

  g_return_if_fail(node != NULL);
  if (node == NULL) /* a client is broken probably */
    return;

  g_assert(lt->tree != NULL);
  
  lte = node->data;
  
  tmp = lte->listeners;

  g_return_if_fail(tmp != NULL);

  while (tmp != NULL)
    {
      Listener* l = tmp->data;

      if (l->cnxn == cnxn)
        {
          if (tmp->prev)
            {
              tmp->prev->next = tmp->next;
            }
          else
            {
              /* tmp was the first (and maybe last) node */
              lte->listeners = tmp->next;
            }
          if (tmp->next)
            {
              tmp->next->prev = tmp->prev;
            }
          g_list_free_1(tmp);

          lt->removed_indices = g_slist_prepend(lt->removed_indices,
                                                GUINT_TO_POINTER(index));

          l->removed = TRUE;
          listener_unref (l);

          break;
        }

      tmp = g_list_next(tmp);
    }

  /* note that tmp is invalid, but should be nonzero if
     the connection was found. If the connection wasn't found,
     then this is a duplicate index and we have a broken client;
     we were saved by the uniqueness bits */

  if (tmp == NULL)
    return;

  /* Since we did have a valid connection, set the
     flat table entry to NULL */
  g_ptr_array_index(lt->listeners, index) = NULL;

  /* Remove from the tree if this node is now pointless */
  {
    GNode* cur = node;
    while (cur != NULL)
      {
        GNode* parent = cur->parent;
        lte = cur->data;
        if (lte->listeners == NULL && cur->children == NULL)
          {
            if (cur == lt->tree)
              lt->tree = NULL;
              
            ltable_entry_destroy(lte);
            g_node_destroy(cur);
          }
        else
          break; /* don't delete more parents since this node exists */
          
        cur = parent;
      }
  }

  lt->active_listeners -= 1;

#ifdef DEBUG_LISTENERS
  g_print ("Removed %u, spewing:\n", cnxn);
  ltable_spew(lt);
#endif
}

static gboolean
destroy_func(GNode* node, gpointer data)
{
  LTableEntry* lte = node->data;  
  GList* tmp;

  tmp = lte->listeners;
  while (tmp != NULL)
    {
      Listener* l = tmp->data;

      l->removed = TRUE;
      listener_unref (l);

      tmp = g_list_next(tmp);
    }
  g_list_free (lte->listeners);
  lte->listeners = NULL;
  
  ltable_entry_destroy (lte);
  
  return FALSE;
}

static void    
ltable_destroy(LTable* ltable)
{
  if (ltable->tree != NULL)
    {
      g_node_traverse(ltable->tree, G_POST_ORDER, G_TRAVERSE_ALL, -1,
                      destroy_func, NULL);  
      g_node_destroy(ltable->tree);
    }
      
  g_ptr_array_free(ltable->listeners, TRUE);

  g_slist_free(ltable->removed_indices);
  
  g_free(ltable);
}

static void
notify_listener_list(GConfListeners* listeners,
                     GList* list,
                     const gchar* key,
                     GConfListenersCallback callback,
                     gpointer user_data)
{
  GList* tmp;

  tmp = list;
  while (tmp != NULL)
    {
      Listener* l = tmp->data;

      /* don't notify listeners that were removed during the notify */
      if (!l->removed)
        (*callback)(listeners, key, l->cnxn, l->listener_data, user_data);

      tmp = g_list_next(tmp);
    }
}

static void    
ltable_notify(LTable* lt, const gchar* key,
              GConfListenersCallback callback, gpointer user_data)
{
  gchar** dirs;
  guint i;
  const gchar* noroot_key;
  GNode* cur;
  GList *to_notify;
  
  noroot_key = key + 1;

  g_return_if_fail(*key == '/');
  g_return_if_fail(gconf_valid_key(key, NULL));

  if (lt->tree == NULL)
    return; /* no one to notify */

  /* we build a list "to_notify" to be safe against tree
   * modifications during the notification.
   */
  
  /* Notify "/" listeners */
  to_notify = g_list_copy (((LTableEntry*)lt->tree->data)->listeners);  

  dirs = g_strsplit (noroot_key, "/", -1);

  cur = lt->tree;
  i = 0;
  while (dirs[i] && cur)
    {
      GNode* child = cur->children;

      while (child != NULL)
        {
          LTableEntry* lte = child->data;

          if (strcmp(lte->name, dirs[i]) == 0)
            {
              GList *copy = g_list_copy (lte->listeners);
              to_notify = g_list_concat (to_notify, copy);

              break;
            }

          child = g_node_next_sibling(child);
        }

      if (child != NULL) /* we found a child, scan below it */
        cur = child;
      else               /* end of the line */
        cur = NULL;

      ++i;
    }
  
  g_strfreev(dirs);

  g_list_foreach (to_notify, (GFunc) listener_ref, NULL);
  
  notify_listener_list ((GConfListeners*)lt,
                        to_notify, key,
                        callback, user_data);

  g_list_foreach (to_notify, (GFunc) listener_unref, NULL);

  g_list_free (to_notify);
}

struct NodeTraverseData
{
  GConfListenersForeach func;
  gpointer user_data;
};

static gboolean
node_traverse_func (GNode *node,
                    gpointer user_data)
{
  struct NodeTraverseData *td = user_data;
  LTableEntry *lte = node->data;
  GList *tmp;

  tmp = lte->listeners;
  while (tmp != NULL)
    {
      Listener *l = tmp->data;
      
      (* td->func) (lte->full_name,
                    l->cnxn,
                    l->listener_data,
                    td->user_data);
      
      tmp = g_list_next (tmp);
    }

  return FALSE; /* continue traversal */
}

static void
ltable_foreach  (LTable* ltable,
                 GConfListenersForeach callback,
                 gpointer user_data)
{
  struct NodeTraverseData td;
  td.func = callback;
  td.user_data = user_data;

  if (ltable->tree == NULL)
    return;
  
  g_node_traverse (ltable->tree,
                   G_IN_ORDER,
                   G_TRAVERSE_ALL,
                   -1,
                   node_traverse_func,
                   &td);
}

static gboolean
ltable_get_data (LTable *lt,
                 guint cnxn_id,
                 gpointer *listener_data_p,
                 const gchar   **location_p)
{
  LTableEntry* lte;
  GList* tmp;
  GNode* node;
  guint index = CNXN_ID_INDEX(cnxn_id);
  
  g_return_val_if_fail(index < lt->listeners->len, FALSE);
  if (index >= lt->listeners->len) /* robust even with checks off */
    return FALSE;
  
  /* Lookup in the flat table */
  node = g_ptr_array_index(lt->listeners, index);
  
  g_return_val_if_fail(node != NULL, FALSE);
  if (node == NULL) /* a client is broken probably */
    return FALSE;

  g_assert(lt->tree != NULL);
  
  lte = node->data;
  
  tmp = lte->listeners;

  g_return_val_if_fail(tmp != NULL, FALSE);

  while (tmp != NULL)
    {
      Listener* l = tmp->data;

      if (l->cnxn == cnxn_id)
        {
          if (listener_data_p)
            *listener_data_p = l->listener_data;
          if (location_p)
            *location_p = lte->full_name;
          return TRUE;
        }

      tmp = g_list_next(tmp);
    }

  return FALSE;
}



struct NodeRemoveData
{
  GConfListenersPredicate predicate;
  gpointer user_data;
  GSList *dead;
};

static gboolean
node_remove_func (GNode   *node,
                  gpointer user_data)
{
  struct NodeRemoveData *rd = user_data;
  LTableEntry *lte = node->data;
  GList *tmp;

  tmp = lte->listeners;
  while (tmp != NULL)
    {
      Listener *l = tmp->data;
      
      if ((* rd->predicate) (lte->full_name,
                             l->cnxn,
                             l->listener_data,
                             rd->user_data))
        rd->dead = g_slist_prepend (rd->dead, GINT_TO_POINTER (l->cnxn));
      
      tmp = g_list_next (tmp);
    }

  return FALSE; /* continue traversal */
}

static void
ltable_remove_if  (LTable                 *ltable,
                   GConfListenersPredicate predicate,
                   gpointer                user_data)
{
  GSList *tmp;
  struct NodeRemoveData rd;
  rd.predicate = predicate;
  rd.user_data = user_data;
  rd.dead = NULL;
  
  if (ltable->tree == NULL)
    return;
  
  g_node_traverse (ltable->tree,
                   G_IN_ORDER,
                   G_TRAVERSE_ALL,
                   -1,
                   node_remove_func,
                   &rd);

  tmp = rd.dead;
  while (tmp != NULL)
    {
      ltable_remove (ltable, GPOINTER_TO_INT (tmp->data));

      tmp = g_slist_next (tmp);
    }

  g_slist_free (rd.dead);
}

static LTableEntry* 
ltable_entry_new(gchar **pathv,
		 int     end)
{
  LTableEntry* lte;

  lte = g_new0(LTableEntry, 1);

  if (pathv != NULL)
    {
      GString* full_name;
      guint i;

      lte->name = g_strdup(pathv[end]);

      full_name = g_string_new("/");
      i = 0;
      while (i <= end)
	{
	  g_string_append(full_name, pathv[i]);
	  if (i != end)
	    g_string_append_c(full_name, '/');
	  i++;
	}
      lte->full_name = g_string_free(full_name, FALSE);
    }
  else
    {
      lte->name = g_strdup("/");
      lte->full_name = g_strdup("/");
    }
  
  return lte;
}

static void         
ltable_entry_destroy(LTableEntry* lte)
{
  g_return_if_fail(lte->listeners == NULL); /* should destroy all listeners first. */
  g_free(lte->name);
  g_free(lte->full_name);
  g_free(lte);
}

#ifdef DEBUG_LISTENERS
/* Debug */
gboolean
spew_func(GNode* node, gpointer data)
{
  LTableEntry* lte = node->data;  
  GList* tmp;
  gchar spaces[256];
  memset(spaces, ' ', 256);
  spaces[g_node_depth(node)] = '\0';

  
  g_print (" %sSpewing node `%s' (%p): ", spaces, lte->name, node);

  tmp = lte->listeners;
  while (tmp != NULL)
    {
      Listener* l = tmp->data;
      
      g_print ("  %slistener %u is here\n", spaces, (guint)l->cnxn);

      tmp = g_list_next(tmp);
    }

  if (lte->listeners == NULL)
    g_print ("\n");
  
  return FALSE;
}

static void    
ltable_spew(LTable* lt)
{
  guint i;
  g_print ("Flat table:\n");
  i = 0;
  while (i < lt->listeners->len)
    {
      GNode* node = g_ptr_array_index(lt->listeners, i);
      LTableEntry* lte = node ? node->data : NULL;
      g_print ("%u `%s' %p\n", i, lte ? lte->name : "", node);
      
      ++i;
    }
  
  g_node_traverse(lt->tree, G_PRE_ORDER, G_TRAVERSE_ALL, -1, spew_func, NULL);
}
#endif
