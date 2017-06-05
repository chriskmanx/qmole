/* List abstraction used internally */
/* 
 * Copyright (C) 2002 Red Hat, Inc.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "sn-list.h"
#include "sn-internals.h"

typedef struct SnListNode
{
  void *data;
  struct SnListNode *next;
} SnListNode;

struct SnList
{
  SnListNode *head;
};

static SnListNode*
sn_list_node_alloc (void)
{
  return sn_new0 (SnListNode, 1);
}

SnList*
sn_list_new (void)
{
  SnList *list;

  list = sn_new (SnList, 1);
  list->head = NULL;

  return list;
}

void
sn_list_free (SnList *list)
{
  SnListNode *node;

  node = list->head;
  while (node != NULL)
    {
      SnListNode *next = node->next;

      sn_free (node);

      node = next;
    }

  sn_free (list);
}

void
sn_list_prepend (SnList *list,
                 void   *data)
{
  if (list->head == NULL)
    {
      list->head = sn_list_node_alloc ();
      list->head->data = data;
    }
  else
    {
      SnListNode *node;

      node = sn_list_node_alloc ();
      node->data = data;
      node->next = list->head;
      list->head = node;
    }
}

void
sn_list_append (SnList *list,
                void   *data)
{
  if (list->head == NULL)
    {
      list->head = sn_list_node_alloc ();
      list->head->data = data;
    }
  else
    {
      SnListNode *node;
      
      node = list->head;
      while (node->next != NULL)
        node = node->next;
      
      node->next = sn_list_node_alloc ();
      node->next->data = data;
    }
}

void
sn_list_remove (SnList *list,
                void   *data)
{
  SnListNode *node;
  SnListNode *prev;

  prev = NULL;
  node = list->head;
  while (node != NULL)
    {
      if (node->data == data)
        {
          if (prev)
            prev->next = node->next;
          else
            list->head = node->next;

          sn_free (node);

          return;
        }

      prev = node;
      node = node->next;
    }
}

void
sn_list_foreach (SnList            *list,
                 SnListForeachFunc  func,
                 void              *data)
{
  SnListNode *node;

  node = list->head;
  while (node != NULL)
    {
      SnListNode *next = node->next; /* reentrancy safety */
      
      if (!(* func) (node->data, data))
        return;
      
      node = next;
    }
}

sn_bool_t
sn_list_empty (SnList *list)
{
  return list->head == NULL;
}
