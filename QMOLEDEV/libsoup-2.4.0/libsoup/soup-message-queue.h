/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2003, Ximian, Inc.
 */

#ifndef SOUP_MESSAGE_QUEUE_H
#define SOUP_MESSAGE_QUEUE_H 1

#include <glib.h>
#include <libsoup/soup-message.h>

G_BEGIN_DECLS

typedef struct SoupMessageQueue SoupMessageQueue; 

typedef struct {
	GList *cur, *next;
} SoupMessageQueueIter;

SoupMessageQueue *soup_message_queue_new        (void);
void              soup_message_queue_append     (SoupMessageQueue     *queue,
						 SoupMessage          *msg);

SoupMessage      *soup_message_queue_first      (SoupMessageQueue     *queue,
						 SoupMessageQueueIter *iter);
SoupMessage      *soup_message_queue_next       (SoupMessageQueue     *queue,
						 SoupMessageQueueIter *iter);
SoupMessage      *soup_message_queue_remove     (SoupMessageQueue     *queue,
						 SoupMessageQueueIter *iter);

void              soup_message_queue_free_iter  (SoupMessageQueue     *queue,
						 SoupMessageQueueIter *iter);

void              soup_message_queue_destroy    (SoupMessageQueue     *queue);

void              soup_message_queue_remove_message (SoupMessageQueue *queue,
						     SoupMessage      *msg);

G_END_DECLS

#endif /* SOUP_MESSAGE_QUEUE_H */
