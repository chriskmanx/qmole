/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-shutdown.h: internal private init & shutdown routines
 *                    used by bonobo_init & bonobo_shutdown
 *
 * Authors:
 *   Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2001 Ximian, Inc.
 */
#ifndef _BONOBO_SHUTDOWN_H_
#define _BONOBO_SHUTDOWN_H_

void bonobo_context_init     (void);
void bonobo_context_shutdown (void);

void bonobo_object_init      (void);
int  bonobo_object_shutdown  (void);

void bonobo_exception_shutdown       (void);
void bonobo_property_bag_shutdown    (void);
void bonobo_running_context_shutdown (void);

#endif /* _BONOBO_SHUTDOWN_H_ */
