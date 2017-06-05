/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-debug.c: A runtime-controllable debugging API.
 *
 * Author:
 *   Jaka Mocnik  <jaka@gnu.org>
 */
#ifndef _BONOBO_DEBUG_H_
#define _BONOBO_DEBUG_H_

typedef enum {
	BONOBO_DEBUG_NONE = 0,
	BONOBO_DEBUG_REFS = 1 << 0,
	BONOBO_DEBUG_AGGREGATE = 1 << 1,
	BONOBO_DEBUG_LIFECYCLE = 1 << 2,
	BONOBO_DEBUG_RUNNING = 1 << 3,
	BONOBO_DEBUG_OBJECT = 1 << 4
} BonoboDebugFlags;

extern BonoboDebugFlags _bonobo_debug_flags;

void bonobo_debug_init  (void);
void bonobo_debug_print (char *name, char *fmt, ...);

#endif /* _BONOBO_DEBUG_H_ */
