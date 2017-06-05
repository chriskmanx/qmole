/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * Main include file for the Bonobo component model
 *
 * Authors:
 *   Miguel de Icaza (miguel@ximian.com)
 *   Michael Meeks   (michael@ximian.com)
 *
 * Copyright 2001 Ximian, Inc.
 */
#ifndef __LIBBONOBO_H__
#define __LIBBONOBO_H__

#include <bonobo/bonobo-macros.h>

G_BEGIN_DECLS

#include <bonobo/bonobo-types.h>

#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-moniker.h>
#include <bonobo/bonobo-moniker-simple.h>
#include <bonobo/bonobo-context.h>
#include <bonobo/bonobo-exception.h>

#include <bonobo/bonobo-item-container.h>
#include <bonobo/bonobo-moniker-util.h>

#include <bonobo/bonobo-property-bag.h>
#include <bonobo/bonobo-property-bag-client.h>

#include <bonobo/bonobo-listener.h>
#include <bonobo/bonobo-event-source.h>
#include <bonobo/bonobo-generic-factory.h>
#include <bonobo/bonobo-main.h>

#include <bonobo/bonobo-stream.h>
#include <bonobo/bonobo-stream-memory.h>
#include <bonobo/bonobo-stream-client.h>

#include <bonobo/bonobo-persist.h>
#include <bonobo/bonobo-persist-file.h>
#include <bonobo/bonobo-persist-stream.h>

#include <bonobo/bonobo-storage.h>
#include <bonobo/bonobo-storage-memory.h>

G_END_DECLS

#endif /* __LIBBONOBO_H__ */
