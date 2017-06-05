/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-stream-memory.h: Memory based stream
 *
 * Author:
 *   Miguel de Icaza (miguel@gnu.org)
 *
 * Copyright 1999, 2000 Helix Code, Inc.
 */
#ifndef _BONOBO_STREAM_MEM_H_
#define _BONOBO_STREAM_MEM_H_

#include <bonobo/bonobo-storage.h>

G_BEGIN_DECLS

struct _BonoboStreamMem;
typedef struct _BonoboStreamMem BonoboStreamMem;
typedef struct _BonoboStreamMemPrivate BonoboStreamMemPrivate;

#define BONOBO_TYPE_STREAM_MEM        (bonobo_stream_mem_get_type ())
#define BONOBO_STREAM_MEM_TYPE        BONOBO_TYPE_STREAM_MEM /* deprecated, you should use BONOBO_TYPE_STREAM_MEM */
#define BONOBO_STREAM_MEM(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_STREAM_MEM, BonoboStreamMem))
#define BONOBO_STREAM_MEM_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_STREAM_MEM, BonoboStreamMemClass))
#define BONOBO_IS_STREAM_MEM(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_STREAM_MEM))
#define BONOBO_IS_STREAM_MEM_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_STREAM_MEM))

struct _BonoboStreamMem {
	BonoboObject parent;

	char         *buffer;
	size_t        size;
	long          pos;
	gboolean      read_only;
	gboolean      resizable;
	char         *content_type;
	char         *name;

	BonoboStreamMemPrivate *priv;
};

typedef struct {
	BonoboObjectClass parent_class;

	POA_Bonobo_Stream__epv epv;

	char           *(*get_buffer) (BonoboStreamMem *stream_mem);
	size_t          (*get_size)   (BonoboStreamMem *stream_mem);
} BonoboStreamMemClass;

GType            bonobo_stream_mem_get_type   (void) G_GNUC_CONST;
BonoboStreamMem *bonobo_stream_mem_construct  (BonoboStreamMem  *stream_mem,
					       const char       *buffer,
					       size_t            size,
					       gboolean          read_only,
					       gboolean          resizable);

BonoboObject    *bonobo_stream_mem_create     (const char       *buffer,
					       size_t            size,
					       gboolean          read_only,
					       gboolean          resizable);

const char      *bonobo_stream_mem_get_buffer (BonoboStreamMem  *stream_mem);
size_t           bonobo_stream_mem_get_size   (BonoboStreamMem  *stream_mem);

G_END_DECLS

#endif /* _BONOBO_STREAM_MEM_H_ */
