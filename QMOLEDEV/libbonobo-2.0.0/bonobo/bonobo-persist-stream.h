/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-persist-stream.c: PersistStream implementation.  Can be used as a
 * base class, or directly for implementing objects that use PersistStream.
 *
 * Author:
 *   Miguel de Icaza (miguel@kernel.org)
 *
 * Copyright 1999 Helix Code, Inc.
 */
#ifndef _BONOBO_PERSIST_STREAM_H_
#define _BONOBO_PERSIST_STREAM_H_

#include <bonobo/bonobo-persist.h>

#ifndef BONOBO_DISABLE_DEPRECATED

G_BEGIN_DECLS

#define BONOBO_TYPE_PERSIST_STREAM        (bonobo_persist_stream_get_type ())
#define BONOBO_PERSIST_STREAM_TYPE        BONOBO_TYPE_PERSIST_STREAM /* deprecated, you should use BONOBO_TYPE_PERSIST_STREAM */
#define BONOBO_PERSIST_STREAM(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_PERSIST_STREAM, BonoboPersistStream))
#define BONOBO_PERSIST_STREAM_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_PERSIST_STREAM, BonoboPersistStreamClass))
#define BONOBO_IS_PERSIST_STREAM(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_PERSIST_STREAM))
#define BONOBO_IS_PERSIST_STREAM_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_PERSIST_STREAM))

typedef struct _BonoboPersistStreamPrivate BonoboPersistStreamPrivate;
typedef struct _BonoboPersistStream        BonoboPersistStream;

typedef void  (*BonoboPersistStreamIOFn) (BonoboPersistStream         *ps,
					  const Bonobo_Stream         stream,
					  Bonobo_Persist_ContentType  type,
					  void                       *closure,
					  CORBA_Environment          *ev);

typedef Bonobo_Persist_ContentTypeList * (*BonoboPersistStreamTypesFn) (BonoboPersistStream *ps,
									void                *closure,
									CORBA_Environment   *ev);

struct _BonoboPersistStream {
	BonoboPersist persist;

	gboolean     is_dirty;

	/*
	 * For the sample routines, NULL if we use the
	 * methods from the class
	 */
	BonoboPersistStreamIOFn     save_fn;
	BonoboPersistStreamIOFn     load_fn;
	BonoboPersistStreamTypesFn  types_fn;
	
	void                       *closure;

	BonoboPersistStreamPrivate *priv;
};

typedef struct {
	BonoboPersistClass parent_class;

	POA_Bonobo_PersistStream__epv epv;

	/* methods */
	void       (*load)         (BonoboPersistStream        *ps,
				    Bonobo_Stream              stream,
				    Bonobo_Persist_ContentType type,
				    CORBA_Environment          *ev);
	void       (*save)         (BonoboPersistStream        *ps,
				    Bonobo_Stream              stream,
				    Bonobo_Persist_ContentType type,
				    CORBA_Environment          *ev);

	Bonobo_Persist_ContentTypeList * (*get_content_types) (BonoboPersistStream *ps,
							       CORBA_Environment   *ev);

} BonoboPersistStreamClass;

GType                bonobo_persist_stream_get_type  (void) G_GNUC_CONST;

BonoboPersistStream *bonobo_persist_stream_new       (BonoboPersistStreamIOFn    load_fn,
						      BonoboPersistStreamIOFn    save_fn,
						      BonoboPersistStreamTypesFn types_fn,
						      const gchar               *iid,
						      void                      *closure);

BonoboPersistStream *bonobo_persist_stream_construct (BonoboPersistStream       *ps,
						      BonoboPersistStreamIOFn    load_fn,
						      BonoboPersistStreamIOFn    save_fn,
						      BonoboPersistStreamTypesFn types_fn,
						      const gchar               *iid,
						      void                      *closure);

G_END_DECLS

#endif /* BONOBO_DISABLE_DEPRECATED */

#endif /* _BONOBO_PERSIST_STREAM_H_ */
