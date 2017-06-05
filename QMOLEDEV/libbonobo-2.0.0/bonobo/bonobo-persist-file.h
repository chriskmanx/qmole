/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Bonobo PersistFile
 *
 * Author:
 *   Matt Loper (matt@gnome-support.com)
 *
 * Copyright 1999, 2000 Helix Code, Inc.
 */

#ifndef _BONOBO_PERSIST_FILE_H_
#define _BONOBO_PERSIST_FILE_H_

#include <bonobo/bonobo-persist.h>

#ifndef BONOBO_DISABLE_DEPRECATED

G_BEGIN_DECLS

#define BONOBO_TYPE_PERSIST_FILE (bonobo_persist_file_get_type ())
#define BONOBO_PERSIST_FILE_TYPE        BONOBO_TYPE_PERSIST_FILE /* deprecated, you should use BONOBO_TYPE_PERSIST_FILE */
#define BONOBO_PERSIST_FILE(o)   (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_PERSIST_FILE, BonoboPersistFile))
#define BONOBO_PERSIST_FILE_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_PERSIST_FILE, BonoboPersistFileClass))
#define BONOBO_IS_PERSIST_FILE(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_PERSIST_FILE))
#define BONOBO_IS_PERSIST_FILE_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_PERSIST_FILE))

typedef struct _BonoboPersistFilePrivate BonoboPersistFilePrivate;
typedef struct _BonoboPersistFile        BonoboPersistFile;

typedef int (*BonoboPersistFileIOFn) (BonoboPersistFile *pf,
				      const CORBA_char  *uri,
				      CORBA_Environment *ev,
				      void              *closure);

struct _BonoboPersistFile {
	BonoboPersist persist;

	char *uri;

	/*
	 * For the sample routines, NULL if we use the ::save and ::load
	 * methods from the class
	 */
	BonoboPersistFileIOFn  save_fn;
	BonoboPersistFileIOFn  load_fn;
	void *closure;

	BonoboPersistFilePrivate *priv;
};

typedef struct {
	BonoboPersistClass parent_class;

	POA_Bonobo_PersistFile__epv epv;

	/* methods */
	int   (*load)             (BonoboPersistFile *ps,
				   const CORBA_char  *uri,
				   CORBA_Environment *ev);

	int   (*save)             (BonoboPersistFile *ps,
				   const CORBA_char  *uri,
				   CORBA_Environment *ev);

	char *(*get_current_file) (BonoboPersistFile *ps,
				   CORBA_Environment *ev);

} BonoboPersistFileClass;

GType              bonobo_persist_file_get_type  (void) G_GNUC_CONST;

BonoboPersistFile *bonobo_persist_file_new       (BonoboPersistFileIOFn load_fn,
						  BonoboPersistFileIOFn save_fn,
						  const gchar          *iid,
						  void                 *closure);

BonoboPersistFile *bonobo_persist_file_construct (BonoboPersistFile    *ps,
						  BonoboPersistFileIOFn load_fn,
						  BonoboPersistFileIOFn save_fn,
						  const gchar          *iid,
						  void                 *closure);

G_END_DECLS

#endif /* BONOBO_DISABLE_DEPRECATED */

#endif /* _BONOBO_PERSIST_FILE_H_ */
