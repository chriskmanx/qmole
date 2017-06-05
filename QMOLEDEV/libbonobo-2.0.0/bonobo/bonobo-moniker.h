/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-moniker: Object naming abstraction
 *
 * Author:
 *	Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000, Helix Code, Inc.
 */
#ifndef _BONOBO_MONIKER_H_
#define _BONOBO_MONIKER_H_

#include <bonobo/bonobo-object.h>

G_BEGIN_DECLS

typedef struct _BonoboMonikerPrivate BonoboMonikerPrivate;

#define BONOBO_TYPE_MONIKER        (bonobo_moniker_get_type ())
#define BONOBO_MONIKER_TYPE        BONOBO_TYPE_MONIKER /* deprecated, you should use BONOBO_TYPE_MONIKER */
#define BONOBO_MONIKER(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_MONIKER, BonoboMoniker))
#define BONOBO_MONIKER_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_MONIKER, BonoboMonikerClass))
#define BONOBO_IS_MONIKER(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_MONIKER))
#define BONOBO_IS_MONIKER_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_MONIKER))

typedef struct {
        BonoboObject         object;
	
	BonoboMonikerPrivate *priv;
} BonoboMoniker;

typedef struct {
	BonoboObjectClass      parent_class;

	POA_Bonobo_Moniker__epv epv;

	/* virtual methods */
	Bonobo_Unknown (*resolve)            (BonoboMoniker               *moniker,
					      const Bonobo_ResolveOptions *options,
					      const CORBA_char            *requested_interface,
					      CORBA_Environment           *ev);

	void           (*set_internal_name)  (BonoboMoniker               *moniker,
					      const char                  *unescaped_name);
	const char    *(*get_internal_name)  (BonoboMoniker               *moniker);

	gpointer        dummy;
} BonoboMonikerClass;

GType          bonobo_moniker_get_type           (void) G_GNUC_CONST;

BonoboMoniker *bonobo_moniker_construct          (BonoboMoniker     *moniker,
						  const char        *prefix);

Bonobo_Moniker bonobo_moniker_get_parent         (BonoboMoniker     *moniker,
						  CORBA_Environment *opt_ev);

void           bonobo_moniker_set_parent         (BonoboMoniker     *moniker,
						  Bonobo_Moniker     parent,
						  CORBA_Environment *opt_ev);

const char    *bonobo_moniker_get_name           (BonoboMoniker     *moniker);

const char    *bonobo_moniker_get_name_full      (BonoboMoniker     *moniker);
char          *bonobo_moniker_get_name_escaped   (BonoboMoniker     *moniker);

void           bonobo_moniker_set_name           (BonoboMoniker     *moniker,
						  const char        *unescaped_name);

const char    *bonobo_moniker_get_prefix         (BonoboMoniker     *moniker);

void           bonobo_moniker_set_case_sensitive (BonoboMoniker     *moniker,
						  gboolean           sensitive);
gboolean       bonobo_moniker_get_case_sensitive (BonoboMoniker     *moniker);

G_END_DECLS

#endif /* _BONOBO_MONIKER_H_ */
