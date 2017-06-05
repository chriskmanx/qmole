/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * bonobo-moniker-extender: extending monikers
 *
 * Author:
 *	Dietmar Maurer (dietmar@maurer-it.com)
 *
 * Copyright 2000, Dietmar Maurer.
 */
#ifndef _BONOBO_MONIKER_EXTENDER_H_
#define _BONOBO_MONIKER_EXTENDER_H_

#include <bonobo/bonobo-moniker.h>

G_BEGIN_DECLS

#define BONOBO_TYPE_MONIKER_EXTENDER        (bonobo_moniker_extender_get_type ())
#define BONOBO_MONIKER_EXTENDER_TYPE        BONOBO_TYPE_MONIKER_EXTENDER /* deprecated, you should use BONOBO_TYPE_MONIKER_EXTENDER */
#define BONOBO_MONIKER_EXTENDER(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_MONIKER_EXTENDER, BonoboMonikerExtender))
#define BONOBO_MONIKER_EXTENDER_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_MONIKER_EXTENDER, BonoboMonikerExtenderClass))
#define BONOBO_IS_MONIKER_EXTENDER(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_MONIKER_EXTENDER))
#define BONOBO_IS_MONIKER_EXTENDER_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_MONIKER_EXTENDER))

typedef struct _BonoboMonikerExtender BonoboMonikerExtender;

typedef Bonobo_Unknown (*BonoboMonikerExtenderFn) (BonoboMonikerExtender       *extender,
						   const Bonobo_Moniker         parent,
						   const Bonobo_ResolveOptions *options,
						   const CORBA_char            *display_name,
						   const CORBA_char            *requested_interface,
						   CORBA_Environment           *ev);
struct _BonoboMonikerExtender {
        BonoboObject           object;
	BonoboMonikerExtenderFn resolve;
	gpointer                data;
};

typedef struct {
	BonoboObjectClass      parent_class;

	POA_Bonobo_MonikerExtender__epv epv;
	
	BonoboMonikerExtenderFn resolve;
} BonoboMonikerExtenderClass;

GType                  bonobo_moniker_extender_get_type (void) G_GNUC_CONST;
BonoboMonikerExtender *bonobo_moniker_extender_new      (BonoboMonikerExtenderFn      resolve,
							 gpointer                     data);
 
Bonobo_MonikerExtender bonobo_moniker_find_extender     (const gchar                 *name,
							 const gchar                 *interface,
							 CORBA_Environment           *opt_ev);

Bonobo_Unknown         bonobo_moniker_use_extender      (const gchar                 *extender_oafiid,
							 BonoboMoniker               *moniker,
							 const Bonobo_ResolveOptions *options,
							 const CORBA_char            *requested_interface,
							 CORBA_Environment           *opt_ev);

G_END_DECLS

#endif /* _BONOBO_MONIKER_EXTENDER_H_ */
