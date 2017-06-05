/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
#ifndef _BONOBO_MONIKER_QUERY_H_
#define _BONOBO_MONIKER_QUERY_H_

G_BEGIN_DECLS

#define BONOBO_TYPE_MONIKER_QUERY        (bonobo_moniker_query_get_type ())
#define BONOBO_MONIKER_QUERY_TYPE        BONOBO_TYPE_MONIKER_QUERY /* deprecated, you should use BONOBO_TYPE_MONIKER_QUERY */
#define BONOBO_MONIKER_QUERY(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_MONIKER_QUERY, BonoboMonikerQuery))
#define BONOBO_MONIKER_QUERY_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_MONIKER_QUERY, BonoboMonikerQueryClass))
#define BONOBO_IS_MONIKER_QUERY(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_MONIKER_QUERY))
#define BONOBO_IS_MONIKER_QUERY_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_MONIKER_QUERY))

typedef struct _BonoboMonikerQuery        BonoboMonikerQuery;
typedef struct _BonoboMonikerQueryPrivate BonoboMonikerQueryPrivate;

struct _BonoboMonikerQuery {
	BonoboMoniker parent;

	BonoboMonikerQueryPrivate *priv;
};

typedef struct {
	BonoboMonikerClass parent_class;
} BonoboMonikerQueryClass;

GType          bonobo_moniker_query_get_type  (void);
BonoboMoniker *bonobo_moniker_query_construct (BonoboMonikerQuery *moniker,
					       Bonobo_Moniker      corba_moniker);
BonoboMoniker *bonobo_moniker_query_new       (void);
	
G_END_DECLS

#endif /* _BONOBO_MONIKER_QUERY_H_ */
