/**
 * bonobo-config-bag.h: config bag object implementation.
 *
 * Author:
 *   Dietmar Maurer  (dietmar@ximian.com)
 *   Rodrigo Moya    (rodrigo@ximian.com)
 *
 * Copyright 2000, 2001 Ximian, Inc.
 */
#ifndef __BONOBO_CONFIG_BAG_H__
#define __BONOBO_CONFIG_BAG_H__

#include <gconf/gconf-client.h>
#include <bonobo/bonobo-object.h>
#include <bonobo/bonobo-event-source.h>

G_BEGIN_DECLS

#define BONOBO_TYPE_CONFIG_BAG        (bonobo_config_bag_get_type ())
#define BONOBO_CONFIG_BAG_TYPE        BONOBO_TYPE_CONFIG_BAG // deprecated, you should use BONOBO_TYPE_CONFIG_BAG
#define BONOBO_CONFIG_BAG(o)	      (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_CONFIG_BAG, BonoboConfigBag))
#define BONOBO_CONFIG_BAG_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), BONOBO_TYPE_CONFIG_BAG, BonoboConfigBagClass))
#define BONOBO_IS_CONFIG_BAG(o)	      (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_CONFIG_BAG))
#define BONOBO_IS_CONFIG_BAG_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), BONOBO_TYPE_CONFIG_BAG))

typedef struct _BonoboConfigBag        BonoboConfigBag;

struct _BonoboConfigBag {
	BonoboObject          base;

	gchar                 *path;
	BonoboEventSource     *es;
	GConfClient           *conf_client;
};

typedef struct {
	BonoboObjectClass  parent_class;

	POA_Bonobo_PropertyBag__epv epv;

} BonoboConfigBagClass;


GType		  bonobo_config_bag_get_type  (void);
BonoboConfigBag	 *bonobo_config_bag_new	      (const gchar *path);

G_END_DECLS

#endif /* ! __BONOBO_CONFIG_BAG_H__ */
