/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
#ifndef _ECHO_H_
#define _ECHO_H_

#include <bonobo/bonobo-object.h>

G_BEGIN_DECLS

#define ECHO_TYPE         (echo_get_type ())
#define ECHO(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), ECHO_TYPE, Echo))
#define ECHO_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), ECHO_TYPE, EchoClass))
#define ECHO_IS_OBJECT(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), ECHO_TYPE))
#define ECHO_IS_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), ECHO_TYPE))
#define ECHO_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), ECHO_TYPE, EchoClass))

typedef struct {
	BonoboObject parent;

	char *instance_data;
} Echo;

typedef struct {
	BonoboObjectClass parent_class;

	POA_Bonobo_Sample_Echo__epv epv;
} EchoClass;

GType echo_get_type (void);

G_END_DECLS

#endif /* _ECHO_H_ */
