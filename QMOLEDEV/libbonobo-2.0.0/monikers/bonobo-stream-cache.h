#ifndef _BONOBO_STREAM_CACHE_H_
#define _BONOBO_STREAM_CACHE_H_

#include <bonobo/bonobo-stream.h>

G_BEGIN_DECLS

#define BONOBO_TYPE_STREAM_CACHE        (bonobo_stream_cache_get_type ())
#define BONOBO_STREAM_CACHE_TYPE        BONOBO_TYPE_STREAM_CACHE /* deprecated, you should use BONOBO_TYPE_STREAM_CACHE */
#define BONOBO_STREAM_CACHE(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), BONOBO_TYPE_STREAM_CACHE, BonoboStreamCache))
#define BONOBO_STREAM_CACHE_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), BONOBO_TYPE_STREAM_CACHE, BonoboStreamCacheClass))
#define BONOBO_IS_STREAM_CACHE(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), BONOBO_TYPE_STREAM_CACHE))
#define BONOBO_IS_STREAM_CACHE_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), BONOBO_TYPE_STREAM_CACHE))

typedef struct _BonoboStreamCachePrivate BonoboStreamCachePrivate;

typedef struct {
	BonoboObject object;

	BonoboStreamCachePrivate *priv;
} BonoboStreamCache;

typedef struct {
	BonoboObjectClass      parent_class;

	POA_Bonobo_Stream__epv epv;
} BonoboStreamCacheClass;

GType         bonobo_stream_cache_get_type (void);
BonoboObject *bonobo_stream_cache_create   (Bonobo_Stream      cs,
					    CORBA_Environment *opt_ev);
	
G_END_DECLS

#endif /* _BONOBO_STREAM_CACHE_H_ */
