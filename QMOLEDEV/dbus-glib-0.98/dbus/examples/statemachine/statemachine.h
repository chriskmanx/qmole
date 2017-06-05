#ifndef _SM_OBJECT_H
#define _SM_OBJECT_H

#include <glib.h>
#include <glib-object.h>

GQuark sm_error_quark (void);

#define SM_ERROR (sm_error_quark ())

typedef enum
{
	SM_ERROR_INVALID_STATE = 0,
	SM_ERROR_NAME_IN_USE,
	SM_NUM_ERRORS
} SMError;

GType sm_error_get_type (void);
#define SM_TYPE_ERROR (sm_error_get_type ())

typedef enum
{
	SM_OBJECT_STATE_SHUTDOWN = 0,
	SM_OBJECT_STATE_INITIALIZED,
	SM_OBJECT_STATE_ACQUIRED,
	SM_OBJECT_STATE_OPERATING,
	SM_OBJECT_NUM_STATES
} SMObjectState;

GType sm_object_state_get_type (void);

#define SM_TYPE_OBJECT_STATE (sm_object_state_get_type ())

typedef struct SMObject SMObject;
typedef struct SMObjectClass SMObjectClass;

struct SMObject
{
  GObject parent;

  /* Private */
  char *name;
  SMObjectState state;
  double acquisition_progress;

  GSList /* guint */ *pending_tasks;

  SMObjectState requested_state;
};

struct SMObjectClass
{
  GObjectClass parent;
};

#define SM_TYPE_OBJECT              (sm_object_get_type ())
#define SM_OBJECT(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), SM_TYPE_OBJECT, SMObject))
#define SM_OBJECT_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), SM_TYPE_OBJECT, SMObjectClass))
#define SM_IS_OBJECT(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), SM_TYPE_OBJECT))
#define SM_IS_OBJECT_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), SM_TYPE_OBJECT))
#define SM_OBJECT_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), SM_TYPE_OBJECT, SMObjectClass))

GType sm_object_get_type (void);

gboolean sm_object_get_info (SMObject *object, char **name, char **state, GError **error);

gboolean sm_object_start (SMObject *object, GError **error);

gboolean sm_object_shutdown (SMObject *object, GError **error);

gboolean sm_object_reinitialize (SMObject *object, GError **error);

gboolean sm_object_reacquire (SMObject *object, GError **error);

gboolean sm_object_get_acquiring_progress (SMObject *object, gdouble *out, GError **error);

#endif
