#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include "statemachine.h"

static void clear_pending_tasks (SMObject *object);
static void state_change (SMObject *object, SMObjectState new_state);
static void sm_object_set_property (GObject *object,
				    guint prop_id,
				    const GValue *value,
				    GParamSpec *pspec);
static void sm_object_get_property (GObject *object,
				    guint prop_id,
				    GValue *value,
				    GParamSpec *pspec);
enum
{
  PROP_0,
  PROP_NAME
};

enum
{
  STATE_CHANGED,
  ACQUISITION_FAILED,
  ACQUISITION_PROGRESS,
  LAST_SIGNAL
};

static guint sm_object_signals[LAST_SIGNAL] = { 0 };

G_DEFINE_TYPE(SMObject, sm_object, G_TYPE_OBJECT)

static void
sm_object_init (SMObject *obj)
{
  obj->state = SM_OBJECT_STATE_SHUTDOWN;
}

static void
sm_object_class_init (SMObjectClass *klass)
{
  GObjectClass *object_class;
  
  object_class = G_OBJECT_CLASS (klass);

  object_class->set_property = sm_object_set_property;
  object_class->get_property = sm_object_get_property;
  
  g_object_class_install_property (object_class,
				   PROP_NAME,
				   g_param_spec_string ("name",
							"name",
							"name",
							NULL,
							G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  sm_object_signals[STATE_CHANGED] =
    g_signal_new ("state-changed",
		  G_OBJECT_CLASS_TYPE (klass),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  g_cclosure_marshal_VOID__STRING,
                  G_TYPE_NONE, 1, G_TYPE_STRING);
  sm_object_signals[ACQUISITION_PROGRESS] =
    g_signal_new ("acquisition-progress",
		  G_OBJECT_CLASS_TYPE (klass),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  g_cclosure_marshal_VOID__DOUBLE,
                  G_TYPE_NONE, 1, G_TYPE_DOUBLE);
  sm_object_signals[ACQUISITION_FAILED] =
    g_signal_new ("acquisition-failed",
		  G_OBJECT_CLASS_TYPE (klass),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
                  0,
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);
}

/* This should really be standard. */
#define ENUM_ENTRY(NAME, DESC) { NAME, "" #NAME "", DESC }

GQuark
sm_error_quark (void)
{
  static GQuark ret = 0;
  if (!ret)
    ret = g_quark_from_static_string ("SMObjectErrorQuark");
  return ret;
}

GType
sm_object_state_get_type (void)
{
  static GType etype = 0;

  if (etype == 0)
    {
      static const GEnumValue values[] =
	{

	  ENUM_ENTRY (SM_OBJECT_STATE_SHUTDOWN, "Shutdown"),
	  ENUM_ENTRY (SM_OBJECT_STATE_INITIALIZED, "Loading"),
	  ENUM_ENTRY (SM_OBJECT_STATE_ACQUIRED, "Acquired"),
	  ENUM_ENTRY (SM_OBJECT_STATE_OPERATING, "Operating"),
	  { 0, 0, 0 }
	};

      etype = g_enum_register_static ("SMObjectState", values);
    }

  return etype;
}

GType
sm_error_get_type (void)
{
  static GType etype = 0;

  if (etype == 0)
    {
      static const GEnumValue values[] =
	{

	  ENUM_ENTRY (SM_ERROR_INVALID_STATE, "InvalidState"),
	  ENUM_ENTRY (SM_ERROR_NAME_IN_USE, "NameInUse"),
	  { 0, 0, 0 }
	};

      g_assert (SM_NUM_ERRORS == G_N_ELEMENTS (values) - 1);

      etype = g_enum_register_static ("SMError", values);
    }

  return etype;
}

static void
sm_object_set_property (GObject *object,
			guint prop_id,
			const GValue *value,
			GParamSpec *pspec)
{
  SMObject *sm = SM_OBJECT (object);

  switch (prop_id)
    {
    case PROP_NAME:
      sm->name = g_strdup (g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void 
sm_object_get_property (GObject *object,
			guint prop_id,
			GValue *value,
			GParamSpec *pspec)
{
  SMObject *sm= SM_OBJECT (object);

  switch (prop_id)
    {
    case PROP_NAME:
      g_value_set_string (value, sm->name);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static const char *
state_to_string (SMObjectState state)
{
  GEnumValue *value;
  GEnumClass *prop_class;
  const char *ret;
  
  prop_class = g_type_class_ref (SM_TYPE_OBJECT_STATE);
  value = g_enum_get_value (prop_class, state);
  ret = value->value_nick;

  g_type_class_unref (prop_class);
  return ret;
}

static void
queue_task (SMObject *object, guint delay, GSourceFunc func)
{
  guint id;
  id = g_timeout_add (delay, func, object);
  object->pending_tasks = g_slist_prepend (object->pending_tasks, GUINT_TO_POINTER (id));
}

static gboolean
idle_state_change (gpointer data)
{
  SMObject *object = data;

  g_print ("doing idle state change for %s to %s\n",
	   object->name, state_to_string (object->requested_state));
  state_change (object, object->requested_state);
  return FALSE;
}

static gboolean
idle_further_acquire (gpointer data)
{
  SMObject *object = data;

  g_print ("doing idle acquisition for machine %s\n", object->name);
  object->acquisition_progress += g_random_double_range (0.20, 0.7);
  if (object->acquisition_progress > 1.0)
    {
      object->acquisition_progress = 1.0;
      return FALSE;
    }
  else
    {
      g_signal_emit (object, sm_object_signals[ACQUISITION_PROGRESS], 0, object->acquisition_progress);
      return TRUE;
    }
}

static void
clear_pending_tasks (SMObject *object)
{
  GSList *tmp;
  for (tmp = object->pending_tasks; tmp; tmp = tmp->next)
    g_source_remove (GPOINTER_TO_UINT (tmp->data));
  g_slist_free (object->pending_tasks);
  object->pending_tasks = NULL;
}

static void
state_change (SMObject *object, SMObjectState new_state)
{
  g_signal_emit (object, sm_object_signals[STATE_CHANGED], 0,
		 state_to_string (new_state));

  clear_pending_tasks (object);

  if (new_state == SM_OBJECT_STATE_ACQUIRED)
    {
      object->acquisition_progress = 0.0;
      queue_task (object, 1000, idle_further_acquire);
    }
  else if (new_state == SM_OBJECT_STATE_INITIALIZED)
    {
      if (g_random_int_range (0, 2) == 0)
	{
	  object->requested_state = SM_OBJECT_STATE_ACQUIRED;
	  queue_task (object, 3000, idle_state_change);
	}
    }
  
  object->state = new_state;
}

gboolean
sm_object_get_info (SMObject *object, char **name, char **state, GError **error)
{
  *name= g_strdup (object->name);
  *state = g_strdup (state_to_string (object->state));
  return TRUE;
}

gboolean
sm_object_start (SMObject *object, GError **error)
{
  if (object->state != SM_OBJECT_STATE_SHUTDOWN)
    {
      g_set_error (error,
		   SM_ERROR,
		   SM_ERROR_INVALID_STATE,
		   "%s",
		   "Can't start from non-shutdown state");
      return FALSE;
    }
  state_change (object, SM_OBJECT_STATE_INITIALIZED);
  return TRUE;
}

gboolean
sm_object_shutdown (SMObject *object, GError **error)
{
  if (object->state == SM_OBJECT_STATE_SHUTDOWN)
    {
      g_set_error (error,
		   SM_ERROR,
		   SM_ERROR_INVALID_STATE,
		   "%s",
		   "Can't shutdown from shutdown state");
      return FALSE;
    }
  state_change (object, SM_OBJECT_STATE_SHUTDOWN);
  return TRUE;
}

gboolean
sm_object_reinitialize (SMObject *object, GError **error)
{
  if (object->state != SM_OBJECT_STATE_ACQUIRED
      && object->state != SM_OBJECT_STATE_OPERATING)
    {
      g_set_error (error,
		   SM_ERROR,
		   SM_ERROR_INVALID_STATE,
		   "Can't reinitialize from state %d",
		   object->state);
      return FALSE;
    }
  state_change (object, SM_OBJECT_STATE_INITIALIZED);
  return TRUE;
}

gboolean
sm_object_reacquire (SMObject *object, GError **error)
{
  if (object->state == SM_OBJECT_STATE_ACQUIRED)
    {
      g_set_error (error,
		   SM_ERROR,
		   SM_ERROR_INVALID_STATE,
		   "Can't reacquire from state %d",
		   object->state);
      return FALSE;
    }
  state_change (object, SM_OBJECT_STATE_ACQUIRED);
  return TRUE;
}

gboolean
sm_object_get_acquiring_progress (SMObject *object, gdouble *out, GError **error)
{
  if (object->state != SM_OBJECT_STATE_ACQUIRED)
    {
      g_set_error (error,
		   SM_ERROR,
		   SM_ERROR_INVALID_STATE,
		   "Can't get progress from state %d",
		   object->state);
      return FALSE;
    }
  *out = object->acquisition_progress;
  return TRUE;
}
