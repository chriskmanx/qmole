#ifndef _PYGOBJECT_PRIVATE_H_
#define _PYGOBJECT_PRIVATE_H_

#ifdef _PYGOBJECT_H_
#  error "include pygobject.h or pygobject-private.h, but not both"
#endif

#define _INSIDE_PYGOBJECT_
#include "pygobject.h"

#include "pyglib-python-compat.h"

#define PYGOBJECT_REGISTER_GTYPE(d, type, name, gtype)      \
  {                                                         \
    PyObject *o;					    \
    PYGLIB_REGISTER_TYPE(d, type, name);                    \
    PyDict_SetItemString(type.tp_dict, "__gtype__",         \
			 o=pyg_type_wrapper_new(gtype));    \
    Py_DECREF(o);                                           \
}

/* from gobjectmodule.c */
extern struct _PyGObject_Functions pygobject_api_functions;
#define pyg_block_threads()   G_STMT_START { \
    if (pygobject_api_functions.block_threads != NULL)    \
      (* pygobject_api_functions.block_threads)();        \
  } G_STMT_END
#define pyg_unblock_threads() G_STMT_START { \
    if (pygobject_api_functions.unblock_threads != NULL)  \
      (* pygobject_api_functions.unblock_threads)();      \
  } G_STMT_END

#define pyg_threads_enabled (pygobject_api_functions.threads_enabled)


#define pyg_gil_state_ensure() (pygobject_api_functions.threads_enabled? (PyGILState_Ensure()) : 0)
#define pyg_gil_state_release(state) G_STMT_START {     \
    if (pygobject_api_functions.threads_enabled)        \
        PyGILState_Release(state);                      \
    } G_STMT_END

#define pyg_begin_allow_threads                         \
    G_STMT_START {                                      \
        PyThreadState *_save = NULL;                    \
        if (pygobject_api_functions.threads_enabled)    \
            _save = PyEval_SaveThread();
#define pyg_end_allow_threads                           \
        if (pygobject_api_functions.threads_enabled)    \
            PyEval_RestoreThread(_save);                \
    } G_STMT_END


#ifndef Py_CLEAR /* since Python 2.4 */
# define Py_CLEAR(op)				\
        do {                            	\
                if (op) {			\
                        PyObject *tmp = (PyObject *)(op);	\
                        (op) = NULL;		\
                        Py_DECREF(tmp);		\
                }				\
        } while (0)
#endif

extern GType PY_TYPE_OBJECT;

extern GQuark pygboxed_type_key;
extern GQuark pygboxed_marshal_key;
extern GQuark pygenum_class_key;
extern GQuark pygflags_class_key;
extern GQuark pyginterface_type_key;
extern GQuark pyginterface_info_key;
extern GQuark pygobject_class_init_key;
extern GQuark pygobject_class_key;
extern GQuark pygobject_wrapper_key;
extern GQuark pygpointer_class_key;
extern GQuark pygobject_has_updated_constructor_key;
extern GQuark pygobject_instance_data_key;

void     pygobject_data_free  (PyGObjectData *data);
void     pyg_destroy_notify   (gpointer     user_data);
gboolean pyg_handler_marshal  (gpointer     user_data);
gboolean pyg_error_check      (GError     **error);
int      pygobject_constructv (PyGObject   *self,
                               guint        n_parameters,
                               GParameter  *parameters);
int      pygobject_construct  (PyGObject   *self,
                               const char  *first_property_name,
                               ...);
void     pyg_set_object_has_new_constructor (GType gtype);

PyObject *pyg_integer_richcompare(PyObject *v,
                                  PyObject *w,
                                  int op);

gboolean pyg_gerror_exception_check(GError **error);

/* from pygtype.h */
extern PyTypeObject PyGTypeWrapper_Type;

PyObject *pyg_type_wrapper_new (GType type);
GType     pyg_type_from_object (PyObject *obj);

gint pyg_enum_get_value  (GType enum_type, PyObject *obj, gint *val);
gint pyg_flags_get_value (GType flag_type, PyObject *obj, gint *val);
int pyg_pyobj_to_unichar_conv (PyObject* py_obj, void* ptr);

typedef PyObject *(* fromvaluefunc)(const GValue *value);
typedef int (*tovaluefunc)(GValue *value, PyObject *obj);

void      pyg_register_gtype_custom(GType gtype,
			     fromvaluefunc from_func,
			     tovaluefunc to_func);
int       pyg_value_from_pyobject(GValue *value, PyObject *obj);
PyObject *pyg_value_as_pyobject(const GValue *value, gboolean copy_boxed);
int       pyg_param_gvalue_from_pyobject(GValue* value,
                                         PyObject* py_obj, 
                                         const GParamSpec* pspec);
PyObject *pyg_param_gvalue_as_pyobject(const GValue* gvalue,
                                       gboolean copy_boxed, 
                                       const GParamSpec* pspec);

GClosure *pyg_closure_new(PyObject *callback, PyObject *extra_args, PyObject *swap_data);
void	  pyg_closure_set_exception_handler(GClosure *closure,
					    PyClosureExceptionHandler handler);
GClosure *pyg_signal_class_closure_get(void);
GClosure *gclosure_from_pyfunc(PyGObject *object, PyObject *func);

PyObject *pyg_object_descr_doc_get(void);
void pygobject_object_register_types(PyObject *d);

extern PyTypeObject *PyGObject_MetaType;

/* from pygobject.h */
extern PyTypeObject PyGObject_Type;
extern PyTypeObject PyGProps_Type;
extern PyTypeObject PyGPropsDescr_Type;
extern PyTypeObject PyGPropsIter_Type;

  /* Data that belongs to the GObject instance, not the Python wrapper */
struct _PyGObjectData {
    PyTypeObject *type; /* wrapper type for this instance */
    GSList *closures;
};

void          pygobject_register_class   (PyObject *dict,
					  const gchar *type_name,
					  GType gtype, PyTypeObject *type,
					  PyObject *bases);
void          pygobject_register_wrapper (PyObject *self);
PyObject *    pygobject_new              (GObject *obj);
PyObject *    pygobject_new_full         (GObject *obj, gboolean sink, gpointer g_class);
void          pygobject_sink             (GObject *obj);
PyTypeObject *pygobject_lookup_class     (GType gtype);
void          pygobject_watch_closure    (PyObject *self, GClosure *closure);
void          pygobject_register_sinkfunc(GType type,
					  void (* sinkfunc)(GObject *object));
int           pyg_type_register          (PyTypeObject *class,
					  const gchar *type_name);

/* from pygboxed.c */
extern PyTypeObject PyGBoxed_Type;

void       pyg_register_boxed (PyObject *dict, const gchar *class_name,
			       GType boxed_type, PyTypeObject *type);
PyObject * pyg_boxed_new      (GType boxed_type, gpointer boxed,
			       gboolean copy_boxed, gboolean own_ref);

extern PyTypeObject PyGPointer_Type;

void       pyg_register_pointer (PyObject *dict, const gchar *class_name,
				 GType pointer_type, PyTypeObject *type);
PyObject * pyg_pointer_new      (GType pointer_type, gpointer pointer);

const gchar * pyg_constant_strip_prefix(const gchar *name, const gchar *strip_prefix);

/* pygflags */
typedef struct {
    _PyLongObject parent;
    GType gtype;
} PyGFlags;

extern PyTypeObject PyGFlags_Type;

#define PyGFlags_Check(x) (g_type_is_a(((PyGFlags*)x)->gtype, G_TYPE_FLAGS))
			   
extern PyObject * pyg_flags_add        (PyObject *   module,
					const char * type_name,
					const char * strip_prefix,
					GType        gtype);
extern PyObject * pyg_flags_from_gtype (GType        gtype,
					int          value);

/* pygenum */
#define PyGEnum_Check(x) (g_type_is_a(((PyGFlags*)x)->gtype, G_TYPE_ENUM))

typedef struct {
    _PyLongObject parent;
    GType gtype;
} PyGEnum;

extern PyTypeObject PyGEnum_Type;

extern PyObject * pyg_enum_add        (PyObject *   module,
				       const char * type_name,
				       const char * strip_prefix,
				       GType        gtype);
extern PyObject * pyg_enum_from_gtype (GType        gtype,
				       int          value);

/* pygtype.c */
extern GHashTable *custom_type_registration;
void pyg_type_register_custom_callback(const gchar *type_name,
				       PyGTypeRegistrationFunction callback,
				       gpointer data);
PyTypeObject * pyg_type_get_custom(const gchar *name);
GType _pyg_type_from_name(const gchar *name);

/* pygobject.c */
extern PyTypeObject PyGObjectWeakRef_Type;

static inline PyGObjectData *
pyg_object_peek_inst_data(GObject *obj)
{
    return ((PyGObjectData *) 
            g_object_get_qdata(obj, pygobject_instance_data_key));
}


#endif
