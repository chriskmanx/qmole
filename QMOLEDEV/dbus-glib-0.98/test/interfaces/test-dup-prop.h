#ifndef __TEST_DUP_PROP_H__
#define __TEST_DUP_PROP_H__

#include <glib-object.h>

#define TEST_TYPE_A		(test_a_get_type ())
#define TEST_A(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_A, TestA))
#define TEST_A_IFACE(obj)	(G_TYPE_CHECK_CLASS_CAST ((obj), TEST_TYPE_A, TestAIface))
#define TEST_IS_IFACE_A(obj)	(G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_A))
#define TEST_A_GET_IFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), TEST_TYPE_A, TestAIface))

#define TEST_TYPE_B		(test_b_get_type ())
#define TEST_B(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_B, TestB))
#define TEST_B_IFACE(obj)	(G_TYPE_CHECK_CLASS_CAST ((obj), TEST_TYPE_B, TestBIface))
#define TEST_IS_B(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_B))
#define TEST_B_GET_IFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), TEST_TYPE_B, TestBIface))

#define TEST_TYPE_DP_OBJ            (test_dp_obj_get_type ())
#define TEST_DP_OBJ(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_DP_OBJ, TestDpObj))
#define TEST_DP_OBJ_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  TEST_TYPE_DP_OBJ, TestDpObjClass))
#define TEST_IS_DP_OBJ(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_DP_OBJ))
#define TEST_IS_DP_OBJ_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  TEST_TYPE_DP_OBJ))
#define TEST_DP_OBJ_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  TEST_TYPE_DP_OBJ, TestDpObjClass))


typedef struct _TestA		TestA; /* dummy */
typedef struct _TestAIface	TestAIface;

typedef struct _TestB		TestB; /* dummy */
typedef struct _TestBIface	TestBIface;

typedef struct _TestDpObj	TestDpObj;
typedef struct _TestDpObjClass	TestDpObjClass;

struct _TestAIface {
	GTypeInterface interface;
};

struct _TestBIface {
	GTypeInterface interface;
};

struct _TestDpObj {
    GObject parent;
};

struct _TestDpObjClass {
    GObjectClass parent;
};


GType test_a_get_type (void) G_GNUC_CONST;

GType test_b_get_type (void) G_GNUC_CONST;

GType test_dp_obj_get_type (void) G_GNUC_CONST;

TestDpObj *test_dp_obj_new (void);

#endif
