#ifndef __TEST_INTERFACES_H__
#define __TEST_INTERFACES_H__

#include <glib-object.h>

#define TEST_TYPE_HELLO			(test_hello_get_type ())
#define TEST_HELLO(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_HELLO, TestHello))
#define TEST_HELLO_IFACE(obj)		(G_TYPE_CHECK_CLASS_CAST ((obj), TEST_TYPE_HELLO, TestHelloIface))
#define TEST_IS_HELLO(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_HELLO))
#define TEST_HELLO_GET_IFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), TEST_TYPE_HELLO, TestHelloIface))

#define TEST_TYPE_GOODBYE		(test_goodbye_get_type ())
#define TEST_GOODBYE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_GOODBYE, TestGoodbye))
#define TEST_GOODBYE_IFACE(obj)		(G_TYPE_CHECK_CLASS_CAST ((obj), TEST_TYPE_GOODBYE, TestGoodbyeIface))
#define TEST_IS_GOODBYE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_GOODBYE))
#define TEST_GOODBYE_GET_IFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), TEST_TYPE_GOODBYE, TestGoodbyeIface))

typedef struct _TestHello		TestHello; /* dummy */
typedef struct _TestHelloIface		TestHelloIface;

typedef struct _TestGoodbye		TestGoodbye; /* dummy */
typedef struct _TestGoodbyeIface	TestGoodbyeIface;

struct _TestHelloIface {
	GTypeInterface interface;
	
	/* VTable */
	gchar	*(* say_hello)		(TestHello *hello);
	
	/* Signals */
	void	 (* greetings)		(TestHello *hello);
};

struct _TestGoodbyeIface {
	GTypeInterface interface;
	
	/* VTable */
	gchar	*(* say_goodbye)	(TestGoodbye *goodbye);
};

GType    test_hello_get_type		(void) G_GNUC_CONST;
gchar	*test_hello_say_hello		(TestHello *hello);
void	 test_hello_greetings		(TestHello *hello);

GType    test_goodbye_get_type		(void) G_GNUC_CONST;
gchar	*test_goodbye_say_goodbye	(TestGoodbye *goodbye);

#endif
