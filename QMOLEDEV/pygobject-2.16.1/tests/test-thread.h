#include <glib-object.h>

typedef struct {
  GObject parent;
  GThread *thread;
} TestThread;

typedef struct {
  GObjectClass parent_class;
  void (*emit_signal) (TestThread *sink);
  void (*from_thread)	(TestThread *sink);
} TestThreadClass;

GType        test_thread_get_type   (void);

#define TEST_TYPE_THREAD            (test_thread_get_type())
#define TEST_THREAD(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TEST_TYPE_THREAD, TestTHREAD))
#define TEST_THREAD_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), TEST_TYPE_THREAD, TestTHREADClass))
#define TEST_IS_THREAD(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TEST_TYPE_THREAD))
#define TEST_IS_THREAD_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), TEST_TYPE_THREAD))
#define TEST_THREAD_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), TEST_TYPE_THREAD, TestTHREADClass))

