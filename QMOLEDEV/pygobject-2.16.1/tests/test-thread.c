#include "test-thread.h"

enum
{
  /* methods */
  SIGNAL_EMIT_SIGNAL,
  SIGNAL_FROM_THREAD,
  LAST_SIGNAL
};

static guint test_thread_signals[LAST_SIGNAL] = { 0 };

typedef enum {
  TEST_THREAD_A,
  TEST_THREAD_B
} ThreadEnumType;

static GType
test_thread_enum_get_type (void)
{
  static GType enum_type = 0;
  static GEnumValue enum_values[] = {
    {TEST_THREAD_A, "TEST_THREAD_A", "a as in apple"},
    {0, NULL, NULL},
  };

  if (!enum_type) {
    enum_type =
        g_enum_register_static ("TestThreadEnum", enum_values);
  }
  return enum_type;
}

G_DEFINE_TYPE(TestThread, test_thread, G_TYPE_OBJECT);

static void
other_thread_cb (TestThread *self)
{
  g_signal_emit_by_name (self, "from-thread", 0, NULL);
  g_thread_exit (0);
}

static void
test_thread_emit_signal (TestThread *self)
{
  self->thread = g_thread_create ((GThreadFunc)other_thread_cb,
				  self, TRUE, NULL);
}

static void test_thread_init (TestThread *self) {}
static void test_thread_class_init (TestThreadClass *klass)
{
  test_thread_signals[SIGNAL_EMIT_SIGNAL] =
    g_signal_new ("emit-signal", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (TestThreadClass, emit_signal),
		  NULL, NULL, g_cclosure_marshal_VOID__VOID, G_TYPE_NONE, 0);
  test_thread_signals[SIGNAL_FROM_THREAD] =
    g_signal_new ("from-thread", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (TestThreadClass, from_thread),
		  NULL, NULL, g_cclosure_marshal_VOID__BOXED, G_TYPE_NONE, 1,
		  test_thread_enum_get_type ());

  klass->emit_signal = test_thread_emit_signal;
}
