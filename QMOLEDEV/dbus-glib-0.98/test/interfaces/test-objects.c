#include <config.h>

#include "test-objects.h"
#include "test-interfaces.h"

static gboolean
test_song_dbus_get_title (TestSong  *song,
			  gchar    **title,
			  GError   **error)
{
	*title = g_strdup ("Hello, Goodbye");
	return TRUE;
}

#include "test-song-glue.h"

static gchar *
test_song_say_hello (TestHello *hello)
{
	return g_strdup ("Hello, hello...");
}

static void
test_song_init (TestSong *song)
{
}

static void
test_song_hello_init (TestHelloIface *iface)
{
	iface->say_hello = test_song_say_hello;
}


static void
test_song_class_init (TestSongClass *klass)
{
	dbus_g_object_type_install_info (G_TYPE_FROM_CLASS (klass),
					 &dbus_glib_test_song_object_info);
}

G_DEFINE_TYPE_WITH_CODE (TestSong, test_song, G_TYPE_OBJECT,
			 G_IMPLEMENT_INTERFACE (TEST_TYPE_HELLO, test_song_hello_init))

static gchar *
test_beatles_song_say_goodbye (TestGoodbye *goodbye)
{
	return g_strdup ("I don't know why you say goodbye, I say hello.");
}

static void
test_beatles_song_init (TestBeatlesSong *song)
{
}

static void
test_beatles_song_goodbye_init (TestGoodbyeIface *iface)
{
	iface->say_goodbye = test_beatles_song_say_goodbye;
}

static void
test_beatles_song_class_init (TestBeatlesSongClass *klass)
{
}

G_DEFINE_TYPE_WITH_CODE (TestBeatlesSong, test_beatles_song, TEST_TYPE_SONG,
			 G_IMPLEMENT_INTERFACE (TEST_TYPE_GOODBYE, test_beatles_song_goodbye_init))


TestBeatlesSong *
test_beatles_song_new (void)
{
	return TEST_BEATLES_SONG (g_object_new (TEST_TYPE_BEATLES_SONG, NULL));
}
