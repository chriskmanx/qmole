#ifndef __TEST_OBJECTS_H__
#define __TEST_OBJECTS_H__

#include <glib-object.h>

#define TEST_TYPE_SONG		(test_song_get_type ())
#define TEST_SONG(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), TEST_TYPE_SONG, TestSong))
#define TEST_SONG_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST((k), TEST_TYPE_SONG, TestSongClass))
#define TEST_IS_SONG(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), TEST_TYPE_SONG))
#define TEST_IS_SONG_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), TEST_TYPE_SONG))
#define TEST_SONG_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), TEST_TYPE_SONG, TestSongClass))

#define TEST_TYPE_BEATLES_SONG		(test_beatles_song_get_type ())
#define TEST_BEATLES_SONG(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), TEST_TYPE_BEATLES_SONG, TestBeatlesSong))
#define TEST_BEATLES_SONG_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST((k), TEST_TYPE_BEATLES_SONG, TestBeatlesSongClass))
#define TEST_IS_BEATLES_SONG(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), TEST_TYPE_BEATLES_SONG))
#define TEST_IS_BEATLES_SONG_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), TEST_TYPE_BEATLES_SONG))
#define TEST_BEATLES_SONG_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), TEST_TYPE_BEATLES_SONG, TestBeatlesSongClass))

typedef GObject		TestSong;
typedef GObjectClass	TestSongClass;

typedef TestSong	TestBeatlesSong;
typedef TestSongClass	TestBeatlesSongClass;

GType		 test_song_get_type		(void);

GType		 test_beatles_song_get_type	(void);
TestSong	*test_beatles_song_new		(void);

#endif
