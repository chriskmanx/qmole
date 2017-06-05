/*
 * Copyright (C) 1997-1998 Stuart Parmenter and Elliot Lee
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
 */

#include "config.h"

#include "libgnome.h"
#include "gnome-sound.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#ifdef HAVE_ESD
#include <esd.h>
#endif

#ifdef HAVE_ESD
static char *esound_hostname = NULL;
static gboolean esound_hostname_null_ok = FALSE;
static int gnome_sound_connection = -1;
#endif

typedef struct _sample
  {
    int rate, format, samples, id, size;
    short *data;
  }
GnomeSoundSample;

#ifndef HAVE_LIBAUDIOFILE
typedef struct _WAVFormatChunk
  {
    char chunkID[4];
    int chunkSize;

    unsigned int dwSamplesPerSec;
    unsigned int dwAvgBytesPerSec;
    short wFormatTag;
    unsigned short wChannels;
    unsigned short wBlockAlign;
    unsigned short wBitsPerSample;
  }
WAVFormatChunk;

#ifdef WORDS_BIGENDIAN
#define SWAP_SHORT( x ) x = ( ( x & 0x00ff ) << 8 ) | ( ( x >> 8 ) & 0x00ff )
#define SWAP_LONG( x ) x = ( ( ( x & 0x000000ff ) << 24 ) |\
( ( x & 0x0000ff00 ) << 8 ) |\
( ( x & 0x00ff0000 ) >> 8 ) |\
( ( x & 0xff000000 ) >> 24 ) )
#endif

/**
 * gnome_sound_sample_load_wav:
 * @file: filename to try loading a WAV file from.
 * 
 * Used to load a .wav file into esound.
 *
 * Returns a GnomeSoundSample or NULL if file did not exist.
 *
 */
#ifdef HAVE_ESD
static GnomeSoundSample *
gnome_sound_sample_load_wav(const char *file)
{
  FILE *f;
  GnomeSoundSample *s;
  char buf[4];
  WAVFormatChunk fmt;
  int skipl = 0;
  int skipr = 0;
  char bytes = 0;
  char stereo = 0;
  int len;

  /* int                 count; */

  f = fopen (file, "r");
  if (!f)
    return NULL;
  s = g_malloc (sizeof (GnomeSoundSample));
  if (!s)
    {
      fclose (f);
      return NULL;
    }
  s->rate = 44100;
  s->format = ESD_STREAM | ESD_PLAY;
  s->samples = 0;
  s->data = NULL;
  s->id = 0;
  fread (buf, 1, 4, f);
  if ((buf[0] != 'R') ||
      (buf[1] != 'I') ||
      (buf[2] != 'F') ||
      (buf[3] != 'F'))
    {
      /* not a RIFF WAV file */
      fclose (f);
      g_free (s);
      return NULL;
    }
  fread (buf, 1, 4, f);
  fread (buf, 1, 4, f);
  fread (fmt.chunkID, 1, 4, f);
  fread (&(fmt.chunkSize), 1, 4, f);

#ifdef WORDS_BIGENDIAN
  SWAP_LONG (fmt.chunkSize);
#endif

  if ((fmt.chunkID[0] == 'f') &&
      (fmt.chunkID[1] == 'm') &&
      (fmt.chunkID[2] == 't') &&
      (fmt.chunkID[3] == ' ') &&
      16 == fmt.chunkSize)
    /* fmt chunk */
    {
      fread (&(fmt.wFormatTag), 1, 2, f);
      fread (&(fmt.wChannels), 1, 2, f);
      fread (&(fmt.dwSamplesPerSec), 1, 4, f);
      fread (&(fmt.dwAvgBytesPerSec), 1, 4, f);
      fread (&(fmt.wBlockAlign), 1, 2, f);
      fread (&(fmt.wBitsPerSample), 1, 2, f);
#ifdef WORDS_BIGENDIAN
      SWAP_SHORT (fmt.wFormatTag);
      SWAP_SHORT (fmt.wChannels);
      SWAP_LONG (fmt.dwSamplesPerSec);
      SWAP_LONG (fmt.dwAvgBytesPerSec);
      SWAP_SHORT (fmt.wBlockAlign);
      SWAP_SHORT (fmt.wBitsPerSample);
#endif

      if (fmt.wFormatTag != 1)
	{
	  /* unknown WAV encoding format - exit */
	  fclose (f);
	  g_free (s);
	  return NULL;
	}
      skipl = 0;
      skipr = 0;
      bytes = 0;
      stereo = 0;
      if (fmt.wChannels == 1)
	s->format |= ESD_MONO;
      else if (fmt.wChannels == 2)
	{
	  stereo = 1;
	  s->format |= ESD_STEREO;
	}
      else
	{
	  stereo = 1;
	  s->format |= ESD_STEREO;
	  if (fmt.wChannels == 3)
	    {
	      skipl = 0;
	      skipr = 1;
	    }
	  else if (fmt.wChannels == 4)
	    {
	      skipl = 0;
	      skipr = 2;
	    }
	  else if (fmt.wChannels == 4)
	    {
	      skipl = 0;
	      skipr = 2;
	    }
	  else if (fmt.wChannels == 6)
	    {
	      skipl = 3;
	      skipr = 1;
	    }
	  else
	    {
	      /* unknown channel encoding */
	      fclose (f);
	      g_free (s);
	      return NULL;
	    }
	}
      s->rate = fmt.dwSamplesPerSec;
      if (fmt.wBitsPerSample <= 8)
	{
	  bytes = 1;
	  s->format |= ESD_BITS8;
	}
      else if (fmt.wBitsPerSample <= 16)
	s->format |= ESD_BITS16;
      else
	{
	  /* unknown bits encoding encoding */
	  fclose (f);
	  g_free (s);
	  return NULL;
	}
    }
  for (;;)
    {
      if (fread (buf, 1, 4, f) &&
	  fread (&len, 4, 1, f))
	{
#ifdef WORDS_BIGENDIAN
	  SWAP_LONG (len);
#endif

	  if ((buf[0] != 'd') ||
	      (buf[1] != 'a') ||
	      (buf[2] != 't') ||
	      (buf[3] != 'a'))
	    fseek (f, len, SEEK_CUR);
	  else
	    {
	      s->data = g_malloc (len);
	      if (!s->data)
		{
		  fclose (f);
		  g_free (s);
		  return NULL;
		}
	      if ((skipl == 0) && (skipr == 0))
		{
		  fread (s->data, len, 1, f);
#ifdef WORDS_BIGENDIAN
		  if (fmt.wBitsPerSample > 8 && fmt.wBitsPerSample <= 16)
		    {
		      char *tmp;
		      char tmpval;
		      int i;

		      tmp = (char *) (s->data);

		      for (i = 0; i < len; i++)
			{
			  tmpval = tmp[i];
			  tmp[i] = tmp[i + 1];
			  tmp[i + 1] = tmpval;
			}
		    }
#endif
		}
	      else
		{
		}
	      s->samples = len;
	      if (stereo)
		s->samples /= 2;
	      if (!bytes)
		s->samples /= 2;
	      fclose (f);
	      return s;
	    }
	}
      else
	{
	  fclose (f);
	  return NULL;
	}
    }
  fclose (f);
  g_free (s);
  if (s->data)
    g_free (s->data);

  return NULL;
}
#endif
#endif

#ifdef HAVE_ESD
/*
 * This does delayed initialization of Esound
 */
static gboolean
use_sound (void)
{
  if (gnome_sound_connection == -1){
    if (esound_hostname || esound_hostname_null_ok){
      gnome_sound_connection = esd_open_sound (esound_hostname);
      if (gnome_sound_connection == -1){
	g_free (esound_hostname);
	esound_hostname = NULL;
	esound_hostname_null_ok = FALSE;
	return FALSE;
      }
    }
  }
  return TRUE;
}
#endif

#if defined(HAVE_LIBAUDIOFILE) && defined(HAVE_ESD)
#include <audiofile.h>

static GnomeSoundSample *
gnome_sound_sample_load_audiofile(const char *file)
{
  AFfilehandle in_file;
  GnomeSoundSample *s;
  int in_format, in_width, in_channels;
  double in_rate;
  int bytes_per_frame;
  AFframecount frame_count, frames_read;

  int out_bits, out_channels, out_rate;
  int out_mode = ESD_STREAM, out_func = ESD_PLAY;
  esd_format_t out_format;

  in_file = afOpenFile(file, "r", NULL);
  if(!in_file)
    return NULL;

  frame_count = afGetFrameCount(in_file, AF_DEFAULT_TRACK);
  in_channels = afGetChannels(in_file, AF_DEFAULT_TRACK);
  in_rate = afGetRate (in_file, AF_DEFAULT_TRACK);
  afGetSampleFormat (in_file, AF_DEFAULT_TRACK, &in_format, &in_width);
  if (in_width == 8)
    out_bits = ESD_BITS8;
  else if (in_width == 16)
    out_bits = ESD_BITS16;
  else {
      g_warning ("only sample widths of 8 and 16 supported");
      return NULL;
  }

  bytes_per_frame = in_width / 8;

  if (in_channels == 1)
    out_channels = ESD_MONO;
  else if (in_channels == 2)
    out_channels = ESD_STEREO;
  else {
      g_warning ("only 1 or 2 channel samples supported");
      return NULL;
  }

  out_format = out_bits | out_channels | out_mode | out_func;

  out_rate = (int) in_rate;

  s = g_new0 (GnomeSoundSample, 1);

  s->rate = out_rate;
  s->format = out_format;
  s->samples = frame_count;
  s->data = g_malloc(frame_count * in_channels * bytes_per_frame);
  s->id = 0;

  frames_read = afReadFrames(in_file, AF_DEFAULT_TRACK, s->data,
			     frame_count * in_channels);

  afCloseFile(in_file);

  return s;
}
#endif


/**
 * gnome_sound_sample_load:
 * @sample_name: The name of the sample.
 * @filename: The filename where the audio is stored.
 *
 * Loads the audio from @filename and load it into the esd cache for later
 * playing. Programs will rarely want to call this function directly. Use
 * gnome_sound_play() instead for fire and forget sound playing.
 *
 * Returns: The esound sample_id or %-1 if the sample was unable to be cached
 * for esound.
 */
int
gnome_sound_sample_load(const char *sample_name, const char *filename)
{
#ifdef HAVE_ESD
  GnomeSoundSample *s = NULL;
  int sample_id;
  int size;
  int confirm = 0;

  if (!use_sound ())
    return -2;

  if(!filename || !*filename)
    return -2;

#ifdef HAVE_LIBAUDIOFILE
  s = gnome_sound_sample_load_audiofile(filename);
#else
  s = gnome_sound_sample_load_wav(filename);
#endif
  if(s)
    goto playsamp;

  /* XXX: Add handlers for more formats here */

 playsamp:
  if (!s)
    return -1;

  size = s->samples;
  if (s->format & ESD_STEREO)
    size *= 2;
  if (s->format & ESD_BITS16)
    size *= 2;

  if (gnome_sound_connection >= 0)
    {
      if (s->data)
	{
	  /* "name" of all samples is currently "E", should be name of sound 
	   * file, or event type, for later identification */
	  s->id = esd_sample_cache (gnome_sound_connection, s->format, s->rate,
				    size, (char *)sample_name);
	  write (gnome_sound_connection, s->data, size);
	  confirm = esd_confirm_sample_cache (gnome_sound_connection);
	  if (s->id <= 0 || confirm != s->id)
	    {
	      g_warning ("error caching sample <%d>!\n", s->id);
	      s->id = 0;
	    }
	  g_free (s->data);
	  s->data = NULL;
	}
    }

  sample_id = s->id;

  g_free(s->data); g_free(s);

  return sample_id;
#else
  return -1;
#endif
}

/**
 * gnome_sound_play:
 * @filename: File containing the sound sample.
 *
 * Plays the audio stored in @filename, if possible. Fail quietly if playing is
 * not possible (due to missing sound support or for other reasons).
 */
void 
gnome_sound_play (const char * filename)
{
#ifdef HAVE_ESD
  char buf[256];
  int sample;
  static guint cookie = 0;

  if(!use_sound ())
    return;

  if (cookie == 0)
	  cookie = rand ();

  g_snprintf(buf, sizeof(buf), "%d-%u-%d",
	     getpid(), cookie++, rand ());
 
  /* overflow, make sure we don't reinit with rand again */
  if (cookie == 0)
	  cookie = 1;

  sample = gnome_sound_sample_load (buf, filename);

  esd_sample_play(gnome_sound_connection, sample);
  fsync (gnome_sound_connection);
  esd_sample_free(gnome_sound_connection, sample);
#endif
}

/**
 * gnome_sound_init:
 * @hostname: Hostname where esd daemon resides.
 *
 * Initialize the esd connection.
 */
void
gnome_sound_init(const char *hostname)
{
#ifdef HAVE_ESD
	srand(time(NULL));
	g_free (esound_hostname);
	if (hostname)
		esound_hostname = g_strdup (hostname);
	else
		esound_hostname_null_ok = TRUE;
#endif
}

/** 
 * gnome_sound_shutdown:
 *
 * Shuts down the gnome sound support.
 */
void
gnome_sound_shutdown(void)
{
#ifdef HAVE_ESD
	g_free (esound_hostname);
	esound_hostname = NULL;
	if(gnome_sound_connection >= 0){
		esd_close(gnome_sound_connection);
		gnome_sound_connection = -1;
	}
#endif
}

/** 
 * gnome_sound_connection_get:
 *
 * Rarely needed to by programs directly, this function may be useful if a
 * program has cached a sample with gnome_sound_sample_load() and now wishes to
 * call esd_sample_play() to play the sample.
 *
 * Returns: the file descriptor of our esound connection or %-1
 * on error.
 **/
int
gnome_sound_connection_get (void)
{
#ifdef HAVE_ESD
	if ( ! use_sound ())
		return -1;
	return gnome_sound_connection;
#else
	return -1;
#endif
}
