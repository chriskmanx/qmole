/* $Id: stroke.c 2064 2010-03-12 13:15:36Z tpgww $

libstroke2 - a pointer-device gesture-management library for *NIX
Derived from libstroke 0.5.1 @ www.etla.net/libstroke
Copyright (C) 2008-2010 tooar <tooar@emelfm2.net>
Portions copyright (C) 1996-1999  Mark F. Willey, ETLA Technical

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this file. If not, see http://www.gnu.org/licenses.
*/

/* This code is independent of glib and gtk, though the documentation is mostly
   formatted for gtk-doc */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef __USE_UNIX98
/* get stuff for recursive mutex initialization */
# define __USE_UNIX98
#endif
#include <pthread.h>

/* some libc's don't support obstacks e.g.
  MacOS X 10.3, FreeBSD 6.0, NetBSD 3.0, OpenBSD 3.8, AIX 5.1, HP-UX 11,
  IRIX 6.5, OSF/1 5.1, Solaris 10, Cygwin, mingw, Interix 3.5 */
/* and maybe code is smaller with local allocations for points data ? */
/*#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
# ifdef HAVE_OBSTACK
#  undef HAVE_OBSTACK
# endif
#else
# define HAVE_OBSTACK
#endif
*/
#ifdef HAVE_OBSTACK
# include <obstack.h>
#endif
#include <math.h>

#include "stroke.h"

#ifdef EMBEDDED
# include "emelfm2.h"
#endif

#if ! defined (EMBEDDED) || defined (E2_PTRGESTURES)

/* The various #defines here and in the corresponsing .c file are not intended
   for build-time configuration */

/* no. of bins on each grid axis, a.k.a gesture grid granularity. Best if odd,
   allowed 2..15, normally 3 or 5 */
#define STROKE_AXIS_BINS 3

/* minimum ratio of a gesture's axes for the smaller axis to be 'centred'
   For STROKE_AXIS_BINS < 4, best if == STROKE_AXIS_BINS + 1,
   otherwise STROKE_AXIS_BINS */
//#define STROKE_SCALE_RATIO STROKE_AXIS_BINS
#define STROKE_SCALE_RATIO 4

/* smallest acceptable no. of pixels for an axis of the grid */
#define STROKE_AXIS_MIN 25

/* a value > maximum feasible reportable (x or y) position, for ensuring the
   outcome of some initial relativity-tests */
#define STROKE_BIG_VALUE 10000

/* minimum number of sample points required for a valid gesture with 'big' bins */
#define STROKE_MIN_POINTS 30

/* maximum number of points that can be logged for a gesture */
#define STROKE_MAX_POINTS 10000

/* minimum percentage of points in a bin needed to add the bin to the sequence */
#define STROKE_BIN_COUNT_PERCENT 0.07

/* include some debug messages */
//see Makefile #define STROKE_DEBUG

#ifdef HAVE_OBSTACK
# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free free
#endif

#define STROKE_WARNING "<libgstroke2> WARNING: "
#define STROKE_MEMORY_MESSAGE "virtual memory exhausted"
#define STROKE_MEMORY_ERROR fprintf (stderr, STROKE_WARNING STROKE_MEMORY_MESSAGE"\n");

#ifndef FALSE
# define FALSE 0
# define TRUE 1
#endif

/* structure for holding position data in linked list */
typedef struct _point
{
	int x;
	int y;
	struct _point *next;
} point;

/* 2^n chunk size for about 340 points @ 12 bytes each */
#define PCHUNK_SIZE 4096

#ifndef HAVE_OBSTACK
/* each block of points is alolocated heapspace with pointer to next chunk at
   offset 0, followed by up to POINTS_PER_CHUNK points data items */
# define POINTS_PER_CHUNK ((PCHUNK_SIZE - sizeof (struct _chunk *)) / sizeof (struct _point))

typedef struct _pchunk
{
	point points [POINTS_PER_CHUNK];
	struct _pchunk *next;
	/* in the allocated space (PCHUNK_SIZE), there may be a few spare trailing bytes */
} pchunk;

typedef struct _pchunkdata
{
	pchunk *firstchunk;		/* first-allocated chunk in linked list of chunks */
	pchunk *currentchunk;	/* last-allocated chunk in the linked list */
	point *next_point;		/* next available place for point data in last-allocated
								chunk (often == tail->next) */
	unsigned int unusedpoints; /* no. of places left in current chunk */
} pchunkdata;

#endif

/* metrics for monitored gesture */
#if STROKE_MAX_DEVICES > 1
# define DEVICE_METRIC device_data[count].
typedef struct _stroke_metrics
{
	int min_x;
	int min_y;
	int max_x;
	int max_y;
	int points_count;
	point *points_head;
	point *points_tail;
#ifdef HAVE_OBSTACK
	struct obstack pointstack;
#else
	pchunkdata pointchunks;
#endif
	pthread_mutex_t lock;
} stroke_metrics;
#else
# define DEVICE_METRIC
#endif

struct _Stroke
{
#if STROKE_MAX_DEVICES > 1
	void *devices [STROKE_MAX_DEVICES];
	stroke_metrics device_data [STROKE_MAX_DEVICES];
#else
	/* stroke boundaries, min & max horiz & vert recorded values */
	int min_x;
	int min_y;
	int max_x;
	int max_y;
	/* no. of listed points (some of them interpolated between reports) */
	int points_count;
	point *points_head; /* first point in linked list */
	point *points_tail; /* last point in linked list */
#ifdef HAVE_OBSTACK
	struct obstack pointstack;  /* data for block-allocation of points for list */
#else
	pchunkdata pointchunks;
#endif
	pthread_mutex_t lock; /* thread-protection */
#endif
	int scale_x;	/* scale factors common to all devices */
	int scale_y;
	int max_bin;	/* size-limit for reported sequences, 0 = no limit */
};

static const int bincount = STROKE_AXIS_BINS * STROKE_AXIS_BINS;

#ifdef HAVE_OBSTACK
# ifndef xmalloc
/*
 * xmalloc:
 * @size: the number of bytes to allocate
 *
 * For obstack allocation - the allocator must not return after a failed
 * allocation attempt
 *
 * Returns: newly-allocated heap-space, or doesn't return at all
 */
void *xmalloc (size_t size)
{
	register void *value = malloc (size);
	if (value != NULL)
		return value;
	fprintf (stderr, "<libgstroke2> FATAL: "STROKE_MEMORY_MESSAGE"\n");
	exit (127);
}
# endif
#endif

#ifndef HAVE_OBSTACK
/*
 * _stroke_clear_chunks:
 * @pointchunks: pointer to data struct
 * Cleanup allocations for points data
 */
static void _stroke_clear_chunks (pchunkdata *allchunks)
{
	pchunk *this;
	pchunk *next;
	for (this = allchunks->firstchunk; this != NULL; this = next)
	{
		next = this->next;
		free (this);
	}
/* not needed when destroying
	allchunks->pointchunks = NULL;
	allchunks->currentchunk = NULL;
	allchunks->next_point = NULL;
	allchunks->unusedpoints = 0;
*/
}

/*
 * _stroke_freshen_chunks:
 * @pointchunks: pointer to data struct
 * Re-initialise pointers for allocation(s) for points data
 */
inline static void _stroke_freshen_chunks (pchunkdata *allchunks)
{
	if (allchunks->firstchunk != NULL)
	{
		allchunks->currentchunk = allchunks->firstchunk;
		allchunks->next_point = allchunks->firstchunk->points;
		allchunks->unusedpoints = POINTS_PER_CHUNK;
	}
}

/*
 * _stroke_allocate_point:
 * @pointchunks: pointer to data struct
 *
 * Returns a point's data struct or NULL upon failure
 */
static point *_stroke_allocate_point (pchunkdata *allchunks)
{
	if (allchunks->unusedpoints > 0)
	{
		point *point_p;
		allchunks->unusedpoints--;
		point_p = allchunks->next_point;
		allchunks->next_point++;
		return point_p;
	}
	else /* no space left in current chunk */
		if (allchunks->currentchunk != NULL && allchunks->currentchunk->next != NULL)
	{
		pchunk *next;
		next = allchunks->currentchunk->next;
		allchunks->currentchunk = next;
		allchunks->unusedpoints = POINTS_PER_CHUNK - 1;
		allchunks->next_point = &next->points[1];
		return next->points;
	}
	else /* no more allocated space */
	{
		pchunk *extra;
		extra = malloc (PCHUNK_SIZE);
		if (extra != NULL)
		{
			extra->next = NULL;
			if (allchunks->firstchunk == NULL) /* is this the first chunk ? */
				allchunks->firstchunk = extra;
			else
				allchunks->currentchunk->next = extra; /* link it */
			allchunks->currentchunk = extra;
			allchunks->unusedpoints = POINTS_PER_CHUNK - 1;
			allchunks->next_point = &extra->points[1];
			return extra->points;
		}
		else
		{
			STROKE_MEMORY_ERROR;
			return NULL;
		}
	}
}
#endif

/*
 * _stroke_init_data:
 * @xmax: initial horizontal px-count
 * @ymax: initial vertical px-count
 *
 * Allocate and initialise a #Stroke data struct
 *
 * Returns the data struct or NULL upon failure
 */
static Stroke *_stroke_init_data (int xmax, int ymax)
{
	Stroke *handle = calloc (1, sizeof (Stroke));
	if (handle != NULL)
	{
		pthread_mutexattr_t attr;
		pthread_mutexattr_init (&attr);
		pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
#if STROKE_MAX_DEVICES > 1
		int count;
		for (count = 0; count < STROKE_MAX_DEVICES; count++)
		{
#endif
#ifdef HAVE_OBSTACK
			obstack_init (&handle->DEVICE_METRIC pointstack);
			obstack_chunk_size (&handle->DEVICE_METRIC pointstack) = PCHUNK_SIZE;
#endif
			pthread_mutex_init (&handle->DEVICE_METRIC lock, &attr);
#if STROKE_MAX_DEVICES > 1
		}
#endif
		pthread_mutexattr_destroy (&attr);
		/* ignore stupidly-small and default (-1) values */
		handle->scale_x = (xmax < STROKE_AXIS_MIN) ? 100 : xmax;
		handle->scale_y = (ymax < STROKE_AXIS_MIN) ? 100 : ymax;
	}
	return handle;
}

/*
 * _stroke_bin:
 * @ point: struct with data for a logged pointer-position
 * @ bin_right: an array of horizontal px-values for comparison with data in @point
 * @ bin_bottom: an array of vertical px-values for comparison with data in @point
 *
 * Figure out which bin @point belongs to, 1 = bot.left .. bincount = top.right
 * Each member of bin_right[] may have same value. Ditto bin_bottom[]
 *
 * Returns the relevant bin-number
 */
static int _stroke_bin (point *point_p,
   int bin_right[STROKE_AXIS_BINS], int bin_bottom[STROKE_AXIS_BINS])
{
	/* bigger vertical positions are at bottom, hence bigger bin numbers */
	int bin_num = 1; //this for reversed order = bincount - STROKE_AXIS_BINS + 1;
	int count, previous = -STROKE_BIG_VALUE; /* ensure the first loop works */
	register int this;
	for (count = 0; count < STROKE_AXIS_BINS; count++)
	{
		this = bin_right[count];
		if (point_p->x > this && this > previous)
		{
			previous = this;
			bin_num += 1;
		}
		else break;
	}
//	if (bin_num > bincount)
//		bin_num = bincount;
	previous = -STROKE_BIG_VALUE;
	for (count = 0; count < STROKE_AXIS_BINS; count++)
	{
		this = bin_bottom[count];
		if (point_p->y > this && this > previous)
		{
			previous = this;
			bin_num += STROKE_AXIS_BINS; //or for reversed order .. -= STROKE_AXIS_BINS;
		}
		else break;
	}
//	if (bin_num < 1)
//		bin_num += STROKE_AXIS_BINS;
//	if (bin_num > bincount)
//		bin_num -= STROKE_AXIS_BINS;
	return bin_num;
}

/*
 * _stroke_bin_centre:
 * @handle:
 * @binnum: bin-grid number 1 .. STROKE_AXIS_BINS^2
 * @x: store for horizontal px-value
 * @y: store for vertical px-value
 *
 * Determine the centre position of bin @binnum
 *
 * Returns TRUE unless some problem occurred
 */
static int _stroke_bin_centre (Stroke *handle, int binnum, int *x, int *y)
{
	int bin_x, bin_y;
	int xpos, ypos;

	if (handle->scale_x == 0 || handle->scale_y == 0)
		return FALSE;

	bin_y = handle->scale_y / STROKE_AXIS_BINS;
	ypos = bin_y / 2;
	for ( ; binnum > STROKE_AXIS_BINS; binnum -= STROKE_AXIS_BINS)
		ypos += bin_y;

	bin_x = handle->scale_x / STROKE_AXIS_BINS;
	xpos = bin_x / 2;
	for ( ; binnum > 1; binnum--)
		xpos += bin_x;

	*x = xpos;
	*y = ypos;
	return TRUE;
}

/*
 * _stroke_bins_adjacent:
 * @bin1: the 'reference' bin-grid number, 1 .. STROKE_AXIS_BINS^2
 * @bin2: bin-grid number, 1 .. STROKE_AXIS_BINS^2, to be checked agains @bin1
 *
 * Determine whether @bin2 is adjacent to @bin1 in the grid
 *
 * Returns TRUE if the bins are adjacent (and both numbers are in-range)
 */
static int _stroke_bins_adjacent (int bin1, int bin2)
{
	if (bin1 == bin2)
		return TRUE;
	else if (bin1 < 1 || bin1 > bincount || bin2 < 1 || bin2 > bincount) /* should never happen */
		return FALSE;

	int adjusted;
	for (adjusted = bin1; adjusted > STROKE_AXIS_BINS; adjusted -= STROKE_AXIS_BINS);
	/* some tests are redundant but harmless for top or bottom rows */
	if (adjusted == 1) /* generic left-side*/
	{
		return (
			bin2 == bin1 + 1 ||
			bin2 == bin1 + STROKE_AXIS_BINS ||
			bin2 == bin1 + STROKE_AXIS_BINS + 1 ||
			bin2 == bin1 - STROKE_AXIS_BINS ||
			bin2 == bin1 - STROKE_AXIS_BINS + 1
		);
	}
	else if (adjusted < STROKE_AXIS_BINS) /* generic middle */
	{
		return (
			bin2 == bin1 - 1 ||
			bin2 == bin1 + 1 ||
			bin2 == bin1 + STROKE_AXIS_BINS ||
			bin2 == bin1 + STROKE_AXIS_BINS - 1 ||
			bin2 == bin1 + STROKE_AXIS_BINS + 1 ||
			bin2 == bin1 - STROKE_AXIS_BINS ||
			bin2 == bin1 - STROKE_AXIS_BINS - 1 ||
			bin2 == bin1 - STROKE_AXIS_BINS + 1
		);
	}
	else /* generic right-side */
	{
		return (
			bin2 == bin1 - 1 ||
			bin2 == bin1 + STROKE_AXIS_BINS ||
			bin2 == bin1 + STROKE_AXIS_BINS - 1 ||
			bin2 == bin1 - STROKE_AXIS_BINS ||
			bin2 == bin1 - STROKE_AXIS_BINS - 1
		);
	}
}

/*
 * _stroke_bins_adjacent_vector:
 * @bin1: bin-grid number of direction-finder bin
 * @bin2: the 'reference' bin-grid number, 1 .. STROKE_AXIS_BINS^2
 * @bin3: bin-grid number, 1 .. STROKE_AXIS_BINS^2, to be checked agains @bin2
 *
 * Determine whether @bin3 is adjacent to @bin2 in the grid, and on the same
 * 'direction-line' as applies between @bin1 and @bin2
 *
 * Returns TRUE if the bins are adjacent (and both numbers are in-range)
 */
static int _stroke_bins_adjacent_vector (int bin1, int bin2, int bin3)
{
	int delx1, dely1, delx2, dely2;
	int faketan1, faketan2;
	float tan1, tan2;

	if (!_stroke_bins_adjacent (bin2, bin3) || bin1 == bin2)
		return FALSE;
	else if (bin2 == bin3)
		return TRUE;
	/* reference direction */
	delx1 = (bin2 - 1) % STROKE_AXIS_BINS - (bin1 - 1) % STROKE_AXIS_BINS;
	if (delx1 == 0)
	{
		faketan1 = TRUE;
		tan1 = 1.0; tan2 = 2.0;
	}
	else
	{
		faketan1 = FALSE;
		dely1 = (bin2 - 1) / STROKE_AXIS_BINS - (bin1 - 1) / STROKE_AXIS_BINS;
		tan1 = (float) dely1 / delx1;
	}
	/* test direction */
	delx2 = (bin3 - 1) % STROKE_AXIS_BINS - (bin2 - 1) % STROKE_AXIS_BINS;
	if (delx2 == 0)
	{
		faketan2 = TRUE;
		tan1 = 1.0; tan2 = 2.0;
	}
	else
	{
		faketan2 = FALSE;
		dely2 = (bin3 - 1) / STROKE_AXIS_BINS - (bin2 - 1) / STROKE_AXIS_BINS;
		tan2 = (float) dely2 / delx2;
	}
	return ((faketan1 && faketan2) ||
			(tan2 > (tan1 - 0.01) && tan2 < (tan1 + 0.01)));
}

/* populate an about-face turn, (properly) assumes array size >= 3 */
static void _stroke_switchback (char *bins)
{
	switch (bins[0])
	{
		case 1: /* top left */
			bins[1] = bincount;
			bins[2] = 1;
			break;
		case STROKE_AXIS_BINS: /* top right */
			bins[1] = bincount - STROKE_AXIS_BINS + 1;
			bins[2] = STROKE_AXIS_BINS;
			break;
		case STROKE_AXIS_BINS * STROKE_AXIS_BINS: /* bottom right */
			bins[1] = 1;
			bins[2] = bincount;
			break;
		case STROKE_AXIS_BINS * STROKE_AXIS_BINS - STROKE_AXIS_BINS + 1: /* bottom left */
			bins[1] = STROKE_AXIS_BINS;
			bins[2] = bins[0];
			break;
		default:
		{
			int line;
			line = (bins[0] - 1) / STROKE_AXIS_BINS;
			if (line == 0)	/* top row */
			{
				bins[0] = STROKE_AXIS_BINS / 2 + 1; /* middle of an odd-sized grid */
				bins[1] = bincount - STROKE_AXIS_BINS / 2;
			}
			else if (line == STROKE_AXIS_BINS - 1) /* bottom row */
			{
				bins[0] = bincount - STROKE_AXIS_BINS / 2;/* middle of an odd-sized grid */
				bins[1] = STROKE_AXIS_BINS / 2 + 1;
			}
			else
			{
				line = (bins[0] - 1) % STROKE_AXIS_BINS;
				if (line == 0)	/* left column */
				{
					bins[0] = STROKE_AXIS_BINS * (STROKE_AXIS_BINS / 2) + 1; /* middle of an odd-sized grid */
					bins[1] = bins[0] + STROKE_AXIS_BINS - 1;
				}
				else	/* default */
				{
					bins[0] = STROKE_AXIS_BINS * (STROKE_AXIS_BINS / 2 + 1); /* middle of an odd-sized grid */
					bins[1] = bins[0] - STROKE_AXIS_BINS + 1;
				}
			}
			bins[2] = bins[0];
		}

		break;
	}
}

/* populate a V-turn, (properly) assumes array size >= 3 */
static void _stroke_arrow (char *bins)
{
	int row1, row2, row3;
	int col1, col2, col3;
	row1 = (bins[0] - 1) / STROKE_AXIS_BINS;
	row2 = (bins[1] - 1) / STROKE_AXIS_BINS;
	row3 = (bins[2] - 1) / STROKE_AXIS_BINS;
	col1 = (bins[0] - 1) % STROKE_AXIS_BINS;
	col2 = (bins[1] - 1) % STROKE_AXIS_BINS;
	col3 = (bins[2] - 1) % STROKE_AXIS_BINS;

/*	if ((row1 > row2 && row2 > row3) / * points horizontally * /
	 || (row1 < row2 && row2 < row3))
	{
*/
		if (col1 > col2 && col3 > col2)	/* points left */
		{
			bins[0] = STROKE_AXIS_BINS;
			bins[1] = STROKE_AXIS_BINS * (STROKE_AXIS_BINS / 2) + 1;
			bins[2] = bincount;
		}
		else if (col1 < col2 && col3 < col2) /* points right */
		{
			bins[0] = 1;
			bins[1] = STROKE_AXIS_BINS * (STROKE_AXIS_BINS / 2 + 1);
			bins[2] = bincount - STROKE_AXIS_BINS + 1;
		}
/*	}
	else if ((col1 > col2 && col2 > col3) / * vertical * /
			|| (col1 < col2 && col2 < col3))
	{
*/
	else
		if (row1 > row2 && row3 > row2)	/* points up */
		{
			bins[0] = bincount - STROKE_AXIS_BINS + 1;
			bins[1] = STROKE_AXIS_BINS / 2 + 1;
			bins[2] = bincount;
		}
		else if (row1 < row2 && row3 < row2) /* points down */
		{
			bins[0] = 1;
			bins[1] = bincount - STROKE_AXIS_BINS / 2;
			bins[2] = STROKE_AXIS_BINS;
		}
//	}
}

/* populate a square, (improperly FIXME) assumes array size >= 5 */
static void _stroke_square (char *bins)
{
		bins[0] = bins[4] = 1;
		bins[1] = STROKE_AXIS_BINS;
		bins[2] = bincount;
		bins[3] = bincount - STROKE_AXIS_BINS + 1;
}
	/* CHECKME walk again to check for >=1 bin in each of T/B row, L/R column ? */
/*	int minrow = STROKE_AXIS_BINS, maxrow = 0, mincol = STROKE_AXIS_BINS, maxcol = 0;
	int x1 = 0, y1 = 0, x2 = 0, y2 = 0;
	int col, row;
	for (i = 1; i < count; i++)
	{
		row = (bins[i] - 1) / STROKE_AXIS_BINS;
		col = (bins[i] - 1) % STROKE_AXIS_BINS;
		if (row < minrow)
		{
			minrow = row;
			y1 = i;
		}
		if (col < mincol)
		{
			mincol = col;
			x1 = i;
		}
		if (row > maxrow)
		{
			maxrow = row;
			y2 = i;
		}
		if (col > maxcol)
		{
			maxcol = col;
			x2 = i;
		}
	}

	if (minrow > 0)
	{
		row = (bins[y1] - 1) / STROKE_AXIS_BINS;
		while (row-- > 0)
			bins[y1] -= STROKE_AXIS_BINS;
	}
	if (mincol > 0)
	{
		col = (bins[x1] - 1) % STROKE_AXIS_BINS;
		while (col-- > 0)
			bins[x1]--;
	}
	if (maxrow < STROKE_AXIS_BINS - 1)
	{
		row = (bins[y2] - 1) / STROKE_AXIS_BINS;
		while (row++ < STROKE_AXIS_BINS - 1)
			bins[y2] += STROKE_AXIS_BINS;
	}
	if (maxcol < STROKE_AXIS_BINS - 1)
	{
		col = (bins[x2] - 1) % STROKE_AXIS_BINS;
		while (col++ < STROKE_AXIS_BINS - 1)
			bins[x2]++;
	}
*/

/*
 * _stroke_simplify:
 * bins: array of grid-bin-numbers, not 0-terminated
 * n_bins: no. of members in bins
 * thresh: minimum acceptable distance between adjacent points
 *
 * Eliminate from array @bins all members which are closer than @thresh to
 * each other, and then handle various resultant special-cases
 *
 * Returns the number of members in the cleaned array
 */
static int _stroke_simplify (char *bins, int n_bins, int thresh)
{
	int start, count, limit, i;
#if STROKE_AXIS_BINS >= 5
	int delx, dely, dst;
#endif

	/* if there's possibly something to remove, do a vertex-reduction pass */
	if (n_bins > 2)
	{
		start = 0;
		count = 1;			/* we never remove the first point */
		limit = n_bins - 1; /* or handle the last one in-loop */

#if STROKE_AXIS_BINS >= 5
		thresh *= thresh;
#endif

		for (i = 1; i < limit; i++)
		{
#if STROKE_AXIS_BINS < 5
			/* can't validly compare 1^2 with anything */
			if (!_stroke_bins_adjacent (bins[start], bins[i]))
#else
			delx = (bins[i] - 1) % STROKE_AXIS_BINS - (bins[start] - 1) % STROKE_AXIS_BINS;
			dely = (bins[i] - 1) / STROKE_AXIS_BINS - (bins[start] - 1) / STROKE_AXIS_BINS;
			dst = delx * delx + dely * dely;
			if (dst > thresh)
#endif
			{
				bins[count++] = bins[i];
				start = i;
			}
			/* CHECKME can omission reduce the outer-dimension(s) of the stroke ? */
		}

#if STROKE_AXIS_BINS < 5
		if (!_stroke_bins_adjacent (bins[start], bins[limit]))
#else
		delx = (bins[limit] - 1) % STROKE_AXIS_BINS - (bins[start] - 1) % STROKE_AXIS_BINS;
		dely = (bins[limit] - 1) / STROKE_AXIS_BINS - (bins[start] - 1) / STROKE_AXIS_BINS;
		dst = delx * delx + dely * dely;

		if (dst > thresh)
#endif
			bins[count++] = bins[limit];
		else
			bins[count-1] = bins[limit];
	}
	else
		count = n_bins;

	/* handle special-cases */
	switch (count)
	{
		case 1: /* should never happen, always get start and end */
			fprintf (stderr, "single member in cleaned bins array \n");
			break;
		case 2:
			if (bins[0] == bins[1]) /* back to starting bin in 2 straight lines */
				count = 3;  /* FIXME n_bins may be 2, though more space was allocated */
			else			/* one straight line */
				count = 2;
			_stroke_switchback (bins);
			break;
		default:
				/* 'close' nearly-closed shapes */
			if (_stroke_bins_adjacent (bins[0], bins[count-1]))
				bins[count-1] = bins[0];
			{
				switch (count)
				{
					case 3: /* arrow */
						if (bins[2] == bins[0]) /* closed up now */
							_stroke_switchback (bins);
						else
							_stroke_arrow (bins);
						break;
					case 4:
//						if (n_bins < 5)	/* FIXME stroke square needs 5 bins*/
//							break;
					case 5:
						if (bins[0] == bins[count - 1])
						{
							_stroke_square (bins);
							count = 5;
						}
						break;
					default:
						break;
				}
			}
			break;
	}

	return count;
}

/**
 * stroke_new:
 * @xmax: initial horizontal px-count for the data
 * @ymax: initial vertical px-count for the data
 *
 * Initialise a new stroke handle. The px-counts will be used later if any
 * data-scaling is requested. Either px-count, if < STROKE_AXIS_MIN (e.g. -1),
 * is set to default (100).
 *
 * Returns pointer to #Stroke data struct, or NULL upon some setup error
 **/
Stroke *stroke_new (int xmax, int ymax)
{
	return (_stroke_init_data (xmax, ymax));
}

/**
 * stroke_destroy:
 * @handle: pointer to #Stroke data struct
 *
 * Cleanup infrastructure and allocated memory when finished with @handle
 **/
void stroke_destroy (Stroke *handle)
{
#if STROKE_MAX_DEVICES > 1
	int count;
	for (count = 0; count < STROKE_MAX_DEVICES; count ++)
	{
#endif
#ifdef HAVE_OBSTACK
		if (handle->DEVICE_METRIC points_count > 0)
			obstack_free (&handle->DEVICE_METRIC pointstack, NULL);
#else
		_stroke_clear_chunks (&handle->DEVICE_METRIC pointchunks);
#endif
		pthread_mutex_destroy (&handle->DEVICE_METRIC lock);
#if STROKE_MAX_DEVICES > 1
	}
#endif
	free (handle);
}

/**
 * stroke_limit:
 * @handle: pointer to #Stroke data struct
 * @binmax: maximum length of bin-sequences
 *
 * Set maximum no. of 'grid-bins' (not necessarily bytes) included in any future
 * sequence translation for @handle
 * 0 (= default) for no limit
 **/
void stroke_limit (Stroke *handle, int binmax)
{
	handle->max_bin = binmax;
}

/**
 * stroke_get_limit:
 * @handle: pointer to #Stroke data struct
 *
 * Returns current value of length-limit on bin-sequences for @handle
 **/
int stroke_get_limit (Stroke *handle)
{
	return handle->max_bin;
}

/**
 * stroke_get_count:
 * @handle: pointer to #Stroke data struct
 *
 * Get no. of recorded data points from @handle
 *
 * Returns the number
 **/
int stroke_get_count (Stroke *handle
#if STROKE_MAX_DEVICES > 1
  , void *device
#endif
)
{
#if STROKE_MAX_DEVICES > 1
	int count;
	for (count = 0; count < STROKE_MAX_DEVICES; count++)
	{
		if (handle->devices[count] == device)
			break;
	}
	if (count == STROKE_MAX_DEVICES)
	{
		/* should not be here ! */
		return;
	}
#endif
	int pointscount;

	pthread_mutex_lock (&handle->DEVICE_METRIC lock);
	pointscount = handle->DEVICE_METRIC points_count;
	pthread_mutex_unlock (&handle->DEVICE_METRIC lock);

	return pointscount;
}

/**
 * stroke_clear:
 * @handle: pointer to #Stroke data struct
 *
 * Clear all points data from @handle, ready for a fresh start
 **/
void stroke_clear (Stroke *handle
#if STROKE_MAX_DEVICES > 1
  , void *device
#endif
)
{
#if STROKE_MAX_DEVICES > 1
	int count;
	for (count = 0; count < STROKE_MAX_DEVICES; count++)
	{
		if (handle->devices[count] == device)
			break;
	}
	if (count == STROKE_MAX_DEVICES)
	{
		/* should not be here ! */
		return;
	}
#endif
	pthread_mutex_lock (&handle->DEVICE_METRIC lock);
	if (handle->DEVICE_METRIC points_head != NULL)
	{
#ifdef HAVE_OBSTACK
		obstack_free (&handle->DEVICE_METRIC pointstack, handle->DEVICE_METRIC points_head);
#else
		_stroke_freshen_chunks (&handle->DEVICE_METRIC pointchunks);
#endif
		handle->DEVICE_METRIC points_head = NULL;
	}
	handle->DEVICE_METRIC points_tail = NULL;;
	handle->DEVICE_METRIC points_count = 0;
	pthread_mutex_unlock (&handle->DEVICE_METRIC lock);
}

/**
 * stroke_scale:
 * @handle: pointer to #Stroke data struct
 * @xmax: new horizontal px-count for the data
 * @ymax: new vertical px-count for the data
 *
 * Scale all stored position data to reflect @xmax, @ymax.
 **/
void stroke_scale (Stroke *handle, int xmax, int ymax)
{
#if STROKE_MAX_DEVICES > 1
	int count;
#endif
	int oldx = handle->scale_x;
	int oldy = handle->scale_y;
	handle->scale_x = (xmax < STROKE_AXIS_MIN) ? STROKE_AXIS_MIN : xmax;
	handle->scale_y = (ymax < STROKE_AXIS_MIN) ? STROKE_AXIS_MIN : ymax;
	int newx = (oldx != handle->scale_x);
	int newy = (oldy != handle->scale_y);
	if (newx || newy)
	{
		point *point_p;
		float xscale = (oldx == 0) ? 1.0 : xmax / oldx; /* insurance */
		float yscale = (oldy == 0) ? 1.0 : ymax / oldy;
		pthread_mutex_lock (&handle->DEVICE_METRIC lock);
#if STROKE_MAX_DEVICES > 1
		for (count = 0; count < STROKE_MAX_DEVICES; count++)
		{
#endif
			for (point_p = handle->DEVICE_METRIC points_head; point_p != NULL;
				 point_p = point_p->next)
			{
				if (newx) point_p->x *= xscale;
				if (newy) point_p->y *= yscale;
			}
#if STROKE_MAX_DEVICES > 1
		}
#endif
		pthread_mutex_unlock (&handle->DEVICE_METRIC lock);
	}
}

/**
 * stroke_get_scale:
 * @handle: pointer to #Stroke data struct
 * @xmax: store for horizontal px-count in @handle, or NULL
 * @xmax: store for vertical px-count in @handle, or NULL
 *
 * Get current px-count(s).
 **/
void stroke_get_scale (Stroke *handle, int *xmax, int *ymax)
{
	if (xmax != NULL)
		*xmax = handle->scale_x;
	if (ymax != NULL)
		*ymax = handle->scale_y;
}

/**
 * stroke_translate:
 * @handle: pointer to #Stroke data struct
 * @clear: TRUE to cleanup processed data FALSE to leave data in place
 * @readable: TRUE to provide comma-separated ascii bin-numbers in sequence,
 *   FALSE to provide raw unsigned chars 1 .. STROKE_AXIS_BINS^2 (MAX 255)
 * @sequence = store for allocated result-string pointer
 *
 * Translate current points list in @handle to 0-terminated series of bin-numbers,
 * and, if @clean is TRUE, clean the processed-points data.
 * device = non-NULL pointer for distinction between points lists,
 *
 * Returns count of items in the returned string
 **/
int stroke_translate (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  int clear, int readable, char **sequence)
{
	char *results;
	point *point_p;
	size_t block, space;
	int min;
#if STROKE_MAX_DEVICES > 1
	int count;
	for (count = 0; count < STROKE_MAX_DEVICES; count++)
	{
		if (handle->devices[count] == device)
			break;
	}
	if (count == STROKE_MAX_DEVICES)
	{
		/* should not be here ! */
		return 0;
	}
#endif
	/* bail out on error cases */
	min =  (handle->scale_x < handle->scale_y) ? handle->scale_x : handle->scale_y;
	/* count > min min will be useless anyway, if all are inside a single bin */
	min = min / STROKE_AXIS_BINS / 2;
	if (min > STROKE_MIN_POINTS)
		min = STROKE_MIN_POINTS;

	if (handle->DEVICE_METRIC points_count < min)
	{
		if (handle->DEVICE_METRIC points_count > 0)
		{
#ifdef HAVE_OBSTACK
			/* clean all but keep the stack */
			point *head = handle->DEVICE_METRIC points_head;
			obstack_free (&handle->DEVICE_METRIC pointstack, head);
#else
			_stroke_freshen_chunks (&handle->DEVICE_METRIC pointchunks);
#endif
			handle->DEVICE_METRIC points_count = 0;
		}
		return 0;
	}

	/* initial allocation to hold results string, grown later if needed */
	block = (handle->max_bin == 0) ?
	((bincount / 4 + 1) * 8): /* approx. 2-passes per bin is typically enough */
	handle->max_bin;
	space = block;
	results = (char *) alloca (block);

	/* number of bins recorded in the stroke */
	int sequence_count = 0;

	/* points-->sequence translation scratch variables */
	int prev_bin = 0;
	int current_bin = 0;
	int bin_count = 0;

	/* bin boundary and size variables */
	int delta_x, delta_y, bin_x, bin_y, indx;
	int bin_right[STROKE_AXIS_BINS];
	int bin_bottom[STROKE_AXIS_BINS];

	/* determine extent of stroke hence the bins-grid size */
	delta_x = handle->DEVICE_METRIC max_x - handle->DEVICE_METRIC min_x;
	delta_y = handle->DEVICE_METRIC max_y - handle->DEVICE_METRIC min_y;
	/* and each bin's size */
	bin_x = delta_x / STROKE_AXIS_BINS;
	bin_y = delta_y / STROKE_AXIS_BINS;
	/* and bin boundary positions
	   shift them to effectively centre the points if the sroke is elongated
	   on either axis */
	if (STROKE_SCALE_RATIO * delta_y < delta_x)
	{ /* short wide stroke, shift bounds up */
		bin_bottom[0] =
		(handle->DEVICE_METRIC max_y + handle->DEVICE_METRIC min_y - delta_x) / 2
			+ bin_x;
		for (indx = 0; indx < STROKE_AXIS_BINS - 2; indx++)
			bin_bottom[indx+1] = bin_bottom[indx] + bin_x;
	}
	else
	{
		bin_bottom[0] = handle->DEVICE_METRIC min_y + bin_y;
		for (indx = 0; indx < STROKE_AXIS_BINS - 2; indx++)
			bin_bottom[indx+1] = bin_bottom[indx] + bin_y;
	}

	if (delta_y > STROKE_SCALE_RATIO * delta_x)
	{ /* tall thin stroke, shift bounds left */
		bin_right[0] =
		(handle->DEVICE_METRIC max_x + handle->DEVICE_METRIC min_x - delta_y) / 2
		+ bin_y;
		for (indx = 0; indx < STROKE_AXIS_BINS - 2; indx++)
			bin_right[indx+1] = bin_right[indx] + bin_y;
	}
	else
	{
		bin_right[0] = handle->DEVICE_METRIC min_x + bin_x;
		for (indx = 0; indx < STROKE_AXIS_BINS - 2; indx++)
			bin_right[indx+1] = bin_right[indx] + bin_x;
	}
	/* big values for last bins to capture all the rest */
	bin_right[STROKE_AXIS_BINS - 1] = STROKE_BIG_VALUE;
	bin_bottom[STROKE_AXIS_BINS - 1] = STROKE_BIG_VALUE;
/*
#ifdef STROKE_DEBUG
	printf ("DEBUG:: point count: %d\n",handle->DEVICE_METRIC points_count);
	printf ("DEBUG:: min_x: %d\n",handle->DEVICE_METRIC min_x);
	printf ("DEBUG:: max_x: %d\n",handle->DEVICE_METRIC max_x);
	printf ("DEBUG:: min_y: %d\n",handle->DEVICE_METRIC min_y);
	printf ("DEBUG:: max_y: %d\n",handle->DEVICE_METRIC max_y);
	printf ("DEBUG:: delta_x: %d\n",delta_x);
	printf ("DEBUG:: delta_y: %d\n",delta_y);
	printf ("DEBUG:: bin_right[0]: %d\n",bin_right[0]);
	printf ("DEBUG:: bin_right[1]: %d\n",bin_right[1]);
	printf ("DEBUG:: bin_right[2]: %d\n",bin_right[2]);
	printf ("DEBUG:: bin_bottom[0]: %d\n",bin_bottom[0]);
	printf ("DEBUG:: bin_bottom[1]: %d\n",bin_bottom[1]);
	printf ("DEBUG:: bin_bottom[2]: %d\n",bin_bottom[2]);
#endif
*/
	/*
	build string by placing points in bins, collapsing bins and
	discarding those with too few points...
	*/
	pthread_mutex_lock (&handle->DEVICE_METRIC lock);
	for (point_p = handle->DEVICE_METRIC points_head; point_p != NULL;
		 point_p = point_p->next)
	{
		/* figure out which bin the point falls in */
		current_bin = _stroke_bin (point_p, bin_right, bin_bottom);
		/* if this is the first point, consider it the previous bin, too. */
		if (prev_bin == 0)
			prev_bin = current_bin;
		/*
#ifdef STROKE_DEBUG
		printf ("DEBUG:: current bin: %d\n",current_bin);
#endif
		*/
		if (prev_bin == current_bin)
			bin_count++;
		else
		{  /* we are moving to a new bin - consider adding to the sequence */
			if (bin_count > (handle->DEVICE_METRIC points_count * STROKE_BIN_COUNT_PERCENT)
				|| sequence_count == 0 /* first bin is always included */
			)
			{
				if (sequence_count > 1
					&& _stroke_bins_adjacent_vector (
							results[sequence_count - 2],
							results[sequence_count - 1],
							prev_bin))
				{
#ifdef EMBEDDED
					printd (DEBUG, "former bin %d ignored in favour of %d",
					results[sequence_count - 1], prev_bin);
#endif
					/* simplify stroke by ignoring 1-bin increments */
					results[sequence_count - 1] = prev_bin;
				}
				else
				{
					results[sequence_count++] = prev_bin;
					/*
#ifdef STROKE_DEBUG
					printf ("DEBUG:: adding sequence: %d\n",prev_bin);
#endif
					*/
					if (sequence_count == handle->max_bin && handle->max_bin > 0)
					{
						sequence_count--;
						break;
					}
					else if (sequence_count == space)	/* no overflow */
					{
						char *tmp = alloca (block + space);
						memcpy (tmp, results, space);
						results = tmp;
						space += block;
					}
				}
			}

			/* restart counting points in the new bin */
			bin_count = 0;
			prev_bin = current_bin;
		}
		/* CHECKME ever need to preserve a 'tail' of points ? then must remember
		where we get to */
	}

	if (handle->max_bin == 0 || sequence_count < handle->max_bin - 1)
	{
		/* if appropriate, add the last run of points to the sequence */
		/*
#ifdef STROKE_DEBUG
		printf ("DEBUG:: adding final sequence: %d\n",current_bin);
#endif
		*/
		if (sequence_count == 0 || current_bin != results[sequence_count - 1])
		{
			if (sequence_count > 1 /* never change the first value */
				&& _stroke_bins_adjacent_vector (
							results[sequence_count - 2],
							results[sequence_count - 1],
							prev_bin))
			{
				/* simplify stroke by ignoring 1-bin increments */
				results[sequence_count - 1] = prev_bin;
			}
			else
				results[sequence_count++] = current_bin;
		}
	}

	/* CHECKME ever need to preserve a 'tail' of points ? then must copy tail to
	   head then clear leftover */
	if (clear)
	{
		if (handle->DEVICE_METRIC points_head == NULL)
		{
#ifdef HAVE_OBSTACK
			obstack_free (&handle->DEVICE_METRIC pointstack,
				handle->DEVICE_METRIC points_head);
#else
			_stroke_freshen_chunks (&handle->DEVICE_METRIC pointchunks);
#endif
			handle->DEVICE_METRIC points_head = NULL;
		}
		handle->DEVICE_METRIC points_tail = NULL;
		handle->DEVICE_METRIC points_count = 0;
	}
	pthread_mutex_unlock (&handle->DEVICE_METRIC lock);

	if (sequence_count > 0)
	{
		char *returned;
#ifdef EMBEDDED
# ifdef DEBUG_MESSAGES
		returned = g_strndup (results, sequence_count);
		gchar *tmp = stroke_make_ascii_sequence (returned);
		printd (DEBUG, "before simplify, sequence is %s", tmp);
		g_free (returned);
		g_free (tmp);
# endif
#endif
		sequence_count = _stroke_simplify (results, sequence_count,
			STROKE_AXIS_BINS/2);  /* CHECKME compromise threshold for large grids ? */
#ifdef EMBEDDED
#ifdef DEBUG_MESSAGES
		returned = g_strndup (results, sequence_count);
		tmp = stroke_make_ascii_sequence (returned);
		printd (DEBUG, "AFTER simplify, sequence is %s", tmp);
		g_free (returned);
		g_free (tmp);
#endif
#endif
		if (readable)
		{
			int indx;
			size_t lentotal;
			char number[8]; /* 3 digits + ',' + '\0', rounded up */
			char *this;
			lentotal = 0;
/*			if (STROKE_AXIS_BINS < 3) / * each readable number is a single digit * /
				lentotal = space * 2; / * for digits and commas * /
			else if (STROKE_AXIS_BINS < 10) / * each readable number can be a double digit * /
				lentotal = space * 3;
			else
				lentotal = space * 4;
*/
			for (indx = 0, this = results; indx < sequence_count; indx++, this++)
			{
				snprintf (number, sizeof (number), "%u,", *this);
				lentotal += strlen (number);
			}
			returned = malloc (lentotal + 1);
			if (returned != NULL)
			{
				lentotal = 0;
				int lenthis;
				for (indx = 0, this = results; indx < sequence_count; indx++, this++)
				{
					snprintf (number, sizeof (number), "%u,", *this);
					lenthis = strlen (number);
					memcpy (returned + lentotal, number, lenthis);
					lentotal += lenthis;
				}
				*(returned + lentotal - 1) = '\0'; /* no trailing ',' */
				*sequence = returned;
			}
			else
			{
				STROKE_MEMORY_ERROR
				sequence_count = 0;
			}
		}
		else
		{
			returned = malloc (sequence_count + 1);
			if (returned != NULL)
			{
				memcpy (returned, results, sequence_count);
				*(returned + sequence_count) = '\0';
				*sequence = returned;
			}
			else
			{
				STROKE_MEMORY_ERROR
				sequence_count = 0;
			}
		}
	}
	return sequence_count;
}

/**
 * stroke_fake:
 * @handle: pointer to #Stroke data struct
 * @device: non-NULL pointer
 * @sequence:
 *
 * Create a list of points in @handle reflecting the bins described in @sequence
 **/
void stroke_fake (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  const char *sequence)
{
#if STROKE_MAX_DEVICES > 1
	int count;
	for (count = 0; count < STROKE_MAX_DEVICES; count++)
	{
		if (handle->devices[count] == device)
		  break;
	}
	if (count == STROKE_MAX_DEVICES)
	{
		for (count = 0; count < STROKE_MAX_DEVICES; count++)
		{
	  		if (handle->devices[count] == NULL)
				break;
		}
	}
	if (count == STROKE_MAX_DEVICES)
		return;
	handle->devices[count] = device;
#endif
	char *raw, *this;
	if (strchr (sequence, ',') != NULL)
	{
		raw = stroke_make_raw_sequence (sequence);
		if (raw == NULL)
			return;
	}
	else
	{
		unsigned char *check;
		for (check = (unsigned char *) sequence; *check != '\0'; check++)
		{
			if (*check > bincount)
				return;
		}
		raw = (char *)sequence;
	}

	if (handle->DEVICE_METRIC points_count > 0)
		stroke_clear (handle
#if STROKE_MAX_DEVICES > 1
			, device
#endif
		);

	for (this = raw;
		 *this != '\0' && (handle->max_bin == 0 || (this - raw) <= handle->max_bin);
		 this++)
	{
		int x, y;
		if (_stroke_bin_centre (handle, (int)*this, &x, &y))
			stroke_record (handle,
#if STROKE_MAX_DEVICES > 1
				device,
#endif
				x, y);
	}
	if (raw != sequence)
		free (raw);
}

/**
 * stroke_record:
 * @handle: pointer to #Stroke data struct
 * @device: non-NULL pointer
 * @x: horiz. position (bigger toward screen-right)
 * @y: vert. position (bigger toward screen-bottom)
 *
 * Append (@x,@y) to the list of points in @handle
 **/
void stroke_record (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  int x, int y)
{
	point *new_point_p;
	int delx, dely;
	float ix, iy, ox, oy;
//    /*
#ifdef STROKE_DEBUG
	printf ("DEBUG:: stroke record: x %d y %d \n",x, y);
#endif
//	*/
//	if (x < 0 || y < 0)	/* ignore data quickly seen to be outside area of interest ? */
//		return;
#if STROKE_MAX_DEVICES > 1
	int count;
	for (count = 0; count < STROKE_MAX_DEVICES; count++)
	{
		if (handle->devices[count] == device)
		  break;
	}
	if (count == STROKE_MAX_DEVICES)
	{
		for (count = 0; count < STROKE_MAX_DEVICES; count++)
		{
	  		if (handle->devices[count] == NULL)
				break;
		}
	}
	if (count == STROKE_MAX_DEVICES)
		return;
	handle->devices[count] = device;
#endif

	if (handle->DEVICE_METRIC points_tail != NULL)
	{
		delx = x - handle->DEVICE_METRIC points_tail->x;
		dely = y - handle->DEVICE_METRIC points_tail->y;
		if (delx == 0 && dely == 0)
			return;
	}
	else
	{
		delx = dely = 0;	/* no interpolation */
	}

	if (handle->DEVICE_METRIC points_count < STROKE_MAX_POINTS)
	{
		new_point_p =
#ifdef HAVE_OBSTACK
		 (point*) obstack_alloc (&handle->DEVICE_METRIC pointstack, sizeof(point));
#else
		_stroke_allocate_point (&handle->DEVICE_METRIC pointchunks);
#endif
		if (new_point_p == NULL)
		{
/*
#ifdef STROKE_DEBUG
		printf ("DEBUG:: stroke record: failed to allocate new point data\n");
#endif
*/
		 return;
		}

		pthread_mutex_lock (&handle->DEVICE_METRIC lock);
		if (handle->DEVICE_METRIC points_tail == NULL)
		{
			/* first point in list - initialize list and metrics */
			handle->DEVICE_METRIC points_head = handle->DEVICE_METRIC points_tail = new_point_p;
			/* setup for initial 'boundary' checks to always succeed */
			handle->DEVICE_METRIC min_x = handle->DEVICE_METRIC min_y = STROKE_BIG_VALUE;
			handle->DEVICE_METRIC max_x = handle->DEVICE_METRIC max_y = -1;
			handle->DEVICE_METRIC points_count = 1;
		}
		else
		{
			/* interpolate between last and current point */
			/* interpolate by the greatest delta direction */
			if (abs(delx) > abs(dely))
			{
				ox = handle->DEVICE_METRIC points_tail->x;
				iy = handle->DEVICE_METRIC points_tail->y;
				/* go from the last point to the current, whatever direction it may be, in 1-px increments */
				for (ix = ox; (delx > 0) ? (ix < x) : (ix > x); ix += (delx > 0) ? 1 : -1)
				{
					oy = iy;
					/* step the other axis by the correct increment (which may be 0) */
					iy += fabs(((float) dely / (float) delx)) * (float) ((dely < 0) ? -1.0 : 1.0);

					if (iy != oy || ox != ix)
					{
						/* add an interpolated point */
						handle->DEVICE_METRIC points_tail->next = new_point_p;
						handle->DEVICE_METRIC points_tail = new_point_p;
						new_point_p->x = ix;
						new_point_p->y = iy;
						new_point_p->next = NULL;

						handle->DEVICE_METRIC points_count++;

						new_point_p =
#ifdef HAVE_OBSTACK
						(point*) obstack_alloc (&handle->DEVICE_METRIC pointstack, sizeof(point));
#else
						_stroke_allocate_point (&handle->DEVICE_METRIC pointchunks);
#endif
						if (new_point_p == NULL)
						{
/*
#ifdef STROKE_DEBUG
							printf ("DEBUG:: stroke record: failed to allocate new point data\n");
#endif
*/
							/* update stroke 'boundaries' if needed, as we're returning now */
							if (((int) ix) < handle->DEVICE_METRIC min_x) handle->DEVICE_METRIC min_x = (int) ix;
							if (((int) ix) > handle->DEVICE_METRIC max_x) handle->DEVICE_METRIC max_x = (int) ix;
							if (((int) iy) < handle->DEVICE_METRIC min_y) handle->DEVICE_METRIC min_y = (int) iy;
							if (((int) iy) > handle->DEVICE_METRIC max_y) handle->DEVICE_METRIC max_y = (int) iy;
							pthread_mutex_unlock (&handle->DEVICE_METRIC lock);
							return;
						}
					}
					ox = ix; /* ready for next loop check */
				}
			}
			else
			{  /* same thing, but for dely larger than delx case... */
				ix = handle->DEVICE_METRIC points_tail->x;
				oy = handle->DEVICE_METRIC points_tail->y;
				/* go from the last point to the current, whatever direction it
				   may be, in 1-px increments */
				for (iy = oy; (dely > 0) ? (iy < y) : (iy > y); iy += (dely > 0) ? 1 : -1)
				{
					ox = ix;
					/* step the other axis by the correct increment */
					ix += fabs(((float) delx / (float) dely)) * (float) ((delx < 0) ? -1.0 : 1.0);

					if (ix != ox || iy != oy)
					{
						/* add an interpolated point */
						handle->DEVICE_METRIC points_tail->next = new_point_p;
						handle->DEVICE_METRIC points_tail = new_point_p;
						new_point_p->y = iy;
						new_point_p->x = ix;
						new_point_p->next = NULL;

						handle->DEVICE_METRIC points_count++;

						new_point_p =
#ifdef HAVE_OBSTACK
						(point*) obstack_alloc (&handle->DEVICE_METRIC pointstack, sizeof(point));
#else
						_stroke_allocate_point (&handle->DEVICE_METRIC pointchunks);
#endif
						if (new_point_p == NULL)
						{
/*
#ifdef STROKE_DEBUG
							printf ("DEBUG:: stroke record: failed to allocate new point data\n");
#endif
*/
							if (((int) ix) < handle->DEVICE_METRIC min_x) handle->DEVICE_METRIC min_x = (int) ix;
							if (((int) ix) > handle->DEVICE_METRIC max_x) handle->DEVICE_METRIC max_x = (int) ix;
							if (((int) iy) < handle->DEVICE_METRIC min_y) handle->DEVICE_METRIC min_y = (int) iy;
							if (((int) iy) > handle->DEVICE_METRIC max_y) handle->DEVICE_METRIC max_y = (int) iy;
							pthread_mutex_unlock (&handle->DEVICE_METRIC lock);
							return;
						}
						oy = iy; /* ready for next loop's check */
					}
				}
			}

			/* add the sampled point */
			handle->DEVICE_METRIC points_tail->next = new_point_p;
			handle->DEVICE_METRIC points_tail = new_point_p;
			handle->DEVICE_METRIC points_count++;
		}

		/* record the sampled point values */
		new_point_p->x = x;
		new_point_p->y = y;
		new_point_p->next = NULL;
		/* update stroke 'boundaries' if needed */
		if (x < handle->DEVICE_METRIC min_x) handle->DEVICE_METRIC min_x = x;
		if (x > handle->DEVICE_METRIC max_x) handle->DEVICE_METRIC max_x = x;
		if (y < handle->DEVICE_METRIC min_y) handle->DEVICE_METRIC min_y = y;
		if (y > handle->DEVICE_METRIC max_y) handle->DEVICE_METRIC max_y = y;

		pthread_mutex_unlock (&handle->DEVICE_METRIC lock);
	}
/*
#ifdef STROKE_DEBUG
	else
	{
		printf ("DEBUG:: stroke record: ignore data after reaching point-limit\n");
	}
#endif
*/
}

/**
 * stroke_replay:
 * @handle: pointer to #Stroke data struct
 * @array: store for pointer to start of data, or NULL
 *
 * Get allocated array of listed points data as (int x, int y pairs)
 *
 * Returns no. of pairs, with the array start stored in @array if that's non-NULL
 **/
int stroke_replay (Stroke *handle,
#if STROKE_MAX_DEVICES > 1
  void *device,
#endif
  void **array)
{
#if STROKE_MAX_DEVICES > 1
	int count;
	for (count = 0; count < STROKE_MAX_DEVICES; count++)
	{
		if (handle->devices[count] == device)
		break;
	}
	if (count == STROKE_MAX_DEVICES)
	{
		for (count = 0; count < STROKE_MAX_DEVICES; count++)
		{
			if (handle->devices[count] == NULL)
			break;
		}
	}
	if (count == STROKE_MAX_DEVICES)
		return 0;
	handle->devices[count] = device;
#endif
	int pcount = handle->DEVICE_METRIC points_count;
#ifdef EMBEDDED
	printd (DEBUG, "%d drawing area data points for stroke replay", pcount);
#endif
	if (pcount > 0 && array != NULL)
	{
		point *point_p;
		void *copy;
		struct _dump
		{
			int x; /* same order as start of struct _point, for quick transfers */
			int y;
		};

		copy = malloc (pcount * sizeof (struct _dump));
		if (copy != NULL)
		{
			struct _dump *to;
			for (point_p = handle->DEVICE_METRIC points_head, to = copy; point_p != NULL;
				 point_p = point_p->next, to++)
			{
				*to = *(struct _dump*) point_p;
			}
			*array = copy;
		}
		else
		{
			STROKE_MEMORY_ERROR
			*array = NULL;
			pcount = 0;
		}
	}
	return pcount;
}

/**
 * stroke_verify_sequence:
 * @ascii: string comprising comma-separated ascii bin numbers 1 .. 255
 *
 * Returns newly-allocated string, or NULL if there's a problem
 **/
char *stroke_verify_sequence (const char *ascii)
{
	char *verified;
	Stroke *handle;

	verified = NULL;
	/* to provide centering and smoothing, do a simple draw and recover */
	handle = _stroke_init_data (STROKE_AXIS_BINS * 5, STROKE_AXIS_BINS * 5);
	if (handle != NULL)
	{
		int count;
		stroke_fake (handle,
#if STROKE_MAX_DEVICES > 1
  			stroke_verify_sequence, /* some non-NULL ptr */
#endif
			ascii);
		count = stroke_translate (handle,
#if STROKE_MAX_DEVICES > 1
	  			stroke_verify_sequence,
#endif
				TRUE, TRUE, &verified);
		stroke_destroy (handle);
		if (count == 0)
			return NULL;
	}
	return verified;
}

/**
 * stroke_make_ascii_sequence:
 * @raw: string comprising unsigned char bin numbers 1 .. 255
 *
 * Convert @raw into a corresponding string with bin-numbers comma-separated
 * and ASCII'fied
 * NO validity checking is done
 *
 * Returns newly-allocated string, or NULL upon allocation error
 **/
char *stroke_make_ascii_sequence (const char *raw)
{
	size_t lentotal;
	char number[8]; /* 3 digits + ',' + '\0', rounded up */
	char *returned;
	const char *this;
	lentotal = 0;
/*	if (STROKE_AXIS_BINS < 3) / * each readable number is a single digit * /
		lentotal = space * 2; / * for digits and commas * /
	else if (STROKE_AXIS_BINS < 10) / * each readable number can be a double digit * /
		lentotal = space * 3;
	else
		lentotal = space * 4;
	*/
	for (this = raw; *this != '\0'; this++)
	{
		snprintf (number, sizeof (number), "%u,", *this);
		lentotal += strlen (number);
	}
	returned = malloc (lentotal + 1);
	if (returned != NULL)
	{
		lentotal = 0;
		int lenthis;
		for (this = raw; *this != '\0'; this++)
		{
			snprintf (number, sizeof (number), "%u,", *this);
			lenthis = strlen (number);
			memcpy (returned + lentotal, number, lenthis);
			lentotal += lenthis;
		}
		*(returned + lentotal - 1) = '\0'; /* no trailing ',' */
	}
	return returned;
}

/**
 * stroke_make_raw_sequence:
 * @ascii: string comprising comma-separated ascii bin numbers 1 .. 255
 *
 * Convert @ascii into a corresponding string with single-byte bin-numbers and
 * no comma-separation. Bin numbers are checked for validity (in-range).
 * Leading and/or trailing whitespace or ',', and embedded whitespace, are ignored
 *
 * Returns newly-allocated string, or NULL if there's a problem
 **/
char *stroke_make_raw_sequence (const char *ascii)
{
	int count;
	const char *realstart;
	char *start, *end, *returned;

	realstart = ascii;
	while (*realstart < '1')
		realstart++;	/* ignore leading irrelevants */

	count = 1; /* even if ascii is empty */
	start = (char *) realstart;
	while ((end = strchr (start, ',')) != NULL)
	{
		count++;
		start = ++end;
	}
	returned = (char *) malloc (count + 1); /* initially assume no interpolation */
	if (returned != NULL)
	{
		count = 0;
		if (*realstart != '\0')
		{
			unsigned long val;
			char *tail;
			start = (char *) realstart;
			while ((end = strchr (start, ',')) != NULL)
			{
				val = strtol (start, &tail, 10);
				if (tail != end || val < 1 || val > bincount)
				{
					count = -1;
					break;
				}
				returned[count++] = (char) val;
				start = end + 1;
			}
			/* now the last number */
			if (count > -1 && *start != '\0')
			{
				val = strtol (start, &tail, 10);
				if (*tail != '\0' || val < 1 || val > bincount)
					count = -1;
				else
					returned[count++] = (char) val;
			}
		}
		if (count > -1)
			returned[count] = '\0';
		else
		{
			free (returned);
			return NULL;
		}
	}
	return returned;
}

#endif //! defined (EMBEDDED) etc
