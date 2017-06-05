/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"

#include "gkm-timer.h"

#include "egg/egg-error.h"

#include <glib.h>

struct _GkmTimer {
	glong when;
	GMutex *mutex;
	gpointer identifier;
	GkmTimerFunc callback;
	gpointer user_data;
};

static GStaticMutex timer_mutex = G_STATIC_MUTEX_INIT;
static GQueue *timer_queue = NULL;
static GThread *timer_thread = NULL;
static GCond *timer_cond = NULL;
static gboolean timer_run = FALSE;
static gint timer_refs = 0;

static gint
compare_timers (gconstpointer a, gconstpointer b, gpointer user_data)
{
	const GkmTimer *ta = a;
	const GkmTimer *tb = b;
	if (ta->when < tb->when)
		return -1;
	return ta->when > tb->when;
}

static gpointer
timer_thread_func (gpointer unused)
{
	GMutex *mutex = g_static_mutex_get_mutex (&timer_mutex);
	GkmTimer *timer;
	GTimeVal tv;

	g_mutex_lock (mutex);

	while (timer_run) {
		timer = g_queue_peek_head (timer_queue);

		/* Nothing in the queue, wait until we have action */
		if (!timer) {
			g_cond_wait (timer_cond, mutex);
			continue;
		}

		g_get_current_time (&tv);

		/* We have to wait until the next timer? */
		if (tv.tv_sec < timer->when) {
			tv.tv_sec = timer->when;
			tv.tv_usec = 0;
			g_cond_timed_wait (timer_cond, mutex, &tv);
			continue;
		}

		/* Leave our thread mutex, and enter the module */
		g_mutex_unlock (mutex);
		g_mutex_lock (timer->mutex);

			if (timer->callback)
				(timer->callback) (timer, timer->user_data);

		/* Leave the module, and go back into our thread mutex */
		g_mutex_unlock (timer->mutex);
		g_mutex_lock (mutex);

		/* There's a chance that the timer may no longer be at head of queue */
		g_queue_remove (timer_queue, timer);
		g_slice_free (GkmTimer, timer);
	}

	g_mutex_unlock (mutex);
	return NULL;
}

void
gkm_timer_initialize (void)
{
	GError *error = NULL;
	g_static_mutex_lock (&timer_mutex);

		g_atomic_int_inc (&timer_refs);
		if (!timer_thread) {
			timer_run = TRUE;
			timer_thread = g_thread_create (timer_thread_func, NULL, TRUE, &error);
			if (timer_thread) {
				g_assert (timer_queue == NULL);
				timer_queue = g_queue_new ();

				g_assert (timer_cond == NULL);
				timer_cond = g_cond_new ();
			} else {
				g_warning ("could not create timer thread: %s",
				           egg_error_message (error));
			}
		}

	g_static_mutex_unlock (&timer_mutex);
}

void
gkm_timer_shutdown (void)
{
	GkmTimer *timer;

	if (g_atomic_int_dec_and_test (&timer_refs)) {
		timer_run = FALSE;
		g_assert (timer_cond);
		g_cond_broadcast (timer_cond);

		g_assert (timer_thread);
		g_thread_join (timer_thread);
		timer_thread = NULL;

		g_assert (timer_queue);

		/* Cleanup any outstanding timers */
		while (!g_queue_is_empty (timer_queue)) {
			timer = g_queue_pop_head (timer_queue);
			g_slice_free (GkmTimer, timer);
		}

		g_queue_free (timer_queue);
		timer_queue = NULL;

		g_cond_free (timer_cond);
		timer_cond = NULL;
	}
}

/* We're the only caller of this function */
GMutex* _gkm_module_get_scary_mutex_that_you_should_not_touch (GkmModule *self);

GkmTimer*
gkm_timer_start (GkmModule *module, glong seconds, GkmTimerFunc callback, gpointer user_data)
{
	GkmTimer *timer;
	GTimeVal tv;

	g_return_val_if_fail (callback, NULL);
	g_return_val_if_fail (timer_queue, NULL);

	g_get_current_time (&tv);

	timer = g_slice_new (GkmTimer);
	timer->when = seconds + tv.tv_sec;
	timer->callback = callback;
	timer->user_data = user_data;

	timer->mutex = _gkm_module_get_scary_mutex_that_you_should_not_touch (module);
	g_return_val_if_fail (timer->mutex, NULL);

	g_static_mutex_lock (&timer_mutex);

		g_assert (timer_queue);
		g_queue_insert_sorted (timer_queue, timer, compare_timers, NULL);
		g_assert (timer_cond);
		g_cond_broadcast (timer_cond);

	g_static_mutex_unlock (&timer_mutex);

	/*
	 * Note that the timer thread could not already completed this timer.
	 * This is because we're in the module, and in order to complete a timer
	 * the timer thread must enter the module mutex.
	 */

	return timer;
}

void
gkm_timer_cancel (GkmTimer *timer)
{
	GList *link;

	g_return_if_fail (timer_queue);

	g_static_mutex_lock (&timer_mutex);

		g_assert (timer_queue);

		link = g_queue_find (timer_queue, timer);
		if (link) {

			/*
			 * For thread safety the timer struct must be freed
			 * from the timer thread. So to cancel, what we do
			 * is move the timer to the front of the queue,
			 * and reset the callback and when.
			 */

			timer->when = 0;
			timer->callback = NULL;

			g_queue_delete_link (timer_queue, link);
			g_queue_push_head (timer_queue, timer);

			g_assert (timer_cond);
			g_cond_broadcast (timer_cond);
		}

	g_static_mutex_unlock (&timer_mutex);
}
