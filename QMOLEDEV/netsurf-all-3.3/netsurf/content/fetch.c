/*
 * Copyright 2006,2007 Daniel Silverstone <dsilvers@digital-scurf.org>
 * Copyright 2007 James Bursa <bursa@users.sourceforge.net>
 * Copyright 2003 Phil Mellor <monkeyson@users.sourceforge.net>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * \file
 * Implementation of fetching of data from a URL.
 *
 * The implementation is the fetch factory and the generic operations
 * around the fetcher specific methods.
 *
 * Active fetches are held in the circular linked list ::fetch_ring. There may
 * be at most ::option_max_fetchers_per_host active requests per Host: header.
 * There may be at most ::option_max_fetchers active requests overall. Inactive
 * fetches are stored in the ::queue_ring waiting for use.
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>
#include <time.h>
#include <libwapcaplet/libwapcaplet.h>
#include <curl/curl.h>

#include "utils/config.h"
#include "utils/corestrings.h"
#include "utils/nsoption.h"
#include "utils/log.h"
#include "utils/messages.h"
#include "utils/nsurl.h"
#include "utils/utils.h"
#include "utils/ring.h"
#include "desktop/gui_misc.h"
#include "desktop/gui_internal.h"

#include "content/fetch.h"
#include "content/fetchers.h"
#include "content/fetchers/resource.h"
#include "content/fetchers/about.h"
#include "content/fetchers/curl.h"
#include "content/fetchers/data.h"
#include "content/fetchers/file.h"
#include "javascript/fetcher.h"
#include "content/urldb.h"

/* Define this to turn on verbose fetch logging */
#undef DEBUG_FETCH_VERBOSE

/** Verbose fetcher logging */
#ifdef DEBUG_FETCH_VERBOSE
#define FETCH_LOG(x) LOG(x)
#else
#define FETCH_LOG(x)
#endif

/** The maximum number of fetchers that can be added */
#define MAX_FETCHERS 10

/** The time in ms between polling the fetchers.
 *
 * \todo The schedule timeout should be profiled to see if there is a
 * better value or even if it needs to be dynamic.
 */
#define SCHEDULE_TIME 10

/** The fdset timeout in ms */
#define FDSET_TIMEOUT 1000

/**
 * Information about a fetcher for a given scheme.
 */
typedef struct scheme_fetcher_s {
	lwc_string *scheme; /**< The scheme. */

	struct fetcher_operation_table ops; /**< The fetchers operations. */
	int refcount; /**< When zero the fetcher is no longer in use. */
} scheme_fetcher;

static scheme_fetcher fetchers[MAX_FETCHERS];

/** Information for a single fetch. */
struct fetch {
	fetch_callback callback;/**< Callback function. */
	nsurl *url;		/**< URL. */
	nsurl *referer;		/**< Referer URL. */
	bool send_referer;	/**< Valid to send the referer */
	bool verifiable;	/**< Transaction is verifiable */
	void *p;		/**< Private data for callback. */
	lwc_string *host;	/**< Host part of URL, interned */
	long http_code;		/**< HTTP response code, or 0. */
	int fetcherd;           /**< Fetcher descriptor for this fetch */
	void *fetcher_handle;	/**< The handle for the fetcher. */
	bool fetch_is_active;	/**< This fetch is active. */
	struct fetch *r_prev;	/**< Previous active fetch in ::fetch_ring. */
	struct fetch *r_next;	/**< Next active fetch in ::fetch_ring. */
};

static struct fetch *fetch_ring = NULL;	/**< Ring of active fetches. */
static struct fetch *queue_ring = NULL;	/**< Ring of queued fetches */

/******************************************************************************
 * fetch internals							      *
 ******************************************************************************/

static inline void fetch_ref_fetcher(int fetcherd)
{
	fetchers[fetcherd].refcount++;
}

static inline void fetch_unref_fetcher(int fetcherd)
{
	fetchers[fetcherd].refcount--;
	if (fetchers[fetcherd].refcount == 0) {
		fetchers[fetcherd].ops.finalise(fetchers[fetcherd].scheme);
		lwc_string_unref(fetchers[fetcherd].scheme);
	}
}

/**
 * Find a suitable fetcher for a scheme.
 */
static int get_fetcher_for_scheme(lwc_string *scheme)
{
	int fetcherd;
	bool match;

	for (fetcherd = 0; fetcherd < MAX_FETCHERS; fetcherd++) {
		if ((fetchers[fetcherd].refcount > 0) &&
		    (lwc_string_isequal(fetchers[fetcherd].scheme,
					scheme, &match) == lwc_error_ok) &&
		    (match == true)) {
			return fetcherd;
	       }
	}
	return -1;
}

/**
 * Dispatch a single job
 */
static bool fetch_dispatch_job(struct fetch *fetch)
{
	RING_REMOVE(queue_ring, fetch);
	FETCH_LOG(("Attempting to start fetch %p, fetcher %p, url %s", fetch,
	     fetch->fetcher_handle, nsurl_access(fetch->url)));

	if (!fetchers[fetch->fetcherd].ops.start(fetch->fetcher_handle)) {
		RING_INSERT(queue_ring, fetch); /* Put it back on the end of the queue */
		return false;
	} else {
		RING_INSERT(fetch_ring, fetch);
		fetch->fetch_is_active = true;
		return true;
	}
}

/**
 * Choose and dispatch a single job. Return false if we failed to dispatch
 * anything.
 *
 * We don't check the overall dispatch size here because we're not called unless
 * there is room in the fetch queue for us.
 */
static bool fetch_choose_and_dispatch(void)
{
	bool same_host;
	struct fetch *queueitem;
	queueitem = queue_ring;
	do {
		/* We can dispatch the selected item if there is room in the
		 * fetch ring
		 */
		int countbyhost;
		RING_COUNTBYLWCHOST(struct fetch, fetch_ring, countbyhost,
				    queueitem->host);
		if (countbyhost < nsoption_int(max_fetchers_per_host)) {
			/* We can dispatch this item in theory */
			return fetch_dispatch_job(queueitem);
		}
		/* skip over other items with the same host */
		same_host = true;
		while (same_host == true && queueitem->r_next != queue_ring) {
			if (lwc_string_isequal(queueitem->host,
					       queueitem->r_next->host, &same_host) ==
			    lwc_error_ok && same_host == true) {
				queueitem = queueitem->r_next;
			}
		}
		queueitem = queueitem->r_next;
	} while (queueitem != queue_ring);
	return false;
}

static void dump_rings(void)
{
#ifdef DEBUG_FETCH_VERBOSE
	struct fetch *q;
	struct fetch *f;

	q = queue_ring;
	if (q) {
		do {
			LOG(("queue_ring: %s", nsurl_access(q->url)));
			q = q->r_next;
		} while (q != queue_ring);
	}
	f = fetch_ring;
	if (f) {
		do {
			LOG(("fetch_ring: %s", nsurl_access(f->url)));
			f = f->r_next;
		} while (f != fetch_ring);
	}
#endif
}

/**
 * Dispatch as many jobs as we have room to dispatch.
 *
 * @return true if there are active fetchers that require polling else false.
 */
static bool fetch_dispatch_jobs(void)
{
	int all_active;
	int all_queued;

	RING_GETSIZE(struct fetch, queue_ring, all_queued);
	RING_GETSIZE(struct fetch, fetch_ring, all_active);

	FETCH_LOG(("queue_ring %i, fetch_ring %i", all_queued, all_active));
	dump_rings();

	while ((all_queued != 0) &&
	       (all_active < nsoption_int(max_fetchers)) &&
	       fetch_choose_and_dispatch()) {
			all_queued--;
			all_active++;
			FETCH_LOG(("%d queued, %d fetching",
				   all_queued, all_active));
	}

	FETCH_LOG(("Fetch ring is now %d elements.", all_active));
	FETCH_LOG(("Queue ring is now %d elements.", all_queued));

	return (all_active > 0);
}

static void fetcher_poll(void *unused)
{
	int fetcherd;

	if (fetch_dispatch_jobs()) {
		FETCH_LOG(("Polling fetchers"));
		for (fetcherd = 0; fetcherd < MAX_FETCHERS; fetcherd++) {
			if (fetchers[fetcherd].refcount > 0) {
				/* fetcher present */
				fetchers[fetcherd].ops.poll(fetchers[fetcherd].scheme);
			}
		}

		/* schedule active fetchers to run again in 10ms */
		guit->browser->schedule(SCHEDULE_TIME, fetcher_poll, NULL);
	}
}

/******************************************************************************
 * Public API								      *
 ******************************************************************************/

/* exported interface documented in content/fetch.h */
nserror fetcher_init(void)
{
	nserror ret;

	ret = fetch_curl_register();
	if (ret != NSERROR_OK) {
		return ret;
	}

	ret = fetch_data_register();
	if (ret != NSERROR_OK) {
		return ret;
	}

	ret = fetch_file_register();
	if (ret != NSERROR_OK) {
		return ret;
	}

	ret = fetch_resource_register();
	if (ret != NSERROR_OK) {
		return ret;
	}

	ret = fetch_about_register();
	if (ret != NSERROR_OK) {
		return ret;
	}

	ret = fetch_javascript_register();

	return ret;
}

/* exported interface documented in content/fetchers.h */
void fetcher_quit(void)
{
	int fetcherd; /* fetcher index */
	for (fetcherd = 0; fetcherd < MAX_FETCHERS; fetcherd++) {
		if (fetchers[fetcherd].refcount > 1) {
			/* fetcher still has reference at quit. This
			 * should not happen as the fetch should have
			 * been aborted in llcache shutdown.
			 *
			 * This appears to be normal behaviour if a
			 * curl operation is still in progress at exit
			 * as the abort waits for curl to complete.
			 *
			 * We could make the user wait for curl to
			 * complete but we are exiting anyway so thats
			 * unhelpful. Instead we just log it and force
			 * the reference count to allow the fetcher to
			 * be stopped.
			 */
			LOG(("Fetcher for scheme %s still has %d active users at quit.",
			     lwc_string_data(fetchers[fetcherd].scheme),
			     fetchers[fetcherd].refcount));

			fetchers[fetcherd].refcount = 1;
		}
		if (fetchers[fetcherd].refcount == 1) {

			fetch_unref_fetcher(fetcherd);
		}
	}
}

/* exported interface documented in content/fetchers.h */
nserror
fetcher_add(lwc_string *scheme, const struct fetcher_operation_table *ops)
{
	int fetcherd;

	/* find unused fetcher descriptor */
	for (fetcherd = 0; fetcherd < MAX_FETCHERS; fetcherd++) {
		if (fetchers[fetcherd].refcount == 0) {
			break;
		}
	}
	if (fetcherd == MAX_FETCHERS) {
		return NSERROR_INIT_FAILED;
	}

	if (!ops->initialise(scheme)) {
		return NSERROR_INIT_FAILED;
	}

	fetchers[fetcherd].scheme = scheme;
	fetchers[fetcherd].ops = *ops;

	fetch_ref_fetcher(fetcherd);

	return NSERROR_OK;
}

/* exported interface documented in content/fetch.h */
nserror fetcher_fdset(fd_set *read_fd_set,
		      fd_set *write_fd_set,
		      fd_set *except_fd_set,
		      int *maxfd_out)
{
	CURLMcode code;
	int maxfd;
	int fetcherd; /* fetcher index */

	if (!fetch_dispatch_jobs()) {
		FETCH_LOG(("No jobs"));
		*maxfd_out = -1;
		return NSERROR_OK;
	}

	FETCH_LOG(("Polling fetchers"));

	for (fetcherd = 0; fetcherd < MAX_FETCHERS; fetcherd++) {
		if (fetchers[fetcherd].refcount > 0) {
			/* fetcher present */
			fetchers[fetcherd].ops.poll(fetchers[fetcherd].scheme);
		}
	}

	FD_ZERO(read_fd_set);
	FD_ZERO(write_fd_set);
	FD_ZERO(except_fd_set);
	code = curl_multi_fdset(fetch_curl_multi,
				read_fd_set,
				write_fd_set,
				except_fd_set,
				&maxfd);
	assert(code == CURLM_OK);

	if (maxfd >= 0) {
		/* change the scheduled poll to happen is a 1000ms as
		 * we assume fetching an fdset means the fetchers will
		 * be run by the client waking up on data available on
		 * the fd and re-calling fetcher_fdset() if this does
		 * not happen the fetch polling will continue as
		 * usual.
		 */
		/** @note adjusting the schedule time is only done for
		 * curl currently. This is because as it is assumed to
		 * be the only fetcher that can possibly have fd to
		 * select on. All the other fetchers continue to need
		 * polling frequently.
		 */
		guit->browser->schedule(FDSET_TIMEOUT, fetcher_poll, NULL);
	}

	*maxfd_out = maxfd;

	return NSERROR_OK;
}

/* exported interface documented in content/fetch.h */
struct fetch *
fetch_start(nsurl *url,
	    nsurl *referer,
	    fetch_callback callback,
	    void *p,
	    bool only_2xx,
	    const char *post_urlenc,
	    const struct fetch_multipart_data *post_multipart,
	    bool verifiable,
	    bool downgrade_tls,
	    const char *headers[])
{
	struct fetch *fetch;
	lwc_string *scheme;
	bool match;

	fetch = malloc(sizeof (*fetch));
	if (fetch == NULL) {
		return NULL;
	}

	/* The URL we're fetching must have a scheme */
	scheme = nsurl_get_component(url, NSURL_SCHEME);
	assert(scheme != NULL);

	/* try and obtain a fetcher for this scheme */
	fetch->fetcherd = get_fetcher_for_scheme(scheme);
	if (fetch->fetcherd == -1) {
		lwc_string_unref(scheme);
		free(fetch);
		return NULL;
	}

	FETCH_LOG(("fetch %p, url '%s'", fetch, nsurl_access(url)));

	/* construct a new fetch structure */
	fetch->callback = callback;
	fetch->url = nsurl_ref(url);
	fetch->verifiable = verifiable;
	fetch->p = p;
	fetch->http_code = 0;
	fetch->r_prev = NULL;
	fetch->r_next = NULL;
	fetch->referer = NULL;
	fetch->send_referer = false;
	fetch->fetcher_handle = NULL;
	fetch->fetch_is_active = false;
	fetch->host = nsurl_get_component(url, NSURL_HOST);

	if (referer != NULL) {
		lwc_string *ref_scheme;
		fetch->referer = nsurl_ref(referer);

		ref_scheme = nsurl_get_component(referer, NSURL_SCHEME);
		/* Not a problem if referer has no scheme */

		/* Determine whether to send the Referer header */
		if (nsoption_bool(send_referer) && ref_scheme != NULL) {
			/* User permits us to send the header
			 * Only send it if:
			 *    1) The fetch and referer schemes match
			 * or 2) The fetch is https and the referer is http
			 *
			 * This ensures that referer information is only sent
			 * across schemes in the special case of an https
			 * request from a page served over http. The inverse
			 * (https -> http) should not send the referer (15.1.3)
			 */
			bool match1;
			bool match2;
			if (lwc_string_isequal(scheme, ref_scheme,
					       &match) != lwc_error_ok) {
				match = false;
			}
			if (lwc_string_isequal(scheme, corestring_lwc_https,
					       &match1) != lwc_error_ok) {
				match1 = false;
			}
			if (lwc_string_isequal(ref_scheme, corestring_lwc_http,
					       &match2) != lwc_error_ok) {
				match2= false;
			}
			if (match == true || (match1 == true && match2 == true))
				fetch->send_referer = true;
		}
		if (ref_scheme != NULL)
			lwc_string_unref(ref_scheme);
	}

	/* these aren't needed past here */
	lwc_string_unref(scheme);

	/* try and set up the fetch */
	fetch->fetcher_handle = fetchers[fetch->fetcherd].ops.setup(fetch, url,
						only_2xx, downgrade_tls,
						post_urlenc, post_multipart,
						headers);
	if (fetch->fetcher_handle == NULL) {

		if (fetch->host != NULL)
			lwc_string_unref(fetch->host);

		if (fetch->url != NULL)
			nsurl_unref(fetch->url);

		if (fetch->referer != NULL)
			nsurl_unref(fetch->referer);

		free(fetch);

		return NULL;
	}

	/* Rah, got it, so ref the fetcher. */
	fetch_ref_fetcher(fetch->fetcherd);

	/* Dump new fetch in the queue. */
	RING_INSERT(queue_ring, fetch);

	/* Ask the queue to run. */
	if (fetch_dispatch_jobs()) {
		FETCH_LOG(("scheduling poll"));
		/* schedule active fetchers to run again in 10ms */
		guit->browser->schedule(10, fetcher_poll, NULL);
	}

	return fetch;
}

/* exported interface documented in content/fetch.h */
void fetch_abort(struct fetch *f)
{
	assert(f);
	FETCH_LOG(("fetch %p, fetcher %p, url '%s'", f, f->fetcher_handle,
	     nsurl_access(f->url)));
	fetchers[f->fetcherd].ops.abort(f->fetcher_handle);
}

/* exported interface documented in content/fetch.h */
void fetch_free(struct fetch *f)
{
	FETCH_LOG(("Freeing fetch %p, fetcher %p", f, f->fetcher_handle));

	fetchers[f->fetcherd].ops.free(f->fetcher_handle);

	fetch_unref_fetcher(f->fetcherd);

	nsurl_unref(f->url);
	if (f->referer != NULL) {
		nsurl_unref(f->referer);
	}
	if (f->host != NULL) {
		lwc_string_unref(f->host);
	}
	free(f);
}



/* exported interface documented in content/fetch.h */
bool fetch_can_fetch(const nsurl *url)
{
	lwc_string *scheme = nsurl_get_component(url, NSURL_SCHEME);
	int fetcherd;

	fetcherd = get_fetcher_for_scheme(scheme);
	lwc_string_unref(scheme);

	if (fetcherd == -1) {
		return false;
	}

	return fetchers[fetcherd].ops.acceptable(url);
}

/* exported interface documented in content/fetch.h */
void fetch_change_callback(struct fetch *fetch,
			   fetch_callback callback,
			   void *p)
{
	assert(fetch);
	fetch->callback = callback;
	fetch->p = p;
}

/* exported interface documented in content/fetch.h */
long fetch_http_code(struct fetch *fetch)
{
	return fetch->http_code;
}

/* exported interface documented in content/fetch.h */
bool fetch_get_verifiable(struct fetch *fetch)
{
	assert(fetch);

	return fetch->verifiable;
}

/* exported interface documented in content/fetch.h */
struct fetch_multipart_data *
fetch_multipart_data_clone(const struct fetch_multipart_data *list)
{
	struct fetch_multipart_data *clone, *last = NULL;
	struct fetch_multipart_data *result = NULL;

	for (; list != NULL; list = list->next) {
		clone = malloc(sizeof(struct fetch_multipart_data));
		if (clone == NULL) {
			if (result != NULL)
				fetch_multipart_data_destroy(result);

			return NULL;
		}

		clone->file = list->file;

		clone->name = strdup(list->name);
		if (clone->name == NULL) {
			free(clone);
			if (result != NULL)
				fetch_multipart_data_destroy(result);

			return NULL;
		}

		clone->value = strdup(list->value);
		if (clone->value == NULL) {
			free(clone->name);
			free(clone);
			if (result != NULL)
				fetch_multipart_data_destroy(result);

			return NULL;
		}

		if (clone->file) {
			clone->rawfile = strdup(list->rawfile);
			if (clone->rawfile == NULL) {
				free(clone->value);
				free(clone->name);
				free(clone);
				if (result != NULL)
					fetch_multipart_data_destroy(result);

				return NULL;
			}
		} else {
			clone->rawfile = NULL;
		}

		clone->next = NULL;

		if (result == NULL)
			result = clone;
		else
			last->next = clone;

		last = clone;
	}

	return result;
}

/* exported interface documented in content/fetch.h */
void fetch_multipart_data_destroy(struct fetch_multipart_data *list)
{
	struct fetch_multipart_data *next;

	for (; list != NULL; list = next) {
		next = list->next;
		free(list->name);
		free(list->value);
		if (list->file) {
			FETCH_LOG(("Freeing rawfile: %s", list->rawfile));
			free(list->rawfile);
		}
		free(list);
	}
}

/* exported interface documented in content/fetch.h */
void
fetch_send_callback(const fetch_msg *msg, struct fetch *fetch)
{
	fetch->callback(msg, fetch->p);
}


/* exported interface documented in content/fetch.h */
void fetch_remove_from_queues(struct fetch *fetch)
{
	FETCH_LOG(("Fetch %p, fetcher %p can be freed",
		   fetch, fetch->fetcher_handle));

	/* Go ahead and free the fetch properly now */
	if (fetch->fetch_is_active) {
		RING_REMOVE(fetch_ring, fetch);
	} else {
		RING_REMOVE(queue_ring, fetch);
	}

#ifdef DEBUG_FETCH_VERBOSE
	int all_active;
	int all_queued;

	RING_GETSIZE(struct fetch, fetch_ring, all_active);
	RING_GETSIZE(struct fetch, queue_ring, all_queued);

	LOG(("Fetch ring is now %d elements.", all_active));

	LOG(("Queue ring is now %d elements.", all_queued));
#endif
}


/* exported interface documented in content/fetch.h */
void fetch_set_http_code(struct fetch *fetch, long http_code)
{
	FETCH_LOG(("Setting HTTP code to %ld", http_code));

	fetch->http_code = http_code;
}

/* exported interface documented in content/fetch.h */
const char *fetch_get_referer_to_send(struct fetch *fetch)
{
	if (fetch->send_referer)
		return nsurl_access(fetch->referer);
	return NULL;
}

/* exported interface documented in content/fetch.h */
void fetch_set_cookie(struct fetch *fetch, const char *data)
{
	assert(fetch && data);

	/* If the fetch is unverifiable err on the side of caution and
	 * do not set the cookie */

	if (fetch->verifiable) {
		/* If the transaction's verifiable, we don't require
		 * that the request uri and the parent domain match,
		 * so don't pass in any referer/parent in this case. */
		urldb_set_cookie(data, fetch->url, NULL);
	} else if (fetch->referer != NULL) {
		/* Permit the cookie to be set if the fetch is unverifiable
		 * and the fetch URI domain matches the referer. */
		/** \todo Long-term, this needs to be replaced with a
		 * comparison against the origin fetch URI. In the case
		 * where a nested object requests a fetch, the origin URI
		 * is the nested object's parent URI, whereas the referer
		 * for the fetch will be the nested object's URI. */
		urldb_set_cookie(data, fetch->url, fetch->referer);
	}
}
