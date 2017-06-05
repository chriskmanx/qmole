/*
 * Copyright 2014 Vincent Sanders <vince@netsurf-browser.org>
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
 * Low-level source data cache backing store interface.
 */

#ifndef NETSURF_CONTENT_LLCACHE_PRIVATE_H_
#define NETSURF_CONTENT_LLCACHE_PRIVATE_H_

#include "content/llcache.h"

/** storage control flags */
enum backing_store_flags {
	/** no special processing */
	BACKING_STORE_NONE = 0,
	/** data is metadata */
	BACKING_STORE_META = 1,
};

/**
 * low level cache backing store operation table
 *
 * The low level cache (source objects) has the capability to make
 * objects and their metadata (headers etc) persistant by writing to a
 * backing store using these operations.
 */
struct gui_llcache_table {
	/**
	 * Initialise the backing store.
	 *
	 * @param parameters to configure backing store.
	 * @return NSERROR_OK on success or error code on faliure.
	 */
	nserror (*initialise)(const struct llcache_store_parameters *parameters);

	/**
	 * Finalise the backing store.
	 *
	 * @return NSERROR_OK on success or error code on faliure.
	 */
	nserror (*finalise)(void);

	/**
	 * Place an object in the backing store.
	 *
	 * The object is placed in the persistent store and may be
	 *  retrieved with the fetch method.
	 *
	 * The backing store will take a reference to the
	 *  passed data, subsequently the caller should explicitly
	 *  release the allocation using the release method and not
	 *  free the data itself.
	 *
	 * The caller may not assume that the persistent storage has
	 *  been completely written on return.
	 *
	 * @param[in] url The url is used as the unique primary key for the data.
	 * @param[in] flags The flags to control how the obejct is stored.
	 * @param[in] data The objects data.
	 * @param[in] datalen The length of the \a data.
	 * @return NSERROR_OK on success or error code on faliure.
	 */
	nserror (*store)(struct nsurl *url, enum backing_store_flags flags,
			 uint8_t *data, const size_t datalen);

	/**
	 * Retrive an object from the backing store.
	 *
	 * The caller may provide a buffer in \a data and a buffer
	 *  length in \a datalen. Alternatively the backing store will
	 *  allocate its own buffer if \a data is NULL, this memory is
	 *  managed by the backing store.
	 *
	 * The caller must assume nothing about the backing store
	 *  allocated buffers and the storage and *must* be freed by
	 *  calling the release method.
	 *
	 * @param[in] url The url is used as the unique primary key for the data.
	 * @param[in] flags The flags to control how the object is retrived.
	 * @param[in,out] data The retrived objects data.
	 * @param[in,out] datalen The length of the \a data retrieved.
	 * @return NSERROR_OK on success or error code on faliure.
	 */
	nserror (*fetch)(struct nsurl *url, enum backing_store_flags flags,
			 uint8_t **data, size_t *datalen);

	/**
	 * release a previously fetched or stored memory object.
	 *
	 * @param url The url is used as the unique primary key to invalidate.
	 * @param[in] flags The flags to control how the object data is released.
	 * @return NSERROR_OK on success or error code on faliure.
	 */
	nserror (*release)(struct nsurl *url, enum backing_store_flags flags);

	/**
	 * Invalidate a source object from the backing store.
	 *
	 * The entry (if present in the backing store) must no longer
	 * be returned as a result to the fetch or meta operations.
	 *
	 * If the entry had data allocated it will be released.
	 *
	 * @param url The url is used as the unique primary key to invalidate.
	 * @return NSERROR_OK on success or error code on faliure.
	 */
	nserror (*invalidate)(struct nsurl *url);

};

extern struct gui_llcache_table* null_llcache_table;
extern struct gui_llcache_table* filesystem_llcache_table;

#endif
