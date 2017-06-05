#pragma once

#include "util.h"

/*
XXX: evil macros, but it shows the arrays concept, so.. leaving it here
#define DECLARE_ARRAY(type, name)						\
	type *name;								\
	size_t name##_n;							\
	size_t name##_alloc
*/

/* Init functions, at least one of them should be called on uninitialized
 * array. */

#define INIT_EMPTY_ARRAY(array)							\
do {										\
	array = 0;								\
	array##_n = 0;								\
	array##_alloc = 0;							\
} while (0)

#define INIT_ARRAY(array, size)							\
do {										\
	array = xmalloc(size * sizeof(array[0]));				\
	array##_n = 0;								\
	array##_alloc = size;							\
} while (0)

/* ----------------------------------------------------------------------- */

#define ALLOC_NR(x) (((x) + 16) * 3 / 2)

/* this one can be omitted in non-debug version */
#ifdef NDEBUG
	#define CHECK_ARRAY_BOUNDS(array, index) ((void)0);
#else
	#define CHECK_ARRAY_BOUNDS(array, index)				\
		if (index >= array##_n) {					\
			XWARNING("Array: bounds were broken: (r: %u, n: %u)", 	\
					index, array##_n);			\
			break;							\
		}
#endif

/* ----------------------------------------------------------------------- */

#define ENSURE_ARRAY_CAPACITY(array, capacity)					\
do {										\
	if (capacity > array##_alloc) {						\
		size_t newsize = ALLOC_NR(array##_alloc);			\
		if (newsize < capacity)						\
			newsize = capacity;					\
										\
		void *newmem = xmalloc(newsize * sizeof(array[0]));		\
		if (array##_n) {						\
			memcpy(newmem, array, array##_n * sizeof(array[0]));	\
		}								\
		if (array)							\
			xfree(array);						\
		array = newmem;							\
		array##_alloc = newsize;					\
	}									\
} while (0)

#define FREE_ARRAY(array)							\
do { 										\
	if (array)								\
		xfree(array);							\
	array = 0;								\
	array##_n = 0;								\
	array##_alloc = 0;							\
} while (0)

#define CLEAR_ARRAY(array)							\
do {										\
	array##_n = 0;								\
} while (0)

#define ARRAY_INSERT_AFTER(array, index, elt)					\
do {										\
	CHECK_ARRAY_BOUNDS(array, index)					\
	ENSURE_ARRAY_CAPACITY(array, array##_n + 1);				\
	if (index == array##_n - 1) {						\
		array[array##_n++] = elt;					\
	} else {								\
		memmove(&array[index+2], &array[index+1], 			\
				(array##_n-index-1) * sizeof(array[0]));	\
		array[index+1] = elt;						\
		array##_n++;							\
	}									\
} while (0)

#define ARRAY_INSERT_BEFORE(array, index, elt)					\
do {										\
	CHECK_ARRAY_BOUNDS(array, index)					\
	ENSURE_ARRAY_CAPACITY(array, array##_n + 1);				\
	memmove(&array[index+1], &array[index], 				\
			(array##_n-index) * sizeof(array[0]));			\
	array[index] = elt;							\
	array##_n++;								\
} while (0)

#define ARRAY_APPEND(array, elt)						\
do {										\
	ENSURE_ARRAY_CAPACITY(array, array##_n + 1);				\
	array[array##_n++] = elt;						\
} while (0)

#define ARRAY_PREPEND(array, elt)						\
do {										\
	ENSURE_ARRAY_CAPACITY(array, array##_n + 1);				\
	memmove(array+1, array, array##_n * sizeof(array[0]));			\
	array[0] = elt;								\
	array##_n++;								\
} while (0)

#define ARRAY_REMOVE(array, index)						\
do {										\
	CHECK_ARRAY_BOUNDS(array, index)					\
	memmove(&array[index], &array[index+1], 				\
			(array##_n-index-1) * sizeof(array[0]));		\
	array##_n--;								\
} while (0)

#define SHRINK_ARRAY(array)							\
do {										\
	if (array##_n == 0) {							\
		xfree(array);							\
		array = 0;							\
		array##_alloc = 0;						\
	} else if (array##_n < array##_alloc) {					\
		void *newmem = xmalloc(array##_n * sizeof(array[0]));		\
		memcpy(newmem, array, array##_n * sizeof(array[0]));		\
		xfree(array);							\
		array = newmem;							\
		array##_alloc = array##_n;					\
	}									\
} while (0)
