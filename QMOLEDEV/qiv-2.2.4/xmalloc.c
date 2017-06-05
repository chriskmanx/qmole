/* xmalloc.c - Do-or-die Memory management functions.
 * 
 * Created by Kevin Locke (from numerous canonical examples)
 *
 * I hereby place this file in the public domain.  It may be freely reproduced,
 * distributed, used, modified, built upon, or otherwise employed by anyone
 * for any purpose without restriction.
 */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#endif

void *xmalloc(size_t size)
{
	void *allocated = malloc(size);

	if (allocated == NULL) {
		fprintf(stderr, "Error:  Insufficient memory "
# if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
				"(attempt to malloc %zu bytes)\n",
#else
				"(attempt to malloc %u bytes)\n",
#endif
				 (unsigned int) size);
		exit(EXIT_FAILURE);
	}

	return allocated;
}

void *xcalloc(size_t num, size_t size)
{
	void *allocated = calloc(num, size);

	if (allocated == NULL) {
		fprintf(stderr, "Error:  Insufficient memory "
# if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
				"(attempt to calloc %zu bytes)\n",
#else
				"(attempt to calloc %u bytes)\n",
#endif
				 (unsigned int) size);
		exit(EXIT_FAILURE);
	}

	return allocated;
}

void *xrealloc(void *ptr, size_t size)
{
	void *allocated;

	/* Protect against non-standard behavior */
	if (ptr == NULL) {
		allocated = malloc(size);
	} else {
		allocated = realloc(ptr, size);
	}

	if (allocated == NULL) {
		fprintf(stderr, "Error:  Insufficient memory "
# if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
				"(attempt to realloc %zu bytes)\n",
#else
				"(attempt to realloc %u bytes)\n",
#endif
				 (unsigned int) size);
		exit(EXIT_FAILURE);
	}

	return allocated;
}
