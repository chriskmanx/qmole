#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include "util.h"

struct memory_source msrc_default = MEMSRC(
	"Default", 
	MEMSRC_DEFAULT_MALLOC, 
	MEMSRC_DEFAULT_FREE, 
	MEMSRC_NO_FLAGS
);

/**************************************************************************
  No debug
**************************************************************************/
#ifdef NDEBUG
void *impl_xmalloc(size_t size, struct memory_source *src)
{
	void *ret = 0;

	if (src->malloc) {
		if (src->flags & MEMSRC_RETURN_IMMEDIATELY)
			return (*src->malloc)(size, src);
		else
			ret = (*src->malloc)(size + MEMDEBUG_OVERHEAD, src);
	}
		
	if (!ret)
		ret = malloc(size + MEMDEBUG_OVERHEAD);

	if (!ret)
		XDIE("Out of memory, xmalloc failed.");

	return ret;
}

void *impl_xmallocz(size_t size, struct memory_source *src)
{
	void *ret = impl_xmalloc(size, src);
	memset(ret, 0, size);
	return ret;
}

void impl_xfree(void *ptr, struct memory_source *src)
{
	if (src->free)
		(*src->free)(ptr, src);
	else	
		free(ptr);
}

char *impl_xstrdup(const char *str, struct memory_source *src)
{
	size_t len = strlen(str);
	char *ret = impl_xmalloc(len+1, src);
	return strcpy(ret, str);
}
#else
/**************************************************************************
  Memory debug
**************************************************************************/
void *impl_xmalloc(size_t size, struct memory_source *src, const char *file, unsigned int line)
{
	void *ret = 0;

	if (src->malloc) {
		if (src->flags & MEMSRC_RETURN_IMMEDIATELY)
			return (*src->malloc)(size, src);
		else
			ret = (*src->malloc)(size + MEMDEBUG_OVERHEAD, src);
	}
	
	if (!ret)
		ret = malloc(size + MEMDEBUG_OVERHEAD);

	if (!ret)
		XDIE("Out of memory, xmalloc(z) failed.");

	struct memory_stat *stat = (struct memory_stat*)ret;
	stat->file = file;
	stat->line = line;
	stat->size = size;
	stat->prev = 0;

	if (!src->stat_list) {
		stat->next = 0;
		src->stat_list = stat;
	} else {
		stat->next = src->stat_list;
		src->stat_list->prev = stat;
		src->stat_list = stat;
	}
	src->allocs++;
	src->bytes += size;

	ret += sizeof(struct memory_stat);
	return ret;
}

void *impl_xmallocz(size_t size, struct memory_source *src, const char *file, unsigned int line)
{
	void *ret = impl_xmalloc(size, src, file, line);
	memset(ret, 0, size);
	return ret;
}

void impl_xfree(void *ptr, struct memory_source *src)
{
	if (src->free && (src->flags & MEMSRC_RETURN_IMMEDIATELY)) {
		(*src->free)(ptr, src);
		return;
	}

	struct memory_stat *memstat = ptr - sizeof(struct memory_stat);
	
	if (memstat->next)
		memstat->next->prev = (memstat->prev) ? memstat->prev : 0;
	if (memstat->prev)
		memstat->prev->next = (memstat->next) ? memstat->next : 0;
	if (src->stat_list == memstat)
		src->stat_list = memstat->next;

	src->frees++;
	src->bytes -= memstat->size;
	
	if (src->free)
		(*src->free)(memstat, src);
	else
		free(memstat);
}

char *impl_xstrdup(const char *str, struct memory_source *src, const char *file, unsigned int line)
{
	size_t len = strlen(str);
	char *ret = impl_xmalloc(len+1, src, file, line);
	return strcpy(ret, str);
}
/**************************************************************************
  Debug report utils
**************************************************************************/
#ifndef MEMDEBUG_ASCII_STATS
static void print_source_stat(struct memory_source *src, int details)
{
	int diff = (int)src->allocs - src->frees;

	printf("┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
	printf("┃ Source: %-63s ┃\n", src->name);
	printf("┠─────────────────────────────────────────────────────────────────────────┨\n");
	printf("┃ Allocs:      %-58u ┃\n", src->allocs);
	printf("┃ Frees:       %-58u ┃\n", src->frees);
	printf("┃ Diff:        %-58d ┃\n", diff);
	printf("┃ Bytes taken: %-58u ┃\n", src->bytes);
	printf("┃  + overhead: %-58d ┃\n", diff * (int)MEMDEBUG_OVERHEAD);
	if (!diff || !src->stat_list || !details) {
		printf("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
	} else {
		printf("┠────────────┬────────────┬───────────────────────────────────────────────┨\n");
		printf("┃     ptr    │    size    │                   location                    ┃\n");
		printf("┠────────────┼────────────┼───────────────────────────────────────────────┨\n");
		struct memory_stat *stat = src->stat_list;
		while (stat) {
			char location[50];
			snprintf(location, sizeof(location), "%s:%u", stat->file, stat->line);
			location[sizeof(location)-1] = '\0';
			printf("┃ %10p │ %10zu │ %-45s ┃\n", stat+1, stat->size, 
				location);
			stat = stat->next;
		}
		printf("┗━━━━━━━━━━━━┷━━━━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
	}
}
#else
static void print_source_stat(struct memory_source *src, int details)
{
	int diff = (int)src->allocs - src->frees;

	printf("/=========================================================================\\\n");
	printf("| Source: %-63s |\n", src->name);
	printf("+-------------------------------------------------------------------------+\n");
	printf("| Allocs:      %-58u |\n", src->allocs);
	printf("| Frees:       %-58u |\n", src->frees);
	printf("| Diff:        %-58d |\n", diff);
	printf("| Bytes taken: %-58u |\n", src->bytes);
	printf("|  + overhead: %-58d |\n", diff * MEMDEBUG_OVERHEAD);
	if (!diff || !src->stat_list || !details) {
		printf("\\=========================================================================/\n");
	} else {
		printf("+------------+------------+-----------------------------------------------+\n");
		printf("|     ptr    |    size    |                   location                    |\n");
		printf("+------------+------------+-----------------------------------------------+\n");
		struct memory_stat *stat = src->stat_list;
		while (stat) {
			char location[50];
			snprintf(location, sizeof(location), "%s:%u", stat->file, stat->line);
			location[sizeof(location)-1] = '\0';
			printf("| %10p | %10u | %-45s |\n", stat+1, stat->size, 
				location);
			stat = stat->next;
		}
		printf("\\============+============+===============================================/\n");
	}
}
#endif /* #ifndef else MEMDEBUG_ASCII_STATS */
#endif /* #ifdef else NDEBUG */

void xmemstat(struct memory_source **sources, size_t n, int details)
{
#ifndef NDEBUG
	size_t i;

	printf("\033[32m");
	print_source_stat(&msrc_default, details);
	
	for (i = 0; i < n; ++i) {
		if (i % 2)
			printf("\033[36m");
		else 
			printf("\033[0m");

		print_source_stat(sources[i], details);
	}
	printf("\033[0m");
	fflush(stdout);
#endif
}
