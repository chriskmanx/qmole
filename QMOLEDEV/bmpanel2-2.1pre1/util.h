#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define STRINGIZE_(x) #x
#define STRINGIZE(x) STRINGIZE_(x)

#define MAX_ALLOCA 2048

#define CLEAR_STRUCT(s) ((void)memset((s), 0, sizeof(*(s)))) 

#ifdef NDEBUG
	#define ENSURE(cond, ...) ((void)0)
#else
 	#define ENSURE(cond, ...)					\
	do {								\
		if (!(cond)) {						\
			fprintf(stderr, "%s (%s:%d): ", STRINGIZE(cond),\
				pretty_print_FILE(__FILE__), __LINE__);	\
			fprintf(stderr, __VA_ARGS__);			\
			fprintf(stderr, "\n");				\
			exit(EXIT_FAILURE);				\
		}							\
	} while (0)
#endif

static inline int is_file_exists(const char *file)
{
	FILE *f = fopen(file, "r");
	if (f) {
		fclose(f);
		return 1;
	}
	return 0;
}

/**************************************************************************
  string buffer
**************************************************************************/

struct strbuf {
	char *buf;
	size_t alloc;
};

void strbuf_assign(struct strbuf *sb, const char *str);
void strbuf_free(struct strbuf *sb);

/**************************************************************************
  message utils
**************************************************************************/

#define XERROR(...) xerror(pretty_print_FILE(__FILE__), __LINE__, __VA_ARGS__)
#define XWARNING(...) xwarning(pretty_print_FILE(__FILE__), __LINE__, __VA_ARGS__)
#define XDIE(...) xdie(pretty_print_FILE(__FILE__), __LINE__, __VA_ARGS__)

int xerror(const char *file, unsigned int line, const char *fmt, ...);
void xwarning(const char *file, unsigned int line, const char *fmt, ...);
void xdie(const char *file, unsigned int line, const char *fmt, ...);

#define PRETTY_PRINT_FILE_BASE "bmpanel2"
const char *pretty_print_FILE(const char *file);

/**************************************************************************
  memory utils
**************************************************************************/

/* Memory source helper macro */
#define MEMSRC(name, malloc, free, flags) \
	{name, 0, 0, 0, 0, (malloc), (free), (flags)}

/* Defaults for convenience. */
#define MEMSRC_DEFAULT_MALLOC (0)
#define MEMSRC_DEFAULT_FREE (0)
#define MEMSRC_NO_FLAGS (0)

/* 
 * When this flag is set "x" alloc functions directly return the result of
 * custom malloc (defined in a memory_source) without any other activity. Also
 * allocs pass exact requested size to this custom function (without
 * MEMDEBUG_OVERHEAD added). If a memory source have no custom malloc
 * function, the flag is ignored.
 */
#define MEMSRC_RETURN_IMMEDIATELY (1 << 0)

struct memory_stat {
	struct memory_stat *next;
	struct memory_stat *prev;
	const char *file;
	unsigned int line;
	size_t size;
};

struct memory_source {
	const char *name;
	unsigned int allocs;
	unsigned int frees;
	int bytes;
	struct memory_stat *stat_list;

	void *(*malloc)(size_t, struct memory_source*);
	void (*free)(void*, struct memory_source*);

	unsigned int flags;
};

/* overheads */
#ifdef NDEBUG
	#define MEMDEBUG_OVERHEAD (0)
#else
	#define MEMDEBUG_OVERHEAD (sizeof(struct memory_stat))
#endif

extern struct memory_source msrc_default;

/* functions */
#ifdef NDEBUG
	#define xmalloc(a) 	impl_xmalloc((a), &msrc_default)
	#define xmallocz(a) 	impl_xmallocz((a), &msrc_default)
	#define xfree(a) 	impl_xfree((a), &msrc_default)
	#define xstrdup(a) 	impl_xstrdup((a), &msrc_default)
	
	#define xmalloc_from_source(a, s) 	impl_xmalloc((a), (s))
	#define xmallocz_from_source(a, s) 	impl_xmallocz((a), (s))
	#define xfree_from_source(a, s) 	impl_xfree((a), (s))
	#define xstrdup_from_source(a, s) 	impl_xstrdup((a), (s))
	
	void *impl_xmalloc(size_t size, struct memory_source *src);
	void *impl_xmallocz(size_t size, struct memory_source *src);
	void impl_xfree(void *ptr, struct memory_source *src);
	char *impl_xstrdup(const char *str, struct memory_source *src);
#else
	#define xmalloc(a) 	impl_xmalloc((a), &msrc_default, __FILE__, __LINE__)
	#define xmallocz(a) 	impl_xmallocz((a), &msrc_default, __FILE__, __LINE__)
	#define xfree(a) 	impl_xfree((a), &msrc_default)
	#define xstrdup(a) 	impl_xstrdup((a), &msrc_default, __FILE__, __LINE__)
	
	#define xmalloc_from_source(a, s) 	impl_xmalloc((a), (s), __FILE__, __LINE__)
	#define xmallocz_from_source(a, s) 	impl_xmallocz((a), (s), __FILE__, __LINE__)
	#define xfree_from_source(a, s) 	impl_xfree((a), (s))
	#define xstrdup_from_source(a, s) 	impl_xstrdup((a), (s), __FILE__, __LINE__)

	void *impl_xmalloc(size_t size, struct memory_source *src, const char *file, unsigned int line);
	void *impl_xmallocz(size_t size, struct memory_source *src, const char *file, unsigned int line);
	void impl_xfree(void *ptr, struct memory_source *src);
	char *impl_xstrdup(const char *str, struct memory_source *src, const char *file, unsigned int line);
#endif

/* #define MEMDEBUG_ASCII_STATS 1 */ 
/*
 * Prints out an info table about memory sources array "sources" of size "n".
 * "details" boolean for detailed statistics (memleaks).
 */
void xmemstat(struct memory_source **sources, size_t n, int details);
