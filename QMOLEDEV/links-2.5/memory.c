/* memory.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"

struct cache_upcall {
	struct cache_upcall *next;
	struct cache_upcall *prev;
	int (*upcall)(int);
	unsigned char name[1];
};

static struct list_head cache_upcalls = { &cache_upcalls, &cache_upcalls }; /* cache_upcall */

int shrink_memory(int type)
{
	struct cache_upcall *c;
	int a = 0;
	foreach(c, cache_upcalls) a |= c->upcall(type);
	if (a & ST_SOMETHING_FREED) {
#if defined(HAVE__HEAPMIN)
		_heapmin();
#endif
	}
	return a;
}

void register_cache_upcall(int (*upcall)(int), unsigned char *name)
{
	struct cache_upcall *c;
	c = mem_alloc(sizeof(struct cache_upcall) + strlen(name) + 1);
	c->upcall = upcall;
	strcpy(c->name, name);
	add_to_list(cache_upcalls, c);
}

void free_all_caches(void)
{
	struct cache_upcall *c;
	int a, b;
	do {
		a = 0;
		b = ~0;
		foreach(c, cache_upcalls) {
			int x = c->upcall(SH_FREE_ALL);
			a |= x;
			b &= x;
		}
	} while (a & ST_SOMETHING_FREED);
	if (!(b & ST_CACHE_EMPTY)) {
		unsigned char *m = init_str();
		int l = 0;
		foreach(c, cache_upcalls) if (!(c->upcall(SH_FREE_ALL) & ST_CACHE_EMPTY)) {
			if (l) add_to_str(&m, &l, ", ");
			add_to_str(&m, &l, c->name);
		}
		internal("could not release entries from caches: %s", m);
		mem_free(m);
	}
	free_list(cache_upcalls);
}

int malloc_try_hard = 0;

int out_of_memory(unsigned char *msg, size_t size)
{
	int sh = shrink_memory(SH_FREE_SOMETHING);
	if (sh & ST_SOMETHING_FREED) return 1;
	if (!malloc_try_hard) {
		malloc_try_hard = 1;
		return 1;
	}
	if (!msg) return 0;

	fprintf(stderr, "File cache: %lu bytes, %lu files, %lu locked, %lu loading\n", cache_info(CI_BYTES), cache_info(CI_FILES), cache_info(CI_LOCKED), cache_info(CI_LOADING));
#ifdef G
	if (F) {
		fprintf(stderr, "Image cache: %lu bytes, %lu files, %lu locked\n", imgcache_info(CI_BYTES), imgcache_info(CI_FILES), imgcache_info(CI_LOCKED));
	}
#endif
	fprintf(stderr, "Formatted document cache: %lu documents, %lu locked\n", formatted_info(CI_FILES), formatted_info(CI_LOCKED));

	error("ERROR: out of memory (%s(%lu) returned NULL)", msg, (unsigned long)size);
	fatal_tty_exit();
	exit(RET_FATAL);
	return 0;
}

#ifdef DEBUG_TEST_FREE

struct debug_test_free_slot {
	struct debug_test_free_slot *next;
	struct debug_test_free_slot *prev;
	unsigned char *file;
	int line;
	unsigned long count;
};

static struct list_head debug_test_free_slots = {&debug_test_free_slots, &debug_test_free_slots};

#define DEBUG_TEST_FREE_DEFAULT_PROB	1024
#define DEBUG_TEST_FREE_INIT_COUNT	16

void debug_test_free(unsigned char *file, int line)
{
	struct debug_test_free_slot *sl = NULL;
	unsigned long prob;
	if (!file) {
		prob = DEBUG_TEST_FREE_DEFAULT_PROB;
		goto fixed_prob;
	}
	foreach(sl, debug_test_free_slots) {
		if (sl->line == line && (sl->file == file || !strcmp(sl->file, file))) {
			del_from_list(sl);
			goto have_it;
		}
	}
	retry:
	sl = malloc(sizeof(struct debug_test_free_slot));
	if (!sl) {
		if (out_of_memory(NULL, 0))
			goto retry;
		return;
	}
	sl->file = file;
	sl->line = line;
	sl->count = DEBUG_TEST_FREE_INIT_COUNT;
	have_it:
	add_to_list(debug_test_free_slots, sl);
	prob = sl->count;
	sl->count++;

	fixed_prob:
	if (!prob) prob = 1;
	if (!(random() % prob)) {
		if (shrink_memory(SH_FREE_SOMETHING) & ST_SOMETHING_FREED) {
			/*if (sl) sl->count++;*/
		}
	}
}

#endif
