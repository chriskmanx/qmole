/*
 * Copyright (C) 2008 nsf
 */

#include <string.h>
#include <stdio.h>
#include "logger.h"
#include "common.h"

static int memleaks;

#ifndef MEMDEBUG
/**************************************************************************
  local memory routines (release)
**************************************************************************/

void *impl__xmalloc(size_t size)
{
	void *ret = malloc(size);
	if (!ret)
		LOG_ERROR("common: out of memory, malloc failed >:-O");

	memleaks++;
	return ret;
}

void *impl__xmallocz(size_t size)
{
	void *ret = impl__xmalloc(size);
	memset(ret, 0, size);
	return ret;
}

void impl__xfree(void *ptr)
{
	free(ptr);
	memleaks--;
}

char *impl__xstrdup(const char *str)
{
	size_t strl = strlen(str);
	char *ret = impl__xmalloc(strl+1);
	return strcpy(ret, str);
}

void xmemleaks()
{
	LOG_DEBUG("common: memory leaks = %d", memleaks);
}

#else
/**************************************************************************
  local memory routines (debug)
**************************************************************************/

/* TODO: deal with low deletion performance? O(N) really bad? */

struct mem_entry {
	void *ptr;
	size_t size;
	const char *file;
	uint line;
	struct mem_entry *next;
};

static struct mem_entry *entries;
static void add_mem_entry(void *ptr, size_t size, const char *file, uint line);
static void del_mem_entry(void *ptr, const char *file, uint line);

void *impl__xmalloc(size_t size, const char *file, uint line)
{
	void *ret = malloc(size);
	if (!ret)
		LOG_ERROR("common: out of memory, malloc failed >:-O");

	add_mem_entry(ret, size, file, line);
	memleaks++;
	return ret;
}

void *impl__xmallocz(size_t size, const char *file, uint line)
{
	void *ret = impl__xmalloc(size, file, line);
	memset(ret, 0, size);
	return ret;
}

void impl__xfree(void *ptr, const char *file, uint line)
{
	del_mem_entry(ptr, file, line);
	free(ptr);
	memleaks--;
}

char *impl__xstrdup(const char *str, const char *file, uint line)
{
	size_t strl = strlen(str);
	char *ret = impl__xmalloc(strl+1, file, line);
	return strcpy(ret, str);
}

static void add_mem_entry(void *ptr, size_t size, const char *file, uint line)
{
	struct mem_entry *e = malloc(sizeof(struct mem_entry));
	e->ptr = ptr;
	e->size = size;
	e->file = file;
	e->line = line;

	if (!entries) {
		entries = e;
		e->next = 0;
	} else {
		e->next = entries;
		entries = e;
	}
}

static void del_mem_entry(void *ptr, const char *file, uint line)
{
	struct mem_entry *prev, *current;
	prev = current = entries;
	while (current) {
		if (current->ptr == ptr)
			break;
		prev = current;
		current = current->next;
	}

	if (!current) {
		LOG_WARNING("common: cannot find previously allocated memory entry, "
			"wrong pointer (%p) to xfree or double free [%s:%u]", ptr, file, line);
		return;
	}
		
	if (current == entries)
		entries = current->next;
	else 
		prev->next = current->next;
	free(current);
}

void xmemleaks()
{
	LOG_DEBUG("common: dumping memory leaks (count: %u) table...", memleaks);
	LOG_DEBUG("+------------+------------+-------------------------+-------+");
	LOG_DEBUG("|     ptr    |    size    |           file          | line  |");
	LOG_DEBUG("+------------+------------+-------------------------+-------+");
	struct mem_entry *cur = entries;
	if (!cur)
		LOG_DEBUG("| there are no memory leaks                                 |");
	while (cur) {
		LOG_DEBUG("| %10p | %10u | %-23s | %-5u |", cur->ptr, cur->size, 
				cur->file, cur->line);
		cur = cur->next;
	}
	LOG_DEBUG("+------------+------------+-------------------------+-------+");
}
#endif

