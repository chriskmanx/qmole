#include <stdarg.h>
#include <stdio.h>
#include "util.h"

int xerror(const char *file, unsigned int line, const char *fmt, ...)
{
	va_list args;

#ifndef NDEBUG
	fprintf(stderr, "(%s:%u) ", file, line);
#endif
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	fputs("\n", stderr);
	va_end(args);

	return -1;
}

void xwarning(const char *file, unsigned int line, const char *fmt, ...)
{
	va_list args;

#ifndef NDEBUG
	fprintf(stderr, "(%s:%u) ", file, line);
#endif
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	fputs("\n", stderr);
	va_end(args);
}

void xdie(const char *file, unsigned int line, const char *fmt, ...)
{
	va_list args;

#ifndef NDEBUG
	fprintf(stderr, "FATAL (%s:%u) ", file, line);
#endif
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	fputs("\n", stderr);
	va_end(args);
	exit(EXIT_FAILURE);
}

const char *pretty_print_FILE(const char *file)
{
	const char *base = strstr(file, PRETTY_PRINT_FILE_BASE);
	return ((base) ? base + strlen(PRETTY_PRINT_FILE_BASE) + 1 : file);
}

