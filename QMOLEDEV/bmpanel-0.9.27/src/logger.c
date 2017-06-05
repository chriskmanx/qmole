/*
 * Copyright (C) 2008 nsf
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "logger.h"

#define BUFFER_SIZE 4096
#define MAX_CALLBACKS 32

static const int verbosity_map[] = {
	LOG_ERRORS,
	LOG_WARNINGS,
	LOG_MESSAGES,
	LOG_INFOS,
	LOG_DEBUGS
};

static void (*log_callbacks[MAX_CALLBACKS])(int, const char*);
static int log_callbacks_count = 0;
static int log_verbosity = LOG_EVERYTHING;
static int file_callback_initialized = 0;

static void send_to_all(int level, const char *msg);

void log_msg(int level, const char *msg, ...)
{
	if (log_callbacks_count) {
		char buf[BUFFER_SIZE];
		va_list vl;

		if (log_verbosity & verbosity_map[level]) {
			va_start(vl, msg);
			vsnprintf(buf, sizeof(char)*BUFFER_SIZE, msg, vl);
			va_end(vl);

			send_to_all(level, buf);
		}
	}

	if (level == LOG_LEVEL_ERROR)	
		exit(1);
}

void log_attach_callback(void (*callback)(int, const char*))
{
	if (log_callbacks_count < MAX_CALLBACKS) {
		log_callbacks[log_callbacks_count] = callback;
		log_callbacks_count++;
	}
}

void log_clear_callbacks()
{
	int i;

	for (i = 0; i < log_callbacks_count; ++i)
		log_callbacks[i] = 0;

	log_callbacks_count = 0;
}

void log_set_verbosity(int level)
{
	log_verbosity = level;
}

void log_console_callback(int level, const char *msg)
{
	printf("%s\n", msg);
}

void log_console_color_callback(int level, const char *msg)
{
	static const struct { 
		int color; 
		char ch; 
	} log_console_color_params[] = {
		{1, '!'}, /* ERROR */
		{3, '!'}, /* WARNING */
		{2, '+'}, /* MESSAGE */
		{6, '-'}, /* INFO */
		{5, '*'}  /* DEBUG */
	};
	
	printf("\033[1m\033[30m[ \033[3%dm%c%c\033[0m \033[1m\033[30m]\033[0m ",
			log_console_color_params[level].color,
			log_console_color_params[level].ch,
			log_console_color_params[level].ch);
	printf("%s\n", msg);
}

void log_file_callback(int level, const char *msg)
{
	char buftime[256];
	FILE *f;
	time_t current_time;

	static const char *log_file_params[] = {
		"[EE]",
		"[WW]",
		"[MM]",
		"[II]",
		"[DB]"
	};

	if (!file_callback_initialized) {
		f = fopen(LOG_FILENAME, "w");
		file_callback_initialized = 1;
	} else
		f = fopen(LOG_FILENAME, "a");

	if (!f) 
		return;

	current_time = time(0);
	strftime(buftime, 256, "%d/%m/%y * %H:%M:%S", localtime(&current_time));
	fprintf(f, "%s | %s %s\n", buftime, log_file_params[level], msg);	
	fclose(f);
}

static void send_to_all(int level, const char *msg)
{
	int i;

	for (i = 0; i < log_callbacks_count; ++i)
		(*log_callbacks[i])(level, msg);
}

