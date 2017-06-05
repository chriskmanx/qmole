/*
 * Copyright (C) 2008 nsf
 */

#ifndef NSF_LOGGER_H
#define NSF_LOGGER_H

/* log levels */
#define LOG_LEVEL_ERROR		0
#define LOG_LEVEL_WARNING	1
#define	LOG_LEVEL_MESSAGE	2
#define	LOG_LEVEL_INFO		3
#define	LOG_LEVEL_DEBUG		4
#define LOG_LEVEL_NUM		5

/* log verbosity flags */
#define LOG_ERRORS		0x0001
#define LOG_WARNINGS 		0x0002
#define LOG_MESSAGES 		0x0004
#define LOG_INFOS 		0x0008
#define LOG_DEBUGS 		0x0010
#define LOG_EVERYTHING 		0xFFFF /* default */

/* log filename for log_file_receiver() */
#define LOG_FILENAME 		"log-bmpanel.log"

/* main log routines */
void log_msg(int, const char*, ...);
void log_attach_callback(void (*)(int, const char*));
void log_clear_callbacks();
void log_set_verbosity(int);

/* built-in receivers */
void log_console_callback(int, const char*);
void log_console_color_callback(int, const char*);
void log_file_callback(int, const char*);

/* useful macros */
#define LOG_MESSAGE(...) 	log_msg(LOG_LEVEL_MESSAGE, __VA_ARGS__)
#define LOG_ERROR(...) 		log_msg(LOG_LEVEL_ERROR, __VA_ARGS__)
#define LOG_WARNING(...) 	log_msg(LOG_LEVEL_WARNING, __VA_ARGS__)
#define LOG_INFO(...) 		log_msg(LOG_LEVEL_INFO, __VA_ARGS__)
#define LOG_DEBUG(...) 		log_msg(LOG_LEVEL_DEBUG, __VA_ARGS__)

#ifdef LOG_ASSERT_ENABLED
 #define LOG_ASSERT(what) ((what) ? ((void)0) : LOG_WARNING("assertion failed: " #what " (%s:%d)", __FILE__, __LINE__))
#else
 #define LOG_ASSERT(nothing) ((void)0)
#endif

#endif
