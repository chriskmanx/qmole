/* include/config.h.  Generated from config.h.in by configure.  */
/* File: "config.h.in" */

/*
 * Copyright (c) 1994-2012 by Marc Feeley, All Rights Reserved.
 */

#ifndef CONFIG_H
#define CONFIG_H

/*---------------------------------------------------------------------------*/

/*
 * These definitions are used by the "gambit.h" header file.
 */

/* Define as 1 if you want to compile for debugging.  */
/* #undef ___DEBUG */

/* Define as 1 if you want to compile with profiling.  */
/* #undef ___PROFILE */

/* Define as 1 if you want to compile each Scheme file into one C function.  */
/* #undef ___SINGLE_HOST */

/* Define as 1 if you want to compile as a shared library.  */
/* #undef ___SHARED */

/* Define as 1 if dynamically loaded code should not be unloaded.  */
/* #undef ___DONT_UNLOAD_DYN_CODE */

/*---------------------------------------------------------------------------*/

/* Determine which header files are available. */

#define HAVE_ERRNO_H 1
#define HAVE_UNISTD_H 1
#define HAVE_PWD_H 1
#define HAVE_DIRENT_H 1
#define HAVE_DLFCN_H 1
/* #undef HAVE_DL_H */
/* #undef HAVE_SYS_DXE_H */
#define HAVE_MACH_O_DYLD_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_TIMEB_H 1
/* #undef HAVE_SYS_TIMERS_H */
#define HAVE_TIME_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_SYS_MMAN_H 1
/* #undef HAVE_STAT_H */
#define HAVE_SIGNAL_H 1
#define HAVE_NETDB_H 1
#define HAVE_GRP_H 1
/* #undef HAVE_WINDOWS_H */
/* #undef HAVE_IO_H */
/* #undef HAVE_TCHAR_H */
#define HAVE_FLOAT_H 1
/* #undef HAVE_FPU_CONTROL_H */
/* #undef HAVE_OS2_H */
/* #undef HAVE_DOS_H */
/* #undef HAVE_DIRECT_H */
/* #undef HAVE_RETRACE_H */
/* #undef HAVE_FILES_H */
/* #undef HAVE_FINDER_H */
/* #undef HAVE_ERRORS_H */
/* #undef HAVE_FOLDERS_H */
/* #undef HAVE_OSUTILS_H */
/* #undef HAVE_POWER_H */
/* #undef HAVE_CODEFRAGMENTS_H */
/* #undef HAVE_SIOUX_H */
/* #undef HAVE_MAC_GUI_H */
/* #undef HAVE_UNIX_H */
/* #undef HAVE_WDEFWIN_H */
/* #undef HAVE_TFORK_H */
#define HAVE_CURSES_H 1
#define HAVE_NCURSES_H 1
#define HAVE_NETINET_IN_H 1
#define HAVE_ARPA_INET_H 1
#define HAVE_TERMIOS_H 1
/* #undef HAVE_TERM_H */
/* #undef HAVE_PTY_H */
/* #undef HAVE_STROPTS_H */
/* #undef HAVE_LIBUTIL_H */
#define HAVE_UTIL_H 1
/* #undef HAVE_SYS_FPU_H */
#define HAVE_FENV_H 1
#define HAVE_FCNTL_H 1
#define HAVE_SYS_IOCTL_H 1
#define HAVE_SYS_SOCKET_H 1
#define HAVE_STDIO_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_STRINGS_H 1
#define HAVE_MEMORY_H 1
#define HAVE_SYS_SYSCTL_H 1
/* #undef HAVE_CRT_EXTERNS_H */
/* #undef HAVE_WS2TCPIP_H */
#define HAVE_TARGETCONDITIONALS_H 1

/*---------------------------------------------------------------------------*/

/* Determine which library functions are available. */

#define HAVE_PIPE 1
#define HAVE_SOCKETPAIR 1
#define HAVE_CHDIR 1
#define HAVE_EXECVP 1
/* #undef HAVE_GETENV */
#define HAVE_GETGRNAM 1
#define HAVE_GETPID 1
#define HAVE_GETPPID 1
#define HAVE_GETPWNAM 1
#define HAVE_IOCTL 1
#define HAVE_LINK 1
#define HAVE_MKDIR 1
#define HAVE_MKFIFO 1
#define HAVE_OPENDIR 1
#define HAVE_RENAME 1
#define HAVE_RMDIR 1
#define HAVE_SOCKET 1
#define HAVE_STAT 1
#define HAVE_STAT64 1
#define HAVE_STRERROR 1
#define HAVE_SYMLINK 1
#define HAVE_SYSCONF 1
#define HAVE_SYSCTL 1
#define HAVE_UNLINK 1
#define HAVE_WAITPID 1
#define HAVE_MMAP 1

#define HAVE_TCGETATTR 1

#define HAVE_SIGACTION 1
#define HAVE_SIGEMPTYSET 1
#define HAVE_SIGADDSET 1
#define HAVE_SIGPROCMASK 1
#define HAVE_SIGNAL 1

/* #undef HAVE_CLOCK_GETTIME */
/* #undef HAVE_GETCLOCK */
/* #undef HAVE_GETSYSTEMTIME */
#define HAVE_GETTIMEOFDAY 1
#define HAVE_FTIME 1
#define HAVE_TIME 1

#define HAVE_NANOSLEEP 1
/* #undef HAVE_Sleep */
#define HAVE_SLEEP 1

/* #undef HAVE_GETPROCESSTIMES */
#define HAVE_GETRUSAGE 1
#define HAVE_TIMES 1
#define HAVE_CLOCK 1
/* #undef HAVE_DOSQUERYSYSINFO */

#define HAVE_SETITIMER 1
/* #undef HAVE_DOS_SETVECT */
/* #undef HAVE_DOSSTARTTIMER */
/* #undef HAVE_VINSTALL */
/* #undef HAVE_CREATETHREAD */

/* #undef HAVE_SHL_LOAD */
/* #undef HAVE_LOADLIBRARY */
/* #undef HAVE_DOSLOADMODULE */
/* #undef HAVE_DXE_LOAD */
/* #undef HAVE_GETDISKFRAGMENT */
#define HAVE_NSLINKMODULE 1
#define HAVE_DLOPEN 1

#define HAVE_GETHOSTNAME 1
#define HAVE_INET_PTON 1
#define HAVE_GETADDRINFO 1
#define HAVE_GETHOSTBYNAME 1
#define HAVE_GETHOSTBYADDR 1
#define HAVE_GETSERVBYNAME 1
#define HAVE_GETSERVBYPORT 1
#define HAVE_GETPROTOBYNAME 1
#define HAVE_GETPROTOBYNUMBER 1
#define HAVE_GETNETBYNAME 1

#define HAVE_SELECT 1
/* #undef HAVE_MSGWAITFORMULTIPLEOBJECTS */

/* #undef HAVE_TGETSTR */
/* #undef HAVE_TIGETSTR */

#define HAVE_OPENPTY 1
/* #undef HAVE_GETPT */
#define HAVE_PTSNAME 1
#define HAVE_CTERMID 1
/* #undef HAVE_ISASTREAM */

#define HAVE_HSTRERROR 1

/* #undef HAVE_GET_FPC_CSR */

#define HAVE_ENVIRON 1
/* #undef HAVE__NSGETENVIRON */

/* Determine which types are available */

/* #undef HAVE_STRUCT_STAT64 */

/* Define appropriately if sys/types.h does not define socklen_t.  */
/* #undef socklen_t */

/*
 * Define appropriately as the prefix and suffix added to function
 * names by the C compiler.
 */
/* #undef ___IMPORTED_ID_PREFIX */
/* #undef ___IMPORTED_ID_SUFFIX */

/*---------------------------------------------------------------------------*/

#endif
