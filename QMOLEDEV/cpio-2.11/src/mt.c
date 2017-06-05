/* mt -- control magnetic tape drive operation
   Copyright (C) 1991, 1992, 1995, 2001, 2007, 2010 Free Software
   Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301 USA
*/


/* If -f is not given, the environment variable TAPE is used;
   if that is not set, a default device defined in sys/mtio.h is used.
   The device must be either a character special file or a remote
   tape drive with the form "[user@]system:path".
   The default count is 1.  Some operations ignore it.

   Exit status:
   0	success
   1	invalid operation or device name
   2	operation failed

   Operations (unique abbreviations are accepted):
   eof, weof	Write COUNT EOF marks at current position on tape.
   fsf		Forward space COUNT files.
		Tape is positioned on the first block of the file.
   bsf		Backward space COUNT files.
		Tape is positioned on the first block of the file.
   fsr		Forward space COUNT records.
   bsr		Backward space COUNT records.
   bsfm		Backward space COUNT file marks.
		Tape is positioned on the beginning-of-the-tape side of
		the file mark.
   asf		Absolute space to file number COUNT.
		Equivalent to rewind followed by fsf COUNT.
   eom		Space to the end of the recorded media on the tape
		(for appending files onto tapes).
   rewind	Rewind the tape.
   offline, rewoffl
		Rewind the tape and, if applicable, unload the tape.
   status	Print status information about the tape unit.
   retension	Rewind the tape, then wind it to the end of the reel,
		then rewind it again.
   erase	Erase the tape.

   David MacKenzie <djm@gnu.ai.mit.edu> */

#include <system.h>

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#ifdef HAVE_SYS_MTIO_H
# ifdef HAVE_SYS_IO_TRIOCTL_H
#  include <sys/io/trioctl.h>
# endif
# include <sys/mtio.h>
#endif
#include <sys/file.h>
#include <fcntl.h>
#include <errno.h>
#include <stdbool.h>
#include <argp.h>
#include <argp-version-etc.h>
#include <progname.h>

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif
#include <rmt-command.h>

#include <rmt.h>

#include <argmatch.h>
#include <paxlib.h>
#include "configmake.h"

#define MT_EXIT_SUCCESS 0
#define MT_EXIT_INVOP   1
#define MT_EXIT_FAILURE 2

char const * const opnames[] =
{
  "eof",
  "weof",
  "fsf",
  "bsf",
  "fsr",
  "bsr",
  "rewind",
  "offline",
  "rewoffl",
  "eject",
  "status",
#ifdef MTBSFM
  "bsfm",
#endif
#ifdef MTEOM
  "eom",
#endif
#ifdef MTRETEN
  "retension",
#endif
#ifdef MTERASE
  "erase",
#endif
  "asf",
#ifdef MTFSFM
  "fsfm",
#endif
#ifdef MTSEEK
  "seek",
#endif
  NULL
};

#define MTASF 600		/* Random unused number.  */
short operations[] =
{
  MTWEOF,
  MTWEOF,
  MTFSF,
  MTBSF,
  MTFSR,
  MTBSR,
  MTREW,
  MTOFFL,
  MTOFFL,
  MTOFFL,
  MTNOP,
#ifdef MTBSFM
  MTBSFM,
#endif
#ifdef MTEOM
  MTEOM,
#endif
#ifdef MTRETEN
  MTRETEN,
#endif
#ifdef MTERASE
  MTERASE,
#endif
  MTASF,
#ifdef MTFSFM
  MTFSFM,
#endif
#ifdef MTSEEK
  MTSEEK,
#endif
};

ARGMATCH_VERIFY (opnames, operations);

const char *argp_program_bug_address = "<" PACKAGE_BUGREPORT ">";
static char doc[] = N_("control magnetic tape drive operation");
const char *program_authors[] =
  {
    "David MacKenzie",
    "Sergey Poznyakoff",
    NULL
  };

enum
  {
    RSH_COMMAND_OPTION = 256
  };

static struct argp_option options[] = {
  { "file", 'f', N_("DEVICE"), 0,
    N_("use device as the file name of the tape drive to operate on") },
  { "rsh-command", RSH_COMMAND_OPTION, N_("COMMAND"), 0,
    N_("use remote COMMAND instead of rsh") },
  { NULL }
};

char *tapedev;                   /* tape device */
char *rsh_command_option = NULL; /* rsh command */
short operation;                 /* operation code */ 
int count = 1;                   /* count */

int argcnt = 0;                  /* number of command line arguments
				    processed so far */

static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case ARGP_KEY_ARG:
      switch (argcnt++)
	{
	case 0:
	  operation = XARGMATCH (N_("operation"), arg, opnames, operations);
	  break;

	case 1:
	  {
	    char *p;
	    long val = strtol (arg, &p, 0);
	    if (*p || (count = val) != count)
	      error (MT_EXIT_INVOP, 0, _("invalid count value"));
	  }
	  break;

	default:
	  argp_usage (state);
	}
      break;

    case ARGP_KEY_FINI:
      if (argcnt == 0)
	argp_usage (state);
      if (tapedev == NULL)
	{
	  tapedev = getenv ("TAPE");
	  if (tapedev == NULL)
#ifdef DEFTAPE			/* From sys/mtio.h.  */
	    tapedev = DEFTAPE;
#else
	  error (MT_EXIT_INVOP, 0, _("no tape device specified"));
#endif
	}
      break;
      
    case 'f':
    case 't':
      tapedev = arg;
      break;

    case RSH_COMMAND_OPTION:
      rsh_command_option = arg;
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

static struct argp argp = {
  options,
  parse_opt,
  N_("operation [count]"),
  doc,
  NULL,
  NULL,
  NULL
};

void
check_type (char *dev, int desc)
{
  struct stat stats;

  if (_isrmt (desc))
    return;
  if (fstat (desc, &stats) == -1)
    stat_error (dev);
  if ((stats.st_mode & S_IFMT) != S_IFCHR)
    error (MT_EXIT_INVOP, 0, _("%s is not a character special file"), dev);
}

void
perform_operation (char *dev, int desc, short op, int count)
{
  struct mtop control;

  control.mt_op = op;
  control.mt_count = count;
  if (rmtioctl (desc, MTIOCTOP, (char*)&control) == -1)
    error (MT_EXIT_FAILURE, errno, _("%s: rmtioctl failed"), dev);
}

void
print_status (char *dev, int desc)
{
  struct mtget status;

  if (rmtioctl (desc, MTIOCGET, (char*)&status) == -1)
    error (MT_EXIT_FAILURE, errno, _("%s: rmtioctl failed"), dev);

  printf ("drive type = %d\n", (int) status.mt_type);
#if defined(hpux) || defined(__hpux)
  printf ("drive status (high) = %d\n", (int) status.mt_dsreg1);
  printf ("drive status (low) = %d\n", (int) status.mt_dsreg2);
#else
  printf ("drive status = %d\n", (int) status.mt_dsreg);
#endif
  printf ("sense key error = %d\n", (int) status.mt_erreg);
  printf ("residue count = %d\n", (int) status.mt_resid);
#if !defined(ultrix) && !defined(__ultrix__) && !defined(hpux) && !defined(__hpux) && !defined(__osf__)
  printf ("file number = %d\n", (int) status.mt_fileno);
  printf ("block number = %d\n", (int) status.mt_blkno);
#endif
}

void
fatal_exit ()
{
  exit (MT_EXIT_INVOP);
}

int
main (int argc, char **argv)
{
  int tapedesc;

  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
  
  set_program_name (argv[0]);
  argp_version_setup ("mt", program_authors);
  argmatch_die = fatal_exit;
  argp_err_exit_status = MT_EXIT_INVOP;
  if (argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, NULL))
    exit (MT_EXIT_INVOP);

  switch (operation)
    {
    case MTWEOF:
#ifdef MTERASE
    case MTERASE:
#endif
      tapedesc = rmtopen (tapedev, O_WRONLY, 0, rsh_command_option);
      break;

    default:
      tapedesc = rmtopen (tapedev, O_RDONLY, 0, rsh_command_option);
    }
  
  if (tapedesc == -1)
    error (MT_EXIT_INVOP, errno, _("%s: rmtopen failed"), tapedev);
  check_type (tapedev, tapedesc);

  if (operation == MTASF)
    {
      perform_operation (tapedev, tapedesc, MTREW, 1);
      operation = MTFSF;
    }
  perform_operation (tapedev, tapedesc, operation, count);
  if (operation == MTNOP)
    print_status (tapedev, tapedesc);

  if (rmtclose (tapedesc) == -1)
    error (MT_EXIT_FAILURE, errno, _("%s: rmtclose failed"), tapedev);

  exit (MT_EXIT_SUCCESS);
}

