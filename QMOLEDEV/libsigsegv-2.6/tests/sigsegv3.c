/* Test that the handler can be exited multiple times.
   Copyright (C) 2002-2006, 2008  Bruno Haible <bruno@clisp.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _MSC_VER
# include <config.h>
#endif

#include "sigsegv.h"
#include <stdio.h>

#if HAVE_SIGSEGV_RECOVERY

#if defined _WIN32 && !defined __CYGWIN__
  /* Windows doesn't have sigset_t.  */
  typedef int sigset_t;
# define sigemptyset(set)
# define sigprocmask(how,set,oldset)
#endif

#include "mmaputil.h"
#include <stdlib.h> /* for abort, exit */
#include <signal.h>
#include <setjmp.h>

jmp_buf mainloop;
sigset_t mainsigset;

volatile int pass = 0;
unsigned long page;

volatile int handler_called = 0;

static void
handler_continuation (void *arg1, void *arg2, void *arg3)
{
  longjmp (mainloop, pass);
}

int
handler (void *fault_address, int serious)
{
  handler_called++;
  if (handler_called > 10)
    abort ();
  if (fault_address != (void *)(page + 0x678 + 8 * pass))
    abort ();
  pass++;
  printf ("Stack overflow %d caught.\n", pass);
  sigprocmask (SIG_SETMASK, &mainsigset, NULL);
  return sigsegv_leave_handler (handler_continuation, NULL, NULL, NULL);
}

void
crasher (unsigned long p)
{
  *(volatile int *) (p + 0x678 + 8 * pass) = 42;
}

int
main ()
{
  sigset_t emptyset;
  void *p;

  /* Preparations.  */
#if !HAVE_MMAP_ANON && !HAVE_MMAP_ANONYMOUS && HAVE_MMAP_DEVZERO
  zero_fd = open ("/dev/zero", O_RDONLY, 0644);
#endif

  /* Setup some mmaped memory.  */
  p = mmap_zeromap ((void *) 0x12340000, 0x4000);
  if (p == (void *)(-1))
    {
      fprintf (stderr, "mmap_zeromap failed.\n");
      exit (2);
    }
  page = (unsigned long) p;

  /* Make it read-only.  */
  if (mprotect ((void *) page, 0x4000, PROT_READ) < 0)
    {
      fprintf (stderr, "mprotect failed.\n");
      exit (2);
    }

  /* Install the SIGSEGV handler.  */
  if (sigsegv_install_handler (&handler) < 0)
    exit (2);

  /* Save the current signal mask.  */
  sigemptyset (&emptyset);
  sigprocmask (SIG_BLOCK, &emptyset, &mainsigset);

  /* Provoke two SIGSEGVs in a row.  */
  switch (setjmp (mainloop))
    {
    case 0: case 1:
      printf ("Doing SIGSEGV pass %d.\n", pass + 1);
      crasher (page);
      printf ("no SIGSEGV?!\n"); exit (1);
    case 2:
      break;
    default:
      abort ();
    }

  /* Test passed!  */
  printf ("Test passed.\n");
  return 0;
}

#else

int
main ()
{
  return 77;
}

#endif
