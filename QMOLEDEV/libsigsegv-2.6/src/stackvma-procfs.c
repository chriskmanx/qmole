/* Determine the virtual memory area of a given address.
   Copyright (C) 2002, 2006, 2008  Bruno Haible <bruno@clisp.org>

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

#include "stackvma.h"
#include <unistd.h> /* open, close */
#include <fcntl.h> /* open */
#include <sys/types.h>
#include <sys/mman.h> /* mmap, munmap */
#include <sys/procfs.h> /* PIOC*, prmap_t */

#include "stackvma-simple.c"

#if HAVE_MINCORE
# define sigsegv_get_vma mincore_get_vma
# define STATIC static
# include "stackvma-mincore.c"
# undef sigsegv_get_vma
#else
/* Cache for getpagesize().  */
static unsigned long pagesize;
/* Initialize pagesize.  */
static void
init_pagesize (void)
{
  pagesize = getpagesize ();
}
#endif

int
sigsegv_get_vma (unsigned long address, struct vma_struct *vma)
{
  char fnamebuf[6+10+1];
  char *fname;
  int fd;
  int nmaps;
  size_t memneed;
#if HAVE_MMAP_ANON
# define zero_fd -1
# define map_flags MAP_ANON
#elif HAVE_MMAP_ANONYMOUS
# define zero_fd -1
# define map_flags MAP_ANONYMOUS
#else
  int zero_fd;
# define map_flags 0
#endif
  void *auxmap;
  unsigned long auxmap_start;
  unsigned long auxmap_end;
  prmap_t* maps;
  prmap_t* mp;
  unsigned long start, end;
#if STACK_DIRECTION < 0
  unsigned long prev;
#endif

  if (pagesize == 0)
    init_pagesize ();

  /* Construct fname = sprintf (fnamebuf+i, "/proc/%u", getpid ()).  */
  fname = fnamebuf + sizeof (fnamebuf) - 1;
  *fname = '\0';
  {
    unsigned int value = getpid ();
    do
      *--fname = (value % 10) + '0';
    while ((value = value / 10) > 0);
  }
  fname -= 6;
  memcpy (fname, "/proc/", 6);

  fd = open (fname, O_RDONLY);
  if (fd < 0)
    goto failed;

  if (ioctl (fd, PIOCNMAP, &nmaps) < 0)
    goto fail2;

  memneed = (nmaps + 10) * sizeof (prmap_t);
  /* Allocate memneed bytes of memory.
     We cannot use alloca here, because we are low on stack space.
     We also cannot use malloc here, because a malloc() call may have been
     interrupted.
     So use mmap(), and ignore the resulting VMA.  */
  memneed = ((memneed - 1) / pagesize + 1) * pagesize;
#if !(HAVE_MMAP_ANON || HAVE_MMAP_ANONYMOUS)
  zero_fd = open ("/dev/zero", O_RDONLY, 0644);
  if (zero_fd < 0)
    goto fail2;
#endif
  auxmap = (void *) mmap ((void *) 0, memneed, PROT_READ | PROT_WRITE, map_flags | MAP_PRIVATE, zero_fd, 0);
#if !(HAVE_MMAP_ANON || HAVE_MMAP_ANONYMOUS)
  close (zero_fd);
#endif
  if (auxmap == (void *) -1)
    goto fail2;
  auxmap_start = (unsigned long) auxmap;
  auxmap_end = auxmap_start + memneed;
  maps = (prmap_t *) auxmap;

  if (ioctl (fd, PIOCMAP, maps) < 0)
    goto fail1;

#if STACK_DIRECTION < 0
  prev = 0;
#endif
  for (mp = maps;;)
    {
      start = (unsigned long) mp->pr_vaddr;
      end = start + mp->pr_size;
      if (start == 0 && end == 0)
        break;
      mp++;
      if (start <= auxmap_start && auxmap_end - 1 <= end - 1)
        {
          /* Consider [start,end-1] \ [auxmap_start,auxmap_end-1]
             = [start,auxmap_start-1] u [auxmap_end,end-1].  */
          if (start != auxmap_start)
            {
              if (address >= start && address <= auxmap_start - 1)
                {
                  end = auxmap_start;
                  goto found;
                }
#if STACK_DIRECTION < 0
              prev = auxmap_start;
#endif
            }
          if (end != auxmap_end)
            {
              if (address >= auxmap_end && address <= end - 1)
                {
                  start = auxmap_end;
                  goto found;
                }
#if STACK_DIRECTION < 0
              prev = end;
#endif
            }
        }
      else
        {
          if (address >= start && address <= end - 1)
            goto found;
#if STACK_DIRECTION < 0
          prev = end;
#endif
        }
    }

 fail1:
  munmap (auxmap, memneed);
 fail2:
  close (fd);
 failed:
#if HAVE_MINCORE
  return mincore_get_vma (address, vma);
#else
  return -1;
#endif

 found:
  vma->start = start;
  vma->end = end;
#if STACK_DIRECTION < 0
  vma->prev_end = prev;
#else
  vma->next_start = (unsigned long) mp->pr_vaddr;
#endif
  munmap (auxmap, memneed);
  close (fd);
  vma->is_near_this = simple_is_near_this;
  return 0;
}
