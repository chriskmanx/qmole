/* Buffered read-only streams.
   Copyright (C) 2008  Bruno Haible <bruno@clisp.org>

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

#include <errno.h> /* errno, EINTR */
#include <fcntl.h> /* O_RDONLY */
#include <stddef.h> /* size_t */
#include <unistd.h> /* read, close */

/* Buffered read-only streams.
   We cannot use <stdio.h> here, because fopen() calls malloc(), and a malloc()
   call may have been interrupted.  */

struct rofile
  {
    int fd;
    size_t position;
    size_t filled;
    int eof_seen;
    char buffer[4096];
  };

/* Open a read-only file stream.  */
static int
rof_open (struct rofile *rof, const char *filename)
{
  int fd = open (filename, O_RDONLY);
  if (fd < 0)
    return -1;
  rof->fd = fd;
  rof->position = 0;
  rof->filled = 0;
  rof->eof_seen = 0;
  return 0;
}

/* Return the next byte from a read-only file stream without consuming it,
   or -1 at EOF.  */
static int
rof_peekchar (struct rofile *rof)
{
  if (rof->position == rof->filled)
    {
      if (rof->eof_seen)
        return -1;
      else
        for (;;)
          {
            int n = read (rof->fd, rof->buffer, sizeof (rof->buffer));
#ifdef EINTR
            if (n < 0 && errno == EINTR)
              continue;
#endif
            if (n <= 0)
              {
                rof->eof_seen = 1;
                return -1;
              }
            rof->filled = n;
            rof->position = 0;
            break;
          }
    }
  return (unsigned char) rof->buffer[rof->position];
}

/* Return the next byte from a read-only file stream, or -1 at EOF.  */
static int
rof_getchar (struct rofile *rof)
{
  int c = rof_peekchar (rof);
  if (c >= 0)
    rof->position++;
  return c;
}

/* Parse an unsigned hexadecimal number from a read-only file stream.  */
static int
rof_scanf_lx (struct rofile *rof, unsigned long *valuep)
{
  unsigned long value = 0;
  unsigned int numdigits = 0;
  for (;;)
    {
      int c = rof_peekchar (rof);
      if (c >= '0' && c <= '9')
        value = (value << 4) + (c - '0');
      else if (c >= 'A' && c <= 'F')
        value = (value << 4) + (c - 'A' + 10);
      else if (c >= 'a' && c <= 'f')
        value = (value << 4) + (c - 'a' + 10);
      else
        break;
      rof_getchar (rof);
      numdigits++;
    }
  if (numdigits == 0)
    return -1;
  *valuep = value;
  return 0;
}

/* Close a read-only file stream.  */
static void
rof_close (struct rofile *rof)
{
  close (rof->fd);
}
