/* Fault handler information.
   Copyright (C) 1993-1999, 2002, 2008  Bruno Haible <bruno@clisp.org>

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

#include "sigsegv.h"

int
sigsegv_install_handler (sigsegv_handler_t handler)
{
  return -1;
}

void
sigsegv_deinstall_handler (void)
{
}

int
sigsegv_leave_handler (void (*continuation) (void*, void*, void*),
                       void* cont_arg1, void* cont_arg2, void* cont_arg3)
{
  (*continuation) (cont_arg1, cont_arg2, cont_arg3);
  return 1;
}

int
stackoverflow_install_handler (stackoverflow_handler_t handler,
                               void *extra_stack, unsigned long extra_stack_size)
{
  return -1;
}

void
stackoverflow_deinstall_handler (void)
{
}
