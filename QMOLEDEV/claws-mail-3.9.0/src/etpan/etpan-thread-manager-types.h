/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2005-2012 DINH Viet Hoa and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef ETPAN_THREAD_MANAGER_TYPES_H

#define ETPAN_THREAD_MANAGER_TYPES_H

#include <pthread.h>
#include <libetpan/libetpan.h>

struct etpan_thread_manager {
  /* thread pool */
  carray * thread_pool;
  carray * thread_pending;
  int can_create_thread;
  
  int unbound_count;
  
  int notify_fds[2];
};

struct etpan_thread {
  struct etpan_thread_manager * manager;
  
  pthread_t th_id;
  
  pthread_mutex_t lock;
  carray * op_list;
  carray * op_done_list;
  
  int bound_count;
  int terminate_state;
  
  struct mailsem * start_sem;
  struct mailsem * stop_sem;
  struct mailsem * op_sem;
};

struct etpan_thread_op {
  struct etpan_thread * thread;
  
  void (* run)(struct etpan_thread_op * op);
  
  void (* callback)(int cancelled, void * result, void * callback_data);
  void * callback_data;
  
  void (* cleanup)(struct etpan_thread_op * op);
  
  pthread_mutex_t lock;
  int callback_called;
  int cancellable;
  int cancelled;
  void * param;
  void * result;
  int finished;
  mailimap *imap;
  newsnntp *nntp;
};

#endif
