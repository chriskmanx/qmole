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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef HAVE_LIBETPAN

#include "etpan-thread-manager.h"

#include <glib.h>
#include <stdlib.h>
#include <pthread.h>
#include <libetpan/mailsem.h>
#include <semaphore.h>
#include <unistd.h>
#include <fcntl.h>

#include "etpan-errors.h"
#include "utils.h"

#define POOL_UNBOUND_MAX 4

#define POOL_INIT_SIZE 8
#define OP_INIT_SIZE 8

static int etpan_thread_start(struct etpan_thread * thread);
static void etpan_thread_free(struct etpan_thread * thread);
static unsigned int etpan_thread_get_load(struct etpan_thread * thread);
static int etpan_thread_is_bound(struct etpan_thread * thread);
static int etpan_thread_manager_is_stopped(struct etpan_thread_manager * manager);
static void etpan_thread_join(struct etpan_thread * thread);
static struct etpan_thread * etpan_thread_new(void);
static int etpan_thread_op_cancelled(struct etpan_thread_op * op);
static void etpan_thread_op_lock(struct etpan_thread_op * op);
static void etpan_thread_op_unlock(struct etpan_thread_op * op);
static void etpan_thread_stop(struct etpan_thread * thread);

#if 0
static void etpan_thread_bind(struct etpan_thread * thread);
static int etpan_thread_manager_op_schedule(struct etpan_thread_manager * manager,
	     struct etpan_thread_op * op);
static void etpan_thread_manager_start(struct etpan_thread_manager * manager);
static void etpan_thread_op_cancel(struct etpan_thread_op * op);
#endif

enum {
  TERMINATE_STATE_NONE,
  TERMINATE_STATE_REQUESTED,
  TERMINATE_STATE_DONE,
};

struct etpan_thread_manager * etpan_thread_manager_new(void)
{
  struct etpan_thread_manager * manager;
  int r;
  
  manager = malloc(sizeof(* manager));
  if (manager == NULL)
    goto err;
  
  manager->thread_pool = carray_new(POOL_INIT_SIZE);
  if (manager->thread_pool == NULL)
    goto free;

  manager->thread_pending = carray_new(POOL_INIT_SIZE);
  if (manager->thread_pending == NULL)
    goto free_pool;
  
  manager->can_create_thread = 1;
  manager->unbound_count = 0;
  
  r = pipe(manager->notify_fds);
  if (r < 0)
    goto free_pending;
  
  return manager;
  
 free_pending:
  carray_free(manager->thread_pending);
 free_pool:
  carray_free(manager->thread_pool);
 free:
  free(manager);
 err:
  return NULL;
}

void etpan_thread_manager_free(struct etpan_thread_manager * manager)
{
  close(manager->notify_fds[1]);
  close(manager->notify_fds[0]);
  carray_free(manager->thread_pending);
  carray_free(manager->thread_pool);
  free(manager);
}

static struct etpan_thread * etpan_thread_new(void)
{
  struct etpan_thread * thread;
  int r;
  
  thread = malloc(sizeof(* thread));
  if (thread == NULL)
    goto err;
  
  r = pthread_mutex_init(&thread->lock, NULL);
  if (r != 0)
    goto free;

  thread->op_list = carray_new(OP_INIT_SIZE);
  if (thread->op_list == NULL)
    goto destroy_lock;
  
  thread->op_done_list = carray_new(OP_INIT_SIZE);
  if (thread->op_done_list == NULL)
    goto free_op_list;
  
  thread->start_sem = mailsem_new();
  if (thread->start_sem == NULL)
    goto free_op_done_list;
  
  thread->stop_sem = mailsem_new();
  if (thread->stop_sem == NULL)
    goto free_startsem;
  
  thread->op_sem = mailsem_new();
  if (thread->op_sem == NULL)
    goto free_stopsem;
  
  thread->manager = NULL;
  thread->bound_count = 0;
  thread->terminate_state = TERMINATE_STATE_NONE;
  
  return thread;
  
 free_stopsem:
  mailsem_free(thread->stop_sem);
 free_startsem:
  mailsem_free(thread->start_sem);
 free_op_done_list:
  carray_free(thread->op_done_list);
 free_op_list:
  carray_free(thread->op_list);
 destroy_lock:
  pthread_mutex_destroy(&thread->lock);
 free:
  free(thread);
 err:
  return NULL;
}

static void etpan_thread_free(struct etpan_thread * thread)
{
  mailsem_free(thread->op_sem);
  mailsem_free(thread->stop_sem);
  mailsem_free(thread->start_sem);
  carray_free(thread->op_done_list);
  carray_free(thread->op_list);
  pthread_mutex_destroy(&thread->lock);
  free(thread);
}

struct etpan_thread_op * etpan_thread_op_new(void)
{
  struct etpan_thread_op * op;
  int r;
  
  op = malloc(sizeof(* op));
  if (op == NULL)
    goto err;
  
  op->thread = NULL;
  op->run = NULL;
  op->callback = NULL;
  op->callback_data = NULL;
  op->callback_called = 0;
  op->cancellable = 0;
  op->cancelled = 0;
  op->param = NULL;
  op->result = NULL;
  op->finished = 0;
  op->imap = NULL;
  op->nntp = NULL;

  r = pthread_mutex_init(&op->lock, NULL);
  if (r != 0)
    goto free;
  
  return op;
  
 free:
  free(op);
 err:
  return NULL;
}

void etpan_thread_op_free(struct etpan_thread_op * op)
{
  pthread_mutex_destroy(&op->lock);
  free(op);
}

static struct etpan_thread *
etpan_thread_manager_create_thread(struct etpan_thread_manager * manager)
{
  struct etpan_thread * thread;
  int r;
  
  thread = etpan_thread_new();
  if (thread == NULL)
    goto err;
  
  thread->manager = manager;
  
  r = etpan_thread_start(thread);
  if (r != NO_ERROR)
    goto free_thread;
  
  r = carray_add(manager->thread_pool, thread, NULL);
  if (r < 0) {
    etpan_thread_stop(thread);
    goto free_thread;
  }
  
  return thread;
  
 free_thread:
  etpan_thread_free(thread);
 err:
  return NULL;
}

static void
etpan_thread_manager_terminate_thread(struct etpan_thread_manager * manager,
    struct etpan_thread * thread)
{
  unsigned int i;
  int r;
  
  for(i = 0 ; i < carray_count(manager->thread_pool) ; i ++) {
    if (carray_get(manager->thread_pool, i) == thread) {
      carray_delete(manager->thread_pool, i);
      break;
    }
  }
  
  if (!etpan_thread_is_bound(thread))
    manager->unbound_count --;
  
  r = carray_add(manager->thread_pending, thread, NULL);
  if (r < 0) {
    g_warning("complete failure of thread due to lack of memory (thread stop)");
  }
  
  etpan_thread_stop(thread);
}

static void manager_notify(struct etpan_thread_manager * manager)
{
  char ch;
  ssize_t r;
  
  ch = 1;
  r = write(manager->notify_fds[1], &ch, 1);
  if (r < 0) {
    g_warning("error writing notification to etpan thread manager");
  }
}

static void manager_ack(struct etpan_thread_manager * manager)
{
#ifndef G_OS_WIN32
  char ch;
  ssize_t r;
  r = read(manager->notify_fds[0], &ch, 1);
  if (r != 1) {
    g_warning("error reading notification from etpan thread manager");
  }
#else
  /* done in the GIOChannel handler in imap-thread.c and nntp-thread.c */
#endif
}

static void thread_lock(struct etpan_thread * thread)
{
  pthread_mutex_lock(&thread->lock);
}

static void thread_unlock(struct etpan_thread * thread)
{
  pthread_mutex_unlock(&thread->lock);
}

static void thread_notify(struct etpan_thread * thread)
{
  manager_notify(thread->manager);
}

static void * thread_run(void * data)
{
  struct etpan_thread * thread;
  int r;
  
  thread = data;
  
  mailsem_up(thread->start_sem);
  
  while (1) {
    int do_quit;
    struct etpan_thread_op * op;
    
    mailsem_down(thread->op_sem);
    
    do_quit = 0;
    op = NULL;
    thread_lock(thread);
    if (carray_count(thread->op_list) > 0) {
      op = carray_get(thread->op_list, 0);
      carray_delete_slow(thread->op_list, 0);
    }
    else {
      do_quit = 1;
    }
    thread_unlock(thread);
    
    if (do_quit) {
      break;
    }
    
    if (!etpan_thread_op_cancelled(op)) {
      if (op->run != NULL)
        op->run(op);
    }
    
    thread_lock(thread);
    r = carray_add(thread->op_done_list, op, NULL);
    if (r < 0) {
      g_warning("complete failure of thread due to lack of memory (op done)");
    }
    thread_unlock(thread);
    
    thread_notify(thread);
  }
  
  thread_lock(thread);
  thread->terminate_state = TERMINATE_STATE_DONE;
  thread_unlock(thread);
  
  thread_notify(thread);
  
  mailsem_up(thread->stop_sem);
  
  return NULL;
}

static int etpan_thread_start(struct etpan_thread * thread)
{
  int r;
  
  r = pthread_create(&thread->th_id, NULL, thread_run, thread);
  if (r != 0)
    return ERROR_MEMORY;
  
  mailsem_down(thread->start_sem);
  
  return NO_ERROR;
}

static void etpan_thread_stop(struct etpan_thread * thread)
{
  thread_lock(thread);
  thread->terminate_state = TERMINATE_STATE_REQUESTED;
  thread_unlock(thread);
  
  mailsem_up(thread->op_sem);
  
  /* this thread will be joined in the manager loop */
}

static int etpan_thread_is_stopped(struct etpan_thread * thread)
{
  int stopped;
  
  thread_lock(thread);
  stopped = (thread->terminate_state == TERMINATE_STATE_DONE);
  thread_unlock(thread);
  
  return stopped;
}

static void etpan_thread_join(struct etpan_thread * thread)
{
  mailsem_down(thread->stop_sem);
  pthread_join(thread->th_id, NULL);
}

struct etpan_thread *
etpan_thread_manager_get_thread(struct etpan_thread_manager * manager)
{
  struct etpan_thread * chosen_thread;
  unsigned int chosen_thread_load;
  unsigned int i;
  struct etpan_thread * thread;
  
  /* chose a thread */
  
  chosen_thread = NULL;
  chosen_thread_load = 0;
  
  for(i = 0 ; i < carray_count(manager->thread_pool) ; i ++) {
    thread = carray_get(manager->thread_pool, i);
    if (etpan_thread_is_bound(thread))
      continue;
    
    if (chosen_thread == NULL) {
      chosen_thread = thread;
      chosen_thread_load = etpan_thread_get_load(thread);
      
      if (chosen_thread_load == 0)
        break;
    }
    else {
      unsigned int load;
      
      load = etpan_thread_get_load(thread);
      
      if (load < chosen_thread_load) {
        chosen_thread = thread;
        chosen_thread_load = load;
      }
    }
  }
  
  if (chosen_thread != NULL) {
    if (manager->can_create_thread && (chosen_thread_load != 0)) {
      chosen_thread = NULL;
    }
  }
  
  /* choice done */
  
  if (chosen_thread != NULL)
    return chosen_thread;
  
  thread = etpan_thread_manager_create_thread(manager);
  if (thread == NULL)
    goto err;
  
  manager->unbound_count ++;
  if (manager->unbound_count >= POOL_UNBOUND_MAX)
    manager->can_create_thread = 0;
  
  return thread;
  
 err:
  return NULL;
}

static unsigned int etpan_thread_get_load(struct etpan_thread * thread)
{
  unsigned int load;
  
  thread_lock(thread);
  load = carray_count(thread->op_list);
  thread_unlock(thread);
  
  return load;
}

#if 0
static void etpan_thread_bind(struct etpan_thread * thread)
{
  thread->bound_count ++;
}
#endif

void etpan_thread_unbind(struct etpan_thread * thread)
{
  thread->bound_count --;
}

static int etpan_thread_is_bound(struct etpan_thread * thread)
{
  return (thread->bound_count != 0);
}

int etpan_thread_op_schedule(struct etpan_thread * thread,
                             struct etpan_thread_op * op)
{
  int r;
  
  if (thread->terminate_state != TERMINATE_STATE_NONE)
    return ERROR_INVAL;
  
  thread_lock(thread);
  r = carray_add(thread->op_list, op, NULL);
  thread_unlock(thread);
  
  if (r < 0)
    return ERROR_MEMORY;
  
  op->thread = thread;
  mailsem_up(thread->op_sem);
  
  return NO_ERROR;
}

static void etpan_thread_op_lock(struct etpan_thread_op * op)
{
  pthread_mutex_lock(&op->lock);
}

static void etpan_thread_op_unlock(struct etpan_thread_op * op)
{
  pthread_mutex_unlock(&op->lock);
}

static int etpan_thread_op_cancelled(struct etpan_thread_op * op)
{
  int cancelled;
  
  cancelled = 0;
  etpan_thread_op_lock(op);
  if (op->cancellable)
    cancelled = op->cancelled;
  etpan_thread_op_unlock(op);
  
  return cancelled;
}

int etpan_thread_manager_get_fd(struct etpan_thread_manager * manager)
{
  return manager->notify_fds[0];
}

static void loop_thread_list(carray * op_to_notify,
    carray * thread_list)
{
  unsigned int i;
  int r;
  
  for(i = 0 ; i < carray_count(thread_list) ; i ++) {
    struct etpan_thread * thread;
    unsigned int j;
    
    thread = carray_get(thread_list, i);
    
    thread_lock(thread);
    
    for(j = 0 ; j < carray_count(thread->op_done_list) ; j ++) {
      struct etpan_thread_op * op;
      
      op = carray_get(thread->op_done_list, j);
      r = carray_add(op_to_notify, op, NULL);
      if (r < 0) {
        g_warning("complete failure of thread due to lack of memory (callback)");
        break;
      }
    }
    carray_set_size(thread->op_done_list, 0);
    
    thread_unlock(thread);
  }
}

void etpan_thread_manager_loop(struct etpan_thread_manager * manager)
{
  carray * op_to_notify;
  unsigned int i;
  
  manager_ack(manager);
  
  op_to_notify = carray_new(OP_INIT_SIZE);
  
  loop_thread_list(op_to_notify, manager->thread_pool);
  loop_thread_list(op_to_notify, manager->thread_pending);
  
  for(i = 0 ; i < carray_count(op_to_notify) ; i ++) {
    struct etpan_thread_op * op;
    
    op = carray_get(op_to_notify, i);
    
    etpan_thread_op_lock(op);
    
    if (!op->callback_called) {
      if (op->callback != NULL)
        op->callback(op->cancelled, op->result, op->callback_data);
    }
    
    etpan_thread_op_unlock(op);
    
    if (op->cleanup != NULL)
      op->cleanup(op);
  }
  
  carray_free(op_to_notify);
  
  i = 0;
  while (i < carray_count(manager->thread_pending)) {
    struct etpan_thread * thread;
    
    thread = carray_get(manager->thread_pending, i);
    
    if (etpan_thread_is_stopped(thread)) {
      etpan_thread_join(thread);
      
      etpan_thread_free(thread);
      
      carray_delete_slow(manager->thread_pending, i);
    }
    else {
      i ++;
    }
  }
}

#if 0
static void etpan_thread_manager_start(struct etpan_thread_manager * manager)
{
  /* do nothing */
}
#endif

void etpan_thread_manager_stop(struct etpan_thread_manager * manager)
{
  while (carray_count(manager->thread_pool) > 0) {
    struct etpan_thread * thread;
    
    thread = carray_get(manager->thread_pool, 0);
    etpan_thread_manager_terminate_thread(manager, thread);
  }
}

static int etpan_thread_manager_is_stopped(struct etpan_thread_manager * manager)
{
  return ((carray_count(manager->thread_pending) == 0) && 
      (carray_count(manager->thread_pool) == 0));
}

void etpan_thread_manager_join(struct etpan_thread_manager * manager)
{
  while (!etpan_thread_manager_is_stopped(manager)) {
    etpan_thread_manager_loop(manager);
  }
}
#endif
