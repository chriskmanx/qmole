/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    threads_mutex.d -- Native mutually exclusive locks.
*/
/*
    Copyright (c) 2003, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef __sun__ /* See unixinit.d for this */
#define _XOPEN_SOURCE 600	/* For pthread mutex attributes */
#endif
#include <errno.h>
#include <ecl/ecl.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
#else
# include <pthread.h>
#endif
#include <ecl/internal.h>

/*----------------------------------------------------------------------
 * LOCKS or MUTEX
 */

static void
FEerror_not_a_lock(cl_object lock)
{
        FEwrong_type_argument(@'mp::lock', lock);
}

static void
FEerror_not_a_recursive_lock(cl_object lock)
{
        FEerror("Attempted to recursively lock ~S which is already owned by ~S",
                2, lock, lock->lock.holder);
}

static void
FEerror_not_owned(cl_object lock)
{
        FEerror("Attempted to give up lock ~S that is not owned by process ~S",
                2, lock, mp_current_process());
}

static void
FEunknown_lock_error(cl_object lock)
{
#ifdef ECL_WINDOWS_THREADS
        FEwin32_error("When acting on lock ~A, got an unexpected error.", 1, lock);
#else
        FEerror("When acting on lock ~A, got an unexpected error.", 1, lock);
#endif
}

cl_object
ecl_make_lock(cl_object name, bool recursive)
{
        cl_env_ptr the_env = ecl_process_env();
	cl_object output = ecl_alloc_object(t_lock);
	ecl_disable_interrupts_env(the_env);
	output->lock.name = name;
#ifdef ECL_WINDOWS_THREADS
	output->lock.mutex = CreateMutex(NULL, FALSE, NULL);
#else
	{
        pthread_mutexattr_t mutexattr_recursive[1];
	pthread_mutexattr_init(mutexattr_recursive);
	pthread_mutexattr_settype(mutexattr_recursive, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&output->lock.mutex, mutexattr_recursive);
	}
#endif
	output->lock.holder = Cnil;
	output->lock.counter = 0;
	output->lock.recursive = recursive;
	ecl_set_finalizer_unprotected(output, Ct);
	ecl_enable_interrupts_env(the_env);
        return output;
}

@(defun mp::make-lock (&key name ((:recursive recursive) Ct))
@
	@(return ecl_make_lock(name, !Null(recursive)))
@)

cl_object
mp_recursive_lock_p(cl_object lock)
{
	cl_env_ptr env = ecl_process_env();
	if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
	ecl_return1(env, lock->lock.recursive? Ct : Cnil);
}

cl_object
mp_lock_name(cl_object lock)
{
	cl_env_ptr env = ecl_process_env();
	if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
        ecl_return1(env, lock->lock.name);
}

cl_object
mp_lock_holder(cl_object lock)
{
	cl_env_ptr env = ecl_process_env();
	if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
        ecl_return1(env, lock->lock.holder);
}

cl_object
mp_lock_mine_p(cl_object lock)
{
	cl_env_ptr env = ecl_process_env();
	if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
        ecl_return1(env, (lock->lock.holder == mp_current_process())? Ct : Cnil);
}

cl_object
mp_lock_count(cl_object lock)
{
	cl_env_ptr env = ecl_process_env();
	if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
	ecl_return1(env, MAKE_FIXNUM(lock->lock.counter));
}

cl_object
mp_lock_count_mine(cl_object lock)
{
	cl_env_ptr env = ecl_process_env();
	if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
        ecl_return1(env,
                    (lock->lock.holder == mp_current_process())?
                    MAKE_FIXNUM(lock->lock.counter) :
                    MAKE_FIXNUM(0));
}

cl_object
mp_giveup_lock(cl_object lock)
{
        /* Must be called with interrupts disabled. */
        cl_env_ptr env = ecl_process_env();
	cl_object own_process = env->own_process;
	if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
	if (lock->lock.holder != own_process)
                FEerror_not_owned(lock);
	if (--lock->lock.counter == 0) {
		lock->lock.holder = Cnil;
#ifdef ECL_WINDOWS_THREADS
		if (ReleaseMutex(lock->lock.mutex) == 0)
			FEunknown_lock_error(lock);
#else
		pthread_mutex_unlock(&lock->lock.mutex);
#endif
	}
        ecl_return1(env, Ct);
}

cl_object
mp_get_lock_nowait(cl_object lock)
{
        cl_env_ptr env = ecl_process_env();
	cl_object own_process = env->own_process;
	int rc;
        if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
        if (lock->lock.holder == own_process) {
                if (!lock->lock.recursive)
                        FEerror_not_a_recursive_lock(lock);
                lock->lock.counter++;
                ecl_return1(env, lock);
        }
	/* FIXME!  This code has a nonzero chance of problems with
         * interrupts. If an interupt happens right after we locked the mutex
         * but before we set count and owner, we are in trouble, since the
         * mutex might be locked. */
#ifdef ECL_WINDOWS_THREADS
	switch (WaitForSingleObject(lock->lock.mutex, 0)) {
		case WAIT_OBJECT_0:
                        lock->lock.counter++;
                        lock->lock.holder = own_process;
                        ecl_return1(env, lock);
		case WAIT_TIMEOUT:
                        ecl_return1(env, Cnil);
		case WAIT_ABANDONED:
		case WAIT_FAILED:
			FEunknown_lock_error(lock);
                        ecl_return1(env, Cnil);
	}
#else
        rc = pthread_mutex_trylock(&lock->lock.mutex);
	if (rc == 0) {
		lock->lock.counter++;
		lock->lock.holder = own_process;
                ecl_return1(env, lock);
	} else {
                if (rc != EBUSY)
			FEunknown_lock_error(lock);
                ecl_return1(env, Cnil);
        }
#endif
}

cl_object
mp_get_lock_wait(cl_object lock)
{
        cl_env_ptr env = ecl_process_env();
	cl_object own_process = env->own_process;
	int rc;
        if (type_of(lock) != t_lock)
		FEerror_not_a_lock(lock);
        if (lock->lock.holder == own_process) {
                if (!lock->lock.recursive)
                        FEerror_not_a_recursive_lock(lock);
                lock->lock.counter++;
                ecl_return1(env, lock);
        }
	/* FIXME!  This code has a nonzero chance of problems with
         * interrupts. If an interupt happens right after we locked the mutex
         * but before we set count and owner, we are in trouble, since the
         * mutex might be locked. */
#ifdef ECL_WINDOWS_THREADS
	switch (WaitForSingleObject(lock->lock.mutex, INFINITE)) {
		case WAIT_OBJECT_0:
                        lock->lock.counter++;
                        lock->lock.holder = own_process;
                        ecl_return1(env, lock);
		case WAIT_TIMEOUT:
                        ecl_return1(env, Cnil);
		case WAIT_ABANDONED:
		case WAIT_FAILED:
                        FEunknown_lock_error(lock);
                        ecl_return1(env, Cnil);
	}
#else
        rc = pthread_mutex_lock(&lock->lock.mutex);
	if (rc == 0) {
		lock->lock.counter++;
		lock->lock.holder = own_process;
                ecl_return1(env, lock);
	} else {
                FEunknown_lock_error(lock);
                ecl_return1(env, Cnil);
        }
#endif
}

@(defun mp::get-lock (lock &optional (wait Ct))
@
	if (Null(wait))
        	return mp_get_lock_nowait(lock);
        else
        	return mp_get_lock_wait(lock);
@)
