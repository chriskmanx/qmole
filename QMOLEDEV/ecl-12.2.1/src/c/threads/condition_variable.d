/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    condition_variable.d -- Native threads.
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
#include <time.h>
#include <signal.h>
#define ECL_INCLUDE_MATH_H
#include <ecl/ecl.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
#else
# include <pthread.h>
#endif
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#ifdef HAVE_SCHED_YIELD
# include <sched.h>
#endif
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

/*----------------------------------------------------------------------
 * CONDITION VARIABLES
 */

cl_object
mp_make_condition_variable(void)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
	@(return Cnil)
#else
	cl_object output;

	output = ecl_alloc_object(t_condition_variable);
	pthread_cond_init(&output->condition_variable.cv, NULL);
	si_set_finalizer(output, Ct);
	@(return output)
#endif
}

cl_object
mp_condition_variable_wait(cl_object cv, cl_object lock)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
        int count, rc;
        cl_object own_process = mp_current_process();
	if (ecl_unlikely(type_of(cv) != t_condition_variable))
                FEwrong_type_nth_arg(@[mp::condition-variable-wait], 1, cv,
                                     @[mp::condition-variable]);
	if (ecl_unlikely(type_of(lock) != t_lock))
                FEwrong_type_nth_arg(@[mp::condition-variable-wait], 2, lock,
                                     @[mp::lock]);
        if (ecl_unlikely(lock->lock.holder != own_process)) {
                FEerror("Attempt to wait on a condition variable using lock~%~S"
                        "~%which is not owned by process~%~S", 2, lock, own_process);
        }
        if (ecl_unlikely(lock->lock.counter > 1)) {
                FEerror("mp:condition-variable-wait can not be used with recursive"
                        " locks:~%~S", 1, lock);
        }
        /* Note: this is highly unsafe. We are marking the lock as released
         * without knowing whether pthread_cond_wait worked as expected. */
        lock->lock.counter = 0;
        lock->lock.holder = Cnil;
	rc = pthread_cond_wait(&cv->condition_variable.cv,
                               &lock->lock.mutex);
        lock->lock.holder = own_process;
        lock->lock.counter = 1;
        if (ecl_unlikely(rc != 0)) {
                FEerror("System returned error code ~D "
                        "when waiting on condition variable~%~A~%and lock~%~A.",
                        3, MAKE_FIXNUM(rc), cv, lock);
        }
#endif
	@(return Ct)
}

cl_object
mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
        int rc;
        cl_object own_process = mp_current_process();
	double r;
	struct timespec   ts;
	struct timeval    tp;

	if (ecl_unlikely(type_of(cv) != t_condition_variable))
                FEwrong_type_nth_arg(@[mp::condition-variable-timedwait],
                                     1, cv, @[mp::condition-variable]);
	if (ecl_unlikely(type_of(lock) != t_lock))
                FEwrong_type_nth_arg(@[mp::condition-variable-timedwait],
                                     2, lock, @[mp::lock]);
        if (ecl_unlikely(lock->lock.holder != own_process)) {
                FEerror("Attempt to wait on a condition variable using lock~%~S"
                        "~%which is not owned by process~%~S", 2, lock, own_process);
        }
        if (ecl_unlikely(lock->lock.counter > 1)) {
                FEerror("mp:condition-variable-wait can not be used with recursive"
                        " locks:~%~S", 1, lock);
        }
	/* INV: ecl_minusp() makes sure `seconds' is real */
	if (ecl_unlikely(ecl_minusp(seconds))) {
		cl_error(9, @'simple-type-error', @':format-control',
			 make_constant_base_string("Not a non-negative number ~S"),
			 @':format-arguments', cl_list(1, seconds),
			 @':expected-type', @'real', @':datum', seconds);
        }
	gettimeofday(&tp, NULL);
	/* Convert from timeval to timespec */
	ts.tv_sec  = tp.tv_sec;
	ts.tv_nsec = tp.tv_usec * 1000;

	/* Add `seconds' delta */
	r = ecl_to_double(seconds);
	ts.tv_sec += (time_t)floor(r);
	ts.tv_nsec += (long)((r - floor(r)) * 1e9);
	if (ts.tv_nsec >= 1e9) {
		ts.tv_nsec -= 1e9;
		ts.tv_sec++;
	}
        /* Note: this is highly unsafe. We are marking the lock as released
         * without knowing whether pthread_cond_wait worked as expected. */
        lock->lock.counter = 0;
        lock->lock.holder = Cnil;
	rc = pthread_cond_timedwait(&cv->condition_variable.cv,
                                    &lock->lock.mutex, &ts);
        lock->lock.holder = own_process;
        lock->lock.counter = 1;
        if (rc != 0 && rc != ETIMEDOUT) {
                FEerror("System returned error code ~D "
                        "when waiting on condition variable~%~A~%and lock~%~A.",
                        3, MAKE_FIXNUM(rc), cv, lock);
        }
        @(return (rc? Ct : Cnil))
#endif
}

cl_object
mp_condition_variable_signal(cl_object cv)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	if (ecl_unlikely(type_of(cv) != t_condition_variable)) {
                FEwrong_type_only_arg(@[mp::condition-variable-signal],
                                      cv, @[mp::condition-variable]);
        }
	pthread_cond_signal(&cv->condition_variable.cv);
#endif
	@(return Ct)
}

cl_object
mp_condition_variable_broadcast(cl_object cv)
{
#ifdef ECL_WINDOWS_THREADS
	FEerror("Condition variables are not supported under Windows.", 0);
#else
	if (ecl_unlikely(type_of(cv) != t_condition_variable)) {
                FEwrong_type_only_arg(@[mp::condition-variable-broadcast],
                                      cv, @[mp::condition-variable]);
        }
	pthread_cond_broadcast(&cv->condition_variable.cv);
#endif
	@(return Ct)
}

