/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    semaphores.d -- POSIX semaphores
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
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

/*----------------------------------------------------------------------
 * SEMAPHORES
 */

#ifdef ECL_SEMAPHORES

# ifdef ECL_MACH_SEMAPHORES
struct ecl_semaphore_inner {
        task_t owner;
        semaphore_t counter[1];
};
# endif

@(defun mp::make-semaphore (max &key name ((:count count) MAKE_FIXNUM(0)))
	cl_object output;
	cl_index initial_count, max_count;
@
{
        output = ecl_alloc_object(t_semaphore);
        ecl_disable_interrupts_env(the_env);
        output->semaphore.name = name;
        output->semaphore.handle = NULL;
        ecl_set_finalizer_unprotected(output, Ct);
        if (ecl_unlikely(!ECL_FIXNUMP(max) ||
                         ecl_fixnum_minusp(max) ||
                         ecl_fixnum_greaterp(max, MAKE_FIXNUM(0xFFFF)))) {
                FEwrong_type_nth_arg(@[mp::make-semaphore], 1, max,
                                     ecl_make_integer_type(MAKE_FIXNUM(0),
                                                           MAKE_FIXNUM(0xFFFF)));
        }
        max_count = fix(max);
        if (ecl_unlikely(!ECL_FIXNUMP(count) ||
                         ((initial_count = fix(count)) < 0) ||
                         initial_count > max_count)) {
                FEwrong_type_key_arg(@[mp::make-semaphore], @[:count], count,
                                     ecl_make_integer_type(MAKE_FIXNUM(0),
                                                           max));
        }
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = CreateSemaphore(NULL,
                                           initial_count,
                                           0xFFFF,
                                           NULL);
                output->semaphore.handle = h;
                ecl_enable_interrupts_env(the_env);
                if (h == NULL)
                        FEwin32_error("Unable to create semaphore object.", 0);
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = ecl_alloc_atomic(sizeof(sem_t));
                int rc = sem_init(h, 0, initial_count);
                if (!rc)
                        output->semaphore.handle = h;
                ecl_enable_interrupts();
                if (rc)
                        FEerror("Unable to create semaphore object.", 0);
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return output)
}
@)

cl_object
mp_semaphore_trywait(cl_object sem)
{
        cl_object output;
        if (ecl_unlikely(typeof(sem) != t_semaphore)) {
                FEwrong_type_only_arg(@[mp::semaphore-trywait], sem, @[mp::semaphore]);
        }
 AGAIN:
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                switch (WaitForSingleObject(h, 0)) {
		case WAIT_OBJECT_0:
                        output = Ct;
			break;
                case WAIT_TIMEOUT:
                        output = Cnil;
                        break;
		default:
			FEwin32_error("Unable to wait on semaphore", 0);
                        output = Cnil;
                }
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = (sem_t *)(sem->semaphore.handle);
                int rc = sem_trywait(h);
                if (sem_trywait(h)) {
                        if (errno != EAGAIN) {
                                FElibc_error("Unable to wait on semaphore", 0);
                        }
                        output = Cnil;
                } else {
                        output = Ct;
                }
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return output)
}


cl_object
mp_semaphore_wait(cl_object sem)
{
        cl_object output;
        if (ecl_unlikely(typeof(sem) != t_semaphore)) {
                FEwrong_type_only_arg(@[mp::semaphore-wait], sem, @[mp::semaphore]);
        }
 AGAIN:
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                if (WaitForSingleObject(h, INFINITE) != WAIT_OBJECT_0) {
			FEwin32_error("Unable to wait on semaphore", 0);
                }
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = (sem_t *)(sem->semaphore.handle);
                int rc = sem_wait(h);
                if (sem_wait(h)) {
                        if (errno == EINTR) {
                                ecl_check_pending_interrupts();
                                goto AGAIN;
                        }
                        FElibc_error("Unable to wait on semaphore", 0);
                }
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return Ct)
}

cl_object
mp_semaphore_signal(cl_object sem)
{
        if (ecl_unlikely(typeof(sem) != t_semaphore)) {
                FEwrong_type_only_arg(@[mp::semaphore-signal], sem, @[mp::semaphore]);
        }
 AGAIN:
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                if (!ReleaseSemaphore(h, 1, NULL)) {
                        FEwin32_error("Unable to post on semaphore ~A" 1, sem);
                }
        }
#else
# ifdef HAVE_SEM_INIT
        {
                sem_t *h = (sem_t *)(sem->semaphore.handle);
                int rc = sem_wait(h);
                if (sem_wait(h)) {
                        if (errno == EINTR) {
                                ecl_check_pending_interrupts();
                                goto AGAIN;
                        }
                        FElibc_error("Unable to post on semaphore ~A", 1, sem);
                }
        }
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return Ct)
}

cl_object
mp_semaphore_close(cl_object sem)
{
        if (ecl_unlikely(typeof(sem) != t_semaphore)) {
                FEwrong_type_only_arg(@[mp::semaphore-close], sem, @[mp::semaphore]);
        }
#ifdef ECL_WINDOWS_THREADS
        {
                HANDLE h = (HANDLE)(sem->semaphore.handle);
                if (h) CloseHandle(h);
        }
#else
# ifdef HAVE_SEM_INIT
        /*
         * No need for closing.
         */
# endif /* HAVE_SEM_INIT */
#endif /* ECL_WINDOWS_THREADS */
        @(return Ct)
}

#endif /* ECL_SEMAPHORES */

