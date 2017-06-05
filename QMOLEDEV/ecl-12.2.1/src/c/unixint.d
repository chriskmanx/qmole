/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    unixint.c -- Unix interrupt interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/**********************************************************************
 * HOW WE HANDLE SIGNALS AND EXCEPTIONS
 *
 * (the following should be correlated with the manual)
 *
 * POSIX contemplates the notion of "signals", which are events that
 * cause a process or a thread to be interrupted. Windows uses the
 * term "exception", which includes also a more general kind of
 * errors.
 *
 * In both cases the consequence is that a thread or process may be
 * interrupted at any time, either by causes which are intrinsic to
 * them (synchronous signals), such as floating point exceptions, or
 * extrinsic (asynchronous signals), such as the process being aborted
 * by the user.
 * 
 * Of course, those interruptions are not always welcome. When the
 * interrupt is delivered and a handler is invoked, the thread or even
 * the whole program may be in an inconsistent state. For instance the
 * thread may have acquired a lock, or it may be in the process of
 * filling the fields of a structure. Understanding this POSIX
 * restricts severely what functions can be called from a signal
 * handler, thereby limiting its usefulness.
 *
 * There is a simple solution, which ECL uses, and which is to mark
 * sections of code which are interruptible, and in which it is safe
 * for the handler to run arbitrary code, protect anything else. In
 * principle this "marking" can be done using POSIX functions such as
 * pthread_sigmask() or sigprocmask().
 *
 * However in practice this is slow, as it involves at least a
 * function call, resolving thread-local variables, etc, etc, and it
 * will not work in Windows. Furthermore, sometimes we want signals to
 * be detected but not to be immediately processed. For instance, when
 * reading from the terminal we want to be able to interrupt the
 * process, but we can not execute the code from the handler, since
 * read() may leave the input stream in an inconsistent, or even
 * locked state.
 *
 * Our approach is slightly different: we install our own signal
 * hander which reads a single, thread-local variable stored in the
 * ecl_process_env()->disable_interrupts. If the variable marks that
 * signals should be postponed, then the information about the signal
 * is queued. Otherwise the appropriate code is executed: for instance
 * invoking the debugger, jumping to a condition handler, quitting,
 * etc.
 */

#ifdef __sun__
/* For SA_SIGINFO in Solaris. We could have used _XOPEN_SOURCE=600, but
 * this requires C99 and the default GCC for Solaris (3.4.3) does not
 * support this C standard. */
# define __EXTENSIONS__
#endif
#include <errno.h>
#include <string.h>
#include <stdio.h>
/* To get APCProc calls */
#define _WIN32_WINNT 0x400
#include <signal.h>

#if defined(_MSC_VER) || defined(__MINGW32__)
# include <windows.h>
#endif
#if !defined(_MSC_VER)
# include <unistd.h>
#endif

#include <ecl/ecl.h>

#ifdef ECL_USE_MPROTECT
# ifndef SA_SIGINFO
#  error "We cannot use the mmap code without siginfo"
# endif
# include <sys/mman.h>
#endif
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>
#include <ecl/impl/math_fenv.h>

static struct {
	int code;
	char *text;
} known_signals[] = {
#ifdef SIGHUP
	{ SIGHUP, "+SIGHUP+" },
#endif
#ifdef SIGINT
	{ SIGINT, "+SIGINT+" },
#endif
#ifdef SIGQUIT
	{ SIGQUIT, "+SIGQUIT+" },
#endif
#ifdef SIGILL
	{ SIGILL, "+SIGILL+" },
#endif
#ifdef SIGTRAP
	{ SIGTRAP, "+SIGTRAP+" },
#endif
#ifdef SIGABRT
	{ SIGABRT, "+SIGABRT+" },
#endif
#ifdef SIGEMT
	{ SIGEMT, "+SIGEMT+" },
#endif
#ifdef SIGFPE
	{ SIGFPE, "+SIGFPE+" },
#endif
#ifdef SIGKILL
	{ SIGKILL, "+SIGKILL+" },
#endif
#ifdef SIGBUS
	{ SIGBUS, "+SIGBUS+" },
#endif
#ifdef SIGSEGV
	{ SIGSEGV, "+SIGSEGV+" },
#endif
#ifdef SIGSYS
	{ SIGSYS, "+SIGSYS+" },
#endif
#ifdef SIGPIPE
	{ SIGPIPE, "+SIGPIPE+" },
#endif
#ifdef SIGALRM
	{ SIGALRM, "+SIGALRM+" },
#endif
#ifdef SIGTERM
	{ SIGTERM, "+SIGTERM+" },
#endif
#ifdef SIGURG
	{ SIGURG, "+SIGURG+" },
#endif
#ifdef SIGSTOP
	{ SIGSTOP, "+SIGSTOP+" },
#endif
#ifdef SIGTSTP
	{ SIGTSTP, "+SIGTSTP+" },
#endif
#ifdef SIGCONT
	{ SIGCONT, "+SIGCONT+" },
#endif
#ifdef SIGCHLD
	{ SIGCHLD, "+SIGCHLD+" },
#endif
#ifdef SIGTTIN
	{ SIGTTIN, "+SIGTTIN+" },
#endif
#ifdef SIGTTOU
	{ SIGTTOU, "+SIGTTOU+" },
#endif
#ifdef SIGIO
	{ SIGIO, "+SIGIO+" },
#endif
#ifdef SIGXCPU
	{ SIGXCPU, "+SIGXCPU+" },
#endif
#ifdef SIGXFSZ
	{ SIGXFSZ, "+SIGXFSZ+" },
#endif
#ifdef SIGVTALRM
	{ SIGVTALRM, "+SIGVTALRM+" },
#endif
#ifdef SIGPROF
	{ SIGPROF, "+SIGPROF+" },
#endif
#ifdef SIGWINCH
	{ SIGWINCH, "+SIGWINCH+" },
#endif
#ifdef SIGINFO
	{ SIGINFO, "+SIGINFO+" },
#endif
#ifdef SIGUSR1
	{ SIGUSR1, "+SIGUSR1+" },
#endif
#ifdef SIGUSR2
	{ SIGUSR2, "+SIGUSR2+" },
#endif
#ifdef SIGTHR
	{ SIGTHR, "+SIGTHR+" },
#endif
	{ -1, "" }
};

#ifdef HAVE_SIGPROCMASK
static sigset_t main_thread_sigmask;
# define handler_fn_protype(name, sig, info, aux) name(sig, info, aux)
# define call_handler(name, sig, info, aux) name(sig, info, aux)
# define reinstall_signal(x,y) mysignal(x,y)
# define copy_siginfo(x,y) memcpy(x, y, sizeof(struct sigaction))
static void
mysignal(int code, void (*handler)(int, siginfo_t *, void*))
{
	struct sigaction action;
	sigaction(code, NULL, &action);
        if (handler == SIG_IGN || handler == SIG_DFL) {
                action.sa_handler = handler;
        } else {
#ifdef SA_SIGINFO
                action.sa_sigaction = handler;
                action.sa_flags = SA_SIGINFO;
# if 0 && defined(SA_ONSTACK)
                if (code == SIGSEGV) {
                        action.sa_flags |= SA_ONSTACK;
                }
# endif
#else
                action.sa_handler = handler;
                action.sa_flags = 0;
#endif
                sigfillset(&action.sa_mask);
        }
	sigaction(code, &action, NULL);
}
#else /* HAVE_SIGPROCMASK */
# define handler_fn_protype(name, sig, info, aux) name(sig)
# define call_handler(name, sig, info, aux) name(sig)
# define mysignal(x,y) signal(x,y)
# define reinstall_signal(x,y) signal(x,y)
# define copy_siginfo(x,y)
#endif

static bool
interrupts_disabled_by_C(cl_env_ptr the_env)
{
	return the_env->disable_interrupts;
}

static bool
interrupts_disabled_by_lisp(cl_env_ptr the_env)
{
	return (ecl_get_option(ECL_OPT_BOOTED) &&
		ecl_symbol_value(@'ext::*interrupts-enabled*') == Cnil);
}

static cl_object pop_signal(cl_env_ptr env);

static cl_object
handler_fn_protype(lisp_signal_handler, int sig, siginfo_t *info, void *aux)
{
	cl_env_ptr the_env = ecl_process_env();
        /* The lisp environment might not be installed. */
        if (the_env == NULL)
                return Cnil;
#if defined(ECL_THREADS) && !defined(ECL_MS_WINDOWS_HOST)
        if (sig == ecl_get_option(ECL_OPT_THREAD_INTERRUPT_SIGNAL)) {
		return pop_signal(the_env);
        }
#endif
	switch (sig) {
	case SIGINT: {
                cl_object function = SYM_FUN(@'si::terminal-interrupt');
                return function? function : Cnil;
        }
	case SIGFPE: {
		cl_object condition = @'arithmetic-error';
		int code = 0;
#ifdef _MSC_VER
                switch (_fpecode) {
                case _FPE_INVALID:
                        condition = @'floating-point-invalid-operation';
			code = FE_INVALID;
                        break;
                case _FPE_OVERFLOW:
                        condition = @'floating-point-overflow';
			code = FE_OVERFLOW;
                        break;
                case _FPE_UNDERFLOW:
                        condition = @'floating-point-underflow';
			code = FE_UNDERFLOW;
                        break;
                case _FPE_ZERODIVIDE:
                        condition = @'division-by-zero';
			code = FE_DIVBYZERO;
                        break;
                }
#else
# if defined(HAVE_FENV_H) & !defined(ECL_AVOID_FENV_H)
                code = fetestexcept(FE_ALL_EXCEPT);
		if (code & FE_DIVBYZERO) {
			condition = @'division-by-zero';
			code = FE_DIVBYZERO;
		} else if (code & FE_INVALID) {
			condition = @'floating-point-invalid-operation';
			code = FE_INVALID;
		} else if (code & FE_OVERFLOW) {
			condition = @'floating-point-overflow';
			code = FE_OVERFLOW;
		} else if (code & FE_UNDERFLOW) {
			condition = @'floating-point-underflow';
			code = FE_UNDERFLOW;
		} else if (code & FE_INEXACT) {
			condition = @'floating-point-inexact';
			code = FE_INEXACT;
		}
                feclearexcept(FE_ALL_EXCEPT);
# endif
#endif /* !_MSC_VER */
#ifdef SA_SIGINFO
		if (info) {
			if (info->si_code == FPE_INTDIV || info->si_code == FPE_FLTDIV) {
				condition = @'division-by-zero';
				code = FE_DIVBYZERO;
			} else if (info->si_code == FPE_FLTOVF) {
				condition = @'floating-point-overflow';
				code = FE_OVERFLOW;
			} else if (info->si_code == FPE_FLTUND) {
				condition = @'floating-point-underflow';
				code = FE_UNDERFLOW;
			} else if (info->si_code == FPE_FLTRES) {
				condition = @'floating-point-inexact';
				code = FE_INEXACT;
			} else if (info->si_code == FPE_FLTINV) {
				condition = @'floating-point-invalid-operation';
				code = FE_INVALID;
			}
		}
#endif
                /*
		if (code && !(code & the_env->trap_fpe_bits))
			condition = Cnil;
                */
		si_trap_fpe(@'last', Ct);
                return condition;
	}
	case SIGSEGV:
                return @'ext::segmentation-violation';
#ifdef SIGBUS
	case SIGBUS:
                return @'ext::segmentation-violation';
#endif
#ifdef SIGCHLD
        case SIGCHLD:
                return SYM_FUN(@'si::wait-for-all-processes');
#endif
	default:
		return MAKE_FIXNUM(sig);
	}
}

#define unblock_signal(sig)
#ifdef HAVE_SIGPROCMASK
# undef unblock_signal
static void
unblock_signal(int signal)
{
	/*
	 * We do not really "unblock" the signal, but rather restore
	 * ECL's default sigmask.
	 */
# ifdef ECL_THREADS
	pthread_sigmask(SIG_SETMASK, ecl_process_env()->default_sigmask, NULL);
# else
	sigprocmask(SIG_SETMASK, ecl_process_env()->default_sigmask, NULL);
# endif
}
#endif

ecl_def_ct_base_string(str_ignore_signal,"Ignore signal",13,static,const);

static void
handle_signal_now(cl_object signal_code)
{
        switch (type_of(signal_code)) {
        case t_fixnum:
                cl_cerror(4, str_ignore_signal, @'ext::unix-signal-received',
                          @':code', signal_code);
                break;
        case t_symbol:
                cl_cerror(2, str_ignore_signal, signal_code);
                break;
        case t_cfun:
        case t_cfunfixed:
        case t_cclosure:
        case t_bytecodes:
        case t_bclosure:
                cl_funcall(1, signal_code);
        default:
                break;
        }
}

cl_object
si_handle_signal(cl_object signal_code)
{
	handle_signal_now(signal_code);
	@(return)
}

static void
create_signal_queue(cl_index size)
{
	cl_object base = cl_make_list(1, MAKE_FIXNUM(size));
#ifdef ECL_THREADS
	{
		cl_object lock = mp_make_lock(2, @':name', @'mp::interrupt-process');
		mp_get_lock(1, lock);
		cl_core.signal_queue = base;
		cl_core.signal_queue_lock = lock;
		mp_giveup_lock(lock);
	}
#else
	cl_core.signal_queue = base;
#endif
}

static void
queue_signal(cl_env_ptr env, cl_object code)
{
	cl_object record = Cnil;
#ifdef ECL_THREADS
	cl_object lock = cl_core.signal_queue_lock;
	/* The queue only exists when we have booted */
	if (lock == Cnil) {
                return;
        }
        mp_get_lock(1, lock);
#endif
        record = cl_core.signal_queue;
	if (record == OBJNULL)
		return;
        cl_core.signal_queue = CDR(record);
#ifdef ECL_THREADS
        mp_giveup_lock(lock);
#endif
        /* If record == Cnil, signal is lost! The queue was
         * too small.  We can no allocate further memory,
         * since we are in a signal handler. */
	if (record != Cnil) {
		ECL_CONS_CDR(record) = env->pending_interrupt;
		ECL_CONS_CAR(record) = code;
		env->pending_interrupt = record;
	}
}

static cl_object
pop_signal(cl_env_ptr env)
{
	cl_object record = env->pending_interrupt;
	if (record != Cnil && record != NULL) {
#ifdef ECL_THREADS
		cl_object lock = cl_core.signal_queue_lock;
		mp_get_lock(1, lock);
#endif
                env->pending_interrupt = ECL_CONS_CDR(record);
		ECL_CONS_CDR(record) = cl_core.signal_queue;
		cl_core.signal_queue = record;
		record = ECL_CONS_CAR(record);
#ifdef ECL_THREADS
		mp_giveup_lock(lock);
#endif
	}
	return record;
}

static void
handle_or_queue(cl_object signal_code, int code)
{
	int old_errno = errno;
	cl_env_ptr the_env;
        if (Null(signal_code) || signal_code == NULL)
                return;
        the_env = ecl_process_env();
	/*
	 * If interrupts are disabled by lisp we are not so eager on
	 * detecting when the interrupts become enabled again. We
	 * queue the signal and are done with that.
	 */
	if (interrupts_disabled_by_lisp(the_env)) {
		queue_signal(the_env, signal_code);
		errno = old_errno;
	}
	/*
	 * If interrupts are disabled by C, and we have not pushed a
	 * pending signal, save this signal and return. On platforms
	 * in which mprotect() works, we block all write access to the
	 * environment for a cheap check of pending interrupts. On other
	 * platforms we change the value of disable_interrupts to 3, so
	 * that we detect changes.
	 */
	else if (interrupts_disabled_by_C(the_env)) {
		the_env->disable_interrupts = 3;
		queue_signal(the_env, signal_code);
#ifdef ECL_USE_MPROTECT
		if (mprotect(the_env, sizeof(*the_env), PROT_READ) < 0) {
			ecl_internal_error("Unable to mprotect environment.");
		}
#else
# ifdef ECL_USE_GUARD_PAGE
                if (!VirtualProtect(the_env, sizeof(*the_env), PAGE_GUARD, NULL)) {
			ecl_internal_error("Unable to mprotect environment.");
		}
# endif
#endif
		errno = old_errno;
	}
	/*
	 * If interrupts are enabled, that means we are in a safe area
	 * and may execute arbitrary lisp code. We can thus call the
	 * appropriate handlers.
	 */
	else {
                errno = old_errno;
                if (code) unblock_signal(code);
		si_trap_fpe(@'last', Ct); /* Clear FPE exception flag */
                handle_signal_now(signal_code);
        }
}

static void
handler_fn_protype(non_evil_signal_handler, int sig, siginfo_t *siginfo, void *data)
{
        int old_errno = errno;
        cl_object signal_object;
	reinstall_signal(sig, non_evil_signal_handler);
	if (!ecl_get_option(ECL_OPT_BOOTED)) {
		ecl_internal_error("Got signal before environment was installed"
				   " on our thread.");
	}
        signal_object = call_handler(lisp_signal_handler, sig, siginfo, data);
        errno = old_errno;
        handle_or_queue(signal_object, sig);
}

static void
handler_fn_protype(sigsegv_handler, int sig, siginfo_t *info, void *aux)
{
        static const char *stack_overflow_msg =
                "\n;;;\n;;; Stack overflow.\n"
                ";;; Jumping to the outermost toplevel prompt\n"
                ";;;\n\n";
#ifndef HAVE_SIGPROCMASK
        static const char *segv_msg =
                "\n;;;\n"
                ";;; Detected access to protected memory, "
                "also kwown as 'segmentation fault'.\n"
                ";;; Jumping to the outermost toplevel prompt\n"
                ";;;\n\n";
#endif
	cl_env_ptr the_env;
	reinstall_signal(sig, sigsegv_handler);
	if (!ecl_get_option(ECL_OPT_BOOTED)) {
		ecl_internal_error("Got signal before environment was installed"
				   " on our thread.");
	}
	the_env = ecl_process_env();
        /* The lisp environment might not be installed. */
        if (the_env == NULL)
                return;
#if defined(SA_SIGINFO) && defined(ECL_USE_MPROTECT)
	/* We access the environment when it was protected. That
	 * means there was a pending signal. */
	if (((char*)the_env <= (char*)info->si_addr) &&
            ((char*)info->si_addr <= (char*)(the_env+1)))
        {
		cl_object signal;
		mprotect(the_env, sizeof(*the_env), PROT_READ | PROT_WRITE);
                the_env->disable_interrupts = 0;
                unblock_signal(SIGBUS);
                for (signal = pop_signal(the_env); !Null(signal) && signal; ) {
                        handle_signal_now(signal);
                        signal = pop_signal(the_env);
                }
                return;
	}
#endif
#ifdef HAVE_SIGPROCMASK
# ifdef ECL_DOWN_STACK
	if ((char*)info->si_addr > the_env->cs_barrier &&
	    (char*)info->si_addr <= the_env->cs_org) {
                unblock_signal(SIGSEGV);
		ecl_unrecoverable_error(the_env, stack_overflow_msg);
                return;
	}
# else
	if ((char*)info->si_addr < the_env->cs_barrier &&
	    (char*)info->si_addr >= the_env->cs_org) {
                unblock_signal(SIGSEGV);
		ecl_unrecoverable_error(the_env, stack_overflow_msg);
                return;
	}
# endif
# if 0 && defined(SA_ONSTACK)
	/* The handler is executed in an externally allocated stack, and
         * thus it is not safe to execute lisp code here. We just bounce
         * up to the outermost toplevel.
	 */
        unblock_signal(SIGSEGV);
	ecl_unrecoverable_error(the_env, segv_msg);
# else
        handle_or_queue(@'ext::segmentation-violation', SIGSEGV);
# endif
#else
	/*
	 * We cannot distinguish between a stack overflow and a simple
	 * access violation. Thus we assume the worst case and jump to
	 * the outermost handler.
	 */
        unblock_signal(SIGSEGV);
	ecl_unrecoverable_error(the_env, segv_msg);
#endif
}

#ifdef SIGBUS
static void
handler_fn_protype(sigbus_handler, int sig, siginfo_t *info, void *aux)
{
        cl_env_ptr the_env;
	reinstall_signal(sig, sigsegv_handler);
	the_env = ecl_process_env();
        /* The lisp environment might not be installed. */
        if (the_env == NULL)
                return;
#if defined(SA_SIGINFO) && defined(ECL_USE_MPROTECT)
	/* We access the environment when it was protected. That
	 * means there was a pending signal. */
	if (((char*)the_env <= (char*)info->si_addr) &&
            ((char*)info->si_addr <= (char*)(the_env+1)))
        {
		cl_object signal;
		mprotect(the_env, sizeof(*the_env), PROT_READ | PROT_WRITE);
                the_env->disable_interrupts = 0;
                unblock_signal(SIGBUS);
                for (signal = pop_signal(the_env); !Null(signal) && signal; ) {
                        handle_signal_now(signal);
                        signal = pop_signal(the_env);
                }
                return;
	}
#endif
        handle_or_queue(@'ext::segmentation-violation', SIGBUS);
}
#endif

cl_object
si_check_pending_interrupts(void)
{
	ecl_check_pending_interrupts();
	@(return)
}

void
ecl_check_pending_interrupts(void)
{
	const cl_env_ptr env = ecl_process_env();
	cl_object sig;
	env->disable_interrupts = 0;
	sig = env->pending_interrupt;
	if (sig != Cnil && sig != NULL) {
		handle_signal_now(pop_signal(env));
	}
}

static cl_object
do_catch_signal(int code, cl_object action, cl_object process)
{
        if (action == Cnil || action == @':ignore') {
                mysignal(code, SIG_IGN);
                return Ct;
        } else if (action == @':default') {
                mysignal(code, SIG_DFL);
                return Ct;
        } else if (action == Ct || action == @':catch') {
                if (code == SIGSEGV) {
                        mysignal(code, sigsegv_handler);
                }
#ifdef SIGBUS
                else if (code == SIGBUS) {
                        mysignal(code, sigbus_handler);
                }
#endif
#ifdef SIGCHLD
                else if (code == SIGCHLD) {
# ifndef ECL_THREADS
                        mysignal(SIGCHLD, non_evil_signal_handler);
# endif
                }
#endif
                else {
                        mysignal(code, non_evil_signal_handler);
                }
        }
#ifdef HAVE_SIGPROCMASK
# ifdef ECL_THREADS
        if (type_of(process) == t_process) {
                cl_env_ptr env = process->process.env;
                sigset_t *handled_set = (sigset_t *)env->default_sigmask;
                if (action == @':mask') {
                        sigaddset(handled_set, code);
                } else if (action == @':unmask') {
                        sigdelset(handled_set, code);
                } else {
                        return do_catch_signal(code, Ct, process);
                }
                if (env == ecl_process_env()) {
                        pthread_sigmask(SIG_SETMASK, handled_set, NULL);
                }
                return Ct;
        }
# endif
        {
                sigset_t handled_set;
                sigprocmask(SIG_SETMASK, NULL, &handled_set);
                if (action == @':mask') {
                        sigaddset(&handled_set, code);
                        printf(";;; %d masked\n", code);
                } else if (action == @':unmask') {
                        sigdelset(&handled_set, code);
                        printf(";;; %d unmasked\n", code);
                } else {
                        return do_catch_signal(code, Ct, process);
                }
                sigprocmask(SIG_SETMASK, &handled_set, NULL);
                return Ct;
        }
#else
        return Cnil;
#endif
}

@(defun ext::catch-signal (code flag &key process)
@
{
        cl_object output = Cnil;
	int code_int = ecl_to_int(code);
	int i;
#ifdef GBC_BOEHM
# ifdef SIGSEGV
	if ((code_int == SIGSEGV) && ecl_get_option(ECL_OPT_INCREMENTAL_GC))
		FEerror("It is not allowed to change the behavior of SIGSEGV.",
			0);
# endif
# ifdef SIGBUS
	if (code_int == SIGBUS)
		FEerror("It is not allowed to change the behavior of SIGBUS.",
			0);
# endif
#endif
#if defined(ECL_THREADS) && !defined(ECL_MS_WINDOWS_HOST)
	if (code_int == ecl_get_option(ECL_OPT_THREAD_INTERRUPT_SIGNAL)) {
		FEerror("It is not allowed to change the behavior of ~D", 1,
                        MAKE_FIXNUM(code_int));
	}
#endif
	for (i = 0; known_signals[i].code >= 0; i++) {
		if (known_signals[i].code == code_int) {
                        output = do_catch_signal(code_int, flag, process);
                        break;
		}
	}
	@(return output)
}
@)

#ifdef ECL_THREADS
# ifdef ECL_WINDOWS_THREADS
static VOID CALLBACK
wakeup_function(ULONG_PTR foo)
{
	cl_env_ptr env = ecl_process_env();
	volatile i = env->nvalues;
	env->nvalues = i;
}
# endif

static bool
do_interrupt_thread(cl_object process)
{
# ifdef ECL_WINDOWS_THREADS
#  ifndef ECL_USE_GUARD_PAGE
#   error "Cannot implement ecl_interrupt_process without guard pages"
#  endif
        HANDLE thread = (HANDLE)process->process.thread;
	CONTEXT context;
        void *trap_address = process->process.env;
	DWORD guard = PAGE_GUARD | PAGE_READWRITE;
        int ok = 1;
        if (SuspendThread(thread) == (DWORD)-1) {
		FEwin32_error("Unable to suspend thread ~A", 1,
			      process);
		ok = 0;
		goto EXIT;
	}
        process->process.interrupt = Ct;
        if (!VirtualProtect(process->process.env,
			    sizeof(struct cl_env_struct),
			    guard,
			    &guard))
	{
		FEwin32_error("Unable to protect memory from thread ~A",
			      1, process);
		ok = 0;
	}
 RESUME:
	if (!QueueUserAPC(wakeup_function, thread, 0)) {
		FEwin32_error("Unable to queue APC call to thread ~A",
			      1, process);
		ok = 0;
	}
	if (ResumeThread(thread) == (DWORD)-1)  {
		FEwin32_error("Unable to resume thread ~A", 1,
			      process);
		ok = 0;
		goto EXIT;
	}
 EXIT:
        return ok;
# else
        int signal = ecl_get_option(ECL_OPT_THREAD_INTERRUPT_SIGNAL);
        return pthread_kill(process->process.thread, signal) == 0;
# endif
}

void
ecl_interrupt_process(cl_object process, cl_object function)
{
        /*
         * We first get the lock to ensure that we do not interrupt
         * the thread while acquiring the queue lock. Then the code
         * takes two different approaches depending on the platform:
         *
         * - In Windows it sets up a trap in the stack, so that the
         *   uncaught exception handler can catch it and process it.
         * - In POSIX systems it sends a user level interrupt to
         *   the thread, which then decides how to act.
         */
        cl_object lock;
        if (process->process.active == 1) {
                int ok;
                function = si_coerce_to_function(function);
                lock = mp_get_lock_wait(cl_core.signal_queue_lock);
                queue_signal(process->process.env, function);
                ok = do_interrupt_thread(process);
                mp_giveup_lock(lock);
                if (ok) return;
        }
        FEerror("Cannot interrupt process ~A", 1, process);
}
#endif /* ECL_THREADS */

#ifdef ECL_WINDOWS_THREADS
static LPTOP_LEVEL_EXCEPTION_FILTER old_W32_exception_filter = NULL;

LONG WINAPI
_ecl_w32_exception_filter(struct _EXCEPTION_POINTERS* ep)
{
	LONG excpt_result;

	excpt_result = EXCEPTION_CONTINUE_EXECUTION;
	switch (ep->ExceptionRecord->ExceptionCode)
	{
                /* Access to guard page */
        	case STATUS_GUARD_PAGE_VIOLATION: {
                        cl_env_ptr env = ecl_process_env();
                        cl_object process = env->own_process;
                        if (!Null(process->process.interrupt)) {
                                cl_object signal = pop_signal(env);
                                process->process.interrupt = Cnil;
                                while (signal != Cnil && signal) {
                                        handle_signal_now(signal);
                                        signal = pop_signal(env);
                                }
                                return EXCEPTION_CONTINUE_EXECUTION;
                        }
                }
		/* Catch all arithmetic exceptions */
		case EXCEPTION_INT_DIVIDE_BY_ZERO:
                        handle_or_queue(@'division-by-zero', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_INT_OVERFLOW:
                        handle_or_queue(@'arithmetic-error', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_DIVIDE_BY_ZERO:
                        handle_or_queue(@'floating-point-overflow', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_OVERFLOW:
                        handle_or_queue(@'floating-point-overflow', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_UNDERFLOW:
                        handle_or_queue(@'floating-point-underflow', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_INEXACT_RESULT:
                        handle_or_queue(@'floating-point-inexact', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_DENORMAL_OPERAND:
		case EXCEPTION_FLT_INVALID_OPERATION:
                        handle_or_queue(@'floating-point-invalid-operation', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		case EXCEPTION_FLT_STACK_CHECK:
                        handle_or_queue(@'arithmetic-error', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		/* Catch segmentation fault */
		case EXCEPTION_ACCESS_VIOLATION:
                        handle_or_queue(@'ext::segmentation-violation', 0);
                        return EXCEPTION_CONTINUE_EXECUTION;
		/* Catch illegal instruction */
		case EXCEPTION_ILLEGAL_INSTRUCTION:
			handle_or_queue(MAKE_FIXNUM(SIGILL), 0);
			return EXCEPTION_CONTINUE_EXECUTION;
		/* Do not catch anything else */
		default:
			excpt_result = EXCEPTION_CONTINUE_SEARCH;
			break;
	}
        if (old_W32_exception_filter)
                return old_W32_exception_filter(ep);
	return excpt_result;
}

static cl_object
W32_handle_in_new_thread(cl_object signal_code)
{
	int outside_ecl = ecl_import_current_thread(@'si::handle-signal', Cnil);
	mp_process_run_function(3, @'si::handle-signal',
				@'si::handle-signal',
				signal_code);
	if (outside_ecl) ecl_release_current_thread();
}

BOOL WINAPI W32_console_ctrl_handler(DWORD type)
{
	switch (type)
	{
		/* Catch CTRL-C */
	case CTRL_C_EVENT: {
		cl_object function = SYM_FUN(@'si::terminal-interrupt');
		if (function)
			W32_handle_in_new_thread(function);
		return TRUE;
	}
	}
	return FALSE;
}
#endif /* ECL_WINDOWS_THREADS */

#if defined(ECL_THREADS) && defined(HAVE_SIGPROCMASK)
static cl_object
asynchronous_signal_servicing_thread()
{
	sigset_t handled_set;
	cl_object signal_code;
	int signo;
	int interrupt_signal = 0;
	if (ecl_get_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL)) {
		interrupt_signal = ecl_get_option(ECL_OPT_THREAD_INTERRUPT_SIGNAL);
	}
        /*
         * We wait here for all signals that are blocked in all other
         * threads. It would be desirable to be able to wait for _all_
         * signals, but this can not be done for SIGFPE, SIGSEGV, etc.
         */
	pthread_sigmask(SIG_SETMASK, NULL, &handled_set);
	/*
	 * Under OS X we also have to explicitely add the signal we
	 * use to communicate process interrupts. For some unknown
	 * reason those signals may get lost.
	 */
#ifdef SIGCHLD
        sigaddset(&handled_set, SIGCHLD);
#endif
	if (interrupt_signal) {
		sigaddset(&handled_set, interrupt_signal);
		pthread_sigmask(SIG_SETMASK, &handled_set, NULL);
	}
	CL_CATCH_ALL_BEGIN(ecl_process_env()) {
	for (;;) {
		/* Waiting may fail! */
		int status = sigwait(&handled_set, &signo);
		if (status == 0) {
			if (signo == interrupt_signal)
				goto RETURN;
#ifdef SIGCHLD
                        if (signo == SIGCHLD) {
                                si_wait_for_all_processes(0);
                                continue;
                        }
#endif
			signal_code = call_handler(lisp_signal_handler, signo,
						   NULL, NULL);
			if (!Null(signal_code)) {
				mp_process_run_function(3, @'si::handle-signal',
							@'si::handle-signal',
							signal_code);
			}
		}
	}
	} CL_CATCH_ALL_END;
 RETURN:
	@(return)
}
#endif

cl_object
si_trap_fpe(cl_object condition, cl_object flag)
{
        cl_env_ptr the_env = ecl_process_env();
#ifndef FE_ALL_EXCEPT
# define FE_ALL_EXCEPT FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID
#endif
        const int all = FE_ALL_EXCEPT;
	int bits = 0;
        if (condition == @'last') {
		bits = the_env->trap_fpe_bits;
        } else {
                if (condition == Ct)
                        bits = FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID;
		else if (condition == @'division-by-zero')
                        bits = FE_DIVBYZERO;
                else if (condition == @'floating-point-overflow')
                        bits = FE_OVERFLOW;
                else if (condition == @'floating-point-underflow')
                        bits = FE_UNDERFLOW;
                else if (condition == @'floating-point-invalid-operation')
                        bits = FE_INVALID;
                else if (condition == @'floating-point-inexact')
                        bits = FE_INEXACT;
                else if (FIXNUMP(condition))
			bits = fix(condition) & all;
                if (flag == Cnil) {
                        bits = the_env->trap_fpe_bits & ~bits;
                } else {
                        bits = the_env->trap_fpe_bits | bits;
                }
        }
#if !defined(ECL_AVOID_FPE_H)
# ifdef HAVE_FENV_H
        feclearexcept(all);
# endif
# if defined(ECL_MS_WINDOWS_HOST)
	_fpreset();
# endif
# ifdef HAVE_FEENABLEEXCEPT
        fedisableexcept(all & ~bits);
        feenableexcept(all & bits);
# endif
#endif
        the_env->trap_fpe_bits = bits;
	@(return MAKE_FIXNUM(bits))
}

/*
 * In this code we decide whether to install a process-wide signal
 * handler for each of the asynchronous signals (SIGINT, SIGTERM,
 * SIGCHLD...) or we block the signal and let the background thread
 * detect and process them.
 */
static void
install_asynchronous_signal_handlers()
{
#if defined(ECL_MS_WINDOWS_HOST)
# define async_handler(signal,handler,mask)
#else
# if defined(ECL_THREADS) && defined(HAVE_SIGPROCMASK)
#  define async_handler(signal,handler,mask)  {				\
		if (ecl_get_option(ECL_OPT_SIGNAL_HANDLING_THREAD)) {	\
			sigaddset(mask, signal);			\
		} else {						\
			mysignal(signal,handler);			\
		}}
# else
#  define async_handler(signal,handler,mask)	\
	mysignal(signal,handler)
# endif
#endif
#ifdef HAVE_SIGPROCMASK
	sigset_t *sigmask = cl_core.default_sigmask = &main_thread_sigmask;
        cl_core.default_sigmask_bytes = sizeof(sigset_t);
# ifdef ECL_THREADS
	pthread_sigmask(SIG_SETMASK, NULL, sigmask);
# else
        sigprocmask(SIG_SETMASK, NULL, sigmask);
# endif
#endif
#ifdef SIGINT
	if (ecl_get_option(ECL_OPT_TRAP_SIGINT)) {
		async_handler(SIGINT, non_evil_signal_handler, sigmask);
	}
#endif
#ifdef SIGCHLD
	if (ecl_get_option(ECL_OPT_TRAP_SIGCHLD)) {
                /* We have to set the process signal handler explicitly,
                 * because on many platforms the default is SIG_IGN. */
		mysignal(SIGCHLD, lisp_signal_handler);
		async_handler(SIGCHLD, lisp_signal_handler, sigmask);
	}
#endif
#ifdef HAVE_SIGPROCMASK
# if defined(ECL_THREADS)
	pthread_sigmask(SIG_SETMASK, sigmask, NULL);
# else
	sigprocmask(SIG_SETMASK, sigmask, NULL);
# endif
#endif
#ifdef ECL_WINDOWS_THREADS
	old_W32_exception_filter =
		SetUnhandledExceptionFilter(_ecl_w32_exception_filter);
	if (ecl_get_option(ECL_OPT_TRAP_SIGINT)) {
		SetConsoleCtrlHandler(W32_console_ctrl_handler, TRUE);
	}
#endif
#undef async_handler
}

/*
 * In POSIX systems we may set up a background thread that detects
 * synchronous signals and spawns a new thread to handle each of them.
 */
static void
install_signal_handling_thread()
{
#if defined(ECL_THREADS) && defined(HAVE_SIGPROCMASK)
        ecl_process_env()->default_sigmask = &main_thread_sigmask;
	if (ecl_get_option(ECL_OPT_SIGNAL_HANDLING_THREAD)) {
		cl_object fun =
			ecl_make_cfun((cl_objectfn_fixed)
				      asynchronous_signal_servicing_thread,
				      @'si::signal-servicing',
				      Cnil,
				      0);
		cl_object process =
			mp_process_run_function_wait(2,
                                                     @'si::signal-servicing',
                                                     fun);
		if (Null(process)) {
			ecl_internal_error("Unable to create signal "
					   "servicing thread");
		}
	}
#endif
}

/*
 * In order to implement MP:INTERRUPT-PROCESS, MP:PROCESS-KILL and the
 * like, we use signals. This routine sets up a synchronous signal
 * handler for that particular signal.
 */
static void
install_process_interrupt_handler()
{
#ifdef SIGRTMIN
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGRTMIN + 2
#else
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGUSR1
#endif
#if defined(ECL_THREADS) && !defined(ECL_MS_WINDOWS_HOST)
	if (ecl_get_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL)) {
		int signal = ecl_get_option(ECL_OPT_THREAD_INTERRUPT_SIGNAL);
		if (signal == 0) {
			signal = DEFAULT_THREAD_INTERRUPT_SIGNAL;
			ecl_set_option(ECL_OPT_THREAD_INTERRUPT_SIGNAL,
				       signal);
		}
		mysignal(signal, non_evil_signal_handler);
#ifdef HAVE_SIGROCMASK
                sigdelset(ecl_process_env()->default_sigmask, signal);
                pthread_sigmask(SIG_SETMASK, ecl_process_env()->default_sigmask, NULL);
#endif
	}
#endif
}

/*
 * This routine sets up handlers for all exceptions, such as access to
 * restricted regions of memory. They have to be set up before we call
 * init_GC().
 */
static void
install_synchronous_signal_handlers()
{
#ifdef SIGBUS
	if (ecl_get_option(ECL_OPT_TRAP_SIGBUS)) {
		mysignal(SIGBUS, sigbus_handler);
	}
#endif
#ifdef SIGSEGV
	if (ecl_get_option(ECL_OPT_TRAP_SIGSEGV)) {
		mysignal(SIGSEGV, sigsegv_handler);
	}
#endif
#ifdef SIGPIPE
	if (ecl_get_option(ECL_OPT_TRAP_SIGPIPE)) {
		mysignal(SIGPIPE, non_evil_signal_handler);
	}
#endif
}

/*
 * This routine sets up handlers for floating point exceptions. We
 * cannot do it earlier because it requires the memory allocator to
 * be set up.
 */
static void
install_fpe_signal_handlers()
{
#ifdef SIGFPE
	if (ecl_get_option(ECL_OPT_TRAP_SIGFPE)) {
		mysignal(SIGFPE, non_evil_signal_handler);
		si_trap_fpe(Ct, Ct);
# ifdef ECL_IEEE_FP
		/* By default deactivate errors and accept
		 * denormals in floating point computations */
		si_trap_fpe(@'floating-point-invalid-operation', Cnil);
		si_trap_fpe(@'division-by-zero', Cnil);
		si_trap_fpe(@'floating-point-overflow', Cnil);
# endif
	}
#endif
}

/*
 * Create one Common Lisp constant for each signal that we know,
 * such as +SIGINT+ for SIGINT, etc.
 */
static void
create_signal_code_constants()
{
	int i;
	for (i = 0; known_signals[i].code >= 0; i++) {
		cl_object name =
			_ecl_intern(known_signals[i].text,
				    cl_core.ext_package);
                cl_export2(name, cl_core.ext_package);
		si_Xmake_constant(name, MAKE_FIXNUM(known_signals[i].code));
	}
}

void
init_unixint(int pass)
{
	if (pass == 0) {
#ifdef ECL_THREADS
		cl_core.signal_queue_lock = Cnil;
#endif
		cl_core.signal_queue = OBJNULL;
		install_asynchronous_signal_handlers();
		install_process_interrupt_handler();
		install_synchronous_signal_handlers();
	} else {
		create_signal_queue(ecl_get_option(ECL_OPT_SIGNAL_QUEUE_SIZE));
		create_signal_code_constants();
		install_fpe_signal_handlers();
		install_signal_handling_thread();
		ECL_SET(@'ext::*interrupts-enabled*', Ct);
		ecl_process_env()->disable_interrupts = 0;
	}
}
