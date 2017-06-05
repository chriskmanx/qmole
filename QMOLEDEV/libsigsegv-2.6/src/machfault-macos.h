/* Fault handler information.  MacOSX version (both PowerPC and i386).
   Copyright (C) 2003-2004, 2006-2008  Bruno Haible <bruno@clisp.org>

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

#if defined __ppc__ || defined __ppc64__

#if defined __ppc64__
/* 64 bit registers */

#define SIGSEGV_EXC_STATE_TYPE                      ppc_exception_state64_t
#define SIGSEGV_EXC_STATE_FLAVOR                    PPC_EXCEPTION_STATE64
#define SIGSEGV_EXC_STATE_COUNT                     PPC_EXCEPTION_STATE64_COUNT
#define SIGSEGV_THREAD_STATE_TYPE                   ppc_thread_state64_t
#define SIGSEGV_THREAD_STATE_FLAVOR                 PPC_THREAD_STATE64
#define SIGSEGV_THREAD_STATE_COUNT                  PPC_THREAD_STATE64_COUNT

#else
/* 32 bit registers */

#define SIGSEGV_EXC_STATE_TYPE                      ppc_exception_state_t
#define SIGSEGV_EXC_STATE_FLAVOR                    PPC_EXCEPTION_STATE
#define SIGSEGV_EXC_STATE_COUNT                     PPC_EXCEPTION_STATE_COUNT
#define SIGSEGV_THREAD_STATE_TYPE                   ppc_thread_state_t
#define SIGSEGV_THREAD_STATE_FLAVOR                 PPC_THREAD_STATE
#define SIGSEGV_THREAD_STATE_COUNT                  PPC_THREAD_STATE_COUNT

#endif

#if MacOS_X_10_5_HEADERS && __DARWIN_UNIX03
#define SIGSEGV_FAULT_ADDRESS(thr_state,exc_state)  (exc_state).__dar
#define SIGSEGV_STACK_POINTER(thr_state)            (thr_state).__r1
#define SIGSEGV_PROGRAM_COUNTER(thr_state)          (thr_state).__srr0
#define SIGSEGV_INTEGER_ARGUMENT_1(thr_state)       (thr_state).__r3
#define SIGSEGV_INTEGER_ARGUMENT_2(thr_state)       (thr_state).__r4
#define SIGSEGV_INTEGER_ARGUMENT_3(thr_state)       (thr_state).__r5
#else
#define SIGSEGV_FAULT_ADDRESS(thr_state,exc_state)  (exc_state).dar
#define SIGSEGV_STACK_POINTER(thr_state)            (thr_state).r1
#define SIGSEGV_PROGRAM_COUNTER(thr_state)          (thr_state).srr0
#define SIGSEGV_INTEGER_ARGUMENT_1(thr_state)       (thr_state).r3
#define SIGSEGV_INTEGER_ARGUMENT_2(thr_state)       (thr_state).r4
#define SIGSEGV_INTEGER_ARGUMENT_3(thr_state)       (thr_state).r5
#endif

#endif

#if defined __i386__ || defined __x86_64__

#if defined __x86_64__
/* 64 bit registers */

#define SIGSEGV_THREAD_STATE_TYPE                   x86_thread_state64_t
#define SIGSEGV_THREAD_STATE_FLAVOR                 x86_THREAD_STATE64
#define SIGSEGV_THREAD_STATE_COUNT                  x86_THREAD_STATE64_COUNT
#define SIGSEGV_EXC_STATE_TYPE                      x86_exception_state64_t
#define SIGSEGV_EXC_STATE_FLAVOR                    x86_EXCEPTION_STATE64
#define SIGSEGV_EXC_STATE_COUNT                     x86_EXCEPTION_STATE64_COUNT
#if MacOS_X_10_5_HEADERS && __DARWIN_UNIX03
#define SIGSEGV_FAULT_ADDRESS(thr_state,exc_state)  (exc_state).__faultvaddr
#define SIGSEGV_STACK_POINTER(thr_state)            (thr_state).__rsp
#define SIGSEGV_PROGRAM_COUNTER(thr_state)          (thr_state).__rip
#define SIGSEGV_FRAME_POINTER(thr_state)            (thr_state).__rbp
#define SIGSEGV_INTEGER_ARGUMENT_1(thr_state)       (thr_state).__rdi
#define SIGSEGV_INTEGER_ARGUMENT_2(thr_state)       (thr_state).__rsi
#define SIGSEGV_INTEGER_ARGUMENT_3(thr_state)       (thr_state).__rdx
#else
#define SIGSEGV_FAULT_ADDRESS(thr_state,exc_state)  (exc_state).faultvaddr
#define SIGSEGV_STACK_POINTER(thr_state)            (thr_state).rsp
#define SIGSEGV_PROGRAM_COUNTER(thr_state)          (thr_state).rip
#define SIGSEGV_FRAME_POINTER(thr_state)            (thr_state).rbp
#define SIGSEGV_INTEGER_ARGUMENT_1(thr_state)       (thr_state).rdi
#define SIGSEGV_INTEGER_ARGUMENT_2(thr_state)       (thr_state).rsi
#define SIGSEGV_INTEGER_ARGUMENT_3(thr_state)       (thr_state).rdx
#endif

#else
/* 32 bit registers */

#if defined x86_THREAD_STATE32
/* MacOS X 10.5 or newer introduces the new names and deprecates the old ones */
#define SIGSEGV_THREAD_STATE_TYPE                   x86_thread_state32_t
#define SIGSEGV_THREAD_STATE_FLAVOR                 x86_THREAD_STATE32
#define SIGSEGV_THREAD_STATE_COUNT                  x86_THREAD_STATE32_COUNT
#define SIGSEGV_EXC_STATE_TYPE                      x86_exception_state32_t
#define SIGSEGV_EXC_STATE_FLAVOR                    x86_EXCEPTION_STATE32
#define SIGSEGV_EXC_STATE_COUNT                     x86_EXCEPTION_STATE32_COUNT
#else
#define SIGSEGV_THREAD_STATE_TYPE                   i386_thread_state_t
#define SIGSEGV_THREAD_STATE_FLAVOR                 i386_THREAD_STATE
#define SIGSEGV_THREAD_STATE_COUNT                  i386_THREAD_STATE_COUNT
#define SIGSEGV_EXC_STATE_TYPE                      i386_exception_state_t
#define SIGSEGV_EXC_STATE_FLAVOR                    i386_EXCEPTION_STATE
#define SIGSEGV_EXC_STATE_COUNT                     i386_EXCEPTION_STATE_COUNT
#endif
#if MacOS_X_10_5_HEADERS && __DARWIN_UNIX03
#define SIGSEGV_FAULT_ADDRESS(thr_state,exc_state)  (exc_state).__faultvaddr
#define SIGSEGV_STACK_POINTER(thr_state)            (thr_state).__esp
#define SIGSEGV_PROGRAM_COUNTER(thr_state)          (thr_state).__eip
#else
#define SIGSEGV_FAULT_ADDRESS(thr_state,exc_state)  (exc_state).faultvaddr
#define SIGSEGV_STACK_POINTER(thr_state)            (thr_state).esp
#define SIGSEGV_PROGRAM_COUNTER(thr_state)          (thr_state).eip
#endif

#endif

#endif
