/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

static const char cvs_ident[] = "$Id: system.c 38480 2009-01-06 09:08:48Z mej $";

#include "../config.h"
#include "feature.h"

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif

#include "command.h"
#include "misc.h"
#include "system.h"

/*static eterm_sighandler_t old_handler = (eterm_sighandler_t) NULL;*/

int
wait_for_chld(int system_pid)
{
    int pid, status = 0, save_errno = errno, code;

    D_OPTIONS(("wait_for_chld(%ld) called.\n", system_pid));

    while (1) {
        do {
            errno = 0;
            usleep(10);
        } while ((((pid = waitpid(system_pid, &status, WNOHANG)) == -1) && (errno == EINTR)) || !pid);
        /* If the child that exited is the command we spawned, or if the
           child exited before fork() returned in the parent, it must be
           our immediate child that exited.  We exit gracefully. */
        if ((pid == -1) && (errno == ECHILD)) { /* No children exist.  Punt. */
            errno = save_errno;
            break;
        }
        D_OPTIONS(("%ld exited.\n", pid));
        if (pid == system_pid || system_pid == -1) {
            if (WIFEXITED(status)) {
                code = WEXITSTATUS(status);
                D_OPTIONS(("Child process exited with return code %lu\n", code));
            } else if (WIFSIGNALED(status)) {
                code = WTERMSIG(status);
                D_OPTIONS(("Child process was terminated by unhandled signal %lu\n", code));
            } else {
                code = 0;
            }
            return (code);
        }
        errno = save_errno;
    }
    return 0;
}

/* Replace the system() call with a fork-and-exec that unprivs the child process */
int
system_wait(char *command)
{
    pid_t pid;

    D_OPTIONS(("system_wait(%s) called.\n", command));

    pid = system_no_wait(command);
    return (wait_for_chld(pid));
}

pid_t
system_no_wait(char *command)
{
    pid_t pid;

    D_OPTIONS(("system_no_wait(%s) called.\n", command));

    if (!(pid = fork())) {
        setreuid(my_ruid, my_ruid);
        setregid(my_rgid, my_rgid);
        execl("/bin/sh", "sh", "-c", command, (char *) NULL);
        libast_print_error("execl(%s) failed -- %s\n", command, strerror(errno));
        exit(EXIT_FAILURE);
    }
    D_OPTIONS(("%d:  fork() returned %d\n", getpid(), pid));
    return (pid);
}
