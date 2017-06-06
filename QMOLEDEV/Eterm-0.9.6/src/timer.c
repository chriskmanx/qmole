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

static const char cvs_ident[] = "$Id: timer.c 38480 2009-01-06 09:08:48Z mej $";

#include "config.h"
#include "feature.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>

#include "startup.h"
#include "command.h"
#include "events.h"
#include "options.h"
#include "pixmap.h"
#include "timer.h"

static etimer_t *timers = NULL;

timerhdl_t
timer_add(unsigned long msec, timer_handler_t handler, void *data)
{
    static etimer_t *timer;
    struct timeval tv;

    if (!timers) {
        timers = (etimer_t *) MALLOC(sizeof(etimer_t));
        timer = timers;
        timer->next = NULL;
    } else {
        timer = (etimer_t *) MALLOC(sizeof(etimer_t));
        timer->next = timers;
        timers = timer;
    }
    timer->msec = msec;
    gettimeofday(&tv, NULL);
    timer->time.tv_sec = (msec / 1000) + tv.tv_sec;
    timer->time.tv_usec = ((msec % 1000) * 1000) + tv.tv_usec;
    timer->handler = handler;
    timer->data = data;
    D_TIMER(("Added timer.  Timer set to %lu/%lu with handler %8p and data %8p\n", timer->time.tv_sec, timer->time.tv_usec,
             timer->handler, timer->data));
    return ((timerhdl_t) timer);
}

unsigned char
timer_del(timerhdl_t handle)
{
    register etimer_t *current;
    etimer_t *temp;

    if (timers == handle) {
        timers = handle->next;
        FREE(handle);
        return 1;
    }
    for (current = timers; current->next; current = current->next) {
        if (current->next == handle) {
            break;
        }
    }
    if (!(current->next)) {
        return 0;
    }
    temp = current->next;
    current->next = temp->next;
    FREE(temp);
    return 1;
}

unsigned char
timer_change_delay(timerhdl_t handle, unsigned long msec)
{
    struct timeval tv;

    handle->msec = msec;
    gettimeofday(&tv, NULL);
    handle->time.tv_sec = (msec / 1000) + tv.tv_sec;
    handle->time.tv_usec = ((msec % 1000) * 1000) + tv.tv_usec;
    return 1;
}

void
timer_check(void)
{
    register etimer_t *current;
    struct timeval tv;

    REQUIRE(timers);

    gettimeofday(&tv, NULL);
    for (current = timers; current; current = current->next) {
        if ((current->time.tv_sec > tv.tv_sec) || ((current->time.tv_sec == tv.tv_sec) && (current->time.tv_usec >= tv.tv_usec))) {
            if (!((current->handler) (current->data))) {
                timer_del(current);
            } else {
                timer_change_delay(current, current->msec);
            }
        }
    }
}
