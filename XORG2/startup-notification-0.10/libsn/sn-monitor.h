/* Monitor API - if you are a program that monitors launch sequences */
/* 
 * Copyright (C) 2002 Red Hat, Inc.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */


#ifndef __SN_MONITOR_H__
#define __SN_MONITOR_H__

#include <libsn/sn-common.h>

SN_BEGIN_DECLS

typedef struct SnMonitorContext SnMonitorContext;
typedef struct SnMonitorEvent   SnMonitorEvent;
typedef struct SnStartupSequence SnStartupSequence;

typedef void (* SnMonitorEventFunc) (SnMonitorEvent *event,
                                     void           *user_data);

typedef enum
{
  SN_MONITOR_EVENT_INITIATED,
  SN_MONITOR_EVENT_COMPLETED,
  SN_MONITOR_EVENT_CHANGED,
  SN_MONITOR_EVENT_CANCELED /* not used for now */
} SnMonitorEventType;

SnMonitorContext*  sn_monitor_context_new                  (SnDisplay           *display,
                                                            int                  screen,
                                                            SnMonitorEventFunc   event_func,
                                                            void                *event_func_data,
                                                            SnFreeFunc           free_data_func);
void               sn_monitor_context_ref                  (SnMonitorContext *context);
void               sn_monitor_context_unref                (SnMonitorContext *context);

void               sn_monitor_event_ref                  (SnMonitorEvent *event);
void               sn_monitor_event_unref                (SnMonitorEvent *event);
SnMonitorEvent*    sn_monitor_event_copy                 (SnMonitorEvent *event);
SnMonitorEventType sn_monitor_event_get_type             (SnMonitorEvent *event);
SnStartupSequence* sn_monitor_event_get_startup_sequence (SnMonitorEvent *event);
SnMonitorContext*  sn_monitor_event_get_context          (SnMonitorEvent *event);


void        sn_startup_sequence_ref                       (SnStartupSequence *sequence);
void        sn_startup_sequence_unref                     (SnStartupSequence *sequence);
const char* sn_startup_sequence_get_id                    (SnStartupSequence *sequence);
sn_bool_t   sn_startup_sequence_get_completed             (SnStartupSequence *sequence);
const char* sn_startup_sequence_get_name                  (SnStartupSequence *sequence);
const char* sn_startup_sequence_get_description           (SnStartupSequence *sequence);
int         sn_startup_sequence_get_workspace             (SnStartupSequence *sequence);
Time        sn_startup_sequence_get_timestamp             (SnStartupSequence *sequence);
const char* sn_startup_sequence_get_wmclass               (SnStartupSequence *sequence);
const char* sn_startup_sequence_get_binary_name           (SnStartupSequence *sequence);
const char* sn_startup_sequence_get_icon_name             (SnStartupSequence *sequence);
int         sn_startup_sequence_get_screen                (SnStartupSequence *sequence);

void        sn_startup_sequence_get_initiated_time        (SnStartupSequence *sequence,
                                                           long              *tv_sec,
                                                           long              *tv_usec);
void        sn_startup_sequence_get_last_active_time      (SnStartupSequence *sequence,
                                                           long              *tv_sec,
                                                           long              *tv_usec);

void        sn_startup_sequence_complete                  (SnStartupSequence *sequence);

SN_END_DECLS

#endif /* __SN_MONITOR_H__ */
