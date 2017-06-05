/* Launcher API - if you are a program that starts other programs */
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


#ifndef __SN_LAUNCHER_H__
#define __SN_LAUNCHER_H__

#include <libsn/sn-common.h>

SN_BEGIN_DECLS

typedef struct SnLauncherContext SnLauncherContext;

SnLauncherContext* sn_launcher_context_new   (SnDisplay           *display,
                                              int                  screen);
void        sn_launcher_context_ref               (SnLauncherContext *context);
void        sn_launcher_context_unref             (SnLauncherContext *context);


void        sn_launcher_context_initiate          (SnLauncherContext *context,
                                                   const char        *launcher_name,
                                                   const char        *launchee_name,
                                                   Time               timestamp);
void        sn_launcher_context_complete          (SnLauncherContext *context);
const char* sn_launcher_context_get_startup_id    (SnLauncherContext *context);
sn_bool_t   sn_launcher_context_get_initiated     (SnLauncherContext *context);

void        sn_launcher_context_setup_child_process (SnLauncherContext *context);

void sn_launcher_context_set_name        (SnLauncherContext *context,
                                          const char        *name);
void sn_launcher_context_set_description (SnLauncherContext *context,
                                          const char        *description);
void sn_launcher_context_set_workspace   (SnLauncherContext *context,
                                          int                workspace);
void sn_launcher_context_set_wmclass     (SnLauncherContext *context,
                                          const char        *klass);
void sn_launcher_context_set_binary_name (SnLauncherContext *context,
                                          const char        *name);
void sn_launcher_context_set_icon_name   (SnLauncherContext *context,
                                          const char        *name);

void sn_launcher_context_set_extra_property (SnLauncherContext *context,
                                             const char        *name,
                                             const char        *value);


void sn_launcher_context_get_initiated_time   (SnLauncherContext *context,
                                               long              *tv_sec,
                                               long              *tv_usec);
void sn_launcher_context_get_last_active_time (SnLauncherContext *context,
                                               long              *tv_sec,
                                               long              *tv_usec);


SN_END_DECLS

#endif /* __SN_LAUNCHER_H__ */
