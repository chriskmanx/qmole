/* Launchee API - if you are a program started by other programs */
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


#ifndef __SN_LAUNCHEE_H__
#define __SN_LAUNCHEE_H__

#include <libsn/sn-common.h>

SN_BEGIN_DECLS

typedef struct SnLauncheeContext SnLauncheeContext;

SnLauncheeContext* sn_launchee_context_new                  (SnDisplay         *display,
                                                             int                screen,
                                                             const char        *startup_id);
SnLauncheeContext* sn_launchee_context_new_from_environment (SnDisplay         *display,
                                                             int                screen);
void               sn_launchee_context_ref                  (SnLauncheeContext *context);
void               sn_launchee_context_unref                (SnLauncheeContext *context);
const char*        sn_launchee_context_get_startup_id       (SnLauncheeContext *context);
int                sn_launchee_context_get_id_has_timestamp (SnLauncheeContext *context);
Time               sn_launchee_context_get_timestamp        (SnLauncheeContext *context);
void               sn_launchee_context_complete             (SnLauncheeContext *context);
void               sn_launchee_context_setup_window         (SnLauncheeContext *context,
                                                             Window             xwindow);

SN_END_DECLS

#endif /* __SN_LAUNCHEE_H__ */
