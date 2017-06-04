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

#ifndef __SN_XMESSAGES_H__
#define __SN_XMESSAGES_H__

#include <libsn/sn-common.h>

SN_BEGIN_DECLS

typedef void (* SnXmessageFunc) (SnDisplay       *display,
                                 const char      *message_type,
                                 const char      *message,
                                 void            *user_data);

void sn_internal_add_xmessage_func    (SnDisplay      *display,
                                       int             screen,
                                       const char     *message_type,
                                       const char     *message_type_begin,
                                       SnXmessageFunc  func,
                                       void           *func_data,
                                       SnFreeFunc      free_data_func);
void sn_internal_remove_xmessage_func (SnDisplay      *display,
                                       int             screen,
                                       const char     *message_type,
                                       SnXmessageFunc  func,
                                       void           *func_data);
void sn_internal_broadcast_xmessage   (SnDisplay      *display,
                                       int             screen,
                                       const char     *message_type,
                                       const char     *message_type_begin,
                                       const char     *message);

char*     sn_internal_serialize_message   (const char   *prefix,
                                           const char  **property_names,
                                           const char  **property_values);
sn_bool_t sn_internal_unserialize_message (const char   *message,
                                           char        **prefix,
                                           char       ***property_names,
                                           char       ***property_values);

SN_END_DECLS

#endif /* __SN_XMESSAGES_H__ */
