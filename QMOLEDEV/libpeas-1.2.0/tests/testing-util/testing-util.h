/*
 * testing-util.h
 * This file is part of libpeas
 *
 * Copyright (C) 2011 - Garrett Regier
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef __TESTING_UTIL_H__
#define __TESTING_UTIL_H__

#include <libpeas/peas-engine.h>

G_BEGIN_DECLS

void        testing_util_init          (void);

PeasEngine *testing_util_engine_new    (void);
void        testing_util_engine_free   (PeasEngine *engine);

int         testing_util_run_tests     (void);

void        testing_util_push_log_hook (const gchar *pattern);
void        testing_util_pop_log_hook  (void);
void        testing_util_pop_log_hooks (void);

G_END_DECLS

#endif /* __TESTING_UTIL_H__ */
