/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  bonobo-activation: A library for accessing bonobo-activation-server.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Author: Elliot Lee <sopwith@redhat.com>
 */
#ifndef BONOBO_ACTIVATION_H
#define BONOBO_ACTIVATION_H

#include <bonobo-activation/Bonobo_Unknown.h>
#include <bonobo-activation/Bonobo_GenericFactory.h>

#include <bonobo-activation/bonobo-activation-version.h>

#include <bonobo-activation/bonobo-activation-activate.h>
#include <bonobo-activation/bonobo-activation-server-info.h>
#include <bonobo-activation/bonobo-activation-init.h>
#include <bonobo-activation/bonobo-activation-base-service.h>
#include <bonobo-activation/bonobo-activation-id.h>
#include <bonobo-activation/bonobo-activation-shlib.h>
#include <bonobo-activation/bonobo-activation-register.h>

#include <bonobo-activation/bonobo-activation-async.h>

extern const guint bonobo_activation_major_version,
	bonobo_activation_minor_version, bonobo_activation_micro_version;
extern const char bonobo_activation_version[];

#endif /* BONOBO_ACTIVATION_H */
