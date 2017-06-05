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
#ifndef BONOBO_ACTIVATION_ACTIVATE_PRIVATE_H
#define BONOBO_ACTIVATION_ACTIVATE_PRIVATE_H

#include <bonobo-activation/Bonobo_GenericFactory.h>

char *bonobo_activation_maybe_add_test_requirements            (const char *requirements);
void  bonobo_activation_copy_string_array_to_Bonobo_StringList (char *const *selection_order, 
                                                                Bonobo_StringList *ret_val);


#endif /* BONOBO_ACTIVATION_ACTIVATE_PRIVATE_H */
