/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2007 Red Hat, Inc.
 */

#ifndef SOUP_AUTH_MANAGER_H
#define SOUP_AUTH_MANAGER_H 1

#include "soup-types.h"
#include "soup-auth.h"

G_BEGIN_DECLS

typedef struct SoupAuthManager SoupAuthManager;

SoupAuthManager *soup_auth_manager_new         (SoupSession     *session);

void             soup_auth_manager_add_type    (SoupAuthManager *manager,
						GType            type);
void             soup_auth_manager_remove_type (SoupAuthManager *manager,
						GType            type);

void             soup_auth_manager_free        (SoupAuthManager *manager);

G_END_DECLS

#endif /* SOUP_AUTH_MANAGER_H */
