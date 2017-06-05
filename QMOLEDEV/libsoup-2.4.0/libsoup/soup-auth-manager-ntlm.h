/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * Copyright (C) 2008 Red Hat, Inc.
 */

#ifndef SOUP_AUTH_MANAGER_NTLM_H
#define SOUP_AUTH_MANAGER_NTLM_H 1

#include "soup-types.h"

G_BEGIN_DECLS

typedef struct SoupAuthManagerNTLM SoupAuthManagerNTLM;

SoupAuthManagerNTLM *soup_auth_manager_ntlm_new  (SoupSession         *session);
void                 soup_auth_manager_ntlm_free (SoupAuthManagerNTLM *manager);

G_END_DECLS

#endif /* SOUP_AUTH_MANAGER_NTLM_H */
