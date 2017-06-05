/*
 * Copyright (C) 2000, 2002, 2003, 2004, 2005, 2008, 2010 Free Software
 * Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * The GnuTLS is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA
 *
 */

/* This file contains functions that manipulate a database backend for
 * resumed sessions.
 */

#include "gnutls_int.h"
#include "gnutls_errors.h"
#include <gnutls_db.h>
#include "debug.h"
#include <gnutls_session_pack.h>
#include <gnutls_datum.h>

/**
 * gnutls_db_set_retrieve_function:
 * @session: is a #gnutls_session_t structure.
 * @retr_func: is the function.
 *
 * Sets the function that will be used to retrieve data from the
 * resumed sessions database.  This function must return a
 * gnutls_datum_t containing the data on success, or a gnutls_datum_t
 * containing null and 0 on failure.
 *
 * The datum's data must be allocated using the function
 * gnutls_malloc().
 *
 * The first argument to @retr_func will be null unless
 * gnutls_db_set_ptr() has been called.
 **/
void
gnutls_db_set_retrieve_function (gnutls_session_t session,
                                 gnutls_db_retr_func retr_func)
{
  session->internals.db_retrieve_func = retr_func;
}

/**
 * gnutls_db_set_remove_function:
 * @session: is a #gnutls_session_t structure.
 * @rem_func: is the function.
 *
 * Sets the function that will be used to remove data from the
 * resumed sessions database. This function must return 0 on success.
 *
 * The first argument to @rem_func will be null unless
 * gnutls_db_set_ptr() has been called.
 **/
void
gnutls_db_set_remove_function (gnutls_session_t session,
                               gnutls_db_remove_func rem_func)
{
  session->internals.db_remove_func = rem_func;
}

/**
 * gnutls_db_set_store_function:
 * @session: is a #gnutls_session_t structure.
 * @store_func: is the function
 *
 * Sets the function that will be used to store data from the resumed
 * sessions database. This function must remove 0 on success.
 *
 * The first argument to store_func() will be null unless
 * gnutls_db_set_ptr() has been called.
 **/
void
gnutls_db_set_store_function (gnutls_session_t session,
                              gnutls_db_store_func store_func)
{
  session->internals.db_store_func = store_func;
}

/**
 * gnutls_db_set_ptr:
 * @session: is a #gnutls_session_t structure.
 * @ptr: is the pointer
 *
 * Sets the pointer that will be provided to db store, retrieve and
 * delete functions, as the first argument.
 **/
void
gnutls_db_set_ptr (gnutls_session_t session, void *ptr)
{
  session->internals.db_ptr = ptr;
}

/**
 * gnutls_db_get_ptr:
 * @session: is a #gnutls_session_t structure.
 *
 * Get db function pointer.
 *
 * Returns: the pointer that will be sent to db store, retrieve and
 *   delete functions, as the first argument.
 **/
void *
gnutls_db_get_ptr (gnutls_session_t session)
{
  return session->internals.db_ptr;
}

/**
 * gnutls_db_set_cache_expiration:
 * @session: is a #gnutls_session_t structure.
 * @seconds: is the number of seconds.
 *
 * Set the expiration time for resumed sessions. The default is 3600
 * (one hour) at the time writing this.
 **/
void
gnutls_db_set_cache_expiration (gnutls_session_t session, int seconds)
{
  session->internals.expire_time = seconds;
}

/**
 * gnutls_db_check_entry:
 * @session: is a #gnutls_session_t structure.
 * @session_entry: is the session data (not key)
 *
 * Check if database entry has expired.  This function is to be used
 * when you want to clear unnesessary session which occupy space in
 * your backend.
 *
 * Returns: Returns %GNUTLS_E_EXPIRED, if the database entry has
 *   expired or 0 otherwise.
 **/
int
gnutls_db_check_entry (gnutls_session_t session, gnutls_datum_t session_entry)
{
  time_t timestamp;

  timestamp = gnutls_time (0);

  if (session_entry.data != NULL)
    if (timestamp -
        ((security_parameters_st *) (session_entry.data))->timestamp <=
        session->internals.expire_time
        || ((security_parameters_st *) (session_entry.data))->timestamp >
        timestamp
        || ((security_parameters_st *) (session_entry.data))->timestamp == 0)
      return GNUTLS_E_EXPIRED;

  return 0;
}

/* The format of storing data is:
 * (forget it). Check gnutls_session_pack.c
 */
int
_gnutls_server_register_current_session (gnutls_session_t session)
{
  gnutls_datum_t key;
  gnutls_datum_t content;
  int ret = 0;

  key.data = session->security_parameters.session_id;
  key.size = session->security_parameters.session_id_size;

  if (session->internals.resumable == RESUME_FALSE)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }

  if (session->security_parameters.session_id == NULL
      || session->security_parameters.session_id_size == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }

/* copy data */
  ret = _gnutls_session_pack (session, &content);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  ret = _gnutls_store_session (session, key, content);
  _gnutls_free_datum (&content);

  return ret;
}

/* Checks if both db_store and db_retrieve functions have
 * been set up.
 */
static int
_gnutls_db_func_is_ok (gnutls_session_t session)
{
  if (session->internals.db_store_func != NULL &&
      session->internals.db_retrieve_func != NULL &&
      session->internals.db_remove_func != NULL)
    return 0;
  else
    return GNUTLS_E_DB_ERROR;
}


int
_gnutls_server_restore_session (gnutls_session_t session,
                                uint8_t * session_id, int session_id_size)
{
  gnutls_datum_t data;
  gnutls_datum_t key;
  int ret;

  key.data = session_id;
  key.size = session_id_size;

  if (_gnutls_db_func_is_ok (session) != 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }

  data = _gnutls_retrieve_session (session, key);

  if (data.data == NULL)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }

  /* expiration check is performed inside */
  ret = gnutls_session_set_data (session, data.data, data.size);
  if (ret < 0)
    {
      gnutls_assert ();
      return ret;
    }

  gnutls_free (data.data);

  return 0;
}

int
_gnutls_db_remove_session (gnutls_session_t session, uint8_t * session_id,
                           int session_id_size)
{
  gnutls_datum_t key;

  key.data = session_id;
  key.size = session_id_size;

  return _gnutls_remove_session (session, key);
}


/* Stores session data to the db backend.
 */
int
_gnutls_store_session (gnutls_session_t session,
                       gnutls_datum_t session_id, gnutls_datum_t session_data)
{
  int ret = 0;

  if (session->internals.resumable == RESUME_FALSE)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }

  if (_gnutls_db_func_is_ok (session) != 0)
    {
      return GNUTLS_E_DB_ERROR;
    }

  if (session_id.data == NULL || session_id.size == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }

  if (session_data.data == NULL || session_data.size == 0)
    {
      gnutls_assert ();
      return GNUTLS_E_INVALID_SESSION;
    }
  /* if we can't read why bother writing? */

  if (session->internals.db_store_func != NULL)
    ret =
      session->internals.db_store_func (session->internals.db_ptr,
                                        session_id, session_data);

  return (ret == 0 ? ret : GNUTLS_E_DB_ERROR);

}

/* Retrieves session data from the db backend.
 */
gnutls_datum_t
_gnutls_retrieve_session (gnutls_session_t session, gnutls_datum_t session_id)
{
  gnutls_datum_t ret = { NULL, 0 };

  if (session_id.data == NULL || session_id.size == 0)
    {
      gnutls_assert ();
      return ret;
    }

  if (session->internals.db_retrieve_func != NULL)
    ret =
      session->internals.db_retrieve_func (session->internals.db_ptr,
                                           session_id);

  return ret;

}

/* Removes session data from the db backend.
 */
int
_gnutls_remove_session (gnutls_session_t session, gnutls_datum_t session_id)
{
  int ret = 0;

  if (_gnutls_db_func_is_ok (session) != 0)
    {
      return GNUTLS_E_DB_ERROR;
    }

  if (session_id.data == NULL || session_id.size == 0)
    return GNUTLS_E_INVALID_SESSION;

  /* if we can't read why bother writing? */
  if (session->internals.db_remove_func != NULL)
    ret =
      session->internals.db_remove_func (session->internals.db_ptr,
                                         session_id);

  return (ret == 0 ? ret : GNUTLS_E_DB_ERROR);

}

/**
 * gnutls_db_remove_session:
 * @session: is a #gnutls_session_t structure.
 *
 * This function will remove the current session data from the
 * session database.  This will prevent future handshakes reusing
 * these session data.  This function should be called if a session
 * was terminated abnormally, and before gnutls_deinit() is called.
 *
 * Normally gnutls_deinit() will remove abnormally terminated
 * sessions.
 **/
void
gnutls_db_remove_session (gnutls_session_t session)
{
  _gnutls_db_remove_session (session,
                             session->security_parameters.session_id,
                             session->security_parameters.session_id_size);
}
