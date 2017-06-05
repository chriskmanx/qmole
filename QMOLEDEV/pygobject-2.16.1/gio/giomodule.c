/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygobject - Python bindings for GObject
 * Copyright (C) 2008  Johan Dahlin
 *
 *   giomodule.c: module wrapping the GIO library
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <Python.h>
#include <pyglib.h>
#include <pygobject.h>

#include <gio/gio.h>

#define PYGIO_MAJOR_VERSION PYGOBJECT_MAJOR_VERSION
#define PYGIO_MINOR_VERSION PYGOBJECT_MINOR_VERSION
#define PYGIO_MICRO_VERSION PYGOBJECT_MICRO_VERSION

/* include any extra headers needed here */

void pygio_register_classes(PyObject *d);
void pygio_add_constants(PyObject *module, const gchar *strip_prefix);

extern PyMethodDef pygio_functions[];

DL_EXPORT(void)
init_gio(void)
{
    PyObject *m, *d;
    PyObject *tuple;
    PyObject *e;
    /* perform any initialisation required by the library here */

    m = Py_InitModule("gio._gio", pygio_functions);
    d = PyModule_GetDict(m);

    init_pygobject_check(2, 15, 2);

    pygio_register_classes(d);
    pygio_add_constants(m, "G_IO_");

    PyModule_AddStringConstant(m, "ERROR", g_quark_to_string(G_IO_ERROR));
    e = pyglib_register_exception_for_domain("gio.Error", G_IO_ERROR);
    PyDict_SetItemString(d, "Error", e);
    Py_DECREF(e);

    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_TYPE",
			       G_FILE_ATTRIBUTE_STANDARD_TYPE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_IS_HIDDEN",
			       G_FILE_ATTRIBUTE_STANDARD_IS_HIDDEN);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_IS_BACKUP",
			       G_FILE_ATTRIBUTE_STANDARD_IS_BACKUP);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_IS_SYMLINK",
			       G_FILE_ATTRIBUTE_STANDARD_IS_SYMLINK);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_IS_VIRTUAL",
			       G_FILE_ATTRIBUTE_STANDARD_IS_VIRTUAL);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_NAME",
			       G_FILE_ATTRIBUTE_STANDARD_NAME);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME",
			       G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_EDIT_NAME",
			       G_FILE_ATTRIBUTE_STANDARD_EDIT_NAME);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_COPY_NAME",
			       G_FILE_ATTRIBUTE_STANDARD_COPY_NAME);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_DESCRIPTION",
			       G_FILE_ATTRIBUTE_STANDARD_DESCRIPTION);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_ICON",
			       G_FILE_ATTRIBUTE_STANDARD_ICON);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE",
			       G_FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_FAST_CONTENT_TYPE",
			       G_FILE_ATTRIBUTE_STANDARD_FAST_CONTENT_TYPE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_SIZE",
			       G_FILE_ATTRIBUTE_STANDARD_SIZE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_SYMLINK_TARGET",
			       G_FILE_ATTRIBUTE_STANDARD_SYMLINK_TARGET);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_TARGET_URI",
			       G_FILE_ATTRIBUTE_STANDARD_TARGET_URI);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_STANDARD_SORT_ORDER",
			       G_FILE_ATTRIBUTE_STANDARD_SORT_ORDER);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ETAG_VALUE",
			       G_FILE_ATTRIBUTE_ETAG_VALUE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ID_FILE",
			       G_FILE_ATTRIBUTE_ID_FILE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ID_FILESYSTEM",
			       G_FILE_ATTRIBUTE_ID_FILESYSTEM);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ACCESS_CAN_READ",
			       G_FILE_ATTRIBUTE_ACCESS_CAN_READ);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ACCESS_CAN_WRITE",
			       G_FILE_ATTRIBUTE_ACCESS_CAN_WRITE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ACCESS_CAN_EXECUTE",
			       G_FILE_ATTRIBUTE_ACCESS_CAN_EXECUTE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ACCESS_CAN_DELETE",
			       G_FILE_ATTRIBUTE_ACCESS_CAN_DELETE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ACCESS_CAN_TRASH",
			       G_FILE_ATTRIBUTE_ACCESS_CAN_TRASH);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_ACCESS_CAN_RENAME",
			       G_FILE_ATTRIBUTE_ACCESS_CAN_RENAME);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_MOUNTABLE_CAN_MOUNT",
			       G_FILE_ATTRIBUTE_MOUNTABLE_CAN_MOUNT);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_MOUNTABLE_CAN_UNMOUNT",
			       G_FILE_ATTRIBUTE_MOUNTABLE_CAN_UNMOUNT);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_MOUNTABLE_CAN_EJECT",
			       G_FILE_ATTRIBUTE_MOUNTABLE_CAN_EJECT);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_MOUNTABLE_UNIX_DEVICE",
			       G_FILE_ATTRIBUTE_MOUNTABLE_UNIX_DEVICE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_MOUNTABLE_HAL_UDI",
			       G_FILE_ATTRIBUTE_MOUNTABLE_HAL_UDI);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_MODIFIED",
			       G_FILE_ATTRIBUTE_TIME_MODIFIED);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_MODIFIED_USEC",
			       G_FILE_ATTRIBUTE_TIME_MODIFIED_USEC);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_ACCESS",
			       G_FILE_ATTRIBUTE_TIME_ACCESS);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_ACCESS_USEC",
			       G_FILE_ATTRIBUTE_TIME_ACCESS_USEC);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_CHANGED",
			       G_FILE_ATTRIBUTE_TIME_CHANGED);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_CHANGED_USEC",
			       G_FILE_ATTRIBUTE_TIME_CHANGED_USEC);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_CREATED",
			       G_FILE_ATTRIBUTE_TIME_CREATED);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TIME_CREATED_USEC",
			       G_FILE_ATTRIBUTE_TIME_CREATED_USEC);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_DEVICE",
			       G_FILE_ATTRIBUTE_UNIX_DEVICE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_INODE",
			       G_FILE_ATTRIBUTE_UNIX_INODE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_MODE",
			       G_FILE_ATTRIBUTE_UNIX_MODE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_NLINK",
			       G_FILE_ATTRIBUTE_UNIX_NLINK);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_UID",
			       G_FILE_ATTRIBUTE_UNIX_UID);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_GID",
			       G_FILE_ATTRIBUTE_UNIX_GID);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_RDEV",
			       G_FILE_ATTRIBUTE_UNIX_RDEV);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_BLOCK_SIZE",
			       G_FILE_ATTRIBUTE_UNIX_BLOCK_SIZE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_BLOCKS",
			       G_FILE_ATTRIBUTE_UNIX_BLOCKS);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_UNIX_IS_MOUNTPOINT",
			       G_FILE_ATTRIBUTE_UNIX_IS_MOUNTPOINT);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_DOS_IS_ARCHIVE",
			       G_FILE_ATTRIBUTE_DOS_IS_ARCHIVE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_DOS_IS_SYSTEM",
			       G_FILE_ATTRIBUTE_DOS_IS_SYSTEM);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_OWNER_USER",
			       G_FILE_ATTRIBUTE_OWNER_USER);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_OWNER_USER_REAL",
			       G_FILE_ATTRIBUTE_OWNER_USER_REAL);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_OWNER_GROUP",
			       G_FILE_ATTRIBUTE_OWNER_GROUP);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_THUMBNAIL_PATH",
			       G_FILE_ATTRIBUTE_THUMBNAIL_PATH);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_THUMBNAILING_FAILED",
			       G_FILE_ATTRIBUTE_THUMBNAILING_FAILED);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_FILESYSTEM_SIZE",
			       G_FILE_ATTRIBUTE_FILESYSTEM_SIZE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_FILESYSTEM_FREE",
			       G_FILE_ATTRIBUTE_FILESYSTEM_FREE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_FILESYSTEM_TYPE",
			       G_FILE_ATTRIBUTE_FILESYSTEM_TYPE);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_FILESYSTEM_READONLY",
			       G_FILE_ATTRIBUTE_FILESYSTEM_READONLY);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_FILESYSTEM_USE_PREVIEW",
			       G_FILE_ATTRIBUTE_FILESYSTEM_USE_PREVIEW);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_GVFS_BACKEND",
			       G_FILE_ATTRIBUTE_GVFS_BACKEND);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_SELINUX_CONTEXT",
			       G_FILE_ATTRIBUTE_SELINUX_CONTEXT);
    PyModule_AddStringConstant(m, "FILE_ATTRIBUTE_TRASH_ITEM_COUNT",
			       G_FILE_ATTRIBUTE_TRASH_ITEM_COUNT);
    
    PyModule_AddStringConstant(m, "ERROR", g_quark_to_string(G_IO_ERROR));

    /* pygio version */
    tuple = Py_BuildValue ("(iii)",
			   PYGIO_MAJOR_VERSION,
			   PYGIO_MINOR_VERSION,
			   PYGIO_MICRO_VERSION);
    PyDict_SetItemString(d, "pygio_version", tuple); 
    Py_DECREF(tuple);
}

