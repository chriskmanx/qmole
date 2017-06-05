/* $Id: e2p_upgrade.h 2819 2013-10-23 07:20:16Z tpgww $

Copyright (C) 2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation - either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/e2p_upgrade.h
@brief header for config-file updater plugin
*/

#ifndef __E2P_UPGRADE_H__
#define __E2P_UPGRADE_H__

#include "emelfm2.h"
#include "e2_plugins.h"

#define OLDEST_UPGRADE "0.4.1.3"

//interface type, a pointer to which is castable to/from Plugin[Iface]*
typedef struct _UpgradeIface
{
	PluginIface pdata;
	gboolean (*update_config) (void);
} UpgradeIface;

#define UPGRADE_IFACE(d) (UpgradeIface*)(d)

#endif //ndef __E2P_UPGRADE_H__
