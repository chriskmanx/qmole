/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#include <glib.h>

#include "hooks.h"
#include "progressindicator.h"

void progressindicator_start(ProgressType type)
{
	ProgressData data;

	data.cmd = PROGRESS_COMMAND_START;
	data.type = type;

	hooks_invoke(PROGRESSINDICATOR_HOOKLIST, &data);
}

void progressindicator_set_percentage(ProgressType type, gfloat percent)
{
	ProgressData data;

	data.cmd = PROGRESS_COMMAND_SET_PERCENTAGE;
	data.type = type;
	data.value = percent;

	hooks_invoke(PROGRESSINDICATOR_HOOKLIST, &data);
}

void progressindicator_stop(ProgressType type)
{
	ProgressData data;

	data.cmd = PROGRESS_COMMAND_START;
	data.type = type;

	hooks_invoke(PROGRESSINDICATOR_HOOKLIST, &data);
}
