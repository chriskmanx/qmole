/*
 * Copyright 2008-2014 Chris Young <chris@unsatisfactorysoftware.co.uk>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef AMIGA_SCHEDULE_H
#define AMIGA_SCHEDULE_H
#include "amiga/os3support.h"

/**
 * Schedule a callback.
 *
 * \param  t         interval before the callback should be made / ms
 * \param  callback  callback function
 * \param  p         user parameter, passed to callback function
 * \return NSERROR_OK on sucess or appropriate error on faliure
 *
 * The callback function will be called as soon as possible after t ms have
 * passed.
 */
nserror ami_schedule(int t, void (*callback)(void *p), void *p);

/**
 * Handle a message received from the scheduler process.
 *
 * \param nsmsgport Message port to process.
 */
void ami_schedule_handle(struct MsgPort *nsmsgport);

/**
 * Create a new process for the scheduler.
 *
 * \param nsmsgport Message port for the scheduler to send events to.
 * \return NSERROR_OK on success or error code on failure.
 */
nserror ami_scheduler_process_create(struct MsgPort *nsmsgport);

/**
 * Signal the scheduler process to exit.
 */
void ami_scheduler_process_delete(void);
#endif

