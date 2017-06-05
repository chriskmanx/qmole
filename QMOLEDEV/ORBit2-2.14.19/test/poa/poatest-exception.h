/*
 * CORBA POA tests
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Author: Mark McLoughlin <mark@skynet.ie>
 */

#ifndef __POATEST_EXCEPTION_H__
#define __POATEST_EXCEPTION_H__

#include <stdio.h>
#include <orbit/orbit.h>

/*
 * knackered from bonobo-exception.h
 */
#define POATEST_EX(ev)         ((ev != NULL) && (ev)->_major != CORBA_NO_EXCEPTION)

#define POATEST_PRINT_EX(str, ev)                                             \
	G_STMT_START {                                                        \
		switch ((ev)->_major) {                                       \
		case CORBA_NO_EXCEPTION :                                     \
			fprintf (stderr, str "No exception.\n");              \
			break;                                                \
		case CORBA_USER_EXCEPTION :                                   \
			fprintf (stderr, str "User Exception : %s\n",         \
						CORBA_exception_id ((ev)));   \
			break;                                                \
		case CORBA_SYSTEM_EXCEPTION :                                 \
			fprintf (stderr, str "System Exception : %s\n",       \
						CORBA_exception_id ((ev)));   \
			break;                                                \
		default :                                                     \
			break;                                                \
		}                                                             \
	} G_STMT_END;

#endif /* __POATEST_EXCEPTION_H__ */
