/*--------------------------------*-H-*---------------------------------*
 * File:	grkelot.h
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1994        Angelo Haritsis
 * Copyright (c) 1997,1998   Oezguer Kesim <kesim@math.fu-berlin.de>
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: grkelot.h,v 1.4 2004/09/24 02:52:37 cvs Exp $
*/

#ifndef __GRKELOT_H__
#define __GRKELOT_H__

#define GREEK_ELOT928	0
#define GREEK_IBM437	1

void	greek_init            (void);
void	greek_end             (void);
void	greek_reset           (void);
void    greek_setmode         (int greek_mode);
int     greek_getmode         (void);
int     greek_xlat            (char *s, int num_chars);


#endif	/* __GRKELOT_H__ */
/*----------------------- end-of-file (H source) -----------------------*/
