/**
 *
 * $Id: ManagerI.h,v 1.1 2004/08/28 19:23:30 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright (C) 1997-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/


#ifndef _XMI_MANAGERI_H
#define _XMI_MANAGERI_H

/* 
 *  Interfaces previously (1.2) declared in Xm/ManagerP.h.
    More stuff should be moved ...
 */

void _XmManagerHelp(Widget w,  
                    XEvent *event,
                    String *params,
                    Cardinal *num_params);

#endif /* _XMI_MANAGERI_H */
