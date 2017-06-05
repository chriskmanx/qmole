/********************************************************************************
*                                                                               *
*                          E x c e p t i o n  T y p e s                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2000,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXException.cpp,v 1.13 2006/01/22 17:58:25 fox Exp $                     *
********************************************************************************/
#include "fxver.h"
#include "fxdefs.h"
#include "FXException.h"

using namespace FX;

namespace FX {

// Generic unspecified exception
const FXchar FXException::exceptionName[]="unknown";


// Error occured
const FXchar FXErrorException::exceptionName[]="error";


// Index out of range
const FXchar FXRangeException::exceptionName[]="out of range";


// Invalid pointer argument
const FXchar FXPointerException::exceptionName[]="invalid pointer";


// Some resource exhausted
const FXchar FXResourceException::exceptionName[]="resource exhausted";


// Out of memory
const FXchar FXMemoryException::exceptionName[]="out of memory";


// Window exception
const FXchar FXWindowException::exceptionName[]="window exception";


// Image, cursor, bitmap exception
const FXchar FXImageException::exceptionName[]="image exception";


// Font exception
const FXchar FXFontException::exceptionName[]="font exception";


}
