/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005-2008, Øyvind Kolås and others.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include "babl-internal.h"


void     
babl_get_version (int *major,   
                  int *minor,   
                  int *micro)   
{       
  if (major != NULL)    
    *major = BABL_MAJOR_VERSION;        
                 
  if (minor != NULL)    
    *minor = BABL_MINOR_VERSION;        
                 
  if (micro != NULL)    
    *micro = BABL_MICRO_VERSION;        
}
