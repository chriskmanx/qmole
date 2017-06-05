/* AbiSource
 * 
 * Copyright (C) 2002 Dom Lachowicz <cinamod@hotmail.com>
 * Copyright (C) 2004 Robert Staudinger <robsta@stereolyzer.net>
 * 
 * Copyright (C) 2005 INdT
 * Author: Daniel d'Andrada T. de Carvalho <daniel.carvalho@indt.org.br>
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  
 * 02111-1307, USA.
 */

#ifndef _ODE_COMMON_H_
#define _ODE_COMMON_H_


// AbiWord includes
#include <ut_types.h>

// External includes
#include <gsf/gsf-output.h>
#include <stdio.h>

// Abiword classes
class UT_UTF8String;

void ODe_gsf_output_write(GsfOutput* output, size_t num_bytes,
    guint8 const *data);

void ODe_gsf_output_close(GsfOutput* output);

void ODe_writeToStream (GsfOutput* stream, const char* const message [],
    size_t nElements);

void ODe_writeUTF8String (GsfOutput* output, const UT_UTF8String& str);

void ODe_writeAttribute(UT_UTF8String& rOutput,
                        const gchar* pName,
                        const UT_UTF8String& rValue);
                        
void ODe_writeAttribute(UT_UTF8String& rOutput,
                        const gchar* pName,
                        const gchar* pValue);
                        
void ODe_writeToFile(GsfOutput* pFile, const UT_UTF8String& rString);

// The source file is rewinded before writing its contents into the destination
// and after that it's left on its EOF state.
void ODe_writeToFile(GsfOutput* pDestinationFile, GsfInput* pSourceFile);

#endif //_ODE_COMMON_H_
