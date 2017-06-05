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
 
 
// Class definition include
#include "ODe_PicturesWriter.h"

// Internal includes
#include "ODe_Common.h"

// Abiword includes
#include <ut_bytebuf.h>
#include <pd_Document.h>


/**
 * Writes all pictures inside the Pictures subdirectory.
 */
bool ODe_PicturesWriter::writePictures(PD_Document* pDoc, GsfOutfile* pODT) {
    const char * szName;
    const char * szMimeType;
	const char ** pszMimeType = &szMimeType;
    const UT_ByteBuf * pByteBuf;
    GsfOutput* pImg;
    UT_UTF8String name;
    GsfOutput* pPicsDir = NULL;
    
    for (UT_uint32 k=0;
         (pDoc->enumDataItems(k,
                              NULL,
                              &szName,
                              &pByteBuf,
                              reinterpret_cast<const void **>(pszMimeType)));
         k++) {
            
        if (szMimeType && !strcmp(szMimeType, "image/png")) {
            
            if (pPicsDir == NULL) {
                // create Pictures directory
                pPicsDir = gsf_outfile_new_child(pODT, "Pictures", TRUE);
            }
        
            // create individual pictures
            UT_UTF8String_sprintf(name, "%s.png", szName);
            
            pImg = gsf_outfile_new_child(GSF_OUTFILE(pPicsDir),
                                         name.utf8_str(), FALSE);    
                                                    
            ODe_gsf_output_write(pImg, pByteBuf->getLength(),
                                pByteBuf->getPointer(0));
    
            ODe_gsf_output_close(pImg);
        }
    }

    if (pPicsDir != NULL) {
        ODe_gsf_output_close(pPicsDir);
    }

    return true;
}
