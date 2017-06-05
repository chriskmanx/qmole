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
#include "ODe_ManifestWriter.h"
 
// Internal includes
#include "ODe_Common.h"

// Abiword includes
#include <ut_string_class.h>
#include <pd_Document.h>
 
// External includes
#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-outfile.h>

 
/**
 * 
 */
bool ODe_ManifestWriter::writeManifest(PD_Document* pDoc, GsfOutfile* pODT)
{
    // Create META-INF directory
    GsfOutput* meta_inf = gsf_outfile_new_child(pODT, "META-INF", TRUE);
    
    GsfOutput* manifest = gsf_outfile_new_child(
                            GSF_OUTFILE(meta_inf), "manifest.xml", FALSE);

    UT_String name;

    static const char * const preamble [] = {
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<!DOCTYPE manifest:manifest PUBLIC \"-//OpenOffice.org//DTD Manifest 1.0//EN\" \"Manifest.dtd\">\n",
        "<manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\">\n",
        " <manifest:file-entry manifest:media-type=\"application/vnd.oasis.opendocument.text\" manifest:full-path=\"/\"/>\n",
        " <manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"content.xml\"/>\n",
        " <manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"styles.xml\"/>\n",
        " <manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"meta.xml\"/>\n",
        " <manifest:file-entry manifest:media-type=\"text/xml\" manifest:full-path=\"settings.xml\"/>\n"
    };

    static const char * const postamble [] = {
        "</manifest:manifest>\n"
    };

    ODe_writeToStream (manifest, preamble, G_N_ELEMENTS(preamble));

    const char* szName;
    const char* szMimeType;
	const char **pszMimeType = &szMimeType;
    const UT_ByteBuf* pByteBuf;
    bool wroteDirManifest = false;
    
    for (UT_uint32 k = 0;
         (pDoc->enumDataItems(k,
                              NULL,
                              &szName,
                              &pByteBuf,
                              reinterpret_cast<const void **>(pszMimeType))); k++) {
                                
        if (szMimeType && !strcmp(szMimeType, "image/png")) {
            
            if (!wroteDirManifest) {
                name = " <manifest:file-entry manifest:media-type=\"\" manifest:full-path=\"Pictures/\"/>\n";
                ODe_gsf_output_write(manifest, name.size(),
                    reinterpret_cast<const guint8 *>(name.c_str()));
                    
                wroteDirManifest = true;
            }

            name = UT_String_sprintf(
                " <manifest:file-entry manifest:media-type=\"%s\" manifest:full-path=\"Pictures/%s.png\"/>\n",
                szMimeType, szName);
                
            ODe_gsf_output_write (manifest, name.size(),
                reinterpret_cast<const guint8 *>(name.c_str()));
        }
    }

    ODe_writeToStream (manifest, postamble, G_N_ELEMENTS(postamble));

    ODe_gsf_output_close(manifest);
    ODe_gsf_output_close(meta_inf);

    return true;
}
