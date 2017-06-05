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
#include "ODe_SettingsWriter.h"
 
// Internal includes
#include "ODe_Common.h"

// Abiword includes
#include <ut_types.h>
 
 
bool ODe_SettingsWriter::writeSettings(PD_Document* /*pDoc*/, GsfOutfile* oo)
{
    GsfOutput* pSettings = gsf_outfile_new_child (oo, "settings.xml", FALSE);

    static const char * const contents [] = 
    {
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<office:document-settings"
            " xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\""
            " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
            " xmlns:config=\"urn:oasis:names:tc:opendocument:xmlns:config:1.0\""
            " xmlns:ooo=\"http://openoffice.org/2004/office\" office:version=\"1.0\">\n",
        "</office:document-settings>"
    };

    ODe_writeToStream (pSettings, contents, G_N_ELEMENTS(contents));

    ODe_gsf_output_close(pSettings);

    return true;
}
