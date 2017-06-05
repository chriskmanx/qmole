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
#include "ODe_MetaDataWriter.h"

// Internal includes
#include "ODe_Common.h"
 
// Abiword includes
#include <ut_types.h>
#include <ut_string_class.h>
#include <pd_Document.h>
 
// External includes
#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-outfile.h>
 
 
/**
 * 
 */
bool ODe_MetaDataWriter::writeMetaData(PD_Document* pDoc, GsfOutfile* oo) {
    
    GsfOutput* meta = gsf_outfile_new_child (oo, "meta.xml", FALSE);

    static const char * const preamble [] = {
        "<?xml version='1.0' encoding='UTF-8'?>\n",
        "<office:document-meta"
            " xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\""
            " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
            " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
            " xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\""
            " xmlns:ooo=\"http://openoffice.org/2004/office\""
            " office:version=\"1.0\">\n",
        "<office:meta>\n",
        "<meta:generator>AbiWord</meta:generator>\n"
    };

    static const char * const postamble [] = {
        "</office:meta>\n",
        "</office:document-meta>\n"
    };

    ODe_writeToStream(meta, preamble, G_N_ELEMENTS(preamble));

    UT_UTF8String meta_val, val;
    
#define WRITE_METADATA_ELEMENT(abiwordKey, odElementName) if (pDoc->getMetaDataProp(abiwordKey, meta_val) && meta_val.size()) { \
                                                               meta_val.escapeXML(); \
                                                               val = UT_UTF8String_sprintf("<%s>%s</%s>\n", odElementName, meta_val.utf8_str(), odElementName); \
                                                               ODe_gsf_output_write(meta, val.size(), reinterpret_cast<const guint8*>(val.utf8_str())); \
                                                          }
    
    WRITE_METADATA_ELEMENT(PD_META_KEY_TITLE, "dc:title");
    WRITE_METADATA_ELEMENT(PD_META_KEY_DESCRIPTION, "dc:description");
    WRITE_METADATA_ELEMENT(PD_META_KEY_SUBJECT, "dc:subject");

    //Each keyword needs to be exported individually:

    UT_UTF8String keywords;
    if (pDoc->getMetaDataProp (PD_META_KEY_KEYWORDS, keywords) && keywords.size())
    {
        UT_UTF8String buf = "";
        UT_UCS4String keyword = keywords.utf8_str();

        for(UT_uint32 i = 0;i < keyword.length(); i++)
        {
            if(keyword[i] != ' ')
            {
                buf += keyword[i];
            }
            else
            {
                if(buf.empty())  //only blank space encountered
                    continue;

                buf.escapeXML();
                val = UT_UTF8String_sprintf("<meta:keyword>%s</meta:keyword>\n", buf.utf8_str());
                ODe_gsf_output_write(meta, val.size(), reinterpret_cast<const guint8*>(val.utf8_str()));
                buf.clear();
            }
        }

        if(buf.length())  //there may only be one keyword (i.e. no spaces encountered)
        {
            buf.escapeXML();
            val = UT_UTF8String_sprintf("<meta:keyword>%s</meta:keyword>\n", buf.utf8_str());
            ODe_gsf_output_write(meta, val.size(), reinterpret_cast<const guint8*>(val.utf8_str()));
        }
    }

    // Should have a PD_META_KEY_INITIAL_CREATOR macro for this one, but only
    // if it gets implemented on the document properties dialog.
    WRITE_METADATA_ELEMENT("meta:initial-creator", "meta:initial-creator");
    
    WRITE_METADATA_ELEMENT(PD_META_KEY_CREATOR, "dc:creator");
    
    WRITE_METADATA_ELEMENT("meta:printed-by", "meta:printed-by");
    
    // ATTENTION: I'm assuming that dc.date is used by AbiWord as
    // the document creation date & time.
    WRITE_METADATA_ELEMENT(PD_META_KEY_DATE, "meta:creation-date");
    
    // Note that, for the OpenDocument standard, dc.date
    // is the last modification date & time.
    WRITE_METADATA_ELEMENT(PD_META_KEY_DATE_LAST_CHANGED, "dc:date");
    
    WRITE_METADATA_ELEMENT("meta:print-date", "meta:print-date");
    
    WRITE_METADATA_ELEMENT(PD_META_KEY_LANGUAGE, "dc:language");
    
#undef WRITE_METADATA_ELEMENT

    ODe_writeToStream(meta, postamble, G_N_ELEMENTS(postamble));

    ODe_gsf_output_close(meta);

    return true;
}
