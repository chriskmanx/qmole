/* AbiSource
 * 
 * Copyright (C) 2002 Dom Lachowicz <cinamod@hotmail.com>
 * Copyright (C) 2004 Robert Staudinger <robsta@stereolyzer.net>
 * Copyright (C) 2005 Daniel d'Andrada T. de Carvalho
 * <daniel.carvalho@indt.org.br>
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
#include "ie_exp_OpenDocument_Sniffer.h"


// Internal includes
#include "ie_exp_OpenDocument.h"



/**
 * Constructor
 */
IE_Exp_OpenDocument_Sniffer::IE_Exp_OpenDocument_Sniffer()
  : IE_ExpSniffer ("OpenDocument::ODT")
{
}


/**
 * Destructor
 */
IE_Exp_OpenDocument_Sniffer::~IE_Exp_OpenDocument_Sniffer()
{
}



/**
 * Recognize this suffix
 */
bool IE_Exp_OpenDocument_Sniffer::recognizeSuffix(const char * szSuffix)
{
  return (!g_ascii_strcasecmp(szSuffix,".odt"));
}


/**
 * Recognize this MIMEType
 */
UT_Confidence_t IE_Exp_OpenDocument_Sniffer::supportsMIME(const char * szMIME)
{
  if (g_ascii_strcasecmp(szMIME, "application/vnd.oasis.opendocument.text") == 0)
  {
      return UT_CONFIDENCE_PERFECT;
  }
  return UT_CONFIDENCE_ZILCH;
}


/**
 * Construct an exporter for us
 */
UT_Error IE_Exp_OpenDocument_Sniffer::constructExporter(PD_Document * pDocument,
                              IE_Exp ** ppie)
{
  *ppie = new IE_Exp_OpenDocument (pDocument);
  return UT_OK;
}



/**
 * Get the dialog labels
 */
bool IE_Exp_OpenDocument_Sniffer::getDlgLabels(const char ** pszDesc,
                         const char ** pszSuffixList,
                         IEFileType * ft)
{
  *pszDesc = "OpenDocument (.odt)";
  *pszSuffixList = "*.odt";
  *ft = getFileType();
  return true;
}
