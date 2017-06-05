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
#include "ie_exp_OpenDocument.h"

// Internal includes
#include "ODe_AbiDocListener.h"
#include "ODe_AuxiliaryData.h"
#include "ODe_Common.h"
#include "ODe_DocumentData.h"
#include "ODe_HeadingSearcher_Listener.h"
#include "ODe_ManifestWriter.h"
#include "ODe_Main_Listener.h"
#include "ODe_MetaDataWriter.h"
#include "ODe_PicturesWriter.h"
#include "ODe_SettingsWriter.h"

// Abiword includes
#include <ut_assert.h>
#include <ut_locale.h>
#include <pd_Document.h>

// External includes
#include <gsf/gsf-outfile.h>
#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-outfile-zip.h>
#include <gsf/gsf-outfile-stdio.h>


/**
 * Constructor
 */
IE_Exp_OpenDocument::IE_Exp_OpenDocument (PD_Document * pDoc)
  : IE_Exp (pDoc), m_odt(0)
{
}


/**
 * Destructor
 */
IE_Exp_OpenDocument::~IE_Exp_OpenDocument()
{
}

GsfOutput* IE_Exp_OpenDocument::_openFile(const char *szFilename)
{
  GsfOutput *output = NULL;

  const std::string & prop = getProperty ("uncompressed");

  if (!prop.empty() && UT_parseBool (prop.c_str (), false))
    {
      char *filename = UT_go_filename_from_uri (szFilename);
      if (filename) 
	{
	  output = (GsfOutput*)gsf_outfile_stdio_new (filename, NULL);
	  g_free (filename);
	}
    }
  else
    {
      output = IE_Exp::_openFile (szFilename);
    }

  return output;
}

/**
 * This writes out our AbiWord file as an OpenOffice
 * compound document
 */
UT_Error IE_Exp_OpenDocument::_writeDocument(void)
{
	ODe_DocumentData docData;
	ODe_AuxiliaryData auxData;
	ODe_AbiDocListener* pAbiDocListener = NULL;
	ODe_AbiDocListenerImpl* pAbiDocListenerImpl = NULL;
    
	UT_return_val_if_fail (getFp(), UT_ERROR);

	const std::string & prop = getProperty ("uncompressed");
	
	if (!prop.empty() && UT_parseBool (prop.c_str (), false))
	  {
	    m_odt = GSF_OUTFILE(g_object_ref(G_OBJECT(getFp())));
	  }
	else
	  {
	    GError* error = NULL;
	    m_odt = GSF_OUTFILE (gsf_outfile_zip_new (getFp(), &error));
	    
	    if (error)
	      {
		UT_DEBUGMSG(("Error writing odt file: %s\n", error->message));
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	      }
	  }

	UT_return_val_if_fail(m_odt, UT_ERROR);

	// Needed to ensure that all *printf writes numbers correctly,
	// like "45.56mm" instead of "45,56mm".
	UT_LocaleTransactor numericLocale (LC_NUMERIC, "C");
	{
		GsfOutput * mimetype = gsf_outfile_new_child_full (m_odt, "mimetype", FALSE, "compression-level", 0, (void*)0);
		if (!mimetype)
		{
			ODe_gsf_output_close(GSF_OUTPUT(m_odt));
			return UT_ERROR;
		}

		ODe_gsf_output_write(mimetype,
				39 /*39 == strlen("application/vnd.oasis.opendocument.text")*/,
				(const guint8 *)"application/vnd.oasis.opendocument.text");

		ODe_gsf_output_close(mimetype);
    }

	if (!ODe_MetaDataWriter::writeMetaData(getDoc(), m_odt))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}

    if (!ODe_SettingsWriter::writeSettings(getDoc(), m_odt))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
	return UT_ERROR;
	}

	if (!ODe_PicturesWriter::writePictures(getDoc(), m_odt))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}

	if (!ODe_ManifestWriter::writeManifest(getDoc(), m_odt))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}


    // Gather all paragraph style names used by heading paragraphs.

    pAbiDocListenerImpl = new ODe_HeadingSearcher_Listener(auxData);
    pAbiDocListener = new ODe_AbiDocListener(getDoc(),
                                             pAbiDocListenerImpl, false);

	if (!getDoc()->tellListener(static_cast<PL_Listener *>(pAbiDocListener)))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}
    pAbiDocListener->finished();
    
    DELETEP(pAbiDocListener);
    DELETEP(pAbiDocListenerImpl);
    
    // Gather document content and styles

    if (!docData.doPreListeningWork(getDoc())) {
      ODe_gsf_output_close(GSF_OUTPUT(m_odt));
      return UT_ERROR;
    }

    pAbiDocListenerImpl = new ODe_Main_Listener(docData, auxData);
    pAbiDocListener = new ODe_AbiDocListener(getDoc(),
                                             pAbiDocListenerImpl, false);

	if (!getDoc()->tellListener(static_cast<PL_Listener *>(pAbiDocListener)))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}
	pAbiDocListener->finished();
    
	DELETEP(pAbiDocListener);
	DELETEP(pAbiDocListenerImpl);
    
	if (!docData.doPostListeningWork())
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}
    
    // Write content and styles
        
	if (!docData.writeStylesXML(m_odt))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}
	if (!docData.writeContentXML(m_odt))
	{
		ODe_gsf_output_close(GSF_OUTPUT(m_odt));
		return UT_ERROR;
	}

	ODe_gsf_output_close(GSF_OUTPUT(m_odt));
	return UT_OK;
}
