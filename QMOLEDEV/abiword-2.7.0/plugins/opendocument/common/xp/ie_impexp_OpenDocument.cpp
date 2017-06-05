/* AbiSource Program Utilities
 * 
 * Copyright (C) 2002 Dom Lachowicz <cinamod@hotmail.com>
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

// External includes
#include <gsf/gsf-utils.h>
#include <xap_Module.h>

// Internal includes
#include "../../imp/xp/ie_imp_OpenDocument_Sniffer.h"
#include "../../exp/xp/ie_exp_OpenDocument_Sniffer.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_opendocument_register
#define abi_plugin_unregister abipgn_opendocument_unregister
#define abi_plugin_supports_version abipgn_opendocument_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("OpenDocument")
#endif

/*****************************************************************************/
/*****************************************************************************/

// completely generic C-interface code to allow this to be a plugin

// we use a reference-counted sniffer
static IE_Imp_OpenDocument_Sniffer* pImp_sniffer = 0;
static IE_Exp_OpenDocument_Sniffer* pExp_sniffer = 0;


/**
 * Register the OpenDocument plugin
 */
ABI_BUILTIN_FAR_CALL int abi_plugin_register (XAP_ModuleInfo * mi)
{
    if (!pImp_sniffer) {
        pImp_sniffer = new IE_Imp_OpenDocument_Sniffer ();
    }
    
    IE_Imp::registerImporter (pImp_sniffer);
    
    if (!pExp_sniffer)
        pExp_sniffer = new IE_Exp_OpenDocument_Sniffer ();
        
    IE_Exp::registerExporter (pExp_sniffer);

    mi->name    = "OpenDocument Filter";
    mi->desc    = "Import/Export OpenDocument (.odt) files";
    mi->version = ABI_VERSION_STRING;
    mi->author  = "Daniel D'Andrada T. de Carvalho, Dom Lachowicz";
    mi->usage   = "No Usage";
  
    return 1;
}



/**
 * Unregister the OpenDocument plugin
 */
ABI_BUILTIN_FAR_CALL int abi_plugin_unregister (XAP_ModuleInfo * mi)
{
  mi->name    = 0;
  mi->desc    = 0;
  mi->version = 0;
  mi->author  = 0;
  mi->usage   = 0;
  
  IE_Imp::unregisterImporter (pImp_sniffer);
  DELETEP(pImp_sniffer);

  IE_Exp::unregisterExporter (pExp_sniffer);
  DELETEP(pExp_sniffer);

  return 1;
}




/**
 * 
 */
ABI_BUILTIN_FAR_CALL int abi_plugin_supports_version (UT_uint32 /*major*/, 
													  UT_uint32 /*minor*/, 
													  UT_uint32 /*release*/)
{
  return 1;
}
