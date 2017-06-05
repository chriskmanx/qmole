#ifndef __CosNaming_name_support_h__
#define __CosNaming_name_support_h__

#include "CosNaming.h"

CosNaming_Name *
ORBit_string_to_CosNaming_Name (const CORBA_char *string, 
				CORBA_Environment * ev);

CORBA_char *
ORBit_CosNaming_NameComponent_to_string (const CosNaming_NameComponent *comp);

CORBA_char *
ORBit_CosNaming_Name_to_string (const CosNaming_Name *name);

#endif /* __CosNaming_name_support_h__ */
