#ifndef __EnumTables_H
#define __EnumTables_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSHashTable.H>
#include <MSGUI/MSGUIEnum.H>
#include <a/fncdcls.h>

class EnumTables
{
public:
  EnumTables(void);
  ~EnumTables(void);

  static MSHashTable *stringEnumHashTable(void)        	  { return _stringEnumHashTable;        }
  static MSHashTable *printEnumHashTable(void)         	  { return _printEnumHashTable;         }
  static MSHashTable *pageSizeEnumHashTable(void)      	  { return _pageSizeEnumHashTable;      }
  static MSHashTable *pageSizeStringHashTable(void)    	  { return _pageSizeStringHashTable;    }
  static MSHashTable *alignEnumHashTable(void)         	  { return _alignEnumHashTable;         }
  static MSHashTable *alignStringHashTable(void)       	  { return _alignStringHashTable;       }
  static MSHashTable *styleEnumHashTable(void)         	  { return _styleEnumHashTable;         }
  static MSHashTable *styleStringHashTable(void)       	  { return _styleStringHashTable;       }
  static MSHashTable *reportStyleEnumHashTable(void)   	  { return _reportStyleEnumHashTable;   }
  static MSHashTable *reportStyleStringHashTable(void) 	  { return _reportStyleStringHashTable; }
  static MSHashTable *reportCompModeEnumHashTable(void)   { return _reportCompModeEnumHashTable;   }
  static MSHashTable *reportCompModeStringHashTable(void) { return _reportCompModeStringHashTable; }
  static MSHashTable *cycleColorModeEnumHashTable(void)   { return _cycleColorModeEnumHashTable;   }
  static MSHashTable *cycleColorModeStringHashTable(void) { return _cycleColorModeStringHashTable; }

  A enumSymbols(const char *str_);
  
  // Format Conversion
  unsigned long alignFormat(A);
  A alignFormat(unsigned long);

  // Style Conversion
  unsigned long formatStyle(A);
  A formatStyle(unsigned long);

  // Computation Mode Conversion
  unsigned long formatCompMode(A);
  A formatCompMode(unsigned long);

  // Color Cycle Mode Conversion
  MSCycleColorMode cycleColorMode(A);
  A cycleColorMode(MSCycleColorMode);

private:
  void initPrintEnumHashTable(void);
  void initStringEnumHashTable(void);

  void initAlignHashTables(void);
  void initStyleHashTables(void);
  void initPageSizeHashTables(void);
  void initReportStyleHashTables(void);
  void initReportCompModeHashTables(void);
  void initCycleColorModeHashTables(void);
  
  static MSHashTable *_stringEnumHashTable;
  static MSHashTable *_printEnumHashTable;

  static MSHashTable *_alignEnumHashTable;
  static MSHashTable *_alignStringHashTable;
  static MSHashTable *_pageSizeEnumHashTable;
  static MSHashTable *_pageSizeStringHashTable;
  static MSHashTable *_styleEnumHashTable;
  static MSHashTable *_styleStringHashTable;
  static MSHashTable *_reportStyleEnumHashTable;
  static MSHashTable *_reportStyleStringHashTable;
  static MSHashTable *_reportCompModeEnumHashTable;
  static MSHashTable *_reportCompModeStringHashTable;
  static MSHashTable *_cycleColorModeEnumHashTable;
  static MSHashTable *_cycleColorModeStringHashTable;
};

extern EnumTables GUIEnum;


#endif
