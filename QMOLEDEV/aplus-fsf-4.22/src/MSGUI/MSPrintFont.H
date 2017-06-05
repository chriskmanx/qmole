#ifndef MSPrintFontHEADER
#define MSPrintFontHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSHashTable.H>

class MSGUIExport MSPrintFont
{
  static MSHashTable       *_fontHashTable;
  static MSStringHashTable *_afmfilesHashTable;
  static const char        *_defaultFontPath;
  static const char        *_altDefaultFontPath;
  
public:
  MSPrintFont(void);
  ~MSPrintFont(void);

  MSHashTable *fontHashTable(void);
  MSStringHashTable *afmfilesHashTable(void);
  void initFontHashTable(void);
  MSBoolean initAfmfilesHashTable(const char *path_=0);

  static const char *defaultFontPath(void);
  static const char *altDefaultFontPath(void);
};

#endif


