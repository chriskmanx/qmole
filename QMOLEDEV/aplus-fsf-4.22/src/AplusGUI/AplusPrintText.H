#ifndef AplusPrintTextHEADER
#define AplusPrintTextHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSParagraph.H>
#include <AplusGUI/AplusCommon.H>

// This class should be renamed to AplusParagraph and be derived from MSParagraph.
// MSPrintText class is obsolete in MStk:  it has been replaced by MSParagraph;
// however, for backward compatibility's sake MSPrintText is currently typedef'd
// to MSParagraph, which makes compiling this code possible.

class AplusPrintText : public MSPrintText
{
public:
  AplusPrintText(void);
  ~AplusPrintText(void);

  static MSHashTable *stringEnumHashTable(void) { return &_stringEnumHashTable; }
  static MSHashTable *enumHashTable(void)       { return &_enumHashTable; }

  static ::A convertMode(unsigned long);
  static unsigned long convertMode(::A);

protected:
  void initEnumHashTable(void);
  void initStringEnumHashTable(void);
  
private:
  static MSHashTable _stringEnumHashTable;
  static MSHashTable _enumHashTable;
  static MSBoolean   _initialized;
};

#endif  //AplusPrintTextHEADER
