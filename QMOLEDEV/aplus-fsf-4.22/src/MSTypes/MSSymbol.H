#ifndef MSSymbolHEADER
#define MSSymbolHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

class MSNameSpace;
class MSString;
#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class istream;
class ostream;
#endif


class MSTypesExport MSSymbol
{
public:
  MSSymbol(void);
  MSSymbol(const char *symbolName_);
  MSSymbol(const MSSymbol& aSymbol_);
  ~MSSymbol(void);

  const char *symbolName(void) const;
  MSString asString(void) const;
  MSString asDebugInfo(void) const;
  MSString className(void) const;
  INLINELINKAGE MSAtom atom(void) const;

  MSSymbol& operator=(const MSSymbol& aSymbol_);
  MSBoolean operator==(const MSSymbol& aSymbol_) const;
  MSBoolean operator!=(const MSSymbol& aSymbol_) const;
  MSBoolean operator<(const MSSymbol& aSymbol_) const;
  MSBoolean operator>(const MSSymbol& aSymbol_) const;
  MSBoolean operator<=(const MSSymbol& aSymbol_) const;
  MSBoolean operator>=(const MSSymbol& aSymbol_) const;

  long compare(const MSSymbol& aSymbol_) const;
  friend MSTypesExport unsigned long hash(const MSSymbol&,unsigned long size_);

  static MSBoolean doesASymbolExistFor(const char *);
  static const MSSymbol& nullSymbol(void);
  static ostream& printSymbolTable(ostream& aStream_);
 
  friend MSTypesExport ostream& operator<<(ostream& aStream_,const MSSymbol& aSymbol_);
  friend MSTypesExport istream& operator>>(istream& aStream_,MSSymbol& aSymbol_);

private:
  MSAtom _atom;
};

inline long compare(const MSSymbol& aSymbol_,const MSSymbol& bSymbol_)
{  return aSymbol_.compare(bSymbol_);  }

#ifndef MS_NO_INLINES
#ifndef MSSymbolINLINES
#include <MSTypes/MSSymbolInlines.C>
#endif
#endif

#endif
