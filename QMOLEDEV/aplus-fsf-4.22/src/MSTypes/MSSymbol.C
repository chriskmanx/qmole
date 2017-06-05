///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSString.H>
#include <MSTypes/MSNameSpace.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

static const unsigned long _intializationMagicNumber=0x55AA;
static unsigned long  _symbolTableInitialized=0;
static MSNameSpace   *_symbolTable=0;

#ifdef MS_NO_INLINES
#include <MSTypes/MSSymbolInlines.C>
#endif

MSSymbol::MSSymbol(void) :
_atom(0x0)
{}

MSSymbol::MSSymbol(const MSSymbol& aSymbol_) :
_atom(aSymbol_._atom)
{}

MSSymbol::MSSymbol(const char *symbolName_) :
_atom(0x0)    
{
  // this is a hack to force initialization
  // of the symbol table before any symbols are constructed
  if (_symbolTableInitialized!=_intializationMagicNumber)
   {                                       
     _symbolTable=new MSNameSpace;         
     _symbolTableInitialized=_intializationMagicNumber;
   }
  if (symbolName_!=0) _atom=_symbolTable->intern(symbolName_);
}

MSSymbol::~MSSymbol(void)
{}

MSSymbol& MSSymbol::operator=(const MSSymbol& aSymbol_)
{ _atom=aSymbol_._atom; return *this; }

MSBoolean MSSymbol::operator==(const MSSymbol& aSymbol_) const
{ return MSBoolean(_atom==aSymbol_._atom); }
MSBoolean MSSymbol::operator!=(const MSSymbol& aSymbol_) const
{ return MSBoolean(_atom!=aSymbol_._atom); }

unsigned long hash(const MSSymbol& aSymbol_,unsigned long size_)
{ return aSymbol_._atom%size_; }

long MSSymbol::compare(const MSSymbol& aSymbol_) const
{
  if (_atom==aSymbol_._atom) return 0;
  if (_atom!=MSNullAtom&&aSymbol_._atom!=MSNullAtom)
   {
     return strcmp(symbolName(),aSymbol_.symbolName());
   }
  else if (_atom==MSNullAtom) return -1;
  else return 1;
}

MSBoolean MSSymbol::operator<(const MSSymbol& aSymbol_) const
{
  if (_atom==aSymbol_._atom) return MSFalse;
  if (_atom!=MSNullAtom&&aSymbol_._atom!=MSNullAtom)
   {
     return MSBoolean(strcmp(symbolName(),aSymbol_.symbolName())<0);
   }
  return MSBoolean(_atom<aSymbol_._atom);  
}

MSBoolean MSSymbol::operator>(const MSSymbol& aSymbol_) const
{
  if (_atom==aSymbol_._atom) return MSFalse;  
  if (_atom!=MSNullAtom&&aSymbol_._atom!=MSNullAtom)
   {
     return MSBoolean(strcmp(symbolName(),aSymbol_.symbolName())>0);
   }
  return MSBoolean(_atom>aSymbol_._atom);
}

MSBoolean MSSymbol::operator<=(const MSSymbol& aSymbol_) const
{
  if (_atom!=MSNullAtom&&aSymbol_._atom!=MSNullAtom)
   {
     return MSBoolean(strcmp(symbolName(),aSymbol_.symbolName())<=0);
   }
  return MSBoolean(_atom<=aSymbol_._atom);  
}

MSBoolean MSSymbol::operator>=(const MSSymbol& aSymbol_) const
{
  if (_atom!=MSNullAtom&&aSymbol_._atom!=MSNullAtom)
   {
     return MSBoolean(strcmp(symbolName(),aSymbol_.symbolName())>=0);
   }
  return MSBoolean(_atom>=aSymbol_._atom);
}

const MSSymbol& MSSymbol::nullSymbol(void)
{
  static MSSymbol aSymbol;
  return aSymbol;
} 

const char *MSSymbol::symbolName(void) const
{ return _symbolTable->atomName(atom()); }  

MSString MSSymbol::asString(void) const
{ return MSString(_symbolTable->atomName(atom())); }  

MSString MSSymbol::asDebugInfo(void) const
{ 
  MSString result("MSSymbol(@");
  result+=MSString((unsigned long)this).d2x().lowerCase();
  result+=",_atom=";
  result+=MSString(atom());
  result+=")";
  return result;
}

MSString MSSymbol::className(void) const
{ return MSString("MSSymbol"); }  

MSBoolean MSSymbol::doesASymbolExistFor(const char *pString_)
{ return (_symbolTable->lookup(pString_)==MSNullAtom)?MSFalse:MSTrue; }

ostream& MSSymbol::printSymbolTable(ostream& aStream_)
{ 
  if (_symbolTable==0) return aStream_;
  return aStream_<<*_symbolTable; 
}

ostream& operator<<(ostream& aStream_,const MSSymbol& aSymbol_)
{ return aStream_<<aSymbol_.symbolName(); }

istream& operator>>(istream &aStream_,MSSymbol &aSymbol_)
{
  MSString aString;
  aStream_>>aString;
  if (aString.length()>0) aSymbol_=MSSymbol(aString);
  else aSymbol_=MSSymbol::nullSymbol();
  return aStream_;
}
