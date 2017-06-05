#ifndef MSKeyTranslationTableHEADER
#define MSKeyTranslationTableHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSKeysymdef.H>
#include <MSGUI/MSKeyPress.H>
#include <MSTypes/MSEnum.H>

class MSHashTable;
class MSNodeItem;
class MSKeyTableData;
class MSWidget;
class MSKeyCallback;

//***********************************************************************************************************************
// This class manages a list of MSKeyCallback/specification String pairs grouped together into keyTableData elements.
//The specification strings( in accordance with the rules defined in MSKeyPress ) define the condition under which their
//MSKeyCallback get invoked. Their are two types of keyTableData elements the default, one for each MSKeyTranslationTable, 
//and the global variety which can be used across multiple MSKeyTranslationTables. Sharing across 
//MSKeyTranslationTables is accomplished by sharing keyTableData elements. If an keyTableData element is added to and
//defined ( assigned a group of MSKeyCallback/specification String pairs to it)  in a MSKeyTranslationTable it can be added
//to another one( with out having to redefine it). This feature is used to allow an MSWidget class to share the same 
//translations. Internally this is done using MSKeyClassCallback<MSWidget_class> callbacks. When they are invoked the 
//MSWidget_class::Method method they were initalized with is called on the MSWidget_class instance they were invoked on.
//
//The order followed in the keyTranslation process is LIFO(derived class MSKeyCallback's specification's will be looked at 
//before their base classes). This processing continues untill all the "matching" MSKeyCallback's are called or until one 
//of them upon invokation returns an MSTrue.    
//***********************************************************************************************************************

class MSGUIExport MSKeyTranslationTable
{
public:
  MSKeyTranslationTable();
  ~MSKeyTranslationTable();
  void addKeyTableData(const char*);
  void removeKeyTableData(const char*);
  unsigned long  addCallback(const char*, MSKeyCallback*, const char* keyTableData=0);
  void removeAllCallbacksThat(const char*);
  void removeCallback(const char*, const char* keyTableData=0);
  void removeCallback(unsigned long, const char* keyTableData=0);
  void removeCallback(MSKeyCallback*, const char* keyTableData=0);
  void removeAll();
  MSBoolean  translate(const MSKeyPress&,MSWidget*) const;
  MSBoolean  hasMatch(const MSKeyPress&) const;
  static MSBoolean keyTableData(const char *pName_);
private:
  static MSHashTable  *_pHashTable;
  MSNodeItem          *_pListHead;
  static MSKeyTableData *data(const char *pName_);
  void add(MSKeyTableData*);
  void remove(MSKeyTableData*);
  MSKeyTableData* defaultKeyTableData(void) const;
};

inline void MSKeyTranslationTable::removeCallback(MSKeyCallback* keyCallback_, const char* keyTableData_)
{ removeCallback((unsigned long)keyCallback_, keyTableData_);  }


  
#endif
