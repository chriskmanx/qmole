#ifndef MSKeyTableDataHEADER
#define MSKeyTableDataHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSKeyPress.H>

class MSWidget;
class MSKeyCallbackNode;
class MSKeyCallback;

class MSGUIExport MSKeyTableData
{
private:
  class List 
  {
  public:
    int                 _size;
    int                 _count;
    MSKeyCallbackNode  **_array;

    List(int size_=0);
    ~List(void);

    void reserve(int size_);
    void add(MSKeyCallbackNode *);
    void remove(MSKeyCallbackNode *);
  };

  unsigned    _referenceCount;
  List        _ktList;
  char       *_pName;
  
  friend class MSKeyTranslationTable;
  void addReference(void);
  void removeReference(void);
  unsigned referenceCount(void) const {return _referenceCount;}
  void remove( KeySym, unsigned int, unsigned int);
  
public:
  MSKeyTableData(const char *pName_);
  ~MSKeyTableData(void);

  const char *name(void)        const {return _pName;}
  void add(const char*,MSKeyCallback*);
  void remove(unsigned long);
  void remove(const char*);
  MSBoolean process(const MSKeyPress&,MSWidget*) const;
  MSBoolean hasMatch(const MSKeyPress&) const;
};

#endif





