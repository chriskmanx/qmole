#ifndef MSCompositeFieldBoxHEADER
#define MSCompositeFieldBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSLayout.H>
#include <MSGUI/MSCompositeField.H>

class MSGUIExport MSCompositeFieldBox : public MSLayout
{
public:  
  MSCompositeFieldBox(MSWidget *,const char *title_=0);
  MSCompositeFieldBox(MSWidget *,const MSStringVector&);
  ~MSCompositeFieldBox(void);

  unsigned editWidth(void) const;
  unsigned valueWidth(void) const;
  void valueWidth(unsigned);
  void editWidth(unsigned);

  MSCompositeField *field(const MSSymbol& tag_);
  const MSCompositeField *field(const MSSymbol& tag_) const;

  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  unsigned _editWidth;
  unsigned _valueWidth;

  void init(void);

  virtual void extraRowSpace(int num_,MSLayoutVector *vec_,int size_);
  virtual void placement(void);
  virtual void alignLabels(void);
  virtual void firstMapNotify(void);
};

inline unsigned MSCompositeFieldBox::editWidth(void) const 
{ return _editWidth; }
inline unsigned MSCompositeFieldBox::valueWidth(void) const
{ return _valueWidth; }

#endif












