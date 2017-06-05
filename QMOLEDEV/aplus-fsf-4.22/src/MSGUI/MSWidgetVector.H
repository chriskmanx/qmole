#ifndef MSWidgetVectorHEADER
#define MSWidgetVectorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSUnsignedLongVector.H>

class MSWidget;

class MSGUIExport MSWidgetVector : public MSUnsignedLongVector
{
public:

  class MSGUIExport Pick
  {
  friend class MSWidgetVector;
  public:
    Pick& operator=(MSWidget *);
  
    operator MSWidget *() const;
  
    MSWidgetVector *pVector(void) const;
    unsigned index(void) const;
    unsigned length(void) const;
    MSWidget *value(void) const;
  
  private:
    Pick(const MSWidgetVector&,unsigned);
    Pick(const Pick&);

    MSWidgetVector *_pVector;
    unsigned        _index; 
  };

  friend class Pick;

  MSWidgetVector(void);
  MSWidgetVector(const MSWidgetVector&);
  MSWidgetVector(MSWidget *);
  ~MSWidgetVector(void);
  
  MSWidget *operator()(unsigned) const;
  MSWidget *operator()(unsigned);
  MSWidget *operator[](unsigned) const;
  Pick operator[](unsigned);

  MSWidgetVector& operator=(const MSWidgetVector&);
  MSWidgetVector& append(MSWidget *);
  MSWidgetVector& append(const MSWidgetVector &);

  MSWidgetVector& operator<<(MSWidget *);
  MSWidgetVector& operator<<(const MSWidgetVector&);
//  friend MSGUIExport MSWidgetVector& operator<<(MSWidgetVector&,MSWidget *);
//  friend MSGUIExport MSWidgetVector& operator<<(MSWidgetVector&,const MSWidgetVector&);
};

// Pick inline methods
inline MSWidgetVector *MSWidgetVector::Pick::pVector(void) const 
{ return _pVector; }

inline unsigned MSWidgetVector::Pick::index(void) const 
{ return _index; }

inline unsigned MSWidgetVector::Pick::length(void) const 
{ return _pVector->length(); }

inline MSWidgetVector::Pick::operator MSWidget *() const
{ 
return (MSWidget *)((const MSWidgetVector *)_pVector)->elementAt(_index);
}

inline MSWidget *MSWidgetVector::Pick::value(void) const
{ 
return (MSWidget *)((const MSWidgetVector *)_pVector)->elementAt(_index); 
}

inline MSWidgetVector::Pick::Pick(const MSWidgetVector& aVector_,unsigned index_)
{ 
  _pVector=&(MSWidgetVector&)aVector_; 
  _index=index_;
}

inline MSWidgetVector::Pick::Pick(const Pick& aPick_) 
{
  _pVector=aPick_.pVector(); 
  _index=aPick_.index();
}

inline MSWidget *MSWidgetVector::operator()(unsigned index_) const
{ return (MSWidget *)elementAt(index_); }

inline MSWidget *MSWidgetVector::operator()(unsigned index_)
{ return (MSWidget *)((const MSWidgetVector *)this)->elementAt(index_); }

inline MSWidget *MSWidgetVector::operator[](unsigned index_) const
{ return (MSWidget *)elementAt(index_); }

inline MSWidgetVector::Pick MSWidgetVector::operator[](unsigned index_)
{ return Pick(*this,index_); }

#endif






