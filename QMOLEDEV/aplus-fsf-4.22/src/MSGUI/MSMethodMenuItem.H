#ifndef MSMethodMenuItemHEADER
#define MSMethodMenuItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenuItem.H>
#include <MSGUI/MSMenu.H>

template <class MethodClass> 
class MSMethodMenuItem : public MSMenuItem
{
typedef void (MethodClass::*Method)(void);
private:
  Method       _method;
  MethodClass *_object;

public:
  MSMethodMenuItem(MSMenu *owner_,const char *label_,
		   MethodClass *object_,Method method_,char mnemonic_=0,int tag_=0) :
  _object(object_),_method(method_),
  MSMenuItem(owner_,label_,mnemonic_,tag_) {}

  MSMethodMenuItem(MSMenu *owner_,const MSString& label_,
		   MethodClass *object_,Method method_,char mnemonic_=0,int tag_=0) :
  _object(object_),_method(method_),
  MSMenuItem(owner_,label_,mnemonic_,tag_) {}

  MSMethodMenuItem(MSMenu *owner_,const MSPixmap &pixmap_,
		   const MSPixmap &insensitivePixmap_,
		   MethodClass *object_,Method method_,int tag_=0) :
  _object(object_),_method(method_),
  MSMenuItem(owner_,pixmap_,insensitivePixmap_,tag_) {}

  ~MSMethodMenuItem(void) {}

  Method method(void)       const {return _method;}
  MethodClass *object(void) const {return _object;}

  virtual void activate(void)
  {
    if (menu()!=0)
     {
       menu()->releaseGrab();
       if (_object!=0&&_method!=0) (_object->*_method)();
       menu()->done();
     }
  }
};

#endif

