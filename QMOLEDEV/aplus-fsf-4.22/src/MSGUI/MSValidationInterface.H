#ifndef MSValidationInterfaceHEADER
#define MSValidationInterfaceHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSValidationCallback.H>

class MSGUIExport MSValidationInterface
{
public:
  typedef MSBoolean (*ValidationFunction)(MSString&,void *);

  class MSGUIExport DefaultValidationCallback : public MSValidationCallback
  {
  private:
    ValidationFunction  _function;
    void               *_clientData;
  public:
    DefaultValidationCallback(ValidationFunction func_,void *clientData_=0);
    ~DefaultValidationCallback(void);
    virtual MSBoolean validate(MSString&);
  };

protected:
  MSValidationCallback *_validationCallback;
  
public:
  MSValidationInterface(void);
  ~MSValidationInterface(void);

  MSValidationCallback *validationCallback(void) {return _validationCallback;}
  
  void validationCallback(ValidationFunction,void *clientData_=0);
  void validationCallback(MSValidationCallback *);  
};

#endif



