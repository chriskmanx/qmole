///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSValidationInterface.H>

MSValidationInterface::DefaultValidationCallback::
DefaultValidationCallback(ValidationFunction func_,void *clientData_) :
_function(func_),
_clientData(clientData_)
{}

MSValidationInterface::DefaultValidationCallback::
~DefaultValidationCallback(void)
{}

MSBoolean MSValidationInterface::DefaultValidationCallback::
validate(MSString& aString_)
{
  if (_function!=0) return _function(aString_,_clientData);
  else return MSFalse;
}

MSValidationInterface::MSValidationInterface(void) :
_validationCallback(0)
{}

MSValidationInterface::~MSValidationInterface(void)
{ if (_validationCallback!=0) delete _validationCallback; }
  
void MSValidationInterface::
validationCallback(MSValidationInterface::ValidationFunction f_,void *clientData_)
{ validationCallback(new DefaultValidationCallback(f_,clientData_)); }


void MSValidationInterface::validationCallback(MSValidationCallback *validationCallback_)  
{
  if (_validationCallback!=0) delete _validationCallback;
  _validationCallback=validationCallback_;
}




