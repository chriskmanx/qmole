#ifndef MSNumberINLINES
#define MSNumberINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


//--------------------------------------------------------------------------------
// MSNumber::Buffer INLINELINKAGE methods
//--------------------------------------------------------------------------------

INLINELINKAGE int MSNumber::Buffer::decimalPlace(void) const
{ return _exponent; }

INLINELINKAGE MSBoolean MSNumber::Buffer::isValid(void) const
{ return MSBoolean(this != _pInvalidBuffer); }

INLINELINKAGE MSBoolean MSNumber::Buffer::isSet(void) const
{ return MSBoolean(this != _pNullBuffer); }

//--------------------------------------------------------------------------------
// MSNumber INLINELINKAGE methods
//--------------------------------------------------------------------------------
INLINELINKAGE MSNumber::Buffer *MSNumber::buffer() const
{ return _pBuffer; }

INLINELINKAGE MSBoolean MSNumber::isValid(void) const
{ return buffer()->isValid(); }

INLINELINKAGE MSBoolean MSNumber::isSet(void) const
{ return buffer()->isSet(); }

INLINELINKAGE int MSNumber::decimalPlace(void) const
{ return buffer()->decimalPlace(); }

//--------------------------------------------------------------------------------
// MSNumber unary operators.
//--------------------------------------------------------------------------------
INLINELINKAGE MSNumber MSNumber::operator+()
{ return MSNumber(*this); }

INLINELINKAGE MSNumber MSNumber::operator-()
{ return MSNumber(0)-MSNumber(*this); }

#endif
