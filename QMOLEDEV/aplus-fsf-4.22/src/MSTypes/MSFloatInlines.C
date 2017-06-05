#ifndef MSFloatINLINES
#define MSFloatINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


//--------------------------------------------------------------------------------
// MSFloat bit flags methods - protected API
//--------------------------------------------------------------------------------
INLINELINKAGE MSBoolean MSFloat::bitState(MSFloat::BitFlag flag_) const
{ return (_flags&flag_)?MSTrue:MSFalse; }

INLINELINKAGE void MSFloat::setToValid(void)
{ _flags|=MSFloat::Valid; }
 
INLINELINKAGE void MSFloat::setToInValid(void)
{ _flags&=~MSFloat::Valid; }

INLINELINKAGE void MSFloat::setBit(MSFloat::BitFlag flag_)
{ _flags|=flag_; }

INLINELINKAGE void MSFloat::unsetBit(MSFloat::BitFlag flag_)
{ _flags&=~flag_; }

//--------------------------------------------------------------------------------
// MSFloat bit flags methods - public API
//--------------------------------------------------------------------------------
INLINELINKAGE MSBoolean MSFloat::isValid(void) const
{ return bitState(MSFloat::Valid); }

INLINELINKAGE MSBoolean MSFloat::isSet(void) const
{ return bitState(MSFloat::Set); }

INLINELINKAGE void MSFloat::setInvalid(void)
{ setToInValid(); }

//--------------------------------------------------------------------------------
// MSFloat comparison methods
//--------------------------------------------------------------------------------
INLINELINKAGE MSBoolean operator==(double d_,const MSFloat& f_)       
{ return MSBoolean(f_==d_); }
INLINELINKAGE MSBoolean operator==(int i_,const MSFloat& f_)          
{ return MSBoolean(f_==i_); }
INLINELINKAGE MSBoolean operator==(const MSInt& i_,const MSFloat& f_) 
{ return MSBoolean(f_==i_); }
INLINELINKAGE MSBoolean operator!=(double d_,const MSFloat& f_)       
{ return MSBoolean(!(f_==d_)); }
INLINELINKAGE MSBoolean operator!=(int i_,const MSFloat& f_)          
{ return MSBoolean(!(f_==i_)); }
INLINELINKAGE MSBoolean operator!=(const MSInt& i_,const MSFloat& f_) 
{ return MSBoolean(!(f_==i_)); }

// compare MSFloat to MSFloat
INLINELINKAGE MSBoolean operator <(const MSFloat& a_,const MSFloat& b_) 
{ return MSBoolean(a_.compare(b_)<0); }
INLINELINKAGE MSBoolean operator >(const MSFloat& a_,const MSFloat& b_) 
{ return MSBoolean(a_.compare(b_)>0); }
INLINELINKAGE MSBoolean operator<=(const MSFloat& a_,const MSFloat& b_) 
{ return MSBoolean(a_.compare(b_)<=0);}
INLINELINKAGE MSBoolean operator>=(const MSFloat& a_,const MSFloat& b_) 
{ return MSBoolean(a_.compare(b_)>=0);}

// compare MSFloat to double
INLINELINKAGE MSBoolean operator <(const MSFloat& f_,double d_) 
{ return MSBoolean(f_.compare(d_)<0); }
INLINELINKAGE MSBoolean operator >(const MSFloat& f_,double d_) 
{ return MSBoolean(f_.compare(d_)>0); }
INLINELINKAGE MSBoolean operator<=(const MSFloat& f_,double d_) 
{ return MSBoolean(f_.compare(d_)<=0);}
INLINELINKAGE MSBoolean operator>=(const MSFloat& f_,double d_) 
{ return MSBoolean(f_.compare(d_)>=0);}

// compare double to MSFloat
INLINELINKAGE MSBoolean operator <(double d_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(d_)>0); }
INLINELINKAGE MSBoolean operator<=(double d_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(d_)>=0);}
INLINELINKAGE MSBoolean operator >(double d_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(d_)<0); }
INLINELINKAGE MSBoolean operator>=(double d_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(d_)<=0);}

// compare MSFloat to int
INLINELINKAGE MSBoolean operator <(const MSFloat& f_,int i_) 
{ return MSBoolean(f_.compare(i_)<0); }
INLINELINKAGE MSBoolean operator<=(const MSFloat& f_,int i_) 
{ return MSBoolean(f_.compare(i_)<=0);}
INLINELINKAGE MSBoolean operator >(const MSFloat& f_,int i_) 
{ return MSBoolean(f_.compare(i_)>0); }
INLINELINKAGE MSBoolean operator>=(const MSFloat& f_,int i_) 
{ return MSBoolean(f_.compare(i_)>=0);}

// compare int to MSFloat
INLINELINKAGE MSBoolean operator <(int i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(i_)>0); }
INLINELINKAGE MSBoolean operator<=(int i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(i_)>=0);}
INLINELINKAGE MSBoolean operator >(int i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(i_)<0); }
INLINELINKAGE MSBoolean operator>=(int i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare(i_)<=0);}

// compare MSFloat to MSInt
INLINELINKAGE MSBoolean operator <(const MSFloat& f_,const MSInt& i_) 
{ return MSBoolean(f_.compare((int)i_)<0); }
INLINELINKAGE MSBoolean operator<=(const MSFloat& f_,const MSInt& i_) 
{ return MSBoolean(f_.compare((int)i_)<=0);}
INLINELINKAGE MSBoolean operator >(const MSFloat& f_,const MSInt& i_) 
{ return MSBoolean(f_.compare((int)i_)>0); }
INLINELINKAGE MSBoolean operator>=(const MSFloat& f_,const MSInt& i_) 
{ return MSBoolean(f_.compare((int)i_)>=0);}

// compare MSInt to MSFloat
INLINELINKAGE MSBoolean operator <(const MSInt& i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare((int)i_)>0); }
INLINELINKAGE MSBoolean operator<=(const MSInt& i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare((int)i_)>=0);}
INLINELINKAGE MSBoolean operator >(const MSInt& i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare((int)i_)<0); }
INLINELINKAGE MSBoolean operator>=(const MSInt& i_,const MSFloat& f_) 
{ return MSBoolean(f_.compare((int)i_)<=0);}

#endif
