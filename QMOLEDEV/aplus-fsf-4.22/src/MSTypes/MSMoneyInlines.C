#ifndef MSMoneyINLINES
#define MSMoneyINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif

INLINELINKAGE MSMoney::MSMoney(void):
_currency(DefaultCurrency) {}
INLINELINKAGE MSMoney::MSMoney(const MSMoney& m_):
_currency(m_._currency),MSFloat(m_) {}
INLINELINKAGE MSMoney::MSMoney(double d_,Currency c_):
_currency(c_),MSFloat(d_) {}
INLINELINKAGE MSMoney::MSMoney(int i_,Currency c_):
_currency(c_),MSFloat(i_) {}
INLINELINKAGE MSMoney::MSMoney(const MSFloat& f_,Currency c_):
_currency(c_),MSFloat(f_) {}
INLINELINKAGE MSMoney::MSMoney(const MSInt& i_,Currency c_):
_currency(c_),MSFloat(i_) {}

INLINELINKAGE MSMoney MSMoney::convert(MSMoney::Currency,const MSTime&) const 
{ return MSMoney(*this); }
INLINELINKAGE MSMoney MSMoney::convert(MSMoney::Currency c_) const 
{ return convert(c_,MSTime()); }

INLINELINKAGE MSMoney::Currency MSMoney::currency(void) const 
{
  if (_currency!=DefaultCurrency) return _currency;
  else return _defaultCurrency;
}

INLINELINKAGE MSMoney::operator double() const
{ return _real; }

INLINELINKAGE double  MSMoney::operator/(const MSMoney& m_) const 
{return _real/m_._real;} 

// Be sure to preserve the currency
INLINELINKAGE MSMoney MSMoney::operator-() 
{ return MSMoney(0.0,*this,Minus); }

INLINELINKAGE MSMoney& MSMoney::operator+=(const MSFloat& aFloat_)
{
  MSFloat::operator+=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator+=(const MSInt& aInt_)
{
  MSFloat::operator+=(aInt_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator+=(double aFloat_)
{
  MSFloat::operator+=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator+=(int aInt_)
{
  MSFloat::operator+=(aInt_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator-=(const MSFloat& aFloat_)
{
  MSFloat::operator-=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator-=(const MSInt& aInt_)
{
  MSFloat::operator-=(aInt_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator-=(double aFloat_)
{
  MSFloat::operator-=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator-=(int aInt_)
{
  MSFloat::operator-=(aInt_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator*=(const MSFloat& aFloat_)
{
  MSFloat::operator*=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator*=(const MSInt& aInt_)
{
  MSFloat::operator*=(aInt_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator*=(double aFloat_)
{
  MSFloat::operator*=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator*=(int aInt_)
{
  MSFloat::operator*=(aInt_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator/=(const MSFloat& aFloat_)
{
  MSFloat::operator/=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator/=(const MSInt& aInt_)
{
  MSFloat::operator/=(aInt_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator/=(double aFloat_)
{
  MSFloat::operator/=(aFloat_);
  return *this;
}


INLINELINKAGE MSMoney& MSMoney::operator/=(int aInt_)
{
  MSFloat::operator/=(aInt_);
  return *this;
}

// friend operators

INLINELINKAGE MSMoney operator+(const MSMoney& a_,const MSFloat& b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(const MSMoney& a_,const MSFloat& b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(const MSMoney& a_,const MSFloat& b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE MSMoney operator/(const MSMoney& a_,const MSFloat& b_) 
{return MSMoney(a_,b_,MSMoney::Divide);}  

INLINELINKAGE MSMoney operator+(const MSMoney& a_,const MSInt& b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(const MSMoney& a_,const MSInt& b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(const MSMoney& a_,const MSInt& b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE MSMoney operator/(const MSMoney& a_,const MSInt& b_) 
{return MSMoney(a_,b_,MSMoney::Divide);}  

INLINELINKAGE MSMoney operator+(const MSMoney& a_,double b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(const MSMoney& a_,double b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(const MSMoney& a_,double b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE MSMoney operator/(const MSMoney& a_,double b_) 
{return MSMoney(a_,b_,MSMoney::Divide);}  

INLINELINKAGE MSMoney operator+(const MSMoney& a_,int b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(const MSMoney& a_,int b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(const MSMoney& a_,int b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE MSMoney operator/(const MSMoney& a_,int b_) 
{return MSMoney(a_,b_,MSMoney::Divide);}  

INLINELINKAGE MSMoney operator+(const MSFloat& a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(const MSFloat& a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(const MSFloat& a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE double  operator/(const MSFloat& a_,const MSMoney& b_) 
{return double(double(a_)/double(b_));}

INLINELINKAGE MSMoney operator+(const MSInt& a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(const MSInt& a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(const MSInt& a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE double  operator/(const MSInt& a_,const MSMoney& b_) 
{return double(double(a_)/double(b_));}

INLINELINKAGE MSMoney operator+(double a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(double a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(double a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE double  operator/(double a_,const MSMoney& b_) 
{return double(double(a_)/double(b_));}

INLINELINKAGE MSMoney operator+(int a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Plus);}    
INLINELINKAGE MSMoney operator-(int a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Minus);}   
INLINELINKAGE MSMoney operator*(int a_,const MSMoney& b_) 
{return MSMoney(a_,b_,MSMoney::Multiply);} 
INLINELINKAGE double  operator/(int a_,const MSMoney& b_) 
{return double(double(a_)/double(b_));}

#endif  //MSMoneyINLINES
