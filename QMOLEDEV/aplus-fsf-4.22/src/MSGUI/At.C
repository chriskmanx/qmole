///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/At.H>
#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream.h>
#endif

At::At(void) 
{ at(0,0,1,1); }
At::At(int row_,int col_,int rowSpan_,int columnSpan_,unsigned long constraints_) 
{ at(row_,col_,rowSpan_,columnSpan_,constraints_); }    
At::At(int row_,int col_,int rowSpan_,int columnSpan_,const char *constraints_)
{ at(row_,col_,rowSpan_,columnSpan_,parseConstraints(constraints_)); }     
At::At(const At& at_) 
{ *this=at_; }

At::At(const char *pString_)
{
  at(0,0,1,1);
  parseString(pString_,strlen(pString_));
}

At::At(const MSString& aString_)
{
  at(0,0,1,1);
  parseString(aString_,aString_.length());
}

void At::parseString(const char *pString_, int len)
{
#if defined(MS_NO_ISTRSTREAM_CONSTCHAR_CONSTRUCTOR)
  istrstream ist((char *)(void*)pString_,len);
#else
#if HAVE_SSTREAM
  istringstream ist(pString_);
#else
  istrstream ist(pString_,len);
#endif
#endif  // MS_NO_ISTRSTREAM_CONSTCHAR_CONSTRUCTOR

  ist>>_row;
  if (ist) ist>>_column;
  if (ist) ist>>_rowSpan;
  if (ist) ist>>_columnSpan;
  if (ist)
   {
     while (ist.peek()==' ') ist.get(); 
     char buf[12];
     buf[0]='\0';
     if (ist) ist.getline(buf,sizeof(buf),' ');
     constraints(buf);  
   }
}

At::~At(void)
{}

MSBoolean At::operator==(const At& at_)
{
  return MSBoolean(_row==at_.row()&&_column==at_.column()&&
		   _rowSpan==at_.rowSpan()&&_columnSpan==at_.columnSpan()&& 
		   _constraints==at_.constraints());
}

MSBoolean At::operator!=(const At& at_)
{
  return MSBoolean(_row!=at_.row()||_column!=at_.column()||
		   _rowSpan!=at_.rowSpan()||_columnSpan!=at_.columnSpan()|| 
		   _constraints!=at_.constraints());
}

At& At::operator=(const At& at_)
{
  if (this!=&at_)
   {
     _row=at_._row,_column=at_._column;
     _rowSpan=at_._rowSpan,_columnSpan=at_._columnSpan; 
     _constraints=at_._constraints;
   }
  return *this; 
}

int At::row(void) const
{ return _row; }
int At::column(void) const
{ return _column; }
int At::rowSpan(void) const
{ return _rowSpan; }     
int At::columnSpan(void) const
{ return _columnSpan; }
unsigned long At::constraints(void) const
{ return _constraints; }

void At::row(int row_)
{ _row=row_; }
void At::column(int col_)
{ _column=col_; }
void At::rowSpan(int span_)
{ _rowSpan=span_; }
void At::columnSpan(int span_)
{ _columnSpan=span_; }
void At::constraints(unsigned long constraints_)
{ _constraints=constraints_; }

void At::at(int row_,int col_,int rowSpan_,int columnSpan_,unsigned long constraints_) 
{ _row=row_,_column=col_,_rowSpan=rowSpan_,_columnSpan=columnSpan_,_constraints=constraints_; }

void At::at(int row_,int col_,int rowSpan_,int columnSpan_,const char *constraints_) 
{ at(row_,col_,rowSpan_,columnSpan_,parseConstraints(constraints_)); }    

//#############################################################################################
// At parsing methods

MSString At::parsedConstraints(void) const
{ return parseConstraints(_constraints); }

void At::constraints(const char *constraints_)
{ _constraints=parseConstraints(constraints_); }

unsigned long At::parseConstraints(const char *constraints_) const
{
  static char options[]= {'+','l','r','t','b','w','h','W','H'};
  unsigned long r=0;
  int len=(constraints_!=0)?strlen(constraints_):0;

  if (len>0)
   {
     char *cp=strchr((char*)(void*)constraints_,options[0]);
     // cast is needed for bug in Borland
     if (cp!=0) r|=_constraints;
     for (int j=1,k=1;j<=MaintainHeight&&len>0;j<<=1,k++) 
      { if ((cp=strchr((char*)(void*)constraints_,options[k]))!=0) r|=j,len--; }
   }
  return r;
}

MSString At::parseConstraints(unsigned long constraints_) const
{
  static char options[]= {'+','l','r','t','b','w','h','W','H'};
  static char opts[16];
  int len=0;
  if (constraints_>0)
   {
     for (int j=1,k=1;j<=128;j<<=1,k++) 
      { if ((constraints_&j)==j) opts[len++]=options[k]; }
   }
  opts[len]='\0';
  return MSString(opts);
}

MSString At::asString(void) const
{
  MSString result;
  result+=MSString(_row);
  result+=" ";
  result+=MSString(_column);
  result+=" ";
  result+=MSString(_rowSpan);
  result+=" ";
  result+=MSString(_columnSpan);
  result+=" ";
  result+=parsedConstraints();
  return MSString(result);
}

