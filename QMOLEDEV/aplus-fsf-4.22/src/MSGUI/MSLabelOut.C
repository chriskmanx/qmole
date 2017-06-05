///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSLabelOut.H>

MSLabelOut::MSLabelOut(void) : _format(MSFloat::Default)
{init();}

MSLabelOut::MSLabelOut(MSAlignment)
{init();}

MSLabelOut::MSLabelOut(const MSStringVector& aStringVector_) : _labels(aStringVector_),_format(MSFloat::Default)
{init();}

MSLabelOut::MSLabelOut(const MSFormat& aFormat_) : _format(aFormat_)
{init();}

void MSLabelOut::init(void)
{_owner=0;}

void MSLabelOut::format(const MSFormat& aFormat_)
{
  _format=aFormat_;
  if (owner()!=0) owner()->redraw();
}

MSLabelOut::~MSLabelOut(void) {}

const char *MSLabelOut::formatOutput(MSString &buffer_,double data_)
{
  if (data_<UINT_MAX)
   {
     unsigned index=unsigned(data_);
     if (index<labels().length())
      {
	buffer_=labels()(index);
	return buffer_.string();
      }
   }
  switch (format().formatType())
   {
   case MSFormat::Date:
     {
       MSDate aDate((MSJulian)data_);
       return aDate.format(buffer_,format());
     }
   case MSFormat::Money:
     {
       MSMoney aMoney(data_);
       return aMoney.format(buffer_,format());
     }
   case MSFormat::Rate:
     {
       MSRate aRate(data_);
       return aRate.format(buffer_,format());
     }
   case MSFormat::Time:
     {
       MSTime aTime((time_t)data_);
       return aTime.format(buffer_,format());
     }
   case MSFormat::Float:
   default:
     {
       MSFloat aFloat(data_);
       return aFloat.format(buffer_,format());
     }
   }
  /* return buffer_.string(); */
}

double MSLabelOut::tickPosition(unsigned index_) const
{
  int len=tickPositions().length();
  return tickPositions()(index_<len?index_:index_%len);
}

double MSLabelOut::tickSize(unsigned index_) const
{
  int len=tickSizes().length();
  return tickSizes()(index_<len?index_:index_%len);
}

unsigned MSLabelOut::gridWidth(unsigned index_) const
{
  int len=gridWidths().length();
  return gridWidths()(index_<len?index_:index_%len);
}

const char *MSLabelOut::label(MSString &buffer_,unsigned index_) const
{
  if (index_<labels().length()) buffer_=labels()(index_);
  return buffer_.string();
}






