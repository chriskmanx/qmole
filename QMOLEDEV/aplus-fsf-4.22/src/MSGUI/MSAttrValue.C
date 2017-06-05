///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSAttrValue.H>
#include <MSGUI/MSWidget.H>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif

MSAttrValue::MSAttrValue(void) :
_valueType(MSAttrValue::Any)
{}

MSAttrValue::~MSAttrValue(void)
{}

MSAttrValue::MSAttrValue(const MSAttrValue& aAttrValue_) :
_attribute(aAttrValue_._attribute),
_value(aAttrValue_._value),
_options(aAttrValue_._options),
_valueType(aAttrValue_._valueType)
{}

MSAttrValue::MSAttrValue(const char *attribute_,const char *value_,
                         unsigned long valueType_) :				
_attribute(attribute_),
_value(value_),
_valueType(valueType_)
{}

MSAttrValue::MSAttrValue(const char *attribute_,const MSString& value_,
                         unsigned long valueType_) :				
_attribute(attribute_),
_value(value_),
_valueType(valueType_)
{}

MSAttrValue::MSAttrValue(const MSString& attribute_,const char *value_,
                         unsigned long valueType_) :				
_attribute(attribute_),
_value(value_),
_valueType(valueType_)
{}

MSAttrValue::MSAttrValue(const MSString& attribute_,const MSString& value_,
                         unsigned long valueType_) :
_attribute(attribute_),
_value(value_),
_valueType(valueType_)
{}

MSAttrValue::MSAttrValue(const MSString& attribute_,const MSString& value_,
                         const MSStringVector& options_,unsigned long valueType_) :
_attribute(attribute_),
_value(value_),
_options(options_),
_valueType(valueType_)
{}

MSAttrValue& MSAttrValue::operator=(const MSAttrValue& aAttrValue_)
{ 
  if (&aAttrValue_!=this)
   {
     _attribute=aAttrValue_._attribute;
     _value=aAttrValue_._value;
     _options=aAttrValue_._options;
     _valueType=aAttrValue_._valueType;
   }
  return *this;
}


MSString MSAttrValue::enumToString(unsigned long enumValue_, const MSStringVector& values_,
				   const MSUnsignedLongVector& enumValues_, 
				   const MSString& empty_, MSBoolean exact_)
{
  MSString aString;
  int i,n=enumValues_.length();
  for(i=0;i<n;i++)
   {
     if(exact_==MSTrue)
      { 
       if(enumValue_==enumValues_(i)) 
	{ 
	  aString=values_(i);
	  break;
	}
      }
     else
      {
        if(enumValue_&enumValues_(i))
	 {
	   if (aString.length()!=0)	aString<<'|';
	   aString<<values_(i);
	 }
      }
   }
  if(aString.length()==0) aString=empty_;
  return aString;
}

unsigned long MSAttrValue::stringToEnum(const MSString& value_, 
					const MSStringVector& values_,
					const MSUnsignedLongVector& enumValues_, 
					unsigned long empty_, MSBoolean exact_)
{
   unsigned long result=empty_;
   int index;
   MSStringVector aVector;
   if(exact_==MSTrue)
    {
      aVector.appendSingle(value_);
    }
   else
    {
      MSString aString=MSString::change(value_,"|","\n");
      aVector=aString;
    }
   for (unsigned i=0;i<aVector.length();i++)
    {
      index=values_.indexOf(aVector(i));
      if(index!=values_.length())
       {
	if(exact_==MSFalse) result|=enumValues_(index);
	else 
	 {
	  result=enumValues_(index);
	  break;
	 }
       }
    }
   return result;
}


MSString MSAttrValue::alignmentToString(unsigned long alignment_)
{
  MSString aString;
  unsigned long alignmentValues[5]= { MSCenter,MSTop,MSBottom,MSRight,MSLeft };
  char *alignmentNames[5]={ "MSCenter","MSTop","MSBottom","MSRight","MSLeft" };

  for(int i=0; i<5; i++)
   {
     if(alignment_&alignmentValues[i])
      {
        if (aString.length()!=0)	aString<<'|';
        aString<<alignmentNames[i];
      }
   }
  if (aString.length()==0) aString="MSNone";
  return aString;
}

unsigned long MSAttrValue::stringToAlignment(const MSString& aString_)
{
  unsigned long alignment=MSNone;
  MSString alignmentString = MSString::change(aString_,"|","\n");
  MSStringVector alignmentVector(alignmentString);
  for (unsigned j=0;j<alignmentVector.length();j++)
   {
     if (alignmentVector(j)=="MSCenter") alignment|=MSCenter;
     else if (alignmentVector(j)=="MSTop") alignment|=MSTop;
     else if (alignmentVector(j)=="MSBottom") alignment|=MSBottom;
     else if (alignmentVector(j)=="MSLeft") alignment|=MSLeft;
     else if (alignmentVector(j)=="MSRight") alignment|=MSRight;
   }
  return alignment;
}

MSString MSAttrValue::stringVectorToString(const MSStringVector& aVector_)
{
  MSString aString;
  unsigned i=0;
  if (aVector_.length()>0)
   {
     for (;i<aVector_.length()-1;i++) aString<<aVector_(i)<<"\\n";
     aString<<aVector_(i);
   }
  return aString;
}

MSStringVector MSAttrValue::stringToStringVector(const MSString& aString_)
{
  MSString aString = MSString::change(aString_,"\\n",MSString('\n'));
  if(aString.length()>0) return MSStringVector(aString);
  else return MSStringVector();
}

MSString MSAttrValue::colorVectorToString(const MSUnsignedLongVector& colorVector_, MSDisplayServer *server_)
{
  MSString colorString;
  int i;
  for (i=0;i<colorVector_.length();i++)
   {
     if (i!=0) colorString<<"\\n";
     colorString<<server_->colorName(colorVector_(i));
   }
  return colorString;
}

MSString MSAttrValue::shadowStyleToString(MSShadowStyle shadowStyle_)
{
  char *value;
  switch (shadowStyle_)
   {
   case MSRaised:    value="MSRaised";    break;
   case MSSunken:    value="MSSunken";    break;
   case MSEtchedIn:  value="MSEtchedIn";  break;
   case MSEtchedOut: value="MSEtchedOut"; break;
   case MSFlat:      value="MSFlat";      break;
   }
  return MSString(value);
}

MSShadowStyle MSAttrValue::stringToShadowStyle(const MSString &string_)
{
  if (string_=="MSRaised")         return MSRaised;
  else if (string_=="MSSunken")    return MSSunken;
  else if (string_=="MSEtchedIn")  return MSEtchedIn;
  else if (string_=="MSEtchedOut") return MSEtchedOut;
  else                             return MSFlat;
}

MSString MSAttrValue::lineStyleToString(MSLineStyle lineStyle_)
{
  if (lineStyle_==MSDot) return MSString("MSDot");
  else if (lineStyle_==MSDash) return MSString("MSDash");
  else return MSString("MSSolid");
}

MSLineStyle MSAttrValue::stringToLineStyle(const MSString &string_)
{
  if (string_=="MSDot") return MSDot;
  else if (string_=="MSDash") return MSDash;
  else return MSSolid;
}

MSAttrValueList::MSAttrValueList(void) 
{ _size=0,_length=0,_array=0; }

MSAttrValueList::~MSAttrValueList(void) 
{ delete [] _array; }

// make sure there are at least size_+1 elements in the array
void MSAttrValueList::reserve(unsigned size_)
{
  unsigned n=size_+1;
  if (size()<n)
   {
     unsigned newSize=(size()==0)?8:size();
     unsigned i;
     while (newSize<n) newSize<<=1; 
     MSAttrValue *array=new MSAttrValue[newSize];
     for (i=0;i<size();i++) array[i]=_array[i];
     delete [] _array;
     _size=newSize;
     _array=array;
   }
}

MSAttrValueList& operator<<(MSAttrValueList& aList_,const MSAttrValue& aAttrValue_)
{ 
  aList_.add(aAttrValue_); 
  return aList_; 
}

void MSAttrValueList::add(const MSAttrValue& aAttrValue_)
{
  reserve(length());
  _array[length()]=aAttrValue_;
  _length++;
}

MSAttrValueList& MSAttrValueList::remove(unsigned index_)
{
  unsigned n=length();
  if (index_<n)
   {
     for (unsigned j=index_;j<n-1;j++) _array[j]=_array[j+1];
     _length--;
   }
  return *this;
}

MSAttrValueList& MSAttrValueList::remove(const MSIndexVector& aIndexVector_)
{
  if (aIndexVector_.length()>0) 
   {
     const MSIndexVector index=aIndexVector_.gradeUp();
     unsigned n=length();
     unsigned i,j,k;
     for (i=0,j=0,k=0;i<n;i++) 
      {
	if (j<index.length())
	 {
	   if (i!=aIndexVector_(index(j))) _array[k++]=_array[i]; 
	   else j++; 
	 }
	else _array[k++]=_array[i]; 
      }
     _length-=j;
   }
  return *this;
}

MSAttrValueList& MSAttrValueList::removeAll(void)
{
  delete [] _array;
  _array=0,_length=0,_size=0;
  return *this;
}

ostream& operator<<(ostream& aStream_,const MSAttrValue& aAttrValue_)
{ return aStream_<<aAttrValue_.attribute()<<"\t\t"<<aAttrValue_.value(); }

ostream& operator<<(ostream& aStream_,const MSAttrValueList& avList_)
{
  for (unsigned i=0;i<avList_.length();i++) aStream_<<avList_[i]<<endl;
  return aStream_;
}











