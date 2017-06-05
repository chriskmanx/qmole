#ifndef MSAttrValueHEADER
#define MSAttrValueHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSGUI/MSGUIEnum.H>

class MSDisplayServer;
#if HAVE_IOSTREAM
#include <iostream>
#else
class ostream;
using namespace std;
#endif

class MSGUIExport MSAttrValue
{
public:
  enum ValueType {Any           =0,
                  String        =0x0001, /* Generated code must use quotes. */
                  Char          =0x0002, /* Generated code must use single quotes. */
                  StringVector  =0x0004, /* Generated code must cast to StringVector. */
                  List          =0x0008, /* Multiple selection option */
                  Callback      =0x0010, /* Callback attribute */
                  ReadOnly      =0x0020, /* Ignored by prototyper, used by save/loadState */
                  Font          =0x0041, /* Font attribute. implies String */
                  Color         =0x0081, /* Color attribute. implies String */
                  ChildAttribute=0x0100, /* Parent's attribute applies to children */
                  Chooser1      =0x0200, /* Attribute which is a chooser in first arg */
                  Chooser2      =0x0400, /* Attribute which is a chooser in second arg */
                  Chooser       =0x0600, /* Chooser1|Chooser2 for masking. */
		  Control       =0x0800, /* This is control attribute don't save/load or display */
		  Reserved1     =0x1000, /* Reserved for future use */
		  Reserved2     =0x2000, /* Reserved for future use */
		  Reserved3     =0x4000,  /* Reserved for future use */
		  Reserved4     =0x8000  /* Reserved for future use */
};
  
  MSAttrValue(void);
  MSAttrValue(const MSAttrValue&);
  MSAttrValue(const char *attribute_,const char *value_,unsigned long valueType_=Any);
  MSAttrValue(const char *attribute_,const MSString& value_,unsigned long valueType_=Any);
  MSAttrValue(const MSString& attribute_,const char *value_,unsigned long valueType_=Any);
  MSAttrValue(const MSString& attribute_,const MSString& value_,unsigned long valueType_=Any);
  MSAttrValue(const MSString& attribute_,const MSString& value_,
	      const MSStringVector& options_,unsigned long valueType_=Any);

  MSAttrValue(const MSString& attribute_, const MSStringVector& values_,
	      const MSStringVector& options_, const MSStringVector& chooserOptions_);
  
  ~MSAttrValue(void);
  
  const MSString& attribute(void) const; 
  const MSString& value(void) const;
  const MSStringVector& options(void) const;
  unsigned long valueType(void) const;  
   
  MSAttrValue& operator=(const MSAttrValue&);
  friend MSGUIExport ostream& operator<<(ostream& aStream_,const MSAttrValue& aAttrValue_);

  static MSString alignmentToString(unsigned long);
  static unsigned long stringToAlignment(const MSString&);
  static MSString stringVectorToString(const MSStringVector&);
  static MSStringVector stringToStringVector(const MSString&);
  static MSString colorVectorToString(const MSUnsignedLongVector&, MSDisplayServer *);
  static MSString shadowStyleToString(MSShadowStyle);
  static MSShadowStyle stringToShadowStyle(const MSString &);
  static MSString lineStyleToString(MSLineStyle);
  static MSLineStyle stringToLineStyle(const MSString &);
  static MSString enumToString(unsigned long,const MSStringVector&,const MSUnsignedLongVector&,
			       const MSString& empty_="",MSBoolean exact_=MSFalse);
  static unsigned long stringToEnum(const MSString&,const MSStringVector&,const MSUnsignedLongVector&,
				    unsigned long empty_=0,MSBoolean exact_=MSFalse);
  
private:
  MSString       _attribute; 
  MSString       _value;
  MSStringVector _options;
  unsigned long  _valueType;
};

inline const MSString& MSAttrValue::attribute(void) const
{ return _attribute; }
inline const MSString& MSAttrValue::value(void) const
{ return _value; }
inline const MSStringVector& MSAttrValue::options(void) const
{ return _options; }
inline unsigned long MSAttrValue::valueType(void) const
{ return _valueType; }

class MSGUIExport MSAttrValueList
{
public:
  MSAttrValueList(void);
  ~MSAttrValueList(void);

  unsigned size(void) const;
  unsigned length(void) const;

  const MSAttrValue& operator[](unsigned i_) const;

  friend MSGUIExport MSAttrValueList& operator<<(MSAttrValueList&,const MSAttrValue&);

  MSAttrValueList& remove(unsigned);
  MSAttrValueList& remove(const MSIndexVector&);
  MSAttrValueList& removeAll(void);

  friend MSGUIExport ostream& operator<<(ostream& aStream_,const MSAttrValueList& avList_);

private:
  MSAttrValue *_array;
  unsigned     _size;
  unsigned     _length;
  
  void add(const MSAttrValue&);
  void reserve(unsigned);
};

inline unsigned MSAttrValueList::size(void) const 
{ return _size; }
inline unsigned MSAttrValueList::length(void) const 
{ return _length; }

inline const MSAttrValue& MSAttrValueList::operator[](unsigned index_) const
{ return _array[index_]; }

#endif
