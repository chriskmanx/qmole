#ifndef AplusEnumConverterHEADER
#define AplusEnumConverterHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <AplusGUI/AplusHashTable.H>

class AplusEnumConverter
{
protected:
  virtual unsigned long enumLookup(const char *) const =0;
  virtual const char *stringLookup(unsigned long) const =0;

  virtual const char *type(void) const =0;

public:
  inline AplusEnumConverter(void);
  inline ~AplusEnumConverter(void);

  virtual unsigned long convert(A) const;
  virtual A convert(unsigned long) const;

  inline unsigned long operator()(A);
  inline A operator()(unsigned long);

  virtual unsigned long enumNotFound(void) const =0;
  virtual const char *stringNotFound(void) const =0;
};

inline AplusEnumConverter::AplusEnumConverter(void) {}
inline AplusEnumConverter::~AplusEnumConverter(void){}

inline unsigned long AplusEnumConverter::operator()(A symbols_) { return convert(symbols_); }
inline A AplusEnumConverter::operator()(unsigned long enums_)   { return convert(enums_); }


class AplusHashEnumConverter : public AplusEnumConverter
{
protected:
  virtual unsigned long enumLookup(const char *) const;
  virtual const char *stringLookup(unsigned long) const;

  virtual const AplusHashTable& enumTable(void) const =0;
  virtual const AplusHashTable& stringTable(void) const =0;

public:
  inline AplusHashEnumConverter(void);
  inline ~AplusHashEnumConverter(void);

  virtual unsigned long enumNotFound(void) const;
  virtual const char *stringNotFound(void) const;

  inline A stringDomain(void) const;
};

inline AplusHashEnumConverter::AplusHashEnumConverter(void) : AplusEnumConverter() {}
inline AplusHashEnumConverter::~AplusHashEnumConverter(void) {}
inline A AplusHashEnumConverter::stringDomain(void) const { return stringTable().listAllEntries(); }


class AplusHashSingleEnumConverter : public AplusHashEnumConverter
{
public:
  inline AplusHashSingleEnumConverter(void);
  inline ~AplusHashSingleEnumConverter(void);

  virtual unsigned long convert(A) const;
  virtual A convert(unsigned long) const;

  unsigned long fromString(const char *) const;
  const char *toString(unsigned long) const;
};
  
inline AplusHashSingleEnumConverter::AplusHashSingleEnumConverter(void) : AplusHashEnumConverter() {}
inline AplusHashSingleEnumConverter::~AplusHashSingleEnumConverter(void) {}


class AplusShadowStyleConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;

public:
  inline AplusShadowStyleConverter(void);
  inline ~AplusShadowStyleConverter(void);
};

inline AplusShadowStyleConverter::AplusShadowStyleConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusShadowStyleConverter::~AplusShadowStyleConverter(void) {}


class AplusAlignmentConverter : public AplusHashEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;

public:
  inline AplusAlignmentConverter(void);
  inline ~AplusAlignmentConverter(void);
};

inline AplusAlignmentConverter::AplusAlignmentConverter(void) : AplusHashEnumConverter() {}
inline AplusAlignmentConverter::~AplusAlignmentConverter(void) {}


class AplusLineStyleConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;

public:
  inline AplusLineStyleConverter(void);
  inline ~AplusLineStyleConverter(void);
};

inline AplusLineStyleConverter::AplusLineStyleConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusLineStyleConverter::~AplusLineStyleConverter(void) {}

#endif //AplusEnumConverterHEADER
