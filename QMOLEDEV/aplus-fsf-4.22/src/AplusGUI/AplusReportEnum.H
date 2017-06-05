#ifndef AplusReportEnumHEADER
#define AplusReportEnumHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AplusGUI/AplusEnumConverter.H>

class AplusReportCompModeConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportCompModeConverter(void);
  inline ~AplusReportCompModeConverter(void);
};

inline AplusReportCompModeConverter::AplusReportCompModeConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusReportCompModeConverter::~AplusReportCompModeConverter(void) {}

class AplusReportOccurrenceConverter : public AplusHashEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportOccurrenceConverter(void);
  inline ~AplusReportOccurrenceConverter(void);
};

inline AplusReportOccurrenceConverter::AplusReportOccurrenceConverter(void) : AplusHashEnumConverter() {}
inline AplusReportOccurrenceConverter::~AplusReportOccurrenceConverter(void) {}

class AplusReportStyleConverter : public AplusHashEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportStyleConverter(void);
  inline ~AplusReportStyleConverter(void);
};

inline AplusReportStyleConverter::AplusReportStyleConverter(void) : AplusHashEnumConverter() {}
inline AplusReportStyleConverter::~AplusReportStyleConverter(void) {}

class AplusReportPageOrientationConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportPageOrientationConverter(void);
  inline ~AplusReportPageOrientationConverter(void);
};

inline AplusReportPageOrientationConverter::AplusReportPageOrientationConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusReportPageOrientationConverter::~AplusReportPageOrientationConverter(void) {}

class AplusReportPageSizeConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportPageSizeConverter(void);
  inline ~AplusReportPageSizeConverter(void);
};

inline AplusReportPageSizeConverter::AplusReportPageSizeConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusReportPageSizeConverter::~AplusReportPageSizeConverter(void) {}

class AplusReportOutputModeConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportOutputModeConverter(void);
  inline ~AplusReportOutputModeConverter(void);
};

inline AplusReportOutputModeConverter::AplusReportOutputModeConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusReportOutputModeConverter::~AplusReportOutputModeConverter(void) {}

class AplusReportDisclaimerStyleConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportDisclaimerStyleConverter(void);
  inline ~AplusReportDisclaimerStyleConverter(void);
};

inline AplusReportDisclaimerStyleConverter::AplusReportDisclaimerStyleConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusReportDisclaimerStyleConverter::~AplusReportDisclaimerStyleConverter(void) {}

class AplusReportPrintModeConverter : public AplusHashSingleEnumConverter
{
protected:
  virtual const AplusHashTable& enumTable(void) const;
  virtual const AplusHashTable& stringTable(void) const;
  virtual const char *type(void) const;
public:
  inline AplusReportPrintModeConverter(void);
  inline ~AplusReportPrintModeConverter(void);
};

inline AplusReportPrintModeConverter::AplusReportPrintModeConverter(void) : AplusHashSingleEnumConverter() {}
inline AplusReportPrintModeConverter::~AplusReportPrintModeConverter(void) {}

#endif //AplusReportEnumHEADER
