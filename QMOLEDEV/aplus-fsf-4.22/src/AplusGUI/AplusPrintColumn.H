#ifndef AplusPrintColumnHEADER
#define AplusPrintColumnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSTime.H>
#include <a/k.h>
#include <MSGUI/MSWidgetView.H>
#include <MSGUI/MSPrintColumn.H>
#include <AplusGUI/Macros.H>

class AplusPrintColumn : public MSWidgetView, public MSPrintColumn
{
public:
  AplusPrintColumn(MSWidget *);
  ~AplusPrintColumn(void);

  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);

  static MSBoolean verifyData(V,::A);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  void constructColumn(void);

  inline AplusModel *model(void);
  inline const AplusModel *model(void) const;

  STANDARD_WIDGET_METHODS_H
};

inline AplusModel *AplusPrintColumn::model(void) { return (AplusModel *)_model; }
inline const AplusModel *AplusPrintColumn::model(void) const { return (const AplusModel *)_model; }

#endif //AplusPrintColumnHEADER
