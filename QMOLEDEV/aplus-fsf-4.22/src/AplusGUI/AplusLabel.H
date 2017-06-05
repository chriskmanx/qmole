#ifndef AplusLabelHEADER
#define AplusLabelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSLabel.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusModel.H>

class  AplusLabel : public MSLabel
{
public:
  AplusLabel(MSWidget *);
  ~AplusLabel(void);
  virtual MSBoolean verifyData(V,A);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual AplusModel *model(void) const { return (AplusModel *)_model; }
  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);
  virtual int numRows(void) const;
  virtual int numColumns(void) const;
  virtual int numColumns(int) const;
  virtual const char *formatOutput(MSString&,int);
  //
  // drawRow() is overriden in order to support functional foreground
  //
  virtual void drawRow(int);
  virtual void drawRow(int,int,const char *,int);
  virtual unsigned long rowForeground(int) const;
  STANDARD_WIDGET_METHODS_H
};

#endif // AplusLabelHEADER
