#ifndef AplusNotebookHEADER
#define AplusNotebookHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSNotebook.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class AplusNotebook : public MSNotebook
{
public:
  AplusNotebook(MSWidget *);
  ~AplusNotebook(void);
  virtual void addSenderNotify(MSEventSender *);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A) { return MSTrue; }

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  STANDARD_WIDGET_METHODS_H
};

#endif //AplusNotebookHEADER
