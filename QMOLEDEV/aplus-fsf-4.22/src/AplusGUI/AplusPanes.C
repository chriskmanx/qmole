///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusPanes.H>

extern long dbg_tmstk;

#define PANES_MACROS(PANETYPE, PANEPARENTTYPE, PANENAME) \
\
PANETYPE::PANETYPE(MSWidget *xwin_) : PANEPARENTTYPE(xwin_)\
{\
}\
\
\
PANETYPE::~PANETYPE(void)\
{}\
\
void PANETYPE::addSenderNotify(MSEventSender *m_)\
{\
  INTERNAL_COUPLE(((AplusModel *) m_));\
}\
\
void PANETYPE::receiveEvent(MSEvent &event_)\
{\
  if (event_.type()==MSIndexedEvent::symbol())\
   {\
     MSIndexedEvent &ev=(MSIndexedEvent&)event_;\
     update(ev.index());\
   }\
  else if (event_.type()==MSNullEvent::symbol())\
    update(MSIndexVector::nullVector());\
  else if (event_.type() == AplusEvent::symbol())\
   {\
    if (dbg_tmstk) cout << "Received UpdateEvent in " << PANENAME  << endl; \
    redraw();\
   }\
  if (event_.type() == AplusVerifyEvent::symbol())\
   {\
    if (dbg_tmstk) cout << "Received VerifyEvent in PANETYPE"  << endl;\
     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;\
     ave->result(verifyData(ave->aplusVar(), ave->a()));\
   }\
}\
\
const MSSymbol& PANETYPE::widgetType(void) const\
{\
  return symbol();\
}\
\
\
const MSSymbol& PANETYPE::symbol(void)\
{\
  static MSSymbol sym(#PANETYPE);\
  return sym;\
}\


PANES_MACROS(AplusPane, MSPane, "Pane")

PANES_MACROS(AplusVPane, MSVPane, "VPane")

PANES_MACROS(AplusHPane, MSHPane, "HPane")

