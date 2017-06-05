#ifndef MSApplicationHEADER
#define MSApplicationHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSStringVector.H>
#include <MSGUI/MSDisplayServer.H>
#include <MSIPC/MSMainLoop.H>

class MSApplication;
class ServerList;
class WidgetDestructionQueue;

// a custom mainloop for mstk applications
// basically we add in deletequeue processing
// which provides safe widget destruction in the
// face of client callbacks
class MSGUIExport MSApplicationMainLoop : public MSMainLoop
{
public:
  MSApplicationMainLoop(MSApplication *pApplication_);
  ~MSApplicationMainLoop(void);
protected:  
  MSApplication *_application;
  MSApplication *application(void);

  virtual MSBoolean flush(void);
  virtual void userProcessing(void);  
  virtual void terminateLoop(void);
};

class MSGUIExport MSApplication
{
friend class MSDisplayServer;
friend MSGUIExport MSBoolean applicationAddServer(MSDisplayServer *);
friend MSGUIExport MSBoolean applicationRemoveServer(MSDisplayServer *);  
public:
  MSApplication(void);  
  MSApplication(int argc_,char **argv_);
  virtual ~MSApplication(void);

  static void quit(void);
  static void flushAndProcess(MSBoolean blocking_=MSFalse);
  static MSBoolean flush(void);
  
  static void backingStoreOption(MSBoolean);
  static void continueLoop(MSBoolean);  
  static void doubleClickInterval(unsigned long);

  static MSApplication *application(void);
  static MSBoolean backingStoreOption(void);
  static MSBoolean continueLoop(void);
  static unsigned long doubleClickInterval(void);
  static MSMainLoop *mainLoop(void);
  static unsigned numberOfServerConnections(void);
  static MSDisplayServer *server(Display *);
  static MSString version(void);

  static void processWidgetDestructionQueue(void);
  static void allowWidgetDestruction(MSBoolean);
  static MSBoolean allowWidgetDestruction(void);
  static void addToWidgetDestructionQueue(MSWidget *);
  
  static const MSString& argument(unsigned);
  static const MSStringVector& argumentList(void);
  static MSString argumentString(void);
  
  virtual void loop(void);
  virtual void terminateLoop(void);
  
protected:
  static MSApplication          *_application;
  static MSMainLoop             *_mainLoop;
  static unsigned long           _doubleClickInterval;
  static MSBoolean               _backingStoreOption;  
  static WidgetDestructionQueue *_widgetDestructionQueue;
  static ServerList             *_serverList;
  static MSStringVector          _argumentList;

  void init(int argc_,char **argv_);
  
  static MSBoolean addServer(MSDisplayServer *);
  static MSBoolean removeServer(MSDisplayServer *);
#ifdef MS_WINDOWS
  static MSWidget *findWidget(Window);
#endif
};

#endif







