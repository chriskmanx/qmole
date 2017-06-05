///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSGUIEnum.H>
#include <MSGUI/MSApplication.H>
#include <MSGUI/MSWidget.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSTypes/MSMessageLog.H>

#define MStkVersion "%W%"

class WidgetDestructionQueue
{
public:
  MSUnsignedLongVector _widgetList;
  MSBoolean            _allowWidgetDestruction;
  MSBoolean            _processingQueue;

  WidgetDestructionQueue(void);
  ~WidgetDestructionQueue(void);  

  MSBoolean allowWidgetDestruction(void) const {return _allowWidgetDestruction;}
  void allowWidgetDestruction(MSBoolean allow_) {_allowWidgetDestruction=allow_;}

  void add(MSWidget *);
  MSBoolean processQueue(void);
};

class ServerList
{
public:
  MSUnsignedLongVector _serverList;
  MSBoolean            _destroyingList;

  ServerList(void);
  ~ServerList(void);  

  unsigned numberOfServers(void) const {return _serverList.length();}
  MSDisplayServer *serverFromDisplay(Display *);
  MSBoolean flushServers(void);
  MSBoolean add(MSDisplayServer *);
  MSBoolean remove(MSDisplayServer *);  
};

MSApplication          *MSApplication::_application=0;
MSMainLoop             *MSApplication::_mainLoop=0;
WidgetDestructionQueue *MSApplication::_widgetDestructionQueue=0;
ServerList             *MSApplication::_serverList=0;
unsigned long           MSApplication::_doubleClickInterval=250;
MSBoolean               MSApplication::_backingStoreOption=MSFalse;
MSStringVector          MSApplication::_argumentList;

WidgetDestructionQueue::WidgetDestructionQueue(void) :
_allowWidgetDestruction(MSFalse),
_processingQueue(MSFalse)
{}

WidgetDestructionQueue::~WidgetDestructionQueue(void)  
{}

// do not allow a widget to be placed on the list twice
void WidgetDestructionQueue::add(MSWidget *pWidget_)
{
  if (_widgetList.indexOf((unsigned long)pWidget_)==_widgetList.length())
   {
     _widgetList.append((unsigned long)pWidget_);
   }
}

// this method is not reentrant and thus a variable is used
// to protect it from being reentered.
// _processingQueue==MSTrue when inside the method.
// _allowWidgetDestruction is set to MSTrue to allow
// children of the widget to be deleted without being
// placed on the queue
MSBoolean WidgetDestructionQueue::processQueue(void)
{
  MSBoolean didWork=MSFalse;
  if (_processingQueue==MSFalse&&_widgetList.length()>0)
   {
     _processingQueue=MSTrue;
     _allowWidgetDestruction=MSTrue;
     // make a copy to prevent add from effecting the list
     // we must maintain the integrity of the list
     // during the processing loop.
     MSUnsignedLongVector widgetList=_widgetList;
     unsigned n=widgetList.length();
     _widgetList.removeAll();
     for (unsigned i=0;i<n;i++)
      {
        MSWidget *pWidget=(MSWidget *)widgetList(i);
        if (pWidget!=0)
         {
           delete pWidget;
           didWork=MSTrue;
         }
      }
     _allowWidgetDestruction=MSFalse;     
     _processingQueue=MSFalse;     
   }
  return didWork;
}

ServerList::ServerList(void) :
_destroyingList(MSFalse)
{}

ServerList::~ServerList(void)
{
  if (_serverList.length()>0)
   {
     _destroyingList=MSTrue;
     unsigned n=_serverList.length();
     for (unsigned i=0;i<n;i++)
      {
        MSDisplayServer *pServer=(MSDisplayServer *)_serverList(i);
        if (pServer!=0) delete pServer;
      }
     _serverList.removeAll();
   }
}

MSBoolean ServerList::flushServers(void)
{
  MSBoolean didWork=MSFalse;
  if (_serverList.length()>0)
   {
     unsigned n=_serverList.length();
     for (unsigned i=0;i<n;i++)
      {
        MSDisplayServer *pServer=(MSDisplayServer *)_serverList(i);
        if (pServer!=0&&pServer->flush()==MSTrue) didWork=MSTrue;
      }
   }
  return didWork;
}

MSDisplayServer *ServerList::serverFromDisplay(Display *pDisplay_)
{
  if (_serverList.length()>0)
   {
     unsigned n=_serverList.length();
     for (unsigned i=0;i<n;i++)
      {
        MSDisplayServer *pServer=(MSDisplayServer *)_serverList(i);
        if (pServer!=0&&pServer->display()==pDisplay_) return pServer;
      }
   }
  return 0;
}

// do not allow a widget to be placed on the list twice
MSBoolean ServerList::add(MSDisplayServer *pServer_)
{
  if (_serverList.indexOf((unsigned long)pServer_)==_serverList.length())
   {
     _serverList.append((unsigned long)pServer_);
     return MSTrue;
   }
  return MSFalse;
}

MSBoolean ServerList::remove(MSDisplayServer *pServer_)
{
  if (_destroyingList==MSFalse&&_serverList.length()>0)
   {
     unsigned index=_serverList.indexOf((unsigned long)pServer_);
     if (index<_serverList.length())
      {
        _serverList.removeAt(index);
        return MSTrue;
      }
   }
  return MSFalse;
}


MSApplicationMainLoop::MSApplicationMainLoop(MSApplication *pApplication_) :
_application(pApplication_)
{}

MSApplicationMainLoop::~MSApplicationMainLoop(void)
{}

MSApplication *MSApplicationMainLoop::application(void) 
{ return _application; }  

MSBoolean MSApplicationMainLoop::flush(void)
{ return _application->flush(); }

void MSApplicationMainLoop::userProcessing(void)
{ _application->processWidgetDestructionQueue(); }

void MSApplicationMainLoop::terminateLoop(void)  
{
  _application->allowWidgetDestruction(MSTrue);
  _application->terminateLoop();
}

MSApplication::MSApplication(void)
{
  init(0,0);
}

MSApplication::MSApplication(int argc_,char **argv_)
{
  init(argc_,argv_);
}

void MSApplication::init(int argc_,char **argv_)
{
  if (_application==0) 
   {
     _application=this;
     for (unsigned i=0;i<argc_;i++) _argumentList.append(argv_[i]);
   }
  if (_widgetDestructionQueue==0)
   {
     _widgetDestructionQueue=new WidgetDestructionQueue();
   }
  if (_serverList==0)
   {
     _serverList=new ServerList();
   }
}

// note: deleteQueue needs to be destroyed before the connection to the
//       server is broken-this will ensure that all server resources are
//       released properly without segv's (i.e. XFreeGC,which require a
//       valid display connection
MSApplication::~MSApplication(void)
{
  processWidgetDestructionQueue();

  if (_application==this)
   {
     if (_mainLoop!=0) delete _mainLoop;
     _mainLoop=0;
     _application=0;
   }
  // must delete serverList before widgetDestructionQueue
  // because deleting a server will force a  processQueue
  // of the widgetDestructionQueue
  if (_serverList!=0)             delete _serverList;
  if (_widgetDestructionQueue!=0) delete _widgetDestructionQueue;
  _widgetDestructionQueue=0;
  _serverList=0; 
}

void MSApplication::addToWidgetDestructionQueue(MSWidget *pWidget_)
{ _widgetDestructionQueue->add(pWidget_); }

void MSApplication::allowWidgetDestruction(MSBoolean allow_)
{ _widgetDestructionQueue->allowWidgetDestruction(allow_); }

MSBoolean MSApplication::allowWidgetDestruction(void)
{ return _widgetDestructionQueue->allowWidgetDestruction(); }
  
void MSApplication::processWidgetDestructionQueue(void)
{ _widgetDestructionQueue->processQueue(); }

// defaults to be overriden by subclasses
void MSApplication::loop(void)
{
  if (_mainLoop==0) _mainLoop=new MSApplicationMainLoop(this);  
  if (_application!=0&&_mainLoop!=0) _mainLoop->loop();
  else
   {
     MSMessageLog::criticalMessage("Exiting Application - need to create an MSApplication object\n");
     MSMessageLog::criticalMessage("exit(255) from MSApplication::loop() ...\n");
     exit(255);
  }
}

// allow the delete queue to delete everything - i.e. do not queue anymore widgets
void MSApplication::terminateLoop(void)
{ allowWidgetDestruction(MSTrue); }

void MSApplication::quit(void)
{ _application->continueLoop(MSFalse); }

MSString MSApplication::version(void)
{
  MSString aString(MStkVersion);
  unsigned index=aString.lastIndexOf(',');
  unsigned index2=aString.lastIndexOf(',',index-1);
  if (index2<aString.length()) return aString.subString(index2+6,index-index2-6);
  else return MSString("unknown");
}

MSString MSApplication::argumentString(void)
{
  const MSStringVector& aStringVector=_argumentList;
  MSString aString;
  for (unsigned i=0;i<aStringVector.length();i++) aString<<aStringVector(i)<<" ";
  return aString;
}

MSBoolean MSApplication::addServer(MSDisplayServer *pServer_)
{
  if (_serverList!=0&&pServer_!=0) return _serverList->add(pServer_);
  return MSFalse;
}

MSBoolean MSApplication::removeServer(MSDisplayServer *pServer_)
{
  if (_serverList!=0)
   {
     processWidgetDestructionQueue();
     return _serverList->remove(pServer_);
   }
  return MSFalse;
}

unsigned MSApplication::numberOfServerConnections(void)
{
  if (_serverList!=0) return _serverList->numberOfServers();
  return 0;
}

MSDisplayServer *MSApplication::server(Display *pDisplay_)
{
  if (_serverList!=0) return _serverList->serverFromDisplay(pDisplay_);
  return 0;
}

MSBoolean MSApplication::flush(void)
{
  if (_serverList!=0) return _serverList->flushServers();
  return MSFalse;
}

// ####################################################################
// extern methods
// ####################################################################

void applicationAddToWidgetDestructionQueue(MSWidget *pWidget_)
{ MSApplication::addToWidgetDestructionQueue(pWidget_); }
  
MSBoolean applicationAllowWidgetDestruction(void)
{ return MSApplication::allowWidgetDestruction(); }

unsigned long applicationDoubleClickInterval(void)
{ return MSApplication::doubleClickInterval(); }

MSBoolean applicationBackingStoreOption(void)
{ return MSApplication::backingStoreOption(); }

MSBoolean applicationAddServer(MSDisplayServer *pServer_)
{ return MSApplication::addServer(pServer_); }

MSBoolean applicationRemoveServer(MSDisplayServer *pServer_)
{ return MSApplication::removeServer(pServer_); }

MSDisplayServer *applicationDisplayServer(Display *display_)
{ return MSApplication::server(display_); }

MSString applicationArgumentString(void)
{ return MSApplication::argumentString(); }

MSString applicationVersionString(void)
{ return MSApplication::version(); }

void applicationQuit(void)
{ MSApplication::quit(); }

void applicationWarningMessage(const char *msg_)
{ MSMessageLog::warningMessage(msg_); }

void applicationExit(void)
{
  // try to exit cleanly - i.e. allow app to terminate normally
  if (MSApplication::application()!=0)
   {
     MSApplication::application()->terminateLoop();
   }
  exit(255);
}

// ####################################################################
// inline methods
// ####################################################################

unsigned long MSApplication::doubleClickInterval(void)
{ return _doubleClickInterval; }

void MSApplication::doubleClickInterval(unsigned long doubleClickInterval_)
{ _doubleClickInterval=doubleClickInterval_; }

MSBoolean MSApplication::backingStoreOption(void)
{ return _backingStoreOption; }

void MSApplication::backingStoreOption(MSBoolean backingStoreOption_)
{ _backingStoreOption=backingStoreOption_; }

MSApplication *MSApplication::application(void)
{ return _application; }

MSMainLoop *MSApplication::mainLoop(void)
{ return _mainLoop; }

const MSStringVector& MSApplication::argumentList(void)
{ return _argumentList; }

const MSString& MSApplication::argument(unsigned index_)
{ return _argumentList(index_); }

void MSApplication::flushAndProcess(MSBoolean blocking_)
{ if (_mainLoop!=0) _mainLoop->flushAndProcess(blocking_); }

MSBoolean MSApplication::continueLoop(void)
{ return (_mainLoop!=0)?_mainLoop->continueLoop():MSFalse; }

void MSApplication::continueLoop(MSBoolean continueLoop_)
{ if (_mainLoop!=0) _mainLoop->continueLoop(continueLoop_); }


#ifdef MS_WINDOWS
MSWidget* MSApplication::findWidget(Window win_)
{
  MSWidget *pWidget=0;
  MSHashTable *hashTable=0;
  for(unsigned i=0;i<_serverList->_serverList.length();i++)
    {
      hashTable=((MSDisplayServer*)_serverList->_serverList(i))->widgetHashTable();
      pWidget=(MSWidget *)hashTable->lookup(win_);
      if((unsigned long)pWidget!=hashTable->notFound()) break;
    }
  return pWidget;
}

#endif



