///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPopup.H>

MSPopup::MSPopup(const char *windowTitle_) :
MSShell(windowTitle_) 
{ init(); }

MSPopup::MSPopup(MSDisplayServer *server_,const char *windowTitle_) :
MSShell(server_,windowTitle_) 
{ init(); }

MSPopup::MSPopup(MSShell *leader_,const char *windowTitle_) :
MSShell(leader_->server(),windowTitle_) 
{
  init();
  windowGroup(leader_);
  foreground(leader_->foreground());
  background(leader_->background());
  font(leader_->font());
}

MSPopup::~MSPopup(void) 
{ 
  if (mapped()==MSTrue&&modal()==MSTrue) 
   {
     server()->removePassiveGrab(this); 
     changeBusyState(MSFalse);
   }
}

void MSPopup::init(void)
{
  pushPinState(MSFalse);
  setWinAttr();
  footer(MSFalse);
  _modal=MSFalse;
}

void MSPopup::pinIn(void)  
{ 
  pushPinState(MSTrue); 
  setWinAttr(); 
}

void MSPopup::pinOut(void) 
{ 
  pushPinState(MSFalse); 
  setWinAttr(); 
}

void MSPopup::map(void)
{
  if (mapped()==MSFalse)
   {
     if (modal()==MSTrue) 
      {
	server()->addPassiveGrab(this);
	changeBusyState(MSTrue);
	changeBusyState(this,MSFalse);
	MSShell::map();

#ifndef MS_MAPNOTIFY_ON_MAP_BUG
	// Waiting for MapNotify Event	
	while (1)
	  {
	    XEvent ev;
	    XPeekEvent(display(),&ev);
	    server()->processOneEvent();
	    if (ev.type==MapNotify&&ev.xmap.window==window())
	      {
		server()->flush();
		return;
	      }
	  }
#endif      
      }
     else MSShell::map();
   }
}

void MSPopup::unmap(void) 
{ 
  if (mapped()==MSTrue)
   {
     unmapFollowers(); 
     if (pushPinState()==MSFalse)
      {
	if (modal()==MSTrue) 
	 {
	   server()->removePassiveGrab(this);
	   changeBusyState(MSFalse);
	 }
	MSWidget::unmap();
	XWithdrawWindow(display(),window(),server()->screenNum());
      }
   }
}

void MSPopup::setWinAttr(void)
{
  if (server()->isCDERunning()==MSFalse)
   {
     //This code is specific to OLWM
     struct {Atom win;Atom menu;Atom pin;} props;
     
     Atom winType=server()->atom(MSAtomTable::WinAttr);
     props.win=server()->atom(MSAtomTable::WTCmd);
     props.menu=server()->atom(MSAtomTable::MenuLimited);
     props.pin=server()->atom((pushPinState()==MSTrue)?MSAtomTable::PinIn:MSAtomTable::PinOut);
     XChangeProperty(display(),window(),winType,winType,32,
                     PropModeReplace,(unsigned char *)&props,3);
   }
  else
   {
     if(firstMap()==MSFalse)
      {
        removeWMDecorations(IconifyButton|MaximizeButton);
        removeWMFunctions(Minimize|Maximize);
      }
   }
}

  
void MSPopup::dismiss(void) 
{ if (activateCallback(MSWidgetCallback::dismiss)==MSFalse) hide(); }

void MSPopup::clientMessage(const XEvent *event_)
{
  if (event_->xclient.message_type==server()->atom(MSAtomTable::WMProtocols))
   {
     if (event_->xclient.data.l[0]==server()->atom(MSAtomTable::WMDeleteWindow))
      {
        pushPinState(MSFalse);
        dismiss();
      }
   }
}

void MSPopup::propertyNotify(const XEvent *event_)
{
  Atom prop=server()->atom(MSAtomTable::PushpinState);
  if (event_->xproperty.atom==prop)
   {
     Atom actualTarget;
     int actualFormat;
     int status;
     unsigned long itemCount;
     unsigned long bytesRemaining;
     unsigned char *actData;
     int *data=0;
     long len=(sizeof(int)+3)>>2;
     
     status=XGetWindowProperty(display(),window(),prop,
				 0L,len,False,XA_INTEGER,
				 &actualTarget,&actualFormat,
				 &itemCount,&bytesRemaining,&actData);
     
     if (status==Success&&actualTarget==XA_INTEGER&& 
         actualFormat==propertyFormat(prop)&&itemCount>0)
      {
        data=(int *)actData;
        pushPinState((MSBoolean)data[0]);
      }  
     if (data!=0) XFree((char *)actData);
   }
  else 
   {
     MSShell::propertyNotify(event_);
   }   
}

void MSPopup::modal(MSBoolean modal_)
{
  if (_modal!=modal_)
   {
     _modal=modal_;
     if (modal()==MSTrue)
      {
	if(leader()!=0) transientFor(leader());
      }
     else transientFor(0);

     if (mapped()==MSTrue)
      {
	if (modal()==MSTrue) 
	 {
	   server()->addPassiveGrab(this);
	   changeBusyState(MSTrue);
	   changeBusyState(this,MSFalse);
	 }
	else
	 {  
	   server()->removePassiveGrab(this);
	   changeBusyState(MSFalse);
	 }
      }
   }
}



void MSPopup::transientFor(MSShell *shell_)
{ XSetTransientForHint(display(),window(),shell_==0?server()->root():shell_->window()); }

void MSPopup::set(MSAttrValueList& avList_)
{
  MSShell::set(avList_);
}


MSAttrValueList& MSPopup::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("dismiss","",MSAttrValue::Callback);
  MSShell::get(avList_);
  //We need to remove "quit" and "saveyourself" as we overide clientMessage
  //and never generate these.
  MSIndexVector index;
  for(int i=0;index.length()<2&&i<avList_.length();i++)
   {
     if(avList_[i].valueType()==MSAttrValue::Callback&&
        (avList_[i].attribute()=="quit"||avList_[i].attribute()=="saveyourself"))
      {
        index<<i;
      }
   }
  avList_.remove(index);
  return avList_;
}
