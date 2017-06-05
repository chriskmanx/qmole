///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSView.H>
#include <MSTypes/MSNullEvent.H>
#include <MSTypes/MSIndexedEvent.H>

MSView::MSView(void) 
{ _model=0,_internalModel=MSFalse; _readOnlyModel=MSFalse; }
MSView::~MSView(void) 
{ decouple(); }

void MSView::couple(MSModel *model_)
{
  if (model_!=model())
   {
     decouple();
     _model=model_;
     if (model()!=0) addSender(model_);
     updateData(); // notify the view that it has been coupled to the model
   }
}

void MSView::decouple(void)
{
  if (model()!=0)
   { 
     MSModel *m=model();
     _model=0;         // prevent removeSenderNotify from doing additional work - i.e. infinite loop
     removeSender(m);
     if (internalModel()==MSTrue) delete m;
     internalModel(MSFalse);
     readOnlyModel(MSFalse);
   }
}

// this function is used when it is desired that the model be deleted 
// when this view is decoupled
void MSView::internalCouple(MSModel *model_)
{
  if (model_!=model())
   {
     couple(model_);
     internalModel(MSTrue);
   }
}

// this function is used when it is desired that the model be const
// and thus readOnly
void MSView::constCouple(const MSModel *model_)
{
  if (model_!=model())
   {
     //we are casting away const for internal purposes.
     //Derive class MUST honor the readOnlyModel flag.
     couple((MSModel*)model_);
     readOnlyModel(MSTrue);
   }
}

// called from MSEventSender::addReceiver and not as a result of MSView::couple
// thus we will blow it off, downcast is unsafe
void MSView::addSenderNotify(MSEventSender *) {}

// called from MSModel::removeReceiver
// only concerned when MSModel::removeReceiver is not as a result of MSView::decouple
// MSEventReceiver::removeSender will be called when ~MSModel is called
void MSView::removeSenderNotify(MSEventSender *model_) 
{
  if (model()!=0&&model()==model_)
   {
     _model=0;
     updateData();
   }
}

void MSView::receiveEvent(MSEvent& aEvent_)
{
  if (aEvent_.type()==MSIndexedEvent::symbol())
   {
     MSIndexedEvent &ev=(MSIndexedEvent&)aEvent_;
     update(ev.index());
   }
  else if (aEvent_.type()==MSNullEvent::symbol()) update(MSIndexVector::nullVector());
}

void MSView::decoupleModel(void)
{ if(internalModel()==MSFalse) couple(0); }

MSBoolean MSView::hasModel(void) const
{ return (_model!=0)?MSTrue:MSFalse; }

MSBoolean MSView::isReadOnlyModel(void) const
{ return _readOnlyModel; }


// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSView::updateData(void) {}
void MSView::update(const MSIndexVector&) {}




