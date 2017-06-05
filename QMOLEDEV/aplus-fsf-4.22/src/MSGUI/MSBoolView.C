///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1999-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSBoolView.H>


MSBoolView::MSBoolView(void)
{
  _internalBoolModel=MSFalse;
  _boolModel=&_internalBoolModel;
  _internalBoolModel.addReceiver(this);
}

MSBoolView::~MSBoolView(void)
{}

void MSBoolView::boolModel(MSBool &model_)
{
  if (_boolModel!=&model_)
   {
     _boolModel->removeReceiver(this);
     _boolModel=&model_;
     _boolModel->addReceiver(this);
   }
  newBoolModelNotify();
}

void MSBoolView::newBoolModelNotify(void)
{}


MSBool &MSBoolView::boolModel(void)
{ return (MSBool &) *_boolModel; }


void MSBoolView::receiveEvent(MSEvent& aEvent_)
{
  boolModelChanged(aEvent_);
}

void MSBoolView::removeSenderNotify(MSEventSender * aEventSender_)
{
// if the model gets removed, then we just reset to the internal one.
  if ( aEventSender_ != &_internalBoolModel) _boolModel=&_internalBoolModel;
}

void MSBoolView::boolModelChanged(MSEvent&)
{}



