#ifndef MSViewHEADER
#define MSViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>
#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSModel.H>
#include <MSTypes/MSIndexVector.H>

class MSGUIExport MSView : public MSEventReceiver
{
friend class MSModel;
protected:
  MSModel   *_model;
  MSBoolean  _internalModel;
  MSBoolean  _readOnlyModel;
  
  MSBoolean internalModel(void) const;
  void internalModel(MSBoolean);

  void readOnlyModel(MSBoolean);
  MSBoolean readOnlyModel(void) const;

  virtual void addSenderNotify(MSEventSender *);
  virtual void removeSenderNotify(MSEventSender *);

  virtual void decouple(void);    // internal notification that this view is no longer dependent on model
  virtual void couple(MSModel *); // internal notification that this view is now dependent on model
  virtual void constCouple(const MSModel*); //binding to a "const model"
  virtual void internalCouple(MSModel *); // couple as an internal model, i.e. delete on decouple

  virtual void updateData(void);
  virtual void update(const MSIndexVector&);
  virtual void receiveEvent(MSEvent&);

public:
  MSView(void);
  ~MSView(void);

  MSModel *model(void);
  const MSModel *model(void) const;

  virtual MSBoolean hasModel(void) const;
  MSBoolean isReadOnlyModel(void) const;

  void decoupleModel(void);
};

inline MSBoolean MSView::internalModel(void) const
{ return _internalModel; }
inline void MSView::internalModel(MSBoolean b_) 
{ _internalModel=b_; }

inline MSBoolean MSView::readOnlyModel(void) const
{ return _readOnlyModel; }
inline void MSView::readOnlyModel(MSBoolean b_) 
{ _readOnlyModel=b_; }

inline MSModel *MSView::model(void)
{ return _model; }
inline const MSModel *MSView::model(void) const 
{ return _model; }

#endif



