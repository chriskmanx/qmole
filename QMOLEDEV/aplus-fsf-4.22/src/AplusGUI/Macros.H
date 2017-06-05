#ifndef MacrosHEADER
#define MacrosHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AplusGUI/AplusCommon.H>

#define INTERNAL_COUPLE(X) \
{ \
  if (X != (AplusModel *)_model)\
    {\
     if (model()!=0)  \
       { \
	 MSModel *tmp=_model; \
         if (_model->type()==AplusModel::symbol()) \
	 { \
	   X->sflags(((AplusModel*)model())->sflags()); \
	 } \
	 \
	 _model=X; \
	 delete tmp;\
       } \
     _model = X; \
     _model->addReceiver(this); \
     updateData(); \
   } \
}

#define STANDARD_WIDGET_METHODS_H \
public: \
private: \
protected: \
virtual void decoupleWidget(void)\
{\
   MSModel *m=_model;\
   _model=0;\
   delete m;\
}

//   decouple();

#endif  //MacrosHEADER
