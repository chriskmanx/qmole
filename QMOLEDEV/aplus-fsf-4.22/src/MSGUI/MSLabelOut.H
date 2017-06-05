#ifndef MSLabelOutHEADER
#define MSLabelOutHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSFormat.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSUnsignedVector.H>
#include <MSTypes/MSManagedPointer.H>
#include <MSGUI/MSWidget.H>

class MSGUIExport MSLabelOut
{
private:
  MSWidget        *_owner;
  MSFloatVector    _tickPositions;
  MSFloatVector    _tickSizes;
  MSUnsignedVector _gridWidths;
  MSStringVector   _labels;
  MSFormat         _format;

protected:
  MSWidget *owner(void)                    {return _owner;} 

public:
  MSLabelOut(void);
  MSLabelOut(MSAlignment);
  MSLabelOut(const MSStringVector&);
  MSLabelOut(const MSFormat&);
  ~MSLabelOut(void);

  void init(void);
  const MSWidget *owner(void)              const {return _owner;} 
  const MSFormat& format(void)             const {return _format;} 
  const MSFloatVector& tickPositions(void) const {return _tickPositions;} 
  const MSFloatVector& tickSizes(void)     const {return _tickSizes;} 
  const MSUnsignedVector& gridWidths(void) const {return _gridWidths;} 
  const MSStringVector& labels(void)       const {return _labels;} 
  void owner(MSWidget *x_)                       {_owner=x_;} 
  void tickPositions(const MSFloatVector& x_)    {_tickPositions=x_;} 
  void tickSizes(const MSFloatVector& x_)        {_tickSizes=x_;} 
  void gridWidths(const MSUnsignedVector& x_)    {_gridWidths=x_;} 
  void labels(const MSStringVector& x_)          {_labels=x_;} 
  void format(const MSFormat&);
  virtual unsigned tickPositionLength(void) const;
  virtual unsigned gridWidthLength(void) const;
  virtual unsigned tickSizeLength(void) const;
  virtual unsigned labelLength(void) const;
  virtual double tickPosition(unsigned) const;
  virtual double tickSize(unsigned) const;
  virtual unsigned gridWidth(unsigned) const;
  virtual const char *formatOutput(MSString &,double);
  virtual const char *label(MSString &,unsigned) const;

  MSFormat::MSFormatType formatType(void)   const {return _format.formatType();}
};

inline unsigned MSLabelOut::tickPositionLength(void) const {return tickPositions().length();}
inline unsigned MSLabelOut::tickSizeLength(void)     const {return tickSizes().length();}
inline unsigned MSLabelOut::gridWidthLength(void)    const {return gridWidths().length();}
inline unsigned MSLabelOut::labelLength(void)        const {return labels().length();}

typedef MSManagedPointer<MSLabelOut> MSLabelOutPtr;

#endif

