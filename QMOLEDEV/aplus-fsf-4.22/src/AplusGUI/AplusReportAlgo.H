#ifndef __AplusReportAlgo_H
#define __AplusReportAlgo_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSFloatVector.H>
#include "AplusCommon.H"


class AplusReportAlgorithm
{
public:
  enum {Sum=1, Min=2, Max=4, Avg=8, StdDev=16, Variance=32} compMode;
  virtual double computeValue(A, int begin_=-1, int end_=-1) = 0;
protected:
  AplusReportAlgorithm(void);
  ~AplusReportAlgorithm(void);
  virtual MSFloatVector convert(A, int, int);
};

class AplusReportSumAlgorithm : public AplusReportAlgorithm
{
public:
  static AplusReportSumAlgorithm *reportSum(void);
protected:
  AplusReportSumAlgorithm(void);
  ~AplusReportSumAlgorithm(void);
  virtual double computeValue(A, int begin_=-1, int end_=-1);
private:
  static AplusReportSumAlgorithm *_reportSum;
};

class AplusReportMaxAlgorithm : public AplusReportAlgorithm
{
public:
  static AplusReportMaxAlgorithm *reportMax(void);
protected:
  AplusReportMaxAlgorithm(void);
  ~AplusReportMaxAlgorithm(void);
  virtual double computeValue(A, int begin_=-1, int end_=-1);
private:
  static AplusReportMaxAlgorithm *_reportMax;
};

class AplusReportMinAlgorithm : public AplusReportAlgorithm
{
public:
  static AplusReportMinAlgorithm *reportMin(void);
protected:
  AplusReportMinAlgorithm(void);
  ~AplusReportMinAlgorithm(void);
  virtual double computeValue(A, int begin_=-1, int end_=-1);
private:
  static AplusReportMinAlgorithm *_reportMin;
};

class AplusReportAvgAlgorithm : public AplusReportAlgorithm
{
public:
  static AplusReportAvgAlgorithm *reportAvg(void);
protected:
  AplusReportAvgAlgorithm(void);
  ~AplusReportAvgAlgorithm(void);
  virtual double computeValue(A, int begin_=-1, int end_=-1);
private:
  static AplusReportAvgAlgorithm *_reportAvg;
};

class AplusReportStdDevAlgorithm : public AplusReportAlgorithm
{
public:
  static AplusReportStdDevAlgorithm *reportStdDev(void);
protected:
  AplusReportStdDevAlgorithm(void);
  ~AplusReportStdDevAlgorithm(void);
  virtual double computeValue(A, int begin_=-1, int end_=-1);
private:
  static AplusReportStdDevAlgorithm *_reportStdDev;
};

class AplusReportVarianceAlgorithm : public AplusReportAlgorithm
{
public:
  static AplusReportVarianceAlgorithm *reportVariance(void);
protected:
  AplusReportVarianceAlgorithm(void);
  ~AplusReportVarianceAlgorithm(void);
  virtual double computeValue(A, int begin_=-1, int end_=-1);
private:
  static AplusReportVarianceAlgorithm *_reportVariance;
};

class AplusReportMedianAlgorithm : public AplusReportAlgorithm
{
public:
  static AplusReportMedianAlgorithm *reportMedian(void);
protected:
  AplusReportMedianAlgorithm(void);
  ~AplusReportMedianAlgorithm(void);
  virtual double computeValue(A, int begin_=-1, int end_=-1);
private:
  static AplusReportMedianAlgorithm *_reportMedian;
};

#endif
