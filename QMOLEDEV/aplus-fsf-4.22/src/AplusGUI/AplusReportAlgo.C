///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include "AplusReportAlgo.H"
#include <MSTypes/MSFloatVector.H>

AplusReportSumAlgorithm *AplusReportSumAlgorithm::_reportSum=0;
AplusReportMaxAlgorithm *AplusReportMaxAlgorithm::_reportMax=0;
AplusReportMinAlgorithm *AplusReportMinAlgorithm::_reportMin=0;
AplusReportAvgAlgorithm *AplusReportAvgAlgorithm::_reportAvg=0;
AplusReportStdDevAlgorithm *AplusReportStdDevAlgorithm::_reportStdDev=0;
AplusReportVarianceAlgorithm *AplusReportVarianceAlgorithm::_reportVariance=0;
AplusReportMedianAlgorithm *AplusReportMedianAlgorithm::_reportMedian=0;

AplusReportAlgorithm::AplusReportAlgorithm(void) {}
AplusReportAlgorithm::~AplusReportAlgorithm(void) {}
MSFloatVector AplusReportAlgorithm::convert(A vals_, int begin_, int end_)
{
  MSFloatVector result;
  if (QA(vals_)&&(vals_->t==It || vals_->t==Ft)&&vals_->n>0)
   {
     P p; p.i = vals_->p;
     if (begin_==-1||end_==-1)
      {
	begin_=0;
	end_=vals_->n;
      }
     for (int j=begin_; j<end_;j++)
      {
	switch(vals_->t)
	 {
	 case It:
	   result << (double) p.i[j];
	   break;
	 case Ft:
	   result << p.f[j];
	   break;
	 }
      }
   }
  return result;
}

AplusReportSumAlgorithm::AplusReportSumAlgorithm(void) {}
AplusReportSumAlgorithm::~AplusReportSumAlgorithm(void) { if (_reportSum!=0) delete _reportSum; }
double AplusReportSumAlgorithm::computeValue(A vals_, int begin_, int end_)
{
  MSFloatVector fv = convert(vals_,begin_,end_);
  return fv.sum();
}

AplusReportSumAlgorithm *AplusReportSumAlgorithm::reportSum(void)
{
  if (_reportSum == 0) _reportSum = new AplusReportSumAlgorithm;
  return _reportSum;
}

AplusReportMaxAlgorithm::AplusReportMaxAlgorithm(void) {}
AplusReportMaxAlgorithm::~AplusReportMaxAlgorithm(void) { if (_reportMax!=0) delete _reportMax; }
double AplusReportMaxAlgorithm::computeValue(A vals_, int begin_, int end_)
{
  MSFloatVector fv = convert(vals_,begin_,end_);
  return fv.max();
}

AplusReportMaxAlgorithm *AplusReportMaxAlgorithm::reportMax(void)
{
  if (_reportMax == 0) _reportMax = new AplusReportMaxAlgorithm;
  return _reportMax;
}

AplusReportMinAlgorithm::AplusReportMinAlgorithm(void) {}
AplusReportMinAlgorithm::~AplusReportMinAlgorithm(void) { if (_reportMin!=0) delete _reportMin; }
double AplusReportMinAlgorithm::computeValue(A vals_, int begin_, int end_)
{
  MSFloatVector fv = convert(vals_,begin_,end_);
  return fv.min();
}

AplusReportMinAlgorithm *AplusReportMinAlgorithm::reportMin(void)
{
  if (_reportMin == 0) _reportMin = new AplusReportMinAlgorithm;
  return _reportMin;
}

AplusReportAvgAlgorithm::AplusReportAvgAlgorithm(void) {}
AplusReportAvgAlgorithm::~AplusReportAvgAlgorithm(void) { if (_reportAvg!=0) delete _reportAvg; }
double AplusReportAvgAlgorithm::computeValue(A vals_, int begin_, int end_)
{
  MSFloatVector fv = convert(vals_,begin_,end_);
  return fv.avg();
}

AplusReportAvgAlgorithm *AplusReportAvgAlgorithm::reportAvg(void)
{
  if (_reportAvg == 0) _reportAvg = new AplusReportAvgAlgorithm;
  return _reportAvg;
}

AplusReportStdDevAlgorithm::AplusReportStdDevAlgorithm(void) {}
AplusReportStdDevAlgorithm::~AplusReportStdDevAlgorithm(void) { if (_reportStdDev!=0) delete _reportStdDev; }
double AplusReportStdDevAlgorithm::computeValue(A vals_, int begin_, int end_)
{
  MSFloatVector fv = convert(vals_,begin_,end_);
  return fv.stdDeviation();
}

AplusReportStdDevAlgorithm *AplusReportStdDevAlgorithm::reportStdDev(void)
{
  if (_reportStdDev == 0) _reportStdDev = new AplusReportStdDevAlgorithm;
  return _reportStdDev;
}

AplusReportVarianceAlgorithm::AplusReportVarianceAlgorithm(void) {}
AplusReportVarianceAlgorithm::~AplusReportVarianceAlgorithm(void) { if (_reportVariance!=0) delete _reportVariance; }
double AplusReportVarianceAlgorithm::computeValue(A vals_, int begin_, int end_)
{
  MSFloatVector fv = convert(vals_,begin_,end_);
  return fv.variance();
}

AplusReportVarianceAlgorithm *AplusReportVarianceAlgorithm::reportVariance(void)
{
  if (_reportVariance == 0) _reportVariance = new AplusReportVarianceAlgorithm;
  return _reportVariance;
}

AplusReportMedianAlgorithm::AplusReportMedianAlgorithm(void) {}
AplusReportMedianAlgorithm::~AplusReportMedianAlgorithm(void) { if (_reportMedian!=0) delete _reportMedian; }
double AplusReportMedianAlgorithm::computeValue(A vals_, int begin_, int end_)
{
  MSFloatVector fv = convert(vals_,begin_,end_);
  return fv.median();
}

AplusReportMedianAlgorithm *AplusReportMedianAlgorithm::reportMedian(void)
{
  if (_reportMedian == 0) _reportMedian = new AplusReportMedianAlgorithm;
  return _reportMedian;
}




