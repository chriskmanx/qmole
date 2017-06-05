/*=========================================================================

  Program:   KWSys - Kitware System Library
  Module:    $RCSfile: SystemInformation.hxx.in,v $
  Language:  C++
  Date:      $Date: 2008-03-11 21:37:17 $
  Version:   $Revision: 1.8 $
  Copyright (c) 2005 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.


     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.
=========================================================================*/
#ifndef cmsys_SystemInformation_h
#define cmsys_SystemInformation_h


/* Define these macros temporarily to keep the code readable.  */
#if !defined (KWSYS_NAMESPACE) && !cmsys_NAME_IS_KWSYS
# define kwsys_stl cmsys_stl
# define kwsys_ios cmsys_ios
#endif
#include <cmsys/stl/string>

namespace cmsys
{


// forward declare the implementation class  
class SystemInformationImplementation;
  
class cmsys_EXPORT SystemInformation 
{

public:
  SystemInformation ();
  ~SystemInformation ();

  const char * GetVendorString();
  const char * GetVendorID();
  kwsys_stl::string GetTypeID();
  kwsys_stl::string GetFamilyID();
  kwsys_stl::string GetModelID();
  kwsys_stl::string GetSteppingCode();
  const char * GetExtendedProcessorName();
  const char * GetProcessorSerialNumber();
  int GetProcessorCacheSize();
  int GetLogicalProcessorsPerPhysical();
  float GetProcessorClockFrequency();
  int GetProcessorAPICID();
  int GetProcessorCacheXSize(long int);
  bool DoesCPUSupportFeature(long int);
  
  const char * GetOSName();
  const char * GetHostname();
  const char * GetOSRelease();
  const char * GetOSVersion();
  const char * GetOSPlatform();

  bool Is64Bits();

  unsigned int GetNumberOfLogicalCPU(); // per physical cpu
  unsigned int GetNumberOfPhysicalCPU();

  bool DoesCPUSupportCPUID();

  // Retrieve memory information in megabyte.
  unsigned long GetTotalVirtualMemory();
  unsigned long GetAvailableVirtualMemory();
  unsigned long GetTotalPhysicalMemory();
  unsigned long GetAvailablePhysicalMemory();  

  /** Run the different checks */
  void RunCPUCheck();
  void RunOSCheck();
  void RunMemoryCheck();
private:
  SystemInformationImplementation* Implementation;

};
} // namespace cmsys

/* Undefine temporary macros.  */
#if !defined (KWSYS_NAMESPACE) && !cmsys_NAME_IS_KWSYS
# undef kwsys_stl
# undef kwsys_ios
#endif

#endif
