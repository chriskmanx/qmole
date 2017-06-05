//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Register Information Header Fragment
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

#include "llvm/Target/TargetRegisterInfo.h"
#include <string>

namespace llvm {

struct SparcGenRegisterInfo : public TargetRegisterInfo {
  explicit SparcGenRegisterInfo(int CallFrameSetupOpcode = -1, int CallFrameDestroyOpcode = -1);
  virtual int getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const;
  virtual int getDwarfRegNum(unsigned RegNum, bool isEH) const = 0;
  virtual bool needsStackRealignment(const MachineFunction &) const
     { return false; }
  unsigned getSubReg(unsigned RegNo, unsigned Index) const;
  unsigned getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const;
  unsigned composeSubRegIndices(unsigned, unsigned) const;
};

namespace SP { // Register classes
  enum {
    DFPRegsRegClassID = 0,
    FPRegsRegClassID = 1,
    IntRegsRegClassID = 2
  };

  struct DFPRegsClass : public TargetRegisterClass {
    DFPRegsClass();
  };
  extern DFPRegsClass	DFPRegsRegClass;
  static TargetRegisterClass * const DFPRegsRegisterClass = &DFPRegsRegClass;
  struct FPRegsClass : public TargetRegisterClass {
    FPRegsClass();
  };
  extern FPRegsClass	FPRegsRegClass;
  static TargetRegisterClass * const FPRegsRegisterClass = &FPRegsRegClass;
  struct IntRegsClass : public TargetRegisterClass {
    IntRegsClass();

    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern IntRegsClass	IntRegsRegClass;
  static TargetRegisterClass * const IntRegsRegisterClass = &IntRegsRegClass;
} // end of namespace Sparc

} // End llvm namespace 
