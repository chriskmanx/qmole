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

struct AlphaGenRegisterInfo : public TargetRegisterInfo {
  explicit AlphaGenRegisterInfo(int CallFrameSetupOpcode = -1, int CallFrameDestroyOpcode = -1);
  virtual int getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const;
  virtual int getDwarfRegNum(unsigned RegNum, bool isEH) const = 0;
  virtual bool needsStackRealignment(const MachineFunction &) const
     { return false; }
  unsigned getSubReg(unsigned RegNo, unsigned Index) const;
  unsigned getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const;
  unsigned composeSubRegIndices(unsigned, unsigned) const;
};

namespace Alpha { // Register classes
  enum {
    F4RCRegClassID = 0,
    F8RCRegClassID = 1,
    GPRCRegClassID = 2
  };

  struct F4RCClass : public TargetRegisterClass {
    F4RCClass();

    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern F4RCClass	F4RCRegClass;
  static TargetRegisterClass * const F4RCRegisterClass = &F4RCRegClass;
  struct F8RCClass : public TargetRegisterClass {
    F8RCClass();

    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern F8RCClass	F8RCRegClass;
  static TargetRegisterClass * const F8RCRegisterClass = &F8RCRegClass;
  struct GPRCClass : public TargetRegisterClass {
    GPRCClass();

    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern GPRCClass	GPRCRegClass;
  static TargetRegisterClass * const GPRCRegisterClass = &GPRCRegClass;
} // end of namespace Alpha

} // End llvm namespace 
