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

struct MipsGenRegisterInfo : public TargetRegisterInfo {
  explicit MipsGenRegisterInfo(int CallFrameSetupOpcode = -1, int CallFrameDestroyOpcode = -1);
  virtual int getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const;
  virtual int getDwarfRegNum(unsigned RegNum, bool isEH) const = 0;
  virtual bool needsStackRealignment(const MachineFunction &) const
     { return false; }
  unsigned getSubReg(unsigned RegNo, unsigned Index) const;
  unsigned getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const;
  unsigned composeSubRegIndices(unsigned, unsigned) const;
};

namespace Mips { // Register classes
  enum {
    AFGR64RegClassID = 0,
    CCRRegClassID = 1,
    CPURegsRegClassID = 2,
    FGR32RegClassID = 3,
    HILORegClassID = 4
  };

  struct AFGR64Class : public TargetRegisterClass {
    AFGR64Class();

    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern AFGR64Class	AFGR64RegClass;
  static TargetRegisterClass * const AFGR64RegisterClass = &AFGR64RegClass;
  struct CCRClass : public TargetRegisterClass {
    CCRClass();
  };
  extern CCRClass	CCRRegClass;
  static TargetRegisterClass * const CCRRegisterClass = &CCRRegClass;
  struct CPURegsClass : public TargetRegisterClass {
    CPURegsClass();

    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern CPURegsClass	CPURegsRegClass;
  static TargetRegisterClass * const CPURegsRegisterClass = &CPURegsRegClass;
  struct FGR32Class : public TargetRegisterClass {
    FGR32Class();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern FGR32Class	FGR32RegClass;
  static TargetRegisterClass * const FGR32RegisterClass = &FGR32RegClass;
  struct HILOClass : public TargetRegisterClass {
    HILOClass();
  };
  extern HILOClass	HILORegClass;
  static TargetRegisterClass * const HILORegisterClass = &HILORegClass;
} // end of namespace Mips

} // End llvm namespace 
