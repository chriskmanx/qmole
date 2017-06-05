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

struct XCoreGenRegisterInfo : public TargetRegisterInfo {
  explicit XCoreGenRegisterInfo(int CallFrameSetupOpcode = -1, int CallFrameDestroyOpcode = -1);
  virtual int getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const;
  virtual int getDwarfRegNum(unsigned RegNum, bool isEH) const = 0;
  virtual bool needsStackRealignment(const MachineFunction &) const
     { return false; }
  unsigned getSubReg(unsigned RegNo, unsigned Index) const;
  unsigned getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const;
  unsigned composeSubRegIndices(unsigned, unsigned) const;
};

namespace XCore { // Register classes
  enum {
    GRRegsRegClassID = 0,
    RRegsRegClassID = 1
  };

  struct GRRegsClass : public TargetRegisterClass {
    GRRegsClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern GRRegsClass	GRRegsRegClass;
  static TargetRegisterClass * const GRRegsRegisterClass = &GRRegsRegClass;
  struct RRegsClass : public TargetRegisterClass {
    RRegsClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern RRegsClass	RRegsRegClass;
  static TargetRegisterClass * const RRegsRegisterClass = &RRegsRegClass;
} // end of namespace XCore

} // End llvm namespace 
