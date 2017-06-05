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

struct SPUGenRegisterInfo : public TargetRegisterInfo {
  explicit SPUGenRegisterInfo(int CallFrameSetupOpcode = -1, int CallFrameDestroyOpcode = -1);
  virtual int getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const;
  virtual int getDwarfRegNum(unsigned RegNum, bool isEH) const = 0;
  virtual bool needsStackRealignment(const MachineFunction &) const
     { return false; }
  unsigned getSubReg(unsigned RegNo, unsigned Index) const;
  unsigned getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const;
  unsigned composeSubRegIndices(unsigned, unsigned) const;
};

namespace SPU { // Register classes
  enum {
    GPRCRegClassID = 0,
    R16CRegClassID = 1,
    R32CRegClassID = 2,
    R32FPRegClassID = 3,
    R64CRegClassID = 4,
    R64FPRegClassID = 5,
    R8CRegClassID = 6,
    VECREGRegClassID = 7
  };

  struct GPRCClass : public TargetRegisterClass {
    GPRCClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern GPRCClass	GPRCRegClass;
  static TargetRegisterClass * const GPRCRegisterClass = &GPRCRegClass;
  struct R16CClass : public TargetRegisterClass {
    R16CClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern R16CClass	R16CRegClass;
  static TargetRegisterClass * const R16CRegisterClass = &R16CRegClass;
  struct R32CClass : public TargetRegisterClass {
    R32CClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern R32CClass	R32CRegClass;
  static TargetRegisterClass * const R32CRegisterClass = &R32CRegClass;
  struct R32FPClass : public TargetRegisterClass {
    R32FPClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern R32FPClass	R32FPRegClass;
  static TargetRegisterClass * const R32FPRegisterClass = &R32FPRegClass;
  struct R64CClass : public TargetRegisterClass {
    R64CClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern R64CClass	R64CRegClass;
  static TargetRegisterClass * const R64CRegisterClass = &R64CRegClass;
  struct R64FPClass : public TargetRegisterClass {
    R64FPClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern R64FPClass	R64FPRegClass;
  static TargetRegisterClass * const R64FPRegisterClass = &R64FPRegClass;
  struct R8CClass : public TargetRegisterClass {
    R8CClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern R8CClass	R8CRegClass;
  static TargetRegisterClass * const R8CRegisterClass = &R8CRegClass;
  struct VECREGClass : public TargetRegisterClass {
    VECREGClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern VECREGClass	VECREGRegClass;
  static TargetRegisterClass * const VECREGRegisterClass = &VECREGRegClass;
} // end of namespace SPU

} // End llvm namespace 
