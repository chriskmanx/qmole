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

struct ARMGenRegisterInfo : public TargetRegisterInfo {
  explicit ARMGenRegisterInfo(int CallFrameSetupOpcode = -1, int CallFrameDestroyOpcode = -1);
  virtual int getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const;
  virtual int getDwarfRegNum(unsigned RegNum, bool isEH) const = 0;
  virtual bool needsStackRealignment(const MachineFunction &) const
     { return false; }
  unsigned getSubReg(unsigned RegNo, unsigned Index) const;
  unsigned getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const;
  unsigned composeSubRegIndices(unsigned, unsigned) const;
};

namespace ARM { // Register classes
  enum {
    CCRRegClassID = 0,
    DPRRegClassID = 1,
    DPR_8RegClassID = 2,
    DPR_VFP2RegClassID = 3,
    GPRRegClassID = 4,
    QPRRegClassID = 5,
    QPR_8RegClassID = 6,
    QPR_VFP2RegClassID = 7,
    QQPRRegClassID = 8,
    QQPR_VFP2RegClassID = 9,
    QQQQPRRegClassID = 10,
    SPRRegClassID = 11,
    SPR_8RegClassID = 12,
    rGPRRegClassID = 13,
    tGPRRegClassID = 14,
    tcGPRRegClassID = 15
  };

  struct CCRClass : public TargetRegisterClass {
    CCRClass();
  };
  extern CCRClass	CCRRegClass;
  static TargetRegisterClass * const CCRRegisterClass = &CCRRegClass;
  struct DPRClass : public TargetRegisterClass {
    DPRClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern DPRClass	DPRRegClass;
  static TargetRegisterClass * const DPRRegisterClass = &DPRRegClass;
  struct DPR_8Class : public TargetRegisterClass {
    DPR_8Class();
  };
  extern DPR_8Class	DPR_8RegClass;
  static TargetRegisterClass * const DPR_8RegisterClass = &DPR_8RegClass;
  struct DPR_VFP2Class : public TargetRegisterClass {
    DPR_VFP2Class();
  };
  extern DPR_VFP2Class	DPR_VFP2RegClass;
  static TargetRegisterClass * const DPR_VFP2RegisterClass = &DPR_VFP2RegClass;
  struct GPRClass : public TargetRegisterClass {
    GPRClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern GPRClass	GPRRegClass;
  static TargetRegisterClass * const GPRRegisterClass = &GPRRegClass;
  struct QPRClass : public TargetRegisterClass {
    QPRClass();
  };
  extern QPRClass	QPRRegClass;
  static TargetRegisterClass * const QPRRegisterClass = &QPRRegClass;
  struct QPR_8Class : public TargetRegisterClass {
    QPR_8Class();
  };
  extern QPR_8Class	QPR_8RegClass;
  static TargetRegisterClass * const QPR_8RegisterClass = &QPR_8RegClass;
  struct QPR_VFP2Class : public TargetRegisterClass {
    QPR_VFP2Class();
  };
  extern QPR_VFP2Class	QPR_VFP2RegClass;
  static TargetRegisterClass * const QPR_VFP2RegisterClass = &QPR_VFP2RegClass;
  struct QQPRClass : public TargetRegisterClass {
    QQPRClass();
  };
  extern QQPRClass	QQPRRegClass;
  static TargetRegisterClass * const QQPRRegisterClass = &QQPRRegClass;
  struct QQPR_VFP2Class : public TargetRegisterClass {
    QQPR_VFP2Class();
  };
  extern QQPR_VFP2Class	QQPR_VFP2RegClass;
  static TargetRegisterClass * const QQPR_VFP2RegisterClass = &QQPR_VFP2RegClass;
  struct QQQQPRClass : public TargetRegisterClass {
    QQQQPRClass();
  };
  extern QQQQPRClass	QQQQPRRegClass;
  static TargetRegisterClass * const QQQQPRRegisterClass = &QQQQPRRegClass;
  struct SPRClass : public TargetRegisterClass {
    SPRClass();
  };
  extern SPRClass	SPRRegClass;
  static TargetRegisterClass * const SPRRegisterClass = &SPRRegClass;
  struct SPR_8Class : public TargetRegisterClass {
    SPR_8Class();
  };
  extern SPR_8Class	SPR_8RegClass;
  static TargetRegisterClass * const SPR_8RegisterClass = &SPR_8RegClass;
  struct rGPRClass : public TargetRegisterClass {
    rGPRClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern rGPRClass	rGPRRegClass;
  static TargetRegisterClass * const rGPRRegisterClass = &rGPRRegClass;
  struct tGPRClass : public TargetRegisterClass {
    tGPRClass();
  };
  extern tGPRClass	tGPRRegClass;
  static TargetRegisterClass * const tGPRRegisterClass = &tGPRRegClass;
  struct tcGPRClass : public TargetRegisterClass {
    tcGPRClass();

    iterator allocation_order_begin(const MachineFunction &MF) const;
    iterator allocation_order_end(const MachineFunction &MF) const;
    };
  extern tcGPRClass	tcGPRRegClass;
  static TargetRegisterClass * const tcGPRRegisterClass = &tcGPRRegClass;
} // end of namespace ARM

} // End llvm namespace 
