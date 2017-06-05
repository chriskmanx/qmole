//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Register Information Source Fragment
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

namespace llvm {

namespace {     // Register classes...
  // GRRegs Register Class...
  static const unsigned GRRegs[] = {
    XCore::R0, XCore::R1, XCore::R2, XCore::R3, XCore::R11, XCore::R4, XCore::R5, XCore::R6, XCore::R7, XCore::R8, XCore::R9, XCore::R10, 
  };

  // RRegs Register Class...
  static const unsigned RRegs[] = {
    XCore::CP, XCore::DP, XCore::SP, XCore::LR, 
  };

  // GRRegsVTs Register Class Value Types...
  static const EVT GRRegsVTs[] = {
    MVT::i32, MVT::Other
  };

  // RRegsVTs Register Class Value Types...
  static const EVT RRegsVTs[] = {
    MVT::i32, MVT::Other
  };

}  // end anonymous namespace

namespace XCore {   // Register class instances
  GRRegsClass	GRRegsRegClass;
  RRegsClass	RRegsRegClass;

  static const TargetRegisterClass* const NullRegClasses[] = { NULL };

  // GRRegs Register Class sub-classes...
  static const TargetRegisterClass* const GRRegsSubclasses[] = {
    NULL
  };

  // RRegs Register Class sub-classes...
  static const TargetRegisterClass* const RRegsSubclasses[] = {
    NULL
  };

  // GRRegs Register Class super-classes...
  static const TargetRegisterClass* const GRRegsSuperclasses[] = {
    NULL
  };

  // RRegs Register Class super-classes...
  static const TargetRegisterClass* const RRegsSuperclasses[] = {
    NULL
  };


    GRRegsClass::iterator
    GRRegsClass::allocation_order_begin(const MachineFunction &MF) const {
      return begin();
    }
    GRRegsClass::iterator
    GRRegsClass::allocation_order_end(const MachineFunction &MF) const {
      const TargetMachine &TM = MF.getTarget();
      const TargetRegisterInfo *RI = TM.getRegisterInfo();
      if (RI->hasFP(MF))
        return end()-1;  // don't allocate R10
      else
        return end();
    }
  
GRRegsClass::GRRegsClass()  : TargetRegisterClass(GRRegsRegClassID, "GRRegs", GRRegsVTs, GRRegsSubclasses, GRRegsSuperclasses, NullRegClasses, NullRegClasses, 4, 4, 1, GRRegs, GRRegs + 12) {}

    RRegsClass::iterator
    RRegsClass::allocation_order_begin(const MachineFunction &MF) const {
      return begin();
    }
    RRegsClass::iterator
    RRegsClass::allocation_order_end(const MachineFunction &MF) const {
      // No allocatable registers
      return begin();
    }
  
RRegsClass::RRegsClass()  : TargetRegisterClass(RRegsRegClassID, "RRegs", RRegsVTs, RRegsSubclasses, RRegsSuperclasses, NullRegClasses, NullRegClasses, 4, 4, 1, RRegs, RRegs + 4) {}
}

namespace {
  const TargetRegisterClass* const RegisterClasses[] = {
    &XCore::GRRegsRegClass,
    &XCore::RRegsRegClass,
  };


  // Number of hash collisions: 0
  const unsigned SubregHashTable[] = { XCore::NoRegister, XCore::NoRegister, 
XCore::NoRegister, XCore::NoRegister };
  const unsigned SubregHashTableSize = 2;


  // Number of hash collisions: 0
  const unsigned AliasesHashTable[] = { XCore::NoRegister, XCore::NoRegister, 
XCore::NoRegister, XCore::NoRegister };
  const unsigned AliasesHashTableSize = 2;


  // Register Alias Sets...
  const unsigned Empty_AliasSet[] = { 0 };


  // Register Sub-registers Sets...
  const unsigned Empty_SubRegsSet[] = { 0 };
  const unsigned Empty_SuperRegsSet[] = { 0 };

  const TargetRegisterDesc RegisterDescriptors[] = { // Descriptors
    { "NOREG",	0,	0,	0 },
    { "CP",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "DP",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "LR",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R0",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R1",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R2",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R3",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R4",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R5",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R6",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R7",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R8",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R9",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R10",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "R11",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "SP",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
  };

  const char *const SubRegIndexTable[] = { "" };

}

unsigned XCoreGenRegisterInfo::getSubReg(unsigned RegNo, unsigned Index) const {
  switch (RegNo) {
  default:
    return 0;
  };
  return 0;
}

unsigned XCoreGenRegisterInfo::getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const {
  switch (RegNo) {
  default:
    return 0;
  };
  return 0;
}

unsigned XCoreGenRegisterInfo::composeSubRegIndices(unsigned IdxA, unsigned IdxB) const {
  switch (IdxA) {
  default:
    return IdxB;
  }
}

XCoreGenRegisterInfo::XCoreGenRegisterInfo(int CallFrameSetupOpcode, int CallFrameDestroyOpcode)
  : TargetRegisterInfo(RegisterDescriptors, 17, RegisterClasses, RegisterClasses+2,
                 SubRegIndexTable,
                 CallFrameSetupOpcode, CallFrameDestroyOpcode,
                 SubregHashTable, SubregHashTableSize,
                 AliasesHashTable, AliasesHashTableSize) {
}

int XCoreGenRegisterInfo::getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const {
  switch (Flavour) {
  default:
    assert(0 && "Unknown DWARF flavour");
    return -1;
  case 0:
    switch (RegNum) {
    default:
      assert(0 && "Invalid RegNum");
      return -1;
    case XCore::CP:
      return 12;
    case XCore::DP:
      return 13;
    case XCore::LR:
      return 15;
    case XCore::R0:
      return 0;
    case XCore::R1:
      return 1;
    case XCore::R2:
      return 2;
    case XCore::R3:
      return 3;
    case XCore::R4:
      return 4;
    case XCore::R5:
      return 5;
    case XCore::R6:
      return 6;
    case XCore::R7:
      return 7;
    case XCore::R8:
      return 8;
    case XCore::R9:
      return 9;
    case XCore::R10:
      return 10;
    case XCore::R11:
      return 11;
    case XCore::SP:
      return 14;
    };
  };
}

} // End llvm namespace 
