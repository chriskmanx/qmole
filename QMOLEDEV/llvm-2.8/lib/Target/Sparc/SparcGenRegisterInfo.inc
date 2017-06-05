//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Register Information Source Fragment
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

namespace llvm {

namespace {     // Register classes...
  // DFPRegs Register Class...
  static const unsigned DFPRegs[] = {
    SP::D0, SP::D1, SP::D2, SP::D3, SP::D4, SP::D5, SP::D6, SP::D7, SP::D8, SP::D9, SP::D10, SP::D11, SP::D12, SP::D13, SP::D14, SP::D15, 
  };

  // FPRegs Register Class...
  static const unsigned FPRegs[] = {
    SP::F0, SP::F1, SP::F2, SP::F3, SP::F4, SP::F5, SP::F6, SP::F7, SP::F8, SP::F9, SP::F10, SP::F11, SP::F12, SP::F13, SP::F14, SP::F15, SP::F16, SP::F17, SP::F18, SP::F19, SP::F20, SP::F21, SP::F22, SP::F23, SP::F24, SP::F25, SP::F26, SP::F27, SP::F28, SP::F29, SP::F30, SP::F31, 
  };

  // IntRegs Register Class...
  static const unsigned IntRegs[] = {
    SP::L0, SP::L1, SP::L2, SP::L3, SP::L4, SP::L5, SP::L6, SP::L7, SP::I0, SP::I1, SP::I2, SP::I3, SP::I4, SP::I5, SP::O0, SP::O1, SP::O2, SP::O3, SP::O4, SP::O5, SP::O7, SP::G1, SP::G2, SP::G3, SP::G4, SP::O6, SP::I6, SP::I7, SP::G0, SP::G5, SP::G6, SP::G7, 
  };

  // DFPRegsVTs Register Class Value Types...
  static const EVT DFPRegsVTs[] = {
    MVT::f64, MVT::Other
  };

  // FPRegsVTs Register Class Value Types...
  static const EVT FPRegsVTs[] = {
    MVT::f32, MVT::Other
  };

  // IntRegsVTs Register Class Value Types...
  static const EVT IntRegsVTs[] = {
    MVT::i32, MVT::Other
  };

}  // end anonymous namespace

namespace SP {   // Register class instances
  DFPRegsClass	DFPRegsRegClass;
  FPRegsClass	FPRegsRegClass;
  IntRegsClass	IntRegsRegClass;

  // DFPRegs Sub-register Classes...
  static const TargetRegisterClass* const DFPRegsSubRegClasses[] = {
    0, 0
  };

  // FPRegs Sub-register Classes...
  static const TargetRegisterClass* const FPRegsSubRegClasses[] = {
    0, 0
  };

  // IntRegs Sub-register Classes...
  static const TargetRegisterClass* const IntRegsSubRegClasses[] = {
    0, 0
  };

  // DFPRegs Super-register Classes...
  static const TargetRegisterClass* const DFPRegsSuperRegClasses[] = {
    NULL
  };

  // FPRegs Super-register Classes...
  static const TargetRegisterClass* const FPRegsSuperRegClasses[] = {
    NULL
  };

  // IntRegs Super-register Classes...
  static const TargetRegisterClass* const IntRegsSuperRegClasses[] = {
    NULL
  };

  // DFPRegs Register Class sub-classes...
  static const TargetRegisterClass* const DFPRegsSubclasses[] = {
    NULL
  };

  // FPRegs Register Class sub-classes...
  static const TargetRegisterClass* const FPRegsSubclasses[] = {
    NULL
  };

  // IntRegs Register Class sub-classes...
  static const TargetRegisterClass* const IntRegsSubclasses[] = {
    NULL
  };

  // DFPRegs Register Class super-classes...
  static const TargetRegisterClass* const DFPRegsSuperclasses[] = {
    NULL
  };

  // FPRegs Register Class super-classes...
  static const TargetRegisterClass* const FPRegsSuperclasses[] = {
    NULL
  };

  // IntRegs Register Class super-classes...
  static const TargetRegisterClass* const IntRegsSuperclasses[] = {
    NULL
  };


DFPRegsClass::DFPRegsClass()  : TargetRegisterClass(DFPRegsRegClassID, "DFPRegs", DFPRegsVTs, DFPRegsSubclasses, DFPRegsSuperclasses, DFPRegsSubRegClasses, DFPRegsSuperRegClasses, 8, 8, 1, DFPRegs, DFPRegs + 16) {}

FPRegsClass::FPRegsClass()  : TargetRegisterClass(FPRegsRegClassID, "FPRegs", FPRegsVTs, FPRegsSubclasses, FPRegsSuperclasses, FPRegsSubRegClasses, FPRegsSuperRegClasses, 4, 4, 1, FPRegs, FPRegs + 32) {}

    IntRegsClass::iterator
    IntRegsClass::allocation_order_end(const MachineFunction &MF) const {
      // FIXME: These special regs should be taken out of the regclass!
      return end()-10  // Don't allocate special registers
         -1;  // FIXME: G1 reserved for large imm generation by frame code.
    }
  
IntRegsClass::IntRegsClass()  : TargetRegisterClass(IntRegsRegClassID, "IntRegs", IntRegsVTs, IntRegsSubclasses, IntRegsSuperclasses, IntRegsSubRegClasses, IntRegsSuperRegClasses, 4, 4, 1, IntRegs, IntRegs + 32) {}
}

namespace {
  const TargetRegisterClass* const RegisterClasses[] = {
    &SP::DFPRegsRegClass,
    &SP::FPRegsRegClass,
    &SP::IntRegsRegClass,
  };


  // Number of hash collisions: 0
  const unsigned SubregHashTable[] = { SP::D15, SP::F31, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D2, SP::F4, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D5, SP::F11, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D9, SP::F18, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D12, SP::F25, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D2, SP::F5, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D6, SP::F12, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D9, SP::F19, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D13, SP::F26, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D3, SP::F6, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D6, SP::F13, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D10, SP::F20, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D13, SP::F27, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D0, SP::F0, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D3, SP::F7, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D7, SP::F14, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D10, SP::F21, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D14, SP::F28, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D0, SP::F1, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D4, SP::F8, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D7, SP::F15, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D11, SP::F22, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D14, SP::F29, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D1, SP::F2, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D4, SP::F9, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D8, SP::F16, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D11, SP::F23, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D15, SP::F30, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D1, SP::F3, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D5, SP::F10, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D8, SP::F17, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D12, SP::F24, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
SP::NoRegister, SP::NoRegister };
  const unsigned SubregHashTableSize = 256;


  // Number of hash collisions: 1
  const unsigned AliasesHashTable[] = { SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F24, SP::D12, 
                                       SP::F25, SP::D12, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D5, SP::F11, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D12, SP::F25, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F26, SP::D13, 
                                       SP::F27, SP::D13, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F0, SP::D0, 
                                       SP::F1, SP::D0, 
                                       SP::D6, SP::F12, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D13, SP::F26, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F28, SP::D14, 
                                       SP::F29, SP::D14, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D6, SP::F13, 
                                       SP::F3, SP::D1, 
                                       SP::F2, SP::D1, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D13, SP::F27, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D0, SP::F0, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F30, SP::D15, 
                                       SP::F31, SP::D15, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D7, SP::F14, 
                                       SP::F4, SP::D2, 
                                       SP::F5, SP::D2, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D14, SP::F28, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D0, SP::F1, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D7, SP::F15, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F6, SP::D3, 
                                       SP::F7, SP::D3, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D14, SP::F29, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D1, SP::F2, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D8, SP::F16, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F8, SP::D4, 
                                       SP::F9, SP::D4, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D15, SP::F30, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D1, SP::F3, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D8, SP::F17, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F10, SP::D5, 
                                       SP::F11, SP::D5, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D15, SP::F31, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D2, SP::F4, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D9, SP::F18, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F12, SP::D6, 
                                       SP::F13, SP::D6, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D2, SP::F5, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D9, SP::F19, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F14, SP::D7, 
                                       SP::F15, SP::D7, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D3, SP::F6, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D10, SP::F20, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F16, SP::D8, 
                                       SP::F17, SP::D8, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D3, SP::F7, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D10, SP::F21, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F18, SP::D9, 
                                       SP::F19, SP::D9, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D4, SP::F8, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D11, SP::F22, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F20, SP::D10, 
                                       SP::F21, SP::D10, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D4, SP::F9, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D11, SP::F23, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::F22, SP::D11, 
                                       SP::F23, SP::D11, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D5, SP::F10, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::D12, SP::F24, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
                                       SP::NoRegister, SP::NoRegister, 
SP::NoRegister, SP::NoRegister };
  const unsigned AliasesHashTableSize = 512;


  // Register Alias Sets...
  const unsigned Empty_AliasSet[] = { 0 };
  const unsigned D0_AliasSet[] = { SP::F0, SP::F1, 0 };
  const unsigned D1_AliasSet[] = { SP::F2, SP::F3, 0 };
  const unsigned D2_AliasSet[] = { SP::F4, SP::F5, 0 };
  const unsigned D3_AliasSet[] = { SP::F6, SP::F7, 0 };
  const unsigned D4_AliasSet[] = { SP::F8, SP::F9, 0 };
  const unsigned D5_AliasSet[] = { SP::F10, SP::F11, 0 };
  const unsigned D6_AliasSet[] = { SP::F12, SP::F13, 0 };
  const unsigned D7_AliasSet[] = { SP::F14, SP::F15, 0 };
  const unsigned D8_AliasSet[] = { SP::F16, SP::F17, 0 };
  const unsigned D9_AliasSet[] = { SP::F18, SP::F19, 0 };
  const unsigned D10_AliasSet[] = { SP::F20, SP::F21, 0 };
  const unsigned D11_AliasSet[] = { SP::F22, SP::F23, 0 };
  const unsigned D12_AliasSet[] = { SP::F24, SP::F25, 0 };
  const unsigned D13_AliasSet[] = { SP::F26, SP::F27, 0 };
  const unsigned D14_AliasSet[] = { SP::F28, SP::F29, 0 };
  const unsigned D15_AliasSet[] = { SP::F30, SP::F31, 0 };
  const unsigned F0_AliasSet[] = { SP::D0, 0 };
  const unsigned F1_AliasSet[] = { SP::D0, 0 };
  const unsigned F2_AliasSet[] = { SP::D1, 0 };
  const unsigned F3_AliasSet[] = { SP::D1, 0 };
  const unsigned F4_AliasSet[] = { SP::D2, 0 };
  const unsigned F5_AliasSet[] = { SP::D2, 0 };
  const unsigned F6_AliasSet[] = { SP::D3, 0 };
  const unsigned F7_AliasSet[] = { SP::D3, 0 };
  const unsigned F8_AliasSet[] = { SP::D4, 0 };
  const unsigned F9_AliasSet[] = { SP::D4, 0 };
  const unsigned F10_AliasSet[] = { SP::D5, 0 };
  const unsigned F11_AliasSet[] = { SP::D5, 0 };
  const unsigned F12_AliasSet[] = { SP::D6, 0 };
  const unsigned F13_AliasSet[] = { SP::D6, 0 };
  const unsigned F14_AliasSet[] = { SP::D7, 0 };
  const unsigned F15_AliasSet[] = { SP::D7, 0 };
  const unsigned F16_AliasSet[] = { SP::D8, 0 };
  const unsigned F17_AliasSet[] = { SP::D8, 0 };
  const unsigned F18_AliasSet[] = { SP::D9, 0 };
  const unsigned F19_AliasSet[] = { SP::D9, 0 };
  const unsigned F20_AliasSet[] = { SP::D10, 0 };
  const unsigned F21_AliasSet[] = { SP::D10, 0 };
  const unsigned F22_AliasSet[] = { SP::D11, 0 };
  const unsigned F23_AliasSet[] = { SP::D11, 0 };
  const unsigned F24_AliasSet[] = { SP::D12, 0 };
  const unsigned F25_AliasSet[] = { SP::D12, 0 };
  const unsigned F26_AliasSet[] = { SP::D13, 0 };
  const unsigned F27_AliasSet[] = { SP::D13, 0 };
  const unsigned F28_AliasSet[] = { SP::D14, 0 };
  const unsigned F29_AliasSet[] = { SP::D14, 0 };
  const unsigned F30_AliasSet[] = { SP::D15, 0 };
  const unsigned F31_AliasSet[] = { SP::D15, 0 };


  // Register Sub-registers Sets...
  const unsigned Empty_SubRegsSet[] = { 0 };
  const unsigned D0_SubRegsSet[] = { SP::F0, SP::F1, 0 };
  const unsigned D1_SubRegsSet[] = { SP::F2, SP::F3, 0 };
  const unsigned D2_SubRegsSet[] = { SP::F4, SP::F5, 0 };
  const unsigned D3_SubRegsSet[] = { SP::F6, SP::F7, 0 };
  const unsigned D4_SubRegsSet[] = { SP::F8, SP::F9, 0 };
  const unsigned D5_SubRegsSet[] = { SP::F10, SP::F11, 0 };
  const unsigned D6_SubRegsSet[] = { SP::F12, SP::F13, 0 };
  const unsigned D7_SubRegsSet[] = { SP::F14, SP::F15, 0 };
  const unsigned D8_SubRegsSet[] = { SP::F16, SP::F17, 0 };
  const unsigned D9_SubRegsSet[] = { SP::F18, SP::F19, 0 };
  const unsigned D10_SubRegsSet[] = { SP::F20, SP::F21, 0 };
  const unsigned D11_SubRegsSet[] = { SP::F22, SP::F23, 0 };
  const unsigned D12_SubRegsSet[] = { SP::F24, SP::F25, 0 };
  const unsigned D13_SubRegsSet[] = { SP::F26, SP::F27, 0 };
  const unsigned D14_SubRegsSet[] = { SP::F28, SP::F29, 0 };
  const unsigned D15_SubRegsSet[] = { SP::F30, SP::F31, 0 };


  // Register Super-registers Sets...
  const unsigned Empty_SuperRegsSet[] = { 0 };
  const unsigned F0_SuperRegsSet[] = { SP::D0, 0 };
  const unsigned F1_SuperRegsSet[] = { SP::D0, 0 };
  const unsigned F2_SuperRegsSet[] = { SP::D1, 0 };
  const unsigned F3_SuperRegsSet[] = { SP::D1, 0 };
  const unsigned F4_SuperRegsSet[] = { SP::D2, 0 };
  const unsigned F5_SuperRegsSet[] = { SP::D2, 0 };
  const unsigned F6_SuperRegsSet[] = { SP::D3, 0 };
  const unsigned F7_SuperRegsSet[] = { SP::D3, 0 };
  const unsigned F8_SuperRegsSet[] = { SP::D4, 0 };
  const unsigned F9_SuperRegsSet[] = { SP::D4, 0 };
  const unsigned F10_SuperRegsSet[] = { SP::D5, 0 };
  const unsigned F11_SuperRegsSet[] = { SP::D5, 0 };
  const unsigned F12_SuperRegsSet[] = { SP::D6, 0 };
  const unsigned F13_SuperRegsSet[] = { SP::D6, 0 };
  const unsigned F14_SuperRegsSet[] = { SP::D7, 0 };
  const unsigned F15_SuperRegsSet[] = { SP::D7, 0 };
  const unsigned F16_SuperRegsSet[] = { SP::D8, 0 };
  const unsigned F17_SuperRegsSet[] = { SP::D8, 0 };
  const unsigned F18_SuperRegsSet[] = { SP::D9, 0 };
  const unsigned F19_SuperRegsSet[] = { SP::D9, 0 };
  const unsigned F20_SuperRegsSet[] = { SP::D10, 0 };
  const unsigned F21_SuperRegsSet[] = { SP::D10, 0 };
  const unsigned F22_SuperRegsSet[] = { SP::D11, 0 };
  const unsigned F23_SuperRegsSet[] = { SP::D11, 0 };
  const unsigned F24_SuperRegsSet[] = { SP::D12, 0 };
  const unsigned F25_SuperRegsSet[] = { SP::D12, 0 };
  const unsigned F26_SuperRegsSet[] = { SP::D13, 0 };
  const unsigned F27_SuperRegsSet[] = { SP::D13, 0 };
  const unsigned F28_SuperRegsSet[] = { SP::D14, 0 };
  const unsigned F29_SuperRegsSet[] = { SP::D14, 0 };
  const unsigned F30_SuperRegsSet[] = { SP::D15, 0 };
  const unsigned F31_SuperRegsSet[] = { SP::D15, 0 };

  const TargetRegisterDesc RegisterDescriptors[] = { // Descriptors
    { "NOREG",	0,	0,	0 },
    { "D0",	D0_AliasSet,	D0_SubRegsSet,	Empty_SuperRegsSet },
    { "D1",	D1_AliasSet,	D1_SubRegsSet,	Empty_SuperRegsSet },
    { "D2",	D2_AliasSet,	D2_SubRegsSet,	Empty_SuperRegsSet },
    { "D3",	D3_AliasSet,	D3_SubRegsSet,	Empty_SuperRegsSet },
    { "D4",	D4_AliasSet,	D4_SubRegsSet,	Empty_SuperRegsSet },
    { "D5",	D5_AliasSet,	D5_SubRegsSet,	Empty_SuperRegsSet },
    { "D6",	D6_AliasSet,	D6_SubRegsSet,	Empty_SuperRegsSet },
    { "D7",	D7_AliasSet,	D7_SubRegsSet,	Empty_SuperRegsSet },
    { "D8",	D8_AliasSet,	D8_SubRegsSet,	Empty_SuperRegsSet },
    { "D9",	D9_AliasSet,	D9_SubRegsSet,	Empty_SuperRegsSet },
    { "D10",	D10_AliasSet,	D10_SubRegsSet,	Empty_SuperRegsSet },
    { "D11",	D11_AliasSet,	D11_SubRegsSet,	Empty_SuperRegsSet },
    { "D12",	D12_AliasSet,	D12_SubRegsSet,	Empty_SuperRegsSet },
    { "D13",	D13_AliasSet,	D13_SubRegsSet,	Empty_SuperRegsSet },
    { "D14",	D14_AliasSet,	D14_SubRegsSet,	Empty_SuperRegsSet },
    { "D15",	D15_AliasSet,	D15_SubRegsSet,	Empty_SuperRegsSet },
    { "F0",	F0_AliasSet,	Empty_SubRegsSet,	F0_SuperRegsSet },
    { "F1",	F1_AliasSet,	Empty_SubRegsSet,	F1_SuperRegsSet },
    { "F2",	F2_AliasSet,	Empty_SubRegsSet,	F2_SuperRegsSet },
    { "F3",	F3_AliasSet,	Empty_SubRegsSet,	F3_SuperRegsSet },
    { "F4",	F4_AliasSet,	Empty_SubRegsSet,	F4_SuperRegsSet },
    { "F5",	F5_AliasSet,	Empty_SubRegsSet,	F5_SuperRegsSet },
    { "F6",	F6_AliasSet,	Empty_SubRegsSet,	F6_SuperRegsSet },
    { "F7",	F7_AliasSet,	Empty_SubRegsSet,	F7_SuperRegsSet },
    { "F8",	F8_AliasSet,	Empty_SubRegsSet,	F8_SuperRegsSet },
    { "F9",	F9_AliasSet,	Empty_SubRegsSet,	F9_SuperRegsSet },
    { "F10",	F10_AliasSet,	Empty_SubRegsSet,	F10_SuperRegsSet },
    { "F11",	F11_AliasSet,	Empty_SubRegsSet,	F11_SuperRegsSet },
    { "F12",	F12_AliasSet,	Empty_SubRegsSet,	F12_SuperRegsSet },
    { "F13",	F13_AliasSet,	Empty_SubRegsSet,	F13_SuperRegsSet },
    { "F14",	F14_AliasSet,	Empty_SubRegsSet,	F14_SuperRegsSet },
    { "F15",	F15_AliasSet,	Empty_SubRegsSet,	F15_SuperRegsSet },
    { "F16",	F16_AliasSet,	Empty_SubRegsSet,	F16_SuperRegsSet },
    { "F17",	F17_AliasSet,	Empty_SubRegsSet,	F17_SuperRegsSet },
    { "F18",	F18_AliasSet,	Empty_SubRegsSet,	F18_SuperRegsSet },
    { "F19",	F19_AliasSet,	Empty_SubRegsSet,	F19_SuperRegsSet },
    { "F20",	F20_AliasSet,	Empty_SubRegsSet,	F20_SuperRegsSet },
    { "F21",	F21_AliasSet,	Empty_SubRegsSet,	F21_SuperRegsSet },
    { "F22",	F22_AliasSet,	Empty_SubRegsSet,	F22_SuperRegsSet },
    { "F23",	F23_AliasSet,	Empty_SubRegsSet,	F23_SuperRegsSet },
    { "F24",	F24_AliasSet,	Empty_SubRegsSet,	F24_SuperRegsSet },
    { "F25",	F25_AliasSet,	Empty_SubRegsSet,	F25_SuperRegsSet },
    { "F26",	F26_AliasSet,	Empty_SubRegsSet,	F26_SuperRegsSet },
    { "F27",	F27_AliasSet,	Empty_SubRegsSet,	F27_SuperRegsSet },
    { "F28",	F28_AliasSet,	Empty_SubRegsSet,	F28_SuperRegsSet },
    { "F29",	F29_AliasSet,	Empty_SubRegsSet,	F29_SuperRegsSet },
    { "F30",	F30_AliasSet,	Empty_SubRegsSet,	F30_SuperRegsSet },
    { "F31",	F31_AliasSet,	Empty_SubRegsSet,	F31_SuperRegsSet },
    { "FCC",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G0",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G1",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G2",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G3",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G4",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G5",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G6",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "G7",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I0",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I1",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I2",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I3",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I4",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I5",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I6",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "I7",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "ICC",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L0",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L1",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L2",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L3",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L4",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L5",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L6",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "L7",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O0",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O1",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O2",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O3",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O4",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O5",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O6",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
    { "O7",	Empty_AliasSet,	Empty_SubRegsSet,	Empty_SuperRegsSet },
  };

  const char *const SubRegIndexTable[] = { "sub_even", "sub_odd" };

}

unsigned SparcGenRegisterInfo::getSubReg(unsigned RegNo, unsigned Index) const {
  switch (RegNo) {
  default:
    return 0;
  case SP::D0:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F0;
    case SP::sub_odd: return SP::F1;
    };
    break;
  case SP::D1:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F2;
    case SP::sub_odd: return SP::F3;
    };
    break;
  case SP::D2:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F4;
    case SP::sub_odd: return SP::F5;
    };
    break;
  case SP::D3:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F6;
    case SP::sub_odd: return SP::F7;
    };
    break;
  case SP::D4:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F8;
    case SP::sub_odd: return SP::F9;
    };
    break;
  case SP::D5:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F10;
    case SP::sub_odd: return SP::F11;
    };
    break;
  case SP::D6:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F12;
    case SP::sub_odd: return SP::F13;
    };
    break;
  case SP::D7:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F14;
    case SP::sub_odd: return SP::F15;
    };
    break;
  case SP::D8:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F16;
    case SP::sub_odd: return SP::F17;
    };
    break;
  case SP::D9:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F18;
    case SP::sub_odd: return SP::F19;
    };
    break;
  case SP::D10:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F20;
    case SP::sub_odd: return SP::F21;
    };
    break;
  case SP::D11:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F22;
    case SP::sub_odd: return SP::F23;
    };
    break;
  case SP::D12:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F24;
    case SP::sub_odd: return SP::F25;
    };
    break;
  case SP::D13:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F26;
    case SP::sub_odd: return SP::F27;
    };
    break;
  case SP::D14:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F28;
    case SP::sub_odd: return SP::F29;
    };
    break;
  case SP::D15:
    switch (Index) {
    default: return 0;
    case SP::sub_even: return SP::F30;
    case SP::sub_odd: return SP::F31;
    };
    break;
  };
  return 0;
}

unsigned SparcGenRegisterInfo::getSubRegIndex(unsigned RegNo, unsigned SubRegNo) const {
  switch (RegNo) {
  default:
    return 0;
  case SP::D0:
    if (SubRegNo == SP::F0)  return SP::sub_even;
    if (SubRegNo == SP::F1)  return SP::sub_odd;
    return 0;
  case SP::D1:
    if (SubRegNo == SP::F2)  return SP::sub_even;
    if (SubRegNo == SP::F3)  return SP::sub_odd;
    return 0;
  case SP::D2:
    if (SubRegNo == SP::F4)  return SP::sub_even;
    if (SubRegNo == SP::F5)  return SP::sub_odd;
    return 0;
  case SP::D3:
    if (SubRegNo == SP::F6)  return SP::sub_even;
    if (SubRegNo == SP::F7)  return SP::sub_odd;
    return 0;
  case SP::D4:
    if (SubRegNo == SP::F8)  return SP::sub_even;
    if (SubRegNo == SP::F9)  return SP::sub_odd;
    return 0;
  case SP::D5:
    if (SubRegNo == SP::F10)  return SP::sub_even;
    if (SubRegNo == SP::F11)  return SP::sub_odd;
    return 0;
  case SP::D6:
    if (SubRegNo == SP::F12)  return SP::sub_even;
    if (SubRegNo == SP::F13)  return SP::sub_odd;
    return 0;
  case SP::D7:
    if (SubRegNo == SP::F14)  return SP::sub_even;
    if (SubRegNo == SP::F15)  return SP::sub_odd;
    return 0;
  case SP::D8:
    if (SubRegNo == SP::F16)  return SP::sub_even;
    if (SubRegNo == SP::F17)  return SP::sub_odd;
    return 0;
  case SP::D9:
    if (SubRegNo == SP::F18)  return SP::sub_even;
    if (SubRegNo == SP::F19)  return SP::sub_odd;
    return 0;
  case SP::D10:
    if (SubRegNo == SP::F20)  return SP::sub_even;
    if (SubRegNo == SP::F21)  return SP::sub_odd;
    return 0;
  case SP::D11:
    if (SubRegNo == SP::F22)  return SP::sub_even;
    if (SubRegNo == SP::F23)  return SP::sub_odd;
    return 0;
  case SP::D12:
    if (SubRegNo == SP::F24)  return SP::sub_even;
    if (SubRegNo == SP::F25)  return SP::sub_odd;
    return 0;
  case SP::D13:
    if (SubRegNo == SP::F26)  return SP::sub_even;
    if (SubRegNo == SP::F27)  return SP::sub_odd;
    return 0;
  case SP::D14:
    if (SubRegNo == SP::F28)  return SP::sub_even;
    if (SubRegNo == SP::F29)  return SP::sub_odd;
    return 0;
  case SP::D15:
    if (SubRegNo == SP::F30)  return SP::sub_even;
    if (SubRegNo == SP::F31)  return SP::sub_odd;
    return 0;
  };
  return 0;
}

unsigned SparcGenRegisterInfo::composeSubRegIndices(unsigned IdxA, unsigned IdxB) const {
  switch (IdxA) {
  default:
    return IdxB;
  }
}

SparcGenRegisterInfo::SparcGenRegisterInfo(int CallFrameSetupOpcode, int CallFrameDestroyOpcode)
  : TargetRegisterInfo(RegisterDescriptors, 83, RegisterClasses, RegisterClasses+3,
                 SubRegIndexTable,
                 CallFrameSetupOpcode, CallFrameDestroyOpcode,
                 SubregHashTable, SubregHashTableSize,
                 AliasesHashTable, AliasesHashTableSize) {
}

int SparcGenRegisterInfo::getDwarfRegNumFull(unsigned RegNum, unsigned Flavour) const {
  switch (Flavour) {
  default:
    assert(0 && "Unknown DWARF flavour");
    return -1;
  case 0:
    switch (RegNum) {
    default:
      assert(0 && "Invalid RegNum");
      return -1;
    case SP::D0:
      return 32;
    case SP::D1:
      return 34;
    case SP::D2:
      return 36;
    case SP::D3:
      return 38;
    case SP::D4:
      return 40;
    case SP::D5:
      return 42;
    case SP::D6:
      return 44;
    case SP::D7:
      return 46;
    case SP::D8:
      return 48;
    case SP::D9:
      return 50;
    case SP::D10:
      return 52;
    case SP::D11:
      return 54;
    case SP::D12:
      return 56;
    case SP::D13:
      return 58;
    case SP::D14:
      return 60;
    case SP::D15:
      return 62;
    case SP::F0:
      return 32;
    case SP::F1:
      return 33;
    case SP::F2:
      return 34;
    case SP::F3:
      return 35;
    case SP::F4:
      return 36;
    case SP::F5:
      return 37;
    case SP::F6:
      return 38;
    case SP::F7:
      return 39;
    case SP::F8:
      return 40;
    case SP::F9:
      return 41;
    case SP::F10:
      return 42;
    case SP::F11:
      return 43;
    case SP::F12:
      return 44;
    case SP::F13:
      return 45;
    case SP::F14:
      return 46;
    case SP::F15:
      return 47;
    case SP::F16:
      return 48;
    case SP::F17:
      return 49;
    case SP::F18:
      return 50;
    case SP::F19:
      return 51;
    case SP::F20:
      return 52;
    case SP::F21:
      return 53;
    case SP::F22:
      return 54;
    case SP::F23:
      return 55;
    case SP::F24:
      return 56;
    case SP::F25:
      return 57;
    case SP::F26:
      return 58;
    case SP::F27:
      return 59;
    case SP::F28:
      return 60;
    case SP::F29:
      return 61;
    case SP::F30:
      return 62;
    case SP::F31:
      return 63;
    case SP::FCC:
      return -1;
    case SP::G0:
      return 0;
    case SP::G1:
      return 1;
    case SP::G2:
      return 2;
    case SP::G3:
      return 3;
    case SP::G4:
      return 4;
    case SP::G5:
      return 5;
    case SP::G6:
      return 6;
    case SP::G7:
      return 7;
    case SP::I0:
      return 24;
    case SP::I1:
      return 25;
    case SP::I2:
      return 26;
    case SP::I3:
      return 27;
    case SP::I4:
      return 28;
    case SP::I5:
      return 29;
    case SP::I6:
      return 30;
    case SP::I7:
      return 31;
    case SP::ICC:
      return -1;
    case SP::L0:
      return 16;
    case SP::L1:
      return 17;
    case SP::L2:
      return 18;
    case SP::L3:
      return 19;
    case SP::L4:
      return 20;
    case SP::L5:
      return 21;
    case SP::L6:
      return 22;
    case SP::L7:
      return 23;
    case SP::O0:
      return 8;
    case SP::O1:
      return 9;
    case SP::O2:
      return 10;
    case SP::O3:
      return 11;
    case SP::O4:
      return 12;
    case SP::O5:
      return 13;
    case SP::O6:
      return 14;
    case SP::O7:
      return 15;
    };
  };
}

} // End llvm namespace 
