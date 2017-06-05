//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Calling Convention Implementation Fragment
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

static bool CC_Mips(unsigned ValNo, EVT ValVT,
                    EVT LocVT, CCValAssign::LocInfo LocInfo,
                    ISD::ArgFlagsTy ArgFlags, CCState &State);
static bool CC_MipsEABI(unsigned ValNo, EVT ValVT,
                        EVT LocVT, CCValAssign::LocInfo LocInfo,
                        ISD::ArgFlagsTy ArgFlags, CCState &State);
static bool RetCC_Mips(unsigned ValNo, EVT ValVT,
                       EVT LocVT, CCValAssign::LocInfo LocInfo,
                       ISD::ArgFlagsTy ArgFlags, CCState &State);
static bool RetCC_MipsEABI(unsigned ValNo, EVT ValVT,
                           EVT LocVT, CCValAssign::LocInfo LocInfo,
                           ISD::ArgFlagsTy ArgFlags, CCState &State);
static bool RetCC_MipsO32(unsigned ValNo, EVT ValVT,
                          EVT LocVT, CCValAssign::LocInfo LocInfo,
                          ISD::ArgFlagsTy ArgFlags, CCState &State);


static bool CC_Mips(unsigned ValNo, EVT ValVT,
                    EVT LocVT, CCValAssign::LocInfo LocInfo,
                    ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (State.getTarget().getSubtarget<MipsSubtarget>().isABI_EABI()) {
    if (!CC_MipsEABI(ValNo, ValVT, LocVT, LocInfo, ArgFlags, State))
      return false;
  }

  return true;  // CC didn't match.
}


static bool CC_MipsEABI(unsigned ValNo, EVT ValVT,
                        EVT LocVT, CCValAssign::LocInfo LocInfo,
                        ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i8 ||
      LocVT == MVT::i16) {
    LocVT = MVT::i32;
    if (ArgFlags.isSExt())
        LocInfo = CCValAssign::SExt;
    else if (ArgFlags.isZExt())
        LocInfo = CCValAssign::ZExt;
    else
        LocInfo = CCValAssign::AExt;
  }

  if (LocVT == MVT::i32) {
    static const unsigned RegList1[] = {
      Mips::A0, Mips::A1, Mips::A2, Mips::A3, Mips::T0, Mips::T1, Mips::T2, Mips::T3
    };
    if (unsigned Reg = State.AllocateReg(RegList1, 8)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f32) {
    if (State.getTarget().getSubtarget<MipsSubtarget>().isSingleFloat()) {
      static const unsigned RegList2[] = {
        Mips::F12, Mips::F13, Mips::F14, Mips::F15, Mips::F16, Mips::F17, Mips::F18, Mips::F19
      };
      if (unsigned Reg = State.AllocateReg(RegList2, 8)) {
        State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
        return false;
      }
    }
  }

  if (LocVT == MVT::f32) {
    if (State.getTarget().getSubtarget<MipsSubtarget>().isNotSingleFloat()) {
      static const unsigned RegList3[] = {
        Mips::F12, Mips::F14, Mips::F16, Mips::F18
      };
      if (unsigned Reg = State.AllocateReg(RegList3, 4)) {
        State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
        return false;
      }
    }
  }

  if (LocVT == MVT::f64) {
    if (State.getTarget().getSubtarget<MipsSubtarget>().isNotSingleFloat()) {
      static const unsigned RegList4[] = {
        Mips::D6, Mips::D7, Mips::D8, Mips::D9
      };
      if (unsigned Reg = State.AllocateReg(RegList4, 4)) {
        State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
        return false;
      }
    }
  }

  if (LocVT == MVT::i32 ||
      LocVT == MVT::f32) {
    unsigned Offset5 = State.AllocateStack(4, 4);
    State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset5, LocVT, LocInfo));
    return false;
  }

  if (LocVT == MVT::f64) {
    if (State.getTarget().getSubtarget<MipsSubtarget>().isNotSingleFloat()) {
      unsigned Offset6 = State.AllocateStack(8, 8);
      State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset6, LocVT, LocInfo));
      return false;
    }
  }

  return true;  // CC didn't match.
}


static bool RetCC_Mips(unsigned ValNo, EVT ValVT,
                       EVT LocVT, CCValAssign::LocInfo LocInfo,
                       ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (State.getTarget().getSubtarget<MipsSubtarget>().isABI_EABI()) {
    if (!RetCC_MipsEABI(ValNo, ValVT, LocVT, LocInfo, ArgFlags, State))
      return false;
  }

  if (!RetCC_MipsO32(ValNo, ValVT, LocVT, LocInfo, ArgFlags, State))
    return false;

  return true;  // CC didn't match.
}


static bool RetCC_MipsEABI(unsigned ValNo, EVT ValVT,
                           EVT LocVT, CCValAssign::LocInfo LocInfo,
                           ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i32) {
    static const unsigned RegList1[] = {
      Mips::V0, Mips::V1
    };
    if (unsigned Reg = State.AllocateReg(RegList1, 2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f32) {
    static const unsigned RegList2[] = {
      Mips::F0, Mips::F1
    };
    if (unsigned Reg = State.AllocateReg(RegList2, 2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f64) {
    if (State.getTarget().getSubtarget<MipsSubtarget>().isNotSingleFloat()) {
      if (unsigned Reg = State.AllocateReg(Mips::D0)) {
        State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
        return false;
      }
    }
  }

  return true;  // CC didn't match.
}


static bool RetCC_MipsO32(unsigned ValNo, EVT ValVT,
                          EVT LocVT, CCValAssign::LocInfo LocInfo,
                          ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i32) {
    static const unsigned RegList1[] = {
      Mips::V0, Mips::V1
    };
    if (unsigned Reg = State.AllocateReg(RegList1, 2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f32) {
    static const unsigned RegList2[] = {
      Mips::F0, Mips::F2
    };
    if (unsigned Reg = State.AllocateReg(RegList2, 2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f64) {
    if (State.getTarget().getSubtarget<MipsSubtarget>().isNotSingleFloat()) {
      static const unsigned RegList3[] = {
        Mips::D0, Mips::D1
      };
      if (unsigned Reg = State.AllocateReg(RegList3, 2)) {
        State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
        return false;
      }
    }
  }

  return true;  // CC didn't match.
}
