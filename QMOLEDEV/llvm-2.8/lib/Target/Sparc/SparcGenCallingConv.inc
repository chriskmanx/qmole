//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Calling Convention Implementation Fragment
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

static bool CC_Sparc32(unsigned ValNo, EVT ValVT,
                       EVT LocVT, CCValAssign::LocInfo LocInfo,
                       ISD::ArgFlagsTy ArgFlags, CCState &State);
static bool RetCC_Sparc32(unsigned ValNo, EVT ValVT,
                          EVT LocVT, CCValAssign::LocInfo LocInfo,
                          ISD::ArgFlagsTy ArgFlags, CCState &State);


static bool CC_Sparc32(unsigned ValNo, EVT ValVT,
                       EVT LocVT, CCValAssign::LocInfo LocInfo,
                       ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i32 ||
      LocVT == MVT::f32 ||
      LocVT == MVT::f64) {
    static const unsigned RegList1[] = {
      SP::I0, SP::I1, SP::I2, SP::I3, SP::I4, SP::I5
    };
    if (unsigned Reg = State.AllocateReg(RegList1, 6)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  unsigned Offset2 = State.AllocateStack(4, 4);
  State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset2, LocVT, LocInfo));
  return false;

  return true;  // CC didn't match.
}


static bool RetCC_Sparc32(unsigned ValNo, EVT ValVT,
                          EVT LocVT, CCValAssign::LocInfo LocInfo,
                          ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i32) {
    static const unsigned RegList1[] = {
      SP::I0, SP::I1, SP::I2, SP::I3, SP::I4, SP::I5
    };
    if (unsigned Reg = State.AllocateReg(RegList1, 6)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f32) {
    static const unsigned RegList2[] = {
      SP::F0, SP::F1, SP::F2, SP::F3
    };
    if (unsigned Reg = State.AllocateReg(RegList2, 4)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f64) {
    static const unsigned RegList3[] = {
      SP::D0, SP::D1
    };
    if (unsigned Reg = State.AllocateReg(RegList3, 2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  return true;  // CC didn't match.
}
