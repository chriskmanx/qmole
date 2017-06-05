//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Calling Convention Implementation Fragment
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

static bool CC_Alpha(unsigned ValNo, EVT ValVT,
                     EVT LocVT, CCValAssign::LocInfo LocInfo,
                     ISD::ArgFlagsTy ArgFlags, CCState &State);
static bool RetCC_Alpha(unsigned ValNo, EVT ValVT,
                        EVT LocVT, CCValAssign::LocInfo LocInfo,
                        ISD::ArgFlagsTy ArgFlags, CCState &State);


static bool CC_Alpha(unsigned ValNo, EVT ValVT,
                     EVT LocVT, CCValAssign::LocInfo LocInfo,
                     ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i64) {
    static const unsigned RegList1[] = {
      Alpha::R16, Alpha::R17, Alpha::R18, Alpha::R19, Alpha::R20, Alpha::R21
    };
    static const unsigned RegList2[] = {
      Alpha::F16, Alpha::F17, Alpha::F18, Alpha::F19, Alpha::F20, Alpha::F21
    };
    if (unsigned Reg = State.AllocateReg(RegList1, RegList2, 6)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f32 ||
      LocVT == MVT::f64) {
    static const unsigned RegList3[] = {
      Alpha::F16, Alpha::F17, Alpha::F18, Alpha::F19, Alpha::F20, Alpha::F21
    };
    static const unsigned RegList4[] = {
      Alpha::R16, Alpha::R17, Alpha::R18, Alpha::R19, Alpha::R20, Alpha::R21
    };
    if (unsigned Reg = State.AllocateReg(RegList3, RegList4, 6)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::i64 ||
      LocVT == MVT::f32 ||
      LocVT == MVT::f64) {
    unsigned Offset5 = State.AllocateStack(8, 8);
    State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset5, LocVT, LocInfo));
    return false;
  }

  return true;  // CC didn't match.
}


static bool RetCC_Alpha(unsigned ValNo, EVT ValVT,
                        EVT LocVT, CCValAssign::LocInfo LocInfo,
                        ISD::ArgFlagsTy ArgFlags, CCState &State) {

  if (LocVT == MVT::i64) {
    static const unsigned RegList1[] = {
      Alpha::R0, Alpha::R1
    };
    if (unsigned Reg = State.AllocateReg(RegList1, 2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  if (LocVT == MVT::f32 ||
      LocVT == MVT::f64) {
    static const unsigned RegList2[] = {
      Alpha::F0, Alpha::F1
    };
    if (unsigned Reg = State.AllocateReg(RegList2, 2)) {
      State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));
      return false;
    }
  }

  return true;  // CC didn't match.
}
