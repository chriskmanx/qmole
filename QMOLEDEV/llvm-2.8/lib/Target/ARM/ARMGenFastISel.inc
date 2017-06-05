//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// "Fast" Instruction Selector for the ARM target
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

// FastEmit functions for ISD::Constant.

unsigned FastEmit_ISD_Constant_MVT_i32_i(MVT RetVT, uint64_t imm0) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV6T2Ops())) {
    return FastEmitInst_i(ARM::MOVi32imm, ARM::GPRRegisterClass, imm0);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_i(ARM::t2MOVi32imm, ARM::rGPRRegisterClass, imm0);
  }
  return 0;
}

unsigned FastEmit_ISD_Constant_i(MVT VT, MVT RetVT, uint64_t imm0) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_Constant_MVT_i32_i(RetVT, imm0);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned FastEmit_i(MVT VT, MVT RetVT, unsigned Opcode, uint64_t imm0) {
  switch (Opcode) {
  case ISD::Constant: return FastEmit_ISD_Constant_i(VT, RetVT, imm0);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::CALL.

unsigned FastEmit_ARMISD_CALL_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV5TOps()) && (!Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::BLX, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((!Subtarget->isThumb()) && (Subtarget->hasV5TOps()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::BLXr9, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb()) && (Subtarget->hasV5TOps()) && (!Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::tBLXr, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb()) && (Subtarget->hasV5TOps()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::tBLXr_r9, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CALL_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_CALL_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::CALL_NOLINK.

unsigned FastEmit_ARMISD_CALL_NOLINK_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb()) && (!Subtarget->hasV4TOps()) && (!Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::BMOVPCRX, ARM::tGPRRegisterClass, Op0, Op0IsKill);
  }
  if ((!Subtarget->isThumb()) && (!Subtarget->hasV4TOps()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::BMOVPCRXr9, ARM::tGPRRegisterClass, Op0, Op0IsKill);
  }
  if ((!Subtarget->isThumb()) && (Subtarget->hasV4TOps()) && (!Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::BX, ARM::tGPRRegisterClass, Op0, Op0IsKill);
  }
  if ((!Subtarget->isThumb()) && (Subtarget->hasV4TOps()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::BXr9, ARM::tGPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb1Only()) && (!Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::tBX, ARM::tGPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb1Only()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::tBXr9, ARM::tGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CALL_NOLINK_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_CALL_NOLINK_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::CMPFPw0.

unsigned FastEmit_ARMISD_CMPFPw0_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VCMPEZS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CMPFPw0_MVT_f64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VCMPEZD, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CMPFPw0_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_CMPFPw0_MVT_f32_r(RetVT, Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ARMISD_CMPFPw0_MVT_f64_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::FTOSI.

unsigned FastEmit_ARMISD_FTOSI_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_r(ARM::VTOSIZS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_FTOSI_MVT_f64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VTOSIZD, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_FTOSI_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_FTOSI_MVT_f32_r(RetVT, Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ARMISD_FTOSI_MVT_f64_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::FTOUI.

unsigned FastEmit_ARMISD_FTOUI_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_r(ARM::VTOUIZS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_FTOUI_MVT_f64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VTOUIZD, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_FTOUI_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_FTOUI_MVT_f32_r(RetVT, Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ARMISD_FTOUI_MVT_f64_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::MEMBARRIER.

unsigned FastEmit_ARMISD_MEMBARRIER_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV6Ops())) {
    return FastEmitInst_r(ARM::DMB_MCR, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_MEMBARRIER_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_MEMBARRIER_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::RBIT.

unsigned FastEmit_ARMISD_RBIT_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV6T2Ops())) {
    return FastEmitInst_r(ARM::RBIT, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_r(ARM::t2RBIT, ARM::rGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_RBIT_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_RBIT_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::RRX.

unsigned FastEmit_ARMISD_RRX_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_r(ARM::MOVrx, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_r(ARM::t2MOVrx, ARM::rGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_RRX_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_RRX_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::SITOF.

unsigned FastEmit_ARMISD_SITOF_MVT_f32_MVT_f32_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_r(ARM::VSITOS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_SITOF_MVT_f32_MVT_f64_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VSITOD, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_SITOF_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
switch (RetVT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_SITOF_MVT_f32_MVT_f32_r(Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ARMISD_SITOF_MVT_f32_MVT_f64_r(Op0, Op0IsKill);
  default: return 0;
}
}

unsigned FastEmit_ARMISD_SITOF_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_SITOF_MVT_f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::SRA_FLAG.

unsigned FastEmit_ARMISD_SRA_FLAG_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_r(ARM::MOVsra_flag, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_r(ARM::t2MOVsra_flag, ARM::rGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_SRA_FLAG_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_SRA_FLAG_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::SRL_FLAG.

unsigned FastEmit_ARMISD_SRL_FLAG_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_r(ARM::MOVsrl_flag, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_r(ARM::t2MOVsrl_flag, ARM::rGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_SRL_FLAG_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_SRL_FLAG_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::SYNCBARRIER.

unsigned FastEmit_ARMISD_SYNCBARRIER_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV6Ops())) {
    return FastEmitInst_r(ARM::DSB_MCR, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_SYNCBARRIER_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_SYNCBARRIER_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::TC_RETURN.

unsigned FastEmit_ARMISD_TC_RETURN_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::TCRETURNriND, ARM::tcGPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::TCRETURNri, ARM::tcGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_TC_RETURN_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_TC_RETURN_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::UITOF.

unsigned FastEmit_ARMISD_UITOF_MVT_f32_MVT_f32_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_r(ARM::VUITOS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_UITOF_MVT_f32_MVT_f64_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VUITOD, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_UITOF_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
switch (RetVT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_UITOF_MVT_f32_MVT_f32_r(Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ARMISD_UITOF_MVT_f32_MVT_f64_r(Op0, Op0IsKill);
  default: return 0;
}
}

unsigned FastEmit_ARMISD_UITOF_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_UITOF_MVT_f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VDUP.

unsigned FastEmit_ARMISD_VDUP_MVT_i32_MVT_v8i8_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUP8d, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_i32_MVT_v16i8_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUP8q, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_i32_MVT_v4i16_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUP16d, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_i32_MVT_v8i16_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUP16q, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_i32_MVT_v2i32_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUP32d, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_i32_MVT_v4i32_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUP32q, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
switch (RetVT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VDUP_MVT_i32_MVT_v8i8_r(Op0, Op0IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VDUP_MVT_i32_MVT_v16i8_r(Op0, Op0IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VDUP_MVT_i32_MVT_v4i16_r(Op0, Op0IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VDUP_MVT_i32_MVT_v8i16_r(Op0, Op0IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VDUP_MVT_i32_MVT_v2i32_r(Op0, Op0IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VDUP_MVT_i32_MVT_v4i32_r(Op0, Op0IsKill);
  default: return 0;
}
}

unsigned FastEmit_ARMISD_VDUP_MVT_f32_MVT_v2f32_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUPfdf, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_f32_MVT_v4f32_r(unsigned Op0, bool Op0IsKill) {
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VDUPfqf, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VDUP_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
switch (RetVT.SimpleTy) {
  case MVT::v2f32: return FastEmit_ARMISD_VDUP_MVT_f32_MVT_v2f32_r(Op0, Op0IsKill);
  case MVT::v4f32: return FastEmit_ARMISD_VDUP_MVT_f32_MVT_v4f32_r(Op0, Op0IsKill);
  default: return 0;
}
}

unsigned FastEmit_ARMISD_VDUP_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_VDUP_MVT_i32_r(RetVT, Op0, Op0IsKill);
  case MVT::f32: return FastEmit_ARMISD_VDUP_MVT_f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VREV16.

unsigned FastEmit_ARMISD_VREV16_MVT_v8i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV16d8, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV16_MVT_v16i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV16q8, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV16_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VREV16_MVT_v8i8_r(RetVT, Op0, Op0IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VREV16_MVT_v16i8_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VREV32.

unsigned FastEmit_ARMISD_VREV32_MVT_v8i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV32d8, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV32_MVT_v16i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV32q8, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV32_MVT_v4i16_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV32d16, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV32_MVT_v8i16_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV32q16, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV32_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VREV32_MVT_v8i8_r(RetVT, Op0, Op0IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VREV32_MVT_v16i8_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VREV32_MVT_v4i16_r(RetVT, Op0, Op0IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VREV32_MVT_v8i16_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VREV64.

unsigned FastEmit_ARMISD_VREV64_MVT_v8i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64d8, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_MVT_v16i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64q8, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_MVT_v4i16_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64d16, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_MVT_v8i16_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64q16, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_MVT_v2i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64d32, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_MVT_v4i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64q32, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_MVT_v2f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64df, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_MVT_v4f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VREV64qf, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VREV64_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VREV64_MVT_v8i8_r(RetVT, Op0, Op0IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VREV64_MVT_v16i8_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VREV64_MVT_v4i16_r(RetVT, Op0, Op0IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VREV64_MVT_v8i16_r(RetVT, Op0, Op0IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VREV64_MVT_v2i32_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VREV64_MVT_v4i32_r(RetVT, Op0, Op0IsKill);
  case MVT::v2f32: return FastEmit_ARMISD_VREV64_MVT_v2f32_r(RetVT, Op0, Op0IsKill);
  case MVT::v4f32: return FastEmit_ARMISD_VREV64_MVT_v4f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::tCALL.

unsigned FastEmit_ARMISD_tCALL_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((Subtarget->isThumb()) && (Subtarget->hasV5TOps()) && (!Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::tBLXr, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb()) && (Subtarget->hasV5TOps()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_r(ARM::tBLXr_r9, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_tCALL_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_tCALL_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::BIT_CONVERT.

unsigned FastEmit_ISD_BIT_CONVERT_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VMOVSR, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_BIT_CONVERT_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VMOVRS, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_BIT_CONVERT_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_BIT_CONVERT_MVT_i32_r(RetVT, Op0, Op0IsKill);
  case MVT::f32: return FastEmit_ISD_BIT_CONVERT_MVT_f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::BRIND.

unsigned FastEmit_ISD_BRIND_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb()) && (!Subtarget->hasV4TOps())) {
    return FastEmitInst_r(ARM::MOVPCRX, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((!Subtarget->isThumb()) && (Subtarget->hasV4TOps())) {
    return FastEmitInst_r(ARM::BRIND, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb())) {
    return FastEmitInst_r(ARM::tBRIND, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_BRIND_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_BRIND_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::BSWAP.

unsigned FastEmit_ISD_BSWAP_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV6Ops())) {
    return FastEmitInst_r(ARM::REV, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb1Only()) && (Subtarget->hasV6Ops())) {
    return FastEmitInst_r(ARM::tREV, ARM::tGPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_r(ARM::t2REV, ARM::rGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_BSWAP_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_BSWAP_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::CTLZ.

unsigned FastEmit_ISD_CTLZ_MVT_i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV5TOps())) {
    return FastEmitInst_r(ARM::CLZ, ARM::GPRRegisterClass, Op0, Op0IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_r(ARM::t2CLZ, ARM::rGPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_CTLZ_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_CTLZ_MVT_i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FABS.

unsigned FastEmit_ISD_FABS_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_r(ARM::VABSS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FABS_MVT_f64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VABSD, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FABS_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FABS_MVT_f32_r(RetVT, Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ISD_FABS_MVT_f64_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FNEG.

unsigned FastEmit_ISD_FNEG_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_r(ARM::VNEGS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FNEG_MVT_f64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VNEGD, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FNEG_MVT_v2f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VNEGfd, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FNEG_MVT_v4f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VNEGf32q, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FNEG_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FNEG_MVT_f32_r(RetVT, Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ISD_FNEG_MVT_f64_r(RetVT, Op0, Op0IsKill);
  case MVT::v2f32: return FastEmit_ISD_FNEG_MVT_v2f32_r(RetVT, Op0, Op0IsKill);
  case MVT::v4f32: return FastEmit_ISD_FNEG_MVT_v4f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_EXTEND.

unsigned FastEmit_ISD_FP_EXTEND_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VCVTDS, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FP_EXTEND_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FP_EXTEND_MVT_f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_ROUND.

unsigned FastEmit_ISD_FP_ROUND_MVT_f64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VCVTSD, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FP_ROUND_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f64: return FastEmit_ISD_FP_ROUND_MVT_f64_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_TO_SINT.

unsigned FastEmit_ISD_FP_TO_SINT_MVT_v2f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTf2sd, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FP_TO_SINT_MVT_v4f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTf2sq, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FP_TO_SINT_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v2f32: return FastEmit_ISD_FP_TO_SINT_MVT_v2f32_r(RetVT, Op0, Op0IsKill);
  case MVT::v4f32: return FastEmit_ISD_FP_TO_SINT_MVT_v4f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FP_TO_UINT.

unsigned FastEmit_ISD_FP_TO_UINT_MVT_v2f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTf2ud, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FP_TO_UINT_MVT_v4f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTf2uq, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FP_TO_UINT_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v2f32: return FastEmit_ISD_FP_TO_UINT_MVT_v2f32_r(RetVT, Op0, Op0IsKill);
  case MVT::v4f32: return FastEmit_ISD_FP_TO_UINT_MVT_v4f32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FSQRT.

unsigned FastEmit_ISD_FSQRT_MVT_f32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VSQRTS, ARM::SPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FSQRT_MVT_f64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_r(ARM::VSQRTD, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FSQRT_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FSQRT_MVT_f32_r(RetVT, Op0, Op0IsKill);
  case MVT::f64: return FastEmit_ISD_FSQRT_MVT_f64_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SIGN_EXTEND.

unsigned FastEmit_ISD_SIGN_EXTEND_MVT_v8i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVLsv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SIGN_EXTEND_MVT_v4i16_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVLsv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SIGN_EXTEND_MVT_v2i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVLsv2i64, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SIGN_EXTEND_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ISD_SIGN_EXTEND_MVT_v8i8_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i16: return FastEmit_ISD_SIGN_EXTEND_MVT_v4i16_r(RetVT, Op0, Op0IsKill);
  case MVT::v2i32: return FastEmit_ISD_SIGN_EXTEND_MVT_v2i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SINT_TO_FP.

unsigned FastEmit_ISD_SINT_TO_FP_MVT_v2i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTs2fd, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SINT_TO_FP_MVT_v4i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTs2fq, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SINT_TO_FP_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v2i32: return FastEmit_ISD_SINT_TO_FP_MVT_v2i32_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i32: return FastEmit_ISD_SINT_TO_FP_MVT_v4i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::TRUNCATE.

unsigned FastEmit_ISD_TRUNCATE_MVT_v8i16_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVNv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_TRUNCATE_MVT_v4i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVNv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_TRUNCATE_MVT_v2i64_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVNv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_TRUNCATE_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i16: return FastEmit_ISD_TRUNCATE_MVT_v8i16_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i32: return FastEmit_ISD_TRUNCATE_MVT_v4i32_r(RetVT, Op0, Op0IsKill);
  case MVT::v2i64: return FastEmit_ISD_TRUNCATE_MVT_v2i64_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::UINT_TO_FP.

unsigned FastEmit_ISD_UINT_TO_FP_MVT_v2i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTu2fd, ARM::DPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_UINT_TO_FP_MVT_v4i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VCVTu2fq, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_UINT_TO_FP_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v2i32: return FastEmit_ISD_UINT_TO_FP_MVT_v2i32_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i32: return FastEmit_ISD_UINT_TO_FP_MVT_v4i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::ZERO_EXTEND.

unsigned FastEmit_ISD_ZERO_EXTEND_MVT_v8i8_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVLuv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ZERO_EXTEND_MVT_v4i16_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVLuv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ZERO_EXTEND_MVT_v2i32_r(MVT RetVT, unsigned Op0, bool Op0IsKill) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_r(ARM::VMOVLuv2i64, ARM::QPRRegisterClass, Op0, Op0IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ZERO_EXTEND_r(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ISD_ZERO_EXTEND_MVT_v8i8_r(RetVT, Op0, Op0IsKill);
  case MVT::v4i16: return FastEmit_ISD_ZERO_EXTEND_MVT_v4i16_r(RetVT, Op0, Op0IsKill);
  case MVT::v2i32: return FastEmit_ISD_ZERO_EXTEND_MVT_v2i32_r(RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned FastEmit_r(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, bool Op0IsKill) {
  switch (Opcode) {
  case ARMISD::CALL: return FastEmit_ARMISD_CALL_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::CALL_NOLINK: return FastEmit_ARMISD_CALL_NOLINK_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::CMPFPw0: return FastEmit_ARMISD_CMPFPw0_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::FTOSI: return FastEmit_ARMISD_FTOSI_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::FTOUI: return FastEmit_ARMISD_FTOUI_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::MEMBARRIER: return FastEmit_ARMISD_MEMBARRIER_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::RBIT: return FastEmit_ARMISD_RBIT_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::RRX: return FastEmit_ARMISD_RRX_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::SITOF: return FastEmit_ARMISD_SITOF_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::SRA_FLAG: return FastEmit_ARMISD_SRA_FLAG_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::SRL_FLAG: return FastEmit_ARMISD_SRL_FLAG_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::SYNCBARRIER: return FastEmit_ARMISD_SYNCBARRIER_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::TC_RETURN: return FastEmit_ARMISD_TC_RETURN_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::UITOF: return FastEmit_ARMISD_UITOF_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::VDUP: return FastEmit_ARMISD_VDUP_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::VREV16: return FastEmit_ARMISD_VREV16_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::VREV32: return FastEmit_ARMISD_VREV32_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::VREV64: return FastEmit_ARMISD_VREV64_r(VT, RetVT, Op0, Op0IsKill);
  case ARMISD::tCALL: return FastEmit_ARMISD_tCALL_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::BIT_CONVERT: return FastEmit_ISD_BIT_CONVERT_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::BRIND: return FastEmit_ISD_BRIND_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::BSWAP: return FastEmit_ISD_BSWAP_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::CTLZ: return FastEmit_ISD_CTLZ_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::FABS: return FastEmit_ISD_FABS_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::FNEG: return FastEmit_ISD_FNEG_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::FP_EXTEND: return FastEmit_ISD_FP_EXTEND_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::FP_ROUND: return FastEmit_ISD_FP_ROUND_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::FP_TO_SINT: return FastEmit_ISD_FP_TO_SINT_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::FP_TO_UINT: return FastEmit_ISD_FP_TO_UINT_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::FSQRT: return FastEmit_ISD_FSQRT_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::SIGN_EXTEND: return FastEmit_ISD_SIGN_EXTEND_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::SINT_TO_FP: return FastEmit_ISD_SINT_TO_FP_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::TRUNCATE: return FastEmit_ISD_TRUNCATE_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::UINT_TO_FP: return FastEmit_ISD_UINT_TO_FP_r(VT, RetVT, Op0, Op0IsKill);
  case ISD::ZERO_EXTEND: return FastEmit_ISD_ZERO_EXTEND_r(VT, RetVT, Op0, Op0IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::PIC_ADD.

unsigned FastEmit_ARMISD_PIC_ADD_MVT_i32_ri(MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_ri(ARM::PICADD, ARM::GPRRegisterClass, Op0, Op0IsKill, imm1);
  }
  if ((Subtarget->isThumb())) {
    return FastEmitInst_ri(ARM::tPICADD, ARM::GPRRegisterClass, Op0, Op0IsKill, imm1);
  }
  return 0;
}

unsigned FastEmit_ARMISD_PIC_ADD_ri(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_PIC_ADD_MVT_i32_ri(RetVT, Op0, Op0IsKill, imm1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SHL.

unsigned FastEmit_ISD_SHL_MVT_i32_ri(MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_ri(ARM::tLSLri, ARM::tGPRRegisterClass, Op0, Op0IsKill, imm1);
  }
  return 0;
}

unsigned FastEmit_ISD_SHL_ri(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SHL_MVT_i32_ri(RetVT, Op0, Op0IsKill, imm1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SRA.

unsigned FastEmit_ISD_SRA_MVT_i32_ri(MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_ri(ARM::tASRri, ARM::tGPRRegisterClass, Op0, Op0IsKill, imm1);
  }
  return 0;
}

unsigned FastEmit_ISD_SRA_ri(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SRA_MVT_i32_ri(RetVT, Op0, Op0IsKill, imm1);
  default: return 0;
  }
}

// FastEmit functions for ISD::SRL.

unsigned FastEmit_ISD_SRL_MVT_i32_ri(MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_ri(ARM::tLSRri, ARM::tGPRRegisterClass, Op0, Op0IsKill, imm1);
  }
  return 0;
}

unsigned FastEmit_ISD_SRL_ri(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SRL_MVT_i32_ri(RetVT, Op0, Op0IsKill, imm1);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned FastEmit_ri(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, bool Op0IsKill, uint64_t imm1) {
  switch (Opcode) {
  case ARMISD::PIC_ADD: return FastEmit_ARMISD_PIC_ADD_ri(VT, RetVT, Op0, Op0IsKill, imm1);
  case ISD::SHL: return FastEmit_ISD_SHL_ri(VT, RetVT, Op0, Op0IsKill, imm1);
  case ISD::SRA: return FastEmit_ISD_SRA_ri(VT, RetVT, Op0, Op0IsKill, imm1);
  case ISD::SRL: return FastEmit_ISD_SRL_ri(VT, RetVT, Op0, Op0IsKill, imm1);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::AND.

unsigned FastEmit_ARMISD_AND_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::ANDSrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2ANDSrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_AND_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_AND_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::CMP.

unsigned FastEmit_ARMISD_CMP_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::CMPrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tCMPr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2CMPrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CMP_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_CMP_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::CMPFP.

unsigned FastEmit_ARMISD_CMPFP_MVT_f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VCMPES, ARM::SPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CMPFP_MVT_f64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VCMPED, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CMPFP_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ARMISD_CMPFP_MVT_f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::f64: return FastEmit_ARMISD_CMPFP_MVT_f64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::CMPZ.

unsigned FastEmit_ARMISD_CMPZ_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::CMPzrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tCMPzr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2CMPzrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_CMPZ_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_CMPZ_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::EH_SJLJ_LONGJMP.

unsigned FastEmit_ARMISD_EH_SJLJ_LONGJMP_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::isVoid)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_rr(ARM::Int_eh_sjlj_longjmp, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb()) && (Subtarget->isTargetDarwin())) {
    return FastEmitInst_rr(ARM::tInt_eh_sjlj_longjmp, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_EH_SJLJ_LONGJMP_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_EH_SJLJ_LONGJMP_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::EH_SJLJ_SETJMP.

unsigned FastEmit_ARMISD_EH_SJLJ_SETJMP_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb()) && (!Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::Int_eh_sjlj_setjmp_nofp, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((!Subtarget->isThumb()) && (Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::Int_eh_sjlj_setjmp, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tInt_eh_sjlj_setjmp, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2()) && (!Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::t2Int_eh_sjlj_setjmp_nofp, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2()) && (Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::t2Int_eh_sjlj_setjmp, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_EH_SJLJ_SETJMP_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_EH_SJLJ_SETJMP_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VCEQ.

unsigned FastEmit_ARMISD_VCEQ_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_MVT_v2f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQfd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_MVT_v4f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCEQfq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCEQ_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VCEQ_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VCEQ_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VCEQ_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VCEQ_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VCEQ_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VCEQ_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2f32: return FastEmit_ARMISD_VCEQ_MVT_v2f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4f32: return FastEmit_ARMISD_VCEQ_MVT_v4f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VCGE.

unsigned FastEmit_ARMISD_VCGE_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEsv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEsv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEsv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEsv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEsv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEsv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_MVT_v2f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEfd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_MVT_v4f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEfq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGE_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VCGE_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VCGE_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VCGE_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VCGE_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VCGE_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VCGE_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2f32: return FastEmit_ARMISD_VCGE_MVT_v2f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4f32: return FastEmit_ARMISD_VCGE_MVT_v4f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VCGEU.

unsigned FastEmit_ARMISD_VCGEU_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEuv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGEU_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEuv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGEU_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEuv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGEU_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEuv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGEU_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEuv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGEU_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGEuv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGEU_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VCGEU_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VCGEU_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VCGEU_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VCGEU_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VCGEU_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VCGEU_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VCGT.

unsigned FastEmit_ARMISD_VCGT_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTsv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTsv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTsv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTsv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTsv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTsv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_MVT_v2f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTfd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_MVT_v4f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTfq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGT_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VCGT_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VCGT_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VCGT_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VCGT_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VCGT_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VCGT_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2f32: return FastEmit_ARMISD_VCGT_MVT_v2f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4f32: return FastEmit_ARMISD_VCGT_MVT_v4f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VCGTU.

unsigned FastEmit_ARMISD_VCGTU_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTuv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGTU_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTuv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGTU_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTuv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGTU_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTuv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGTU_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTuv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGTU_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VCGTuv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VCGTU_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VCGTU_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VCGTU_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VCGTU_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VCGTU_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VCGTU_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VCGTU_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VMOVDRR.

unsigned FastEmit_ARMISD_VMOVDRR_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VMOVDRR, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VMOVDRR_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ARMISD_VMOVDRR_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VMULLs.

unsigned FastEmit_ARMISD_VMULLs_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULLsv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VMULLs_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULLsv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VMULLs_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULLsv2i64, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VMULLs_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VMULLs_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VMULLs_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VMULLs_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VMULLu.

unsigned FastEmit_ARMISD_VMULLu_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULLuv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VMULLu_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULLuv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VMULLu_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULLuv2i64, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VMULLu_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VMULLu_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VMULLu_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VMULLu_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ARMISD::VTST.

unsigned FastEmit_ARMISD_VTST_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VTSTv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VTST_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VTSTv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VTST_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VTSTv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VTST_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VTSTv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VTST_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VTSTv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VTST_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VTSTv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ARMISD_VTST_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::v8i8: return FastEmit_ARMISD_VTST_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ARMISD_VTST_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ARMISD_VTST_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ARMISD_VTST_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ARMISD_VTST_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ARMISD_VTST_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::ADD.

unsigned FastEmit_ISD_ADD_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::ADDrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tADDrr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2ADDrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v1i64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v1i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv1i64, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_MVT_v2i64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDv2i64, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADD_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_ADD_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i8: return FastEmit_ISD_ADD_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ISD_ADD_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ISD_ADD_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ISD_ADD_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ISD_ADD_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ISD_ADD_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v1i64: return FastEmit_ISD_ADD_MVT_v1i64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i64: return FastEmit_ISD_ADD_MVT_v2i64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::ADDC.

unsigned FastEmit_ISD_ADDC_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::ADDSrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tADDrr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2ADDSrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADDC_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_ADDC_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::ADDE.

unsigned FastEmit_ISD_ADDE_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tADC, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ADDE_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_ADDE_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::AND.

unsigned FastEmit_ISD_AND_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::ANDrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tAND, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2ANDrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_AND_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VANDd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_AND_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VANDq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_AND_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_AND_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ISD_AND_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ISD_AND_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FADD.

unsigned FastEmit_ISD_FADD_MVT_f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_rr(ARM::VADDS, ARM::SPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FADD_MVT_f64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VADDD, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FADD_MVT_v2f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDfd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FADD_MVT_v4f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VADDfq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FADD_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FADD_MVT_f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::f64: return FastEmit_ISD_FADD_MVT_f64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2f32: return FastEmit_ISD_FADD_MVT_v2f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4f32: return FastEmit_ISD_FADD_MVT_v4f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FDIV.

unsigned FastEmit_ISD_FDIV_MVT_f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VDIVS, ARM::SPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FDIV_MVT_f64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VDIVD, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FDIV_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FDIV_MVT_f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::f64: return FastEmit_ISD_FDIV_MVT_f64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FMUL.

unsigned FastEmit_ISD_FMUL_MVT_f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_rr(ARM::VMULS, ARM::SPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FMUL_MVT_f64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VMULD, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FMUL_MVT_v2f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULfd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FMUL_MVT_v4f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULfq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FMUL_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FMUL_MVT_f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::f64: return FastEmit_ISD_FMUL_MVT_f64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2f32: return FastEmit_ISD_FMUL_MVT_v2f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4f32: return FastEmit_ISD_FMUL_MVT_v4f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::FSUB.

unsigned FastEmit_ISD_FSUB_MVT_f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f32)
    return 0;
  if ((Subtarget->hasVFP2()) && (!Subtarget->useNEONForSinglePrecisionFP())) {
    return FastEmitInst_rr(ARM::VSUBS, ARM::SPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FSUB_MVT_f64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::f64)
    return 0;
  if ((Subtarget->hasVFP2())) {
    return FastEmitInst_rr(ARM::VSUBD, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FSUB_MVT_v2f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBfd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FSUB_MVT_v4f32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4f32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBfq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_FSUB_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::f32: return FastEmit_ISD_FSUB_MVT_f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::f64: return FastEmit_ISD_FSUB_MVT_f64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2f32: return FastEmit_ISD_FSUB_MVT_v2f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4f32: return FastEmit_ISD_FSUB_MVT_v4f32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::MUL.

unsigned FastEmit_ISD_MUL_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::MUL, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tMUL, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2MUL, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MUL_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MUL_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MUL_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MUL_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MUL_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MUL_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VMULv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MUL_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_MUL_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i8: return FastEmit_ISD_MUL_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ISD_MUL_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ISD_MUL_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ISD_MUL_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ISD_MUL_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ISD_MUL_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::MULHS.

unsigned FastEmit_ISD_MULHS_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb()) && (Subtarget->hasV6Ops())) {
    return FastEmitInst_rr(ARM::SMMUL, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2SMMUL, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_MULHS_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_MULHS_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::OR.

unsigned FastEmit_ISD_OR_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::ORRrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tORR, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2ORRrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_OR_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VORRd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_OR_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VORRq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_OR_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_OR_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ISD_OR_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ISD_OR_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::ROTR.

unsigned FastEmit_ISD_ROTR_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tROR, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2RORrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_ROTR_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_ROTR_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SDIV.

unsigned FastEmit_ISD_SDIV_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->hasDivide())) {
    return FastEmitInst_rr(ARM::t2SDIV, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SDIV_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SDIV_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SHL.

unsigned FastEmit_ISD_SHL_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tLSLrr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2LSLrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SHL_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SHL_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SRA.

unsigned FastEmit_ISD_SRA_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tASRrr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2ASRrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SRA_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SRA_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SRL.

unsigned FastEmit_ISD_SRL_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tLSRrr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2LSRrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SRL_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SRL_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SUB.

unsigned FastEmit_ISD_SUB_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::SUBrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tSUBrr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2SUBrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v8i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv8i8, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v16i8_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v16i8)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv16i8, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v4i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv4i16, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v8i16_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v8i16)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv8i16, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv2i32, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv4i32, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v1i64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v1i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv1i64, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_MVT_v2i64_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i64)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VSUBv2i64, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUB_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SUB_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i8: return FastEmit_ISD_SUB_MVT_v8i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v16i8: return FastEmit_ISD_SUB_MVT_v16i8_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i16: return FastEmit_ISD_SUB_MVT_v4i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v8i16: return FastEmit_ISD_SUB_MVT_v8i16_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ISD_SUB_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ISD_SUB_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v1i64: return FastEmit_ISD_SUB_MVT_v1i64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i64: return FastEmit_ISD_SUB_MVT_v2i64_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SUBC.

unsigned FastEmit_ISD_SUBC_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::SUBSrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tSUBrr, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2SUBSrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUBC_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SUBC_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::SUBE.

unsigned FastEmit_ISD_SUBE_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tSBC, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_SUBE_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_SUBE_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::UDIV.

unsigned FastEmit_ISD_UDIV_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((Subtarget->hasDivide())) {
    return FastEmitInst_rr(ARM::t2UDIV, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_UDIV_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_UDIV_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// FastEmit functions for ISD::XOR.

unsigned FastEmit_ISD_XOR_MVT_i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::i32)
    return 0;
  if ((!Subtarget->isThumb())) {
    return FastEmitInst_rr(ARM::EORrr, ARM::GPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb1Only())) {
    return FastEmitInst_rr(ARM::tEOR, ARM::tGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  if ((Subtarget->isThumb2())) {
    return FastEmitInst_rr(ARM::t2EORrr, ARM::rGPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_XOR_MVT_v2i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v2i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VEORd, ARM::DPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_XOR_MVT_v4i32_rr(MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  if (RetVT.SimpleTy != MVT::v4i32)
    return 0;
  if ((Subtarget->hasNEON())) {
    return FastEmitInst_rr(ARM::VEORq, ARM::QPRRegisterClass, Op0, Op0IsKill, Op1, Op1IsKill);
  }
  return 0;
}

unsigned FastEmit_ISD_XOR_rr(MVT VT, MVT RetVT, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (VT.SimpleTy) {
  case MVT::i32: return FastEmit_ISD_XOR_MVT_i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v2i32: return FastEmit_ISD_XOR_MVT_v2i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case MVT::v4i32: return FastEmit_ISD_XOR_MVT_v4i32_rr(RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

// Top-level FastEmit function.

unsigned FastEmit_rr(MVT VT, MVT RetVT, unsigned Opcode, unsigned Op0, bool Op0IsKill, unsigned Op1, bool Op1IsKill) {
  switch (Opcode) {
  case ARMISD::AND: return FastEmit_ARMISD_AND_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::CMP: return FastEmit_ARMISD_CMP_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::CMPFP: return FastEmit_ARMISD_CMPFP_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::CMPZ: return FastEmit_ARMISD_CMPZ_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::EH_SJLJ_LONGJMP: return FastEmit_ARMISD_EH_SJLJ_LONGJMP_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::EH_SJLJ_SETJMP: return FastEmit_ARMISD_EH_SJLJ_SETJMP_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VCEQ: return FastEmit_ARMISD_VCEQ_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VCGE: return FastEmit_ARMISD_VCGE_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VCGEU: return FastEmit_ARMISD_VCGEU_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VCGT: return FastEmit_ARMISD_VCGT_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VCGTU: return FastEmit_ARMISD_VCGTU_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VMOVDRR: return FastEmit_ARMISD_VMOVDRR_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VMULLs: return FastEmit_ARMISD_VMULLs_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VMULLu: return FastEmit_ARMISD_VMULLu_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ARMISD::VTST: return FastEmit_ARMISD_VTST_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::ADD: return FastEmit_ISD_ADD_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::ADDC: return FastEmit_ISD_ADDC_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::ADDE: return FastEmit_ISD_ADDE_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::AND: return FastEmit_ISD_AND_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::FADD: return FastEmit_ISD_FADD_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::FDIV: return FastEmit_ISD_FDIV_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::FMUL: return FastEmit_ISD_FMUL_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::FSUB: return FastEmit_ISD_FSUB_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::MUL: return FastEmit_ISD_MUL_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::MULHS: return FastEmit_ISD_MULHS_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::OR: return FastEmit_ISD_OR_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::ROTR: return FastEmit_ISD_ROTR_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::SDIV: return FastEmit_ISD_SDIV_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::SHL: return FastEmit_ISD_SHL_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::SRA: return FastEmit_ISD_SRA_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::SRL: return FastEmit_ISD_SRL_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::SUB: return FastEmit_ISD_SUB_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::SUBC: return FastEmit_ISD_SUBC_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::SUBE: return FastEmit_ISD_SUBE_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::UDIV: return FastEmit_ISD_UDIV_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  case ISD::XOR: return FastEmit_ISD_XOR_rr(VT, RetVT, Op0, Op0IsKill, Op1, Op1IsKill);
  default: return 0;
  }
}

