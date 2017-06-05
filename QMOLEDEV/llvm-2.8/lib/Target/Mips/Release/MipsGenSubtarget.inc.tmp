//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Subtarget Enumeration Source Fragment
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/SubtargetFeature.h"
#include "llvm/Target/TargetInstrItineraries.h"

enum {
  FeatureBitCount =  1 << 0,
  FeatureCondMov =  1 << 1,
  FeatureEABI =  1 << 2,
  FeatureFP64Bit =  1 << 3,
  FeatureGP64Bit =  1 << 4,
  FeatureMinMax =  1 << 5,
  FeatureMips1 =  1 << 6,
  FeatureMips2 =  1 << 7,
  FeatureMulDivAdd =  1 << 8,
  FeatureO32 =  1 << 9,
  FeatureSEInReg =  1 << 10,
  FeatureSingleFloat =  1 << 11,
  FeatureSwap =  1 << 12,
  FeatureVFPU =  1 << 13
};

// Sorted (by key) array of values for CPU features.
static const llvm::SubtargetFeatureKV FeatureKV[] = {
  { "bitcount", "Enable 'count leading bits' instructions.", FeatureBitCount, 0 },
  { "condmov", "Enable 'conditional move' instructions.", FeatureCondMov, 0 },
  { "eabi", "Enable eabi ABI", FeatureEABI, 0 },
  { "fp64", "Support 64-bit FP registers.", FeatureFP64Bit, 0 },
  { "gp64", "General Purpose Registers are 64-bit wide.", FeatureGP64Bit, 0 },
  { "minmax", "Enable 'min/max' instructions.", FeatureMinMax, 0 },
  { "mips1", "Mips1 ISA Support", FeatureMips1, 0 },
  { "mips2", "Mips2 ISA Support", FeatureMips2, 0 },
  { "muldivadd", "Enable 'multiply add/sub' instructions.", FeatureMulDivAdd, 0 },
  { "o32", "Enable o32 ABI", FeatureO32, 0 },
  { "seinreg", "Enable 'signext in register' instructions.", FeatureSEInReg, 0 },
  { "single-float", "Only supports single precision float", FeatureSingleFloat, 0 },
  { "swap", "Enable 'byte/half swap' instructions.", FeatureSwap, 0 },
  { "vfpu", "Enable vector FPU instructions.", FeatureVFPU, 0 }
};

enum {
  FeatureKVSize = sizeof(FeatureKV)/sizeof(llvm::SubtargetFeatureKV)
};

// Sorted (by key) array of values for CPU subtype.
static const llvm::SubtargetFeatureKV SubTypeKV[] = {
  { "allegrex", "Select the allegrex processor", FeatureMips2 | FeatureSingleFloat | FeatureEABI | FeatureVFPU | FeatureSEInReg | FeatureCondMov | FeatureMulDivAdd | FeatureMinMax | FeatureSwap | FeatureBitCount, 0 },
  { "mips1", "Select the mips1 processor", FeatureMips1, 0 },
  { "mips2", "Select the mips2 processor", FeatureMips2, 0 },
  { "r2000", "Select the r2000 processor", FeatureMips1, 0 },
  { "r3000", "Select the r3000 processor", FeatureMips1, 0 },
  { "r6000", "Select the r6000 processor", FeatureMips2, 0 }
};

enum {
  SubTypeKVSize = sizeof(SubTypeKV)/sizeof(llvm::SubtargetFeatureKV)
};


enum {
  ItinClassesSize = 21
};

// Functional units for itineraries "MipsGenericItineraries"
namespace MipsGenericItinerariesFU {
  const unsigned ALU = 1 << 0;
  const unsigned IMULDIV = 1 << 1;
}

static const llvm::InstrStage Stages[] = {
  { 0, 0, 0, llvm::InstrStage::Required }, // No itinerary
  { 1, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 1
  { 3, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 2
  { 2, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 3
  { 1, MipsGenericItinerariesFU::IMULDIV, -1, (llvm::InstrStage::ReservationKinds)0 }, // 4
  { 17, MipsGenericItinerariesFU::IMULDIV, -1, (llvm::InstrStage::ReservationKinds)0 }, // 5
  { 38, MipsGenericItinerariesFU::IMULDIV, -1, (llvm::InstrStage::ReservationKinds)0 }, // 6
  { 4, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 7
  { 7, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 8
  { 8, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 9
  { 23, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 10
  { 36, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 11
  { 54, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 12
  { 12, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 13
  { 5, MipsGenericItinerariesFU::ALU, -1, (llvm::InstrStage::ReservationKinds)0 }, // 14
  { 0, 0, 0, llvm::InstrStage::Required } // End itinerary
};
static const unsigned OperandCycles[] = {
  0, // No itinerary
  0 // End itinerary
};

enum {
  StagesSize = sizeof(Stages)/sizeof(llvm::InstrStage),
  OperandCyclesSize = sizeof(OperandCycles)/sizeof(unsigned)
};

static const llvm::InstrItinerary MipsGenericItineraries[] = {
  { 1, 2, 0, 0 }, // 0
  { 1, 2, 0, 0 }, // 1
  { 7, 8, 0, 0 }, // 2
  { 2, 3, 0, 0 }, // 3
  { 1, 2, 0, 0 }, // 4
  { 11, 12, 0, 0 }, // 5
  { 10, 11, 0, 0 }, // 6
  { 3, 4, 0, 0 }, // 7
  { 9, 10, 0, 0 }, // 8
  { 8, 9, 0, 0 }, // 9
  { 14, 15, 0, 0 }, // 10
  { 13, 14, 0, 0 }, // 11
  { 12, 13, 0, 0 }, // 12
  { 4, 5, 0, 0 }, // 13
  { 6, 7, 0, 0 }, // 14
  { 5, 6, 0, 0 }, // 15
  { 2, 3, 0, 0 }, // 16
  { 0, 0, 0, 0 }, // 17
  { 1, 2, 0, 0 }, // 18
  { 3, 4, 0, 0 }, // 19
  { 0, 0, 0, 0 }, // 20
  { ~0U, ~0U, ~0U, ~0U } // end marker
};

// Sorted (by key) array of itineraries for CPU subtype.
static const llvm::SubtargetInfoKV ProcItinKV[] = {
  { "allegrex", (void *)&MipsGenericItineraries },
  { "mips1", (void *)&MipsGenericItineraries },
  { "mips2", (void *)&MipsGenericItineraries },
  { "r2000", (void *)&MipsGenericItineraries },
  { "r3000", (void *)&MipsGenericItineraries },
  { "r6000", (void *)&MipsGenericItineraries }
};

enum {
  ProcItinKVSize = sizeof(ProcItinKV)/sizeof(llvm::SubtargetInfoKV)
};

// ParseSubtargetFeatures - Parses features string setting specified
// subtarget options.
std::string llvm::MipsSubtarget::ParseSubtargetFeatures(const std::string &FS,
                                  const std::string &CPU) {
  DEBUG(dbgs() << "\nFeatures:" << FS);
  DEBUG(dbgs() << "\nCPU:" << CPU);
  SubtargetFeatures Features(FS);
  Features.setCPUIfNone(CPU);
  uint32_t Bits =  Features.getBits(SubTypeKV, SubTypeKVSize,
                                    FeatureKV, FeatureKVSize);
  if ((Bits & FeatureBitCount) != 0) HasBitCount = true;
  if ((Bits & FeatureCondMov) != 0) HasCondMov = true;
  if ((Bits & FeatureEABI) != 0 && MipsABI < EABI) MipsABI = EABI;
  if ((Bits & FeatureFP64Bit) != 0) IsFP64bit = true;
  if ((Bits & FeatureGP64Bit) != 0) IsGP64bit = true;
  if ((Bits & FeatureMinMax) != 0) HasMinMax = true;
  if ((Bits & FeatureMips1) != 0 && MipsArchVersion < Mips1) MipsArchVersion = Mips1;
  if ((Bits & FeatureMips2) != 0 && MipsArchVersion < Mips2) MipsArchVersion = Mips2;
  if ((Bits & FeatureMulDivAdd) != 0) HasMulDivAdd = true;
  if ((Bits & FeatureO32) != 0 && MipsABI < O32) MipsABI = O32;
  if ((Bits & FeatureSEInReg) != 0) HasSEInReg = true;
  if ((Bits & FeatureSingleFloat) != 0) IsSingleFloat = true;
  if ((Bits & FeatureSwap) != 0) HasSwap = true;
  if ((Bits & FeatureVFPU) != 0) HasVFPU = true;

  InstrItinerary *Itinerary = (InstrItinerary *)Features.getInfo(ProcItinKV, ProcItinKVSize);
  InstrItins = InstrItineraryData(Stages, OperandCycles, Itinerary);
  return Features.getCPU();
}
