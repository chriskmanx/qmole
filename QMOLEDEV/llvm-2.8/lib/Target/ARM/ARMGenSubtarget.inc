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
  ArchV4T =  1 << 0,
  ArchV5T =  1 << 1,
  ArchV5TE =  1 << 2,
  ArchV6 =  1 << 3,
  ArchV6M =  1 << 4,
  ArchV6T2 =  1 << 5,
  ArchV7A =  1 << 6,
  ArchV7M =  1 << 7,
  FeatureDB =  1 << 8,
  FeatureFP16 =  1 << 9,
  FeatureHWDiv =  1 << 10,
  FeatureHasSlowVMLx =  1 << 11,
  FeatureNEON =  1 << 12,
  FeatureNEONForFP =  1 << 13,
  FeatureNoARM =  1 << 14,
  FeaturePref32BitThumb =  1 << 15,
  FeatureSlowFPBrcc =  1 << 16,
  FeatureT2XtPk =  1 << 17,
  FeatureThumb2 =  1 << 18,
  FeatureVFP2 =  1 << 19,
  FeatureVFP3 =  1 << 20,
  FeatureVFPOnlySP =  1 << 21
};

// Sorted (by key) array of values for CPU features.
static const llvm::SubtargetFeatureKV FeatureKV[] = {
  { "32bit", "Prefer 32-bit Thumb instrs", FeaturePref32BitThumb, 0 },
  { "db", "Has data barrier (dmb / dsb) instructions", FeatureDB, 0 },
  { "fp-only-sp", "Floating point unit supports single precision only", FeatureVFPOnlySP, 0 },
  { "fp16", "Enable half-precision floating point", FeatureFP16, 0 },
  { "hwdiv", "Enable divide instructions", FeatureHWDiv, 0 },
  { "neon", "Enable NEON instructions", FeatureNEON, 0 },
  { "neonfp", "Use NEON for single precision FP", FeatureNEONForFP, 0 },
  { "noarm", "Does not support ARM mode execution", FeatureNoARM, 0 },
  { "slow-fp-brcc", "FP compare + branch is slow", FeatureSlowFPBrcc, 0 },
  { "t2xtpk", "Enable Thumb2 extract and pack instructions", FeatureT2XtPk, 0 },
  { "thumb2", "Enable Thumb2 instructions", FeatureThumb2, 0 },
  { "v4t", "ARM v4T", ArchV4T, 0 },
  { "v5t", "ARM v5T", ArchV5T, 0 },
  { "v5te", "ARM v5TE, v5TEj, v5TExp", ArchV5TE, 0 },
  { "v6", "ARM v6", ArchV6, 0 },
  { "v6m", "ARM v6m", ArchV6M, FeatureNoARM | FeatureDB },
  { "v6t2", "ARM v6t2", ArchV6T2, FeatureThumb2 },
  { "v7a", "ARM v7A", ArchV7A, FeatureThumb2 | FeatureNEON | FeatureDB },
  { "v7m", "ARM v7M", ArchV7M, FeatureThumb2 | FeatureNoARM | FeatureDB | FeatureHWDiv },
  { "vfp2", "Enable VFP2 instructions", FeatureVFP2, 0 },
  { "vfp3", "Enable VFP3 instructions", FeatureVFP3, 0 },
  { "vmlx", "Disable VFP MAC instructions", FeatureHasSlowVMLx, 0 }
};

enum {
  FeatureKVSize = sizeof(FeatureKV)/sizeof(llvm::SubtargetFeatureKV)
};

// Sorted (by key) array of values for CPU subtype.
static const llvm::SubtargetFeatureKV SubTypeKV[] = {
  { "arm1020e", "Select the arm1020e processor", ArchV5TE, 0 },
  { "arm1020t", "Select the arm1020t processor", ArchV5T, 0 },
  { "arm1022e", "Select the arm1022e processor", ArchV5TE, 0 },
  { "arm10e", "Select the arm10e processor", ArchV5TE, 0 },
  { "arm10tdmi", "Select the arm10tdmi processor", ArchV5T, 0 },
  { "arm1136j-s", "Select the arm1136j-s processor", ArchV6, 0 },
  { "arm1136jf-s", "Select the arm1136jf-s processor", ArchV6 | FeatureVFP2 | FeatureHasSlowVMLx, 0 },
  { "arm1156t2-s", "Select the arm1156t2-s processor", ArchV6T2, 0 },
  { "arm1156t2f-s", "Select the arm1156t2f-s processor", ArchV6T2 | FeatureVFP2, 0 },
  { "arm1176jz-s", "Select the arm1176jz-s processor", ArchV6, 0 },
  { "arm1176jzf-s", "Select the arm1176jzf-s processor", ArchV6 | FeatureVFP2, 0 },
  { "arm710t", "Select the arm710t processor", ArchV4T, 0 },
  { "arm720t", "Select the arm720t processor", ArchV4T, 0 },
  { "arm7tdmi", "Select the arm7tdmi processor", ArchV4T, 0 },
  { "arm7tdmi-s", "Select the arm7tdmi-s processor", ArchV4T, 0 },
  { "arm8", "Select the arm8 processor", 0, 0 },
  { "arm810", "Select the arm810 processor", 0, 0 },
  { "arm9", "Select the arm9 processor", ArchV4T, 0 },
  { "arm920", "Select the arm920 processor", ArchV4T, 0 },
  { "arm920t", "Select the arm920t processor", ArchV4T, 0 },
  { "arm922t", "Select the arm922t processor", ArchV4T, 0 },
  { "arm926ej-s", "Select the arm926ej-s processor", ArchV5TE, 0 },
  { "arm940t", "Select the arm940t processor", ArchV4T, 0 },
  { "arm946e-s", "Select the arm946e-s processor", ArchV5TE, 0 },
  { "arm966e-s", "Select the arm966e-s processor", ArchV5TE, 0 },
  { "arm968e-s", "Select the arm968e-s processor", ArchV5TE, 0 },
  { "arm9e", "Select the arm9e processor", ArchV5TE, 0 },
  { "arm9tdmi", "Select the arm9tdmi processor", ArchV4T, 0 },
  { "cortex-a8", "Select the cortex-a8 processor", ArchV7A | FeatureHasSlowVMLx | FeatureSlowFPBrcc | FeatureNEONForFP | FeatureT2XtPk, 0 },
  { "cortex-a9", "Select the cortex-a9 processor", ArchV7A | FeatureT2XtPk, 0 },
  { "cortex-m0", "Select the cortex-m0 processor", ArchV6M, 0 },
  { "cortex-m3", "Select the cortex-m3 processor", ArchV7M, 0 },
  { "cortex-m4", "Select the cortex-m4 processor", ArchV7M | FeatureVFP2 | FeatureVFPOnlySP, 0 },
  { "ep9312", "Select the ep9312 processor", ArchV4T, 0 },
  { "generic", "Select the generic processor", 0, 0 },
  { "iwmmxt", "Select the iwmmxt processor", ArchV5TE, 0 },
  { "mpcore", "Select the mpcore processor", ArchV6 | FeatureVFP2, 0 },
  { "mpcorenovfp", "Select the mpcorenovfp processor", ArchV6, 0 },
  { "strongarm", "Select the strongarm processor", 0, 0 },
  { "strongarm110", "Select the strongarm110 processor", 0, 0 },
  { "strongarm1100", "Select the strongarm1100 processor", 0, 0 },
  { "strongarm1110", "Select the strongarm1110 processor", 0, 0 },
  { "xscale", "Select the xscale processor", ArchV5TE, 0 }
};

enum {
  SubTypeKVSize = sizeof(SubTypeKV)/sizeof(llvm::SubtargetFeatureKV)
};


enum {
  ItinClassesSize = 139
};

// Functional units for itineraries "ARMV6Itineraries"
namespace ARMV6ItinerariesFU {
  const unsigned V6_Pipe = 1 << 0;
}

// Functional units for itineraries "CortexA8Itineraries"
namespace CortexA8ItinerariesFU {
  const unsigned A8_Issue = 1 << 0;
  const unsigned A8_Pipe0 = 1 << 1;
  const unsigned A8_Pipe1 = 1 << 2;
  const unsigned A8_LdSt0 = 1 << 3;
  const unsigned A8_LdSt1 = 1 << 4;
  const unsigned A8_NPipe = 1 << 5;
  const unsigned A8_NLSPipe = 1 << 6;
}

// Functional units for itineraries "CortexA9Itineraries"
namespace CortexA9ItinerariesFU {
  const unsigned A9_NPipe = 1 << 0;
  const unsigned A9_DRegsN = 1 << 1;
  const unsigned A9_DRegsVFP = 1 << 2;
  const unsigned A9_LSPipe = 1 << 3;
  const unsigned A9_Pipe0 = 1 << 4;
  const unsigned A9_Pipe1 = 1 << 5;
}

static const llvm::InstrStage Stages[] = {
  { 0, 0, 0, llvm::InstrStage::Required }, // No itinerary
  { 1, ARMV6ItinerariesFU::V6_Pipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 1
  { 2, ARMV6ItinerariesFU::V6_Pipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 2
  { 3, ARMV6ItinerariesFU::V6_Pipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 3
  { 15, ARMV6ItinerariesFU::V6_Pipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 4
  { 29, ARMV6ItinerariesFU::V6_Pipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 5
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 6
  { 1, CortexA8ItinerariesFU::A8_Pipe0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 7
  { 1, CortexA8ItinerariesFU::A8_Pipe1, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_Pipe0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 8
  { 2, CortexA8ItinerariesFU::A8_Pipe1, 0, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA8ItinerariesFU::A8_Pipe0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 9
  { 1, CortexA8ItinerariesFU::A8_Issue, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_LdSt0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 10
  { 2, CortexA8ItinerariesFU::A8_Issue, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_LdSt0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 11
  { 2, CortexA8ItinerariesFU::A8_Issue, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_Pipe0, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_LdSt0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 12
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 13
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 14
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 4, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 4, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 15
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 7, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 7, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 16
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 5, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 5, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 17
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 8, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 8, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 18
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 9, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 9, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 19
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 11, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 11, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 20
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 19, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 19, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 21
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 20, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 20, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 22
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 29, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 29, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 23
  { 1, CortexA8ItinerariesFU::A8_Issue, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_LdSt0, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 24
  { 2, CortexA8ItinerariesFU::A8_Issue, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_LdSt0, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 25
  { 3, CortexA8ItinerariesFU::A8_Issue, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_Pipe0, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_LdSt0, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 26
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 27
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 28
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_NLSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 29
  { 1, CortexA8ItinerariesFU::A8_Pipe0 | CortexA8ItinerariesFU::A8_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA8ItinerariesFU::A8_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA8ItinerariesFU::A8_NLSPipe, 0, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA8ItinerariesFU::A8_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 30
  { 1, CortexA9ItinerariesFU::A9_Pipe0 | CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 31
  { 2, CortexA9ItinerariesFU::A9_Pipe0 | CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 32
  { 3, CortexA9ItinerariesFU::A9_Pipe0 | CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 33
  { 1, CortexA9ItinerariesFU::A9_Pipe1, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_Pipe0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 34
  { 2, CortexA9ItinerariesFU::A9_Pipe1, 0, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA9ItinerariesFU::A9_Pipe0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 35
  { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_LSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 36
  { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_LSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 37
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 38
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 39
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 5, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 40
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 6, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 41
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 7, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 42
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 9, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 43
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 10, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 44
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 16, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 10, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 45
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 26, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 20, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 46
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 18, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 13, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 47
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 33, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 28, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 48
  { 1, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_LSPipe, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 49
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 7, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, 0, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_LSPipe, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 50
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 7, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 51
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 8, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 52
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 7, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 53
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 10, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 4, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 54
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_LSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 55
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 4, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 56
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 1, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 57
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 9, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA9ItinerariesFU::A9_LSPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 58
  { 2, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 8, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 59
  { 2, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 9, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 60
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 9, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 3, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 61
  { 1, CortexA9ItinerariesFU::A9_DRegsN, 0, (llvm::InstrStage::ReservationKinds)0 },   { 9, CortexA9ItinerariesFU::A9_DRegsVFP, 0, (llvm::InstrStage::ReservationKinds)1 },   { 1, CortexA9ItinerariesFU::A9_Pipe1, -1, (llvm::InstrStage::ReservationKinds)0 },   { 2, CortexA9ItinerariesFU::A9_NPipe, -1, (llvm::InstrStage::ReservationKinds)0 }, // 62
  { 0, 0, 0, llvm::InstrStage::Required } // End itinerary
};
static const unsigned OperandCycles[] = {
  0, // No itinerary
  2,   2, // 1
  2,   2,   2, // 2
  2,   2,   1, // 3
  3,   3,   2,   1, // 4
  2,   1, // 5
  3,   2,   1, // 6
  2, // 7
  3, // 8
  3,   2, // 9
  3,   1, // 10
  4,   2,   1, // 11
  4,   1,   1, // 12
  4,   1,   1,   2, // 13
  5,   1,   1, // 14
  5,   1,   1,   2, // 15
  6,   1,   1, // 16
  6,   1,   1,   2, // 17
  4,   1, // 18
  5,   2,   1, // 19
  4,   2,   1,   1, // 20
  5,   2,   2,   1, // 21
  2,   1,   1, // 22
  2,   2,   1,   1, // 23
  2,   2,   2,   1, // 24
  5,   2, // 25
  9,   2, // 26
  9,   2,   2, // 27
  9,   2,   2,   2, // 28
  20,   2,   2, // 29
  34,   2,   2, // 30
  5,   2,   2, // 31
  1, // 32
  1,   1, // 33
  1,   1,   1, // 34
  6,   1,   1,   4, // 35
  6,   6,   1,   1, // 36
  3,   1,   1, // 37
  3,   2,   1,   1, // 38
  4,   3,   1,   1, // 39
  2,   3,   1, // 40
  2,   3,   1,   1, // 41
  3,   3,   1,   1, // 42
  7,   1, // 43
  5,   1, // 44
  8,   1, // 45
  7,   1,   1, // 46
  9,   1,   1, // 47
  11,   1,   1, // 48
  7,   2,   1,   1, // 49
  19,   2,   1,   1, // 50
  20,   1,   1, // 51
  29,   1,   1, // 52
  19,   1, // 53
  29,   1, // 54
  2,   2,   2,   2,   1, // 55
  6,   2, // 56
  6,   2,   2, // 57
  20,   1, // 58
  20,   20,   1, // 59
  4,   4,   1,   1, // 60
  9,   3,   2,   2, // 61
  10,   3,   2,   2, // 62
  10,   2,   2, // 63
  3,   2,   2, // 64
  4,   2,   2, // 65
  4,   2, // 66
  6,   3,   1, // 67
  7,   3,   1, // 68
  6,   3,   2,   1, // 69
  7,   2,   1, // 70
  7,   2,   2, // 71
  9,   2,   1, // 72
  6,   3,   2,   2, // 73
  7,   3,   2,   1, // 74
  7,   3,   2,   2, // 75
  9,   3,   2,   1, // 76
  3,   2,   2,   1, // 77
  4,   2,   2,   3,   1, // 78
  4,   2,   2,   3,   3,   1, // 79
  3,   1,   2,   1, // 80
  3,   1,   2,   2,   1, // 81
  4,   1,   2,   2,   3,   1, // 82
  4,   1,   2,   2,   3,   3,   1, // 83
  4,   5,   1,   1, // 84
  8,   0,   1,   1, // 85
  9,   0,   1,   1, // 86
  15,   1,   1, // 87
  25,   1,   1, // 88
  17,   1, // 89
  32,   1, // 90
  8,   4,   2,   1, // 91
  8,   2,   2, // 92
  0 // End itinerary
};

enum {
  StagesSize = sizeof(Stages)/sizeof(llvm::InstrStage),
  OperandCyclesSize = sizeof(OperandCycles)/sizeof(unsigned)
};

static const llvm::InstrItinerary ARMV6Itineraries[] = {
  { 1, 2, 0, 0 }, // 0
  { 0, 0, 0, 0 }, // 1
  { 0, 0, 0, 0 }, // 2
  { 0, 0, 0, 0 }, // 3
  { 0, 0, 0, 0 }, // 4
  { 0, 0, 0, 0 }, // 5
  { 0, 0, 0, 0 }, // 6
  { 0, 0, 0, 0 }, // 7
  { 0, 0, 0, 0 }, // 8
  { 0, 0, 0, 0 }, // 9
  { 0, 0, 0, 0 }, // 10
  { 0, 0, 0, 0 }, // 11
  { 0, 0, 0, 0 }, // 12
  { 0, 0, 0, 0 }, // 13
  { 0, 0, 0, 0 }, // 14
  { 0, 0, 0, 0 }, // 15
  { 0, 0, 0, 0 }, // 16
  { 0, 0, 0, 0 }, // 17
  { 0, 0, 0, 0 }, // 18
  { 0, 0, 0, 0 }, // 19
  { 0, 0, 0, 0 }, // 20
  { 0, 0, 0, 0 }, // 21
  { 0, 0, 0, 0 }, // 22
  { 0, 0, 0, 0 }, // 23
  { 0, 0, 0, 0 }, // 24
  { 0, 0, 0, 0 }, // 25
  { 0, 0, 0, 0 }, // 26
  { 0, 0, 0, 0 }, // 27
  { 0, 0, 0, 0 }, // 28
  { 0, 0, 0, 0 }, // 29
  { 0, 0, 0, 0 }, // 30
  { 0, 0, 0, 0 }, // 31
  { 0, 0, 0, 0 }, // 32
  { 0, 0, 0, 0 }, // 33
  { 0, 0, 0, 0 }, // 34
  { 0, 0, 0, 0 }, // 35
  { 0, 0, 0, 0 }, // 36
  { 0, 0, 0, 0 }, // 37
  { 0, 0, 0, 0 }, // 38
  { 0, 0, 0, 0 }, // 39
  { 0, 0, 0, 0 }, // 40
  { 0, 0, 0, 0 }, // 41
  { 0, 0, 0, 0 }, // 42
  { 0, 0, 0, 0 }, // 43
  { 0, 0, 0, 0 }, // 44
  { 0, 0, 0, 0 }, // 45
  { 0, 0, 0, 0 }, // 46
  { 0, 0, 0, 0 }, // 47
  { 0, 0, 0, 0 }, // 48
  { 0, 0, 0, 0 }, // 49
  { 0, 0, 0, 0 }, // 50
  { 0, 0, 0, 0 }, // 51
  { 0, 0, 0, 0 }, // 52
  { 0, 0, 0, 0 }, // 53
  { 0, 0, 0, 0 }, // 54
  { 0, 0, 0, 0 }, // 55
  { 0, 0, 0, 0 }, // 56
  { 0, 0, 0, 0 }, // 57
  { 0, 0, 0, 0 }, // 58
  { 0, 0, 0, 0 }, // 59
  { 0, 0, 0, 0 }, // 60
  { 0, 0, 0, 0 }, // 61
  { 0, 0, 0, 0 }, // 62
  { 0, 0, 0, 0 }, // 63
  { 0, 0, 0, 0 }, // 64
  { 1, 2, 76, 79 }, // 65
  { 1, 2, 76, 79 }, // 66
  { 1, 2, 1, 3 }, // 67
  { 1, 2, 1, 3 }, // 68
  { 1, 2, 74, 76 }, // 69
  { 1, 2, 72, 74 }, // 70
  { 0, 0, 0, 0 }, // 71
  { 1, 2, 74, 76 }, // 72
  { 1, 2, 74, 76 }, // 73
  { 1, 2, 72, 74 }, // 74
  { 0, 0, 0, 0 }, // 75
  { 1, 2, 74, 76 }, // 76
  { 4, 5, 83, 86 }, // 77
  { 5, 6, 86, 89 }, // 78
  { 1, 2, 89, 92 }, // 79
  { 1, 2, 89, 92 }, // 80
  { 3, 4, 0, 0 }, // 81
  { 1, 2, 79, 83 }, // 82
  { 2, 3, 79, 83 }, // 83
  { 0, 0, 0, 0 }, // 84
  { 0, 0, 0, 0 }, // 85
  { 0, 0, 0, 0 }, // 86
  { 0, 0, 0, 0 }, // 87
  { 1, 2, 76, 79 }, // 88
  { 2, 3, 76, 79 }, // 89
  { 4, 5, 83, 86 }, // 90
  { 5, 6, 86, 89 }, // 91
  { 1, 2, 19, 20 }, // 92
  { 1, 2, 3, 6 }, // 93
  { 1, 2, 3, 6 }, // 94
  { 3, 4, 0, 0 }, // 95
  { 1, 2, 72, 74 }, // 96
  { 1, 2, 72, 74 }, // 97
  { 1, 2, 1, 3 }, // 98
  { 1, 2, 3, 6 }, // 99
  { 1, 2, 6, 9 }, // 100
  { 2, 3, 9, 13 }, // 101
  { 1, 2, 0, 0 }, // 102
  { 1, 2, 19, 20 }, // 103
  { 1, 2, 20, 22 }, // 104
  { 1, 2, 22, 24 }, // 105
  { 1, 2, 24, 27 }, // 106
  { 1, 2, 18, 19 }, // 107
  { 1, 2, 1, 3 }, // 108
  { 1, 2, 13, 15 }, // 109
  { 2, 3, 15, 18 }, // 110
  { 1, 2, 48, 50 }, // 111
  { 1, 2, 24, 27 }, // 112
  { 3, 4, 0, 0 }, // 113
  { 1, 2, 27, 30 }, // 114
  { 1, 2, 53, 57 }, // 115
  { 2, 3, 50, 53 }, // 116
  { 2, 3, 57, 61 }, // 117
  { 1, 2, 30, 34 }, // 118
  { 2, 3, 37, 41 }, // 119
  { 3, 4, 44, 48 }, // 120
  { 1, 2, 18, 19 }, // 121
  { 1, 2, 1, 3 }, // 122
  { 1, 2, 13, 15 }, // 123
  { 2, 3, 15, 18 }, // 124
  { 1, 2, 27, 30 }, // 125
  { 2, 3, 34, 37 }, // 126
  { 3, 4, 41, 44 }, // 127
  { 1, 2, 13, 15 }, // 128
  { 1, 2, 6, 9 }, // 129
  { 3, 4, 0, 0 }, // 130
  { 1, 2, 61, 64 }, // 131
  { 1, 2, 64, 68 }, // 132
  { 2, 3, 6, 9 }, // 133
  { 2, 3, 68, 72 }, // 134
  { 1, 2, 1, 3 }, // 135
  { 1, 2, 13, 15 }, // 136
  { 2, 3, 15, 18 }, // 137
  { 0, 0, 0, 0 }, // 138
  { ~0U, ~0U, ~0U, ~0U } // end marker
};

static const llvm::InstrItinerary CortexA8Itineraries[] = {
  { 6, 7, 0, 0 }, // 0
  { 27, 29, 205, 209 }, // 1
  { 72, 74, 205, 209 }, // 2
  { 27, 29, 89, 92 }, // 3
  { 72, 74, 168, 171 }, // 4
  { 27, 29, 24, 27 }, // 5
  { 27, 29, 24, 27 }, // 6
  { 27, 29, 191, 194 }, // 7
  { 27, 29, 191, 194 }, // 8
  { 27, 29, 191, 194 }, // 9
  { 72, 74, 194, 197 }, // 10
  { 25, 27, 61, 64 }, // 11
  { 74, 76, 106, 109 }, // 12
  { 56, 60, 0, 0 }, // 13
  { 56, 60, 6, 9 }, // 14
  { 56, 60, 68, 72 }, // 15
  { 56, 60, 161, 166 }, // 16
  { 27, 29, 180, 184 }, // 17
  { 72, 74, 184, 188 }, // 18
  { 27, 29, 218, 222 }, // 19
  { 72, 74, 226, 230 }, // 20
  { 72, 74, 222, 226 }, // 21
  { 80, 84, 230, 234 }, // 22
  { 25, 27, 13, 15 }, // 23
  { 25, 27, 173, 176 }, // 24
  { 25, 27, 61, 64 }, // 25
  { 25, 27, 13, 15 }, // 26
  { 74, 76, 106, 109 }, // 27
  { 27, 29, 19, 20 }, // 28
  { 74, 76, 22, 24 }, // 29
  { 25, 27, 171, 173 }, // 30
  { 27, 29, 168, 171 }, // 31
  { 72, 74, 212, 215 }, // 32
  { 72, 74, 209, 212 }, // 33
  { 80, 84, 215, 218 }, // 34
  { 27, 29, 199, 202 }, // 35
  { 72, 74, 202, 205 }, // 36
  { 25, 27, 64, 68 }, // 37
  { 74, 76, 124, 128 }, // 38
  { 76, 80, 176, 180 }, // 39
  { 27, 29, 48, 50 }, // 40
  { 27, 29, 48, 50 }, // 41
  { 27, 29, 76, 79 }, // 42
  { 72, 74, 188, 191 }, // 43
  { 27, 29, 27, 30 }, // 44
  { 72, 74, 34, 37 }, // 45
  { 27, 29, 106, 109 }, // 46
  { 72, 74, 27, 30 }, // 47
  { 56, 60, 0, 0 }, // 48
  { 27, 29, 24, 27 }, // 49
  { 27, 29, 24, 27 }, // 50
  { 27, 29, 15, 18 }, // 51
  { 27, 29, 15, 18 }, // 52
  { 74, 76, 15, 18 }, // 53
  { 74, 76, 234, 238 }, // 54
  { 76, 80, 238, 243 }, // 55
  { 76, 80, 243, 249 }, // 56
  { 74, 76, 249, 253 }, // 57
  { 74, 76, 253, 258 }, // 58
  { 76, 80, 258, 264 }, // 59
  { 76, 80, 264, 271 }, // 60
  { 27, 29, 72, 74 }, // 61
  { 72, 74, 166, 168 }, // 62
  { 27, 29, 197, 199 }, // 63
  { 27, 29, 197, 199 }, // 64
  { 27, 29, 134, 137 }, // 65
  { 41, 44, 137, 140 }, // 66
  { 27, 29, 93, 95 }, // 67
  { 29, 32, 48, 50 }, // 68
  { 38, 41, 132, 134 }, // 69
  { 35, 38, 130, 132 }, // 70
  { 0, 0, 0, 0 }, // 71
  { 38, 41, 132, 134 }, // 72
  { 27, 29, 128, 130 }, // 73
  { 32, 35, 128, 130 }, // 74
  { 0, 0, 0, 0 }, // 75
  { 27, 29, 128, 130 }, // 76
  { 50, 53, 151, 154 }, // 77
  { 53, 56, 154, 157 }, // 78
  { 56, 60, 0, 0 }, // 79
  { 60, 66, 0, 0 }, // 80
  { 66, 72, 0, 0 }, // 81
  { 27, 29, 143, 147 }, // 82
  { 47, 50, 147, 151 }, // 83
  { 0, 0, 0, 0 }, // 84
  { 0, 0, 0, 0 }, // 85
  { 0, 0, 0, 0 }, // 86
  { 0, 0, 0, 0 }, // 87
  { 27, 29, 134, 137 }, // 88
  { 44, 47, 140, 143 }, // 89
  { 47, 50, 157, 159 }, // 90
  { 53, 56, 159, 161 }, // 91
  { 25, 27, 0, 0 }, // 92
  { 56, 60, 0, 0 }, // 93
  { 60, 66, 0, 0 }, // 94
  { 66, 72, 0, 0 }, // 95
  { 27, 29, 128, 130 }, // 96
  { 29, 32, 48, 50 }, // 97
  { 6, 7, 1, 3 }, // 98
  { 6, 7, 3, 6 }, // 99
  { 6, 7, 6, 9 }, // 100
  { 6, 7, 64, 68 }, // 101
  { 6, 7, 0, 0 }, // 102
  { 6, 7, 18, 19 }, // 103
  { 6, 7, 13, 15 }, // 104
  { 6, 7, 13, 15 }, // 105
  { 6, 7, 61, 64 }, // 106
  { 6, 7, 18, 19 }, // 107
  { 6, 7, 1, 3 }, // 108
  { 6, 7, 13, 15 }, // 109
  { 6, 7, 61, 64 }, // 110
  { 12, 15, 22, 24 }, // 111
  { 12, 15, 15, 18 }, // 112
  { 20, 25, 0, 0 }, // 113
  { 12, 15, 106, 109 }, // 114
  { 12, 15, 109, 113 }, // 115
  { 15, 20, 27, 30 }, // 116
  { 15, 20, 113, 117 }, // 117
  { 8, 10, 98, 102 }, // 118
  { 8, 10, 98, 102 }, // 119
  { 10, 12, 102, 106 }, // 120
  { 6, 7, 92, 93 }, // 121
  { 6, 7, 93, 95 }, // 122
  { 6, 7, 93, 95 }, // 123
  { 6, 7, 95, 98 }, // 124
  { 7, 8, 34, 37 }, // 125
  { 8, 10, 41, 44 }, // 126
  { 10, 12, 102, 106 }, // 127
  { 12, 15, 22, 24 }, // 128
  { 12, 15, 117, 120 }, // 129
  { 20, 25, 0, 0 }, // 130
  { 12, 15, 106, 109 }, // 131
  { 12, 15, 120, 124 }, // 132
  { 15, 20, 106, 109 }, // 133
  { 15, 20, 124, 128 }, // 134
  { 6, 7, 1, 3 }, // 135
  { 6, 7, 13, 15 }, // 136
  { 6, 7, 61, 64 }, // 137
  { 0, 0, 0, 0 }, // 138
  { ~0U, ~0U, ~0U, ~0U } // end marker
};

static const llvm::InstrItinerary CortexA9Itineraries[] = {
  { 84, 85, 0, 0 }, // 0
  { 149, 153, 205, 209 }, // 1
  { 157, 161, 205, 209 }, // 2
  { 149, 153, 89, 92 }, // 3
  { 153, 157, 168, 171 }, // 4
  { 149, 153, 194, 197 }, // 5
  { 149, 153, 194, 197 }, // 6
  { 149, 153, 191, 194 }, // 7
  { 149, 153, 191, 194 }, // 8
  { 149, 153, 191, 194 }, // 9
  { 153, 157, 194, 197 }, // 10
  { 149, 153, 61, 64 }, // 11
  { 153, 157, 106, 109 }, // 12
  { 144, 149, 0, 0 }, // 13
  { 144, 149, 6, 9 }, // 14
  { 144, 149, 68, 72 }, // 15
  { 144, 149, 161, 166 }, // 16
  { 153, 157, 205, 209 }, // 17
  { 161, 165, 293, 297 }, // 18
  { 149, 153, 218, 222 }, // 19
  { 153, 157, 226, 230 }, // 20
  { 153, 157, 222, 226 }, // 21
  { 161, 165, 230, 234 }, // 22
  { 165, 169, 13, 15 }, // 23
  { 173, 177, 6, 9 }, // 24
  { 173, 177, 61, 64 }, // 25
  { 173, 177, 13, 15 }, // 26
  { 169, 173, 106, 109 }, // 27
  { 149, 153, 19, 20 }, // 28
  { 169, 173, 22, 24 }, // 29
  { 173, 177, 13, 15 }, // 30
  { 149, 153, 168, 171 }, // 31
  { 153, 157, 212, 215 }, // 32
  { 153, 157, 209, 212 }, // 33
  { 161, 165, 215, 218 }, // 34
  { 149, 153, 199, 202 }, // 35
  { 157, 161, 199, 202 }, // 36
  { 149, 153, 64, 68 }, // 37
  { 153, 157, 124, 128 }, // 38
  { 177, 181, 176, 180 }, // 39
  { 149, 153, 48, 50 }, // 40
  { 149, 153, 48, 50 }, // 41
  { 153, 157, 168, 171 }, // 42
  { 161, 165, 297, 300 }, // 43
  { 149, 153, 27, 30 }, // 44
  { 149, 153, 27, 30 }, // 45
  { 149, 153, 106, 109 }, // 46
  { 149, 153, 106, 109 }, // 47
  { 144, 149, 0, 0 }, // 48
  { 0, 0, 0, 0 }, // 49
  { 0, 0, 0, 0 }, // 50
  { 149, 153, 24, 27 }, // 51
  { 149, 153, 24, 27 }, // 52
  { 153, 157, 15, 18 }, // 53
  { 181, 185, 234, 238 }, // 54
  { 185, 189, 238, 243 }, // 55
  { 189, 193, 243, 249 }, // 56
  { 153, 157, 249, 253 }, // 57
  { 153, 157, 253, 258 }, // 58
  { 189, 193, 258, 264 }, // 59
  { 193, 197, 264, 271 }, // 60
  { 149, 153, 72, 74 }, // 61
  { 153, 157, 166, 168 }, // 62
  { 149, 153, 197, 199 }, // 63
  { 149, 153, 197, 199 }, // 64
  { 103, 107, 27, 30 }, // 65
  { 103, 107, 27, 30 }, // 66
  { 103, 107, 93, 95 }, // 67
  { 103, 107, 93, 95 }, // 68
  { 103, 107, 48, 50 }, // 69
  { 103, 107, 48, 50 }, // 70
  { 99, 103, 13, 15 }, // 71
  { 103, 107, 48, 50 }, // 72
  { 103, 107, 48, 50 }, // 73
  { 103, 107, 48, 50 }, // 74
  { 103, 107, 48, 50 }, // 75
  { 103, 107, 48, 50 }, // 76
  { 123, 127, 283, 286 }, // 77
  { 127, 131, 286, 289 }, // 78
  { 139, 144, 0, 0 }, // 79
  { 139, 144, 0, 0 }, // 80
  { 139, 144, 0, 0 }, // 81
  { 115, 119, 275, 279 }, // 82
  { 119, 123, 279, 283 }, // 83
  { 95, 99, 95, 98 }, // 84
  { 99, 103, 95, 98 }, // 85
  { 99, 103, 93, 95 }, // 86
  { 95, 99, 93, 95 }, // 87
  { 107, 111, 34, 37 }, // 88
  { 111, 115, 41, 44 }, // 89
  { 131, 135, 289, 291 }, // 90
  { 135, 139, 291, 293 }, // 91
  { 95, 99, 0, 0 }, // 92
  { 139, 144, 0, 0 }, // 93
  { 139, 144, 0, 0 }, // 94
  { 139, 144, 0, 0 }, // 95
  { 99, 103, 93, 95 }, // 96
  { 99, 103, 93, 95 }, // 97
  { 84, 85, 1, 3 }, // 98
  { 84, 85, 3, 6 }, // 99
  { 85, 86, 6, 9 }, // 100
  { 86, 87, 64, 68 }, // 101
  { 84, 85, 0, 0 }, // 102
  { 84, 85, 18, 19 }, // 103
  { 84, 85, 13, 15 }, // 104
  { 84, 85, 13, 15 }, // 105
  { 85, 86, 61, 64 }, // 106
  { 84, 85, 18, 19 }, // 107
  { 84, 85, 1, 3 }, // 108
  { 85, 86, 13, 15 }, // 109
  { 86, 87, 61, 64 }, // 110
  { 91, 93, 22, 24 }, // 111
  { 93, 95, 15, 18 }, // 112
  { 91, 93, 0, 0 }, // 113
  { 91, 93, 106, 109 }, // 114
  { 93, 95, 109, 113 }, // 115
  { 93, 95, 27, 30 }, // 116
  { 93, 95, 113, 117 }, // 117
  { 87, 89, 30, 34 }, // 118
  { 87, 89, 30, 34 }, // 119
  { 89, 91, 271, 275 }, // 120
  { 84, 85, 92, 93 }, // 121
  { 84, 85, 93, 95 }, // 122
  { 84, 85, 93, 95 }, // 123
  { 85, 86, 6, 9 }, // 124
  { 87, 89, 27, 30 }, // 125
  { 87, 89, 27, 30 }, // 126
  { 89, 91, 271, 275 }, // 127
  { 91, 93, 22, 24 }, // 128
  { 91, 93, 117, 120 }, // 129
  { 91, 93, 0, 0 }, // 130
  { 91, 93, 106, 109 }, // 131
  { 91, 93, 120, 124 }, // 132
  { 93, 95, 106, 109 }, // 133
  { 93, 95, 124, 128 }, // 134
  { 84, 85, 1, 3 }, // 135
  { 85, 86, 13, 15 }, // 136
  { 86, 87, 61, 64 }, // 137
  { 0, 0, 0, 0 }, // 138
  { ~0U, ~0U, ~0U, ~0U } // end marker
};

static const llvm::InstrItinerary GenericItineraries[] = {
  { 0, 0, 0, 0 }, // 0
  { 0, 0, 0, 0 }, // 1
  { 0, 0, 0, 0 }, // 2
  { 0, 0, 0, 0 }, // 3
  { 0, 0, 0, 0 }, // 4
  { 0, 0, 0, 0 }, // 5
  { 0, 0, 0, 0 }, // 6
  { 0, 0, 0, 0 }, // 7
  { 0, 0, 0, 0 }, // 8
  { 0, 0, 0, 0 }, // 9
  { 0, 0, 0, 0 }, // 10
  { 0, 0, 0, 0 }, // 11
  { 0, 0, 0, 0 }, // 12
  { 0, 0, 0, 0 }, // 13
  { 0, 0, 0, 0 }, // 14
  { 0, 0, 0, 0 }, // 15
  { 0, 0, 0, 0 }, // 16
  { 0, 0, 0, 0 }, // 17
  { 0, 0, 0, 0 }, // 18
  { 0, 0, 0, 0 }, // 19
  { 0, 0, 0, 0 }, // 20
  { 0, 0, 0, 0 }, // 21
  { 0, 0, 0, 0 }, // 22
  { 0, 0, 0, 0 }, // 23
  { 0, 0, 0, 0 }, // 24
  { 0, 0, 0, 0 }, // 25
  { 0, 0, 0, 0 }, // 26
  { 0, 0, 0, 0 }, // 27
  { 0, 0, 0, 0 }, // 28
  { 0, 0, 0, 0 }, // 29
  { 0, 0, 0, 0 }, // 30
  { 0, 0, 0, 0 }, // 31
  { 0, 0, 0, 0 }, // 32
  { 0, 0, 0, 0 }, // 33
  { 0, 0, 0, 0 }, // 34
  { 0, 0, 0, 0 }, // 35
  { 0, 0, 0, 0 }, // 36
  { 0, 0, 0, 0 }, // 37
  { 0, 0, 0, 0 }, // 38
  { 0, 0, 0, 0 }, // 39
  { 0, 0, 0, 0 }, // 40
  { 0, 0, 0, 0 }, // 41
  { 0, 0, 0, 0 }, // 42
  { 0, 0, 0, 0 }, // 43
  { 0, 0, 0, 0 }, // 44
  { 0, 0, 0, 0 }, // 45
  { 0, 0, 0, 0 }, // 46
  { 0, 0, 0, 0 }, // 47
  { 0, 0, 0, 0 }, // 48
  { 0, 0, 0, 0 }, // 49
  { 0, 0, 0, 0 }, // 50
  { 0, 0, 0, 0 }, // 51
  { 0, 0, 0, 0 }, // 52
  { 0, 0, 0, 0 }, // 53
  { 0, 0, 0, 0 }, // 54
  { 0, 0, 0, 0 }, // 55
  { 0, 0, 0, 0 }, // 56
  { 0, 0, 0, 0 }, // 57
  { 0, 0, 0, 0 }, // 58
  { 0, 0, 0, 0 }, // 59
  { 0, 0, 0, 0 }, // 60
  { 0, 0, 0, 0 }, // 61
  { 0, 0, 0, 0 }, // 62
  { 0, 0, 0, 0 }, // 63
  { 0, 0, 0, 0 }, // 64
  { 0, 0, 0, 0 }, // 65
  { 0, 0, 0, 0 }, // 66
  { 0, 0, 0, 0 }, // 67
  { 0, 0, 0, 0 }, // 68
  { 0, 0, 0, 0 }, // 69
  { 0, 0, 0, 0 }, // 70
  { 0, 0, 0, 0 }, // 71
  { 0, 0, 0, 0 }, // 72
  { 0, 0, 0, 0 }, // 73
  { 0, 0, 0, 0 }, // 74
  { 0, 0, 0, 0 }, // 75
  { 0, 0, 0, 0 }, // 76
  { 0, 0, 0, 0 }, // 77
  { 0, 0, 0, 0 }, // 78
  { 0, 0, 0, 0 }, // 79
  { 0, 0, 0, 0 }, // 80
  { 0, 0, 0, 0 }, // 81
  { 0, 0, 0, 0 }, // 82
  { 0, 0, 0, 0 }, // 83
  { 0, 0, 0, 0 }, // 84
  { 0, 0, 0, 0 }, // 85
  { 0, 0, 0, 0 }, // 86
  { 0, 0, 0, 0 }, // 87
  { 0, 0, 0, 0 }, // 88
  { 0, 0, 0, 0 }, // 89
  { 0, 0, 0, 0 }, // 90
  { 0, 0, 0, 0 }, // 91
  { 0, 0, 0, 0 }, // 92
  { 0, 0, 0, 0 }, // 93
  { 0, 0, 0, 0 }, // 94
  { 0, 0, 0, 0 }, // 95
  { 0, 0, 0, 0 }, // 96
  { 0, 0, 0, 0 }, // 97
  { 0, 0, 0, 0 }, // 98
  { 0, 0, 0, 0 }, // 99
  { 0, 0, 0, 0 }, // 100
  { 0, 0, 0, 0 }, // 101
  { 0, 0, 0, 0 }, // 102
  { 0, 0, 0, 0 }, // 103
  { 0, 0, 0, 0 }, // 104
  { 0, 0, 0, 0 }, // 105
  { 0, 0, 0, 0 }, // 106
  { 0, 0, 0, 0 }, // 107
  { 0, 0, 0, 0 }, // 108
  { 0, 0, 0, 0 }, // 109
  { 0, 0, 0, 0 }, // 110
  { 0, 0, 0, 0 }, // 111
  { 0, 0, 0, 0 }, // 112
  { 0, 0, 0, 0 }, // 113
  { 0, 0, 0, 0 }, // 114
  { 0, 0, 0, 0 }, // 115
  { 0, 0, 0, 0 }, // 116
  { 0, 0, 0, 0 }, // 117
  { 0, 0, 0, 0 }, // 118
  { 0, 0, 0, 0 }, // 119
  { 0, 0, 0, 0 }, // 120
  { 0, 0, 0, 0 }, // 121
  { 0, 0, 0, 0 }, // 122
  { 0, 0, 0, 0 }, // 123
  { 0, 0, 0, 0 }, // 124
  { 0, 0, 0, 0 }, // 125
  { 0, 0, 0, 0 }, // 126
  { 0, 0, 0, 0 }, // 127
  { 0, 0, 0, 0 }, // 128
  { 0, 0, 0, 0 }, // 129
  { 0, 0, 0, 0 }, // 130
  { 0, 0, 0, 0 }, // 131
  { 0, 0, 0, 0 }, // 132
  { 0, 0, 0, 0 }, // 133
  { 0, 0, 0, 0 }, // 134
  { 0, 0, 0, 0 }, // 135
  { 0, 0, 0, 0 }, // 136
  { 0, 0, 0, 0 }, // 137
  { 0, 0, 0, 0 }, // 138
  { ~0U, ~0U, ~0U, ~0U } // end marker
};

// Sorted (by key) array of itineraries for CPU subtype.
static const llvm::SubtargetInfoKV ProcItinKV[] = {
  { "arm1020e", (void *)&GenericItineraries },
  { "arm1020t", (void *)&GenericItineraries },
  { "arm1022e", (void *)&GenericItineraries },
  { "arm10e", (void *)&GenericItineraries },
  { "arm10tdmi", (void *)&GenericItineraries },
  { "arm1136j-s", (void *)&ARMV6Itineraries },
  { "arm1136jf-s", (void *)&ARMV6Itineraries },
  { "arm1156t2-s", (void *)&ARMV6Itineraries },
  { "arm1156t2f-s", (void *)&ARMV6Itineraries },
  { "arm1176jz-s", (void *)&ARMV6Itineraries },
  { "arm1176jzf-s", (void *)&ARMV6Itineraries },
  { "arm710t", (void *)&GenericItineraries },
  { "arm720t", (void *)&GenericItineraries },
  { "arm7tdmi", (void *)&GenericItineraries },
  { "arm7tdmi-s", (void *)&GenericItineraries },
  { "arm8", (void *)&GenericItineraries },
  { "arm810", (void *)&GenericItineraries },
  { "arm9", (void *)&GenericItineraries },
  { "arm920", (void *)&GenericItineraries },
  { "arm920t", (void *)&GenericItineraries },
  { "arm922t", (void *)&GenericItineraries },
  { "arm926ej-s", (void *)&GenericItineraries },
  { "arm940t", (void *)&GenericItineraries },
  { "arm946e-s", (void *)&GenericItineraries },
  { "arm966e-s", (void *)&GenericItineraries },
  { "arm968e-s", (void *)&GenericItineraries },
  { "arm9e", (void *)&GenericItineraries },
  { "arm9tdmi", (void *)&GenericItineraries },
  { "cortex-a8", (void *)&CortexA8Itineraries },
  { "cortex-a9", (void *)&CortexA9Itineraries },
  { "cortex-m0", (void *)&ARMV6Itineraries },
  { "cortex-m3", (void *)&GenericItineraries },
  { "cortex-m4", (void *)&GenericItineraries },
  { "ep9312", (void *)&GenericItineraries },
  { "generic", (void *)&GenericItineraries },
  { "iwmmxt", (void *)&GenericItineraries },
  { "mpcore", (void *)&ARMV6Itineraries },
  { "mpcorenovfp", (void *)&ARMV6Itineraries },
  { "strongarm", (void *)&GenericItineraries },
  { "strongarm110", (void *)&GenericItineraries },
  { "strongarm1100", (void *)&GenericItineraries },
  { "strongarm1110", (void *)&GenericItineraries },
  { "xscale", (void *)&GenericItineraries }
};

enum {
  ProcItinKVSize = sizeof(ProcItinKV)/sizeof(llvm::SubtargetInfoKV)
};

// ParseSubtargetFeatures - Parses features string setting specified
// subtarget options.
std::string llvm::ARMSubtarget::ParseSubtargetFeatures(const std::string &FS,
                                  const std::string &CPU) {
  DEBUG(dbgs() << "\nFeatures:" << FS);
  DEBUG(dbgs() << "\nCPU:" << CPU);
  SubtargetFeatures Features(FS);
  Features.setCPUIfNone(CPU);
  uint32_t Bits =  Features.getBits(SubTypeKV, SubTypeKVSize,
                                    FeatureKV, FeatureKVSize);
  if ((Bits & ArchV4T) != 0 && ARMArchVersion < V4T) ARMArchVersion = V4T;
  if ((Bits & ArchV5T) != 0 && ARMArchVersion < V5T) ARMArchVersion = V5T;
  if ((Bits & ArchV5TE) != 0 && ARMArchVersion < V5TE) ARMArchVersion = V5TE;
  if ((Bits & ArchV6) != 0 && ARMArchVersion < V6) ARMArchVersion = V6;
  if ((Bits & ArchV6M) != 0 && ARMArchVersion < V6M) ARMArchVersion = V6M;
  if ((Bits & ArchV6T2) != 0 && ARMArchVersion < V6T2) ARMArchVersion = V6T2;
  if ((Bits & ArchV7A) != 0 && ARMArchVersion < V7A) ARMArchVersion = V7A;
  if ((Bits & ArchV7M) != 0 && ARMArchVersion < V7M) ARMArchVersion = V7M;
  if ((Bits & FeatureDB) != 0) HasDataBarrier = true;
  if ((Bits & FeatureFP16) != 0) HasFP16 = true;
  if ((Bits & FeatureHWDiv) != 0) HasHardwareDivide = true;
  if ((Bits & FeatureHasSlowVMLx) != 0) SlowVMLx = true;
  if ((Bits & FeatureNEON) != 0 && ARMFPUType < NEON) ARMFPUType = NEON;
  if ((Bits & FeatureNEONForFP) != 0) UseNEONForSinglePrecisionFP = true;
  if ((Bits & FeatureNoARM) != 0) NoARM = true;
  if ((Bits & FeaturePref32BitThumb) != 0) Pref32BitThumb = true;
  if ((Bits & FeatureSlowFPBrcc) != 0) SlowFPBrcc = true;
  if ((Bits & FeatureT2XtPk) != 0) HasT2ExtractPack = true;
  if ((Bits & FeatureThumb2) != 0 && ThumbMode < Thumb2) ThumbMode = Thumb2;
  if ((Bits & FeatureVFP2) != 0 && ARMFPUType < VFPv2) ARMFPUType = VFPv2;
  if ((Bits & FeatureVFP3) != 0 && ARMFPUType < VFPv3) ARMFPUType = VFPv3;
  if ((Bits & FeatureVFPOnlySP) != 0) FPOnlySP = true;

  InstrItinerary *Itinerary = (InstrItinerary *)Features.getInfo(ProcItinKV, ProcItinKVSize);
  InstrItins = InstrItineraryData(Stages, OperandCycles, Itinerary);
  return Features.getCPU();
}
