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
  FeatureV8Deprecated =  1 << 0,
  FeatureV9 =  1 << 1,
  FeatureVIS =  1 << 2
};

// Sorted (by key) array of values for CPU features.
static const llvm::SubtargetFeatureKV FeatureKV[] = {
  { "deprecated-v8", "Enable deprecated V8 instructions in V9 mode", FeatureV8Deprecated, 0 },
  { "v9", "Enable SPARC-V9 instructions", FeatureV9, 0 },
  { "vis", "Enable UltraSPARC Visual Instruction Set extensions", FeatureVIS, 0 }
};

enum {
  FeatureKVSize = sizeof(FeatureKV)/sizeof(llvm::SubtargetFeatureKV)
};

// Sorted (by key) array of values for CPU subtype.
static const llvm::SubtargetFeatureKV SubTypeKV[] = {
  { "f934", "Select the f934 processor", 0, 0 },
  { "generic", "Select the generic processor", 0, 0 },
  { "hypersparc", "Select the hypersparc processor", 0, 0 },
  { "sparclet", "Select the sparclet processor", 0, 0 },
  { "sparclite", "Select the sparclite processor", 0, 0 },
  { "sparclite86x", "Select the sparclite86x processor", 0, 0 },
  { "supersparc", "Select the supersparc processor", 0, 0 },
  { "tsc701", "Select the tsc701 processor", 0, 0 },
  { "ultrasparc", "Select the ultrasparc processor", FeatureV9 | FeatureV8Deprecated, 0 },
  { "ultrasparc3", "Select the ultrasparc3 processor", FeatureV9 | FeatureV8Deprecated, 0 },
  { "ultrasparc3-vis", "Select the ultrasparc3-vis processor", FeatureV9 | FeatureV8Deprecated | FeatureVIS, 0 },
  { "v8", "Select the v8 processor", 0, 0 },
  { "v9", "Select the v9 processor", FeatureV9, 0 }
};

enum {
  SubTypeKVSize = sizeof(SubTypeKV)/sizeof(llvm::SubtargetFeatureKV)
};


enum {
  ItinClassesSize = 1
};

// ParseSubtargetFeatures - Parses features string setting specified
// subtarget options.
std::string llvm::SparcSubtarget::ParseSubtargetFeatures(const std::string &FS,
                                  const std::string &CPU) {
  DEBUG(dbgs() << "\nFeatures:" << FS);
  DEBUG(dbgs() << "\nCPU:" << CPU);
  SubtargetFeatures Features(FS);
  Features.setCPUIfNone(CPU);
  uint32_t Bits =  Features.getBits(SubTypeKV, SubTypeKVSize,
                                    FeatureKV, FeatureKVSize);
  if ((Bits & FeatureV8Deprecated) != 0) V8DeprecatedInsts = true;
  if ((Bits & FeatureV9) != 0) IsV9 = true;
  if ((Bits & FeatureVIS) != 0) IsVIS = true;
  return Features.getCPU();
}
