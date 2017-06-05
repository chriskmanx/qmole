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
  FeatureCIX =  1 << 0
};

// Sorted (by key) array of values for CPU features.
static const llvm::SubtargetFeatureKV FeatureKV[] = {
  { "cix", "Enable CIX extentions", FeatureCIX, 0 }
};

enum {
  FeatureKVSize = sizeof(FeatureKV)/sizeof(llvm::SubtargetFeatureKV)
};

// Sorted (by key) array of values for CPU subtype.
static const llvm::SubtargetFeatureKV SubTypeKV[] = {
  { "ev6", "Select the ev6 processor", 0, 0 },
  { "ev67", "Select the ev67 processor", FeatureCIX, 0 },
  { "generic", "Select the generic processor", 0, 0 }
};

enum {
  SubTypeKVSize = sizeof(SubTypeKV)/sizeof(llvm::SubtargetFeatureKV)
};


enum {
  ItinClassesSize = 29
};

// Functional units for itineraries "Alpha21264Itineraries"
namespace Alpha21264ItinerariesFU {
  const unsigned L0 = 1 << 0;
  const unsigned L1 = 1 << 1;
  const unsigned FST0 = 1 << 2;
  const unsigned FST1 = 1 << 3;
  const unsigned U0 = 1 << 4;
  const unsigned U1 = 1 << 5;
  const unsigned FA = 1 << 6;
  const unsigned FM = 1 << 7;
}

static const llvm::InstrStage Stages[] = {
  { 0, 0, 0, llvm::InstrStage::Required }, // No itinerary
  { 3, Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::L1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 1
  { 4, Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::L1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 2
  { 0, Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::L1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 3
  { 0, Alpha21264ItinerariesFU::FST0 | Alpha21264ItinerariesFU::FST1 | Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::L1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 4
  { 1, Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::L1 | Alpha21264ItinerariesFU::U0 | Alpha21264ItinerariesFU::U1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 5
  { 1, Alpha21264ItinerariesFU::L1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 6
  { 1, Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::L1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 7
  { 0, Alpha21264ItinerariesFU::U0 | Alpha21264ItinerariesFU::U1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 8
  { 3, Alpha21264ItinerariesFU::U0 | Alpha21264ItinerariesFU::U1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 9
  { 3, Alpha21264ItinerariesFU::L0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 10
  { 1, Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::U0 | Alpha21264ItinerariesFU::L1 | Alpha21264ItinerariesFU::U1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 11
  { 1, Alpha21264ItinerariesFU::U0 | Alpha21264ItinerariesFU::U1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 12
  { 7, Alpha21264ItinerariesFU::U1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 13
  { 3, Alpha21264ItinerariesFU::U0, -1, (llvm::InstrStage::ReservationKinds)0 }, // 14
  { 0, Alpha21264ItinerariesFU::FA, -1, (llvm::InstrStage::ReservationKinds)0 }, // 15
  { 6, Alpha21264ItinerariesFU::FA, -1, (llvm::InstrStage::ReservationKinds)0 }, // 16
  { 6, Alpha21264ItinerariesFU::FM, -1, (llvm::InstrStage::ReservationKinds)0 }, // 17
  { 12, Alpha21264ItinerariesFU::FA, -1, (llvm::InstrStage::ReservationKinds)0 }, // 18
  { 15, Alpha21264ItinerariesFU::FA, -1, (llvm::InstrStage::ReservationKinds)0 }, // 19
  { 18, Alpha21264ItinerariesFU::FA, -1, (llvm::InstrStage::ReservationKinds)0 }, // 20
  { 33, Alpha21264ItinerariesFU::FA, -1, (llvm::InstrStage::ReservationKinds)0 }, // 21
  { 3, Alpha21264ItinerariesFU::FST0 | Alpha21264ItinerariesFU::FST1 | Alpha21264ItinerariesFU::L0 | Alpha21264ItinerariesFU::L1, -1, (llvm::InstrStage::ReservationKinds)0 }, // 22
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

static const llvm::InstrItinerary Alpha21264Itineraries[] = {
  { 0, 0, 0, 0 }, // 0
  { 11, 12, 0, 0 }, // 1
  { 16, 17, 0, 0 }, // 2
  { 15, 16, 0, 0 }, // 3
  { 16, 17, 0, 0 }, // 4
  { 18, 19, 0, 0 }, // 5
  { 19, 20, 0, 0 }, // 6
  { 2, 3, 0, 0 }, // 7
  { 17, 18, 0, 0 }, // 8
  { 20, 21, 0, 0 }, // 9
  { 21, 22, 0, 0 }, // 10
  { 4, 5, 0, 0 }, // 11
  { 22, 23, 0, 0 }, // 12
  { 11, 12, 0, 0 }, // 13
  { 8, 9, 0, 0 }, // 14
  { 1, 2, 0, 0 }, // 15
  { 11, 12, 0, 0 }, // 16
  { 14, 15, 0, 0 }, // 17
  { 13, 14, 0, 0 }, // 18
  { 12, 13, 0, 0 }, // 19
  { 3, 4, 0, 0 }, // 20
  { 2, 3, 0, 0 }, // 21
  { 10, 11, 0, 0 }, // 22
  { 5, 6, 0, 0 }, // 23
  { 7, 8, 0, 0 }, // 24
  { 0, 0, 0, 0 }, // 25
  { 6, 7, 0, 0 }, // 26
  { 6, 7, 0, 0 }, // 27
  { 9, 10, 0, 0 }, // 28
  { ~0U, ~0U, ~0U, ~0U } // end marker
};

// Sorted (by key) array of itineraries for CPU subtype.
static const llvm::SubtargetInfoKV ProcItinKV[] = {
  { "ev6", (void *)&Alpha21264Itineraries },
  { "ev67", (void *)&Alpha21264Itineraries },
  { "generic", (void *)&Alpha21264Itineraries }
};

enum {
  ProcItinKVSize = sizeof(ProcItinKV)/sizeof(llvm::SubtargetInfoKV)
};

// ParseSubtargetFeatures - Parses features string setting specified
// subtarget options.
std::string llvm::AlphaSubtarget::ParseSubtargetFeatures(const std::string &FS,
                                  const std::string &CPU) {
  DEBUG(dbgs() << "\nFeatures:" << FS);
  DEBUG(dbgs() << "\nCPU:" << CPU);
  SubtargetFeatures Features(FS);
  Features.setCPUIfNone(CPU);
  uint32_t Bits =  Features.getBits(SubTypeKV, SubTypeKVSize,
                                    FeatureKV, FeatureKVSize);
  if ((Bits & FeatureCIX) != 0) HasCT = true;

  InstrItinerary *Itinerary = (InstrItinerary *)Features.getInfo(ProcItinKV, ProcItinKVSize);
  InstrItins = InstrItineraryData(Stages, OperandCycles, Itinerary);
  return Features.getCPU();
}
