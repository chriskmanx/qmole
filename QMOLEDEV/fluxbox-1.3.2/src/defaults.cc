#include "defaults.hh"

std::string realProgramName(const std::string& name) {
  return PROGRAM_PREFIX + name + PROGRAM_SUFFIX;
}

const char* gitrevision() {
  return "";
}
