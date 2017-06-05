//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// Option Parsing Definitions
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

#ifndef OPTION
#error "Define OPTION prior to including this file!"
#endif

/////////
// Groups


//////////
// Options

OPTION("<input>", INPUT, Input, INVALID, INVALID, DriverOption, 0, 0, 0)
OPTION("<unknown>", UNKNOWN, Unknown, INVALID, INVALID, 0, 0, 0, 0)
OPTION("--help", _help, Flag, INVALID, help, 0, 0, 0, 0)
OPTION("--version", _version, Flag, INVALID, version, 0, 0, 0, 0)
OPTION("-I", I, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Add directory to include search path", "<directory>")
OPTION("-filetype", filetype, Separate, INVALID, INVALID, 0, 0,
       "Specify the output file type ('asm', 'null', or 'obj')", 0)
OPTION("-help", help, Flag, INVALID, INVALID, 0, 0,
       "Print this help text", 0)
OPTION("-mllvm", mllvm, Separate, INVALID, INVALID, 0, 0,
       "Additional arguments to forward to LLVM's option processing", 0)
OPTION("-n", n, Flag, INVALID, INVALID, 0, 0,
       "Don't automatically start assembly file with a text section", 0)
OPTION("-output-asm-variant", output_asm_variant, Separate, INVALID, INVALID, 0, 0,
       "Select the asm variant index to use for output", 0)
OPTION("-o", o, Separate, INVALID, INVALID, 0, 0,
       "Specify output file", "<path>")
OPTION("-relax-all", relax_all, Flag, INVALID, INVALID, 0, 0,
       "Relax all fixups (for performance testing)", 0)
OPTION("-show-encoding", show_encoding, Flag, INVALID, INVALID, 0, 0,
       "Show instruction encoding information in transliterate mode", 0)
OPTION("-show-inst", show_inst, Flag, INVALID, INVALID, 0, 0,
       "Show internal instruction representation in transliterate mode", 0)
OPTION("-triple", triple, Separate, INVALID, INVALID, 0, 0,
       "Specify target triple (e.g. x86_64-pc-linux-gnu)", 0)
OPTION("-version", version, Flag, INVALID, INVALID, 0, 0,
       "Print the assembler version", 0)
