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

OPTION("<action group>", Action_Group, Group, INVALID, INVALID, 0, 0, 0, 0)

//////////
// Options

OPTION("<input>", INPUT, Input, INVALID, INVALID, DriverOption, 0, 0, 0)
OPTION("<unknown>", UNKNOWN, Unknown, INVALID, INVALID, 0, 0, 0, 0)
OPTION("--help", _help, Flag, INVALID, help, 0, 0, 0, 0)
OPTION("--version", _version, Flag, INVALID, version, 0, 0, 0, 0)
OPTION("-CC", CC, Flag, INVALID, INVALID, 0, 0,
       "Enable comment output in -E mode, even from macro expansions", 0)
OPTION("-C", C, Flag, INVALID, INVALID, 0, 0,
       "Enable comment output in -E mode", 0)
OPTION("-D", D, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Predefine the specified macro", "<macro>")
OPTION("-Eonly", Eonly, Flag, Action_Group, INVALID, 0, 0,
       "Just run preprocessor, no output (for timings)", 0)
OPTION("-E", E, Flag, Action_Group, INVALID, 0, 0,
       "Run preprocessor, emit preprocessed file", 0)
OPTION("-F", F, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Add directory to framework include search path", "<directory>")
OPTION("-H", H, Flag, INVALID, INVALID, 0, 0,
       "Show header includes and nesting depth", 0)
OPTION("-I", I, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Add directory to include search path", "<directory>")
OPTION("-MP", MP, Flag, INVALID, INVALID, 0, 0,
       "Create phony target for each dependency (other than main file)", 0)
OPTION("-MQ", MQ, Separate, INVALID, INVALID, 0, 0,
       "Specify target to quote for dependency", 0)
OPTION("-MT", MT, Separate, INVALID, INVALID, 0, 0,
       "Specify target for dependency", 0)
OPTION("-Os", Os, Flag, INVALID, INVALID, 0, 0,
       "Optimize for size", 0)
OPTION("-O", O, Joined, INVALID, INVALID, 0, 0,
       "Optimization level", 0)
OPTION("-P", P, Flag, INVALID, INVALID, 0, 0,
       "Disable linemarker output in -E mode", 0)
OPTION("-S", S, Flag, Action_Group, INVALID, 0, 0,
       "Emit native assembly code", 0)
OPTION("-U", U, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Undefine the specified macro", "<macro>")
OPTION("-Wno-rewrite-macros", Wno_rewrite_macros, Flag, INVALID, INVALID, 0, 0,
       "Silence ObjC rewriting warnings", 0)
OPTION("-Wwrite-strings", Wwrite_strings, Flag, INVALID, INVALID, 0, 0,
       "Remove const qualifier from string literals", 0)
OPTION("-W", W, Joined, INVALID, INVALID, 0, 0, 0, 0)
OPTION("-analyze-function=", analyze_function_EQ, Joined, INVALID, analyze_function, 0, 0, 0, 0)
OPTION("-analyze-function", analyze_function, Separate, INVALID, INVALID, 0, 0,
       "Run analysis on specific function", 0)
OPTION("-analyzer-check-dead-stores", analysis_WarnDeadStores, Flag, INVALID, INVALID, 0, 0,
       "Warn about stores to dead variables", 0)
OPTION("-analyzer-check-idempotent-operations", analysis_WarnIdempotentOps, Flag, INVALID, INVALID, 0, 0,
       "Warn about idempotent operations", 0)
OPTION("-analyzer-check-llvm-conventions", analysis_LLVMConventionChecker, Flag, INVALID, INVALID, 0, 0,
       "Check code for LLVM codebase conventions (domain-specific)", 0)
OPTION("-analyzer-check-objc-mem", analysis_ObjCMemChecker, Flag, INVALID, INVALID, 0, 0,
       "Run the [Core] Foundation reference count checker", 0)
OPTION("-analyzer-check-objc-methodsigs", analysis_WarnObjCMethSigs, Flag, INVALID, INVALID, 0, 0,
       "Warn about Objective-C method signatures with type incompatibilities", 0)
OPTION("-analyzer-check-objc-missing-dealloc", analysis_WarnObjCDealloc, Flag, INVALID, INVALID, 0, 0,
       "Warn about Objective-C classes that lack a correct implementation of -dealloc", 0)
OPTION("-analyzer-check-objc-unused-ivars", analysis_WarnObjCUnusedIvars, Flag, INVALID, INVALID, 0, 0,
       "Warn about private ivars that are never used", 0)
OPTION("-analyzer-check-security-syntactic", analysis_SecuritySyntacticChecks, Flag, INVALID, INVALID, 0, 0,
       "Perform quick security checks that require no data flow", 0)
OPTION("-analyzer-constraints=", analyzer_constraints_EQ, Joined, INVALID, analyzer_constraints, 0, 0, 0, 0)
OPTION("-analyzer-constraints", analyzer_constraints, Separate, INVALID, INVALID, 0, 0,
       "Source Code Analysis - Symbolic Constraint Engines", 0)
OPTION("-analyzer-display-progress", analyzer_display_progress, Flag, INVALID, INVALID, 0, 0,
       "Emit verbose output about the analyzer's progress", 0)
OPTION("-analyzer-eagerly-assume", analyzer_eagerly_assume, Flag, INVALID, INVALID, 0, 0,
       "Eagerly assume the truth/falseness of some symbolic constraints", 0)
OPTION("-analyzer-experimental-checks", analyzer_experimental_checks, Flag, INVALID, INVALID, 0, 0,
       "Use experimental path-sensitive checks", 0)
OPTION("-analyzer-experimental-internal-checks", analyzer_experimental_internal_checks, Flag, INVALID, INVALID, 0, 0,
       "Use new default path-sensitive checks currently in testing", 0)
OPTION("-analyzer-inline-call", analyzer_inline_call, Flag, INVALID, INVALID, 0, 0,
       "Experimental transfer function inlining callees when its definition is available.", 0)
OPTION("-analyzer-max-loop", analyzer_max_loop, Separate, INVALID, INVALID, 0, 0,
       "The maximum number of times the analyzer will go through a loop", 0)
OPTION("-analyzer-max-nodes", analyzer_max_nodes, Separate, INVALID, INVALID, 0, 0,
       "The maximum number of nodes the analyzer can generate", 0)
OPTION("-analyzer-no-purge-dead", analyzer_no_purge_dead, Flag, INVALID, INVALID, 0, 0,
       "Don't remove dead symbols, bindings, and constraints before processing a statement", 0)
OPTION("-analyzer-opt-analyze-headers", analyzer_opt_analyze_headers, Flag, INVALID, INVALID, 0, 0,
       "Force the static analyzer to analyze functions defined in header files", 0)
OPTION("-analyzer-opt-analyze-nested-blocks", analyzer_opt_analyze_nested_blocks, Flag, INVALID, INVALID, 0, 0,
       "Analyze the definitions of blocks in addition to functions", 0)
OPTION("-analyzer-output=", analyzer_output_EQ, Joined, INVALID, analyzer_output, 0, 0, 0, 0)
OPTION("-analyzer-output", analyzer_output, Separate, INVALID, INVALID, 0, 0,
       "Source Code Analysis - Output Options", 0)
OPTION("-analyzer-store=", analyzer_store_EQ, Joined, INVALID, analyzer_store, 0, 0, 0, 0)
OPTION("-analyzer-store", analyzer_store, Separate, INVALID, INVALID, 0, 0,
       "Source Code Analysis - Abstract Memory Store Models", 0)
OPTION("-analyzer-viz-egraph-graphviz", analyzer_viz_egraph_graphviz, Flag, INVALID, INVALID, 0, 0,
       "Display exploded graph using GraphViz", 0)
OPTION("-analyzer-viz-egraph-ubigraph", analyzer_viz_egraph_ubigraph, Flag, INVALID, INVALID, 0, 0,
       "Display exploded graph using Ubigraph", 0)
OPTION("-analyze", analyze, Flag, Action_Group, INVALID, 0, 0,
       "Run static analysis engine", 0)
OPTION("-ast-dump", ast_dump, Flag, Action_Group, INVALID, 0, 0,
       "Build ASTs and then debug dump them", 0)
OPTION("-ast-merge", ast_merge, Separate, INVALID, INVALID, 0, 0,
       "Merge the given AST file into the translation unit being compiled.", "<ast file>")
OPTION("-ast-print-xml", ast_print_xml, Flag, Action_Group, INVALID, 0, 0,
       "Build ASTs and then print them in XML format", 0)
OPTION("-ast-print", ast_print, Flag, Action_Group, INVALID, 0, 0,
       "Build ASTs and then pretty-print them", 0)
OPTION("-ast-view", ast_view, Flag, Action_Group, INVALID, 0, 0,
       "Build ASTs and view them with GraphViz", 0)
OPTION("-boostcon", boostcon, Flag, Action_Group, INVALID, 0, 0,
       "BoostCon workshop mode", 0)
OPTION("-cc1", cc1, Flag, INVALID, INVALID, 0, 0, 0, 0)
OPTION("-cfg-dump", analysis_CFGDump, Flag, INVALID, INVALID, 0, 0,
       "Display Control-Flow Graphs", 0)
OPTION("-cfg-view", analysis_CFGView, Flag, INVALID, INVALID, 0, 0,
       "View Control-Flow Graphs using GraphViz", 0)
OPTION("-chained-pch", chained_pch, Flag, INVALID, INVALID, 0, 0,
       "Whether to chain the new precompiled header to the old one.", 0)
OPTION("-code-completion-at=", code_completion_at_EQ, Joined, INVALID, code_completion_at, 0, 0, 0, 0)
OPTION("-code-completion-at", code_completion_at, Separate, INVALID, INVALID, 0, 0,
       "Dump code-completion information at a location", "<file>:<line>:<column>")
OPTION("-code-completion-macros", code_completion_macros, Flag, INVALID, INVALID, 0, 0,
       "Include macros in code-completion results", 0)
OPTION("-code-completion-patterns", code_completion_patterns, Flag, INVALID, INVALID, 0, 0,
       "Include code patterns in code-completion results", 0)
OPTION("-create-module", create_module, Flag, Action_Group, INVALID, 0, 0,
       "Create a module definition file", 0)
OPTION("-cxx-abi", cxx_abi, Separate, INVALID, INVALID, 0, 0,
       "Target a particular C++ ABI type", 0)
OPTION("-cxx-inheritance-view", cxx_inheritance_view, Separate, INVALID, INVALID, 0, 0,
       "View C++ inheritance for a specified class", "<class name>")
OPTION("-dD", dD, Flag, INVALID, INVALID, 0, 0,
       "Print macro definitions in -E mode in addition to normal output", 0)
OPTION("-dM", dM, Flag, INVALID, INVALID, 0, 0,
       "Print macro definitions in -E mode instead of normal output", 0)
OPTION("-dependency-file", dependency_file, Separate, INVALID, INVALID, 0, 0,
       "Filename (or -) to write dependency output to", 0)
OPTION("-detailed-preprocessing-record", detailed_preprocessing_record, Flag, INVALID, INVALID, 0, 0,
       "include a detailed record of preprocessing actions", 0)
OPTION("-disable-free", disable_free, Flag, INVALID, INVALID, 0, 0,
       "Disable freeing of memory on exit", 0)
OPTION("-disable-llvm-optzns", disable_llvm_optzns, Flag, INVALID, INVALID, 0, 0,
       "Don't run LLVM optimization passes", 0)
OPTION("-disable-llvm-verifier", disable_llvm_verifier, Flag, INVALID, INVALID, 0, 0,
       "Don't run the LLVM IR verifier pass", 0)
OPTION("-disable-red-zone", disable_red_zone, Flag, INVALID, INVALID, 0, 0,
       "Do not emit code that uses the red zone.", 0)
OPTION("-dump-build-information", dump_build_information, Separate, INVALID, INVALID, 0, 0,
       "output a dump of some build information to a file", "<filename>")
OPTION("-dump-live-variables", analysis_DisplayLiveVariables, Flag, INVALID, INVALID, 0, 0,
       "Print results of live variable analysis", 0)
OPTION("-dump-raw-tokens", dump_raw_tokens, Flag, Action_Group, INVALID, 0, 0,
       "Lex file in raw mode and dump raw tokens", 0)
OPTION("-dump-tokens", dump_tokens, Flag, Action_Group, INVALID, 0, 0,
       "Run preprocessor, dump internal rep of tokens", 0)
OPTION("-dwarf-debug-flags", dwarf_debug_flags, Separate, INVALID, INVALID, 0, 0,
       "The string to embed in the Dwarf debug flags record.", 0)
OPTION("-emit-codegen-only", emit_codegen_only, Flag, Action_Group, INVALID, 0, 0,
       "Generate machine code, but discard output", 0)
OPTION("-emit-html", emit_html, Flag, Action_Group, INVALID, 0, 0,
       "Output input source as HTML", 0)
OPTION("-emit-llvm-bc", emit_llvm_bc, Flag, Action_Group, INVALID, 0, 0,
       "Build ASTs then convert to LLVM, emit .bc file", 0)
OPTION("-emit-llvm-only", emit_llvm_only, Flag, Action_Group, INVALID, 0, 0,
       "Build ASTs and convert to LLVM, discarding output", 0)
OPTION("-emit-llvm", emit_llvm, Flag, Action_Group, INVALID, 0, 0,
       "Build ASTs then convert to LLVM, emit .ll file", 0)
OPTION("-emit-obj", emit_obj, Flag, Action_Group, INVALID, 0, 0,
       "Emit native object files", 0)
OPTION("-emit-pch", emit_pch, Flag, Action_Group, INVALID, 0, 0,
       "Generate pre-compiled header file", 0)
OPTION("-emit-pth", emit_pth, Flag, Action_Group, INVALID, 0, 0,
       "Generate pre-tokenized header file", 0)
OPTION("-faltivec", faltivec, Flag, INVALID, INVALID, 0, 0,
       "Enable AltiVec vector initializer syntax", 0)
OPTION("-fblocks", fblocks, Flag, INVALID, INVALID, 0, 0,
       "enable the 'blocks' language feature", 0)
OPTION("-fborland-extensions", fborland_extensions, Flag, INVALID, INVALID, 0, 0,
       "Accept non-standard constructs supported by the Borland compiler", 0)
OPTION("-fcatch-undefined-behavior", fcatch_undefined_behavior, Flag, INVALID, INVALID, 0, 0,
       "Generate runtime checks for undefined behavior.", 0)
OPTION("-fcolor-diagnostics", fcolor_diagnostics, Flag, INVALID, INVALID, 0, 0,
       "Use colors in diagnostics", 0)
OPTION("-fconstant-string-class", fconstant_string_class, Separate, INVALID, INVALID, 0, 0,
       "Specify the class to use for constant Objective-C string objects.", "<class name>")
OPTION("-fdata-sections", fdata_sections, Flag, INVALID, INVALID, 0, 0,
       "Place each data in its own section (ELF Only)", 0)
OPTION("-fdiagnostics-binary", fdiagnostics_binary, Flag, INVALID, INVALID, 0, 0, 0, 0)
OPTION("-fdiagnostics-parseable-fixits", fdiagnostics_parseable_fixits, Flag, INVALID, INVALID, 0, 0,
       "Print fix-its in machine parseable form", 0)
OPTION("-fdiagnostics-print-source-range-info", fdiagnostics_print_source_range_info, Flag, INVALID, INVALID, 0, 0,
       "Print source range spans in numeric form", 0)
OPTION("-fdiagnostics-show-category", fdiagnostics_show_category, Separate, INVALID, INVALID, 0, 0,
       "Print diagnostic category", 0)
OPTION("-fdiagnostics-show-option", fdiagnostics_show_option, Flag, INVALID, INVALID, 0, 0,
       "Print diagnostic name with mappable diagnostics", 0)
OPTION("-fdollars-in-identifiers", fdollars_in_identifiers, Flag, INVALID, INVALID, 0, 0,
       "Allow '$' in identifiers", 0)
OPTION("-fdump-record-layouts", fdump_record_layouts, Flag, INVALID, INVALID, 0, 0,
       "Dump record layout information", 0)
OPTION("-fdump-vtable-layouts", fdump_vtable_layouts, Flag, INVALID, INVALID, 0, 0,
       "Dump the layouts of all vtables that will be emitted in a translation unit", 0)
OPTION("-femit-all-decls", femit_all_decls, Flag, INVALID, INVALID, 0, 0,
       "Emit all declarations, even if unused", 0)
OPTION("-ferror-limit", ferror_limit, Separate, INVALID, INVALID, 0, 0,
       "Set the maximum number of errors to emit before stopping (0 = no limit).", "<N>")
OPTION("-fexceptions", fexceptions, Flag, INVALID, INVALID, 0, 0,
       "Enable support for exception handling", 0)
OPTION("-ffreestanding", ffreestanding, Flag, INVALID, INVALID, 0, 0,
       "Assert that the compilation takes place in a freestanding environment", 0)
OPTION("-ffunction-sections", ffunction_sections, Flag, INVALID, INVALID, 0, 0,
       "Place each function in its own section (ELF Only)", 0)
OPTION("-fgnu-keywords", fgnu_keywords, Flag, INVALID, INVALID, 0, 0,
       "Allow GNU-extension keywords regardless of language standard", 0)
OPTION("-fgnu-runtime", fgnu_runtime, Flag, INVALID, INVALID, 0, 0,
       "Generate output compatible with the standard GNU Objective-C runtime", 0)
OPTION("-fheinous-gnu-extensions", fheinous_gnu_extensions, Flag, INVALID, INVALID, 0, 0, 0, 0)
OPTION("-fhidden-weak-vtables", fhidden_weak_vtables, Flag, INVALID, INVALID, 0, 0,
       "Generate weak vtables and RTTI with hidden visibility", 0)
OPTION("-finstrument-functions", finstrument_functions, Flag, INVALID, INVALID, 0, 0,
       "Generate calls to instrument function entry and exit", 0)
OPTION("-fix-what-you-can", fix_what_you_can, Flag, INVALID, INVALID, 0, 0,
       "Apply fix-it advice even in the presence of unfixable errors", 0)
OPTION("-fixit=", fixit_EQ, Joined, Action_Group, INVALID, 0, 0,
       "Apply fix-it advice creating a file with the given suffix", 0)
OPTION("-fixit", fixit, Flag, Action_Group, INVALID, 0, 0,
       "Apply fix-it advice to the input source", 0)
OPTION("-fmacro-backtrace-limit", fmacro_backtrace_limit, Separate, INVALID, INVALID, 0, 0,
       "Set the maximum number of entries to print in a macro instantiation backtrace (0 = no limit).", "<N>")
OPTION("-fmath-errno", fmath_errno, Flag, INVALID, INVALID, 0, 0,
       "Require math functions to indicate errors by setting errno", 0)
OPTION("-fmessage-length", fmessage_length, Separate, INVALID, INVALID, 0, 0,
       "Format message diagnostics so that they fit within N columns or fewer, when possible.", "<N>")
OPTION("-fms-extensions", fms_extensions, Flag, INVALID, INVALID, 0, 0,
       "Accept some non-standard constructs used in Microsoft header files ", 0)
OPTION("-fno-access-control", fno_access_control, Flag, INVALID, INVALID, 0, 0,
       "Disable C++ access control", 0)
OPTION("-fno-assume-sane-operator-new", fno_assume_sane_operator_new, Flag, INVALID, INVALID, 0, 0,
       "Don't assume that C++'s global operator new can't alias any pointer", 0)
OPTION("-fno-bitfield-type-align", fno_bitfield_type_align, Flag, INVALID, INVALID, 0, 0,
       "Ignore bit-field types when aligning structures", 0)
OPTION("-fno-builtin", fno_builtin, Flag, INVALID, INVALID, 0, 0,
       "Disable implicit builtin knowledge of functions", 0)
OPTION("-fno-caret-diagnostics", fno_caret_diagnostics, Flag, INVALID, INVALID, 0, 0,
       "Do not include source line and caret with diagnostics", 0)
OPTION("-fno-common", fno_common, Flag, INVALID, INVALID, 0, 0,
       "Compile common globals like normal definitions", 0)
OPTION("-fno-constant-cfstrings", fno_constant_cfstrings, Flag, INVALID, INVALID, 0, 0,
       "Enable creation of CodeFoundation-type constant strings", 0)
OPTION("-fno-diagnostics-fixit-info", fno_diagnostics_fixit_info, Flag, INVALID, INVALID, 0, 0,
       "Do not include fixit information in diagnostics", 0)
OPTION("-fno-dollars-in-identifiers", fno_dollars_in_identifiers, Flag, INVALID, INVALID, 0, 0,
       "Disallow '$' in identifiers", 0)
OPTION("-fno-elide-constructors", fno_elide_constructors, Flag, INVALID, INVALID, 0, 0,
       "Disable C++ copy constructor elision", 0)
OPTION("-fno-gnu-keywords", fno_gnu_keywords, Flag, INVALID, INVALID, 0, 0,
       "Disallow GNU-extension keywords regardless of language standard", 0)
OPTION("-fno-lax-vector-conversions", fno_lax_vector_conversions, Flag, INVALID, INVALID, 0, 0,
       "Disallow implicit conversions between vectors with a different number of elements or different element types", 0)
OPTION("-fno-merge-all-constants", fno_merge_all_constants, Flag, INVALID, INVALID, 0, 0,
       "Disallow merging of constants.", 0)
OPTION("-fno-operator-names", fno_operator_names, Flag, INVALID, INVALID, 0, 0,
       "Do not treat C++ operator name keywords as synonyms for operators", 0)
OPTION("-fno-rtti", fno_rtti, Flag, INVALID, INVALID, 0, 0,
       "Disable generation of rtti information", 0)
OPTION("-fno-show-column", fno_show_column, Flag, INVALID, INVALID, 0, 0,
       "Do not include column number on diagnostics", 0)
OPTION("-fno-show-source-location", fno_show_source_location, Flag, INVALID, INVALID, 0, 0,
       "Do not include source location information with diagnostics", 0)
OPTION("-fno-signed-char", fno_signed_char, Flag, INVALID, INVALID, 0, 0,
       "Char is unsigned", 0)
OPTION("-fno-spell-checking", fno_spell_checking, Flag, INVALID, INVALID, 0, 0,
       "Disable spell-checking", 0)
OPTION("-fno-threadsafe-statics", fno_threadsafe_statics, Flag, INVALID, INVALID, 0, 0,
       "Do not emit code to make initialization of local statics thread safe", 0)
OPTION("-fno-use-cxa-atexit", fno_use_cxa_atexit, Flag, INVALID, INVALID, 0, 0,
       "Don't use __cxa_atexit for calling destructors", 0)
OPTION("-fno-validate-pch", fno_validate_pch, Flag, INVALID, INVALID, 0, 0,
       "Disable validation of precompiled headers", 0)
OPTION("-fobjc-dispatch-method=", fobjc_dispatch_method_EQ, Joined, INVALID, INVALID, 0, 0,
       "Objective-C dispatch method to use", 0)
OPTION("-fobjc-gc-only", fobjc_gc_only, Flag, INVALID, INVALID, 0, 0,
       "Use GC exclusively for Objective-C related memory management", 0)
OPTION("-fobjc-gc", fobjc_gc, Flag, INVALID, INVALID, 0, 0,
       "Enable Objective-C garbage collection", 0)
OPTION("-fobjc-nonfragile-abi2", fobjc_nonfragile_abi2, Flag, INVALID, INVALID, 0, 0,
       "enable objective-c's enhanced nonfragile abi", 0)
OPTION("-fobjc-nonfragile-abi", fobjc_nonfragile_abi, Flag, INVALID, INVALID, 0, 0,
       "enable objective-c's nonfragile abi", 0)
OPTION("-fpascal-strings", fpascal_strings, Flag, INVALID, INVALID, 0, 0,
       "Recognize and construct Pascal-style string literals", 0)
OPTION("-fshort-wchar", fshort_wchar, Flag, INVALID, INVALID, 0, 0,
       "Force wchar_t to be a short unsigned int", 0)
OPTION("-fshow-overloads=", fshow_overloads_EQ, Joined, INVALID, INVALID, 0, 0,
       "Which overload candidates to show when overload resolution fails: best|all; defaults to all", 0)
OPTION("-fsjlj-exceptions", fsjlj_exceptions, Flag, INVALID, INVALID, 0, 0,
       "Use SjLj style exceptions", 0)
OPTION("-fsyntax-only", fsyntax_only, Flag, Action_Group, INVALID, 0, 0,
       "Run parser and perform semantic analysis", 0)
OPTION("-ftabstop", ftabstop, Separate, INVALID, INVALID, 0, 0,
       "Set the tab stop distance.", "<N>")
OPTION("-ftemplate-backtrace-limit", ftemplate_backtrace_limit, Separate, INVALID, INVALID, 0, 0,
       "Set the maximum number of entries to print in a template instantiation backtrace (0 = no limit).", "<N>")
OPTION("-ftemplate-depth", ftemplate_depth, Separate, INVALID, INVALID, 0, 0,
       "Maximum depth of recursive template instantiation", 0)
OPTION("-ftime-report", ftime_report, Flag, INVALID, INVALID, 0, 0,
       "Print the amount of time each phase of compilation takes", 0)
OPTION("-ftrapv", ftrapv, Flag, INVALID, INVALID, 0, 0,
       "Trap on integer overflow", 0)
OPTION("-funroll-loops", funroll_loops, Flag, INVALID, INVALID, 0, 0,
       "Turn on loop unroller", 0)
OPTION("-fvisibility-inlines-hidden", fvisibility_inlines_hidden, Flag, INVALID, INVALID, 0, 0,
       "Give inline C++ member functions default visibility by default", 0)
OPTION("-fvisibility", fvisibility, Separate, INVALID, INVALID, 0, 0,
       "Default symbol visibility", 0)
OPTION("-fwrapv", fwrapv, Flag, INVALID, INVALID, 0, 0,
       "Treat signed integer overflow as two's complement", 0)
OPTION("-fwritable-strings", fwritable_strings, Flag, INVALID, INVALID, 0, 0,
       "Store string literals as writable data", 0)
OPTION("-g", g, Flag, INVALID, INVALID, 0, 0,
       "Generate source level debug information", 0)
OPTION("-help", help, Flag, INVALID, INVALID, 0, 0,
       "Print this help text", 0)
OPTION("-idirafter", idirafter, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Add directory to AFTER include search path", "<directory>")
OPTION("-imacros", imacros, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Include macros from file before parsing", "<file>")
OPTION("-import-module", import_module, Separate, INVALID, INVALID, 0, 0,
       "Import a module definition file", 0)
OPTION("-include-pch", include_pch, Separate, INVALID, INVALID, 0, 0,
       "Include precompiled header file", "<file>")
OPTION("-include-pth", include_pth, Separate, INVALID, INVALID, 0, 0,
       "Include file before parsing", "<file>")
OPTION("-include", include, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Include file before parsing", "<file>")
OPTION("-init-only", init_only, Flag, Action_Group, INVALID, 0, 0,
       "Only execute frontend initialization", 0)
OPTION("-iprefix", iprefix, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Set the -iwithprefix/-iwithprefixbefore prefix", "<prefix>")
OPTION("-iquote", iquote, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Add directory to QUOTE include search path", "<directory>")
OPTION("-isysroot", isysroot, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Set the system root directory (usually /)", "<dir>")
OPTION("-isystem", isystem, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Add directory to SYSTEM include search path", "<directory>")
OPTION("-iwithprefixbefore", iwithprefixbefore, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Set directory to include search path with prefix", "<dir>")
OPTION("-iwithprefix", iwithprefix, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Set directory to SYSTEM include search path with prefix", "<dir>")
OPTION("-iwithsysroot", iwithsysroot, JoinedOrSeparate, INVALID, INVALID, 0, 0,
       "Add directory to SYSTEM include search path, absolute paths are relative to -isysroot", "<directory>")
OPTION("-load", load, Separate, INVALID, INVALID, 0, 0,
       "Load the named plugin (dynamic shared object)", "<dsopath>")
OPTION("-main-file-name", main_file_name, Separate, INVALID, INVALID, 0, 0,
       "Main file name to use for debug info", 0)
OPTION("-masm-verbose", masm_verbose, Flag, INVALID, INVALID, 0, 0,
       "Generate verbose assembly output", 0)
OPTION("-mcode-model", mcode_model, Separate, INVALID, INVALID, 0, 0,
       "The code model to use", 0)
OPTION("-mconstructor-aliases", mconstructor_aliases, Flag, INVALID, INVALID, 0, 0,
       "Emit complete constructors and destructors as aliases when possible", 0)
OPTION("-mdebug-pass", mdebug_pass, Separate, INVALID, INVALID, 0, 0,
       "Enable additional debug output", 0)
OPTION("-mdisable-fp-elim", mdisable_fp_elim, Flag, INVALID, INVALID, 0, 0,
       "Disable frame pointer elimination optimization", 0)
OPTION("-mfloat-abi", mfloat_abi, Separate, INVALID, INVALID, 0, 0,
       "The float ABI to use", 0)
OPTION("-mlimit-float-precision", mlimit_float_precision, Separate, INVALID, INVALID, 0, 0,
       "Limit float precision to the given value", 0)
OPTION("-mllvm", mllvm, Separate, INVALID, INVALID, 0, 0,
       "Additional arguments to forward to LLVM's option processing", 0)
OPTION("-mno-zero-initialized-in-bss", mno_zero_initialized_in_bss, Flag, INVALID, INVALID, 0, 0,
       "Do not put zero initialized data in the BSS", 0)
OPTION("-momit-leaf-frame-pointer", momit_leaf_frame_pointer, Flag, INVALID, INVALID, 0, 0,
       "Omit frame pointer setup for leaf functions.", 0)
OPTION("-mrelax-all", mrelax_all, Flag, INVALID, INVALID, 0, 0,
       "Relax all machine instructions", 0)
OPTION("-mrelocation-model", mrelocation_model, Separate, INVALID, INVALID, 0, 0,
       "The relocation model to use", 0)
OPTION("-msoft-float", msoft_float, Flag, INVALID, INVALID, 0, 0,
       "Use software floating point", 0)
OPTION("-munwind-tables", munwind_tables, Flag, INVALID, INVALID, 0, 0,
       "Generate unwinding tables for all functions", 0)
OPTION("-no-code-completion-debug-printer", no_code_completion_debug_printer, Flag, INVALID, INVALID, 0, 0,
       "Don't use the \"debug\" code-completion print", 0)
OPTION("-no-code-completion-globals", no_code_completion_globals, Flag, INVALID, INVALID, 0, 0,
       "Do not include global declarations in code-completion results.", 0)
OPTION("-no-implicit-float", no_implicit_float, Flag, INVALID, INVALID, 0, 0,
       "Don't generate implicit floating point instructions (x86-only)", 0)
OPTION("-nobuiltininc", nobuiltininc, Flag, INVALID, INVALID, 0, 0,
       "Disable builtin #include directories", 0)
OPTION("-nostdinc++", nostdincxx, Flag, INVALID, INVALID, 0, 0,
       "Disable standard #include directories for the C++ standard library", 0)
OPTION("-nostdinc", nostdinc, Flag, INVALID, INVALID, 0, 0,
       "Disable standard #include directories", 0)
OPTION("-o", o, Separate, INVALID, INVALID, 0, 0,
       "Specify output file", "<path>")
OPTION("-pedantic-errors", pedantic_errors, Flag, INVALID, INVALID, 0, 0, 0, 0)
OPTION("-pedantic", pedantic, Flag, INVALID, INVALID, 0, 0, 0, 0)
OPTION("-pic-level", pic_level, Separate, INVALID, INVALID, 0, 0,
       "Value for __PIC__", 0)
OPTION("-plugin-arg-", plugin_arg, JoinedAndSeparate, INVALID, INVALID, 0, 0,
       "Pass <arg> to plugin <name>", "<name> <arg>")
OPTION("-plugin", plugin, Separate, INVALID, INVALID, 0, 0,
       "Use the named plugin action (use \"help\" to list available options)", "<name>")
OPTION("-preamble-bytes=", preamble_bytes_EQ, Joined, INVALID, INVALID, 0, 0,
       "Assume that the precompiled header is a precompiled preamble covering the first N bytes of the main file", 0)
OPTION("-print-decl-contexts", print_decl_contexts, Flag, Action_Group, INVALID, 0, 0,
       "Print DeclContexts and their Decls", 0)
OPTION("-print-ivar-layout", print_ivar_layout, Flag, INVALID, INVALID, 0, 0,
       "Enable Objective-C Ivar layout bitmap print trace", 0)
OPTION("-print-preamble", print_preamble, Flag, Action_Group, INVALID, 0, 0,
       "Print the \"preamble\" of a file, which is a candidate for implicit precompiled headers.", 0)
OPTION("-print-stats", print_stats, Flag, INVALID, INVALID, 0, 0,
       "Print performance metrics and statistics", 0)
OPTION("-pthread", pthread, Flag, INVALID, INVALID, 0, 0,
       "Support POSIX threads in generated code", 0)
OPTION("-relocatable-pch", relocatable_pch, Flag, INVALID, INVALID, 0, 0,
       "Whether to build a relocatable precompiled header", 0)
OPTION("-remap-file", remap_file, Separate, INVALID, INVALID, 0, 0,
       "Replace the contents of the <from> file with the contents of the <to> file", "<from>;<to>")
OPTION("-resource-dir", resource_dir, Separate, INVALID, INVALID, 0, 0,
       "The directory which holds the compiler resource files", 0)
OPTION("-rewrite-macros", rewrite_macros, Flag, Action_Group, INVALID, 0, 0,
       "Expand macros without full preprocessing", 0)
OPTION("-rewrite-objc", rewrite_objc, Flag, Action_Group, INVALID, 0, 0,
       "Rewrite ObjC into C (code rewriter example)", 0)
OPTION("-rewrite-test", rewrite_test, Flag, Action_Group, INVALID, 0, 0,
       "Rewriter playground", 0)
OPTION("-stack-protector", stack_protector, Separate, INVALID, INVALID, 0, 0,
       "Enable stack protectors", 0)
OPTION("-static-define", static_define, Flag, INVALID, INVALID, 0, 0,
       "Should __STATIC__ be defined", 0)
OPTION("-std=", std_EQ, Joined, INVALID, INVALID, 0, 0,
       "Language standard to compile for", 0)
OPTION("-sys-header-deps", sys_header_deps, Flag, INVALID, INVALID, 0, 0,
       "Include system headers in dependency output", 0)
OPTION("-target-abi", target_abi, Separate, INVALID, INVALID, 0, 0,
       "Target a particular ABI type", 0)
OPTION("-target-cpu", target_cpu, Separate, INVALID, INVALID, 0, 0,
       "Target a specific cpu type", 0)
OPTION("-target-feature", target_feature, Separate, INVALID, INVALID, 0, 0,
       "Target specific attributes", 0)
OPTION("-target-linker-version", target_linker_version, Separate, INVALID, INVALID, 0, 0,
       "Target linker version", 0)
OPTION("-token-cache", token_cache, Separate, INVALID, INVALID, 0, 0,
       "Use specified token cache file", "<path>")
OPTION("-trigraphs", trigraphs, Flag, INVALID, INVALID, 0, 0,
       "Process trigraph sequences", 0)
OPTION("-trim-egraph", trim_egraph, Flag, INVALID, INVALID, 0, 0,
       "Only show error-related paths in the analysis graph", 0)
OPTION("-triple=", triple_EQ, Joined, INVALID, triple, 0, 0, 0, 0)
OPTION("-triple", triple, Separate, INVALID, INVALID, 0, 0,
       "Specify target triple (e.g. i686-apple-darwin9)", 0)
OPTION("-undef", undef, Flag, INVALID, INVALID, 0, 0,
       "undef all system defines", "<macro>")
OPTION("-unoptimized-cfg", analysis_UnoptimizedCFG, Flag, INVALID, INVALID, 0, 0,
       "Generate unoptimized CFGs for all analyses", 0)
OPTION("-verify", verify, Flag, INVALID, INVALID, 0, 0,
       "Verify emitted diagnostics and warnings", 0)
OPTION("-version", version, Flag, INVALID, INVALID, 0, 0,
       "Print the compiler version", 0)
OPTION("-v", v, Flag, INVALID, INVALID, 0, 0,
       "Enable verbose output", 0)
OPTION("-warn-sizeof-pointer", analysis_WarnSizeofPointer, Flag, INVALID, INVALID, 0, 0,
       "Warn about unintended use of sizeof() on pointer expressions", 0)
OPTION("-warn-uninit-values", analysis_WarnUninitVals, Flag, INVALID, INVALID, 0, 0,
       "Warn about uses of uninitialized variables", 0)
OPTION("-w", w, Flag, INVALID, INVALID, 0, 0,
       "Suppress all warnings", 0)
OPTION("-x", x, Separate, INVALID, INVALID, 0, 0,
       "Input language type", 0)
