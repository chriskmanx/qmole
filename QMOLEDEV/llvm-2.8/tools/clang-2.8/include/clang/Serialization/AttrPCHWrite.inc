  switch (A->getKind()) {
  default:
    llvm_unreachable("Unknown attribute kind!");
    break;
  case attr::Alias: {
    const AliasAttr *SA = cast<AliasAttr>(A);
    AddString(SA->getAliasee(), Record);
    break;
  }
  case attr::AlignMac68k: {
    break;
  }
  case attr::Aligned: {
    const AlignedAttr *SA = cast<AlignedAttr>(A);
    Record.push_back(SA->isAlignmentExpr());
    if (SA->isAlignmentExpr())
      AddStmt(SA->getAlignmentExpr());
    else
      AddTypeSourceInfo(SA->getAlignmentType(), Record);
    break;
  }
  case attr::AlwaysInline: {
    break;
  }
  case attr::AnalyzerNoReturn: {
    break;
  }
  case attr::Annotate: {
    const AnnotateAttr *SA = cast<AnnotateAttr>(A);
    AddString(SA->getAnnotation(), Record);
    break;
  }
  case attr::AsmLabel: {
    const AsmLabelAttr *SA = cast<AsmLabelAttr>(A);
    AddString(SA->getLabel(), Record);
    break;
  }
  case attr::BaseCheck: {
    break;
  }
  case attr::Blocks: {
    const BlocksAttr *SA = cast<BlocksAttr>(A);
Record.push_back(SA->getType());
    break;
  }
  case attr::CDecl: {
    break;
  }
  case attr::CFReturnsNotRetained: {
    break;
  }
  case attr::CFReturnsRetained: {
    break;
  }
  case attr::CarriesDependency: {
    break;
  }
  case attr::Cleanup: {
    const CleanupAttr *SA = cast<CleanupAttr>(A);
    AddDeclRef(SA->getFunctionDecl(), Record);
    break;
  }
  case attr::Const: {
    break;
  }
  case attr::Constructor: {
    const ConstructorAttr *SA = cast<ConstructorAttr>(A);
    Record.push_back(SA->getPriority());
    break;
  }
  case attr::DLLExport: {
    break;
  }
  case attr::DLLImport: {
    break;
  }
  case attr::Deprecated: {
    break;
  }
  case attr::Destructor: {
    const DestructorAttr *SA = cast<DestructorAttr>(A);
    Record.push_back(SA->getPriority());
    break;
  }
  case attr::FastCall: {
    break;
  }
  case attr::Final: {
    break;
  }
  case attr::Format: {
    const FormatAttr *SA = cast<FormatAttr>(A);
    AddString(SA->getType(), Record);
    Record.push_back(SA->getFormatIdx());
    Record.push_back(SA->getFirstArg());
    break;
  }
  case attr::FormatArg: {
    const FormatArgAttr *SA = cast<FormatArgAttr>(A);
    Record.push_back(SA->getFormatIdx());
    break;
  }
  case attr::GNUInline: {
    break;
  }
  case attr::Hiding: {
    break;
  }
  case attr::IBAction: {
    break;
  }
  case attr::IBOutlet: {
    break;
  }
  case attr::IBOutletCollection: {
    const IBOutletCollectionAttr *SA = cast<IBOutletCollectionAttr>(A);
    AddTypeRef(SA->getInterface(), Record);
    break;
  }
  case attr::InitPriority: {
    const InitPriorityAttr *SA = cast<InitPriorityAttr>(A);
    Record.push_back(SA->getPriority());
    break;
  }
  case attr::MSP430Interrupt: {
    const MSP430InterruptAttr *SA = cast<MSP430InterruptAttr>(A);
    Record.push_back(SA->getNumber());
    break;
  }
  case attr::Malloc: {
    break;
  }
  case attr::MaxFieldAlignment: {
    const MaxFieldAlignmentAttr *SA = cast<MaxFieldAlignmentAttr>(A);
    Record.push_back(SA->getAlignment());
    break;
  }
  case attr::NSReturnsNotRetained: {
    break;
  }
  case attr::NSReturnsRetained: {
    break;
  }
  case attr::NoDebug: {
    break;
  }
  case attr::NoInline: {
    break;
  }
  case attr::NoInstrumentFunction: {
    break;
  }
  case attr::NoReturn: {
    break;
  }
  case attr::NoThrow: {
    break;
  }
  case attr::NonNull: {
    const NonNullAttr *SA = cast<NonNullAttr>(A);
    Record.push_back(SA->args_size());
    for (NonNullAttr::args_iterator i = SA->args_begin(), e = SA->args_end(); i != e; ++i)
      Record.push_back((*i));
    break;
  }
  case attr::ObjCException: {
    break;
  }
  case attr::ObjCNSObject: {
    break;
  }
  case attr::Overloadable: {
    break;
  }
  case attr::Override: {
    break;
  }
  case attr::Ownership: {
    const OwnershipAttr *SA = cast<OwnershipAttr>(A);
Record.push_back(SA->getOwnKind());
    AddString(SA->getModule(), Record);
    Record.push_back(SA->args_size());
    for (OwnershipAttr::args_iterator i = SA->args_begin(), e = SA->args_end(); i != e; ++i)
      Record.push_back((*i));
    break;
  }
  case attr::Packed: {
    break;
  }
  case attr::Pascal: {
    break;
  }
  case attr::Pure: {
    break;
  }
  case attr::Regparm: {
    const RegparmAttr *SA = cast<RegparmAttr>(A);
    Record.push_back(SA->getNumParams());
    break;
  }
  case attr::ReqdWorkGroupSize: {
    const ReqdWorkGroupSizeAttr *SA = cast<ReqdWorkGroupSizeAttr>(A);
    Record.push_back(SA->getXDim());
    Record.push_back(SA->getYDim());
    Record.push_back(SA->getZDim());
    break;
  }
  case attr::Section: {
    const SectionAttr *SA = cast<SectionAttr>(A);
    AddString(SA->getName(), Record);
    break;
  }
  case attr::Sentinel: {
    const SentinelAttr *SA = cast<SentinelAttr>(A);
    Record.push_back(SA->getSentinel());
    Record.push_back(SA->getNullPos());
    break;
  }
  case attr::StdCall: {
    break;
  }
  case attr::ThisCall: {
    break;
  }
  case attr::TransparentUnion: {
    break;
  }
  case attr::Unavailable: {
    break;
  }
  case attr::Unused: {
    break;
  }
  case attr::Used: {
    break;
  }
  case attr::VecReturn: {
    break;
  }
  case attr::Visibility: {
    const VisibilityAttr *SA = cast<VisibilityAttr>(A);
Record.push_back(SA->getVisibility());
    break;
  }
  case attr::WarnUnusedResult: {
    break;
  }
  case attr::Weak: {
    break;
  }
  case attr::WeakImport: {
    break;
  }
  case attr::WeakRef: {
    break;
  }
  case attr::X86ForceAlignArgPointer: {
    break;
  }
  }
