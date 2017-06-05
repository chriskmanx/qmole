#ifndef ABSTRACT_STMT
#  define ABSTRACT_STMT(Type) Type
#endif
#ifndef STMT_RANGE
#  define STMT_RANGE(Base, First, Last)
#endif

#ifndef LAST_STMT_RANGE
#  define LAST_STMT_RANGE(Base, First, Last) STMT_RANGE(Base, First, Last)
#endif

#ifndef ASMSTMT
#  define ASMSTMT(Type, Base) STMT(Type, Base)
#endif
ASMSTMT(AsmStmt, Stmt)
#undef ASMSTMT

#ifndef BREAKSTMT
#  define BREAKSTMT(Type, Base) STMT(Type, Base)
#endif
BREAKSTMT(BreakStmt, Stmt)
#undef BREAKSTMT

#ifndef CXXCATCHSTMT
#  define CXXCATCHSTMT(Type, Base) STMT(Type, Base)
#endif
CXXCATCHSTMT(CXXCatchStmt, Stmt)
#undef CXXCATCHSTMT

#ifndef CXXTRYSTMT
#  define CXXTRYSTMT(Type, Base) STMT(Type, Base)
#endif
CXXTRYSTMT(CXXTryStmt, Stmt)
#undef CXXTRYSTMT

#ifndef COMPOUNDSTMT
#  define COMPOUNDSTMT(Type, Base) STMT(Type, Base)
#endif
COMPOUNDSTMT(CompoundStmt, Stmt)
#undef COMPOUNDSTMT

#ifndef CONTINUESTMT
#  define CONTINUESTMT(Type, Base) STMT(Type, Base)
#endif
CONTINUESTMT(ContinueStmt, Stmt)
#undef CONTINUESTMT

#ifndef DECLSTMT
#  define DECLSTMT(Type, Base) STMT(Type, Base)
#endif
DECLSTMT(DeclStmt, Stmt)
#undef DECLSTMT

#ifndef DOSTMT
#  define DOSTMT(Type, Base) STMT(Type, Base)
#endif
DOSTMT(DoStmt, Stmt)
#undef DOSTMT

#ifndef EXPR
#  define EXPR(Type, Base) STMT(Type, Base)
#endif
ABSTRACT_STMT(EXPR(Expr, Stmt))
#ifndef ADDRLABELEXPR
#  define ADDRLABELEXPR(Type, Base) EXPR(Type, Base)
#endif
ADDRLABELEXPR(AddrLabelExpr, Expr)
#undef ADDRLABELEXPR

#ifndef ARRAYSUBSCRIPTEXPR
#  define ARRAYSUBSCRIPTEXPR(Type, Base) EXPR(Type, Base)
#endif
ARRAYSUBSCRIPTEXPR(ArraySubscriptExpr, Expr)
#undef ARRAYSUBSCRIPTEXPR

#ifndef BINARYOPERATOR
#  define BINARYOPERATOR(Type, Base) EXPR(Type, Base)
#endif
BINARYOPERATOR(BinaryOperator, Expr)
#ifndef COMPOUNDASSIGNOPERATOR
#  define COMPOUNDASSIGNOPERATOR(Type, Base) BINARYOPERATOR(Type, Base)
#endif
COMPOUNDASSIGNOPERATOR(CompoundAssignOperator, BinaryOperator)
#undef COMPOUNDASSIGNOPERATOR

STMT_RANGE(BinaryOperator, BinaryOperator, CompoundAssignOperator)

#undef BINARYOPERATOR

#ifndef BLOCKDECLREFEXPR
#  define BLOCKDECLREFEXPR(Type, Base) EXPR(Type, Base)
#endif
BLOCKDECLREFEXPR(BlockDeclRefExpr, Expr)
#undef BLOCKDECLREFEXPR

#ifndef BLOCKEXPR
#  define BLOCKEXPR(Type, Base) EXPR(Type, Base)
#endif
BLOCKEXPR(BlockExpr, Expr)
#undef BLOCKEXPR

#ifndef CXXBINDTEMPORARYEXPR
#  define CXXBINDTEMPORARYEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXBINDTEMPORARYEXPR(CXXBindTemporaryExpr, Expr)
#undef CXXBINDTEMPORARYEXPR

#ifndef CXXBOOLLITERALEXPR
#  define CXXBOOLLITERALEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXBOOLLITERALEXPR(CXXBoolLiteralExpr, Expr)
#undef CXXBOOLLITERALEXPR

#ifndef CXXCONSTRUCTEXPR
#  define CXXCONSTRUCTEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXCONSTRUCTEXPR(CXXConstructExpr, Expr)
#ifndef CXXTEMPORARYOBJECTEXPR
#  define CXXTEMPORARYOBJECTEXPR(Type, Base) CXXCONSTRUCTEXPR(Type, Base)
#endif
CXXTEMPORARYOBJECTEXPR(CXXTemporaryObjectExpr, CXXConstructExpr)
#undef CXXTEMPORARYOBJECTEXPR

STMT_RANGE(CXXConstructExpr, CXXConstructExpr, CXXTemporaryObjectExpr)

#undef CXXCONSTRUCTEXPR

#ifndef CXXDEFAULTARGEXPR
#  define CXXDEFAULTARGEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXDEFAULTARGEXPR(CXXDefaultArgExpr, Expr)
#undef CXXDEFAULTARGEXPR

#ifndef CXXDELETEEXPR
#  define CXXDELETEEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXDELETEEXPR(CXXDeleteExpr, Expr)
#undef CXXDELETEEXPR

#ifndef CXXDEPENDENTSCOPEMEMBEREXPR
#  define CXXDEPENDENTSCOPEMEMBEREXPR(Type, Base) EXPR(Type, Base)
#endif
CXXDEPENDENTSCOPEMEMBEREXPR(CXXDependentScopeMemberExpr, Expr)
#undef CXXDEPENDENTSCOPEMEMBEREXPR

#ifndef CXXEXPRWITHTEMPORARIES
#  define CXXEXPRWITHTEMPORARIES(Type, Base) EXPR(Type, Base)
#endif
CXXEXPRWITHTEMPORARIES(CXXExprWithTemporaries, Expr)
#undef CXXEXPRWITHTEMPORARIES

#ifndef CXXNEWEXPR
#  define CXXNEWEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXNEWEXPR(CXXNewExpr, Expr)
#undef CXXNEWEXPR

#ifndef CXXNULLPTRLITERALEXPR
#  define CXXNULLPTRLITERALEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXNULLPTRLITERALEXPR(CXXNullPtrLiteralExpr, Expr)
#undef CXXNULLPTRLITERALEXPR

#ifndef CXXPSEUDODESTRUCTOREXPR
#  define CXXPSEUDODESTRUCTOREXPR(Type, Base) EXPR(Type, Base)
#endif
CXXPSEUDODESTRUCTOREXPR(CXXPseudoDestructorExpr, Expr)
#undef CXXPSEUDODESTRUCTOREXPR

#ifndef CXXSCALARVALUEINITEXPR
#  define CXXSCALARVALUEINITEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXSCALARVALUEINITEXPR(CXXScalarValueInitExpr, Expr)
#undef CXXSCALARVALUEINITEXPR

#ifndef CXXTHISEXPR
#  define CXXTHISEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXTHISEXPR(CXXThisExpr, Expr)
#undef CXXTHISEXPR

#ifndef CXXTHROWEXPR
#  define CXXTHROWEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXTHROWEXPR(CXXThrowExpr, Expr)
#undef CXXTHROWEXPR

#ifndef CXXTYPEIDEXPR
#  define CXXTYPEIDEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXTYPEIDEXPR(CXXTypeidExpr, Expr)
#undef CXXTYPEIDEXPR

#ifndef CXXUNRESOLVEDCONSTRUCTEXPR
#  define CXXUNRESOLVEDCONSTRUCTEXPR(Type, Base) EXPR(Type, Base)
#endif
CXXUNRESOLVEDCONSTRUCTEXPR(CXXUnresolvedConstructExpr, Expr)
#undef CXXUNRESOLVEDCONSTRUCTEXPR

#ifndef CALLEXPR
#  define CALLEXPR(Type, Base) EXPR(Type, Base)
#endif
CALLEXPR(CallExpr, Expr)
#ifndef CXXMEMBERCALLEXPR
#  define CXXMEMBERCALLEXPR(Type, Base) CALLEXPR(Type, Base)
#endif
CXXMEMBERCALLEXPR(CXXMemberCallExpr, CallExpr)
#undef CXXMEMBERCALLEXPR

#ifndef CXXOPERATORCALLEXPR
#  define CXXOPERATORCALLEXPR(Type, Base) CALLEXPR(Type, Base)
#endif
CXXOPERATORCALLEXPR(CXXOperatorCallExpr, CallExpr)
#undef CXXOPERATORCALLEXPR

STMT_RANGE(CallExpr, CallExpr, CXXOperatorCallExpr)

#undef CALLEXPR

#ifndef CASTEXPR
#  define CASTEXPR(Type, Base) EXPR(Type, Base)
#endif
ABSTRACT_STMT(CASTEXPR(CastExpr, Expr))
#ifndef EXPLICITCASTEXPR
#  define EXPLICITCASTEXPR(Type, Base) CASTEXPR(Type, Base)
#endif
ABSTRACT_STMT(EXPLICITCASTEXPR(ExplicitCastExpr, CastExpr))
#ifndef CSTYLECASTEXPR
#  define CSTYLECASTEXPR(Type, Base) EXPLICITCASTEXPR(Type, Base)
#endif
CSTYLECASTEXPR(CStyleCastExpr, ExplicitCastExpr)
#undef CSTYLECASTEXPR

#ifndef CXXFUNCTIONALCASTEXPR
#  define CXXFUNCTIONALCASTEXPR(Type, Base) EXPLICITCASTEXPR(Type, Base)
#endif
CXXFUNCTIONALCASTEXPR(CXXFunctionalCastExpr, ExplicitCastExpr)
#undef CXXFUNCTIONALCASTEXPR

#ifndef CXXNAMEDCASTEXPR
#  define CXXNAMEDCASTEXPR(Type, Base) EXPLICITCASTEXPR(Type, Base)
#endif
ABSTRACT_STMT(CXXNAMEDCASTEXPR(CXXNamedCastExpr, ExplicitCastExpr))
#ifndef CXXCONSTCASTEXPR
#  define CXXCONSTCASTEXPR(Type, Base) CXXNAMEDCASTEXPR(Type, Base)
#endif
CXXCONSTCASTEXPR(CXXConstCastExpr, CXXNamedCastExpr)
#undef CXXCONSTCASTEXPR

#ifndef CXXDYNAMICCASTEXPR
#  define CXXDYNAMICCASTEXPR(Type, Base) CXXNAMEDCASTEXPR(Type, Base)
#endif
CXXDYNAMICCASTEXPR(CXXDynamicCastExpr, CXXNamedCastExpr)
#undef CXXDYNAMICCASTEXPR

#ifndef CXXREINTERPRETCASTEXPR
#  define CXXREINTERPRETCASTEXPR(Type, Base) CXXNAMEDCASTEXPR(Type, Base)
#endif
CXXREINTERPRETCASTEXPR(CXXReinterpretCastExpr, CXXNamedCastExpr)
#undef CXXREINTERPRETCASTEXPR

#ifndef CXXSTATICCASTEXPR
#  define CXXSTATICCASTEXPR(Type, Base) CXXNAMEDCASTEXPR(Type, Base)
#endif
CXXSTATICCASTEXPR(CXXStaticCastExpr, CXXNamedCastExpr)
#undef CXXSTATICCASTEXPR

STMT_RANGE(CXXNamedCastExpr, CXXConstCastExpr, CXXStaticCastExpr)

#undef CXXNAMEDCASTEXPR

STMT_RANGE(ExplicitCastExpr, CStyleCastExpr, CXXStaticCastExpr)

#undef EXPLICITCASTEXPR

#ifndef IMPLICITCASTEXPR
#  define IMPLICITCASTEXPR(Type, Base) CASTEXPR(Type, Base)
#endif
IMPLICITCASTEXPR(ImplicitCastExpr, CastExpr)
#undef IMPLICITCASTEXPR

STMT_RANGE(CastExpr, CStyleCastExpr, ImplicitCastExpr)

#undef CASTEXPR

#ifndef CHARACTERLITERAL
#  define CHARACTERLITERAL(Type, Base) EXPR(Type, Base)
#endif
CHARACTERLITERAL(CharacterLiteral, Expr)
#undef CHARACTERLITERAL

#ifndef CHOOSEEXPR
#  define CHOOSEEXPR(Type, Base) EXPR(Type, Base)
#endif
CHOOSEEXPR(ChooseExpr, Expr)
#undef CHOOSEEXPR

#ifndef COMPOUNDLITERALEXPR
#  define COMPOUNDLITERALEXPR(Type, Base) EXPR(Type, Base)
#endif
COMPOUNDLITERALEXPR(CompoundLiteralExpr, Expr)
#undef COMPOUNDLITERALEXPR

#ifndef CONDITIONALOPERATOR
#  define CONDITIONALOPERATOR(Type, Base) EXPR(Type, Base)
#endif
CONDITIONALOPERATOR(ConditionalOperator, Expr)
#undef CONDITIONALOPERATOR

#ifndef DECLREFEXPR
#  define DECLREFEXPR(Type, Base) EXPR(Type, Base)
#endif
DECLREFEXPR(DeclRefExpr, Expr)
#undef DECLREFEXPR

#ifndef DEPENDENTSCOPEDECLREFEXPR
#  define DEPENDENTSCOPEDECLREFEXPR(Type, Base) EXPR(Type, Base)
#endif
DEPENDENTSCOPEDECLREFEXPR(DependentScopeDeclRefExpr, Expr)
#undef DEPENDENTSCOPEDECLREFEXPR

#ifndef DESIGNATEDINITEXPR
#  define DESIGNATEDINITEXPR(Type, Base) EXPR(Type, Base)
#endif
DESIGNATEDINITEXPR(DesignatedInitExpr, Expr)
#undef DESIGNATEDINITEXPR

#ifndef EXTVECTORELEMENTEXPR
#  define EXTVECTORELEMENTEXPR(Type, Base) EXPR(Type, Base)
#endif
EXTVECTORELEMENTEXPR(ExtVectorElementExpr, Expr)
#undef EXTVECTORELEMENTEXPR

#ifndef FLOATINGLITERAL
#  define FLOATINGLITERAL(Type, Base) EXPR(Type, Base)
#endif
FLOATINGLITERAL(FloatingLiteral, Expr)
#undef FLOATINGLITERAL

#ifndef GNUNULLEXPR
#  define GNUNULLEXPR(Type, Base) EXPR(Type, Base)
#endif
GNUNULLEXPR(GNUNullExpr, Expr)
#undef GNUNULLEXPR

#ifndef IMAGINARYLITERAL
#  define IMAGINARYLITERAL(Type, Base) EXPR(Type, Base)
#endif
IMAGINARYLITERAL(ImaginaryLiteral, Expr)
#undef IMAGINARYLITERAL

#ifndef IMPLICITVALUEINITEXPR
#  define IMPLICITVALUEINITEXPR(Type, Base) EXPR(Type, Base)
#endif
IMPLICITVALUEINITEXPR(ImplicitValueInitExpr, Expr)
#undef IMPLICITVALUEINITEXPR

#ifndef INITLISTEXPR
#  define INITLISTEXPR(Type, Base) EXPR(Type, Base)
#endif
INITLISTEXPR(InitListExpr, Expr)
#undef INITLISTEXPR

#ifndef INTEGERLITERAL
#  define INTEGERLITERAL(Type, Base) EXPR(Type, Base)
#endif
INTEGERLITERAL(IntegerLiteral, Expr)
#undef INTEGERLITERAL

#ifndef MEMBEREXPR
#  define MEMBEREXPR(Type, Base) EXPR(Type, Base)
#endif
MEMBEREXPR(MemberExpr, Expr)
#undef MEMBEREXPR

#ifndef OBJCENCODEEXPR
#  define OBJCENCODEEXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCENCODEEXPR(ObjCEncodeExpr, Expr)
#undef OBJCENCODEEXPR

#ifndef OBJCIMPLICITSETTERGETTERREFEXPR
#  define OBJCIMPLICITSETTERGETTERREFEXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCIMPLICITSETTERGETTERREFEXPR(ObjCImplicitSetterGetterRefExpr, Expr)
#undef OBJCIMPLICITSETTERGETTERREFEXPR

#ifndef OBJCISAEXPR
#  define OBJCISAEXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCISAEXPR(ObjCIsaExpr, Expr)
#undef OBJCISAEXPR

#ifndef OBJCIVARREFEXPR
#  define OBJCIVARREFEXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCIVARREFEXPR(ObjCIvarRefExpr, Expr)
#undef OBJCIVARREFEXPR

#ifndef OBJCMESSAGEEXPR
#  define OBJCMESSAGEEXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCMESSAGEEXPR(ObjCMessageExpr, Expr)
#undef OBJCMESSAGEEXPR

#ifndef OBJCPROPERTYREFEXPR
#  define OBJCPROPERTYREFEXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCPROPERTYREFEXPR(ObjCPropertyRefExpr, Expr)
#undef OBJCPROPERTYREFEXPR

#ifndef OBJCPROTOCOLEXPR
#  define OBJCPROTOCOLEXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCPROTOCOLEXPR(ObjCProtocolExpr, Expr)
#undef OBJCPROTOCOLEXPR

#ifndef OBJCSELECTOREXPR
#  define OBJCSELECTOREXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCSELECTOREXPR(ObjCSelectorExpr, Expr)
#undef OBJCSELECTOREXPR

#ifndef OBJCSTRINGLITERAL
#  define OBJCSTRINGLITERAL(Type, Base) EXPR(Type, Base)
#endif
OBJCSTRINGLITERAL(ObjCStringLiteral, Expr)
#undef OBJCSTRINGLITERAL

#ifndef OBJCSUPEREXPR
#  define OBJCSUPEREXPR(Type, Base) EXPR(Type, Base)
#endif
OBJCSUPEREXPR(ObjCSuperExpr, Expr)
#undef OBJCSUPEREXPR

#ifndef OFFSETOFEXPR
#  define OFFSETOFEXPR(Type, Base) EXPR(Type, Base)
#endif
OFFSETOFEXPR(OffsetOfExpr, Expr)
#undef OFFSETOFEXPR

#ifndef OVERLOADEXPR
#  define OVERLOADEXPR(Type, Base) EXPR(Type, Base)
#endif
ABSTRACT_STMT(OVERLOADEXPR(OverloadExpr, Expr))
#ifndef UNRESOLVEDLOOKUPEXPR
#  define UNRESOLVEDLOOKUPEXPR(Type, Base) OVERLOADEXPR(Type, Base)
#endif
UNRESOLVEDLOOKUPEXPR(UnresolvedLookupExpr, OverloadExpr)
#undef UNRESOLVEDLOOKUPEXPR

#ifndef UNRESOLVEDMEMBEREXPR
#  define UNRESOLVEDMEMBEREXPR(Type, Base) OVERLOADEXPR(Type, Base)
#endif
UNRESOLVEDMEMBEREXPR(UnresolvedMemberExpr, OverloadExpr)
#undef UNRESOLVEDMEMBEREXPR

STMT_RANGE(OverloadExpr, UnresolvedLookupExpr, UnresolvedMemberExpr)

#undef OVERLOADEXPR

#ifndef PARENEXPR
#  define PARENEXPR(Type, Base) EXPR(Type, Base)
#endif
PARENEXPR(ParenExpr, Expr)
#undef PARENEXPR

#ifndef PARENLISTEXPR
#  define PARENLISTEXPR(Type, Base) EXPR(Type, Base)
#endif
PARENLISTEXPR(ParenListExpr, Expr)
#undef PARENLISTEXPR

#ifndef PREDEFINEDEXPR
#  define PREDEFINEDEXPR(Type, Base) EXPR(Type, Base)
#endif
PREDEFINEDEXPR(PredefinedExpr, Expr)
#undef PREDEFINEDEXPR

#ifndef SHUFFLEVECTOREXPR
#  define SHUFFLEVECTOREXPR(Type, Base) EXPR(Type, Base)
#endif
SHUFFLEVECTOREXPR(ShuffleVectorExpr, Expr)
#undef SHUFFLEVECTOREXPR

#ifndef SIZEOFALIGNOFEXPR
#  define SIZEOFALIGNOFEXPR(Type, Base) EXPR(Type, Base)
#endif
SIZEOFALIGNOFEXPR(SizeOfAlignOfExpr, Expr)
#undef SIZEOFALIGNOFEXPR

#ifndef STMTEXPR
#  define STMTEXPR(Type, Base) EXPR(Type, Base)
#endif
STMTEXPR(StmtExpr, Expr)
#undef STMTEXPR

#ifndef STRINGLITERAL
#  define STRINGLITERAL(Type, Base) EXPR(Type, Base)
#endif
STRINGLITERAL(StringLiteral, Expr)
#undef STRINGLITERAL

#ifndef TYPESCOMPATIBLEEXPR
#  define TYPESCOMPATIBLEEXPR(Type, Base) EXPR(Type, Base)
#endif
TYPESCOMPATIBLEEXPR(TypesCompatibleExpr, Expr)
#undef TYPESCOMPATIBLEEXPR

#ifndef UNARYOPERATOR
#  define UNARYOPERATOR(Type, Base) EXPR(Type, Base)
#endif
UNARYOPERATOR(UnaryOperator, Expr)
#undef UNARYOPERATOR

#ifndef UNARYTYPETRAITEXPR
#  define UNARYTYPETRAITEXPR(Type, Base) EXPR(Type, Base)
#endif
UNARYTYPETRAITEXPR(UnaryTypeTraitExpr, Expr)
#undef UNARYTYPETRAITEXPR

#ifndef VAARGEXPR
#  define VAARGEXPR(Type, Base) EXPR(Type, Base)
#endif
VAARGEXPR(VAArgExpr, Expr)
#undef VAARGEXPR

STMT_RANGE(Expr, AddrLabelExpr, VAArgExpr)

#undef EXPR

#ifndef FORSTMT
#  define FORSTMT(Type, Base) STMT(Type, Base)
#endif
FORSTMT(ForStmt, Stmt)
#undef FORSTMT

#ifndef GOTOSTMT
#  define GOTOSTMT(Type, Base) STMT(Type, Base)
#endif
GOTOSTMT(GotoStmt, Stmt)
#undef GOTOSTMT

#ifndef IFSTMT
#  define IFSTMT(Type, Base) STMT(Type, Base)
#endif
IFSTMT(IfStmt, Stmt)
#undef IFSTMT

#ifndef INDIRECTGOTOSTMT
#  define INDIRECTGOTOSTMT(Type, Base) STMT(Type, Base)
#endif
INDIRECTGOTOSTMT(IndirectGotoStmt, Stmt)
#undef INDIRECTGOTOSTMT

#ifndef LABELSTMT
#  define LABELSTMT(Type, Base) STMT(Type, Base)
#endif
LABELSTMT(LabelStmt, Stmt)
#undef LABELSTMT

#ifndef NULLSTMT
#  define NULLSTMT(Type, Base) STMT(Type, Base)
#endif
NULLSTMT(NullStmt, Stmt)
#undef NULLSTMT

#ifndef OBJCATCATCHSTMT
#  define OBJCATCATCHSTMT(Type, Base) STMT(Type, Base)
#endif
OBJCATCATCHSTMT(ObjCAtCatchStmt, Stmt)
#undef OBJCATCATCHSTMT

#ifndef OBJCATFINALLYSTMT
#  define OBJCATFINALLYSTMT(Type, Base) STMT(Type, Base)
#endif
OBJCATFINALLYSTMT(ObjCAtFinallyStmt, Stmt)
#undef OBJCATFINALLYSTMT

#ifndef OBJCATSYNCHRONIZEDSTMT
#  define OBJCATSYNCHRONIZEDSTMT(Type, Base) STMT(Type, Base)
#endif
OBJCATSYNCHRONIZEDSTMT(ObjCAtSynchronizedStmt, Stmt)
#undef OBJCATSYNCHRONIZEDSTMT

#ifndef OBJCATTHROWSTMT
#  define OBJCATTHROWSTMT(Type, Base) STMT(Type, Base)
#endif
OBJCATTHROWSTMT(ObjCAtThrowStmt, Stmt)
#undef OBJCATTHROWSTMT

#ifndef OBJCATTRYSTMT
#  define OBJCATTRYSTMT(Type, Base) STMT(Type, Base)
#endif
OBJCATTRYSTMT(ObjCAtTryStmt, Stmt)
#undef OBJCATTRYSTMT

#ifndef OBJCFORCOLLECTIONSTMT
#  define OBJCFORCOLLECTIONSTMT(Type, Base) STMT(Type, Base)
#endif
OBJCFORCOLLECTIONSTMT(ObjCForCollectionStmt, Stmt)
#undef OBJCFORCOLLECTIONSTMT

#ifndef RETURNSTMT
#  define RETURNSTMT(Type, Base) STMT(Type, Base)
#endif
RETURNSTMT(ReturnStmt, Stmt)
#undef RETURNSTMT

#ifndef SWITCHCASE
#  define SWITCHCASE(Type, Base) STMT(Type, Base)
#endif
SWITCHCASE(SwitchCase, Stmt)
#ifndef CASESTMT
#  define CASESTMT(Type, Base) SWITCHCASE(Type, Base)
#endif
CASESTMT(CaseStmt, SwitchCase)
#undef CASESTMT

#ifndef DEFAULTSTMT
#  define DEFAULTSTMT(Type, Base) SWITCHCASE(Type, Base)
#endif
DEFAULTSTMT(DefaultStmt, SwitchCase)
#undef DEFAULTSTMT

STMT_RANGE(SwitchCase, SwitchCase, DefaultStmt)

#undef SWITCHCASE

#ifndef SWITCHSTMT
#  define SWITCHSTMT(Type, Base) STMT(Type, Base)
#endif
SWITCHSTMT(SwitchStmt, Stmt)
#undef SWITCHSTMT

#ifndef WHILESTMT
#  define WHILESTMT(Type, Base) STMT(Type, Base)
#endif
WHILESTMT(WhileStmt, Stmt)
#undef WHILESTMT

LAST_STMT_RANGE(Stmt, AsmStmt, WhileStmt)

#undef STMT
#undef STMT_RANGE
#undef LAST_STMT_RANGE
#undef ABSTRACT_STMT
