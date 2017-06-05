//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// DAG Instruction Selector for the Mips target
//
// Automatically generated file, do not edit!
//
//===----------------------------------------------------------------------===//

// *** NOTE: This file is #included into the middle of the target
// *** instruction selector class.  These functions are really methods.

// The main instruction selector code.
SDNode *SelectCode(SDNode *N) {
  // Opcodes are emitted as 2 bytes, TARGET_OPCODE handles this.
  #define TARGET_OPCODE(X) X & 255, unsigned(X) >> 8
  static const unsigned char MatcherTable[] = {
/*0*/     OPC_SwitchOpcode /*46 cases */, 12|128,1/*140*/,  TARGET_OPCODE(ISD::STORE),// ->145
/*5*/       OPC_RecordMemRef,
/*6*/       OPC_RecordNode,   // #0 = 'st' chained node
/*7*/       OPC_Scope, 26, /*->35*/ // 2 children in Scope
/*9*/         OPC_MoveChild, 1,
/*11*/        OPC_CheckInteger, 0, 
/*13*/        OPC_MoveParent,
/*14*/        OPC_RecordChild2, // #1 = $dst
/*15*/        OPC_CheckPredicate, 0, // Predicate_unindexedstore
/*17*/        OPC_CheckPredicate, 1, // Predicate_store
/*19*/        OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$dst #2 #3
/*22*/        OPC_EmitMergeInputChains1_0,
/*23*/        OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*26*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SW), 0|OPFL_Chain|OPFL_MemRefs,
                  0/*#VTs*/, 3/*#Ops*/, 4, 2, 3, 
              // Src: (st 0:i32, addr:iPTR:$dst)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 18
              // Dst: (SW ZERO:i32, addr:i32:$dst)
/*35*/      /*Scope*/ 108, /*->144*/
/*36*/        OPC_RecordChild1, // #1 = $dst
/*37*/        OPC_Scope, 60, /*->99*/ // 3 children in Scope
/*39*/          OPC_CheckChild1Type, MVT::i32,
/*41*/          OPC_RecordChild2, // #2 = $addr
/*42*/          OPC_CheckPredicate, 0, // Predicate_unindexedstore
/*44*/          OPC_Scope, 36, /*->82*/ // 2 children in Scope
/*46*/            OPC_CheckPredicate, 2, // Predicate_truncstore
/*48*/            OPC_Scope, 15, /*->65*/ // 2 children in Scope
/*50*/              OPC_CheckPredicate, 3, // Predicate_truncstorei8
/*52*/              OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddr:$addr #3 #4
/*55*/              OPC_EmitMergeInputChains1_0,
/*56*/              OPC_MorphNodeTo, TARGET_OPCODE(Mips::SB), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st CPURegs:i32:$dst, addr:iPTR:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 13
                    // Dst: (SB CPURegs:i32:$dst, addr:i32:$addr)
/*65*/            /*Scope*/ 15, /*->81*/
/*66*/              OPC_CheckPredicate, 4, // Predicate_truncstorei16
/*68*/              OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddr:$addr #3 #4
/*71*/              OPC_EmitMergeInputChains1_0,
/*72*/              OPC_MorphNodeTo, TARGET_OPCODE(Mips::SH), 0|OPFL_Chain|OPFL_MemRefs,
                        0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                    // Src: (st CPURegs:i32:$dst, addr:iPTR:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 13
                    // Dst: (SH CPURegs:i32:$dst, addr:i32:$addr)
/*81*/            0, /*End of Scope*/
/*82*/          /*Scope*/ 15, /*->98*/
/*83*/            OPC_CheckPredicate, 1, // Predicate_store
/*85*/            OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddr:$addr #3 #4
/*88*/            OPC_EmitMergeInputChains1_0,
/*89*/            OPC_MorphNodeTo, TARGET_OPCODE(Mips::SW), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                  // Src: (st CPURegs:i32:$dst, addr:iPTR:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                  // Dst: (SW CPURegs:i32:$dst, addr:i32:$addr)
/*98*/          0, /*End of Scope*/
/*99*/        /*Scope*/ 22, /*->122*/
/*100*/         OPC_CheckChild1Type, MVT::f64,
/*102*/         OPC_RecordChild2, // #2 = $addr
/*103*/         OPC_CheckPredicate, 0, // Predicate_unindexedstore
/*105*/         OPC_CheckPredicate, 1, // Predicate_store
/*107*/         OPC_CheckPatternPredicate, 0, // (!Subtarget.isSingleFloat()) && (!Subtarget.isMips1())
/*109*/         OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddr:$addr #3 #4
/*112*/         OPC_EmitMergeInputChains1_0,
/*113*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::SDC1), 0|OPFL_Chain|OPFL_MemRefs,
                    0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                // Src: (st AFGR64:f64:$ft, addr:iPTR:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                // Dst: (SDC1 AFGR64:f64:$ft, addr:i32:$addr)
/*122*/       /*Scope*/ 20, /*->143*/
/*123*/         OPC_CheckChild1Type, MVT::f32,
/*125*/         OPC_RecordChild2, // #2 = $addr
/*126*/         OPC_CheckPredicate, 0, // Predicate_unindexedstore
/*128*/         OPC_CheckPredicate, 1, // Predicate_store
/*130*/         OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectAddr:$addr #3 #4
/*133*/         OPC_EmitMergeInputChains1_0,
/*134*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::SWC1), 0|OPFL_Chain|OPFL_MemRefs,
                    0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                // Src: (st FGR32:f32:$ft, addr:iPTR:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                // Dst: (SWC1 FGR32:f32:$ft, addr:i32:$addr)
/*143*/       0, /*End of Scope*/
/*144*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 69|128,1/*197*/,  TARGET_OPCODE(ISD::LOAD),// ->346
/*149*/     OPC_RecordMemRef,
/*150*/     OPC_RecordNode,   // #0 = 'ld' chained node
/*151*/     OPC_RecordChild1, // #1 = $addr
/*152*/     OPC_CheckPredicate, 5, // Predicate_unindexedload
/*154*/     OPC_Scope, 19, /*->175*/ // 7 children in Scope
/*156*/       OPC_CheckPredicate, 6, // Predicate_sextload
/*158*/       OPC_CheckPredicate, 7, // Predicate_sextloadi8
/*160*/       OPC_CheckType, MVT::i32,
/*162*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$addr #2 #3
/*165*/       OPC_EmitMergeInputChains1_0,
/*166*/       OPC_MorphNodeTo, TARGET_OPCODE(Mips::LB), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 addr:iPTR:$addr)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi8>> - Complexity = 13
              // Dst: (LB:i32 addr:i32:$addr)
/*175*/     /*Scope*/ 19, /*->195*/
/*176*/       OPC_CheckPredicate, 8, // Predicate_zextload
/*178*/       OPC_CheckPredicate, 9, // Predicate_zextloadi8
/*180*/       OPC_CheckType, MVT::i32,
/*182*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$addr #2 #3
/*185*/       OPC_EmitMergeInputChains1_0,
/*186*/       OPC_MorphNodeTo, TARGET_OPCODE(Mips::LBu), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 addr:iPTR:$addr)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 13
              // Dst: (LBu:i32 addr:i32:$addr)
/*195*/     /*Scope*/ 19, /*->215*/
/*196*/       OPC_CheckPredicate, 6, // Predicate_sextload
/*198*/       OPC_CheckPredicate, 10, // Predicate_sextloadi16
/*200*/       OPC_CheckType, MVT::i32,
/*202*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$addr #2 #3
/*205*/       OPC_EmitMergeInputChains1_0,
/*206*/       OPC_MorphNodeTo, TARGET_OPCODE(Mips::LH), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 addr:iPTR:$addr)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 13
              // Dst: (LH:i32 addr:i32:$addr)
/*215*/     /*Scope*/ 19, /*->235*/
/*216*/       OPC_CheckPredicate, 8, // Predicate_zextload
/*218*/       OPC_CheckPredicate, 11, // Predicate_zextloadi16
/*220*/       OPC_CheckType, MVT::i32,
/*222*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$addr #2 #3
/*225*/       OPC_EmitMergeInputChains1_0,
/*226*/       OPC_MorphNodeTo, TARGET_OPCODE(Mips::LHu), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 addr:iPTR:$addr)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 13
              // Dst: (LHu:i32 addr:i32:$addr)
/*235*/     /*Scope*/ 17, /*->253*/
/*236*/       OPC_CheckPredicate, 12, // Predicate_load
/*238*/       OPC_CheckType, MVT::i32,
/*240*/       OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$addr #2 #3
/*243*/       OPC_EmitMergeInputChains1_0,
/*244*/       OPC_MorphNodeTo, TARGET_OPCODE(Mips::LW), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (ld:i32 addr:iPTR:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
              // Dst: (LW:i32 addr:i32:$addr)
/*253*/     /*Scope*/ 54, /*->308*/
/*254*/       OPC_CheckPredicate, 13, // Predicate_extload
/*256*/       OPC_CheckType, MVT::i32,
/*258*/       OPC_Scope, 15, /*->275*/ // 3 children in Scope
/*260*/         OPC_CheckPredicate, 14, // Predicate_extloadi1
/*262*/         OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$src #2 #3
/*265*/         OPC_EmitMergeInputChains1_0,
/*266*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::LBu), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i32 addr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi1>> - Complexity = 13
                // Dst: (LBu:i32 addr:i32:$src)
/*275*/       /*Scope*/ 15, /*->291*/
/*276*/         OPC_CheckPredicate, 15, // Predicate_extloadi8
/*278*/         OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$src #2 #3
/*281*/         OPC_EmitMergeInputChains1_0,
/*282*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::LBu), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i32 addr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 13
                // Dst: (LBu:i32 addr:i32:$src)
/*291*/       /*Scope*/ 15, /*->307*/
/*292*/         OPC_CheckPredicate, 16, // Predicate_extloadi16
/*294*/         OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$src #2 #3
/*297*/         OPC_EmitMergeInputChains1_0,
/*298*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::LHu), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i32 addr:iPTR:$src)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 13
                // Dst: (LHu:i32 addr:i32:$src)
/*307*/       0, /*End of Scope*/
/*308*/     /*Scope*/ 36, /*->345*/
/*309*/       OPC_CheckPredicate, 12, // Predicate_load
/*311*/       OPC_SwitchType /*2 cases */, 15,  MVT::f64,// ->329
/*314*/         OPC_CheckPatternPredicate, 0, // (!Subtarget.isSingleFloat()) && (!Subtarget.isMips1())
/*316*/         OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$addr #2 #3
/*319*/         OPC_EmitMergeInputChains1_0,
/*320*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::LDC1), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 3, 
                // Src: (ld:f64 addr:iPTR:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                // Dst: (LDC1:f64 addr:i32:$addr)
              /*SwitchType*/ 13,  MVT::f32,// ->344
/*331*/         OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectAddr:$addr #2 #3
/*334*/         OPC_EmitMergeInputChains1_0,
/*335*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::LWC1), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::f32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:f32 addr:iPTR:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                // Dst: (LWC1:f32 addr:i32:$addr)
              0, // EndSwitchType
/*345*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 102,  TARGET_OPCODE(ISD::XOR),// ->451
/*349*/     OPC_Scope, 31, /*->382*/ // 2 children in Scope
/*351*/       OPC_MoveChild, 0,
/*353*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::OR),
/*356*/       OPC_RecordChild0, // #0 = $b
/*357*/       OPC_RecordChild1, // #1 = $c
/*358*/       OPC_MoveParent,
/*359*/       OPC_MoveChild, 1,
/*361*/       OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*372*/       OPC_MoveParent,
/*373*/       OPC_MorphNodeTo, TARGET_OPCODE(Mips::NOR), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (xor:i32 (or:i32 CPURegs:i32:$b, CPURegs:i32:$c), -1:i32) - Complexity = 11
              // Dst: (NOR:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*382*/     /*Scope*/ 67, /*->450*/
/*383*/       OPC_RecordChild0, // #0 = $in
/*384*/       OPC_Scope, 26, /*->412*/ // 2 children in Scope
/*386*/         OPC_MoveChild, 1,
/*388*/         OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*399*/         OPC_MoveParent,
/*400*/         OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*403*/         OPC_MorphNodeTo, TARGET_OPCODE(Mips::NOR), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (xor:i32 CPURegs:i32:$in, -1:i32) - Complexity = 8
                // Dst: (NOR:i32 CPURegs:i32:$in, ZERO:i32)
/*412*/       /*Scope*/ 36, /*->449*/
/*413*/         OPC_RecordChild1, // #1 = $c
/*414*/         OPC_Scope, 22, /*->438*/ // 2 children in Scope
/*416*/           OPC_MoveChild, 1,
/*418*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*421*/           OPC_CheckPredicate, 17, // Predicate_immZExt16
/*423*/           OPC_MoveParent,
/*424*/           OPC_EmitConvertToTarget, 1,
/*426*/           OPC_EmitNodeXForm, 0, 2, // LO16
/*429*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::XORi), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (xor:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immZExt16>><<X:LO16>>:$c) - Complexity = 7
                  // Dst: (XORi:i32 CPURegs:i32:$b, (LO16:i32 (imm:i32):$c))
/*438*/         /*Scope*/ 9, /*->448*/
/*439*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::XOR), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (xor:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
                  // Dst: (XOR:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*448*/         0, /*End of Scope*/
/*449*/       0, /*End of Scope*/
/*450*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 71|128,3/*455*/,  TARGET_OPCODE(ISD::BRCOND),// ->910
/*455*/     OPC_RecordNode,   // #0 = 'brcond' chained node
/*456*/     OPC_Scope, 44|128,3/*428*/, /*->887*/ // 2 children in Scope
/*459*/       OPC_MoveChild, 1,
/*461*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::SETCC),
/*464*/       OPC_RecordChild0, // #1 = $src
/*465*/       OPC_Scope, 15|128,1/*143*/, /*->611*/ // 3 children in Scope
/*468*/         OPC_MoveChild, 1,
/*470*/         OPC_CheckInteger, 0, 
/*472*/         OPC_MoveParent,
/*473*/         OPC_MoveChild, 2,
/*475*/         OPC_Scope, 20, /*->497*/ // 6 children in Scope
/*477*/           OPC_CheckCondCode, ISD::SETGE,
/*479*/           OPC_MoveParent,
/*480*/           OPC_MoveParent,
/*481*/           OPC_RecordChild2, // #2 = $offset
/*482*/           OPC_MoveChild, 2,
/*484*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*487*/           OPC_MoveParent,
/*488*/           OPC_EmitMergeInputChains1_0,
/*489*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BGEZ), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 1, 2, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$src, 0:i32, SETGE:Other), (bb:Other):$offset) - Complexity = 11
                  // Dst: (BGEZ CPURegs:i32:$src, (bb:Other):$offset)
/*497*/         /*Scope*/ 20, /*->518*/
/*498*/           OPC_CheckCondCode, ISD::SETGT,
/*500*/           OPC_MoveParent,
/*501*/           OPC_MoveParent,
/*502*/           OPC_RecordChild2, // #2 = $offset
/*503*/           OPC_MoveChild, 2,
/*505*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*508*/           OPC_MoveParent,
/*509*/           OPC_EmitMergeInputChains1_0,
/*510*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BGTZ), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 1, 2, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$src, 0:i32, SETGT:Other), (bb:Other):$offset) - Complexity = 11
                  // Dst: (BGTZ CPURegs:i32:$src, (bb:Other):$offset)
/*518*/         /*Scope*/ 20, /*->539*/
/*519*/           OPC_CheckCondCode, ISD::SETLE,
/*521*/           OPC_MoveParent,
/*522*/           OPC_MoveParent,
/*523*/           OPC_RecordChild2, // #2 = $offset
/*524*/           OPC_MoveChild, 2,
/*526*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*529*/           OPC_MoveParent,
/*530*/           OPC_EmitMergeInputChains1_0,
/*531*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BLEZ), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 1, 2, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$src, 0:i32, SETLE:Other), (bb:Other):$offset) - Complexity = 11
                  // Dst: (BLEZ CPURegs:i32:$src, (bb:Other):$offset)
/*539*/         /*Scope*/ 20, /*->560*/
/*540*/           OPC_CheckCondCode, ISD::SETLT,
/*542*/           OPC_MoveParent,
/*543*/           OPC_MoveParent,
/*544*/           OPC_RecordChild2, // #2 = $offset
/*545*/           OPC_MoveChild, 2,
/*547*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*550*/           OPC_MoveParent,
/*551*/           OPC_EmitMergeInputChains1_0,
/*552*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BLTZ), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 1, 2, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$src, 0:i32, SETLT:Other), (bb:Other):$offset) - Complexity = 11
                  // Dst: (BLTZ CPURegs:i32:$src, (bb:Other):$offset)
/*560*/         /*Scope*/ 24, /*->585*/
/*561*/           OPC_CheckCondCode, ISD::SETNE,
/*563*/           OPC_MoveParent,
/*564*/           OPC_MoveParent,
/*565*/           OPC_RecordChild2, // #2 = $dst
/*566*/           OPC_MoveChild, 2,
/*568*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*571*/           OPC_MoveParent,
/*572*/           OPC_EmitMergeInputChains1_0,
/*573*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*576*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BNE), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 2, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, 0:i32, SETNE:Other), (bb:Other):$dst) - Complexity = 11
                  // Dst: (BNE CPURegs:i32:$lhs, ZERO:i32, (bb:Other):$dst)
/*585*/         /*Scope*/ 24, /*->610*/
/*586*/           OPC_CheckCondCode, ISD::SETEQ,
/*588*/           OPC_MoveParent,
/*589*/           OPC_MoveParent,
/*590*/           OPC_RecordChild2, // #2 = $dst
/*591*/           OPC_MoveChild, 2,
/*593*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*596*/           OPC_MoveParent,
/*597*/           OPC_EmitMergeInputChains1_0,
/*598*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*601*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 2, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, 0:i32, SETEQ:Other), (bb:Other):$dst) - Complexity = 11
                  // Dst: (BEQ CPURegs:i32:$lhs, ZERO:i32, (bb:Other):$dst)
/*610*/         0, /*End of Scope*/
/*611*/       /*Scope*/ 85, /*->697*/
/*612*/         OPC_RecordChild1, // #2 = $rhs
/*613*/         OPC_MoveChild, 1,
/*615*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*618*/         OPC_CheckPredicate, 18, // Predicate_immSExt16
/*620*/         OPC_MoveParent,
/*621*/         OPC_MoveChild, 2,
/*623*/         OPC_Scope, 35, /*->660*/ // 2 children in Scope
/*625*/           OPC_CheckCondCode, ISD::SETGE,
/*627*/           OPC_MoveParent,
/*628*/           OPC_MoveParent,
/*629*/           OPC_RecordChild2, // #3 = $dst
/*630*/           OPC_MoveChild, 2,
/*632*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*635*/           OPC_MoveParent,
/*636*/           OPC_EmitMergeInputChains1_0,
/*637*/           OPC_EmitConvertToTarget, 2,
/*639*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLTi), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 4,  // Results = #5 
/*648*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*651*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs, SETGE:Other), (bb:Other):$dst) - Complexity = 10
                  // Dst: (BEQ (SLTi:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs), ZERO:i32, (bb:Other):$dst)
/*660*/         /*Scope*/ 35, /*->696*/
/*661*/           OPC_CheckCondCode, ISD::SETUGE,
/*663*/           OPC_MoveParent,
/*664*/           OPC_MoveParent,
/*665*/           OPC_RecordChild2, // #3 = $dst
/*666*/           OPC_MoveChild, 2,
/*668*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*671*/           OPC_MoveParent,
/*672*/           OPC_EmitMergeInputChains1_0,
/*673*/           OPC_EmitConvertToTarget, 2,
/*675*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLTiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 4,  // Results = #5 
/*684*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*687*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs, SETUGE:Other), (bb:Other):$dst) - Complexity = 10
                  // Dst: (BEQ (SLTiu:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs), ZERO:i32, (bb:Other):$dst)
/*696*/         0, /*End of Scope*/
/*697*/       /*Scope*/ 59|128,1/*187*/, /*->886*/
/*699*/         OPC_CheckChild0Type, MVT::i32,
/*701*/         OPC_RecordChild1, // #2 = $b
/*702*/         OPC_MoveChild, 2,
/*704*/         OPC_Scope, 21, /*->727*/ // 6 children in Scope
/*706*/           OPC_CheckCondCode, ISD::SETEQ,
/*708*/           OPC_MoveParent,
/*709*/           OPC_MoveParent,
/*710*/           OPC_RecordChild2, // #3 = $offset
/*711*/           OPC_MoveChild, 2,
/*713*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*716*/           OPC_MoveParent,
/*717*/           OPC_EmitMergeInputChains1_0,
/*718*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 1, 2, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$a, CPURegs:i32:$b, SETEQ:Other), (bb:Other):$offset) - Complexity = 6
                  // Dst: (BEQ CPURegs:i32:$a, CPURegs:i32:$b, (bb:Other):$offset)
/*727*/         /*Scope*/ 21, /*->749*/
/*728*/           OPC_CheckCondCode, ISD::SETNE,
/*730*/           OPC_MoveParent,
/*731*/           OPC_MoveParent,
/*732*/           OPC_RecordChild2, // #3 = $offset
/*733*/           OPC_MoveChild, 2,
/*735*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*738*/           OPC_MoveParent,
/*739*/           OPC_EmitMergeInputChains1_0,
/*740*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BNE), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 1, 2, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$a, CPURegs:i32:$b, SETNE:Other), (bb:Other):$offset) - Complexity = 6
                  // Dst: (BNE CPURegs:i32:$a, CPURegs:i32:$b, (bb:Other):$offset)
/*749*/         /*Scope*/ 33, /*->783*/
/*750*/           OPC_CheckCondCode, ISD::SETGE,
/*752*/           OPC_MoveParent,
/*753*/           OPC_MoveParent,
/*754*/           OPC_RecordChild2, // #3 = $dst
/*755*/           OPC_MoveChild, 2,
/*757*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*760*/           OPC_MoveParent,
/*761*/           OPC_EmitMergeInputChains1_0,
/*762*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLT), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2,  // Results = #4 
/*771*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*774*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 4, 5, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETGE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BEQ (SLT:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs), ZERO:i32, (bb:Other):$dst)
/*783*/         /*Scope*/ 33, /*->817*/
/*784*/           OPC_CheckCondCode, ISD::SETUGE,
/*786*/           OPC_MoveParent,
/*787*/           OPC_MoveParent,
/*788*/           OPC_RecordChild2, // #3 = $dst
/*789*/           OPC_MoveChild, 2,
/*791*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*794*/           OPC_MoveParent,
/*795*/           OPC_EmitMergeInputChains1_0,
/*796*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLTu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2,  // Results = #4 
/*805*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*808*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 4, 5, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETUGE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BEQ (SLTu:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs), ZERO:i32, (bb:Other):$dst)
/*817*/         /*Scope*/ 33, /*->851*/
/*818*/           OPC_CheckCondCode, ISD::SETLE,
/*820*/           OPC_MoveParent,
/*821*/           OPC_MoveParent,
/*822*/           OPC_RecordChild2, // #3 = $dst
/*823*/           OPC_MoveChild, 2,
/*825*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*828*/           OPC_MoveParent,
/*829*/           OPC_EmitMergeInputChains1_0,
/*830*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLT), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 1,  // Results = #4 
/*839*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*842*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 4, 5, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETLE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BEQ (SLT:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs), ZERO:i32, (bb:Other):$dst)
/*851*/         /*Scope*/ 33, /*->885*/
/*852*/           OPC_CheckCondCode, ISD::SETULE,
/*854*/           OPC_MoveParent,
/*855*/           OPC_MoveParent,
/*856*/           OPC_RecordChild2, // #3 = $dst
/*857*/           OPC_MoveChild, 2,
/*859*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*862*/           OPC_MoveParent,
/*863*/           OPC_EmitMergeInputChains1_0,
/*864*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLTu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 1,  // Results = #4 
/*873*/           OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*876*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::BEQ), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 4, 5, 3, 
                  // Src: (brcond (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETULE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BEQ (SLTu:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs), ZERO:i32, (bb:Other):$dst)
/*885*/         0, /*End of Scope*/
/*886*/       0, /*End of Scope*/
/*887*/     /*Scope*/ 21, /*->909*/
/*888*/       OPC_RecordChild1, // #1 = $cond
/*889*/       OPC_RecordChild2, // #2 = $dst
/*890*/       OPC_MoveChild, 2,
/*892*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*895*/       OPC_MoveParent,
/*896*/       OPC_EmitMergeInputChains1_0,
/*897*/       OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*900*/       OPC_MorphNodeTo, TARGET_OPCODE(Mips::BNE), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 1, 3, 2, 
              // Src: (brcond CPURegs:i32:$cond, (bb:Other):$dst) - Complexity = 3
              // Dst: (BNE CPURegs:i32:$cond, ZERO:i32, (bb:Other):$dst)
/*909*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 23|128,2/*279*/,  TARGET_OPCODE(ISD::SELECT),// ->1193
/*914*/     OPC_Scope, 3|128,2/*259*/, /*->1176*/ // 2 children in Scope
/*917*/       OPC_MoveChild, 0,
/*919*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::SETCC),
/*922*/       OPC_RecordChild0, // #0 = $lhs
/*923*/       OPC_Scope, 73, /*->998*/ // 2 children in Scope
/*925*/         OPC_RecordChild1, // #1 = $rhs
/*926*/         OPC_MoveChild, 1,
/*928*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*931*/         OPC_CheckPredicate, 18, // Predicate_immSExt16
/*933*/         OPC_MoveParent,
/*934*/         OPC_MoveChild, 2,
/*936*/         OPC_Scope, 29, /*->967*/ // 2 children in Scope
/*938*/           OPC_CheckCondCode, ISD::SETGE,
/*940*/           OPC_MoveParent,
/*941*/           OPC_MoveParent,
/*942*/           OPC_RecordChild1, // #2 = $T
/*943*/           OPC_RecordChild2, // #3 = $F
/*944*/           OPC_CheckType, MVT::i32,
/*946*/           OPC_EmitConvertToTarget, 1,
/*948*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLTi), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 4,  // Results = #5 
/*957*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 5, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs, SETGE:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 10
                  // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, (SLTi:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs))
/*967*/         /*Scope*/ 29, /*->997*/
/*968*/           OPC_CheckCondCode, ISD::SETUGE,
/*970*/           OPC_MoveParent,
/*971*/           OPC_MoveParent,
/*972*/           OPC_RecordChild1, // #2 = $T
/*973*/           OPC_RecordChild2, // #3 = $F
/*974*/           OPC_CheckType, MVT::i32,
/*976*/           OPC_EmitConvertToTarget, 1,
/*978*/           OPC_EmitNode, TARGET_OPCODE(Mips::SLTiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 4,  // Results = #5 
/*987*/           OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 5, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lh, (imm:i32)<<P:Predicate_immSExt16>>:$rh, SETUGE:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 10
                  // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, (SLTiu:i32 CPURegs:i32:$lh, (imm:i32)<<P:Predicate_immSExt16>>:$rh))
/*997*/         0, /*End of Scope*/
/*998*/       /*Scope*/ 47|128,1/*175*/, /*->1175*/
/*1000*/        OPC_CheckChild0Type, MVT::i32,
/*1002*/        OPC_RecordChild1, // #1 = $rhs
/*1003*/        OPC_MoveChild, 2,
/*1005*/        OPC_Scope, 27, /*->1034*/ // 6 children in Scope
/*1007*/          OPC_CheckCondCode, ISD::SETGE,
/*1009*/          OPC_MoveParent,
/*1010*/          OPC_MoveParent,
/*1011*/          OPC_RecordChild1, // #2 = $T
/*1012*/          OPC_RecordChild2, // #3 = $F
/*1013*/          OPC_CheckType, MVT::i32,
/*1015*/          OPC_EmitNode, TARGET_OPCODE(Mips::SLT), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #4 
/*1024*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 4, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETGE:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 6
                  // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, (SLT:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs))
/*1034*/        /*Scope*/ 27, /*->1062*/
/*1035*/          OPC_CheckCondCode, ISD::SETUGE,
/*1037*/          OPC_MoveParent,
/*1038*/          OPC_MoveParent,
/*1039*/          OPC_RecordChild1, // #2 = $T
/*1040*/          OPC_RecordChild2, // #3 = $F
/*1041*/          OPC_CheckType, MVT::i32,
/*1043*/          OPC_EmitNode, TARGET_OPCODE(Mips::SLTu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #4 
/*1052*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 4, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETUGE:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 6
                  // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, (SLTu:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs))
/*1062*/        /*Scope*/ 27, /*->1090*/
/*1063*/          OPC_CheckCondCode, ISD::SETLE,
/*1065*/          OPC_MoveParent,
/*1066*/          OPC_MoveParent,
/*1067*/          OPC_RecordChild1, // #2 = $T
/*1068*/          OPC_RecordChild2, // #3 = $F
/*1069*/          OPC_CheckType, MVT::i32,
/*1071*/          OPC_EmitNode, TARGET_OPCODE(Mips::SLT), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #4 
/*1080*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 4, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETLE:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 6
                  // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, (SLT:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs))
/*1090*/        /*Scope*/ 27, /*->1118*/
/*1091*/          OPC_CheckCondCode, ISD::SETULE,
/*1093*/          OPC_MoveParent,
/*1094*/          OPC_MoveParent,
/*1095*/          OPC_RecordChild1, // #2 = $T
/*1096*/          OPC_RecordChild2, // #3 = $F
/*1097*/          OPC_CheckType, MVT::i32,
/*1099*/          OPC_EmitNode, TARGET_OPCODE(Mips::SLTu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #4 
/*1108*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 4, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETULE:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 6
                  // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, (SLTu:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs))
/*1118*/        /*Scope*/ 27, /*->1146*/
/*1119*/          OPC_CheckCondCode, ISD::SETEQ,
/*1121*/          OPC_MoveParent,
/*1122*/          OPC_MoveParent,
/*1123*/          OPC_RecordChild1, // #2 = $T
/*1124*/          OPC_RecordChild2, // #3 = $F
/*1125*/          OPC_CheckType, MVT::i32,
/*1127*/          OPC_EmitNode, TARGET_OPCODE(Mips::XOR), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #4 
/*1136*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 4, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETEQ:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 6
                  // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, (XOR:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs))
/*1146*/        /*Scope*/ 27, /*->1174*/
/*1147*/          OPC_CheckCondCode, ISD::SETNE,
/*1149*/          OPC_MoveParent,
/*1150*/          OPC_MoveParent,
/*1151*/          OPC_RecordChild1, // #2 = $T
/*1152*/          OPC_RecordChild2, // #3 = $F
/*1153*/          OPC_CheckType, MVT::i32,
/*1155*/          OPC_EmitNode, TARGET_OPCODE(Mips::XOR), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #4 
/*1164*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVN), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 3, 2, 4, 
                  // Src: (select:i32 (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETNE:Other), CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 6
                  // Dst: (MOVN:i32 CPURegs:i32:$F, CPURegs:i32:$T, (XOR:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs))
/*1174*/        0, /*End of Scope*/
/*1175*/      0, /*End of Scope*/
/*1176*/    /*Scope*/ 15, /*->1192*/
/*1177*/      OPC_RecordChild0, // #0 = $cond
/*1178*/      OPC_RecordChild1, // #1 = $T
/*1179*/      OPC_RecordChild2, // #2 = $F
/*1180*/      OPC_CheckType, MVT::i32,
/*1182*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVN), 0,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 2, 1, 0, 
              // Src: (select:i32 CPURegs:i32:$cond, CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 3
              // Dst: (MOVN:i32 CPURegs:i32:$F, CPURegs:i32:$T, CPURegs:i32:$cond)
/*1192*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::CALLSEQ_END),// ->1222
/*1196*/    OPC_RecordNode,   // #0 = 'callseq_end' chained node
/*1197*/    OPC_CaptureFlagInput,
/*1198*/    OPC_RecordChild1, // #1 = $amt1
/*1199*/    OPC_MoveChild, 1,
/*1201*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*1204*/    OPC_MoveParent,
/*1205*/    OPC_RecordChild2, // #2 = $amt2
/*1206*/    OPC_MoveChild, 2,
/*1208*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*1211*/    OPC_MoveParent,
/*1212*/    OPC_EmitMergeInputChains1_0,
/*1213*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADJCALLSTACKUP), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
            // Src: (callseq_end (timm:i32):$amt1, (timm:i32):$amt2) - Complexity = 9
            // Dst: (ADJCALLSTACKUP:i32 (timm:i32):$amt1, (timm:i32):$amt2)
          /*SwitchOpcode*/ 15,  TARGET_OPCODE(ISD::FrameIndex),// ->1240
/*1225*/    OPC_RecordNode,   // #0 = $addr
/*1226*/    OPC_CheckType, MVT::i32,
/*1228*/    OPC_CheckComplexPat, /*CP*/0, /*#*/0, // SelectAddr:$addr #1 #2
/*1231*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::LEA_ADDiu), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
            // Src: addr:i32:$addr - Complexity = 9
            // Dst: (LEA_ADDiu:i32 addr:i32:$addr)
          /*SwitchOpcode*/ 97|128,1/*225*/,  TARGET_OPCODE(ISD::ADD),// ->1469
/*1244*/    OPC_Scope, 91, /*->1337*/ // 3 children in Scope
/*1246*/      OPC_RecordChild0, // #0 = $hi
/*1247*/      OPC_MoveChild, 1,
/*1249*/      OPC_SwitchOpcode /*2 cases */, 47,  TARGET_OPCODE(MipsISD::Lo),// ->1300
/*1253*/        OPC_RecordChild0, // #1 = $lo
/*1254*/        OPC_MoveChild, 0,
/*1256*/        OPC_SwitchOpcode /*3 cases */, 11,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->1271
/*1260*/          OPC_MoveParent,
/*1261*/          OPC_MoveParent,
/*1262*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i32 CPURegs:i32:$hi, (MipsLo:i32 (tglobaladdr:i32):$lo)) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$hi, (tglobaladdr:i32):$lo)
                /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::TargetJumpTable),// ->1285
/*1274*/          OPC_MoveParent,
/*1275*/          OPC_MoveParent,
/*1276*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i32 CPURegs:i32:$hi, (MipsLo:i32 (tjumptable:i32):$lo)) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$hi, (tjumptable:i32):$lo)
                /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::TargetConstantPool),// ->1299
/*1288*/          OPC_MoveParent,
/*1289*/          OPC_MoveParent,
/*1290*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i32 CPURegs:i32:$hi, (MipsLo:i32 (tconstpool:i32):$lo)) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$hi, (tconstpool:i32):$lo)
                0, // EndSwitchOpcode
              /*SwitchOpcode*/ 33,  TARGET_OPCODE(MipsISD::GPRel),// ->1336
/*1303*/        OPC_RecordChild0, // #1 = $in
/*1304*/        OPC_MoveChild, 0,
/*1306*/        OPC_SwitchOpcode /*2 cases */, 11,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->1321
/*1310*/          OPC_MoveParent,
/*1311*/          OPC_MoveParent,
/*1312*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i32 CPURegs:i32:$gp, (MipsGPRel:i32 (tglobaladdr:i32):$in)) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$gp, (tglobaladdr:i32):$in)
                /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::TargetConstantPool),// ->1335
/*1324*/          OPC_MoveParent,
/*1325*/          OPC_MoveParent,
/*1326*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i32 CPURegs:i32:$gp, (MipsGPRel:i32 (tconstpool:i32):$in)) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$gp, (tconstpool:i32):$in)
                0, // EndSwitchOpcode
              0, // EndSwitchOpcode
/*1337*/    /*Scope*/ 95, /*->1433*/
/*1338*/      OPC_MoveChild, 0,
/*1340*/      OPC_SwitchOpcode /*2 cases */, 50,  TARGET_OPCODE(MipsISD::Lo),// ->1394
/*1344*/        OPC_RecordChild0, // #0 = $lo
/*1345*/        OPC_MoveChild, 0,
/*1347*/        OPC_SwitchOpcode /*3 cases */, 12,  TARGET_OPCODE(ISD::TargetJumpTable),// ->1363
/*1351*/          OPC_MoveParent,
/*1352*/          OPC_MoveParent,
/*1353*/          OPC_RecordChild1, // #1 = $hi
/*1354*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                  // Src: (add:i32 (MipsLo:i32 (tjumptable:i32):$lo), CPURegs:i32:$hi) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$hi, (tjumptable:i32):$lo)
                /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetConstantPool),// ->1378
/*1366*/          OPC_MoveParent,
/*1367*/          OPC_MoveParent,
/*1368*/          OPC_RecordChild1, // #1 = $hi
/*1369*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                  // Src: (add:i32 (MipsLo:i32 (tconstpool:i32):$lo), CPURegs:i32:$hi) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$hi, (tconstpool:i32):$lo)
                /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->1393
/*1381*/          OPC_MoveParent,
/*1382*/          OPC_MoveParent,
/*1383*/          OPC_RecordChild1, // #1 = $hi
/*1384*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                  // Src: (add:i32 (MipsLo:i32 (tglobaladdr:i32):$lo), CPURegs:i32:$hi) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$hi, (tglobaladdr:i32):$lo)
                0, // EndSwitchOpcode
              /*SwitchOpcode*/ 35,  TARGET_OPCODE(MipsISD::GPRel),// ->1432
/*1397*/        OPC_RecordChild0, // #0 = $in
/*1398*/        OPC_MoveChild, 0,
/*1400*/        OPC_SwitchOpcode /*2 cases */, 12,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->1416
/*1404*/          OPC_MoveParent,
/*1405*/          OPC_MoveParent,
/*1406*/          OPC_RecordChild1, // #1 = $gp
/*1407*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                  // Src: (add:i32 (MipsGPRel:i32 (tglobaladdr:i32):$in), CPURegs:i32:$gp) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$gp, (tglobaladdr:i32):$in)
                /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetConstantPool),// ->1431
/*1419*/          OPC_MoveParent,
/*1420*/          OPC_MoveParent,
/*1421*/          OPC_RecordChild1, // #1 = $gp
/*1422*/          OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                  // Src: (add:i32 (MipsGPRel:i32 (tconstpool:i32):$in), CPURegs:i32:$gp) - Complexity = 9
                  // Dst: (ADDiu:i32 CPURegs:i32:$gp, (tconstpool:i32):$in)
                0, // EndSwitchOpcode
              0, // EndSwitchOpcode
/*1433*/    /*Scope*/ 34, /*->1468*/
/*1434*/      OPC_RecordChild0, // #0 = $b
/*1435*/      OPC_RecordChild1, // #1 = $c
/*1436*/      OPC_Scope, 19, /*->1457*/ // 2 children in Scope
/*1438*/        OPC_MoveChild, 1,
/*1440*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1443*/        OPC_CheckPredicate, 18, // Predicate_immSExt16
/*1445*/        OPC_MoveParent,
/*1446*/        OPC_EmitConvertToTarget, 1,
/*1448*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (add:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immSExt16>>:$c) - Complexity = 7
                // Dst: (ADDiu:i32 CPURegs:i32:$b, (imm:i32):$c)
/*1457*/      /*Scope*/ 9, /*->1467*/
/*1458*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (add:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
                // Dst: (ADDu:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*1467*/      0, /*End of Scope*/
/*1468*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 43,  TARGET_OPCODE(MipsISD::CMov),// ->1515
/*1472*/    OPC_RecordChild0, // #0 = $F
/*1473*/    OPC_RecordChild1, // #1 = $T
/*1474*/    OPC_RecordChild2, // #2 = $cond
/*1475*/    OPC_MoveChild, 3,
/*1477*/    OPC_Scope, 17, /*->1496*/ // 2 children in Scope
/*1479*/      OPC_CheckInteger, 1, 
/*1481*/      OPC_MoveParent,
/*1482*/      OPC_CheckType, MVT::i32,
/*1484*/      OPC_CheckPatternPredicate, 1, // (Subtarget.hasCondMov())
/*1486*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVN), 0,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (MipsCMov:i32 CPURegs:i32:$F, CPURegs:i32:$T, CPURegs:i32:$cond, 1:i32) - Complexity = 8
              // Dst: (MOVN:i32 CPURegs:i32:$F, CPURegs:i32:$T, CPURegs:i32:$cond)
/*1496*/    /*Scope*/ 17, /*->1514*/
/*1497*/      OPC_CheckInteger, 0, 
/*1499*/      OPC_MoveParent,
/*1500*/      OPC_CheckType, MVT::i32,
/*1502*/      OPC_CheckPatternPredicate, 1, // (Subtarget.hasCondMov())
/*1504*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::MOVZ), 0,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (MipsCMov:i32 CPURegs:i32:$F, CPURegs:i32:$T, CPURegs:i32:$cond, 0:i32) - Complexity = 8
              // Dst: (MOVZ:i32 CPURegs:i32:$F, CPURegs:i32:$T, CPURegs:i32:$cond)
/*1514*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 97,  TARGET_OPCODE(MipsISD::FPBrcond),// ->1615
/*1518*/    OPC_RecordNode,   // #0 = 'MipsFPBrcond' chained node
/*1519*/    OPC_MoveChild, 1,
/*1521*/    OPC_Scope, 22, /*->1545*/ // 4 children in Scope
/*1523*/      OPC_CheckInteger, 0, 
/*1525*/      OPC_MoveParent,
/*1526*/      OPC_RecordChild2, // #1 = $dst
/*1527*/      OPC_MoveChild, 2,
/*1529*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1532*/      OPC_MoveParent,
/*1533*/      OPC_RecordChild3, // #2 = physreg input FCR31
/*1534*/      OPC_EmitMergeInputChains1_0,
/*1535*/      OPC_EmitCopyToReg, 2, Mips::FCR31,
/*1538*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::BC1F), 0|OPFL_Chain|OPFL_FlagInput,
                  0/*#VTs*/, 1/*#Ops*/, 1, 
              // Src: (MipsFPBrcond 0:i32, (bb:Other):$dst, FCR31:i32) - Complexity = 8
              // Dst: (BC1F (bb:Other):$dst)
/*1545*/    /*Scope*/ 22, /*->1568*/
/*1546*/      OPC_CheckInteger, 1, 
/*1548*/      OPC_MoveParent,
/*1549*/      OPC_RecordChild2, // #1 = $dst
/*1550*/      OPC_MoveChild, 2,
/*1552*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1555*/      OPC_MoveParent,
/*1556*/      OPC_RecordChild3, // #2 = physreg input FCR31
/*1557*/      OPC_EmitMergeInputChains1_0,
/*1558*/      OPC_EmitCopyToReg, 2, Mips::FCR31,
/*1561*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::BC1T), 0|OPFL_Chain|OPFL_FlagInput,
                  0/*#VTs*/, 1/*#Ops*/, 1, 
              // Src: (MipsFPBrcond 1:i32, (bb:Other):$dst, FCR31:i32) - Complexity = 8
              // Dst: (BC1T (bb:Other):$dst)
/*1568*/    /*Scope*/ 22, /*->1591*/
/*1569*/      OPC_CheckInteger, 2, 
/*1571*/      OPC_MoveParent,
/*1572*/      OPC_RecordChild2, // #1 = $dst
/*1573*/      OPC_MoveChild, 2,
/*1575*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1578*/      OPC_MoveParent,
/*1579*/      OPC_RecordChild3, // #2 = physreg input FCR31
/*1580*/      OPC_EmitMergeInputChains1_0,
/*1581*/      OPC_EmitCopyToReg, 2, Mips::FCR31,
/*1584*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::BC1FL), 0|OPFL_Chain|OPFL_FlagInput,
                  0/*#VTs*/, 1/*#Ops*/, 1, 
              // Src: (MipsFPBrcond 2:i32, (bb:Other):$dst, FCR31:i32) - Complexity = 8
              // Dst: (BC1FL (bb:Other):$dst)
/*1591*/    /*Scope*/ 22, /*->1614*/
/*1592*/      OPC_CheckInteger, 3, 
/*1594*/      OPC_MoveParent,
/*1595*/      OPC_RecordChild2, // #1 = $dst
/*1596*/      OPC_MoveChild, 2,
/*1598*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1601*/      OPC_MoveParent,
/*1602*/      OPC_RecordChild3, // #2 = physreg input FCR31
/*1603*/      OPC_EmitMergeInputChains1_0,
/*1604*/      OPC_EmitCopyToReg, 2, Mips::FCR31,
/*1607*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::BC1TL), 0|OPFL_Chain|OPFL_FlagInput,
                  0/*#VTs*/, 1/*#Ops*/, 1, 
              // Src: (MipsFPBrcond 3:i32, (bb:Other):$dst, FCR31:i32) - Complexity = 8
              // Dst: (BC1TL (bb:Other):$dst)
/*1614*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 56|128,2/*312*/,  TARGET_OPCODE(ISD::SETCC),// ->1931
/*1619*/    OPC_RecordChild0, // #0 = $b
/*1620*/    OPC_Scope, 97, /*->1719*/ // 2 children in Scope
/*1622*/      OPC_RecordChild1, // #1 = $c
/*1623*/      OPC_MoveChild, 1,
/*1625*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1628*/      OPC_CheckPredicate, 18, // Predicate_immSExt16
/*1630*/      OPC_MoveParent,
/*1631*/      OPC_MoveChild, 2,
/*1633*/      OPC_Scope, 14, /*->1649*/ // 4 children in Scope
/*1635*/        OPC_CheckCondCode, ISD::SETLT,
/*1637*/        OPC_MoveParent,
/*1638*/        OPC_EmitConvertToTarget, 1,
/*1640*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLTi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (setcc:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immSExt16>>:$c, SETLT:Other) - Complexity = 7
                // Dst: (SLTi:i32 CPURegs:i32:$b, (imm:i32):$c)
/*1649*/      /*Scope*/ 14, /*->1664*/
/*1650*/        OPC_CheckCondCode, ISD::SETULT,
/*1652*/        OPC_MoveParent,
/*1653*/        OPC_EmitConvertToTarget, 1,
/*1655*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLTiu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (setcc:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immSExt16>>:$c, SETULT:Other) - Complexity = 7
                // Dst: (SLTiu:i32 CPURegs:i32:$b, (imm:i32):$c)
/*1664*/      /*Scope*/ 26, /*->1691*/
/*1665*/        OPC_CheckCondCode, ISD::SETGE,
/*1667*/        OPC_MoveParent,
/*1668*/        OPC_EmitConvertToTarget, 1,
/*1670*/        OPC_EmitNode, TARGET_OPCODE(Mips::SLTi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2,  // Results = #3 
/*1679*/        OPC_EmitInteger, MVT::i32, 1, 
/*1682*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::XORi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 3, 4, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs, SETGE:Other) - Complexity = 7
                // Dst: (XORi:i32 (SLTi:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs), 1:i32)
/*1691*/      /*Scope*/ 26, /*->1718*/
/*1692*/        OPC_CheckCondCode, ISD::SETUGE,
/*1694*/        OPC_MoveParent,
/*1695*/        OPC_EmitConvertToTarget, 1,
/*1697*/        OPC_EmitNode, TARGET_OPCODE(Mips::SLTiu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2,  // Results = #3 
/*1706*/        OPC_EmitInteger, MVT::i32, 1, 
/*1709*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::XORi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 3, 4, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs, SETUGE:Other) - Complexity = 7
                // Dst: (XORi:i32 (SLTiu:i32 CPURegs:i32:$lhs, (imm:i32)<<P:Predicate_immSExt16>>:$rhs), 1:i32)
/*1718*/      0, /*End of Scope*/
/*1719*/    /*Scope*/ 81|128,1/*209*/, /*->1930*/
/*1721*/      OPC_CheckChild0Type, MVT::i32,
/*1723*/      OPC_RecordChild1, // #1 = $c
/*1724*/      OPC_MoveChild, 2,
/*1726*/      OPC_Scope, 12, /*->1740*/ // 10 children in Scope
/*1728*/        OPC_CheckCondCode, ISD::SETLT,
/*1730*/        OPC_MoveParent,
/*1731*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLT), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (setcc:i32 CPURegs:i32:$b, CPURegs:i32:$c, SETLT:Other) - Complexity = 3
                // Dst: (SLT:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*1740*/      /*Scope*/ 12, /*->1753*/
/*1741*/        OPC_CheckCondCode, ISD::SETULT,
/*1743*/        OPC_MoveParent,
/*1744*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLTu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (setcc:i32 CPURegs:i32:$b, CPURegs:i32:$c, SETULT:Other) - Complexity = 3
                // Dst: (SLTu:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*1753*/      /*Scope*/ 12, /*->1766*/
/*1754*/        OPC_CheckCondCode, ISD::SETGT,
/*1756*/        OPC_MoveParent,
/*1757*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLT), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETGT:Other) - Complexity = 3
                // Dst: (SLT:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs)
/*1766*/      /*Scope*/ 12, /*->1779*/
/*1767*/        OPC_CheckCondCode, ISD::SETUGT,
/*1769*/        OPC_MoveParent,
/*1770*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLTu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETUGT:Other) - Complexity = 3
                // Dst: (SLTu:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs)
/*1779*/      /*Scope*/ 24, /*->1804*/
/*1780*/        OPC_CheckCondCode, ISD::SETEQ,
/*1782*/        OPC_MoveParent,
/*1783*/        OPC_EmitNode, TARGET_OPCODE(Mips::XOR), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #2 
/*1792*/        OPC_EmitInteger, MVT::i32, 1, 
/*1795*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLTu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETEQ:Other) - Complexity = 3
                // Dst: (SLTu:i32 (XOR:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs), 1:i32)
/*1804*/      /*Scope*/ 24, /*->1829*/
/*1805*/        OPC_CheckCondCode, ISD::SETNE,
/*1807*/        OPC_MoveParent,
/*1808*/        OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*1811*/        OPC_EmitNode, TARGET_OPCODE(Mips::XOR), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #3 
/*1820*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLTu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETNE:Other) - Complexity = 3
                // Dst: (SLTu:i32 ZERO:i32, (XOR:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs))
/*1829*/      /*Scope*/ 24, /*->1854*/
/*1830*/        OPC_CheckCondCode, ISD::SETLE,
/*1832*/        OPC_MoveParent,
/*1833*/        OPC_EmitNode, TARGET_OPCODE(Mips::SLT), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #2 
/*1842*/        OPC_EmitInteger, MVT::i32, 1, 
/*1845*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::XORi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETLE:Other) - Complexity = 3
                // Dst: (XORi:i32 (SLT:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs), 1:i32)
/*1854*/      /*Scope*/ 24, /*->1879*/
/*1855*/        OPC_CheckCondCode, ISD::SETULE,
/*1857*/        OPC_MoveParent,
/*1858*/        OPC_EmitNode, TARGET_OPCODE(Mips::SLTu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #2 
/*1867*/        OPC_EmitInteger, MVT::i32, 1, 
/*1870*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::XORi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETULE:Other) - Complexity = 3
                // Dst: (XORi:i32 (SLTu:i32 CPURegs:i32:$rhs, CPURegs:i32:$lhs), 1:i32)
/*1879*/      /*Scope*/ 24, /*->1904*/
/*1880*/        OPC_CheckCondCode, ISD::SETGE,
/*1882*/        OPC_MoveParent,
/*1883*/        OPC_EmitNode, TARGET_OPCODE(Mips::SLT), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #2 
/*1892*/        OPC_EmitInteger, MVT::i32, 1, 
/*1895*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::XORi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETGE:Other) - Complexity = 3
                // Dst: (XORi:i32 (SLT:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs), 1:i32)
/*1904*/      /*Scope*/ 24, /*->1929*/
/*1905*/        OPC_CheckCondCode, ISD::SETUGE,
/*1907*/        OPC_MoveParent,
/*1908*/        OPC_EmitNode, TARGET_OPCODE(Mips::SLTu), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #2 
/*1917*/        OPC_EmitInteger, MVT::i32, 1, 
/*1920*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::XORi), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs, SETUGE:Other) - Complexity = 3
                // Dst: (XORi:i32 (SLTu:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs), 1:i32)
/*1929*/      0, /*End of Scope*/
/*1930*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 37,  TARGET_OPCODE(ISD::AND),// ->1971
/*1934*/    OPC_RecordChild0, // #0 = $b
/*1935*/    OPC_RecordChild1, // #1 = $c
/*1936*/    OPC_Scope, 22, /*->1960*/ // 2 children in Scope
/*1938*/      OPC_MoveChild, 1,
/*1940*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1943*/      OPC_CheckPredicate, 17, // Predicate_immZExt16
/*1945*/      OPC_MoveParent,
/*1946*/      OPC_EmitConvertToTarget, 1,
/*1948*/      OPC_EmitNodeXForm, 0, 2, // LO16
/*1951*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::ANDi), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
              // Src: (and:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immZExt16>><<X:LO16>>:$c) - Complexity = 7
              // Dst: (ANDi:i32 CPURegs:i32:$b, (LO16:i32 (imm:i32):$c))
/*1960*/    /*Scope*/ 9, /*->1970*/
/*1961*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::AND), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (and:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
              // Dst: (AND:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*1970*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 37,  TARGET_OPCODE(ISD::OR),// ->2011
/*1974*/    OPC_RecordChild0, // #0 = $b
/*1975*/    OPC_RecordChild1, // #1 = $c
/*1976*/    OPC_Scope, 22, /*->2000*/ // 2 children in Scope
/*1978*/      OPC_MoveChild, 1,
/*1980*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1983*/      OPC_CheckPredicate, 17, // Predicate_immZExt16
/*1985*/      OPC_MoveParent,
/*1986*/      OPC_EmitConvertToTarget, 1,
/*1988*/      OPC_EmitNodeXForm, 0, 2, // LO16
/*1991*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::ORi), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
              // Src: (or:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immZExt16>><<X:LO16>>:$c) - Complexity = 7
              // Dst: (ORi:i32 CPURegs:i32:$b, (LO16:i32 (imm:i32):$c))
/*2000*/    /*Scope*/ 9, /*->2010*/
/*2001*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::OR), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (or:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
              // Dst: (OR:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*2010*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SHL),// ->2048
/*2014*/    OPC_RecordChild0, // #0 = $b
/*2015*/    OPC_RecordChild1, // #1 = $c
/*2016*/    OPC_Scope, 19, /*->2037*/ // 2 children in Scope
/*2018*/      OPC_MoveChild, 1,
/*2020*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2023*/      OPC_CheckPredicate, 19, // Predicate_immZExt5
/*2025*/      OPC_MoveParent,
/*2026*/      OPC_EmitConvertToTarget, 1,
/*2028*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLL), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
              // Src: (shl:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immZExt5>>:$c) - Complexity = 7
              // Dst: (SLL:i32 CPURegs:i32:$b, (imm:i32):$c)
/*2037*/    /*Scope*/ 9, /*->2047*/
/*2038*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SLLV), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (shl:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
              // Dst: (SLLV:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*2047*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SRL),// ->2085
/*2051*/    OPC_RecordChild0, // #0 = $b
/*2052*/    OPC_RecordChild1, // #1 = $c
/*2053*/    OPC_Scope, 19, /*->2074*/ // 2 children in Scope
/*2055*/      OPC_MoveChild, 1,
/*2057*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2060*/      OPC_CheckPredicate, 19, // Predicate_immZExt5
/*2062*/      OPC_MoveParent,
/*2063*/      OPC_EmitConvertToTarget, 1,
/*2065*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SRL), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
              // Src: (srl:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immZExt5>>:$c) - Complexity = 7
              // Dst: (SRL:i32 CPURegs:i32:$b, (imm:i32):$c)
/*2074*/    /*Scope*/ 9, /*->2084*/
/*2075*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SRLV), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (srl:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
              // Dst: (SRLV:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*2084*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SRA),// ->2122
/*2088*/    OPC_RecordChild0, // #0 = $b
/*2089*/    OPC_RecordChild1, // #1 = $c
/*2090*/    OPC_Scope, 19, /*->2111*/ // 2 children in Scope
/*2092*/      OPC_MoveChild, 1,
/*2094*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2097*/      OPC_CheckPredicate, 19, // Predicate_immZExt5
/*2099*/      OPC_MoveParent,
/*2100*/      OPC_EmitConvertToTarget, 1,
/*2102*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SRA), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
              // Src: (sra:i32 CPURegs:i32:$b, (imm:i32)<<P:Predicate_immZExt5>>:$c) - Complexity = 7
              // Dst: (SRA:i32 CPURegs:i32:$b, (imm:i32):$c)
/*2111*/    /*Scope*/ 9, /*->2121*/
/*2112*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SRAV), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (sra:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
              // Dst: (SRAV:i32 CPURegs:i32:$b, CPURegs:i32:$c)
/*2121*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 17,  TARGET_OPCODE(ISD::CALLSEQ_START),// ->2142
/*2125*/    OPC_RecordNode,   // #0 = 'callseq_start' chained node
/*2126*/    OPC_RecordChild1, // #1 = $amt
/*2127*/    OPC_MoveChild, 1,
/*2129*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*2132*/    OPC_MoveParent,
/*2133*/    OPC_EmitMergeInputChains1_0,
/*2134*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADJCALLSTACKDOWN), 0|OPFL_Chain|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
            // Src: (callseq_start (timm:i32):$amt) - Complexity = 6
            // Dst: (ADJCALLSTACKDOWN:i32 (timm:i32):$amt)
          /*SwitchOpcode*/ 67,  TARGET_OPCODE(MipsISD::JmpLink),// ->2212
/*2145*/    OPC_RecordNode,   // #0 = 'MipsJmpLink' chained node
/*2146*/    OPC_CaptureFlagInput,
/*2147*/    OPC_RecordChild1, // #1 = $target
/*2148*/    OPC_Scope, 49, /*->2199*/ // 2 children in Scope
/*2150*/      OPC_MoveChild, 1,
/*2152*/      OPC_SwitchOpcode /*3 cases */, 12,  TARGET_OPCODE(ISD::Constant),// ->2168
/*2156*/        OPC_MoveParent,
/*2157*/        OPC_EmitMergeInputChains1_0,
/*2158*/        OPC_EmitConvertToTarget, 1,
/*2160*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::JAL), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
                // Src: (MipsJmpLink (imm:iPTR):$target) - Complexity = 6
                // Dst: (JAL:i32 (imm:i32):$target)
              /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->2183
/*2171*/        OPC_CheckType, MVT::i32,
/*2173*/        OPC_MoveParent,
/*2174*/        OPC_EmitMergeInputChains1_0,
/*2175*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::JAL), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (MipsJmpLink (tglobaladdr:i32):$dst) - Complexity = 6
                // Dst: (JAL:i32 (tglobaladdr:i32):$dst)
              /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetExternalSymbol),// ->2198
/*2186*/        OPC_CheckType, MVT::i32,
/*2188*/        OPC_MoveParent,
/*2189*/        OPC_EmitMergeInputChains1_0,
/*2190*/        OPC_MorphNodeTo, TARGET_OPCODE(Mips::JAL), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (MipsJmpLink (texternalsym:i32):$dst) - Complexity = 6
                // Dst: (JAL:i32 (texternalsym:i32):$dst)
              0, // EndSwitchOpcode
/*2199*/    /*Scope*/ 11, /*->2211*/
/*2200*/      OPC_CheckChild1Type, MVT::i32,
/*2202*/      OPC_EmitMergeInputChains1_0,
/*2203*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::JALR), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
              // Src: (MipsJmpLink CPURegs:i32:$rs) - Complexity = 3
              // Dst: (JALR:i32 CPURegs:i32:$rs)
/*2211*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 32,  TARGET_OPCODE(ISD::ADDC),// ->2247
/*2215*/    OPC_RecordChild0, // #0 = $src
/*2216*/    OPC_RecordChild1, // #1 = $imm
/*2217*/    OPC_Scope, 17, /*->2236*/ // 2 children in Scope
/*2219*/      OPC_MoveChild, 1,
/*2221*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2224*/      OPC_MoveParent,
/*2225*/      OPC_EmitConvertToTarget, 1,
/*2227*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0|OPFL_FlagOutput,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
              // Src: (addc:i32 CPURegs:i32:$src, (imm:i32):$imm) - Complexity = 6
              // Dst: (ADDiu:i32 CPURegs:i32:$src, (imm:i32):$imm)
/*2236*/    /*Scope*/ 9, /*->2246*/
/*2237*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDu), 0|OPFL_FlagOutput,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (addc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs) - Complexity = 3
              // Dst: (ADDu:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs)
/*2246*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 41,  TARGET_OPCODE(MipsISD::Hi),// ->2291
/*2250*/    OPC_RecordChild0, // #0 = $in
/*2251*/    OPC_MoveChild, 0,
/*2253*/    OPC_SwitchOpcode /*3 cases */, 9,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->2266
/*2257*/      OPC_MoveParent,
/*2258*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::LUi), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (MipsHi:i32 (tglobaladdr:i32):$in) - Complexity = 6
              // Dst: (LUi:i32 (tglobaladdr:i32):$in)
            /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::TargetJumpTable),// ->2278
/*2269*/      OPC_MoveParent,
/*2270*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::LUi), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (MipsHi:i32 (tjumptable:i32):$in) - Complexity = 6
              // Dst: (LUi:i32 (tjumptable:i32):$in)
            /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::TargetConstantPool),// ->2290
/*2281*/      OPC_MoveParent,
/*2282*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::LUi), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (MipsHi:i32 (tconstpool:i32):$in) - Complexity = 6
              // Dst: (LUi:i32 (tconstpool:i32):$in)
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 51,  TARGET_OPCODE(MipsISD::FPCmp),// ->2345
/*2294*/    OPC_RecordChild0, // #0 = $fs
/*2295*/    OPC_Scope, 22, /*->2319*/ // 2 children in Scope
/*2297*/      OPC_CheckChild0Type, MVT::f32,
/*2299*/      OPC_RecordChild1, // #1 = $ft
/*2300*/      OPC_RecordChild2, // #2 = $cc
/*2301*/      OPC_MoveChild, 2,
/*2303*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2306*/      OPC_MoveParent,
/*2307*/      OPC_EmitConvertToTarget, 2,
/*2309*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FCMP_S32), 0,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 1, 3, 
              // Src: (MipsFPCmp:i32 FGR32:f32:$fs, FGR32:f32:$ft, (imm:i32):$cc) - Complexity = 6
              // Dst: (FCMP_S32:i32 FGR32:f32:$fs, FGR32:f32:$ft, (imm:i32):$cc)
/*2319*/    /*Scope*/ 24, /*->2344*/
/*2320*/      OPC_CheckChild0Type, MVT::f64,
/*2322*/      OPC_RecordChild1, // #1 = $ft
/*2323*/      OPC_RecordChild2, // #2 = $cc
/*2324*/      OPC_MoveChild, 2,
/*2326*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2329*/      OPC_MoveParent,
/*2330*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2332*/      OPC_EmitConvertToTarget, 2,
/*2334*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FCMP_D32), 0,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 1, 3, 
              // Src: (MipsFPCmp:i32 AFGR64:f64:$fs, AFGR64:f64:$ft, (imm:i32):$cc) - Complexity = 6
              // Dst: (FCMP_D32:i32 AFGR64:f64:$fs, AFGR64:f64:$ft, (imm:i32):$cc)
/*2344*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 59,  TARGET_OPCODE(MipsISD::FPSelectCC),// ->2407
/*2348*/    OPC_RecordChild0, // #0 = $CmpRes
/*2349*/    OPC_RecordChild1, // #1 = $T
/*2350*/    OPC_RecordChild2, // #2 = $F
/*2351*/    OPC_RecordChild3, // #3 = $cc
/*2352*/    OPC_MoveChild, 3,
/*2354*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2357*/    OPC_MoveParent,
/*2358*/    OPC_SwitchType /*3 cases */, 13,  MVT::i32,// ->2374
/*2361*/      OPC_EmitConvertToTarget, 3,
/*2363*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::Select_FCC), 0,
                  1/*#VTs*/, MVT::i32, 4/*#Ops*/, 0, 1, 2, 4, 
              // Src: (MipsFPSelectCC:i32 CPURegs:i32:$CmpRes, CPURegs:i32:$T, CPURegs:i32:$F, (imm:i32):$cc) - Complexity = 6
              // Dst: (Select_FCC:i32 CPURegs:i32:$CmpRes, CPURegs:i32:$T, CPURegs:i32:$F, (imm:i32):$cc)
            /*SwitchType*/ 13,  MVT::f32,// ->2389
/*2376*/      OPC_EmitConvertToTarget, 3,
/*2378*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::Select_FCC_S32), 0,
                  1/*#VTs*/, MVT::f32, 4/*#Ops*/, 0, 1, 2, 4, 
              // Src: (MipsFPSelectCC:f32 CPURegs:i32:$CmpRes, FGR32:f32:$T, FGR32:f32:$F, (imm:i32):$cc) - Complexity = 6
              // Dst: (Select_FCC_S32:f32 CPURegs:i32:$CmpRes, FGR32:f32:$T, FGR32:f32:$F, (imm:i32):$cc)
            /*SwitchType*/ 15,  MVT::f64,// ->2406
/*2391*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2393*/      OPC_EmitConvertToTarget, 3,
/*2395*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::Select_FCC_D32), 0,
                  1/*#VTs*/, MVT::f64, 4/*#Ops*/, 0, 1, 2, 4, 
              // Src: (MipsFPSelectCC:f64 CPURegs:i32:$CmpRes, AFGR64:f64:$T, AFGR64:f64:$F, (imm:i32):$cc) - Complexity = 6
              // Dst: (Select_FCC_D32:f64 CPURegs:i32:$CmpRes, AFGR64:f64:$T, AFGR64:f64:$F, (imm:i32):$cc)
            0, // EndSwitchType
          /*SwitchOpcode*/ 65,  TARGET_OPCODE(ISD::Constant),// ->2475
/*2410*/    OPC_RecordNode,   // #0 = $in
/*2411*/    OPC_Scope, 16, /*->2429*/ // 3 children in Scope
/*2413*/      OPC_CheckPredicate, 18, // Predicate_immSExt16
/*2415*/      OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*2418*/      OPC_EmitConvertToTarget, 0,
/*2420*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::ADDiu), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
              // Src: (imm:i32)<<P:Predicate_immSExt16>>:$in - Complexity = 4
              // Dst: (ADDiu:i32 ZERO:i32, (imm:i32):$in)
/*2429*/    /*Scope*/ 16, /*->2446*/
/*2430*/      OPC_CheckPredicate, 17, // Predicate_immZExt16
/*2432*/      OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*2435*/      OPC_EmitConvertToTarget, 0,
/*2437*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::ORi), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
              // Src: (imm:i32)<<P:Predicate_immZExt16>><<X:LO16>>:$in - Complexity = 4
              // Dst: (ORi:i32 ZERO:i32, (imm:i32):$in)
/*2446*/    /*Scope*/ 27, /*->2474*/
/*2447*/      OPC_EmitConvertToTarget, 0,
/*2449*/      OPC_EmitNodeXForm, 1, 1, // HI16
/*2452*/      OPC_EmitNode, TARGET_OPCODE(Mips::LUi), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2,  // Results = #3 
/*2460*/      OPC_EmitConvertToTarget, 0,
/*2462*/      OPC_EmitNodeXForm, 0, 4, // LO16
/*2465*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::ORi), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 3, 5, 
              // Src: (imm:i32):$imm - Complexity = 3
              // Dst: (ORi:i32 (LUi:i32 (HI16:i32 (imm:i32):$imm)), (LO16:i32 (imm:i32):$imm))
/*2474*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::SUB),// ->2489
/*2478*/    OPC_RecordChild0, // #0 = $b
/*2479*/    OPC_RecordChild1, // #1 = $c
/*2480*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::SUBu), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
            // Src: (sub:i32 CPURegs:i32:$b, CPURegs:i32:$c) - Complexity = 3
            // Dst: (SUBu:i32 CPURegs:i32:$b, CPURegs:i32:$c)
          /*SwitchOpcode*/ 16,  TARGET_OPCODE(ISD::BR),// ->2508
/*2492*/    OPC_RecordNode,   // #0 = 'br' chained node
/*2493*/    OPC_RecordChild1, // #1 = $target
/*2494*/    OPC_MoveChild, 1,
/*2496*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*2499*/    OPC_MoveParent,
/*2500*/    OPC_EmitMergeInputChains1_0,
/*2501*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::J), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (br (bb:Other):$target) - Complexity = 3
            // Dst: (J (bb:Other):$target)
          /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::BRIND),// ->2523
/*2511*/    OPC_RecordNode,   // #0 = 'brind' chained node
/*2512*/    OPC_RecordChild1, // #1 = $target
/*2513*/    OPC_CheckChild1Type, MVT::i32,
/*2515*/    OPC_EmitMergeInputChains1_0,
/*2516*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::JR), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (brind CPURegs:i32:$target) - Complexity = 3
            // Dst: (JR CPURegs:i32:$target)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(MipsISD::Ret),// ->2537
/*2526*/    OPC_RecordNode,   // #0 = 'MipsRet' chained node
/*2527*/    OPC_CaptureFlagInput,
/*2528*/    OPC_RecordChild1, // #1 = $target
/*2529*/    OPC_EmitMergeInputChains1_0,
/*2530*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::RET), 0|OPFL_Chain|OPFL_FlagInput,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (MipsRet CPURegs:i32:$target) - Complexity = 3
            // Dst: (RET CPURegs:i32:$target)
          /*SwitchOpcode*/ 33,  TARGET_OPCODE(ISD::SIGN_EXTEND_INREG),// ->2573
/*2540*/    OPC_RecordChild0, // #0 = $src
/*2541*/    OPC_MoveChild, 1,
/*2543*/    OPC_Scope, 13, /*->2558*/ // 2 children in Scope
/*2545*/      OPC_CheckValueType, MVT::i8,
/*2547*/      OPC_MoveParent,
/*2548*/      OPC_CheckPatternPredicate, 3, // (Subtarget.hasSEInReg())
/*2550*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SEB), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (sext_inreg:i32 CPURegs:i32:$src, i8:Other) - Complexity = 3
              // Dst: (SEB:i32 CPURegs:i32:$src)
/*2558*/    /*Scope*/ 13, /*->2572*/
/*2559*/      OPC_CheckValueType, MVT::i16,
/*2561*/      OPC_MoveParent,
/*2562*/      OPC_CheckPatternPredicate, 3, // (Subtarget.hasSEInReg())
/*2564*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::SEH), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (sext_inreg:i32 CPURegs:i32:$src, i16:Other) - Complexity = 3
              // Dst: (SEH:i32 CPURegs:i32:$src)
/*2572*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::CTLZ),// ->2587
/*2576*/    OPC_RecordChild0, // #0 = $src
/*2577*/    OPC_CheckPatternPredicate, 4, // (Subtarget.hasBitCount())
/*2579*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::CLZ), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
            // Src: (ctlz:i32 CPURegs:i32:$src) - Complexity = 3
            // Dst: (CLZ:i32 CPURegs:i32:$src)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::BSWAP),// ->2601
/*2590*/    OPC_RecordChild0, // #0 = $src
/*2591*/    OPC_CheckPatternPredicate, 5, // (Subtarget.hasSwap())
/*2593*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::WSBW), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
            // Src: (bswap:i32 CPURegs:i32:$src) - Complexity = 3
            // Dst: (WSBW:i32 CPURegs:i32:$src)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::SUBC),// ->2615
/*2604*/    OPC_RecordChild0, // #0 = $lhs
/*2605*/    OPC_RecordChild1, // #1 = $rhs
/*2606*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::SUBu), 0|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
            // Src: (subc:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs) - Complexity = 3
            // Dst: (SUBu:i32 CPURegs:i32:$lhs, CPURegs:i32:$rhs)
          /*SwitchOpcode*/ 27,  TARGET_OPCODE(ISD::BIT_CONVERT),// ->2645
/*2618*/    OPC_RecordChild0, // #0 = $src
/*2619*/    OPC_SwitchType /*2 cases */, 10,  MVT::i32,// ->2632
/*2622*/      OPC_CheckChild0Type, MVT::f32,
/*2624*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::MFC1), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (bitconvert:i32 FGR32:f32:$src) - Complexity = 3
              // Dst: (MFC1:i32 FGR32:f32:$src)
            /*SwitchType*/ 10,  MVT::f32,// ->2644
/*2634*/      OPC_CheckChild0Type, MVT::i32,
/*2636*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::MTC1), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (bitconvert:f32 CPURegs:i32:$src) - Complexity = 3
              // Dst: (MTC1:f32 CPURegs:i32:$src)
            0, // EndSwitchType
          /*SwitchOpcode*/ 19,  TARGET_OPCODE(ISD::FP_TO_SINT),// ->2667
/*2648*/    OPC_RecordChild0, // #0 = $src
/*2649*/    OPC_CheckChild0Type, MVT::f32,
/*2651*/    OPC_EmitNode, TARGET_OPCODE(Mips::TRUNC_W_S32), 0,
                1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0,  // Results = #1 
/*2659*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::MFC1), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
            // Src: (fp_to_sint:i32 FGR32:f32:$src) - Complexity = 3
            // Dst: (MFC1:i32 (TRUNC_W_S32:f32 FGR32:f32:$src))
          /*SwitchOpcode*/ 43,  TARGET_OPCODE(MipsISD::SelectCC),// ->2713
/*2670*/    OPC_RecordChild0, // #0 = $CmpRes
/*2671*/    OPC_RecordChild1, // #1 = $T
/*2672*/    OPC_RecordChild2, // #2 = $F
/*2673*/    OPC_SwitchType /*3 cases */, 10,  MVT::i32,// ->2686
/*2676*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::Select_CC), 0,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (MipsSelectCC:i32 CPURegs:i32:$CmpRes, CPURegs:i32:$T, CPURegs:i32:$F) - Complexity = 3
              // Dst: (Select_CC:i32 CPURegs:i32:$CmpRes, CPURegs:i32:$T, CPURegs:i32:$F)
            /*SwitchType*/ 10,  MVT::f32,// ->2698
/*2688*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::Select_CC_S32), 0,
                  1/*#VTs*/, MVT::f32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (MipsSelectCC:f32 CPURegs:i32:$CmpRes, FGR32:f32:$T, FGR32:f32:$F) - Complexity = 3
              // Dst: (Select_CC_S32:f32 CPURegs:i32:$CmpRes, FGR32:f32:$T, FGR32:f32:$F)
            /*SwitchType*/ 12,  MVT::f64,// ->2712
/*2700*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2702*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::Select_CC_D32), 0,
                  1/*#VTs*/, MVT::f64, 3/*#Ops*/, 0, 1, 2, 
              // Src: (MipsSelectCC:f64 CPURegs:i32:$CmpRes, AFGR64:f64:$T, AFGR64:f64:$F) - Complexity = 3
              // Dst: (Select_CC_D32:f64 CPURegs:i32:$CmpRes, AFGR64:f64:$T, AFGR64:f64:$F)
            0, // EndSwitchType
          /*SwitchOpcode*/ 40,  TARGET_OPCODE(ISD::ConstantFP),// ->2756
/*2716*/    OPC_CheckType, MVT::f32,
/*2718*/    OPC_Scope, 13, /*->2733*/ // 2 children in Scope
/*2720*/      OPC_CheckPredicate, 20, // Predicate_fpimm0
/*2722*/      OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*2725*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::MTC1), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (fpimm:f32)<<P:Predicate_fpimm0>> - Complexity = 4
              // Dst: (MTC1:f32 ZERO:i32)
/*2733*/    /*Scope*/ 21, /*->2755*/
/*2734*/      OPC_CheckPredicate, 21, // Predicate_fpimm0neg
/*2736*/      OPC_EmitRegister, MVT::i32, Mips::ZERO,
/*2739*/      OPC_EmitNode, TARGET_OPCODE(Mips::MTC1), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0,  // Results = #1 
/*2747*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FNEG_S32), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 1, 
              // Src: (fpimm:f32)<<P:Predicate_fpimm0neg>> - Complexity = 4
              // Dst: (FNEG_S32:f32 (MTC1:f32 ZERO:i32))
/*2755*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 25,  TARGET_OPCODE(ISD::FABS),// ->2784
/*2759*/    OPC_RecordChild0, // #0 = $fs
/*2760*/    OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->2771
/*2763*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FABS_S32), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (fabs:f32 FGR32:f32:$fs) - Complexity = 3
              // Dst: (FABS_S32:f32 FGR32:f32:$fs)
            /*SwitchType*/ 10,  MVT::f64,// ->2783
/*2773*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2775*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FABS_D32), 0,
                  1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
              // Src: (fabs:f64 AFGR64:f64:$fs) - Complexity = 3
              // Dst: (FABS_D32:f64 AFGR64:f64:$fs)
            0, // EndSwitchType
          /*SwitchOpcode*/ 25,  TARGET_OPCODE(ISD::FNEG),// ->2812
/*2787*/    OPC_RecordChild0, // #0 = $fs
/*2788*/    OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->2799
/*2791*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FNEG_S32), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (fneg:f32 FGR32:f32:$fs) - Complexity = 3
              // Dst: (FNEG_S32:f32 FGR32:f32:$fs)
            /*SwitchType*/ 10,  MVT::f64,// ->2811
/*2801*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2803*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FNEG_D32), 0,
                  1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
              // Src: (fneg:f64 AFGR64:f64:$fs) - Complexity = 3
              // Dst: (FNEG_D32:f64 AFGR64:f64:$fs)
            0, // EndSwitchType
          /*SwitchOpcode*/ 25,  TARGET_OPCODE(ISD::FSQRT),// ->2840
/*2815*/    OPC_RecordChild0, // #0 = $fs
/*2816*/    OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->2827
/*2819*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FSQRT_S32), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (fsqrt:f32 FGR32:f32:$fs) - Complexity = 3
              // Dst: (FSQRT_S32:f32 FGR32:f32:$fs)
            /*SwitchType*/ 10,  MVT::f64,// ->2839
/*2829*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2831*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FSQRT_D32), 0,
                  1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
              // Src: (fsqrt:f64 AFGR64:f64:$fs) - Complexity = 3
              // Dst: (FSQRT_D32:f64 AFGR64:f64:$fs)
            0, // EndSwitchType
          /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::FADD),// ->2871
/*2843*/    OPC_RecordChild0, // #0 = $fs
/*2844*/    OPC_RecordChild1, // #1 = $ft
/*2845*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->2857
/*2848*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FADD_S32), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fadd:f32 FGR32:f32:$fs, FGR32:f32:$ft) - Complexity = 3
              // Dst: (FADD_S32:f32 FGR32:f32:$fs, FGR32:f32:$ft)
            /*SwitchType*/ 11,  MVT::f64,// ->2870
/*2859*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2861*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FADD_D32), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fadd:f64 AFGR64:f64:$fs, AFGR64:f64:$ft) - Complexity = 3
              // Dst: (FADD_D32:f64 AFGR64:f64:$fs, AFGR64:f64:$ft)
            0, // EndSwitchType
          /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::FDIV),// ->2902
/*2874*/    OPC_RecordChild0, // #0 = $fs
/*2875*/    OPC_RecordChild1, // #1 = $ft
/*2876*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->2888
/*2879*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FDIV_S32), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fdiv:f32 FGR32:f32:$fs, FGR32:f32:$ft) - Complexity = 3
              // Dst: (FDIV_S32:f32 FGR32:f32:$fs, FGR32:f32:$ft)
            /*SwitchType*/ 11,  MVT::f64,// ->2901
/*2890*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2892*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FDIV_D32), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fdiv:f64 AFGR64:f64:$fs, AFGR64:f64:$ft) - Complexity = 3
              // Dst: (FDIV_D32:f64 AFGR64:f64:$fs, AFGR64:f64:$ft)
            0, // EndSwitchType
          /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::FMUL),// ->2933
/*2905*/    OPC_RecordChild0, // #0 = $fs
/*2906*/    OPC_RecordChild1, // #1 = $ft
/*2907*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->2919
/*2910*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FMUL_S32), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fmul:f32 FGR32:f32:$fs, FGR32:f32:$ft) - Complexity = 3
              // Dst: (FMUL_S32:f32 FGR32:f32:$fs, FGR32:f32:$ft)
            /*SwitchType*/ 11,  MVT::f64,// ->2932
/*2921*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2923*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FMUL_D32), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fmul:f64 AFGR64:f64:$fs, AFGR64:f64:$ft) - Complexity = 3
              // Dst: (FMUL_D32:f64 AFGR64:f64:$fs, AFGR64:f64:$ft)
            0, // EndSwitchType
          /*SwitchOpcode*/ 28,  TARGET_OPCODE(ISD::FSUB),// ->2964
/*2936*/    OPC_RecordChild0, // #0 = $fs
/*2937*/    OPC_RecordChild1, // #1 = $ft
/*2938*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->2950
/*2941*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FSUB_S32), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fsub:f32 FGR32:f32:$fs, FGR32:f32:$ft) - Complexity = 3
              // Dst: (FSUB_S32:f32 FGR32:f32:$fs, FGR32:f32:$ft)
            /*SwitchType*/ 11,  MVT::f64,// ->2963
/*2952*/      OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2954*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::FSUB_D32), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fsub:f64 AFGR64:f64:$fs, AFGR64:f64:$ft) - Complexity = 3
              // Dst: (FSUB_D32:f64 AFGR64:f64:$fs, AFGR64:f64:$ft)
            0, // EndSwitchType
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::FP_ROUND),// ->2978
/*2967*/    OPC_RecordChild0, // #0 = $src
/*2968*/    OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2970*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::CVTS_D32), 0,
                1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
            // Src: (fround:f32 AFGR64:f64:$src) - Complexity = 3
            // Dst: (CVTS_D32:f32 AFGR64:f64:$src)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::FP_EXTEND),// ->2992
/*2981*/    OPC_RecordChild0, // #0 = $src
/*2982*/    OPC_CheckPatternPredicate, 2, // (!Subtarget.isFP64bit())
/*2984*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::CVTD_S32), 0,
                1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
            // Src: (fextend:f64 FGR32:f32:$src) - Complexity = 3
            // Dst: (CVTD_S32:f64 FGR32:f32:$src)
          /*SwitchOpcode*/ 10,  TARGET_OPCODE(MipsISD::FPRound),// ->3005
/*2995*/    OPC_CaptureFlagInput,
/*2996*/    OPC_RecordChild0, // #0 = $src
/*2997*/    OPC_MorphNodeTo, TARGET_OPCODE(Mips::CVTW_D32), 0|OPFL_FlagInput,
                1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
            // Src: (MipsFPRound:f32 AFGR64:f64:$src) - Complexity = 3
            // Dst: (CVTW_D32:f32 AFGR64:f64:$src)
          /*SwitchOpcode*/ 39,  TARGET_OPCODE(ISD::SINT_TO_FP),// ->3047
/*3008*/    OPC_RecordChild0, // #0 = $src
/*3009*/    OPC_SwitchType /*2 cases */, 16,  MVT::f32,// ->3028
/*3012*/      OPC_EmitNode, TARGET_OPCODE(Mips::MTC1), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0,  // Results = #1 
/*3020*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::CVTS_W32), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 1, 
              // Src: (sint_to_fp:f32 CPURegs:i32:$src) - Complexity = 3
              // Dst: (CVTS_W32:f32 (MTC1:f32 CPURegs:i32:$src))
            /*SwitchType*/ 16,  MVT::f64,// ->3046
/*3030*/      OPC_EmitNode, TARGET_OPCODE(Mips::MTC1), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0,  // Results = #1 
/*3038*/      OPC_MorphNodeTo, TARGET_OPCODE(Mips::CVTD_W32), 0,
                  1/*#VTs*/, MVT::f64, 1/*#Ops*/, 1, 
              // Src: (sint_to_fp:f64 CPURegs:i32:$src) - Complexity = 3
              // Dst: (CVTD_W32:f64 (MTC1:f32 CPURegs:i32:$src))
            0, // EndSwitchType
          0, // EndSwitchOpcode
    0
  }; // Total Array size is 3049 bytes

  // Opcode Histogram:
  // #OPC_Scope                          = 36
  // #OPC_RecordNode                     = 12
  // #OPC_RecordChild                    = 133
  // #OPC_RecordMemRef                   = 2
  // #OPC_CaptureFlagInput               = 4
  // #OPC_MoveChild                      = 62
  // #OPC_MoveParent                     = 134
  // #OPC_CheckSame                      = 0
  // #OPC_CheckPatternPredicate          = 20
  // #OPC_CheckPredicate                 = 40
  // #OPC_CheckOpcode                    = 40
  // #OPC_SwitchOpcode                   = 9
  // #OPC_CheckType                      = 21
  // #OPC_SwitchType                     = 12
  // #OPC_CheckChildType                 = 13
  // #OPC_CheckInteger                   = 10
  // #OPC_CheckCondCode                  = 36
  // #OPC_CheckValueType                 = 2
  // #OPC_CheckComplexPat                = 17
  // #OPC_CheckAndImm                    = 0
  // #OPC_CheckOrImm                     = 0
  // #OPC_CheckFoldableChainNode         = 0
  // #OPC_EmitInteger                    = 7
  // #OPC_EmitStringInteger              = 0
  // #OPC_EmitRegister                   = 16
  // #OPC_EmitConvertToTarget            = 26
  // #OPC_EmitMergeInputChains           = 44
  // #OPC_EmitCopyToReg                  = 4
  // #OPC_EmitNode                       = 27
  // #OPC_EmitNodeXForm                  = 5
  // #OPC_MarkFlagResults                = 0
  // #OPC_CompleteMatch                  = 0
  // #OPC_MorphNodeTo                    = 142

  #undef TARGET_OPCODE
  return SelectCodeCommon(N, MatcherTable,sizeof(MatcherTable));
}

bool CheckPatternPredicate(unsigned PredNo) const {
  switch (PredNo) {
  default: assert(0 && "Invalid predicate in table?");
  case 0: return (!Subtarget.isSingleFloat()) && (!Subtarget.isMips1());
  case 1: return (Subtarget.hasCondMov());
  case 2: return (!Subtarget.isFP64bit());
  case 3: return (Subtarget.hasSEInReg());
  case 4: return (Subtarget.hasBitCount());
  case 5: return (Subtarget.hasSwap());
  }
}

bool CheckNodePredicate(SDNode *Node, unsigned PredNo) const {
  switch (PredNo) {
  default: assert(0 && "Invalid predicate in table?");
  case 0: { // Predicate_unindexedstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 1: { // Predicate_store
    SDNode *N = Node;

  return !cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 2: { // Predicate_truncstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 3: { // Predicate_truncstorei8
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 4: { // Predicate_truncstorei16
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 5: { // Predicate_unindexedload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 6: { // Predicate_sextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::SEXTLOAD;

  }
  case 7: { // Predicate_sextloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 8: { // Predicate_zextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::ZEXTLOAD;

  }
  case 9: { // Predicate_zextloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 10: { // Predicate_sextloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 11: { // Predicate_zextloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 12: { // Predicate_load
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::NON_EXTLOAD;

  }
  case 13: { // Predicate_extload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::EXTLOAD;

  }
  case 14: { // Predicate_extloadi1
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i1;

  }
  case 15: { // Predicate_extloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 16: { // Predicate_extloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 17: { // Predicate_immZExt16
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  if (N->getValueType(0) == MVT::i32)
    return (uint32_t)N->getZExtValue() == (unsigned short)N->getZExtValue();
  else
    return (uint64_t)N->getZExtValue() == (unsigned short)N->getZExtValue();

  }
  case 18: { // Predicate_immSExt16
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
 return isInt<16>(N->getSExtValue()); 
  }
  case 19: { // Predicate_immZExt5
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return N->getZExtValue() == ((N->getZExtValue()) & 0x1f) ;

  }
  case 20: { // Predicate_fpimm0
    ConstantFPSDNode*N = cast<ConstantFPSDNode>(Node);

  return N->isExactlyValue(+0.0);

  }
  case 21: { // Predicate_fpimm0neg
    ConstantFPSDNode*N = cast<ConstantFPSDNode>(Node);

  return N->isExactlyValue(-0.0);

  }
  }
}

bool CheckComplexPattern(SDNode *Root, SDValue N,
      unsigned PatternNo, SmallVectorImpl<SDValue> &Result) {
  unsigned NextRes = Result.size();
  switch (PatternNo) {
  default: assert(0 && "Invalid pattern # in table?");
  case 0:
    Result.resize(NextRes+2);
    return SelectAddr(Root, N, Result[NextRes+0], Result[NextRes+1]);
  }
}

SDValue RunSDNodeXForm(SDValue V, unsigned XFormNo) {
  switch (XFormNo) {
  default: assert(0 && "Invalid xform # in table?");
  case 0: {  // LO16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  return getI32Imm((unsigned)N->getZExtValue() & 0xFFFF);

  }
  case 1: {  // HI16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  return getI32Imm((unsigned)N->getZExtValue() >> 16);

  }
  }
}

