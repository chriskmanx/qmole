//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// DAG Instruction Selector for the XCore target
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
/*0*/     OPC_SwitchOpcode /*38 cases */, 122|128,1/*250*/,  TARGET_OPCODE(ISD::ADD),// ->255
/*5*/       OPC_Scope, 29, /*->36*/ // 5 children in Scope
/*7*/         OPC_MoveChild, 0,
/*9*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*12*/        OPC_MoveChild, 0,
/*14*/        OPC_CheckInteger, 1, 
/*16*/        OPC_MoveParent,
/*17*/        OPC_RecordChild1, // #0 = $size
/*18*/        OPC_MoveParent,
/*19*/        OPC_MoveChild, 1,
/*21*/        OPC_CheckInteger, 127|128,127|128,127|128,127|128,15/*4294967295*/, 
/*27*/        OPC_MoveParent,
/*28*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::MKMSK_2r), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (add:i32 (shl:i32 1:i32, GRRegs:i32:$size), 4294967295:i32) - Complexity = 16
              // Dst: (MKMSK_2r:i32 GRRegs:i32:$size)
/*36*/      /*Scope*/ 39, /*->76*/
/*37*/        OPC_RecordChild0, // #0 = $addr
/*38*/        OPC_MoveChild, 1,
/*40*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*43*/        OPC_RecordChild0, // #1 = $offset
/*44*/        OPC_MoveChild, 1,
/*46*/        OPC_Scope, 13, /*->61*/ // 2 children in Scope
/*48*/          OPC_CheckInteger, 2, 
/*50*/          OPC_MoveParent,
/*51*/          OPC_MoveParent,
/*52*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWF_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (add:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 2:i32)) - Complexity = 11
                // Dst: (LDAWF_l3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*61*/        /*Scope*/ 13, /*->75*/
/*62*/          OPC_CheckInteger, 1, 
/*64*/          OPC_MoveParent,
/*65*/          OPC_MoveParent,
/*66*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDA16F_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (add:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 1:i32)) - Complexity = 11
                // Dst: (LDA16F_l3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*75*/        0, /*End of Scope*/
/*76*/      /*Scope*/ 40, /*->117*/
/*77*/        OPC_MoveChild, 0,
/*79*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*82*/        OPC_RecordChild0, // #0 = $offset
/*83*/        OPC_MoveChild, 1,
/*85*/        OPC_Scope, 14, /*->101*/ // 2 children in Scope
/*87*/          OPC_CheckInteger, 2, 
/*89*/          OPC_MoveParent,
/*90*/          OPC_MoveParent,
/*91*/          OPC_RecordChild1, // #1 = $addr
/*92*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWF_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (add:i32 (shl:i32 GRRegs:i32:$offset, 2:i32), GRRegs:i32:$addr) - Complexity = 11
                // Dst: (LDAWF_l3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*101*/       /*Scope*/ 14, /*->116*/
/*102*/         OPC_CheckInteger, 1, 
/*104*/         OPC_MoveParent,
/*105*/         OPC_MoveParent,
/*106*/         OPC_RecordChild1, // #1 = $addr
/*107*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDA16F_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (add:i32 (shl:i32 GRRegs:i32:$offset, 1:i32), GRRegs:i32:$addr) - Complexity = 11
                // Dst: (LDA16F_l3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*116*/       0, /*End of Scope*/
/*117*/     /*Scope*/ 44, /*->162*/
/*118*/       OPC_RecordNode, // #0 = $addr
/*119*/       OPC_CheckType, MVT::i32,
/*121*/       OPC_Scope, 12, /*->135*/ // 3 children in Scope
/*123*/         OPC_CheckComplexPat, /*CP*/0, /*#*/0, // SelectADDRspii:$addr #1 #2
/*126*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWFI), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                // Src: ADDRspii:i32:$addr - Complexity = 9
                // Dst: (LDAWFI:i32 ADDRspii:i32:$addr)
/*135*/       /*Scope*/ 12, /*->148*/
/*136*/         OPC_CheckComplexPat, /*CP*/1, /*#*/0, // SelectADDRdpii:$a #1 #2
/*139*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWDP_lru6), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                // Src: ADDRdpii:i32:$a - Complexity = 9
                // Dst: (LDAWDP_lru6:i32 ADDRdpii:i32:$a)
/*148*/       /*Scope*/ 12, /*->161*/
/*149*/         OPC_CheckComplexPat, /*CP*/2, /*#*/0, // SelectADDRcpii:$a #1 #2
/*152*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWCP_lu6), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                // Src: ADDRcpii:i32:$a - Complexity = 9
                // Dst: (LDAWCP_lu6:i32 ADDRcpii:i32:$a)
/*161*/       0, /*End of Scope*/
/*162*/     /*Scope*/ 91, /*->254*/
/*163*/       OPC_RecordChild0, // #0 = $b
/*164*/       OPC_RecordChild1, // #1 = $c
/*165*/       OPC_Scope, 76, /*->243*/ // 2 children in Scope
/*167*/         OPC_MoveChild, 1,
/*169*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*172*/         OPC_Scope, 14, /*->188*/ // 4 children in Scope
/*174*/           OPC_CheckPredicate, 0, // Predicate_immUs
/*176*/           OPC_MoveParent,
/*177*/           OPC_EmitConvertToTarget, 1,
/*179*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::ADD_2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                  // Src: (add:i32 GRRegs:i32:$b, (imm:i32)<<P:Predicate_immUs>>:$c) - Complexity = 7
                  // Dst: (ADD_2rus:i32 GRRegs:i32:$b, (imm:i32):$c)
/*188*/         /*Scope*/ 17, /*->206*/
/*189*/           OPC_CheckPredicate, 1, // Predicate_immUs4
/*191*/           OPC_MoveParent,
/*192*/           OPC_EmitConvertToTarget, 1,
/*194*/           OPC_EmitNodeXForm, 0, 2, // div4_xform
/*197*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWF_l2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (add:i32 GRRegs:i32:$addr, (imm:i32)<<P:Predicate_immUs4>>:$offset) - Complexity = 7
                  // Dst: (LDAWF_l2rus:i32 GRRegs:i32:$addr, (div4_xform:i32 (imm:i32)<<P:Predicate_immUs4>>:$offset))
/*206*/         /*Scope*/ 17, /*->224*/
/*207*/           OPC_CheckPredicate, 2, // Predicate_immUsNeg
/*209*/           OPC_MoveParent,
/*210*/           OPC_EmitConvertToTarget, 1,
/*212*/           OPC_EmitNodeXForm, 1, 2, // neg_xform
/*215*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::SUB_2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (add:i32 GRRegs:i32:$src1, (imm:i32)<<P:Predicate_immUsNeg>>:$src2) - Complexity = 7
                  // Dst: (SUB_2rus:i32 GRRegs:i32:$src1, (neg_xform:i32 (imm:i32)<<P:Predicate_immUsNeg>>:$src2))
/*224*/         /*Scope*/ 17, /*->242*/
/*225*/           OPC_CheckPredicate, 3, // Predicate_immUs4Neg
/*227*/           OPC_MoveParent,
/*228*/           OPC_EmitConvertToTarget, 1,
/*230*/           OPC_EmitNodeXForm, 2, 2, // div4neg_xform
/*233*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWB_l2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (add:i32 GRRegs:i32:$src1, (imm:i32)<<P:Predicate_immUs4Neg>>:$src2) - Complexity = 7
                  // Dst: (LDAWB_l2rus:i32 GRRegs:i32:$src1, (div4neg_xform:i32 (imm:i32)<<P:Predicate_immUs4Neg>>:$src2))
/*242*/         0, /*End of Scope*/
/*243*/       /*Scope*/ 9, /*->253*/
/*244*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::ADD_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (add:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
                // Dst: (ADD_3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*253*/       0, /*End of Scope*/
/*254*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 45|128,3/*429*/,  TARGET_OPCODE(ISD::LOAD),// ->688
/*259*/     OPC_RecordMemRef,
/*260*/     OPC_RecordNode,   // #0 = 'ld' chained node
/*261*/     OPC_Scope, 40|128,1/*168*/, /*->432*/ // 4 children in Scope
/*264*/       OPC_MoveChild, 1,
/*266*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::ADD),
/*269*/       OPC_Scope, 79, /*->350*/ // 2 children in Scope
/*271*/         OPC_RecordChild0, // #1 = $addr
/*272*/         OPC_MoveChild, 1,
/*274*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*277*/         OPC_RecordChild0, // #2 = $offset
/*278*/         OPC_MoveChild, 1,
/*280*/         OPC_Scope, 43, /*->325*/ // 2 children in Scope
/*282*/           OPC_CheckInteger, 1, 
/*284*/           OPC_MoveParent,
/*285*/           OPC_MoveParent,
/*286*/           OPC_CheckType, MVT::i32,
/*288*/           OPC_MoveParent,
/*289*/           OPC_CheckPredicate, 4, // Predicate_unindexedload
/*291*/           OPC_CheckType, MVT::i32,
/*293*/           OPC_Scope, 14, /*->309*/ // 2 children in Scope
/*295*/             OPC_CheckPredicate, 5, // Predicate_sextload
/*297*/             OPC_CheckPredicate, 6, // Predicate_sextloadi16
/*299*/             OPC_EmitMergeInputChains1_0,
/*300*/             OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD16S_3r), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                    // Src: (ld:i32 (add:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 1:i32)))<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 15
                    // Dst: (LD16S_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*309*/           /*Scope*/ 14, /*->324*/
/*310*/             OPC_CheckPredicate, 7, // Predicate_extload
/*312*/             OPC_CheckPredicate, 8, // Predicate_extloadi16
/*314*/             OPC_EmitMergeInputChains1_0,
/*315*/             OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD16S_3r), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                    // Src: (ld:i32 (add:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 1:i32)))<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 15
                    // Dst: (LD16S_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*324*/           0, /*End of Scope*/
/*325*/         /*Scope*/ 23, /*->349*/
/*326*/           OPC_CheckInteger, 2, 
/*328*/           OPC_MoveParent,
/*329*/           OPC_MoveParent,
/*330*/           OPC_CheckType, MVT::i32,
/*332*/           OPC_MoveParent,
/*333*/           OPC_CheckPredicate, 4, // Predicate_unindexedload
/*335*/           OPC_CheckPredicate, 9, // Predicate_load
/*337*/           OPC_CheckType, MVT::i32,
/*339*/           OPC_EmitMergeInputChains1_0,
/*340*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDW_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                  // Src: (ld:i32 (add:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 2:i32)))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 15
                  // Dst: (LDW_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*349*/         0, /*End of Scope*/
/*350*/       /*Scope*/ 80, /*->431*/
/*351*/         OPC_MoveChild, 0,
/*353*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*356*/         OPC_RecordChild0, // #1 = $offset
/*357*/         OPC_MoveChild, 1,
/*359*/         OPC_Scope, 44, /*->405*/ // 2 children in Scope
/*361*/           OPC_CheckInteger, 1, 
/*363*/           OPC_MoveParent,
/*364*/           OPC_MoveParent,
/*365*/           OPC_RecordChild1, // #2 = $addr
/*366*/           OPC_CheckType, MVT::i32,
/*368*/           OPC_MoveParent,
/*369*/           OPC_CheckPredicate, 4, // Predicate_unindexedload
/*371*/           OPC_CheckType, MVT::i32,
/*373*/           OPC_Scope, 14, /*->389*/ // 2 children in Scope
/*375*/             OPC_CheckPredicate, 7, // Predicate_extload
/*377*/             OPC_CheckPredicate, 8, // Predicate_extloadi16
/*379*/             OPC_EmitMergeInputChains1_0,
/*380*/             OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD16S_3r), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 1, 
                    // Src: (ld:i32 (add:i32 (shl:i32 GRRegs:i32:$offset, 1:i32), GRRegs:i32:$addr))<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 15
                    // Dst: (LD16S_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*389*/           /*Scope*/ 14, /*->404*/
/*390*/             OPC_CheckPredicate, 5, // Predicate_sextload
/*392*/             OPC_CheckPredicate, 6, // Predicate_sextloadi16
/*394*/             OPC_EmitMergeInputChains1_0,
/*395*/             OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD16S_3r), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 1, 
                    // Src: (ld:i32 (add:i32 (shl:i32 GRRegs:i32:$offset, 1:i32), GRRegs:i32:$addr))<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 15
                    // Dst: (LD16S_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*404*/           0, /*End of Scope*/
/*405*/         /*Scope*/ 24, /*->430*/
/*406*/           OPC_CheckInteger, 2, 
/*408*/           OPC_MoveParent,
/*409*/           OPC_MoveParent,
/*410*/           OPC_RecordChild1, // #2 = $addr
/*411*/           OPC_CheckType, MVT::i32,
/*413*/           OPC_MoveParent,
/*414*/           OPC_CheckPredicate, 4, // Predicate_unindexedload
/*416*/           OPC_CheckPredicate, 9, // Predicate_load
/*418*/           OPC_CheckType, MVT::i32,
/*420*/           OPC_EmitMergeInputChains1_0,
/*421*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDW_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:i32 (add:i32 (shl:i32 GRRegs:i32:$offset, 2:i32), GRRegs:i32:$addr))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 15
                  // Dst: (LDW_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*430*/         0, /*End of Scope*/
/*431*/       0, /*End of Scope*/
/*432*/     /*Scope*/ 39, /*->472*/
/*433*/       OPC_RecordChild1, // #1 = $addr
/*434*/       OPC_CheckChild1Type, MVT::i32,
/*436*/       OPC_CheckPredicate, 4, // Predicate_unindexedload
/*438*/       OPC_CheckPredicate, 9, // Predicate_load
/*440*/       OPC_CheckType, MVT::i32,
/*442*/       OPC_Scope, 13, /*->457*/ // 2 children in Scope
/*444*/         OPC_CheckComplexPat, /*CP*/0, /*#*/1, // SelectADDRspii:$addr #2 #3
/*447*/         OPC_EmitMergeInputChains1_0,
/*448*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDWFI), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i32 ADDRspii:i32:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                // Dst: (LDWFI:i32 ADDRspii:i32:$addr)
/*457*/       /*Scope*/ 13, /*->471*/
/*458*/         OPC_CheckComplexPat, /*CP*/1, /*#*/1, // SelectADDRdpii:$a #2 #3
/*461*/         OPC_EmitMergeInputChains1_0,
/*462*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDWDP_lru6), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (ld:i32 ADDRdpii:i32:$a)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 13
                // Dst: (LDWDP_lru6:i32 ADDRdpii:i32:$a)
/*471*/       0, /*End of Scope*/
/*472*/     /*Scope*/ 82, /*->555*/
/*473*/       OPC_MoveChild, 1,
/*475*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::ADD),
/*478*/       OPC_RecordChild0, // #1 = $addr
/*479*/       OPC_RecordChild1, // #2 = $offset
/*480*/       OPC_Scope, 32, /*->514*/ // 2 children in Scope
/*482*/         OPC_MoveChild, 1,
/*484*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*487*/         OPC_CheckPredicate, 1, // Predicate_immUs4
/*489*/         OPC_MoveParent,
/*490*/         OPC_CheckType, MVT::i32,
/*492*/         OPC_MoveParent,
/*493*/         OPC_CheckPredicate, 4, // Predicate_unindexedload
/*495*/         OPC_CheckPredicate, 9, // Predicate_load
/*497*/         OPC_CheckType, MVT::i32,
/*499*/         OPC_EmitMergeInputChains1_0,
/*500*/         OPC_EmitConvertToTarget, 2,
/*502*/         OPC_EmitNodeXForm, 0, 3, // div4_xform
/*505*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDW_2rus), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 4, 
                // Src: (ld:i32 (add:i32 GRRegs:i32:$addr, (imm:i32)<<P:Predicate_immUs4>>:$offset))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 11
                // Dst: (LDW_2rus:i32 GRRegs:i32:$addr, (div4_xform:i32 (imm:i32)<<P:Predicate_immUs4>>:$offset))
/*514*/       /*Scope*/ 39, /*->554*/
/*515*/         OPC_CheckType, MVT::i32,
/*517*/         OPC_MoveParent,
/*518*/         OPC_CheckPredicate, 4, // Predicate_unindexedload
/*520*/         OPC_CheckType, MVT::i32,
/*522*/         OPC_Scope, 14, /*->538*/ // 2 children in Scope
/*524*/           OPC_CheckPredicate, 10, // Predicate_zextload
/*526*/           OPC_CheckPredicate, 11, // Predicate_zextloadi8
/*528*/           OPC_EmitMergeInputChains1_0,
/*529*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD8U_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                  // Src: (ld:i32 (add:i32 GRRegs:i32:$addr, GRRegs:i32:$offset))<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 7
                  // Dst: (LD8U_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*538*/         /*Scope*/ 14, /*->553*/
/*539*/           OPC_CheckPredicate, 7, // Predicate_extload
/*541*/           OPC_CheckPredicate, 12, // Predicate_extloadi8
/*543*/           OPC_EmitMergeInputChains1_0,
/*544*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD8U_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                  // Src: (ld:i32 (add:i32 GRRegs:i32:$addr, GRRegs:i32:$offset))<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 7
                  // Dst: (LD8U_3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*553*/         0, /*End of Scope*/
/*554*/       0, /*End of Scope*/
/*555*/     /*Scope*/ 2|128,1/*130*/, /*->687*/
/*557*/       OPC_RecordChild1, // #1 = $addr
/*558*/       OPC_CheckChild1Type, MVT::i32,
/*560*/       OPC_CheckPredicate, 4, // Predicate_unindexedload
/*562*/       OPC_CheckType, MVT::i32,
/*564*/       OPC_Scope, 15, /*->581*/ // 4 children in Scope
/*566*/         OPC_CheckPredicate, 9, // Predicate_load
/*568*/         OPC_EmitMergeInputChains1_0,
/*569*/         OPC_EmitInteger, MVT::i32, 0, 
/*572*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDW_2rus), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
                // Src: (ld:i32 GRRegs:i32:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 4
                // Dst: (LDW_2rus:i32 GRRegs:i32:$addr, 0:i32)
/*581*/       /*Scope*/ 25, /*->607*/
/*582*/         OPC_CheckPredicate, 10, // Predicate_zextload
/*584*/         OPC_CheckPredicate, 11, // Predicate_zextloadi8
/*586*/         OPC_EmitMergeInputChains1_0,
/*587*/         OPC_EmitInteger, MVT::i32, 0, 
/*590*/         OPC_EmitNode, TARGET_OPCODE(XCore::LDC_ru6), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2,  // Results = #3 
/*598*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD8U_3r), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 3, 
                // Src: (ld:i32 GRRegs:i32:$addr)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 4
                // Dst: (LD8U_3r:i32 GRRegs:i32:$addr, (LDC_ru6:i32 0:i32))
/*607*/       /*Scope*/ 25, /*->633*/
/*608*/         OPC_CheckPredicate, 5, // Predicate_sextload
/*610*/         OPC_CheckPredicate, 6, // Predicate_sextloadi16
/*612*/         OPC_EmitMergeInputChains1_0,
/*613*/         OPC_EmitInteger, MVT::i32, 0, 
/*616*/         OPC_EmitNode, TARGET_OPCODE(XCore::LDC_ru6), 0,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2,  // Results = #3 
/*624*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD16S_3r), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 3, 
                // Src: (ld:i32 GRRegs:i32:$addr)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi16>> - Complexity = 4
                // Dst: (LD16S_3r:i32 GRRegs:i32:$addr, (LDC_ru6:i32 0:i32))
/*633*/       /*Scope*/ 52, /*->686*/
/*634*/         OPC_CheckPredicate, 7, // Predicate_extload
/*636*/         OPC_Scope, 23, /*->661*/ // 2 children in Scope
/*638*/           OPC_CheckPredicate, 12, // Predicate_extloadi8
/*640*/           OPC_EmitMergeInputChains1_0,
/*641*/           OPC_EmitInteger, MVT::i32, 0, 
/*644*/           OPC_EmitNode, TARGET_OPCODE(XCore::LDC_ru6), 0,
                      1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2,  // Results = #3 
/*652*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD8U_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 3, 
                  // Src: (ld:i32 GRRegs:i32:$addr)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 4
                  // Dst: (LD8U_3r:i32 GRRegs:i32:$addr, (LDC_ru6:i32 0:i32))
/*661*/         /*Scope*/ 23, /*->685*/
/*662*/           OPC_CheckPredicate, 8, // Predicate_extloadi16
/*664*/           OPC_EmitMergeInputChains1_0,
/*665*/           OPC_EmitInteger, MVT::i32, 0, 
/*668*/           OPC_EmitNode, TARGET_OPCODE(XCore::LDC_ru6), 0,
                      1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2,  // Results = #3 
/*676*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::LD16S_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 3, 
                  // Src: (ld:i32 GRRegs:i32:$addr)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 4
                  // Dst: (LD16S_3r:i32 GRRegs:i32:$addr, (LDC_ru6:i32 0:i32))
/*685*/         0, /*End of Scope*/
/*686*/       0, /*End of Scope*/
/*687*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 52|128,2/*308*/,  TARGET_OPCODE(ISD::STORE),// ->1000
/*692*/     OPC_RecordMemRef,
/*693*/     OPC_RecordNode,   // #0 = 'st' chained node
/*694*/     OPC_RecordChild1, // #1 = $val
/*695*/     OPC_CheckChild1Type, MVT::i32,
/*697*/     OPC_Scope, 124, /*->823*/ // 4 children in Scope
/*699*/       OPC_MoveChild, 2,
/*701*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::ADD),
/*704*/       OPC_Scope, 57, /*->763*/ // 2 children in Scope
/*706*/         OPC_RecordChild0, // #2 = $addr
/*707*/         OPC_MoveChild, 1,
/*709*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*712*/         OPC_RecordChild0, // #3 = $offset
/*713*/         OPC_MoveChild, 1,
/*715*/         OPC_Scope, 23, /*->740*/ // 2 children in Scope
/*717*/           OPC_CheckInteger, 1, 
/*719*/           OPC_MoveParent,
/*720*/           OPC_MoveParent,
/*721*/           OPC_CheckType, MVT::i32,
/*723*/           OPC_MoveParent,
/*724*/           OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*726*/           OPC_CheckPredicate, 14, // Predicate_truncstore
/*728*/           OPC_CheckPredicate, 15, // Predicate_truncstorei16
/*730*/           OPC_EmitMergeInputChains1_0,
/*731*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::ST16_l3r), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 2, 3, 
                  // Src: (st GRRegs:i32:$val, (add:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 1:i32)))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 15
                  // Dst: (ST16_l3r GRRegs:i32:$val, GRRegs:i32:$addr, GRRegs:i32:$offset)
/*740*/         /*Scope*/ 21, /*->762*/
/*741*/           OPC_CheckInteger, 2, 
/*743*/           OPC_MoveParent,
/*744*/           OPC_MoveParent,
/*745*/           OPC_CheckType, MVT::i32,
/*747*/           OPC_MoveParent,
/*748*/           OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*750*/           OPC_CheckPredicate, 16, // Predicate_store
/*752*/           OPC_EmitMergeInputChains1_0,
/*753*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::STW_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 2, 3, 
                  // Src: (st GRRegs:i32:$val, (add:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 2:i32)))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 15
                  // Dst: (STW_3r GRRegs:i32:$val, GRRegs:i32:$addr, GRRegs:i32:$offset)
/*762*/         0, /*End of Scope*/
/*763*/       /*Scope*/ 58, /*->822*/
/*764*/         OPC_MoveChild, 0,
/*766*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*769*/         OPC_RecordChild0, // #2 = $offset
/*770*/         OPC_MoveChild, 1,
/*772*/         OPC_Scope, 24, /*->798*/ // 2 children in Scope
/*774*/           OPC_CheckInteger, 1, 
/*776*/           OPC_MoveParent,
/*777*/           OPC_MoveParent,
/*778*/           OPC_RecordChild1, // #3 = $addr
/*779*/           OPC_CheckType, MVT::i32,
/*781*/           OPC_MoveParent,
/*782*/           OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*784*/           OPC_CheckPredicate, 14, // Predicate_truncstore
/*786*/           OPC_CheckPredicate, 15, // Predicate_truncstorei16
/*788*/           OPC_EmitMergeInputChains1_0,
/*789*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::ST16_l3r), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 2, 
                  // Src: (st GRRegs:i32:$val, (add:i32 (shl:i32 GRRegs:i32:$offset, 1:i32), GRRegs:i32:$addr))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 15
                  // Dst: (ST16_l3r GRRegs:i32:$val, GRRegs:i32:$addr, GRRegs:i32:$offset)
/*798*/         /*Scope*/ 22, /*->821*/
/*799*/           OPC_CheckInteger, 2, 
/*801*/           OPC_MoveParent,
/*802*/           OPC_MoveParent,
/*803*/           OPC_RecordChild1, // #3 = $addr
/*804*/           OPC_CheckType, MVT::i32,
/*806*/           OPC_MoveParent,
/*807*/           OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*809*/           OPC_CheckPredicate, 16, // Predicate_store
/*811*/           OPC_EmitMergeInputChains1_0,
/*812*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::STW_3r), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 3, 2, 
                  // Src: (st GRRegs:i32:$val, (add:i32 (shl:i32 GRRegs:i32:$offset, 2:i32), GRRegs:i32:$addr))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 15
                  // Dst: (STW_3r GRRegs:i32:$val, GRRegs:i32:$addr, GRRegs:i32:$offset)
/*821*/         0, /*End of Scope*/
/*822*/       0, /*End of Scope*/
/*823*/     /*Scope*/ 37, /*->861*/
/*824*/       OPC_RecordChild2, // #2 = $addr
/*825*/       OPC_CheckChild2Type, MVT::i32,
/*827*/       OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*829*/       OPC_CheckPredicate, 16, // Predicate_store
/*831*/       OPC_Scope, 13, /*->846*/ // 2 children in Scope
/*833*/         OPC_CheckComplexPat, /*CP*/0, /*#*/2, // SelectADDRspii:$addr #3 #4
/*836*/         OPC_EmitMergeInputChains1_0,
/*837*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::STWFI), 0|OPFL_Chain|OPFL_MemRefs,
                    0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                // Src: (st GRRegs:i32:$src, ADDRspii:i32:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                // Dst: (STWFI GRRegs:i32:$src, ADDRspii:i32:$addr)
/*846*/       /*Scope*/ 13, /*->860*/
/*847*/         OPC_CheckComplexPat, /*CP*/1, /*#*/2, // SelectADDRdpii:$addr #3 #4
/*850*/         OPC_EmitMergeInputChains1_0,
/*851*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::STWDP_lru6), 0|OPFL_Chain|OPFL_MemRefs,
                    0/*#VTs*/, 3/*#Ops*/, 1, 3, 4, 
                // Src: (st GRRegs:i32:$val, ADDRdpii:i32:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 13
                // Dst: (STWDP_lru6 GRRegs:i32:$val, ADDRdpii:i32:$addr)
/*860*/       0, /*End of Scope*/
/*861*/     /*Scope*/ 60, /*->922*/
/*862*/       OPC_MoveChild, 2,
/*864*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::ADD),
/*867*/       OPC_RecordChild0, // #2 = $addr
/*868*/       OPC_RecordChild1, // #3 = $offset
/*869*/       OPC_Scope, 30, /*->901*/ // 2 children in Scope
/*871*/         OPC_MoveChild, 1,
/*873*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*876*/         OPC_CheckPredicate, 1, // Predicate_immUs4
/*878*/         OPC_MoveParent,
/*879*/         OPC_CheckType, MVT::i32,
/*881*/         OPC_MoveParent,
/*882*/         OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*884*/         OPC_CheckPredicate, 16, // Predicate_store
/*886*/         OPC_EmitMergeInputChains1_0,
/*887*/         OPC_EmitConvertToTarget, 3,
/*889*/         OPC_EmitNodeXForm, 0, 4, // div4_xform
/*892*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::STW_2rus), 0|OPFL_Chain|OPFL_MemRefs,
                    0/*#VTs*/, 3/*#Ops*/, 1, 2, 5, 
                // Src: (st GRRegs:i32:$val, (add:i32 GRRegs:i32:$addr, (imm:i32)<<P:Predicate_immUs4>>:$offset))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 11
                // Dst: (STW_2rus GRRegs:i32:$val, GRRegs:i32:$addr, (div4_xform:i32 (imm:i32)<<P:Predicate_immUs4>>:$offset))
/*901*/       /*Scope*/ 19, /*->921*/
/*902*/         OPC_CheckType, MVT::i32,
/*904*/         OPC_MoveParent,
/*905*/         OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*907*/         OPC_CheckPredicate, 14, // Predicate_truncstore
/*909*/         OPC_CheckPredicate, 17, // Predicate_truncstorei8
/*911*/         OPC_EmitMergeInputChains1_0,
/*912*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::ST8_l3r), 0|OPFL_Chain|OPFL_MemRefs,
                    0/*#VTs*/, 3/*#Ops*/, 1, 2, 3, 
                // Src: (st GRRegs:i32:$val, (add:i32 GRRegs:i32:$addr, GRRegs:i32:$offset))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 7
                // Dst: (ST8_l3r GRRegs:i32:$val, GRRegs:i32:$addr, GRRegs:i32:$offset)
/*921*/       0, /*End of Scope*/
/*922*/     /*Scope*/ 76, /*->999*/
/*923*/       OPC_RecordChild2, // #2 = $addr
/*924*/       OPC_CheckChild2Type, MVT::i32,
/*926*/       OPC_CheckPredicate, 13, // Predicate_unindexedstore
/*928*/       OPC_Scope, 15, /*->945*/ // 2 children in Scope
/*930*/         OPC_CheckPredicate, 16, // Predicate_store
/*932*/         OPC_EmitMergeInputChains1_0,
/*933*/         OPC_EmitInteger, MVT::i32, 0, 
/*936*/         OPC_MorphNodeTo, TARGET_OPCODE(XCore::STW_2rus), 0|OPFL_Chain|OPFL_MemRefs,
                    0/*#VTs*/, 3/*#Ops*/, 1, 2, 3, 
                // Src: (st GRRegs:i32:$val, GRRegs:i32:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 4
                // Dst: (STW_2rus GRRegs:i32:$val, GRRegs:i32:$addr, 0:i32)
/*945*/       /*Scope*/ 52, /*->998*/
/*946*/         OPC_CheckPredicate, 14, // Predicate_truncstore
/*948*/         OPC_Scope, 23, /*->973*/ // 2 children in Scope
/*950*/           OPC_CheckPredicate, 17, // Predicate_truncstorei8
/*952*/           OPC_EmitMergeInputChains1_0,
/*953*/           OPC_EmitInteger, MVT::i32, 0, 
/*956*/           OPC_EmitNode, TARGET_OPCODE(XCore::LDC_ru6), 0,
                      1/*#VTs*/, MVT::i32, 1/*#Ops*/, 3,  // Results = #4 
/*964*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::ST8_l3r), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 2, 4, 
                  // Src: (st GRRegs:i32:$val, GRRegs:i32:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 4
                  // Dst: (ST8_l3r GRRegs:i32:$val, GRRegs:i32:$addr, (LDC_ru6:i32 0:i32))
/*973*/         /*Scope*/ 23, /*->997*/
/*974*/           OPC_CheckPredicate, 15, // Predicate_truncstorei16
/*976*/           OPC_EmitMergeInputChains1_0,
/*977*/           OPC_EmitInteger, MVT::i32, 0, 
/*980*/           OPC_EmitNode, TARGET_OPCODE(XCore::LDC_ru6), 0,
                      1/*#VTs*/, MVT::i32, 1/*#Ops*/, 3,  // Results = #4 
/*988*/           OPC_MorphNodeTo, TARGET_OPCODE(XCore::ST16_l3r), 0|OPFL_Chain|OPFL_MemRefs,
                      0/*#VTs*/, 3/*#Ops*/, 1, 2, 4, 
                  // Src: (st GRRegs:i32:$val, GRRegs:i32:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 4
                  // Dst: (ST16_l3r GRRegs:i32:$val, GRRegs:i32:$addr, (LDC_ru6:i32 0:i32))
/*997*/         0, /*End of Scope*/
/*998*/       0, /*End of Scope*/
/*999*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 94,  TARGET_OPCODE(ISD::SRA),// ->1097
/*1003*/    OPC_Scope, 35, /*->1040*/ // 2 children in Scope
/*1005*/      OPC_MoveChild, 0,
/*1007*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*1010*/      OPC_RecordChild0, // #0 = $src
/*1011*/      OPC_RecordChild1, // #1 = $imm
/*1012*/      OPC_MoveChild, 1,
/*1014*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1017*/      OPC_CheckPredicate, 18, // Predicate_immBpwSubBitp
/*1019*/      OPC_MoveParent,
/*1020*/      OPC_MoveParent,
/*1021*/      OPC_MoveChild, 1,
/*1023*/      OPC_CheckSame, 1,
/*1025*/      OPC_MoveParent,
/*1026*/      OPC_EmitConvertToTarget, 1,
/*1028*/      OPC_EmitNodeXForm, 3, 2, // bpwsub_xform
/*1031*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SEXT_rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
              // Src: (sra:i32 (shl:i32 GRRegs:i32:$src, (imm:i32)<<P:Predicate_immBpwSubBitp>>:$imm), (imm:i32)<<P:Predicate_immBpwSubBitp>>:$imm) - Complexity = 14
              // Dst: (SEXT_rus:i32 GRRegs:i32:$src, (bpwsub_xform:i32 (imm:i32)<<P:Predicate_immBpwSubBitp>>:$imm))
/*1040*/    /*Scope*/ 55, /*->1096*/
/*1041*/      OPC_RecordChild0, // #0 = $src
/*1042*/      OPC_Scope, 17, /*->1061*/ // 2 children in Scope
/*1044*/        OPC_MoveChild, 1,
/*1046*/        OPC_CheckInteger, 31, 
/*1048*/        OPC_MoveParent,
/*1049*/        OPC_EmitInteger, MVT::i32, 32, 
/*1052*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::ASHR_l2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (sra:i32 GRRegs:i32:$src, 31:i32) - Complexity = 8
                // Dst: (ASHR_l2rus:i32 GRRegs:i32:$src, 32:i32)
/*1061*/      /*Scope*/ 33, /*->1095*/
/*1062*/        OPC_RecordChild1, // #1 = $c
/*1063*/        OPC_Scope, 19, /*->1084*/ // 2 children in Scope
/*1065*/          OPC_MoveChild, 1,
/*1067*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1070*/          OPC_CheckPredicate, 19, // Predicate_immBitp
/*1072*/          OPC_MoveParent,
/*1073*/          OPC_EmitConvertToTarget, 1,
/*1075*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::ASHR_l2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                  // Src: (sra:i32 GRRegs:i32:$b, (imm:i32)<<P:Predicate_immBitp>>:$c) - Complexity = 7
                  // Dst: (ASHR_l2rus:i32 GRRegs:i32:$b, (imm:i32):$c)
/*1084*/        /*Scope*/ 9, /*->1094*/
/*1085*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::ASHR_l3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                  // Src: (sra:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
                  // Dst: (ASHR_l3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*1094*/        0, /*End of Scope*/
/*1095*/      0, /*End of Scope*/
/*1096*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 113,  TARGET_OPCODE(ISD::SUB),// ->1213
/*1100*/    OPC_Scope, 39, /*->1141*/ // 3 children in Scope
/*1102*/      OPC_RecordChild0, // #0 = $addr
/*1103*/      OPC_MoveChild, 1,
/*1105*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*1108*/      OPC_RecordChild0, // #1 = $offset
/*1109*/      OPC_MoveChild, 1,
/*1111*/      OPC_Scope, 13, /*->1126*/ // 2 children in Scope
/*1113*/        OPC_CheckInteger, 2, 
/*1115*/        OPC_MoveParent,
/*1116*/        OPC_MoveParent,
/*1117*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWB_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (sub:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 2:i32)) - Complexity = 11
                // Dst: (LDAWB_l3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*1126*/      /*Scope*/ 13, /*->1140*/
/*1127*/        OPC_CheckInteger, 1, 
/*1129*/        OPC_MoveParent,
/*1130*/        OPC_MoveParent,
/*1131*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDA16B_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (sub:i32 GRRegs:i32:$addr, (shl:i32 GRRegs:i32:$offset, 1:i32)) - Complexity = 11
                // Dst: (LDA16B_l3r:i32 GRRegs:i32:$addr, GRRegs:i32:$offset)
/*1140*/      0, /*End of Scope*/
/*1141*/    /*Scope*/ 14, /*->1156*/
/*1142*/      OPC_MoveChild, 0,
/*1144*/      OPC_CheckInteger, 0, 
/*1146*/      OPC_MoveParent,
/*1147*/      OPC_RecordChild1, // #0 = $b
/*1148*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::NEG), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (sub:i32 0:i32, GRRegs:i32:$b) - Complexity = 8
              // Dst: (NEG:i32 GRRegs:i32:$b)
/*1156*/    /*Scope*/ 55, /*->1212*/
/*1157*/      OPC_RecordChild0, // #0 = $b
/*1158*/      OPC_RecordChild1, // #1 = $c
/*1159*/      OPC_Scope, 40, /*->1201*/ // 2 children in Scope
/*1161*/        OPC_MoveChild, 1,
/*1163*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1166*/        OPC_Scope, 14, /*->1182*/ // 2 children in Scope
/*1168*/          OPC_CheckPredicate, 0, // Predicate_immUs
/*1170*/          OPC_MoveParent,
/*1171*/          OPC_EmitConvertToTarget, 1,
/*1173*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::SUB_2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                  // Src: (sub:i32 GRRegs:i32:$b, (imm:i32)<<P:Predicate_immUs>>:$c) - Complexity = 7
                  // Dst: (SUB_2rus:i32 GRRegs:i32:$b, (imm:i32):$c)
/*1182*/        /*Scope*/ 17, /*->1200*/
/*1183*/          OPC_CheckPredicate, 1, // Predicate_immUs4
/*1185*/          OPC_MoveParent,
/*1186*/          OPC_EmitConvertToTarget, 1,
/*1188*/          OPC_EmitNodeXForm, 0, 2, // div4_xform
/*1191*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWB_l2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                  // Src: (sub:i32 GRRegs:i32:$addr, (imm:i32)<<P:Predicate_immUs4>>:$offset) - Complexity = 7
                  // Dst: (LDAWB_l2rus:i32 GRRegs:i32:$addr, (div4_xform:i32 (imm:i32)<<P:Predicate_immUs4>>:$offset))
/*1200*/        0, /*End of Scope*/
/*1201*/      /*Scope*/ 9, /*->1211*/
/*1202*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::SUB_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (sub:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
                // Dst: (SUB_3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*1211*/      0, /*End of Scope*/
/*1212*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 104,  TARGET_OPCODE(ISD::AND),// ->1320
/*1216*/    OPC_Scope, 31, /*->1249*/ // 3 children in Scope
/*1218*/      OPC_RecordChild0, // #0 = $src1
/*1219*/      OPC_MoveChild, 1,
/*1221*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*1224*/      OPC_RecordChild0, // #1 = $src2
/*1225*/      OPC_MoveChild, 1,
/*1227*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*1238*/      OPC_MoveParent,
/*1239*/      OPC_MoveParent,
/*1240*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::ANDNOT_2r), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (and:i32 GRRegs:i32:$src1, (xor:i32 GRRegs:i32:$src2, -1:i32)) - Complexity = 11
              // Dst: (ANDNOT_2r:i32 GRRegs:i32:$src1, GRRegs:i32:$src2)
/*1249*/    /*Scope*/ 31, /*->1281*/
/*1250*/      OPC_MoveChild, 0,
/*1252*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*1255*/      OPC_RecordChild0, // #0 = $src2
/*1256*/      OPC_MoveChild, 1,
/*1258*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*1269*/      OPC_MoveParent,
/*1270*/      OPC_MoveParent,
/*1271*/      OPC_RecordChild1, // #1 = $src1
/*1272*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::ANDNOT_2r), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
              // Src: (and:i32 (xor:i32 GRRegs:i32:$src2, -1:i32), GRRegs:i32:$src1) - Complexity = 11
              // Dst: (ANDNOT_2r:i32 GRRegs:i32:$src1, GRRegs:i32:$src2)
/*1281*/    /*Scope*/ 37, /*->1319*/
/*1282*/      OPC_RecordChild0, // #0 = $val
/*1283*/      OPC_RecordChild1, // #1 = $mask
/*1284*/      OPC_Scope, 22, /*->1308*/ // 2 children in Scope
/*1286*/        OPC_MoveChild, 1,
/*1288*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1291*/        OPC_CheckPredicate, 20, // Predicate_immMskBitp
/*1293*/        OPC_MoveParent,
/*1294*/        OPC_EmitConvertToTarget, 1,
/*1296*/        OPC_EmitNodeXForm, 4, 2, // msksize_xform
/*1299*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::ZEXT_rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3, 
                // Src: (and:i32 GRRegs:i32:$val, (imm:i32)<<P:Predicate_immMskBitp>>:$mask) - Complexity = 7
                // Dst: (ZEXT_rus:i32 GRRegs:i32:$val, (msksize_xform:i32 (imm:i32)<<P:Predicate_immMskBitp>>:$mask))
/*1308*/      /*Scope*/ 9, /*->1318*/
/*1309*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::AND_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (and:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
                // Dst: (AND_3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*1318*/      0, /*End of Scope*/
/*1319*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 113|128,2/*369*/,  TARGET_OPCODE(ISD::BRCOND),// ->1693
/*1324*/    OPC_RecordNode,   // #0 = 'brcond' chained node
/*1325*/    OPC_Scope, 90|128,2/*346*/, /*->1674*/ // 2 children in Scope
/*1328*/      OPC_MoveChild, 1,
/*1330*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::SETCC),
/*1333*/      OPC_RecordChild0, // #1 = $lhs
/*1334*/      OPC_Scope, 6|128,1/*134*/, /*->1471*/ // 3 children in Scope
/*1337*/        OPC_MoveChild, 1,
/*1339*/        OPC_Scope, 82, /*->1423*/ // 2 children in Scope
/*1341*/          OPC_CheckInteger, 0, 
/*1343*/          OPC_MoveParent,
/*1344*/          OPC_MoveChild, 2,
/*1346*/          OPC_Scope, 20, /*->1368*/ // 3 children in Scope
/*1348*/            OPC_CheckCondCode, ISD::SETNE,
/*1350*/            OPC_MoveParent,
/*1351*/            OPC_MoveParent,
/*1352*/            OPC_RecordChild2, // #2 = $dst
/*1353*/            OPC_MoveChild, 2,
/*1355*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1358*/            OPC_MoveParent,
/*1359*/            OPC_EmitMergeInputChains1_0,
/*1360*/            OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFT_lru6), 0|OPFL_Chain,
                        0/*#VTs*/, 2/*#Ops*/, 1, 2, 
                    // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, 0:i32, SETNE:Other), (bb:Other):$dst) - Complexity = 11
                    // Dst: (BRFT_lru6 GRRegs:i32:$lhs, (bb:Other):$dst)
/*1368*/          /*Scope*/ 20, /*->1389*/
/*1369*/            OPC_CheckCondCode, ISD::SETEQ,
/*1371*/            OPC_MoveParent,
/*1372*/            OPC_MoveParent,
/*1373*/            OPC_RecordChild2, // #2 = $dst
/*1374*/            OPC_MoveChild, 2,
/*1376*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1379*/            OPC_MoveParent,
/*1380*/            OPC_EmitMergeInputChains1_0,
/*1381*/            OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                        0/*#VTs*/, 2/*#Ops*/, 1, 2, 
                    // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, 0:i32, SETEQ:Other), (bb:Other):$dst) - Complexity = 11
                    // Dst: (BRFF_lru6 GRRegs:i32:$lhs, (bb:Other):$dst)
/*1389*/          /*Scope*/ 32, /*->1422*/
/*1390*/            OPC_CheckCondCode, ISD::SETLT,
/*1392*/            OPC_MoveParent,
/*1393*/            OPC_MoveParent,
/*1394*/            OPC_RecordChild2, // #2 = $dst
/*1395*/            OPC_MoveChild, 2,
/*1397*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1400*/            OPC_MoveParent,
/*1401*/            OPC_EmitMergeInputChains1_0,
/*1402*/            OPC_EmitInteger, MVT::i32, 32, 
/*1405*/            OPC_EmitNode, TARGET_OPCODE(XCore::ASHR_l2rus), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 3,  // Results = #4 
/*1414*/            OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFT_lru6), 0|OPFL_Chain,
                        0/*#VTs*/, 2/*#Ops*/, 4, 2, 
                    // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, 0:i32, SETLT:Other), (bb:Other):$dst) - Complexity = 11
                    // Dst: (BRFT_lru6 (ASHR_l2rus:i32 GRRegs:i32:$lhs, 32:i32), (bb:Other):$dst)
/*1422*/          0, /*End of Scope*/
/*1423*/        /*Scope*/ 46, /*->1470*/
/*1424*/          OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*1435*/          OPC_MoveParent,
/*1436*/          OPC_MoveChild, 2,
/*1438*/          OPC_CheckCondCode, ISD::SETGT,
/*1440*/          OPC_MoveParent,
/*1441*/          OPC_MoveParent,
/*1442*/          OPC_RecordChild2, // #2 = $dst
/*1443*/          OPC_MoveChild, 2,
/*1445*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1448*/          OPC_MoveParent,
/*1449*/          OPC_EmitMergeInputChains1_0,
/*1450*/          OPC_EmitInteger, MVT::i32, 32, 
/*1453*/          OPC_EmitNode, TARGET_OPCODE(XCore::ASHR_l2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 3,  // Results = #4 
/*1462*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 4, 2, 
                  // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, -1:i32, SETGT:Other), (bb:Other):$dst) - Complexity = 11
                  // Dst: (BRFF_lru6 (ASHR_l2rus:i32 GRRegs:i32:$lhs, 32:i32), (bb:Other):$dst)
/*1470*/        0, /*End of Scope*/
/*1471*/      /*Scope*/ 42, /*->1514*/
/*1472*/        OPC_RecordChild1, // #2 = $rhs
/*1473*/        OPC_MoveChild, 1,
/*1475*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1478*/        OPC_CheckPredicate, 0, // Predicate_immUs
/*1480*/        OPC_MoveParent,
/*1481*/        OPC_MoveChild, 2,
/*1483*/        OPC_CheckCondCode, ISD::SETNE,
/*1485*/        OPC_MoveParent,
/*1486*/        OPC_MoveParent,
/*1487*/        OPC_RecordChild2, // #3 = $dst
/*1488*/        OPC_MoveChild, 2,
/*1490*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1493*/        OPC_MoveParent,
/*1494*/        OPC_EmitMergeInputChains1_0,
/*1495*/        OPC_EmitConvertToTarget, 2,
/*1497*/        OPC_EmitNode, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 4,  // Results = #5 
/*1506*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                    0/*#VTs*/, 2/*#Ops*/, 5, 3, 
                // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs, SETNE:Other), (bb:Other):$dst) - Complexity = 10
                // Dst: (BRFF_lru6 (EQ_2rus:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs), (bb:Other):$dst)
/*1514*/      /*Scope*/ 29|128,1/*157*/, /*->1673*/
/*1516*/        OPC_CheckChild0Type, MVT::i32,
/*1518*/        OPC_RecordChild1, // #2 = $rhs
/*1519*/        OPC_MoveChild, 2,
/*1521*/        OPC_Scope, 29, /*->1552*/ // 5 children in Scope
/*1523*/          OPC_CheckCondCode, ISD::SETLE,
/*1525*/          OPC_MoveParent,
/*1526*/          OPC_MoveParent,
/*1527*/          OPC_RecordChild2, // #3 = $dst
/*1528*/          OPC_MoveChild, 2,
/*1530*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1533*/          OPC_MoveParent,
/*1534*/          OPC_EmitMergeInputChains1_0,
/*1535*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSS_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 1,  // Results = #4 
/*1544*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 4, 3, 
                  // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETLE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BRFF_lru6 (LSS_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs), (bb:Other):$dst)
/*1552*/        /*Scope*/ 29, /*->1582*/
/*1553*/          OPC_CheckCondCode, ISD::SETULE,
/*1555*/          OPC_MoveParent,
/*1556*/          OPC_MoveParent,
/*1557*/          OPC_RecordChild2, // #3 = $dst
/*1558*/          OPC_MoveChild, 2,
/*1560*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1563*/          OPC_MoveParent,
/*1564*/          OPC_EmitMergeInputChains1_0,
/*1565*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSU_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 1,  // Results = #4 
/*1574*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 4, 3, 
                  // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETULE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BRFF_lru6 (LSU_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs), (bb:Other):$dst)
/*1582*/        /*Scope*/ 29, /*->1612*/
/*1583*/          OPC_CheckCondCode, ISD::SETGE,
/*1585*/          OPC_MoveParent,
/*1586*/          OPC_MoveParent,
/*1587*/          OPC_RecordChild2, // #3 = $dst
/*1588*/          OPC_MoveChild, 2,
/*1590*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1593*/          OPC_MoveParent,
/*1594*/          OPC_EmitMergeInputChains1_0,
/*1595*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSS_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2,  // Results = #4 
/*1604*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 4, 3, 
                  // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETGE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BRFF_lru6 (LSS_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), (bb:Other):$dst)
/*1612*/        /*Scope*/ 29, /*->1642*/
/*1613*/          OPC_CheckCondCode, ISD::SETUGE,
/*1615*/          OPC_MoveParent,
/*1616*/          OPC_MoveParent,
/*1617*/          OPC_RecordChild2, // #3 = $dst
/*1618*/          OPC_MoveChild, 2,
/*1620*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1623*/          OPC_MoveParent,
/*1624*/          OPC_EmitMergeInputChains1_0,
/*1625*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSU_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2,  // Results = #4 
/*1634*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 4, 3, 
                  // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETUGE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BRFF_lru6 (LSU_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), (bb:Other):$dst)
/*1642*/        /*Scope*/ 29, /*->1672*/
/*1643*/          OPC_CheckCondCode, ISD::SETNE,
/*1645*/          OPC_MoveParent,
/*1646*/          OPC_MoveParent,
/*1647*/          OPC_RecordChild2, // #3 = $dst
/*1648*/          OPC_MoveChild, 2,
/*1650*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1653*/          OPC_MoveParent,
/*1654*/          OPC_EmitMergeInputChains1_0,
/*1655*/          OPC_EmitNode, TARGET_OPCODE(XCore::EQ_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2,  // Results = #4 
/*1664*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFF_lru6), 0|OPFL_Chain,
                      0/*#VTs*/, 2/*#Ops*/, 4, 3, 
                  // Src: (brcond (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETNE:Other), (bb:Other):$dst) - Complexity = 6
                  // Dst: (BRFF_lru6 (EQ_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), (bb:Other):$dst)
/*1672*/        0, /*End of Scope*/
/*1673*/      0, /*End of Scope*/
/*1674*/    /*Scope*/ 17, /*->1692*/
/*1675*/      OPC_RecordChild1, // #1 = $cond
/*1676*/      OPC_RecordChild2, // #2 = $addr
/*1677*/      OPC_MoveChild, 2,
/*1679*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*1682*/      OPC_MoveParent,
/*1683*/      OPC_EmitMergeInputChains1_0,
/*1684*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFT_lru6), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 1, 2, 
              // Src: (brcond GRRegs:i32:$cond, (bb:Other):$addr) - Complexity = 3
              // Dst: (BRFT_lru6 GRRegs:i32:$cond, (bb:Other):$addr)
/*1692*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 89|128,2/*345*/,  TARGET_OPCODE(ISD::SELECT),// ->2042
/*1697*/    OPC_Scope, 69|128,2/*325*/, /*->2025*/ // 2 children in Scope
/*1700*/      OPC_MoveChild, 0,
/*1702*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::SETCC),
/*1705*/      OPC_RecordChild0, // #0 = $lhs
/*1706*/      OPC_Scope, 126, /*->1834*/ // 3 children in Scope
/*1708*/        OPC_MoveChild, 1,
/*1710*/        OPC_Scope, 76, /*->1788*/ // 2 children in Scope
/*1712*/          OPC_CheckInteger, 0, 
/*1714*/          OPC_MoveParent,
/*1715*/          OPC_MoveChild, 2,
/*1717*/          OPC_Scope, 18, /*->1737*/ // 3 children in Scope
/*1719*/            OPC_CheckCondCode, ISD::SETNE,
/*1721*/            OPC_MoveParent,
/*1722*/            OPC_MoveParent,
/*1723*/            OPC_RecordChild1, // #1 = $T
/*1724*/            OPC_RecordChild2, // #2 = $F
/*1725*/            OPC_CheckType, MVT::i32,
/*1727*/            OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                        1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 1, 2, 
                    // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, 0:i32, SETNE:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 11
                    // Dst: (SELECT_CC:i32 GRRegs:i32:$lhs, GRRegs:i32:$T, GRRegs:i32:$F)
/*1737*/          /*Scope*/ 18, /*->1756*/
/*1738*/            OPC_CheckCondCode, ISD::SETEQ,
/*1740*/            OPC_MoveParent,
/*1741*/            OPC_MoveParent,
/*1742*/            OPC_RecordChild1, // #1 = $T
/*1743*/            OPC_RecordChild2, // #2 = $F
/*1744*/            OPC_CheckType, MVT::i32,
/*1746*/            OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                        1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 2, 1, 
                    // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, 0:i32, SETEQ:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 11
                    // Dst: (SELECT_CC:i32 GRRegs:i32:$lhs, GRRegs:i32:$F, GRRegs:i32:$T)
/*1756*/          /*Scope*/ 30, /*->1787*/
/*1757*/            OPC_CheckCondCode, ISD::SETLT,
/*1759*/            OPC_MoveParent,
/*1760*/            OPC_MoveParent,
/*1761*/            OPC_RecordChild1, // #1 = $T
/*1762*/            OPC_RecordChild2, // #2 = $F
/*1763*/            OPC_CheckType, MVT::i32,
/*1765*/            OPC_EmitInteger, MVT::i32, 32, 
/*1768*/            OPC_EmitNode, TARGET_OPCODE(XCore::ASHR_l2rus), 0,
                        1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3,  // Results = #4 
/*1777*/            OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                        1/*#VTs*/, MVT::i32, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, 0:i32, SETLT:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 11
                    // Dst: (SELECT_CC:i32 (ASHR_l2rus:i32 GRRegs:i32:$lhs, 32:i32), GRRegs:i32:$T, GRRegs:i32:$F)
/*1787*/          0, /*End of Scope*/
/*1788*/        /*Scope*/ 44, /*->1833*/
/*1789*/          OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*1800*/          OPC_MoveParent,
/*1801*/          OPC_MoveChild, 2,
/*1803*/          OPC_CheckCondCode, ISD::SETGT,
/*1805*/          OPC_MoveParent,
/*1806*/          OPC_MoveParent,
/*1807*/          OPC_RecordChild1, // #1 = $T
/*1808*/          OPC_RecordChild2, // #2 = $F
/*1809*/          OPC_CheckType, MVT::i32,
/*1811*/          OPC_EmitInteger, MVT::i32, 32, 
/*1814*/          OPC_EmitNode, TARGET_OPCODE(XCore::ASHR_l2rus), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 3,  // Results = #4 
/*1823*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 4, 2, 1, 
                  // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, -1:i32, SETGT:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 11
                  // Dst: (SELECT_CC:i32 (ASHR_l2rus:i32 GRRegs:i32:$lhs, 32:i32), GRRegs:i32:$F, GRRegs:i32:$T)
/*1833*/        0, /*End of Scope*/
/*1834*/      /*Scope*/ 40, /*->1875*/
/*1835*/        OPC_RecordChild1, // #1 = $rhs
/*1836*/        OPC_MoveChild, 1,
/*1838*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1841*/        OPC_CheckPredicate, 0, // Predicate_immUs
/*1843*/        OPC_MoveParent,
/*1844*/        OPC_MoveChild, 2,
/*1846*/        OPC_CheckCondCode, ISD::SETNE,
/*1848*/        OPC_MoveParent,
/*1849*/        OPC_MoveParent,
/*1850*/        OPC_RecordChild1, // #2 = $T
/*1851*/        OPC_RecordChild2, // #3 = $F
/*1852*/        OPC_CheckType, MVT::i32,
/*1854*/        OPC_EmitConvertToTarget, 1,
/*1856*/        OPC_EmitNode, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 4,  // Results = #5 
/*1865*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                    1/*#VTs*/, MVT::i32, 3/*#Ops*/, 5, 3, 2, 
                // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs, SETNE:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 10
                // Dst: (SELECT_CC:i32 (EQ_2rus:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs), GRRegs:i32:$F, GRRegs:i32:$T)
/*1875*/      /*Scope*/ 19|128,1/*147*/, /*->2024*/
/*1877*/        OPC_CheckChild0Type, MVT::i32,
/*1879*/        OPC_RecordChild1, // #1 = $rhs
/*1880*/        OPC_MoveChild, 2,
/*1882*/        OPC_Scope, 27, /*->1911*/ // 5 children in Scope
/*1884*/          OPC_CheckCondCode, ISD::SETLE,
/*1886*/          OPC_MoveParent,
/*1887*/          OPC_MoveParent,
/*1888*/          OPC_RecordChild1, // #2 = $T
/*1889*/          OPC_RecordChild2, // #3 = $F
/*1890*/          OPC_CheckType, MVT::i32,
/*1892*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSS_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #4 
/*1901*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 4, 3, 2, 
                  // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETLE:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 6
                  // Dst: (SELECT_CC:i32 (LSS_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs), GRRegs:i32:$F, GRRegs:i32:$T)
/*1911*/        /*Scope*/ 27, /*->1939*/
/*1912*/          OPC_CheckCondCode, ISD::SETULE,
/*1914*/          OPC_MoveParent,
/*1915*/          OPC_MoveParent,
/*1916*/          OPC_RecordChild1, // #2 = $T
/*1917*/          OPC_RecordChild2, // #3 = $F
/*1918*/          OPC_CheckType, MVT::i32,
/*1920*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSU_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #4 
/*1929*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 4, 3, 2, 
                  // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETULE:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 6
                  // Dst: (SELECT_CC:i32 (LSU_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs), GRRegs:i32:$F, GRRegs:i32:$T)
/*1939*/        /*Scope*/ 27, /*->1967*/
/*1940*/          OPC_CheckCondCode, ISD::SETGE,
/*1942*/          OPC_MoveParent,
/*1943*/          OPC_MoveParent,
/*1944*/          OPC_RecordChild1, // #2 = $T
/*1945*/          OPC_RecordChild2, // #3 = $F
/*1946*/          OPC_CheckType, MVT::i32,
/*1948*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSS_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #4 
/*1957*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 4, 3, 2, 
                  // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETGE:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 6
                  // Dst: (SELECT_CC:i32 (LSS_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), GRRegs:i32:$F, GRRegs:i32:$T)
/*1967*/        /*Scope*/ 27, /*->1995*/
/*1968*/          OPC_CheckCondCode, ISD::SETUGE,
/*1970*/          OPC_MoveParent,
/*1971*/          OPC_MoveParent,
/*1972*/          OPC_RecordChild1, // #2 = $T
/*1973*/          OPC_RecordChild2, // #3 = $F
/*1974*/          OPC_CheckType, MVT::i32,
/*1976*/          OPC_EmitNode, TARGET_OPCODE(XCore::LSU_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #4 
/*1985*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 4, 3, 2, 
                  // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETUGE:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 6
                  // Dst: (SELECT_CC:i32 (LSU_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), GRRegs:i32:$F, GRRegs:i32:$T)
/*1995*/        /*Scope*/ 27, /*->2023*/
/*1996*/          OPC_CheckCondCode, ISD::SETNE,
/*1998*/          OPC_MoveParent,
/*1999*/          OPC_MoveParent,
/*2000*/          OPC_RecordChild1, // #2 = $T
/*2001*/          OPC_RecordChild2, // #3 = $F
/*2002*/          OPC_CheckType, MVT::i32,
/*2004*/          OPC_EmitNode, TARGET_OPCODE(XCore::EQ_3r), 0,
                      1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2013*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                      1/*#VTs*/, MVT::i32, 3/*#Ops*/, 4, 3, 2, 
                  // Src: (select:i32 (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETNE:Other), GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 6
                  // Dst: (SELECT_CC:i32 (EQ_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), GRRegs:i32:$F, GRRegs:i32:$T)
/*2023*/        0, /*End of Scope*/
/*2024*/      0, /*End of Scope*/
/*2025*/    /*Scope*/ 15, /*->2041*/
/*2026*/      OPC_RecordChild0, // #0 = $cond
/*2027*/      OPC_RecordChild1, // #1 = $T
/*2028*/      OPC_RecordChild2, // #2 = $F
/*2029*/      OPC_CheckType, MVT::i32,
/*2031*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SELECT_CC), 0,
                  1/*#VTs*/, MVT::i32, 3/*#Ops*/, 0, 1, 2, 
              // Src: (select:i32 GRRegs:i32:$cond, GRRegs:i32:$T, GRRegs:i32:$F) - Complexity = 3
              // Dst: (SELECT_CC:i32 GRRegs:i32:$cond, GRRegs:i32:$T, GRRegs:i32:$F)
/*2041*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::CALLSEQ_END),// ->2071
/*2045*/    OPC_RecordNode,   // #0 = 'callseq_end' chained node
/*2046*/    OPC_CaptureFlagInput,
/*2047*/    OPC_RecordChild1, // #1 = $amt1
/*2048*/    OPC_MoveChild, 1,
/*2050*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*2053*/    OPC_MoveParent,
/*2054*/    OPC_RecordChild2, // #2 = $amt2
/*2055*/    OPC_MoveChild, 2,
/*2057*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*2060*/    OPC_MoveParent,
/*2061*/    OPC_EmitMergeInputChains1_0,
/*2062*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::ADJCALLSTACKUP), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
            // Src: (callseq_end (timm:i32):$amt1, (timm:i32):$amt2) - Complexity = 9
            // Dst: (ADJCALLSTACKUP:i32 (timm:i32):$amt1, (timm:i32):$amt2)
          /*SwitchOpcode*/ 15,  TARGET_OPCODE(ISD::FrameIndex),// ->2089
/*2074*/    OPC_RecordNode,   // #0 = $addr
/*2075*/    OPC_CheckType, MVT::i32,
/*2077*/    OPC_CheckComplexPat, /*CP*/0, /*#*/0, // SelectADDRspii:$addr #1 #2
/*2080*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWFI), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
            // Src: ADDRspii:i32:$addr - Complexity = 9
            // Dst: (LDAWFI:i32 ADDRspii:i32:$addr)
          /*SwitchOpcode*/ 15,  TARGET_OPCODE(XCoreISD::DPRelativeWrapper),// ->2107
/*2092*/    OPC_RecordNode,   // #0 = $a
/*2093*/    OPC_CheckType, MVT::i32,
/*2095*/    OPC_CheckComplexPat, /*CP*/1, /*#*/0, // SelectADDRdpii:$a #1 #2
/*2098*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWDP_lru6), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
            // Src: ADDRdpii:i32:$a - Complexity = 9
            // Dst: (LDAWDP_lru6:i32 ADDRdpii:i32:$a)
          /*SwitchOpcode*/ 15,  TARGET_OPCODE(XCoreISD::CPRelativeWrapper),// ->2125
/*2110*/    OPC_RecordNode,   // #0 = $a
/*2111*/    OPC_CheckType, MVT::i32,
/*2113*/    OPC_CheckComplexPat, /*CP*/2, /*#*/0, // SelectADDRcpii:$a #1 #2
/*2116*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWCP_lu6), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 2, 
            // Src: ADDRcpii:i32:$a - Complexity = 9
            // Dst: (LDAWCP_lu6:i32 ADDRcpii:i32:$a)
          /*SwitchOpcode*/ 37,  TARGET_OPCODE(ISD::XOR),// ->2165
/*2128*/    OPC_RecordChild0, // #0 = $b
/*2129*/    OPC_Scope, 22, /*->2153*/ // 2 children in Scope
/*2131*/      OPC_MoveChild, 1,
/*2133*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*2144*/      OPC_MoveParent,
/*2145*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::NOT), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (xor:i32 GRRegs:i32:$b, -1:i32) - Complexity = 8
              // Dst: (NOT:i32 GRRegs:i32:$b)
/*2153*/    /*Scope*/ 10, /*->2164*/
/*2154*/      OPC_RecordChild1, // #1 = $c
/*2155*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::XOR_l3r), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (xor:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
              // Dst: (XOR_l3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*2164*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 30,  TARGET_OPCODE(ISD::INTRINSIC_WO_CHAIN),// ->2198
/*2168*/    OPC_MoveChild, 0,
/*2170*/    OPC_Scope, 13, /*->2185*/ // 2 children in Scope
/*2172*/      OPC_CheckInteger, 97|128,6/*865*/, 
/*2175*/      OPC_MoveParent,
/*2176*/      OPC_RecordChild1, // #0 = $src
/*2177*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::BITREV_l2r), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (intrinsic_wo_chain:i32 865:iPTR, GRRegs:i32:$src) - Complexity = 8
              // Dst: (BITREV_l2r:i32 GRRegs:i32:$src)
/*2185*/    /*Scope*/ 11, /*->2197*/
/*2186*/      OPC_CheckInteger, 98|128,6/*866*/, 
/*2189*/      OPC_MoveParent,
/*2190*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::GETID_0R), 0,
                  1/*#VTs*/, MVT::i32, 0/*#Ops*/, 
              // Src: (intrinsic_wo_chain:i32 866:iPTR) - Complexity = 8
              // Dst: (GETID_0R:i32)
/*2197*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 67,  TARGET_OPCODE(ISD::MUL),// ->2268
/*2201*/    OPC_RecordChild0, // #0 = $src
/*2202*/    OPC_Scope, 52, /*->2256*/ // 2 children in Scope
/*2204*/      OPC_MoveChild, 1,
/*2206*/      OPC_Scope, 12, /*->2220*/ // 3 children in Scope
/*2208*/        OPC_CheckInteger, 3, 
/*2210*/        OPC_MoveParent,
/*2211*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDA16F_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 0, 
                // Src: (mul:i32 GRRegs:i32:$src, 3:i32) - Complexity = 8
                // Dst: (LDA16F_l3r:i32 GRRegs:i32:$src, GRRegs:i32:$src)
/*2220*/      /*Scope*/ 12, /*->2233*/
/*2221*/        OPC_CheckInteger, 5, 
/*2223*/        OPC_MoveParent,
/*2224*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWF_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 0, 
                // Src: (mul:i32 GRRegs:i32:$src, 5:i32) - Complexity = 8
                // Dst: (LDAWF_l3r:i32 GRRegs:i32:$src, GRRegs:i32:$src)
/*2233*/      /*Scope*/ 21, /*->2255*/
/*2234*/        OPC_CheckInteger, 125|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551613*/, 
/*2245*/        OPC_MoveParent,
/*2246*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAWB_l3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 0, 
                // Src: (mul:i32 GRRegs:i32:$src, -3:i32) - Complexity = 8
                // Dst: (LDAWB_l3r:i32 GRRegs:i32:$src, GRRegs:i32:$src)
/*2255*/      0, /*End of Scope*/
/*2256*/    /*Scope*/ 10, /*->2267*/
/*2257*/      OPC_RecordChild1, // #1 = $c
/*2258*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::MUL_l3r), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (mul:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
              // Dst: (MUL_l3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*2267*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 46|128,2/*302*/,  TARGET_OPCODE(ISD::SETCC),// ->2574
/*2272*/    OPC_RecordChild0, // #0 = $lhs
/*2273*/    OPC_Scope, 43, /*->2318*/ // 3 children in Scope
/*2275*/      OPC_MoveChild, 1,
/*2277*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*2288*/      OPC_MoveParent,
/*2289*/      OPC_MoveChild, 2,
/*2291*/      OPC_CheckCondCode, ISD::SETGT,
/*2293*/      OPC_MoveParent,
/*2294*/      OPC_EmitInteger, MVT::i32, 32, 
/*2297*/      OPC_EmitNode, TARGET_OPCODE(XCore::ASHR_l2rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #2 
/*2306*/      OPC_EmitInteger, MVT::i32, 0, 
/*2309*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
              // Src: (setcc:i32 GRRegs:i32:$lhs, -1:i32, SETGT:Other) - Complexity = 8
              // Dst: (EQ_2rus:i32 (ASHR_l2rus:i32 GRRegs:i32:$lhs, 32:i32), 0:i32)
/*2318*/    /*Scope*/ 55, /*->2374*/
/*2319*/      OPC_RecordChild1, // #1 = $rhs
/*2320*/      OPC_MoveChild, 1,
/*2322*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2325*/      OPC_CheckPredicate, 0, // Predicate_immUs
/*2327*/      OPC_MoveParent,
/*2328*/      OPC_MoveChild, 2,
/*2330*/      OPC_Scope, 14, /*->2346*/ // 2 children in Scope
/*2332*/        OPC_CheckCondCode, ISD::SETEQ,
/*2334*/        OPC_MoveParent,
/*2335*/        OPC_EmitConvertToTarget, 1,
/*2337*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs, SETEQ:Other) - Complexity = 7
                // Dst: (EQ_2rus:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs)
/*2346*/      /*Scope*/ 26, /*->2373*/
/*2347*/        OPC_CheckCondCode, ISD::SETNE,
/*2349*/        OPC_MoveParent,
/*2350*/        OPC_EmitConvertToTarget, 1,
/*2352*/        OPC_EmitNode, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2,  // Results = #3 
/*2361*/        OPC_EmitInteger, MVT::i32, 0, 
/*2364*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 3, 4, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs, SETNE:Other) - Complexity = 7
                // Dst: (EQ_2rus:i32 (EQ_2rus:i32 GRRegs:i32:$lhs, (imm:i32)<<P:Predicate_immUs>>:$rhs), 0:i32)
/*2373*/      0, /*End of Scope*/
/*2374*/    /*Scope*/ 69|128,1/*197*/, /*->2573*/
/*2376*/      OPC_CheckChild0Type, MVT::i32,
/*2378*/      OPC_RecordChild1, // #1 = $rhs
/*2379*/      OPC_MoveChild, 2,
/*2381*/      OPC_Scope, 12, /*->2395*/ // 10 children in Scope
/*2383*/        OPC_CheckCondCode, ISD::SETGT,
/*2385*/        OPC_MoveParent,
/*2386*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LSS_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETGT:Other) - Complexity = 3
                // Dst: (LSS_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs)
/*2395*/      /*Scope*/ 12, /*->2408*/
/*2396*/        OPC_CheckCondCode, ISD::SETUGT,
/*2398*/        OPC_MoveParent,
/*2399*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LSU_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETUGT:Other) - Complexity = 3
                // Dst: (LSU_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs)
/*2408*/      /*Scope*/ 12, /*->2421*/
/*2409*/        OPC_CheckCondCode, ISD::SETLT,
/*2411*/        OPC_MoveParent,
/*2412*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LSS_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETLT:Other) - Complexity = 3
                // Dst: (LSS_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs)
/*2421*/      /*Scope*/ 12, /*->2434*/
/*2422*/        OPC_CheckCondCode, ISD::SETULT,
/*2424*/        OPC_MoveParent,
/*2425*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::LSU_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETULT:Other) - Complexity = 3
                // Dst: (LSU_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs)
/*2434*/      /*Scope*/ 12, /*->2447*/
/*2435*/        OPC_CheckCondCode, ISD::SETEQ,
/*2437*/        OPC_MoveParent,
/*2438*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETEQ:Other) - Complexity = 3
                // Dst: (EQ_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs)
/*2447*/      /*Scope*/ 24, /*->2472*/
/*2448*/        OPC_CheckCondCode, ISD::SETLE,
/*2450*/        OPC_MoveParent,
/*2451*/        OPC_EmitNode, TARGET_OPCODE(XCore::LSS_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #2 
/*2460*/        OPC_EmitInteger, MVT::i32, 0, 
/*2463*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETLE:Other) - Complexity = 3
                // Dst: (EQ_2rus:i32 (LSS_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs), 0:i32)
/*2472*/      /*Scope*/ 24, /*->2497*/
/*2473*/        OPC_CheckCondCode, ISD::SETULE,
/*2475*/        OPC_MoveParent,
/*2476*/        OPC_EmitNode, TARGET_OPCODE(XCore::LSU_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 1, 0,  // Results = #2 
/*2485*/        OPC_EmitInteger, MVT::i32, 0, 
/*2488*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETULE:Other) - Complexity = 3
                // Dst: (EQ_2rus:i32 (LSU_3r:i32 GRRegs:i32:$rhs, GRRegs:i32:$lhs), 0:i32)
/*2497*/      /*Scope*/ 24, /*->2522*/
/*2498*/        OPC_CheckCondCode, ISD::SETGE,
/*2500*/        OPC_MoveParent,
/*2501*/        OPC_EmitNode, TARGET_OPCODE(XCore::LSS_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #2 
/*2510*/        OPC_EmitInteger, MVT::i32, 0, 
/*2513*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETGE:Other) - Complexity = 3
                // Dst: (EQ_2rus:i32 (LSS_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), 0:i32)
/*2522*/      /*Scope*/ 24, /*->2547*/
/*2523*/        OPC_CheckCondCode, ISD::SETUGE,
/*2525*/        OPC_MoveParent,
/*2526*/        OPC_EmitNode, TARGET_OPCODE(XCore::LSU_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #2 
/*2535*/        OPC_EmitInteger, MVT::i32, 0, 
/*2538*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETUGE:Other) - Complexity = 3
                // Dst: (EQ_2rus:i32 (LSU_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), 0:i32)
/*2547*/      /*Scope*/ 24, /*->2572*/
/*2548*/        OPC_CheckCondCode, ISD::SETNE,
/*2550*/        OPC_MoveParent,
/*2551*/        OPC_EmitNode, TARGET_OPCODE(XCore::EQ_3r), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1,  // Results = #2 
/*2560*/        OPC_EmitInteger, MVT::i32, 0, 
/*2563*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::EQ_2rus), 0,
                    1/*#VTs*/, MVT::i32, 2/*#Ops*/, 2, 3, 
                // Src: (setcc:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs, SETNE:Other) - Complexity = 3
                // Dst: (EQ_2rus:i32 (EQ_3r:i32 GRRegs:i32:$lhs, GRRegs:i32:$rhs), 0:i32)
/*2572*/      0, /*End of Scope*/
/*2573*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SHL),// ->2611
/*2577*/    OPC_RecordChild0, // #0 = $b
/*2578*/    OPC_RecordChild1, // #1 = $c
/*2579*/    OPC_Scope, 19, /*->2600*/ // 2 children in Scope
/*2581*/      OPC_MoveChild, 1,
/*2583*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2586*/      OPC_CheckPredicate, 19, // Predicate_immBitp
/*2588*/      OPC_MoveParent,
/*2589*/      OPC_EmitConvertToTarget, 1,
/*2591*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SHL_2rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
              // Src: (shl:i32 GRRegs:i32:$b, (imm:i32)<<P:Predicate_immBitp>>:$c) - Complexity = 7
              // Dst: (SHL_2rus:i32 GRRegs:i32:$b, (imm:i32):$c)
/*2600*/    /*Scope*/ 9, /*->2610*/
/*2601*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SHL_3r), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (shl:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
              // Dst: (SHL_3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*2610*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SRL),// ->2648
/*2614*/    OPC_RecordChild0, // #0 = $b
/*2615*/    OPC_RecordChild1, // #1 = $c
/*2616*/    OPC_Scope, 19, /*->2637*/ // 2 children in Scope
/*2618*/      OPC_MoveChild, 1,
/*2620*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2623*/      OPC_CheckPredicate, 19, // Predicate_immBitp
/*2625*/      OPC_MoveParent,
/*2626*/      OPC_EmitConvertToTarget, 1,
/*2628*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SHR_2rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 2, 
              // Src: (srl:i32 GRRegs:i32:$b, (imm:i32)<<P:Predicate_immBitp>>:$c) - Complexity = 7
              // Dst: (SHR_2rus:i32 GRRegs:i32:$b, (imm:i32):$c)
/*2637*/    /*Scope*/ 9, /*->2647*/
/*2638*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SHR_3r), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (srl:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
              // Dst: (SHR_3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
/*2647*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 42,  TARGET_OPCODE(XCoreISD::STWSP),// ->2693
/*2651*/    OPC_RecordNode,   // #0 = 'XCoreStwsp' chained node
/*2652*/    OPC_RecordChild1, // #1 = $val
/*2653*/    OPC_CheckChild1Type, MVT::i32,
/*2655*/    OPC_RecordChild2, // #2 = $index
/*2656*/    OPC_MoveChild, 2,
/*2658*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2661*/    OPC_Scope, 14, /*->2677*/ // 2 children in Scope
/*2663*/      OPC_CheckPredicate, 21, // Predicate_immU6
/*2665*/      OPC_MoveParent,
/*2666*/      OPC_EmitMergeInputChains1_0,
/*2667*/      OPC_EmitConvertToTarget, 2,
/*2669*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::STWSP_ru6), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 1, 3, 
              // Src: (XCoreStwsp GRRegs:i32:$val, (imm:i32)<<P:Predicate_immU6>>:$index) - Complexity = 7
              // Dst: (STWSP_ru6 GRRegs:i32:$val, (imm:i32):$index)
/*2677*/    /*Scope*/ 14, /*->2692*/
/*2678*/      OPC_CheckPredicate, 22, // Predicate_immU16
/*2680*/      OPC_MoveParent,
/*2681*/      OPC_EmitMergeInputChains1_0,
/*2682*/      OPC_EmitConvertToTarget, 2,
/*2684*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::STWSP_lru6), 0|OPFL_Chain,
                  0/*#VTs*/, 2/*#Ops*/, 1, 3, 
              // Src: (XCoreStwsp GRRegs:i32:$val, (imm:i32)<<P:Predicate_immU16>>:$index) - Complexity = 7
              // Dst: (STWSP_lru6 GRRegs:i32:$val, (imm:i32):$index)
/*2692*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 42,  TARGET_OPCODE(XCoreISD::RETSP),// ->2738
/*2696*/    OPC_RecordNode,   // #0 = 'XCoreRetsp' chained node
/*2697*/    OPC_CaptureFlagInput,
/*2698*/    OPC_RecordChild1, // #1 = $b
/*2699*/    OPC_MoveChild, 1,
/*2701*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*2704*/    OPC_CheckType, MVT::i32,
/*2706*/    OPC_Scope, 14, /*->2722*/ // 2 children in Scope
/*2708*/      OPC_CheckPredicate, 21, // Predicate_immU6
/*2710*/      OPC_MoveParent,
/*2711*/      OPC_EmitMergeInputChains1_0,
/*2712*/      OPC_EmitConvertToTarget, 1,
/*2714*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::RETSP_u6), 0|OPFL_Chain|OPFL_FlagInput,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
              // Src: (XCoreRetsp (imm:i32)<<P:Predicate_immU6>>:$b) - Complexity = 7
              // Dst: (RETSP_u6:i32 (imm:i32):$b)
/*2722*/    /*Scope*/ 14, /*->2737*/
/*2723*/      OPC_CheckPredicate, 22, // Predicate_immU16
/*2725*/      OPC_MoveParent,
/*2726*/      OPC_EmitMergeInputChains1_0,
/*2727*/      OPC_EmitConvertToTarget, 1,
/*2729*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::RETSP_lu6), 0|OPFL_Chain|OPFL_FlagInput,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
              // Src: (XCoreRetsp (imm:i32)<<P:Predicate_immU16>>:$b) - Complexity = 7
              // Dst: (RETSP_lu6:i32 (imm:i32):$b)
/*2737*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 85,  TARGET_OPCODE(XCoreISD::BL),// ->2826
/*2741*/    OPC_RecordNode,   // #0 = 'XCoreBranchLink' chained node
/*2742*/    OPC_CaptureFlagInput,
/*2743*/    OPC_RecordChild1, // #1 = $target
/*2744*/    OPC_Scope, 67, /*->2813*/ // 2 children in Scope
/*2746*/      OPC_MoveChild, 1,
/*2748*/      OPC_SwitchOpcode /*3 cases */, 34,  TARGET_OPCODE(ISD::Constant),// ->2786
/*2752*/        OPC_CheckType, MVT::i32,
/*2754*/        OPC_Scope, 14, /*->2770*/ // 2 children in Scope
/*2756*/          OPC_CheckPredicate, 23, // Predicate_immU10
/*2758*/          OPC_MoveParent,
/*2759*/          OPC_EmitMergeInputChains1_0,
/*2760*/          OPC_EmitConvertToTarget, 1,
/*2762*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BL_u10), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                      1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
                  // Src: (XCoreBranchLink (imm:i32)<<P:Predicate_immU10>>:$target) - Complexity = 7
                  // Dst: (BL_u10:i32 (imm:i32):$target)
/*2770*/        /*Scope*/ 14, /*->2785*/
/*2771*/          OPC_CheckPredicate, 24, // Predicate_immU20
/*2773*/          OPC_MoveParent,
/*2774*/          OPC_EmitMergeInputChains1_0,
/*2775*/          OPC_EmitConvertToTarget, 1,
/*2777*/          OPC_MorphNodeTo, TARGET_OPCODE(XCore::BL_lu10), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                      1/*#VTs*/, MVT::i32, 1/*#Ops*/, 2, 
                  // Src: (XCoreBranchLink (imm:i32)<<P:Predicate_immU20>>:$target) - Complexity = 7
                  // Dst: (BL_lu10:i32 (imm:i32):$target)
/*2785*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 10,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->2799
/*2789*/        OPC_MoveParent,
/*2790*/        OPC_EmitMergeInputChains1_0,
/*2791*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::BL_lu10), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (XCoreBranchLink (tglobaladdr:iPTR):$addr) - Complexity = 6
                // Dst: (BL_lu10:i32 (tglobaladdr:i32):$addr)
              /*SwitchOpcode*/ 10,  TARGET_OPCODE(ISD::TargetExternalSymbol),// ->2812
/*2802*/        OPC_MoveParent,
/*2803*/        OPC_EmitMergeInputChains1_0,
/*2804*/        OPC_MorphNodeTo, TARGET_OPCODE(XCore::BL_lu10), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                    1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
                // Src: (XCoreBranchLink (texternalsym:iPTR):$addr) - Complexity = 6
                // Dst: (BL_lu10:i32 (texternalsym:i32):$addr)
              0, // EndSwitchOpcode
/*2813*/    /*Scope*/ 11, /*->2825*/
/*2814*/      OPC_CheckChild1Type, MVT::i32,
/*2816*/      OPC_EmitMergeInputChains1_0,
/*2817*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::BLA_1r), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput|OPFL_Variadic1,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
              // Src: (XCoreBranchLink GRRegs:i32:$addr) - Complexity = 3
              // Dst: (BLA_1r:i32 GRRegs:i32:$addr)
/*2825*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 17,  TARGET_OPCODE(ISD::CALLSEQ_START),// ->2846
/*2829*/    OPC_RecordNode,   // #0 = 'callseq_start' chained node
/*2830*/    OPC_RecordChild1, // #1 = $amt
/*2831*/    OPC_MoveChild, 1,
/*2833*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*2836*/    OPC_MoveParent,
/*2837*/    OPC_EmitMergeInputChains1_0,
/*2838*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::ADJCALLSTACKDOWN), 0|OPFL_Chain|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
            // Src: (callseq_start (timm:i32):$amt) - Complexity = 6
            // Dst: (ADJCALLSTACKDOWN:i32 (timm:i32):$amt)
          /*SwitchOpcode*/ 33,  TARGET_OPCODE(XCoreISD::PCRelativeWrapper),// ->2882
/*2849*/    OPC_RecordChild0, // #0 = $addr
/*2850*/    OPC_MoveChild, 0,
/*2852*/    OPC_SwitchOpcode /*2 cases */, 11,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->2867
/*2856*/      OPC_MoveParent,
/*2857*/      OPC_CheckType, MVT::i32,
/*2859*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAP_lu10), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (pcrelwrapper:i32 (tglobaladdr:i32):$addr) - Complexity = 6
              // Dst: (LDAP_lu10:i32 (tglobaladdr:i32):$addr)
            /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::TargetBlockAddress),// ->2881
/*2870*/      OPC_MoveParent,
/*2871*/      OPC_CheckType, MVT::i32,
/*2873*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDAP_lu10_ba), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
              // Src: (pcrelwrapper:i32 (tblockaddress:i32):$addr) - Complexity = 6
              // Dst: (LDAP_lu10_ba:i32 (tblockaddress:i32):$addr)
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 18,  TARGET_OPCODE(XCoreISD::BR_JT),// ->2903
/*2885*/    OPC_RecordNode,   // #0 = 'XCoreBR_JT' chained node
/*2886*/    OPC_RecordChild1, // #1 = $t
/*2887*/    OPC_MoveChild, 1,
/*2889*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetJumpTable),
/*2892*/    OPC_MoveParent,
/*2893*/    OPC_RecordChild2, // #2 = $i
/*2894*/    OPC_EmitMergeInputChains1_0,
/*2895*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::BR_JT), 0|OPFL_Chain,
                0/*#VTs*/, 2/*#Ops*/, 1, 2, 
            // Src: (XCoreBR_JT (tjumptable:i32):$t, GRRegs:i32:$i) - Complexity = 6
            // Dst: (BR_JT (tjumptable:i32):$t, GRRegs:i32:$i)
          /*SwitchOpcode*/ 18,  TARGET_OPCODE(XCoreISD::BR_JT32),// ->2924
/*2906*/    OPC_RecordNode,   // #0 = 'XCoreBR_JT32' chained node
/*2907*/    OPC_RecordChild1, // #1 = $t
/*2908*/    OPC_MoveChild, 1,
/*2910*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetJumpTable),
/*2913*/    OPC_MoveParent,
/*2914*/    OPC_RecordChild2, // #2 = $i
/*2915*/    OPC_EmitMergeInputChains1_0,
/*2916*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::BR_JT32), 0|OPFL_Chain,
                0/*#VTs*/, 2/*#Ops*/, 1, 2, 
            // Src: (XCoreBR_JT32 (tjumptable:i32):$t, GRRegs:i32:$i) - Complexity = 6
            // Dst: (BR_JT32 (tjumptable:i32):$t, GRRegs:i32:$i)
          /*SwitchOpcode*/ 29,  TARGET_OPCODE(ISD::Constant),// ->2956
/*2927*/    OPC_RecordNode,   // #0 = $b
/*2928*/    OPC_Scope, 12, /*->2942*/ // 2 children in Scope
/*2930*/      OPC_CheckPredicate, 21, // Predicate_immU6
/*2932*/      OPC_EmitConvertToTarget, 0,
/*2934*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDC_ru6), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
              // Src: (imm:i32)<<P:Predicate_immU6>>:$b - Complexity = 4
              // Dst: (LDC_ru6:i32 (imm:i32):$b)
/*2942*/    /*Scope*/ 12, /*->2955*/
/*2943*/      OPC_CheckPredicate, 22, // Predicate_immU16
/*2945*/      OPC_EmitConvertToTarget, 0,
/*2947*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::LDC_lru6), 0,
                  1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
              // Src: (imm:i32)<<P:Predicate_immU16>>:$b - Complexity = 4
              // Dst: (LDC_lru6:i32 (imm:i32):$b)
/*2955*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::OR),// ->2970
/*2959*/    OPC_RecordChild0, // #0 = $b
/*2960*/    OPC_RecordChild1, // #1 = $c
/*2961*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::OR_3r), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
            // Src: (or:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
            // Dst: (OR_3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::SDIV),// ->2984
/*2973*/    OPC_RecordChild0, // #0 = $b
/*2974*/    OPC_RecordChild1, // #1 = $c
/*2975*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::DIVS_l3r), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
            // Src: (sdiv:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
            // Dst: (DIVS_l3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::UDIV),// ->2998
/*2987*/    OPC_RecordChild0, // #0 = $b
/*2988*/    OPC_RecordChild1, // #1 = $c
/*2989*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::DIVU_l3r), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
            // Src: (udiv:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
            // Dst: (DIVU_l3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::SREM),// ->3012
/*3001*/    OPC_RecordChild0, // #0 = $b
/*3002*/    OPC_RecordChild1, // #1 = $c
/*3003*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::REMS_l3r), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
            // Src: (srem:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
            // Dst: (REMS_l3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(ISD::UREM),// ->3026
/*3015*/    OPC_RecordChild0, // #0 = $b
/*3016*/    OPC_RecordChild1, // #1 = $c
/*3017*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::REMU_l3r), 0,
                1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
            // Src: (urem:i32 GRRegs:i32:$b, GRRegs:i32:$c) - Complexity = 3
            // Dst: (REMU_l3r:i32 GRRegs:i32:$b, GRRegs:i32:$c)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::BSWAP),// ->3038
/*3029*/    OPC_RecordChild0, // #0 = $src
/*3030*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::BYTEREV_l2r), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
            // Src: (bswap:i32 GRRegs:i32:$src) - Complexity = 3
            // Dst: (BYTEREV_l2r:i32 GRRegs:i32:$src)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::CTLZ),// ->3050
/*3041*/    OPC_RecordChild0, // #0 = $src
/*3042*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::CLZ_l2r), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0, 
            // Src: (ctlz:i32 GRRegs:i32:$src) - Complexity = 3
            // Dst: (CLZ_l2r:i32 GRRegs:i32:$src)
          /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::BRIND),// ->3065
/*3053*/    OPC_RecordNode,   // #0 = 'brind' chained node
/*3054*/    OPC_RecordChild1, // #1 = $addr
/*3055*/    OPC_CheckChild1Type, MVT::i32,
/*3057*/    OPC_EmitMergeInputChains1_0,
/*3058*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::BAU_1r), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (brind GRRegs:i32:$addr) - Complexity = 3
            // Dst: (BAU_1r GRRegs:i32:$addr)
          /*SwitchOpcode*/ 53,  TARGET_OPCODE(ISD::SIGN_EXTEND_INREG),// ->3121
/*3068*/    OPC_RecordChild0, // #0 = $b
/*3069*/    OPC_MoveChild, 1,
/*3071*/    OPC_Scope, 15, /*->3088*/ // 3 children in Scope
/*3073*/      OPC_CheckValueType, MVT::i1,
/*3075*/      OPC_MoveParent,
/*3076*/      OPC_EmitInteger, MVT::i32, 1, 
/*3079*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SEXT_rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (sext_inreg:i32 GRRegs:i32:$b, i1:Other) - Complexity = 3
              // Dst: (SEXT_rus:i32 GRRegs:i32:$b, 1:i32)
/*3088*/    /*Scope*/ 15, /*->3104*/
/*3089*/      OPC_CheckValueType, MVT::i8,
/*3091*/      OPC_MoveParent,
/*3092*/      OPC_EmitInteger, MVT::i32, 8, 
/*3095*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SEXT_rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (sext_inreg:i32 GRRegs:i32:$b, i8:Other) - Complexity = 3
              // Dst: (SEXT_rus:i32 GRRegs:i32:$b, 8:i32)
/*3104*/    /*Scope*/ 15, /*->3120*/
/*3105*/      OPC_CheckValueType, MVT::i16,
/*3107*/      OPC_MoveParent,
/*3108*/      OPC_EmitInteger, MVT::i32, 16, 
/*3111*/      OPC_MorphNodeTo, TARGET_OPCODE(XCore::SEXT_rus), 0,
                  1/*#VTs*/, MVT::i32, 2/*#Ops*/, 0, 1, 
              // Src: (sext_inreg:i32 GRRegs:i32:$b, i16:Other) - Complexity = 3
              // Dst: (SEXT_rus:i32 GRRegs:i32:$b, 16:i32)
/*3120*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 16,  TARGET_OPCODE(ISD::BR),// ->3140
/*3124*/    OPC_RecordNode,   // #0 = 'br' chained node
/*3125*/    OPC_RecordChild1, // #1 = $addr
/*3126*/    OPC_MoveChild, 1,
/*3128*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*3131*/    OPC_MoveParent,
/*3132*/    OPC_EmitMergeInputChains1_0,
/*3133*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::BRFU_lu6), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (br (bb:Other):$addr) - Complexity = 3
            // Dst: (BRFU_lu6 (bb:Other):$addr)
          /*SwitchOpcode*/ 17,  TARGET_OPCODE(ISD::CTTZ),// ->3160
/*3143*/    OPC_RecordChild0, // #0 = $src
/*3144*/    OPC_EmitNode, TARGET_OPCODE(XCore::BITREV_l2r), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 0,  // Results = #1 
/*3152*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::CLZ_l2r), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1, 
            // Src: (cttz:i32 GRRegs:i32:$src) - Complexity = 3
            // Dst: (CLZ_l2r:i32 (BITREV_l2r:i32 GRRegs:i32:$src))
          /*SwitchOpcode*/ 20,  TARGET_OPCODE(ISD::TRAP),// ->3183
/*3163*/    OPC_RecordNode,   // #0 = 'trap' chained node
/*3164*/    OPC_EmitMergeInputChains1_0,
/*3165*/    OPC_EmitInteger, MVT::i32, 0, 
/*3168*/    OPC_EmitNode, TARGET_OPCODE(XCore::LDC_ru6), 0,
                1/*#VTs*/, MVT::i32, 1/*#Ops*/, 1,  // Results = #2 
/*3176*/    OPC_MorphNodeTo, TARGET_OPCODE(XCore::ECALLF_1r), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 2, 
            // Src: (trap) - Complexity = 3
            // Dst: (ECALLF_1r (LDC_ru6:i32 0:i32))
          0, // EndSwitchOpcode
    0
  }; // Total Array size is 3185 bytes

  // Opcode Histogram:
  // #OPC_Scope                          = 59
  // #OPC_RecordNode                     = 18
  // #OPC_RecordChild                    = 123
  // #OPC_RecordMemRef                   = 2
  // #OPC_CaptureFlagInput               = 3
  // #OPC_MoveChild                      = 82
  // #OPC_MoveParent                     = 161
  // #OPC_CheckSame                      = 1
  // #OPC_CheckPatternPredicate          = 0
  // #OPC_CheckPredicate                 = 78
  // #OPC_CheckOpcode                    = 48
  // #OPC_SwitchOpcode                   = 3
  // #OPC_CheckType                      = 39
  // #OPC_SwitchType                     = 0
  // #OPC_CheckChildType                 = 11
  // #OPC_CheckInteger                   = 31
  // #OPC_CheckCondCode                  = 33
  // #OPC_CheckValueType                 = 3
  // #OPC_CheckComplexPat                = 10
  // #OPC_CheckAndImm                    = 0
  // #OPC_CheckOrImm                     = 0
  // #OPC_CheckFoldableChainNode         = 0
  // #OPC_EmitInteger                    = 25
  // #OPC_EmitStringInteger              = 0
  // #OPC_EmitRegister                   = 0
  // #OPC_EmitConvertToTarget            = 25
  // #OPC_EmitMergeInputChains           = 54
  // #OPC_EmitCopyToReg                  = 0
  // #OPC_EmitNode                       = 31
  // #OPC_EmitNodeXForm                  = 8
  // #OPC_MarkFlagResults                = 0
  // #OPC_CompleteMatch                  = 0
  // #OPC_MorphNodeTo                    = 135

  #undef TARGET_OPCODE
  return SelectCodeCommon(N, MatcherTable,sizeof(MatcherTable));
}

bool CheckNodePredicate(SDNode *Node, unsigned PredNo) const {
  switch (PredNo) {
  default: assert(0 && "Invalid predicate in table?");
  case 0: { // Predicate_immUs
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return (uint32_t)N->getZExtValue() <= 11;

  }
  case 1: { // Predicate_immUs4
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  uint32_t value = (uint32_t)N->getZExtValue();
  return value%4 == 0 && value/4 <= 11;

  }
  case 2: { // Predicate_immUsNeg
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return -((uint32_t)N->getZExtValue()) <= 11;

  }
  case 3: { // Predicate_immUs4Neg
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  uint32_t value = (uint32_t)N->getZExtValue();
  return (-value)%4 == 0 && (-value)/4 <= 11;

  }
  case 4: { // Predicate_unindexedload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 5: { // Predicate_sextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::SEXTLOAD;

  }
  case 6: { // Predicate_sextloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 7: { // Predicate_extload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::EXTLOAD;

  }
  case 8: { // Predicate_extloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 9: { // Predicate_load
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::NON_EXTLOAD;

  }
  case 10: { // Predicate_zextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::ZEXTLOAD;

  }
  case 11: { // Predicate_zextloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 12: { // Predicate_extloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 13: { // Predicate_unindexedstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 14: { // Predicate_truncstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 15: { // Predicate_truncstorei16
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 16: { // Predicate_store
    SDNode *N = Node;

  return !cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 17: { // Predicate_truncstorei8
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 18: { // Predicate_immBpwSubBitp
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  uint32_t value = (uint32_t)N->getZExtValue();
  return (value >= 24 && value <= 31)
          || value == 16
          || value == 8
          || value == 0;

  }
  case 19: { // Predicate_immBitp
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  uint32_t value = (uint32_t)N->getZExtValue();
  return (value >= 1 && value <= 8)
          || value == 16
          || value == 24
          || value == 32;

  }
  case 20: { // Predicate_immMskBitp
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
 return immMskBitp(N); 
  }
  case 21: { // Predicate_immU6
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return (uint32_t)N->getZExtValue() < (1 << 6);

  }
  case 22: { // Predicate_immU16
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return (uint32_t)N->getZExtValue() < (1 << 16);

  }
  case 23: { // Predicate_immU10
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return (uint32_t)N->getZExtValue() < (1 << 10);

  }
  case 24: { // Predicate_immU20
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return (uint32_t)N->getZExtValue() < (1 << 20);

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
    return SelectADDRspii(Root, N, Result[NextRes+0], Result[NextRes+1]);
  case 1:
    Result.resize(NextRes+2);
    return SelectADDRdpii(Root, N, Result[NextRes+0], Result[NextRes+1]);
  case 2:
    Result.resize(NextRes+2);
    return SelectADDRcpii(Root, N, Result[NextRes+0], Result[NextRes+1]);
  }
}

SDValue RunSDNodeXForm(SDValue V, unsigned XFormNo) {
  switch (XFormNo) {
  default: assert(0 && "Invalid xform # in table?");
  case 0: {  // div4_xform
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: imm/4
  assert(N->getZExtValue() % 4 == 0);
  return getI32Imm(N->getZExtValue()/4);

  }
  case 1: {  // neg_xform
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: -imm
  uint32_t value = N->getZExtValue();
  return getI32Imm(-value);

  }
  case 2: {  // div4neg_xform
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: -imm/4
  uint32_t value = N->getZExtValue();
  assert(-value % 4 == 0);
  return getI32Imm(-value/4);

  }
  case 3: {  // bpwsub_xform
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: 32-imm
  uint32_t value = N->getZExtValue();
  return getI32Imm(32-value);

  }
  case 4: {  // msksize_xform
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  // Transformation function: get the size of a mask
  assert(isMask_32(N->getZExtValue()));
  // look for the first non-zero bit
  return getI32Imm(32 - CountLeadingZeros_32(N->getZExtValue()));

  }
  }
}

