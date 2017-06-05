//===- TableGen'erated file -------------------------------------*- C++ -*-===//
//
// DAG Instruction Selector for the Alpha target
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
/*0*/     OPC_SwitchOpcode /*47 cases */, 58,  TARGET_OPCODE(ISD::MEMBARRIER),// ->62
/*4*/       OPC_RecordNode,   // #0 = 'membarrier' chained node
/*5*/       OPC_MoveChild, 1,
/*7*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*10*/      OPC_MoveParent,
/*11*/      OPC_MoveChild, 2,
/*13*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*16*/      OPC_MoveParent,
/*17*/      OPC_MoveChild, 3,
/*19*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*22*/      OPC_MoveParent,
/*23*/      OPC_MoveChild, 4,
/*25*/      OPC_Scope, 16, /*->43*/ // 2 children in Scope
/*27*/        OPC_CheckInteger, 1, 
/*29*/        OPC_MoveParent,
/*30*/        OPC_MoveChild, 5,
/*32*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*35*/        OPC_MoveParent,
/*36*/        OPC_EmitMergeInputChains1_0,
/*37*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::WMB), 0|OPFL_Chain,
                  0/*#VTs*/, 0/*#Ops*/, 
              // Src: (membarrier (imm:i64), (imm:i64), (imm:i64), 1:i64, (imm:i64)) - Complexity = 20
              // Dst: (WMB)
/*43*/      /*Scope*/ 17, /*->61*/
/*44*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*47*/        OPC_MoveParent,
/*48*/        OPC_MoveChild, 5,
/*50*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*53*/        OPC_MoveParent,
/*54*/        OPC_EmitMergeInputChains1_0,
/*55*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::MB), 0|OPFL_Chain,
                  0/*#VTs*/, 0/*#Ops*/, 
              // Src: (membarrier (imm:i64), (imm:i64), (imm:i64), (imm:i64), (imm:i64)) - Complexity = 18
              // Dst: (MB)
/*61*/      0, /*End of Scope*/
          /*SwitchOpcode*/ 116|128,1/*244*/,  TARGET_OPCODE(ISD::AND),// ->310
/*66*/      OPC_Scope, 31, /*->99*/ // 8 children in Scope
/*68*/        OPC_CheckAndImm, 127|128,1/*255*/, 
/*71*/        OPC_MoveChild, 0,
/*73*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::SRL),
/*76*/        OPC_RecordChild0, // #0 = $RA
/*77*/        OPC_MoveChild, 1,
/*79*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*82*/        OPC_RecordChild0, // #1 = $RB
/*83*/        OPC_MoveChild, 1,
/*85*/        OPC_CheckInteger, 3, 
/*87*/        OPC_MoveParent,
/*88*/        OPC_MoveParent,
/*89*/        OPC_MoveParent,
/*90*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::EXTBL), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (and:i64 (srl:i64 GPRC:i64:$RA, (shl:i64 GPRC:i64:$RB, 3:i64)), 255:i64) - Complexity = 19
              // Dst: (EXTBL:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*99*/      /*Scope*/ 32, /*->132*/
/*100*/       OPC_CheckAndImm, 127|128,127|128,3/*65535*/, 
/*104*/       OPC_MoveChild, 0,
/*106*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::SRL),
/*109*/       OPC_RecordChild0, // #0 = $RA
/*110*/       OPC_MoveChild, 1,
/*112*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*115*/       OPC_RecordChild0, // #1 = $RB
/*116*/       OPC_MoveChild, 1,
/*118*/       OPC_CheckInteger, 3, 
/*120*/       OPC_MoveParent,
/*121*/       OPC_MoveParent,
/*122*/       OPC_MoveParent,
/*123*/       OPC_MorphNodeTo, TARGET_OPCODE(Alpha::EXTWL), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (and:i64 (srl:i64 GPRC:i64:$RA, (shl:i64 GPRC:i64:$RB, 3:i64)), 65535:i64) - Complexity = 19
              // Dst: (EXTWL:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*132*/     /*Scope*/ 34, /*->167*/
/*133*/       OPC_CheckAndImm, 127|128,127|128,127|128,127|128,15/*4294967295*/, 
/*139*/       OPC_MoveChild, 0,
/*141*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::SRL),
/*144*/       OPC_RecordChild0, // #0 = $RA
/*145*/       OPC_MoveChild, 1,
/*147*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*150*/       OPC_RecordChild0, // #1 = $RB
/*151*/       OPC_MoveChild, 1,
/*153*/       OPC_CheckInteger, 3, 
/*155*/       OPC_MoveParent,
/*156*/       OPC_MoveParent,
/*157*/       OPC_MoveParent,
/*158*/       OPC_MorphNodeTo, TARGET_OPCODE(Alpha::EXTLL), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (and:i64 (srl:i64 GPRC:i64:$RA, (shl:i64 GPRC:i64:$RB, 3:i64)), 4294967295:i64) - Complexity = 19
              // Dst: (EXTLL:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*167*/     /*Scope*/ 31, /*->199*/
/*168*/       OPC_RecordChild0, // #0 = $RA
/*169*/       OPC_MoveChild, 1,
/*171*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*174*/       OPC_RecordChild0, // #1 = $RB
/*175*/       OPC_MoveChild, 1,
/*177*/       OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*188*/       OPC_MoveParent,
/*189*/       OPC_MoveParent,
/*190*/       OPC_MorphNodeTo, TARGET_OPCODE(Alpha::BICr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (and:i64 GPRC:i64:$RA, (xor:i64 GPRC:i64:$RB, -1:i64)) - Complexity = 11
              // Dst: (BICr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*199*/     /*Scope*/ 31, /*->231*/
/*200*/       OPC_MoveChild, 0,
/*202*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*205*/       OPC_RecordChild0, // #0 = $RB
/*206*/       OPC_MoveChild, 1,
/*208*/       OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*219*/       OPC_MoveParent,
/*220*/       OPC_MoveParent,
/*221*/       OPC_RecordChild1, // #1 = $RA
/*222*/       OPC_MorphNodeTo, TARGET_OPCODE(Alpha::BICr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
              // Src: (and:i64 (xor:i64 GPRC:i64:$RB, -1:i64), GPRC:i64:$RA) - Complexity = 11
              // Dst: (BICr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*231*/     /*Scope*/ 42, /*->274*/
/*232*/       OPC_RecordChild0, // #0 = $RA
/*233*/       OPC_RecordChild1, // #1 = $L
/*234*/       OPC_MoveChild, 1,
/*236*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*239*/       OPC_Scope, 14, /*->255*/ // 2 children in Scope
/*241*/         OPC_CheckPredicate, 0, // Predicate_immUExt8
/*243*/         OPC_MoveParent,
/*244*/         OPC_EmitConvertToTarget, 1,
/*246*/         OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ANDi), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                // Src: (and:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
                // Dst: (ANDi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*255*/       /*Scope*/ 17, /*->273*/
/*256*/         OPC_CheckPredicate, 1, // Predicate_immUExt8inv
/*258*/         OPC_MoveParent,
/*259*/         OPC_EmitConvertToTarget, 1,
/*261*/         OPC_EmitNodeXForm, 0, 2, // invX
/*264*/         OPC_MorphNodeTo, TARGET_OPCODE(Alpha::BICi), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                // Src: (and:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8inv>><<X:invX>>:$L) - Complexity = 7
                // Dst: (BICi:i64 GPRC:i64:$RA, (invX:i64 (imm:i64):$L))
/*273*/       0, /*End of Scope*/
/*274*/     /*Scope*/ 22, /*->297*/
/*275*/       OPC_RecordNode, // #0 = $imm
/*276*/       OPC_RecordChild0, // #1 = $RA
/*277*/       OPC_MoveChild, 1,
/*279*/       OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*282*/       OPC_MoveParent,
/*283*/       OPC_CheckPredicate, 2, // Predicate_zappat
/*285*/       OPC_EmitNodeXForm, 1, 0, // iZAPX
/*288*/       OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ZAPNOTi), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
              // Src: (and:i64 GPRC:i64:$RA, (imm:i64))<<P:Predicate_zappat>>:$imm - Complexity = 7
              // Dst: (ZAPNOTi:i64 GPRC:i64:$RA, (iZAPX:i64 GPRC:i64:$imm))
/*297*/     /*Scope*/ 11, /*->309*/
/*298*/       OPC_RecordChild0, // #0 = $RA
/*299*/       OPC_RecordChild1, // #1 = $RB
/*300*/       OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ANDr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (and:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
              // Dst: (ANDr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*309*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 116|128,3/*500*/,  TARGET_OPCODE(ISD::SETCC),// ->814
/*314*/     OPC_Scope, 57, /*->373*/ // 2 children in Scope
/*316*/       OPC_MoveChild, 0,
/*318*/       OPC_CheckAndImm, 127|128,1/*255*/, 
/*321*/       OPC_RecordChild0, // #0 = $RA
/*322*/       OPC_MoveParent,
/*323*/       OPC_Scope, 21, /*->346*/ // 2 children in Scope
/*325*/         OPC_MoveChild, 1,
/*327*/         OPC_CheckAndImm, 127|128,1/*255*/, 
/*330*/         OPC_RecordChild0, // #1 = $RB
/*331*/         OPC_MoveParent,
/*332*/         OPC_MoveChild, 2,
/*334*/         OPC_CheckCondCode, ISD::SETUGE,
/*336*/         OPC_MoveParent,
/*337*/         OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPBGE), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (setcc:i64 (and:i64 GPRC:i64:$RA, 255:i64), (and:i64 GPRC:i64:$RB, 255:i64), SETUGE:Other) - Complexity = 19
                // Dst: (CMPBGE:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*346*/       /*Scope*/ 25, /*->372*/
/*347*/         OPC_RecordChild1, // #1 = $L
/*348*/         OPC_MoveChild, 1,
/*350*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*353*/         OPC_CheckPredicate, 0, // Predicate_immUExt8
/*355*/         OPC_MoveParent,
/*356*/         OPC_MoveChild, 2,
/*358*/         OPC_CheckCondCode, ISD::SETUGE,
/*360*/         OPC_MoveParent,
/*361*/         OPC_EmitConvertToTarget, 1,
/*363*/         OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPBGEi), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                // Src: (setcc:i64 (and:i64 GPRC:i64:$RA, 255:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L, SETUGE:Other) - Complexity = 15
                // Dst: (CMPBGEi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*372*/       0, /*End of Scope*/
/*373*/     /*Scope*/ 54|128,3/*438*/, /*->813*/
/*375*/       OPC_RecordChild0, // #0 = $RA
/*376*/       OPC_Scope, 103, /*->481*/ // 4 children in Scope
/*378*/         OPC_RecordChild1, // #1 = $L
/*379*/         OPC_MoveChild, 1,
/*381*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*384*/         OPC_CheckPredicate, 0, // Predicate_immUExt8
/*386*/         OPC_MoveParent,
/*387*/         OPC_MoveChild, 2,
/*389*/         OPC_Scope, 14, /*->405*/ // 6 children in Scope
/*391*/           OPC_CheckCondCode, ISD::SETEQ,
/*393*/           OPC_MoveParent,
/*394*/           OPC_EmitConvertToTarget, 1,
/*396*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (setcc:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L, SETEQ:Other) - Complexity = 7
                  // Dst: (CMPEQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*405*/         /*Scope*/ 14, /*->420*/
/*406*/           OPC_CheckCondCode, ISD::SETLE,
/*408*/           OPC_MoveParent,
/*409*/           OPC_EmitConvertToTarget, 1,
/*411*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLEi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (setcc:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L, SETLE:Other) - Complexity = 7
                  // Dst: (CMPLEi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*420*/         /*Scope*/ 14, /*->435*/
/*421*/           OPC_CheckCondCode, ISD::SETLT,
/*423*/           OPC_MoveParent,
/*424*/           OPC_EmitConvertToTarget, 1,
/*426*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLTi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (setcc:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L, SETLT:Other) - Complexity = 7
                  // Dst: (CMPLTi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*435*/         /*Scope*/ 14, /*->450*/
/*436*/           OPC_CheckCondCode, ISD::SETULE,
/*438*/           OPC_MoveParent,
/*439*/           OPC_EmitConvertToTarget, 1,
/*441*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULEi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (setcc:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L, SETULE:Other) - Complexity = 7
                  // Dst: (CMPULEi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*450*/         /*Scope*/ 14, /*->465*/
/*451*/           OPC_CheckCondCode, ISD::SETULT,
/*453*/           OPC_MoveParent,
/*454*/           OPC_EmitConvertToTarget, 1,
/*456*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULTi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (setcc:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L, SETULT:Other) - Complexity = 7
                  // Dst: (CMPULTi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*465*/         /*Scope*/ 14, /*->480*/
/*466*/           OPC_CheckCondCode, ISD::SETUEQ,
/*468*/           OPC_MoveParent,
/*469*/           OPC_EmitConvertToTarget, 1,
/*471*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (setcc:i64 GPRC:i64:$X, (imm:i64)<<P:Predicate_immUExt8>>:$Y, SETUEQ:Other) - Complexity = 7
                  // Dst: (CMPEQi:i64 GPRC:i64:$X, (imm:i64)<<P:Predicate_immUExt8>>:$Y)
/*480*/         0, /*End of Scope*/
/*481*/       /*Scope*/ 73, /*->555*/
/*482*/         OPC_MoveChild, 0,
/*484*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*487*/         OPC_CheckPredicate, 0, // Predicate_immUExt8
/*489*/         OPC_MoveParent,
/*490*/         OPC_RecordChild1, // #1 = $Y
/*491*/         OPC_MoveChild, 2,
/*493*/         OPC_Scope, 14, /*->509*/ // 4 children in Scope
/*495*/           OPC_CheckCondCode, ISD::SETUGT,
/*497*/           OPC_MoveParent,
/*498*/           OPC_EmitConvertToTarget, 0,
/*500*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULTi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
                  // Src: (setcc:i64 (imm:i64)<<P:Predicate_immUExt8>>:$X, GPRC:i64:$Y, SETUGT:Other) - Complexity = 7
                  // Dst: (CMPULTi:i64 GPRC:i64:$Y, (imm:i64)<<P:Predicate_immUExt8>>:$X)
/*509*/         /*Scope*/ 14, /*->524*/
/*510*/           OPC_CheckCondCode, ISD::SETUGE,
/*512*/           OPC_MoveParent,
/*513*/           OPC_EmitConvertToTarget, 0,
/*515*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULEi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
                  // Src: (setcc:i64 (imm:i64)<<P:Predicate_immUExt8>>:$X, GPRC:i64:$Y, SETUGE:Other) - Complexity = 7
                  // Dst: (CMPULEi:i64 GPRC:i64:$Y, (imm:i64)<<P:Predicate_immUExt8>>:$X)
/*524*/         /*Scope*/ 14, /*->539*/
/*525*/           OPC_CheckCondCode, ISD::SETGT,
/*527*/           OPC_MoveParent,
/*528*/           OPC_EmitConvertToTarget, 0,
/*530*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLTi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
                  // Src: (setcc:i64 (imm:i64)<<P:Predicate_immUExt8>>:$X, GPRC:i64:$Y, SETGT:Other) - Complexity = 7
                  // Dst: (CMPLTi:i64 GPRC:i64:$Y, (imm:i64)<<P:Predicate_immUExt8>>:$X)
/*539*/         /*Scope*/ 14, /*->554*/
/*540*/           OPC_CheckCondCode, ISD::SETGE,
/*542*/           OPC_MoveParent,
/*543*/           OPC_EmitConvertToTarget, 0,
/*545*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLEi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
                  // Src: (setcc:i64 (imm:i64)<<P:Predicate_immUExt8>>:$X, GPRC:i64:$Y, SETGE:Other) - Complexity = 7
                  // Dst: (CMPLEi:i64 GPRC:i64:$Y, (imm:i64)<<P:Predicate_immUExt8>>:$X)
/*554*/         0, /*End of Scope*/
/*555*/       /*Scope*/ 67, /*->623*/
/*556*/         OPC_RecordChild1, // #1 = $Y
/*557*/         OPC_MoveChild, 1,
/*559*/         OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*562*/         OPC_CheckPredicate, 0, // Predicate_immUExt8
/*564*/         OPC_MoveParent,
/*565*/         OPC_MoveChild, 2,
/*567*/         OPC_Scope, 26, /*->595*/ // 2 children in Scope
/*569*/           OPC_CheckCondCode, ISD::SETNE,
/*571*/           OPC_MoveParent,
/*572*/           OPC_EmitConvertToTarget, 1,
/*574*/           OPC_EmitNode, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2,  // Results = #3 
/*583*/           OPC_EmitInteger, MVT::i64, 0, 
/*586*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 3, 4, 
                  // Src: (setcc:i64 GPRC:i64:$X, (imm:i64)<<P:Predicate_immUExt8>>:$Y, SETNE:Other) - Complexity = 7
                  // Dst: (CMPEQi:i64 (CMPEQi:i64 GPRC:i64:$X, (imm:i64)<<P:Predicate_immUExt8>>:$Y), 0:i64)
/*595*/         /*Scope*/ 26, /*->622*/
/*596*/           OPC_CheckCondCode, ISD::SETUNE,
/*598*/           OPC_MoveParent,
/*599*/           OPC_EmitConvertToTarget, 1,
/*601*/           OPC_EmitNode, TARGET_OPCODE(Alpha::CMPEQ), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2,  // Results = #3 
/*610*/           OPC_EmitInteger, MVT::i64, 0, 
/*613*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 3, 4, 
                  // Src: (setcc:i64 GPRC:i64:$X, (imm:i64)<<P:Predicate_immUExt8>>:$Y, SETUNE:Other) - Complexity = 7
                  // Dst: (CMPEQi:i64 (CMPEQ:i64 GPRC:i64:$X, (imm:i64)<<P:Predicate_immUExt8>>:$Y), 0:i64)
/*622*/         0, /*End of Scope*/
/*623*/       /*Scope*/ 59|128,1/*187*/, /*->812*/
/*625*/         OPC_CheckChild0Type, MVT::i64,
/*627*/         OPC_RecordChild1, // #1 = $RB
/*628*/         OPC_MoveChild, 2,
/*630*/         OPC_Scope, 12, /*->644*/ // 12 children in Scope
/*632*/           OPC_CheckCondCode, ISD::SETEQ,
/*634*/           OPC_MoveParent,
/*635*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQ), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (setcc:i64 GPRC:i64:$RA, GPRC:i64:$RB, SETEQ:Other) - Complexity = 3
                  // Dst: (CMPEQ:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*644*/         /*Scope*/ 12, /*->657*/
/*645*/           OPC_CheckCondCode, ISD::SETLE,
/*647*/           OPC_MoveParent,
/*648*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLE), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (setcc:i64 GPRC:i64:$RA, GPRC:i64:$RB, SETLE:Other) - Complexity = 3
                  // Dst: (CMPLE:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*657*/         /*Scope*/ 12, /*->670*/
/*658*/           OPC_CheckCondCode, ISD::SETLT,
/*660*/           OPC_MoveParent,
/*661*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLT), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (setcc:i64 GPRC:i64:$RA, GPRC:i64:$RB, SETLT:Other) - Complexity = 3
                  // Dst: (CMPLT:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*670*/         /*Scope*/ 12, /*->683*/
/*671*/           OPC_CheckCondCode, ISD::SETULE,
/*673*/           OPC_MoveParent,
/*674*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULE), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (setcc:i64 GPRC:i64:$RA, GPRC:i64:$RB, SETULE:Other) - Complexity = 3
                  // Dst: (CMPULE:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*683*/         /*Scope*/ 12, /*->696*/
/*684*/           OPC_CheckCondCode, ISD::SETULT,
/*686*/           OPC_MoveParent,
/*687*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULT), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (setcc:i64 GPRC:i64:$RA, GPRC:i64:$RB, SETULT:Other) - Complexity = 3
                  // Dst: (CMPULT:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*696*/         /*Scope*/ 12, /*->709*/
/*697*/           OPC_CheckCondCode, ISD::SETUEQ,
/*699*/           OPC_MoveParent,
/*700*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQ), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (setcc:i64 GPRC:i64:$X, GPRC:i64:$Y, SETUEQ:Other) - Complexity = 3
                  // Dst: (CMPEQ:i64 GPRC:i64:$X, GPRC:i64:$Y)
/*709*/         /*Scope*/ 12, /*->722*/
/*710*/           OPC_CheckCondCode, ISD::SETUGT,
/*712*/           OPC_MoveParent,
/*713*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULT), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (setcc:i64 GPRC:i64:$X, GPRC:i64:$Y, SETUGT:Other) - Complexity = 3
                  // Dst: (CMPULT:i64 GPRC:i64:$Y, GPRC:i64:$X)
/*722*/         /*Scope*/ 12, /*->735*/
/*723*/           OPC_CheckCondCode, ISD::SETUGE,
/*725*/           OPC_MoveParent,
/*726*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPULE), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (setcc:i64 GPRC:i64:$X, GPRC:i64:$Y, SETUGE:Other) - Complexity = 3
                  // Dst: (CMPULE:i64 GPRC:i64:$Y, GPRC:i64:$X)
/*735*/         /*Scope*/ 12, /*->748*/
/*736*/           OPC_CheckCondCode, ISD::SETGT,
/*738*/           OPC_MoveParent,
/*739*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLT), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (setcc:i64 GPRC:i64:$X, GPRC:i64:$Y, SETGT:Other) - Complexity = 3
                  // Dst: (CMPLT:i64 GPRC:i64:$Y, GPRC:i64:$X)
/*748*/         /*Scope*/ 12, /*->761*/
/*749*/           OPC_CheckCondCode, ISD::SETGE,
/*751*/           OPC_MoveParent,
/*752*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPLE), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (setcc:i64 GPRC:i64:$X, GPRC:i64:$Y, SETGE:Other) - Complexity = 3
                  // Dst: (CMPLE:i64 GPRC:i64:$Y, GPRC:i64:$X)
/*761*/         /*Scope*/ 24, /*->786*/
/*762*/           OPC_CheckCondCode, ISD::SETNE,
/*764*/           OPC_MoveParent,
/*765*/           OPC_EmitNode, TARGET_OPCODE(Alpha::CMPEQ), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1,  // Results = #2 
/*774*/           OPC_EmitInteger, MVT::i64, 0, 
/*777*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (setcc:i64 GPRC:i64:$X, GPRC:i64:$Y, SETNE:Other) - Complexity = 3
                  // Dst: (CMPEQi:i64 (CMPEQ:i64 GPRC:i64:$X, GPRC:i64:$Y), 0:i64)
/*786*/         /*Scope*/ 24, /*->811*/
/*787*/           OPC_CheckCondCode, ISD::SETUNE,
/*789*/           OPC_MoveParent,
/*790*/           OPC_EmitNode, TARGET_OPCODE(Alpha::CMPEQ), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1,  // Results = #2 
/*799*/           OPC_EmitInteger, MVT::i64, 0, 
/*802*/           OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 3, 
                  // Src: (setcc:i64 GPRC:i64:$X, GPRC:i64:$Y, SETUNE:Other) - Complexity = 3
                  // Dst: (CMPEQi:i64 (CMPEQ:i64 GPRC:i64:$X, GPRC:i64:$Y), 0:i64)
/*811*/         0, /*End of Scope*/
/*812*/       0, /*End of Scope*/
/*813*/     0, /*End of Scope*/
          /*SwitchOpcode*/ 55|128,4/*567*/,  TARGET_OPCODE(ISD::SIGN_EXTEND_INREG),// ->1385
/*818*/     OPC_Scope, 5|128,4/*517*/, /*->1338*/ // 2 children in Scope
/*821*/       OPC_MoveChild, 0,
/*823*/       OPC_SwitchOpcode /*3 cases */, 40|128,2/*296*/,  TARGET_OPCODE(ISD::ADD),// ->1124
/*828*/         OPC_Scope, 36|128,1/*164*/, /*->995*/ // 2 children in Scope
/*831*/           OPC_MoveChild, 0,
/*833*/           OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*836*/           OPC_RecordChild0, // #0 = $RA
/*837*/           OPC_MoveChild, 1,
/*839*/           OPC_Scope, 76, /*->917*/ // 2 children in Scope
/*841*/             OPC_CheckInteger, 2, 
/*843*/             OPC_MoveParent,
/*844*/             OPC_MoveParent,
/*845*/             OPC_RecordChild1, // #1 = $L
/*846*/             OPC_Scope, 52, /*->900*/ // 2 children in Scope
/*848*/               OPC_MoveChild, 1,
/*850*/               OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*853*/               OPC_Scope, 20, /*->875*/ // 2 children in Scope
/*855*/                 OPC_CheckPredicate, 0, // Predicate_immUExt8
/*857*/                 OPC_MoveParent,
/*858*/                 OPC_MoveParent,
/*859*/                 OPC_MoveChild, 1,
/*861*/                 OPC_CheckValueType, MVT::i32,
/*863*/                 OPC_MoveParent,
/*864*/                 OPC_EmitConvertToTarget, 1,
/*866*/                 OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDLi), 0,
                            1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                        // Src: (sext_inreg:i64 (add:i64 (shl:i64 GPRC:i64:$RA, 2:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L), i32:Other) - Complexity = 18
                        // Dst: (S4ADDLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*875*/               /*Scope*/ 23, /*->899*/
/*876*/                 OPC_CheckPredicate, 3, // Predicate_immUExt8neg
/*878*/                 OPC_MoveParent,
/*879*/                 OPC_MoveParent,
/*880*/                 OPC_MoveChild, 1,
/*882*/                 OPC_CheckValueType, MVT::i32,
/*884*/                 OPC_MoveParent,
/*885*/                 OPC_EmitConvertToTarget, 1,
/*887*/                 OPC_EmitNodeXForm, 2, 2, // negX
/*890*/                 OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4SUBLi), 0,
                            1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                        // Src: (sext_inreg:i64 (add:i64 (shl:i64 GPRC:i64:$RA, 2:i64), (imm:i64)<<P:Predicate_immUExt8neg>><<X:negX>>:$L), i32:Other) - Complexity = 18
                        // Dst: (S4SUBLi:i64 GPRC:i64:$RA, (negX:i64 (imm:i64)<<P:Predicate_immUExt8neg>>:$L))
/*899*/               0, /*End of Scope*/
/*900*/             /*Scope*/ 15, /*->916*/
/*901*/               OPC_MoveParent,
/*902*/               OPC_MoveChild, 1,
/*904*/               OPC_CheckValueType, MVT::i32,
/*906*/               OPC_MoveParent,
/*907*/               OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDLr), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                      // Src: (sext_inreg:i64 (add:i64 (shl:i64 GPRC:i64:$RA, 2:i64), GPRC:i64:$RB), i32:Other) - Complexity = 14
                      // Dst: (S4ADDLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*916*/             0, /*End of Scope*/
/*917*/           /*Scope*/ 76, /*->994*/
/*918*/             OPC_CheckInteger, 3, 
/*920*/             OPC_MoveParent,
/*921*/             OPC_MoveParent,
/*922*/             OPC_RecordChild1, // #1 = $L
/*923*/             OPC_Scope, 52, /*->977*/ // 2 children in Scope
/*925*/               OPC_MoveChild, 1,
/*927*/               OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*930*/               OPC_Scope, 20, /*->952*/ // 2 children in Scope
/*932*/                 OPC_CheckPredicate, 0, // Predicate_immUExt8
/*934*/                 OPC_MoveParent,
/*935*/                 OPC_MoveParent,
/*936*/                 OPC_MoveChild, 1,
/*938*/                 OPC_CheckValueType, MVT::i32,
/*940*/                 OPC_MoveParent,
/*941*/                 OPC_EmitConvertToTarget, 1,
/*943*/                 OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8ADDLi), 0,
                            1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                        // Src: (sext_inreg:i64 (add:i64 (shl:i64 GPRC:i64:$RA, 3:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L), i32:Other) - Complexity = 18
                        // Dst: (S8ADDLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*952*/               /*Scope*/ 23, /*->976*/
/*953*/                 OPC_CheckPredicate, 3, // Predicate_immUExt8neg
/*955*/                 OPC_MoveParent,
/*956*/                 OPC_MoveParent,
/*957*/                 OPC_MoveChild, 1,
/*959*/                 OPC_CheckValueType, MVT::i32,
/*961*/                 OPC_MoveParent,
/*962*/                 OPC_EmitConvertToTarget, 1,
/*964*/                 OPC_EmitNodeXForm, 2, 2, // negX
/*967*/                 OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8SUBLi), 0,
                            1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                        // Src: (sext_inreg:i64 (add:i64 (shl:i64 GPRC:i64:$RA, 3:i64), (imm:i64)<<P:Predicate_immUExt8neg>><<X:negX>>:$L), i32:Other) - Complexity = 18
                        // Dst: (S8SUBLi:i64 GPRC:i64:$RA, (negX:i64 (imm:i64)<<P:Predicate_immUExt8neg>>:$L))
/*976*/               0, /*End of Scope*/
/*977*/             /*Scope*/ 15, /*->993*/
/*978*/               OPC_MoveParent,
/*979*/               OPC_MoveChild, 1,
/*981*/               OPC_CheckValueType, MVT::i32,
/*983*/               OPC_MoveParent,
/*984*/               OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8ADDLr), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                      // Src: (sext_inreg:i64 (add:i64 (shl:i64 GPRC:i64:$RA, 3:i64), GPRC:i64:$RB), i32:Other) - Complexity = 14
                      // Dst: (S8ADDLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*993*/             0, /*End of Scope*/
/*994*/           0, /*End of Scope*/
/*995*/         /*Scope*/ 127, /*->1123*/
/*996*/           OPC_RecordChild0, // #0 = $RB
/*997*/           OPC_Scope, 50, /*->1049*/ // 2 children in Scope
/*999*/             OPC_MoveChild, 1,
/*1001*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*1004*/            OPC_RecordChild0, // #1 = $RA
/*1005*/            OPC_MoveChild, 1,
/*1007*/            OPC_Scope, 19, /*->1028*/ // 2 children in Scope
/*1009*/              OPC_CheckInteger, 2, 
/*1011*/              OPC_MoveParent,
/*1012*/              OPC_MoveParent,
/*1013*/              OPC_MoveParent,
/*1014*/              OPC_MoveChild, 1,
/*1016*/              OPC_CheckValueType, MVT::i32,
/*1018*/              OPC_MoveParent,
/*1019*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDLr), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                      // Src: (sext_inreg:i64 (add:i64 GPRC:i64:$RB, (shl:i64 GPRC:i64:$RA, 2:i64)), i32:Other) - Complexity = 14
                      // Dst: (S4ADDLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*1028*/            /*Scope*/ 19, /*->1048*/
/*1029*/              OPC_CheckInteger, 3, 
/*1031*/              OPC_MoveParent,
/*1032*/              OPC_MoveParent,
/*1033*/              OPC_MoveParent,
/*1034*/              OPC_MoveChild, 1,
/*1036*/              OPC_CheckValueType, MVT::i32,
/*1038*/              OPC_MoveParent,
/*1039*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8ADDLr), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                      // Src: (sext_inreg:i64 (add:i64 GPRC:i64:$RB, (shl:i64 GPRC:i64:$RA, 3:i64)), i32:Other) - Complexity = 14
                      // Dst: (S8ADDLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*1048*/            0, /*End of Scope*/
/*1049*/          /*Scope*/ 72, /*->1122*/
/*1050*/            OPC_RecordChild1, // #1 = $L
/*1051*/            OPC_Scope, 52, /*->1105*/ // 2 children in Scope
/*1053*/              OPC_MoveChild, 1,
/*1055*/              OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1058*/              OPC_Scope, 20, /*->1080*/ // 2 children in Scope
/*1060*/                OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1062*/                OPC_MoveParent,
/*1063*/                OPC_MoveParent,
/*1064*/                OPC_MoveChild, 1,
/*1066*/                OPC_CheckValueType, MVT::i32,
/*1068*/                OPC_MoveParent,
/*1069*/                OPC_EmitConvertToTarget, 1,
/*1071*/                OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDLi), 0,
                            1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                        // Src: (sext_inreg:i64 (add:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L), i32:Other) - Complexity = 10
                        // Dst: (ADDLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*1080*/              /*Scope*/ 23, /*->1104*/
/*1081*/                OPC_CheckPredicate, 3, // Predicate_immUExt8neg
/*1083*/                OPC_MoveParent,
/*1084*/                OPC_MoveParent,
/*1085*/                OPC_MoveChild, 1,
/*1087*/                OPC_CheckValueType, MVT::i32,
/*1089*/                OPC_MoveParent,
/*1090*/                OPC_EmitConvertToTarget, 1,
/*1092*/                OPC_EmitNodeXForm, 2, 2, // negX
/*1095*/                OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBLi), 0,
                            1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                        // Src: (sext_inreg:i64 (add:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8neg>><<X:negX>>:$L), i32:Other) - Complexity = 10
                        // Dst: (SUBLi:i64 GPRC:i64:$RA, (negX:i64 (imm:i64)<<P:Predicate_immUExt8neg>>:$L))
/*1104*/              0, /*End of Scope*/
/*1105*/            /*Scope*/ 15, /*->1121*/
/*1106*/              OPC_MoveParent,
/*1107*/              OPC_MoveChild, 1,
/*1109*/              OPC_CheckValueType, MVT::i32,
/*1111*/              OPC_MoveParent,
/*1112*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDLr), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                      // Src: (sext_inreg:i64 (add:i64 GPRC:i64:$RA, GPRC:i64:$RB), i32:Other) - Complexity = 6
                      // Dst: (ADDLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*1121*/            0, /*End of Scope*/
/*1122*/          0, /*End of Scope*/
/*1123*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 32|128,1/*160*/,  TARGET_OPCODE(ISD::SUB),// ->1288
/*1128*/        OPC_Scope, 110, /*->1240*/ // 2 children in Scope
/*1130*/          OPC_MoveChild, 0,
/*1132*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*1135*/          OPC_RecordChild0, // #0 = $RA
/*1136*/          OPC_MoveChild, 1,
/*1138*/          OPC_Scope, 49, /*->1189*/ // 2 children in Scope
/*1140*/            OPC_CheckInteger, 2, 
/*1142*/            OPC_MoveParent,
/*1143*/            OPC_MoveParent,
/*1144*/            OPC_RecordChild1, // #1 = $L
/*1145*/            OPC_Scope, 25, /*->1172*/ // 2 children in Scope
/*1147*/              OPC_MoveChild, 1,
/*1149*/              OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1152*/              OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1154*/              OPC_MoveParent,
/*1155*/              OPC_MoveParent,
/*1156*/              OPC_MoveChild, 1,
/*1158*/              OPC_CheckValueType, MVT::i32,
/*1160*/              OPC_MoveParent,
/*1161*/              OPC_EmitConvertToTarget, 1,
/*1163*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4SUBLi), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                      // Src: (sext_inreg:i64 (sub:i64 (shl:i64 GPRC:i64:$RA, 2:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L), i32:Other) - Complexity = 18
                      // Dst: (S4SUBLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*1172*/            /*Scope*/ 15, /*->1188*/
/*1173*/              OPC_MoveParent,
/*1174*/              OPC_MoveChild, 1,
/*1176*/              OPC_CheckValueType, MVT::i32,
/*1178*/              OPC_MoveParent,
/*1179*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4SUBLr), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                      // Src: (sext_inreg:i64 (sub:i64 (shl:i64 GPRC:i64:$RA, 2:i64), GPRC:i64:$RB), i32:Other) - Complexity = 14
                      // Dst: (S4SUBLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*1188*/            0, /*End of Scope*/
/*1189*/          /*Scope*/ 49, /*->1239*/
/*1190*/            OPC_CheckInteger, 3, 
/*1192*/            OPC_MoveParent,
/*1193*/            OPC_MoveParent,
/*1194*/            OPC_RecordChild1, // #1 = $L
/*1195*/            OPC_Scope, 25, /*->1222*/ // 2 children in Scope
/*1197*/              OPC_MoveChild, 1,
/*1199*/              OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1202*/              OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1204*/              OPC_MoveParent,
/*1205*/              OPC_MoveParent,
/*1206*/              OPC_MoveChild, 1,
/*1208*/              OPC_CheckValueType, MVT::i32,
/*1210*/              OPC_MoveParent,
/*1211*/              OPC_EmitConvertToTarget, 1,
/*1213*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8SUBLi), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                      // Src: (sext_inreg:i64 (sub:i64 (shl:i64 GPRC:i64:$RA, 3:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L), i32:Other) - Complexity = 18
                      // Dst: (S8SUBLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*1222*/            /*Scope*/ 15, /*->1238*/
/*1223*/              OPC_MoveParent,
/*1224*/              OPC_MoveChild, 1,
/*1226*/              OPC_CheckValueType, MVT::i32,
/*1228*/              OPC_MoveParent,
/*1229*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8SUBLr), 0,
                          1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                      // Src: (sext_inreg:i64 (sub:i64 (shl:i64 GPRC:i64:$RA, 3:i64), GPRC:i64:$RB), i32:Other) - Complexity = 14
                      // Dst: (S8SUBLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*1238*/            0, /*End of Scope*/
/*1239*/          0, /*End of Scope*/
/*1240*/        /*Scope*/ 46, /*->1287*/
/*1241*/          OPC_RecordChild0, // #0 = $RA
/*1242*/          OPC_RecordChild1, // #1 = $L
/*1243*/          OPC_Scope, 25, /*->1270*/ // 2 children in Scope
/*1245*/            OPC_MoveChild, 1,
/*1247*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1250*/            OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1252*/            OPC_MoveParent,
/*1253*/            OPC_MoveParent,
/*1254*/            OPC_MoveChild, 1,
/*1256*/            OPC_CheckValueType, MVT::i32,
/*1258*/            OPC_MoveParent,
/*1259*/            OPC_EmitConvertToTarget, 1,
/*1261*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBLi), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                    // Src: (sext_inreg:i64 (sub:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L), i32:Other) - Complexity = 10
                    // Dst: (SUBLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*1270*/          /*Scope*/ 15, /*->1286*/
/*1271*/            OPC_MoveParent,
/*1272*/            OPC_MoveChild, 1,
/*1274*/            OPC_CheckValueType, MVT::i32,
/*1276*/            OPC_MoveParent,
/*1277*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBLr), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                    // Src: (sext_inreg:i64 (sub:i64 GPRC:i64:$RA, GPRC:i64:$RB), i32:Other) - Complexity = 6
                    // Dst: (SUBLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*1286*/          0, /*End of Scope*/
/*1287*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 46,  TARGET_OPCODE(ISD::MUL),// ->1337
/*1291*/        OPC_RecordChild0, // #0 = $RA
/*1292*/        OPC_RecordChild1, // #1 = $L
/*1293*/        OPC_Scope, 25, /*->1320*/ // 2 children in Scope
/*1295*/          OPC_MoveChild, 1,
/*1297*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1300*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1302*/          OPC_MoveParent,
/*1303*/          OPC_MoveParent,
/*1304*/          OPC_MoveChild, 1,
/*1306*/          OPC_CheckValueType, MVT::i32,
/*1308*/          OPC_MoveParent,
/*1309*/          OPC_EmitConvertToTarget, 1,
/*1311*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::MULLi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (sext_inreg:i64 (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L), i32:Other) - Complexity = 10
                  // Dst: (MULLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*1320*/        /*Scope*/ 15, /*->1336*/
/*1321*/          OPC_MoveParent,
/*1322*/          OPC_MoveChild, 1,
/*1324*/          OPC_CheckValueType, MVT::i32,
/*1326*/          OPC_MoveParent,
/*1327*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::MULLr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (sext_inreg:i64 (mul:i64 GPRC:i64:$RA, GPRC:i64:$RB), i32:Other) - Complexity = 6
                  // Dst: (MULLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*1336*/        0, /*End of Scope*/
              0, // EndSwitchOpcode
/*1338*/    /*Scope*/ 45, /*->1384*/
/*1339*/      OPC_RecordChild0, // #0 = $RB
/*1340*/      OPC_MoveChild, 1,
/*1342*/      OPC_Scope, 11, /*->1355*/ // 3 children in Scope
/*1344*/        OPC_CheckValueType, MVT::i8,
/*1346*/        OPC_MoveParent,
/*1347*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SEXTB), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (sext_inreg:i64 GPRC:i64:$RB, i8:Other) - Complexity = 3
                // Dst: (SEXTB:i64 GPRC:i64:$RB)
/*1355*/      /*Scope*/ 11, /*->1367*/
/*1356*/        OPC_CheckValueType, MVT::i16,
/*1358*/        OPC_MoveParent,
/*1359*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SEXTW), 0,
                    1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
                // Src: (sext_inreg:i64 GPRC:i64:$RB, i16:Other) - Complexity = 3
                // Dst: (SEXTW:i64 GPRC:i64:$RB)
/*1367*/      /*Scope*/ 15, /*->1383*/
/*1368*/        OPC_CheckValueType, MVT::i32,
/*1370*/        OPC_MoveParent,
/*1371*/        OPC_EmitInteger, MVT::i64, 0, 
/*1374*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDLi), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (sext_inreg:i64 GPRC:i64:$RB, i32:Other) - Complexity = 3
                // Dst: (ADDLi:i64 GPRC:i64:$RB, 0:i64)
/*1383*/      0, /*End of Scope*/
/*1384*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 104|128,12/*1640*/,  TARGET_OPCODE(ISD::SELECT),// ->3029
/*1389*/    OPC_Scope, 15|128,4/*527*/, /*->1919*/ // 4 children in Scope
/*1392*/      OPC_MoveChild, 0,
/*1394*/      OPC_SwitchOpcode /*3 cases */, 37|128,3/*421*/,  TARGET_OPCODE(ISD::SETCC),// ->1820
/*1399*/        OPC_RecordChild0, // #0 = $RCOND
/*1400*/        OPC_MoveChild, 1,
/*1402*/        OPC_CheckInteger, 0, 
/*1404*/        OPC_MoveParent,
/*1405*/        OPC_MoveChild, 2,
/*1407*/        OPC_Scope, 26, /*->1435*/ // 17 children in Scope
/*1409*/          OPC_CheckCondCode, ISD::SETEQ,
/*1411*/          OPC_MoveParent,
/*1412*/          OPC_MoveParent,
/*1413*/          OPC_RecordChild1, // #1 = $RTRUE
/*1414*/          OPC_MoveChild, 1,
/*1416*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1419*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1421*/          OPC_MoveParent,
/*1422*/          OPC_RecordChild2, // #2 = $RFALSE
/*1423*/          OPC_EmitConvertToTarget, 1,
/*1425*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVEQi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETEQ:Other), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVEQi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1435*/        /*Scope*/ 26, /*->1462*/
/*1436*/          OPC_CheckCondCode, ISD::SETNE,
/*1438*/          OPC_MoveParent,
/*1439*/          OPC_MoveParent,
/*1440*/          OPC_RecordChild1, // #1 = $RTRUE
/*1441*/          OPC_MoveChild, 1,
/*1443*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1446*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1448*/          OPC_MoveParent,
/*1449*/          OPC_RecordChild2, // #2 = $RFALSE
/*1450*/          OPC_EmitConvertToTarget, 1,
/*1452*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVNEi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETNE:Other), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVNEi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1462*/        /*Scope*/ 26, /*->1489*/
/*1463*/          OPC_CheckCondCode, ISD::SETLT,
/*1465*/          OPC_MoveParent,
/*1466*/          OPC_MoveParent,
/*1467*/          OPC_RecordChild1, // #1 = $RTRUE
/*1468*/          OPC_MoveChild, 1,
/*1470*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1473*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1475*/          OPC_MoveParent,
/*1476*/          OPC_RecordChild2, // #2 = $RFALSE
/*1477*/          OPC_EmitConvertToTarget, 1,
/*1479*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLTi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETLT:Other), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVLTi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1489*/        /*Scope*/ 26, /*->1516*/
/*1490*/          OPC_CheckCondCode, ISD::SETLE,
/*1492*/          OPC_MoveParent,
/*1493*/          OPC_MoveParent,
/*1494*/          OPC_RecordChild1, // #1 = $RTRUE
/*1495*/          OPC_MoveChild, 1,
/*1497*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1500*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1502*/          OPC_MoveParent,
/*1503*/          OPC_RecordChild2, // #2 = $RFALSE
/*1504*/          OPC_EmitConvertToTarget, 1,
/*1506*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLEi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETLE:Other), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVLEi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1516*/        /*Scope*/ 26, /*->1543*/
/*1517*/          OPC_CheckCondCode, ISD::SETGT,
/*1519*/          OPC_MoveParent,
/*1520*/          OPC_MoveParent,
/*1521*/          OPC_RecordChild1, // #1 = $RTRUE
/*1522*/          OPC_MoveChild, 1,
/*1524*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1527*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1529*/          OPC_MoveParent,
/*1530*/          OPC_RecordChild2, // #2 = $RFALSE
/*1531*/          OPC_EmitConvertToTarget, 1,
/*1533*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVGTi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETGT:Other), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVGTi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1543*/        /*Scope*/ 26, /*->1570*/
/*1544*/          OPC_CheckCondCode, ISD::SETGE,
/*1546*/          OPC_MoveParent,
/*1547*/          OPC_MoveParent,
/*1548*/          OPC_RecordChild1, // #1 = $RTRUE
/*1549*/          OPC_MoveChild, 1,
/*1551*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1554*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1556*/          OPC_MoveParent,
/*1557*/          OPC_RecordChild2, // #2 = $RFALSE
/*1558*/          OPC_EmitConvertToTarget, 1,
/*1560*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVGEi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETGE:Other), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVGEi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1570*/        /*Scope*/ 26, /*->1597*/
/*1571*/          OPC_CheckCondCode, ISD::SETNE,
/*1573*/          OPC_MoveParent,
/*1574*/          OPC_MoveParent,
/*1575*/          OPC_RecordChild1, // #1 = $RTRUE
/*1576*/          OPC_RecordChild2, // #2 = $RFALSE
/*1577*/          OPC_MoveChild, 2,
/*1579*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1582*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1584*/          OPC_MoveParent,
/*1585*/          OPC_EmitConvertToTarget, 2,
/*1587*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVEQi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 1, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETNE:Other), GPRC:i64:$RTRUE, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE) - Complexity = 15
                  // Dst: (CMOVEQi:i64 GPRC:i64:$RCOND, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE, GPRC:i64:$RTRUE)
/*1597*/        /*Scope*/ 26, /*->1624*/
/*1598*/          OPC_CheckCondCode, ISD::SETGT,
/*1600*/          OPC_MoveParent,
/*1601*/          OPC_MoveParent,
/*1602*/          OPC_RecordChild1, // #1 = $RTRUE
/*1603*/          OPC_RecordChild2, // #2 = $RFALSE
/*1604*/          OPC_MoveChild, 2,
/*1606*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1609*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1611*/          OPC_MoveParent,
/*1612*/          OPC_EmitConvertToTarget, 2,
/*1614*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLEi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 1, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETGT:Other), GPRC:i64:$RTRUE, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE) - Complexity = 15
                  // Dst: (CMOVLEi:i64 GPRC:i64:$RCOND, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE, GPRC:i64:$RTRUE)
/*1624*/        /*Scope*/ 26, /*->1651*/
/*1625*/          OPC_CheckCondCode, ISD::SETGE,
/*1627*/          OPC_MoveParent,
/*1628*/          OPC_MoveParent,
/*1629*/          OPC_RecordChild1, // #1 = $RTRUE
/*1630*/          OPC_RecordChild2, // #2 = $RFALSE
/*1631*/          OPC_MoveChild, 2,
/*1633*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1636*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1638*/          OPC_MoveParent,
/*1639*/          OPC_EmitConvertToTarget, 2,
/*1641*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLTi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 1, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETGE:Other), GPRC:i64:$RTRUE, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE) - Complexity = 15
                  // Dst: (CMOVLTi:i64 GPRC:i64:$RCOND, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE, GPRC:i64:$RTRUE)
/*1651*/        /*Scope*/ 26, /*->1678*/
/*1652*/          OPC_CheckCondCode, ISD::SETLT,
/*1654*/          OPC_MoveParent,
/*1655*/          OPC_MoveParent,
/*1656*/          OPC_RecordChild1, // #1 = $RTRUE
/*1657*/          OPC_RecordChild2, // #2 = $RFALSE
/*1658*/          OPC_MoveChild, 2,
/*1660*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1663*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1665*/          OPC_MoveParent,
/*1666*/          OPC_EmitConvertToTarget, 2,
/*1668*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVGEi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 1, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETLT:Other), GPRC:i64:$RTRUE, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE) - Complexity = 15
                  // Dst: (CMOVGEi:i64 GPRC:i64:$RCOND, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE, GPRC:i64:$RTRUE)
/*1678*/        /*Scope*/ 26, /*->1705*/
/*1679*/          OPC_CheckCondCode, ISD::SETLE,
/*1681*/          OPC_MoveParent,
/*1682*/          OPC_MoveParent,
/*1683*/          OPC_RecordChild1, // #1 = $RTRUE
/*1684*/          OPC_RecordChild2, // #2 = $RFALSE
/*1685*/          OPC_MoveChild, 2,
/*1687*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1690*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1692*/          OPC_MoveParent,
/*1693*/          OPC_EmitConvertToTarget, 2,
/*1695*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVGTi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 1, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETLE:Other), GPRC:i64:$RTRUE, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE) - Complexity = 15
                  // Dst: (CMOVGTi:i64 GPRC:i64:$RCOND, (imm:i64)<<P:Predicate_immUExt8>>:$RFALSE, GPRC:i64:$RTRUE)
/*1705*/        /*Scope*/ 18, /*->1724*/
/*1706*/          OPC_CheckCondCode, ISD::SETEQ,
/*1708*/          OPC_MoveParent,
/*1709*/          OPC_MoveParent,
/*1710*/          OPC_RecordChild1, // #1 = $RTRUE
/*1711*/          OPC_RecordChild2, // #2 = $RFALSE
/*1712*/          OPC_CheckType, MVT::i64,
/*1714*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVEQr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETEQ:Other), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVEQr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1724*/        /*Scope*/ 18, /*->1743*/
/*1725*/          OPC_CheckCondCode, ISD::SETNE,
/*1727*/          OPC_MoveParent,
/*1728*/          OPC_MoveParent,
/*1729*/          OPC_RecordChild1, // #1 = $RTRUE
/*1730*/          OPC_RecordChild2, // #2 = $RFALSE
/*1731*/          OPC_CheckType, MVT::i64,
/*1733*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVNEr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETNE:Other), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVNEr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1743*/        /*Scope*/ 18, /*->1762*/
/*1744*/          OPC_CheckCondCode, ISD::SETLT,
/*1746*/          OPC_MoveParent,
/*1747*/          OPC_MoveParent,
/*1748*/          OPC_RecordChild1, // #1 = $RTRUE
/*1749*/          OPC_RecordChild2, // #2 = $RFALSE
/*1750*/          OPC_CheckType, MVT::i64,
/*1752*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLTr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETLT:Other), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVLTr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1762*/        /*Scope*/ 18, /*->1781*/
/*1763*/          OPC_CheckCondCode, ISD::SETLE,
/*1765*/          OPC_MoveParent,
/*1766*/          OPC_MoveParent,
/*1767*/          OPC_RecordChild1, // #1 = $RTRUE
/*1768*/          OPC_RecordChild2, // #2 = $RFALSE
/*1769*/          OPC_CheckType, MVT::i64,
/*1771*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLEr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETLE:Other), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVLEr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1781*/        /*Scope*/ 18, /*->1800*/
/*1782*/          OPC_CheckCondCode, ISD::SETGT,
/*1784*/          OPC_MoveParent,
/*1785*/          OPC_MoveParent,
/*1786*/          OPC_RecordChild1, // #1 = $RTRUE
/*1787*/          OPC_RecordChild2, // #2 = $RFALSE
/*1788*/          OPC_CheckType, MVT::i64,
/*1790*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVGTr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETGT:Other), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVGTr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1800*/        /*Scope*/ 18, /*->1819*/
/*1801*/          OPC_CheckCondCode, ISD::SETGE,
/*1803*/          OPC_MoveParent,
/*1804*/          OPC_MoveParent,
/*1805*/          OPC_RecordChild1, // #1 = $RTRUE
/*1806*/          OPC_RecordChild2, // #2 = $RFALSE
/*1807*/          OPC_CheckType, MVT::i64,
/*1809*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVGEr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (setcc:i64 GPRC:i64:$RCOND, 0:i64, SETGE:Other), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVGEr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1819*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 46,  TARGET_OPCODE(ISD::XOR),// ->1869
/*1823*/        OPC_RecordChild0, // #0 = $RCOND
/*1824*/        OPC_MoveChild, 1,
/*1826*/        OPC_CheckInteger, 1, 
/*1828*/        OPC_MoveParent,
/*1829*/        OPC_MoveParent,
/*1830*/        OPC_RecordChild1, // #1 = $RTRUE
/*1831*/        OPC_Scope, 21, /*->1854*/ // 2 children in Scope
/*1833*/          OPC_MoveChild, 1,
/*1835*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1838*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1840*/          OPC_MoveParent,
/*1841*/          OPC_RecordChild2, // #2 = $RFALSE
/*1842*/          OPC_EmitConvertToTarget, 1,
/*1844*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLBCi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (xor:i64 GPRC:i64:$RCOND, 1:i64), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVLBCi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1854*/        /*Scope*/ 13, /*->1868*/
/*1855*/          OPC_RecordChild2, // #2 = $RFALSE
/*1856*/          OPC_CheckType, MVT::i64,
/*1858*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLBCr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (xor:i64 GPRC:i64:$RCOND, 1:i64), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVLBCr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1868*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 46,  TARGET_OPCODE(ISD::AND),// ->1918
/*1872*/        OPC_RecordChild0, // #0 = $RCOND
/*1873*/        OPC_MoveChild, 1,
/*1875*/        OPC_CheckInteger, 1, 
/*1877*/        OPC_MoveParent,
/*1878*/        OPC_MoveParent,
/*1879*/        OPC_RecordChild1, // #1 = $RTRUE
/*1880*/        OPC_Scope, 21, /*->1903*/ // 2 children in Scope
/*1882*/          OPC_MoveChild, 1,
/*1884*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1887*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1889*/          OPC_MoveParent,
/*1890*/          OPC_RecordChild2, // #2 = $RFALSE
/*1891*/          OPC_EmitConvertToTarget, 1,
/*1893*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLBSi), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 3, 2, 
                  // Src: (select:i64 (and:i64 GPRC:i64:$RCOND, 1:i64), (imm:i64)<<P:Predicate_immUExt8>>:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 15
                  // Dst: (CMOVLBSi:i64 GPRC:i64:$RCOND, (imm:i64):$RTRUE, GPRC:i64:$RFALSE)
/*1903*/        /*Scope*/ 13, /*->1917*/
/*1904*/          OPC_RecordChild2, // #2 = $RFALSE
/*1905*/          OPC_CheckType, MVT::i64,
/*1907*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVLBSr), 0,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 1, 2, 
                  // Src: (select:i64 (and:i64 GPRC:i64:$RCOND, 1:i64), GPRC:i64:$RTRUE, GPRC:i64:$RFALSE) - Complexity = 11
                  // Dst: (CMOVLBSr:i64 GPRC:i64:$RCOND, GPRC:i64:$RTRUE, GPRC:i64:$RFALSE)
/*1917*/        0, /*End of Scope*/
              0, // EndSwitchOpcode
/*1919*/    /*Scope*/ 39, /*->1959*/
/*1920*/      OPC_RecordChild0, // #0 = $which
/*1921*/      OPC_RecordChild1, // #1 = $src1
/*1922*/      OPC_RecordChild2, // #2 = $src2
/*1923*/      OPC_Scope, 20, /*->1945*/ // 2 children in Scope
/*1925*/        OPC_MoveChild, 2,
/*1927*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*1930*/        OPC_CheckPredicate, 0, // Predicate_immUExt8
/*1932*/        OPC_MoveParent,
/*1933*/        OPC_EmitConvertToTarget, 2,
/*1935*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVEQi), 0,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 0, 
                // Src: (select:i64 GPRC:i64:$which, GPRC:i64:$src1, (imm:i64)<<P:Predicate_immUExt8>>:$src2) - Complexity = 7
                // Dst: (CMOVEQi:i64 GPRC:i64:$src1, (imm:i64)<<P:Predicate_immUExt8>>:$src2, GPRC:i64:$which)
/*1945*/      /*Scope*/ 12, /*->1958*/
/*1946*/        OPC_CheckType, MVT::i64,
/*1948*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CMOVNEr), 0,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 2, 1, 0, 
                // Src: (select:i64 GPRC:i64:$which, GPRC:i64:$src1, GPRC:i64:$src2) - Complexity = 3
                // Dst: (CMOVNEr:i64 GPRC:i64:$src2, GPRC:i64:$src1, GPRC:i64:$which)
/*1958*/      0, /*End of Scope*/
/*1959*/    /*Scope*/ 125|128,7/*1021*/, /*->2982*/
/*1961*/      OPC_MoveChild, 0,
/*1963*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::SETCC),
/*1966*/      OPC_RecordChild0, // #0 = $RA
/*1967*/      OPC_CheckChild0Type, MVT::f64,
/*1969*/      OPC_RecordChild1, // #1 = $RB
/*1970*/      OPC_MoveChild, 2,
/*1972*/      OPC_Scope, 27, /*->2001*/ // 36 children in Scope
/*1974*/        OPC_CheckCondCode, ISD::SETEQ,
/*1976*/        OPC_MoveParent,
/*1977*/        OPC_MoveParent,
/*1978*/        OPC_RecordChild1, // #2 = $st
/*1979*/        OPC_RecordChild2, // #3 = $sf
/*1980*/        OPC_CheckType, MVT::f64,
/*1982*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*1991*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETEQ:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2001*/      /*Scope*/ 27, /*->2029*/
/*2002*/        OPC_CheckCondCode, ISD::SETOEQ,
/*2004*/        OPC_MoveParent,
/*2005*/        OPC_MoveParent,
/*2006*/        OPC_RecordChild1, // #2 = $st
/*2007*/        OPC_RecordChild2, // #3 = $sf
/*2008*/        OPC_CheckType, MVT::f64,
/*2010*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2019*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOEQ:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2029*/      /*Scope*/ 27, /*->2057*/
/*2030*/        OPC_CheckCondCode, ISD::SETUEQ,
/*2032*/        OPC_MoveParent,
/*2033*/        OPC_MoveParent,
/*2034*/        OPC_RecordChild1, // #2 = $st
/*2035*/        OPC_RecordChild2, // #3 = $sf
/*2036*/        OPC_CheckType, MVT::f64,
/*2038*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2047*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUEQ:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2057*/      /*Scope*/ 27, /*->2085*/
/*2058*/        OPC_CheckCondCode, ISD::SETNE,
/*2060*/        OPC_MoveParent,
/*2061*/        OPC_MoveParent,
/*2062*/        OPC_RecordChild1, // #2 = $st
/*2063*/        OPC_RecordChild2, // #3 = $sf
/*2064*/        OPC_CheckType, MVT::f64,
/*2066*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2075*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQT), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETNE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVEQT:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2085*/      /*Scope*/ 27, /*->2113*/
/*2086*/        OPC_CheckCondCode, ISD::SETONE,
/*2088*/        OPC_MoveParent,
/*2089*/        OPC_MoveParent,
/*2090*/        OPC_RecordChild1, // #2 = $st
/*2091*/        OPC_RecordChild2, // #3 = $sf
/*2092*/        OPC_CheckType, MVT::f64,
/*2094*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2103*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQT), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETONE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVEQT:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2113*/      /*Scope*/ 27, /*->2141*/
/*2114*/        OPC_CheckCondCode, ISD::SETUNE,
/*2116*/        OPC_MoveParent,
/*2117*/        OPC_MoveParent,
/*2118*/        OPC_RecordChild1, // #2 = $st
/*2119*/        OPC_RecordChild2, // #3 = $sf
/*2120*/        OPC_CheckType, MVT::f64,
/*2122*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2131*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQT), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUNE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVEQT:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2141*/      /*Scope*/ 27, /*->2169*/
/*2142*/        OPC_CheckCondCode, ISD::SETGT,
/*2144*/        OPC_MoveParent,
/*2145*/        OPC_MoveParent,
/*2146*/        OPC_RecordChild1, // #2 = $st
/*2147*/        OPC_RecordChild2, // #3 = $sf
/*2148*/        OPC_CheckType, MVT::f64,
/*2150*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2159*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETGT:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2169*/      /*Scope*/ 27, /*->2197*/
/*2170*/        OPC_CheckCondCode, ISD::SETOGT,
/*2172*/        OPC_MoveParent,
/*2173*/        OPC_MoveParent,
/*2174*/        OPC_RecordChild1, // #2 = $st
/*2175*/        OPC_RecordChild2, // #3 = $sf
/*2176*/        OPC_CheckType, MVT::f64,
/*2178*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2187*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOGT:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2197*/      /*Scope*/ 27, /*->2225*/
/*2198*/        OPC_CheckCondCode, ISD::SETUGT,
/*2200*/        OPC_MoveParent,
/*2201*/        OPC_MoveParent,
/*2202*/        OPC_RecordChild1, // #2 = $st
/*2203*/        OPC_RecordChild2, // #3 = $sf
/*2204*/        OPC_CheckType, MVT::f64,
/*2206*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2215*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUGT:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2225*/      /*Scope*/ 27, /*->2253*/
/*2226*/        OPC_CheckCondCode, ISD::SETGE,
/*2228*/        OPC_MoveParent,
/*2229*/        OPC_MoveParent,
/*2230*/        OPC_RecordChild1, // #2 = $st
/*2231*/        OPC_RecordChild2, // #3 = $sf
/*2232*/        OPC_CheckType, MVT::f64,
/*2234*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2243*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETGE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2253*/      /*Scope*/ 27, /*->2281*/
/*2254*/        OPC_CheckCondCode, ISD::SETOGE,
/*2256*/        OPC_MoveParent,
/*2257*/        OPC_MoveParent,
/*2258*/        OPC_RecordChild1, // #2 = $st
/*2259*/        OPC_RecordChild2, // #3 = $sf
/*2260*/        OPC_CheckType, MVT::f64,
/*2262*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2271*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOGE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2281*/      /*Scope*/ 27, /*->2309*/
/*2282*/        OPC_CheckCondCode, ISD::SETUGE,
/*2284*/        OPC_MoveParent,
/*2285*/        OPC_MoveParent,
/*2286*/        OPC_RecordChild1, // #2 = $st
/*2287*/        OPC_RecordChild2, // #3 = $sf
/*2288*/        OPC_CheckType, MVT::f64,
/*2290*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2299*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUGE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2309*/      /*Scope*/ 27, /*->2337*/
/*2310*/        OPC_CheckCondCode, ISD::SETLT,
/*2312*/        OPC_MoveParent,
/*2313*/        OPC_MoveParent,
/*2314*/        OPC_RecordChild1, // #2 = $st
/*2315*/        OPC_RecordChild2, // #3 = $sf
/*2316*/        OPC_CheckType, MVT::f64,
/*2318*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2327*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETLT:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2337*/      /*Scope*/ 27, /*->2365*/
/*2338*/        OPC_CheckCondCode, ISD::SETOLT,
/*2340*/        OPC_MoveParent,
/*2341*/        OPC_MoveParent,
/*2342*/        OPC_RecordChild1, // #2 = $st
/*2343*/        OPC_RecordChild2, // #3 = $sf
/*2344*/        OPC_CheckType, MVT::f64,
/*2346*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2355*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOLT:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2365*/      /*Scope*/ 27, /*->2393*/
/*2366*/        OPC_CheckCondCode, ISD::SETULT,
/*2368*/        OPC_MoveParent,
/*2369*/        OPC_MoveParent,
/*2370*/        OPC_RecordChild1, // #2 = $st
/*2371*/        OPC_RecordChild2, // #3 = $sf
/*2372*/        OPC_CheckType, MVT::f64,
/*2374*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2383*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETULT:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2393*/      /*Scope*/ 27, /*->2421*/
/*2394*/        OPC_CheckCondCode, ISD::SETLE,
/*2396*/        OPC_MoveParent,
/*2397*/        OPC_MoveParent,
/*2398*/        OPC_RecordChild1, // #2 = $st
/*2399*/        OPC_RecordChild2, // #3 = $sf
/*2400*/        OPC_CheckType, MVT::f64,
/*2402*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2411*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETLE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2421*/      /*Scope*/ 27, /*->2449*/
/*2422*/        OPC_CheckCondCode, ISD::SETOLE,
/*2424*/        OPC_MoveParent,
/*2425*/        OPC_MoveParent,
/*2426*/        OPC_RecordChild1, // #2 = $st
/*2427*/        OPC_RecordChild2, // #3 = $sf
/*2428*/        OPC_CheckType, MVT::f64,
/*2430*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2439*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOLE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2449*/      /*Scope*/ 27, /*->2477*/
/*2450*/        OPC_CheckCondCode, ISD::SETULE,
/*2452*/        OPC_MoveParent,
/*2453*/        OPC_MoveParent,
/*2454*/        OPC_RecordChild1, // #2 = $st
/*2455*/        OPC_RecordChild2, // #3 = $sf
/*2456*/        OPC_CheckType, MVT::f64,
/*2458*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2467*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNET), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f64 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETULE:Other), F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 6
                // Dst: (FCMOVNET:f64 F8RC:f64:$sf, F8RC:f64:$st, (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2477*/      /*Scope*/ 27, /*->2505*/
/*2478*/        OPC_CheckCondCode, ISD::SETEQ,
/*2480*/        OPC_MoveParent,
/*2481*/        OPC_MoveParent,
/*2482*/        OPC_RecordChild1, // #2 = $st
/*2483*/        OPC_RecordChild2, // #3 = $sf
/*2484*/        OPC_CheckType, MVT::f32,
/*2486*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2495*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETEQ:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2505*/      /*Scope*/ 27, /*->2533*/
/*2506*/        OPC_CheckCondCode, ISD::SETOEQ,
/*2508*/        OPC_MoveParent,
/*2509*/        OPC_MoveParent,
/*2510*/        OPC_RecordChild1, // #2 = $st
/*2511*/        OPC_RecordChild2, // #3 = $sf
/*2512*/        OPC_CheckType, MVT::f32,
/*2514*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2523*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOEQ:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2533*/      /*Scope*/ 27, /*->2561*/
/*2534*/        OPC_CheckCondCode, ISD::SETUEQ,
/*2536*/        OPC_MoveParent,
/*2537*/        OPC_MoveParent,
/*2538*/        OPC_RecordChild1, // #2 = $st
/*2539*/        OPC_RecordChild2, // #3 = $sf
/*2540*/        OPC_CheckType, MVT::f32,
/*2542*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2551*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUEQ:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2561*/      /*Scope*/ 27, /*->2589*/
/*2562*/        OPC_CheckCondCode, ISD::SETNE,
/*2564*/        OPC_MoveParent,
/*2565*/        OPC_MoveParent,
/*2566*/        OPC_RecordChild1, // #2 = $st
/*2567*/        OPC_RecordChild2, // #3 = $sf
/*2568*/        OPC_CheckType, MVT::f32,
/*2570*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2579*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQS), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETNE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVEQS:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2589*/      /*Scope*/ 27, /*->2617*/
/*2590*/        OPC_CheckCondCode, ISD::SETONE,
/*2592*/        OPC_MoveParent,
/*2593*/        OPC_MoveParent,
/*2594*/        OPC_RecordChild1, // #2 = $st
/*2595*/        OPC_RecordChild2, // #3 = $sf
/*2596*/        OPC_CheckType, MVT::f32,
/*2598*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2607*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQS), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETONE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVEQS:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2617*/      /*Scope*/ 27, /*->2645*/
/*2618*/        OPC_CheckCondCode, ISD::SETUNE,
/*2620*/        OPC_MoveParent,
/*2621*/        OPC_MoveParent,
/*2622*/        OPC_RecordChild1, // #2 = $st
/*2623*/        OPC_RecordChild2, // #3 = $sf
/*2624*/        OPC_CheckType, MVT::f32,
/*2626*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2635*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQS), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUNE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVEQS:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2645*/      /*Scope*/ 27, /*->2673*/
/*2646*/        OPC_CheckCondCode, ISD::SETGT,
/*2648*/        OPC_MoveParent,
/*2649*/        OPC_MoveParent,
/*2650*/        OPC_RecordChild1, // #2 = $st
/*2651*/        OPC_RecordChild2, // #3 = $sf
/*2652*/        OPC_CheckType, MVT::f32,
/*2654*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2663*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETGT:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2673*/      /*Scope*/ 27, /*->2701*/
/*2674*/        OPC_CheckCondCode, ISD::SETOGT,
/*2676*/        OPC_MoveParent,
/*2677*/        OPC_MoveParent,
/*2678*/        OPC_RecordChild1, // #2 = $st
/*2679*/        OPC_RecordChild2, // #3 = $sf
/*2680*/        OPC_CheckType, MVT::f32,
/*2682*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2691*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOGT:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2701*/      /*Scope*/ 27, /*->2729*/
/*2702*/        OPC_CheckCondCode, ISD::SETUGT,
/*2704*/        OPC_MoveParent,
/*2705*/        OPC_MoveParent,
/*2706*/        OPC_RecordChild1, // #2 = $st
/*2707*/        OPC_RecordChild2, // #3 = $sf
/*2708*/        OPC_CheckType, MVT::f32,
/*2710*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2719*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUGT:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2729*/      /*Scope*/ 27, /*->2757*/
/*2730*/        OPC_CheckCondCode, ISD::SETGE,
/*2732*/        OPC_MoveParent,
/*2733*/        OPC_MoveParent,
/*2734*/        OPC_RecordChild1, // #2 = $st
/*2735*/        OPC_RecordChild2, // #3 = $sf
/*2736*/        OPC_CheckType, MVT::f32,
/*2738*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2747*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETGE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2757*/      /*Scope*/ 27, /*->2785*/
/*2758*/        OPC_CheckCondCode, ISD::SETOGE,
/*2760*/        OPC_MoveParent,
/*2761*/        OPC_MoveParent,
/*2762*/        OPC_RecordChild1, // #2 = $st
/*2763*/        OPC_RecordChild2, // #3 = $sf
/*2764*/        OPC_CheckType, MVT::f32,
/*2766*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2775*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOGE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2785*/      /*Scope*/ 27, /*->2813*/
/*2786*/        OPC_CheckCondCode, ISD::SETUGE,
/*2788*/        OPC_MoveParent,
/*2789*/        OPC_MoveParent,
/*2790*/        OPC_RecordChild1, // #2 = $st
/*2791*/        OPC_RecordChild2, // #3 = $sf
/*2792*/        OPC_CheckType, MVT::f32,
/*2794*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0,  // Results = #4 
/*2803*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUGE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA))
/*2813*/      /*Scope*/ 27, /*->2841*/
/*2814*/        OPC_CheckCondCode, ISD::SETLT,
/*2816*/        OPC_MoveParent,
/*2817*/        OPC_MoveParent,
/*2818*/        OPC_RecordChild1, // #2 = $st
/*2819*/        OPC_RecordChild2, // #3 = $sf
/*2820*/        OPC_CheckType, MVT::f32,
/*2822*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2831*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETLT:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2841*/      /*Scope*/ 27, /*->2869*/
/*2842*/        OPC_CheckCondCode, ISD::SETOLT,
/*2844*/        OPC_MoveParent,
/*2845*/        OPC_MoveParent,
/*2846*/        OPC_RecordChild1, // #2 = $st
/*2847*/        OPC_RecordChild2, // #3 = $sf
/*2848*/        OPC_CheckType, MVT::f32,
/*2850*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2859*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOLT:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2869*/      /*Scope*/ 27, /*->2897*/
/*2870*/        OPC_CheckCondCode, ISD::SETULT,
/*2872*/        OPC_MoveParent,
/*2873*/        OPC_MoveParent,
/*2874*/        OPC_RecordChild1, // #2 = $st
/*2875*/        OPC_RecordChild2, // #3 = $sf
/*2876*/        OPC_CheckType, MVT::f32,
/*2878*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2887*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETULT:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2897*/      /*Scope*/ 27, /*->2925*/
/*2898*/        OPC_CheckCondCode, ISD::SETLE,
/*2900*/        OPC_MoveParent,
/*2901*/        OPC_MoveParent,
/*2902*/        OPC_RecordChild1, // #2 = $st
/*2903*/        OPC_RecordChild2, // #3 = $sf
/*2904*/        OPC_CheckType, MVT::f32,
/*2906*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2915*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETLE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2925*/      /*Scope*/ 27, /*->2953*/
/*2926*/        OPC_CheckCondCode, ISD::SETOLE,
/*2928*/        OPC_MoveParent,
/*2929*/        OPC_MoveParent,
/*2930*/        OPC_RecordChild1, // #2 = $st
/*2931*/        OPC_RecordChild2, // #3 = $sf
/*2932*/        OPC_CheckType, MVT::f32,
/*2934*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2943*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOLE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2953*/      /*Scope*/ 27, /*->2981*/
/*2954*/        OPC_CheckCondCode, ISD::SETULE,
/*2956*/        OPC_MoveParent,
/*2957*/        OPC_MoveParent,
/*2958*/        OPC_RecordChild1, // #2 = $st
/*2959*/        OPC_RecordChild2, // #3 = $sf
/*2960*/        OPC_CheckType, MVT::f32,
/*2962*/        OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1,  // Results = #4 
/*2971*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVNES), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 3, 2, 4, 
                // Src: (select:f32 (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETULE:Other), F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 6
                // Dst: (FCMOVNES:f32 F4RC:f32:$sf, F4RC:f32:$st, (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB))
/*2981*/      0, /*End of Scope*/
/*2982*/    /*Scope*/ 45, /*->3028*/
/*2983*/      OPC_RecordChild0, // #0 = $RC
/*2984*/      OPC_RecordChild1, // #1 = $st
/*2985*/      OPC_RecordChild2, // #2 = $sf
/*2986*/      OPC_SwitchType /*2 cases */, 18,  MVT::f64,// ->3007
/*2989*/        OPC_EmitNode, TARGET_OPCODE(Alpha::ITOFT), 0,
                    1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0,  // Results = #3 
/*2997*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQT), 0,
                    1/*#VTs*/, MVT::f64, 3/*#Ops*/, 1, 2, 3, 
                // Src: (select:f64 GPRC:i64:$RC, F8RC:f64:$st, F8RC:f64:$sf) - Complexity = 3
                // Dst: (FCMOVEQT:f64 F8RC:f64:$st, F8RC:f64:$sf, (ITOFT:f64 GPRC:i64:$RC))
              /*SwitchType*/ 18,  MVT::f32,// ->3027
/*3009*/        OPC_EmitNode, TARGET_OPCODE(Alpha::ITOFT), 0,
                    1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0,  // Results = #3 
/*3017*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FCMOVEQS), 0,
                    1/*#VTs*/, MVT::f32, 3/*#Ops*/, 1, 2, 3, 
                // Src: (select:f32 GPRC:i64:$RC, F4RC:f32:$st, F4RC:f32:$sf) - Complexity = 3
                // Dst: (FCMOVEQS:f32 F4RC:f32:$st, F4RC:f32:$sf, (ITOFT:f64 GPRC:i64:$RC))
              0, // EndSwitchType
/*3028*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 118|128,1/*246*/,  TARGET_OPCODE(ISD::ADD),// ->3279
/*3033*/    OPC_Scope, 0|128,1/*128*/, /*->3164*/ // 2 children in Scope
/*3036*/      OPC_MoveChild, 0,
/*3038*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*3041*/      OPC_RecordChild0, // #0 = $RA
/*3042*/      OPC_MoveChild, 1,
/*3044*/      OPC_Scope, 58, /*->3104*/ // 2 children in Scope
/*3046*/        OPC_CheckInteger, 2, 
/*3048*/        OPC_MoveParent,
/*3049*/        OPC_MoveParent,
/*3050*/        OPC_RecordChild1, // #1 = $L
/*3051*/        OPC_Scope, 40, /*->3093*/ // 2 children in Scope
/*3053*/          OPC_MoveChild, 1,
/*3055*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3058*/          OPC_Scope, 14, /*->3074*/ // 2 children in Scope
/*3060*/            OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3062*/            OPC_MoveParent,
/*3063*/            OPC_EmitConvertToTarget, 1,
/*3065*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDQi), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                    // Src: (add:i64 (shl:i64 GPRC:i64:$RA, 2:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 15
                    // Dst: (S4ADDQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3074*/          /*Scope*/ 17, /*->3092*/
/*3075*/            OPC_CheckPredicate, 3, // Predicate_immUExt8neg
/*3077*/            OPC_MoveParent,
/*3078*/            OPC_EmitConvertToTarget, 1,
/*3080*/            OPC_EmitNodeXForm, 2, 2, // negX
/*3083*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4SUBQi), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                    // Src: (add:i64 (shl:i64 GPRC:i64:$RA, 2:i64), (imm:i64)<<P:Predicate_immUExt8neg>><<X:negX>>:$L) - Complexity = 15
                    // Dst: (S4SUBQi:i64 GPRC:i64:$RA, (negX:i64 (imm:i64)<<P:Predicate_immUExt8neg>>:$L))
/*3092*/          0, /*End of Scope*/
/*3093*/        /*Scope*/ 9, /*->3103*/
/*3094*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i64 (shl:i64 GPRC:i64:$RA, 2:i64), GPRC:i64:$RB) - Complexity = 11
                  // Dst: (S4ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3103*/        0, /*End of Scope*/
/*3104*/      /*Scope*/ 58, /*->3163*/
/*3105*/        OPC_CheckInteger, 3, 
/*3107*/        OPC_MoveParent,
/*3108*/        OPC_MoveParent,
/*3109*/        OPC_RecordChild1, // #1 = $L
/*3110*/        OPC_Scope, 40, /*->3152*/ // 2 children in Scope
/*3112*/          OPC_MoveChild, 1,
/*3114*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3117*/          OPC_Scope, 14, /*->3133*/ // 2 children in Scope
/*3119*/            OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3121*/            OPC_MoveParent,
/*3122*/            OPC_EmitConvertToTarget, 1,
/*3124*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8ADDQi), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                    // Src: (add:i64 (shl:i64 GPRC:i64:$RA, 3:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 15
                    // Dst: (S8ADDQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3133*/          /*Scope*/ 17, /*->3151*/
/*3134*/            OPC_CheckPredicate, 3, // Predicate_immUExt8neg
/*3136*/            OPC_MoveParent,
/*3137*/            OPC_EmitConvertToTarget, 1,
/*3139*/            OPC_EmitNodeXForm, 2, 2, // negX
/*3142*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8SUBQi), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                    // Src: (add:i64 (shl:i64 GPRC:i64:$RA, 3:i64), (imm:i64)<<P:Predicate_immUExt8neg>><<X:negX>>:$L) - Complexity = 15
                    // Dst: (S8SUBQi:i64 GPRC:i64:$RA, (negX:i64 (imm:i64)<<P:Predicate_immUExt8neg>>:$L))
/*3151*/          0, /*End of Scope*/
/*3152*/        /*Scope*/ 9, /*->3162*/
/*3153*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i64 (shl:i64 GPRC:i64:$RA, 3:i64), GPRC:i64:$RB) - Complexity = 11
                  // Dst: (S8ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3162*/        0, /*End of Scope*/
/*3163*/      0, /*End of Scope*/
/*3164*/    /*Scope*/ 113, /*->3278*/
/*3165*/      OPC_RecordChild0, // #0 = $RB
/*3166*/      OPC_Scope, 38, /*->3206*/ // 2 children in Scope
/*3168*/        OPC_MoveChild, 1,
/*3170*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*3173*/        OPC_RecordChild0, // #1 = $RA
/*3174*/        OPC_MoveChild, 1,
/*3176*/        OPC_Scope, 13, /*->3191*/ // 2 children in Scope
/*3178*/          OPC_CheckInteger, 2, 
/*3180*/          OPC_MoveParent,
/*3181*/          OPC_MoveParent,
/*3182*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (add:i64 GPRC:i64:$RB, (shl:i64 GPRC:i64:$RA, 2:i64)) - Complexity = 11
                  // Dst: (S4ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3191*/        /*Scope*/ 13, /*->3205*/
/*3192*/          OPC_CheckInteger, 3, 
/*3194*/          OPC_MoveParent,
/*3195*/          OPC_MoveParent,
/*3196*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
                  // Src: (add:i64 GPRC:i64:$RB, (shl:i64 GPRC:i64:$RA, 3:i64)) - Complexity = 11
                  // Dst: (S8ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3205*/        0, /*End of Scope*/
/*3206*/      /*Scope*/ 70, /*->3277*/
/*3207*/        OPC_RecordChild1, // #1 = $L
/*3208*/        OPC_Scope, 56, /*->3266*/ // 2 children in Scope
/*3210*/          OPC_MoveChild, 1,
/*3212*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3215*/          OPC_Scope, 14, /*->3231*/ // 3 children in Scope
/*3217*/            OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3219*/            OPC_MoveParent,
/*3220*/            OPC_EmitConvertToTarget, 1,
/*3222*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDQi), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                    // Src: (add:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
                    // Dst: (ADDQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3231*/          /*Scope*/ 17, /*->3249*/
/*3232*/            OPC_CheckPredicate, 3, // Predicate_immUExt8neg
/*3234*/            OPC_MoveParent,
/*3235*/            OPC_EmitConvertToTarget, 1,
/*3237*/            OPC_EmitNodeXForm, 2, 2, // negX
/*3240*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBQi), 0,
                        1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                    // Src: (add:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8neg>><<X:negX>>:$L) - Complexity = 7
                    // Dst: (SUBQi:i64 GPRC:i64:$RA, (negX:i64 (imm:i64)<<P:Predicate_immUExt8neg>>:$L))
/*3249*/          /*Scope*/ 15, /*->3265*/
/*3250*/            OPC_CheckPredicate, 4, // Predicate_immSExt16
/*3252*/            OPC_MoveParent,
/*3253*/            OPC_EmitConvertToTarget, 1,
/*3255*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDA), 0,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 0, 
                    // Src: (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP) - Complexity = 7
                    // Dst: (LDA:i64:i64 (imm:i64):$DISP, GPRC:i64:$RB)
/*3265*/          0, /*End of Scope*/
/*3266*/        /*Scope*/ 9, /*->3276*/
/*3267*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (add:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
                  // Dst: (ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3276*/        0, /*End of Scope*/
/*3277*/      0, /*End of Scope*/
/*3278*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 124,  TARGET_OPCODE(ISD::SUB),// ->3406
/*3282*/    OPC_Scope, 86, /*->3370*/ // 2 children in Scope
/*3284*/      OPC_MoveChild, 0,
/*3286*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::SHL),
/*3289*/      OPC_RecordChild0, // #0 = $RA
/*3290*/      OPC_MoveChild, 1,
/*3292*/      OPC_Scope, 37, /*->3331*/ // 2 children in Scope
/*3294*/        OPC_CheckInteger, 2, 
/*3296*/        OPC_MoveParent,
/*3297*/        OPC_MoveParent,
/*3298*/        OPC_RecordChild1, // #1 = $L
/*3299*/        OPC_Scope, 19, /*->3320*/ // 2 children in Scope
/*3301*/          OPC_MoveChild, 1,
/*3303*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3306*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3308*/          OPC_MoveParent,
/*3309*/          OPC_EmitConvertToTarget, 1,
/*3311*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4SUBQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (sub:i64 (shl:i64 GPRC:i64:$RA, 2:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 15
                  // Dst: (S4SUBQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3320*/        /*Scope*/ 9, /*->3330*/
/*3321*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4SUBQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (sub:i64 (shl:i64 GPRC:i64:$RA, 2:i64), GPRC:i64:$RB) - Complexity = 11
                  // Dst: (S4SUBQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3330*/        0, /*End of Scope*/
/*3331*/      /*Scope*/ 37, /*->3369*/
/*3332*/        OPC_CheckInteger, 3, 
/*3334*/        OPC_MoveParent,
/*3335*/        OPC_MoveParent,
/*3336*/        OPC_RecordChild1, // #1 = $L
/*3337*/        OPC_Scope, 19, /*->3358*/ // 2 children in Scope
/*3339*/          OPC_MoveChild, 1,
/*3341*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3344*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3346*/          OPC_MoveParent,
/*3347*/          OPC_EmitConvertToTarget, 1,
/*3349*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8SUBQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (sub:i64 (shl:i64 GPRC:i64:$RA, 3:i64), (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 15
                  // Dst: (S8SUBQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3358*/        /*Scope*/ 9, /*->3368*/
/*3359*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8SUBQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                  // Src: (sub:i64 (shl:i64 GPRC:i64:$RA, 3:i64), GPRC:i64:$RB) - Complexity = 11
                  // Dst: (S8SUBQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3368*/        0, /*End of Scope*/
/*3369*/      0, /*End of Scope*/
/*3370*/    /*Scope*/ 34, /*->3405*/
/*3371*/      OPC_RecordChild0, // #0 = $RA
/*3372*/      OPC_RecordChild1, // #1 = $L
/*3373*/      OPC_Scope, 19, /*->3394*/ // 2 children in Scope
/*3375*/        OPC_MoveChild, 1,
/*3377*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3380*/        OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3382*/        OPC_MoveParent,
/*3383*/        OPC_EmitConvertToTarget, 1,
/*3385*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBQi), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                // Src: (sub:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
                // Dst: (SUBQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3394*/      /*Scope*/ 9, /*->3404*/
/*3395*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBQr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (sub:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
                // Dst: (SUBQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3404*/      0, /*End of Scope*/
/*3405*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 122,  TARGET_OPCODE(ISD::OR),// ->3531
/*3409*/    OPC_Scope, 31, /*->3442*/ // 3 children in Scope
/*3411*/      OPC_RecordChild0, // #0 = $RA
/*3412*/      OPC_MoveChild, 1,
/*3414*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*3417*/      OPC_RecordChild0, // #1 = $RB
/*3418*/      OPC_MoveChild, 1,
/*3420*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3431*/      OPC_MoveParent,
/*3432*/      OPC_MoveParent,
/*3433*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ORNOTr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (or:i64 GPRC:i64:$RA, (xor:i64 GPRC:i64:$RB, -1:i64)) - Complexity = 11
              // Dst: (ORNOTr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3442*/    /*Scope*/ 31, /*->3474*/
/*3443*/      OPC_MoveChild, 0,
/*3445*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*3448*/      OPC_RecordChild0, // #0 = $RB
/*3449*/      OPC_MoveChild, 1,
/*3451*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3462*/      OPC_MoveParent,
/*3463*/      OPC_MoveParent,
/*3464*/      OPC_RecordChild1, // #1 = $RA
/*3465*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ORNOTr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 0, 
              // Src: (or:i64 (xor:i64 GPRC:i64:$RB, -1:i64), GPRC:i64:$RA) - Complexity = 11
              // Dst: (ORNOTr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3474*/    /*Scope*/ 55, /*->3530*/
/*3475*/      OPC_RecordChild0, // #0 = $RA
/*3476*/      OPC_RecordChild1, // #1 = $L
/*3477*/      OPC_Scope, 40, /*->3519*/ // 2 children in Scope
/*3479*/        OPC_MoveChild, 1,
/*3481*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3484*/        OPC_Scope, 14, /*->3500*/ // 2 children in Scope
/*3486*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3488*/          OPC_MoveParent,
/*3489*/          OPC_EmitConvertToTarget, 1,
/*3491*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::BISi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (or:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
                  // Dst: (BISi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3500*/        /*Scope*/ 17, /*->3518*/
/*3501*/          OPC_CheckPredicate, 1, // Predicate_immUExt8inv
/*3503*/          OPC_MoveParent,
/*3504*/          OPC_EmitConvertToTarget, 1,
/*3506*/          OPC_EmitNodeXForm, 0, 2, // invX
/*3509*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ORNOTi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                  // Src: (or:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8inv>><<X:invX>>:$L) - Complexity = 7
                  // Dst: (ORNOTi:i64 GPRC:i64:$RA, (invX:i64 (imm:i64):$L))
/*3518*/        0, /*End of Scope*/
/*3519*/      /*Scope*/ 9, /*->3529*/
/*3520*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::BISr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (or:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
                // Dst: (BISr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3529*/      0, /*End of Scope*/
/*3530*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 122,  TARGET_OPCODE(ISD::XOR),// ->3656
/*3534*/    OPC_Scope, 31, /*->3567*/ // 3 children in Scope
/*3536*/      OPC_RecordChild0, // #0 = $RA
/*3537*/      OPC_MoveChild, 1,
/*3539*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*3542*/      OPC_RecordChild0, // #1 = $RB
/*3543*/      OPC_MoveChild, 1,
/*3545*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3556*/      OPC_MoveParent,
/*3557*/      OPC_MoveParent,
/*3558*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::EQVr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (xor:i64 GPRC:i64:$RA, (xor:i64 GPRC:i64:$RB, -1:i64)) - Complexity = 11
              // Dst: (EQVr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3567*/    /*Scope*/ 31, /*->3599*/
/*3568*/      OPC_MoveChild, 0,
/*3570*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::XOR),
/*3573*/      OPC_RecordChild0, // #0 = $RA
/*3574*/      OPC_MoveChild, 1,
/*3576*/      OPC_CheckInteger, 127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,127|128,1/*18446744073709551615*/, 
/*3587*/      OPC_MoveParent,
/*3588*/      OPC_MoveParent,
/*3589*/      OPC_RecordChild1, // #1 = $RB
/*3590*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::EQVr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (xor:i64 (xor:i64 GPRC:i64:$RA, -1:i64), GPRC:i64:$RB) - Complexity = 11
              // Dst: (EQVr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3599*/    /*Scope*/ 55, /*->3655*/
/*3600*/      OPC_RecordChild0, // #0 = $RA
/*3601*/      OPC_RecordChild1, // #1 = $L
/*3602*/      OPC_Scope, 40, /*->3644*/ // 2 children in Scope
/*3604*/        OPC_MoveChild, 1,
/*3606*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3609*/        OPC_Scope, 14, /*->3625*/ // 2 children in Scope
/*3611*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*3613*/          OPC_MoveParent,
/*3614*/          OPC_EmitConvertToTarget, 1,
/*3616*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::XORi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (xor:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
                  // Dst: (XORi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*3625*/        /*Scope*/ 17, /*->3643*/
/*3626*/          OPC_CheckPredicate, 1, // Predicate_immUExt8inv
/*3628*/          OPC_MoveParent,
/*3629*/          OPC_EmitConvertToTarget, 1,
/*3631*/          OPC_EmitNodeXForm, 0, 2, // invX
/*3634*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::EQVi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3, 
                  // Src: (xor:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8inv>><<X:invX>>:$L) - Complexity = 7
                  // Dst: (EQVi:i64 GPRC:i64:$RA, (invX:i64 (imm:i64):$L))
/*3643*/        0, /*End of Scope*/
/*3644*/      /*Scope*/ 9, /*->3654*/
/*3645*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::XORr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (xor:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
                // Dst: (XORr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*3654*/      0, /*End of Scope*/
/*3655*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 104|128,4/*616*/,  TARGET_OPCODE(ISD::LOAD),// ->4276
/*3660*/    OPC_RecordMemRef,
/*3661*/    OPC_RecordNode,   // #0 = 'ld' chained node
/*3662*/    OPC_Scope, 41|128,3/*425*/, /*->4090*/ // 2 children in Scope
/*3665*/      OPC_MoveChild, 1,
/*3667*/      OPC_SwitchOpcode /*2 cases */, 56|128,1/*184*/,  TARGET_OPCODE(ISD::ADD),// ->3856
/*3672*/        OPC_RecordChild0, // #1 = $RB
/*3673*/        OPC_RecordChild1, // #2 = $DISP
/*3674*/        OPC_MoveChild, 1,
/*3676*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*3679*/        OPC_CheckPredicate, 4, // Predicate_immSExt16
/*3681*/        OPC_MoveParent,
/*3682*/        OPC_CheckType, MVT::i64,
/*3684*/        OPC_MoveParent,
/*3685*/        OPC_CheckPredicate, 5, // Predicate_unindexedload
/*3687*/        OPC_Scope, 17, /*->3706*/ // 5 children in Scope
/*3689*/          OPC_CheckPredicate, 6, // Predicate_load
/*3691*/          OPC_CheckType, MVT::i64,
/*3693*/          OPC_EmitMergeInputChains1_0,
/*3694*/          OPC_EmitConvertToTarget, 2,
/*3696*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDQ), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 1, 
                  // Src: (ld:i64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 11
                  // Dst: (LDQ:i64:i64 (imm:i64):$DISP, GPRC:i64:$RB)
/*3706*/        /*Scope*/ 19, /*->3726*/
/*3707*/          OPC_CheckPredicate, 7, // Predicate_sextload
/*3709*/          OPC_CheckPredicate, 8, // Predicate_sextloadi32
/*3711*/          OPC_CheckType, MVT::i64,
/*3713*/          OPC_EmitMergeInputChains1_0,
/*3714*/          OPC_EmitConvertToTarget, 2,
/*3716*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDL), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 1, 
                  // Src: (ld:i64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi32>> - Complexity = 11
                  // Dst: (LDL:i64:i64 (imm:i64):$DISP, GPRC:i64:$RB)
/*3726*/        /*Scope*/ 38, /*->3765*/
/*3727*/          OPC_CheckPredicate, 9, // Predicate_zextload
/*3729*/          OPC_CheckType, MVT::i64,
/*3731*/          OPC_Scope, 15, /*->3748*/ // 2 children in Scope
/*3733*/            OPC_CheckPredicate, 10, // Predicate_zextloadi8
/*3735*/            OPC_EmitMergeInputChains1_0,
/*3736*/            OPC_EmitConvertToTarget, 2,
/*3738*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDBU), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 1, 
                    // Src: (ld:i64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 11
                    // Dst: (LDBU:i64:i64 (imm:i64):$DISP, GPRC:i64:$RB)
/*3748*/          /*Scope*/ 15, /*->3764*/
/*3749*/            OPC_CheckPredicate, 11, // Predicate_zextloadi16
/*3751*/            OPC_EmitMergeInputChains1_0,
/*3752*/            OPC_EmitConvertToTarget, 2,
/*3754*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDWU), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 1, 
                    // Src: (ld:i64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 11
                    // Dst: (LDWU:i64:i64 (imm:i64):$DISP, GPRC:i64:$RB)
/*3764*/          0, /*End of Scope*/
/*3765*/        /*Scope*/ 54, /*->3820*/
/*3766*/          OPC_CheckPredicate, 12, // Predicate_extload
/*3768*/          OPC_CheckType, MVT::i64,
/*3770*/          OPC_Scope, 15, /*->3787*/ // 3 children in Scope
/*3772*/            OPC_CheckPredicate, 13, // Predicate_extloadi8
/*3774*/            OPC_EmitMergeInputChains1_0,
/*3775*/            OPC_EmitConvertToTarget, 2,
/*3777*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDBU), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 1, 
                    // Src: (ld:i64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 11
                    // Dst: (LDBU:i64:i64 (imm:i64)<<P:Predicate_immSExt16>>:$DISP, GPRC:i64:$RB)
/*3787*/          /*Scope*/ 15, /*->3803*/
/*3788*/            OPC_CheckPredicate, 14, // Predicate_extloadi16
/*3790*/            OPC_EmitMergeInputChains1_0,
/*3791*/            OPC_EmitConvertToTarget, 2,
/*3793*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDWU), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 1, 
                    // Src: (ld:i64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 11
                    // Dst: (LDWU:i64:i64 (imm:i64)<<P:Predicate_immSExt16>>:$DISP, GPRC:i64:$RB)
/*3803*/          /*Scope*/ 15, /*->3819*/
/*3804*/            OPC_CheckPredicate, 15, // Predicate_extloadi32
/*3806*/            OPC_EmitMergeInputChains1_0,
/*3807*/            OPC_EmitConvertToTarget, 2,
/*3809*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDL), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 1, 
                    // Src: (ld:i64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi32>> - Complexity = 11
                    // Dst: (LDL:i64:i64 (imm:i64)<<P:Predicate_immSExt16>>:$DISP, GPRC:i64:$RB)
/*3819*/          0, /*End of Scope*/
/*3820*/        /*Scope*/ 34, /*->3855*/
/*3821*/          OPC_CheckPredicate, 6, // Predicate_load
/*3823*/          OPC_SwitchType /*2 cases */, 13,  MVT::f32,// ->3839
/*3826*/            OPC_EmitMergeInputChains1_0,
/*3827*/            OPC_EmitConvertToTarget, 2,
/*3829*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDS), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::f32, MVT::i64, 2/*#Ops*/, 3, 1, 
                    // Src: (ld:f32 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 11
                    // Dst: (LDS:f32:i64 (imm:i64):$DISP, GPRC:i64:$RB)
                  /*SwitchType*/ 13,  MVT::f64,// ->3854
/*3841*/            OPC_EmitMergeInputChains1_0,
/*3842*/            OPC_EmitConvertToTarget, 2,
/*3844*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDT), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::f64, MVT::i64, 2/*#Ops*/, 3, 1, 
                    // Src: (ld:f64 (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 11
                    // Dst: (LDT:f64:i64 (imm:i64):$DISP, GPRC:i64:$RB)
                  0, // EndSwitchType
/*3855*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 101|128,1/*229*/,  TARGET_OPCODE(AlphaISD::GPRelLo),// ->4089
/*3860*/        OPC_RecordChild0, // #1 = $DISP
/*3861*/        OPC_MoveChild, 0,
/*3863*/        OPC_SwitchOpcode /*2 cases */, 109,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->3976
/*3867*/          OPC_MoveParent,
/*3868*/          OPC_RecordChild1, // #2 = $RB
/*3869*/          OPC_CheckType, MVT::i64,
/*3871*/          OPC_MoveParent,
/*3872*/          OPC_CheckPredicate, 5, // Predicate_unindexedload
/*3874*/          OPC_Scope, 15, /*->3891*/ // 4 children in Scope
/*3876*/            OPC_CheckPredicate, 6, // Predicate_load
/*3878*/            OPC_CheckType, MVT::i64,
/*3880*/            OPC_EmitMergeInputChains1_0,
/*3881*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDQr), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                    // Src: (ld:i64 (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 10
                    // Dst: (LDQr:i64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*3891*/          /*Scope*/ 17, /*->3909*/
/*3892*/            OPC_CheckPredicate, 7, // Predicate_sextload
/*3894*/            OPC_CheckPredicate, 8, // Predicate_sextloadi32
/*3896*/            OPC_CheckType, MVT::i64,
/*3898*/            OPC_EmitMergeInputChains1_0,
/*3899*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDLr), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                    // Src: (ld:i64 (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi32>> - Complexity = 10
                    // Dst: (LDLr:i64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*3909*/          /*Scope*/ 34, /*->3944*/
/*3910*/            OPC_CheckPredicate, 9, // Predicate_zextload
/*3912*/            OPC_CheckType, MVT::i64,
/*3914*/            OPC_Scope, 13, /*->3929*/ // 2 children in Scope
/*3916*/              OPC_CheckPredicate, 10, // Predicate_zextloadi8
/*3918*/              OPC_EmitMergeInputChains1_0,
/*3919*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDBUr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:i64 (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 10
                      // Dst: (LDBUr:i64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*3929*/            /*Scope*/ 13, /*->3943*/
/*3930*/              OPC_CheckPredicate, 11, // Predicate_zextloadi16
/*3932*/              OPC_EmitMergeInputChains1_0,
/*3933*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDWUr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:i64 (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 10
                      // Dst: (LDWUr:i64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*3943*/            0, /*End of Scope*/
/*3944*/          /*Scope*/ 30, /*->3975*/
/*3945*/            OPC_CheckPredicate, 6, // Predicate_load
/*3947*/            OPC_SwitchType /*2 cases */, 11,  MVT::f32,// ->3961
/*3950*/              OPC_EmitMergeInputChains1_0,
/*3951*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDSr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::f32, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:f32 (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 10
                      // Dst: (LDSr:f32:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
                    /*SwitchType*/ 11,  MVT::f64,// ->3974
/*3963*/              OPC_EmitMergeInputChains1_0,
/*3964*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDTr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::f64, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:f64 (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 10
                      // Dst: (LDTr:f64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
                    0, // EndSwitchType
/*3975*/          0, /*End of Scope*/
                /*SwitchOpcode*/ 109,  TARGET_OPCODE(ISD::TargetConstantPool),// ->4088
/*3979*/          OPC_MoveParent,
/*3980*/          OPC_RecordChild1, // #2 = $RB
/*3981*/          OPC_CheckType, MVT::i64,
/*3983*/          OPC_MoveParent,
/*3984*/          OPC_CheckPredicate, 5, // Predicate_unindexedload
/*3986*/          OPC_Scope, 15, /*->4003*/ // 4 children in Scope
/*3988*/            OPC_CheckPredicate, 6, // Predicate_load
/*3990*/            OPC_CheckType, MVT::i64,
/*3992*/            OPC_EmitMergeInputChains1_0,
/*3993*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDQr), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                    // Src: (ld:i64 (Alpha_gprello:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 10
                    // Dst: (LDQr:i64:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
/*4003*/          /*Scope*/ 17, /*->4021*/
/*4004*/            OPC_CheckPredicate, 7, // Predicate_sextload
/*4006*/            OPC_CheckPredicate, 8, // Predicate_sextloadi32
/*4008*/            OPC_CheckType, MVT::i64,
/*4010*/            OPC_EmitMergeInputChains1_0,
/*4011*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDLr), 0|OPFL_Chain|OPFL_MemRefs,
                        2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                    // Src: (ld:i64 (Alpha_gprello:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi32>> - Complexity = 10
                    // Dst: (LDLr:i64:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
/*4021*/          /*Scope*/ 34, /*->4056*/
/*4022*/            OPC_CheckPredicate, 9, // Predicate_zextload
/*4024*/            OPC_CheckType, MVT::i64,
/*4026*/            OPC_Scope, 13, /*->4041*/ // 2 children in Scope
/*4028*/              OPC_CheckPredicate, 10, // Predicate_zextloadi8
/*4030*/              OPC_EmitMergeInputChains1_0,
/*4031*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDBUr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:i64 (Alpha_gprello:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 10
                      // Dst: (LDBUr:i64:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
/*4041*/            /*Scope*/ 13, /*->4055*/
/*4042*/              OPC_CheckPredicate, 11, // Predicate_zextloadi16
/*4044*/              OPC_EmitMergeInputChains1_0,
/*4045*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDWUr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:i64 (Alpha_gprello:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 10
                      // Dst: (LDWUr:i64:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
/*4055*/            0, /*End of Scope*/
/*4056*/          /*Scope*/ 30, /*->4087*/
/*4057*/            OPC_CheckPredicate, 6, // Predicate_load
/*4059*/            OPC_SwitchType /*2 cases */, 11,  MVT::f32,// ->4073
/*4062*/              OPC_EmitMergeInputChains1_0,
/*4063*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDSr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::f32, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:f32 (Alpha_gprello:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 10
                      // Dst: (LDSr:f32:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
                    /*SwitchType*/ 11,  MVT::f64,// ->4086
/*4075*/              OPC_EmitMergeInputChains1_0,
/*4076*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDTr), 0|OPFL_Chain|OPFL_MemRefs,
                          2/*#VTs*/, MVT::f64, MVT::i64, 2/*#Ops*/, 1, 2, 
                      // Src: (ld:f64 (Alpha_gprello:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 10
                      // Dst: (LDTr:f64:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
                    0, // EndSwitchType
/*4087*/          0, /*End of Scope*/
                0, // EndSwitchOpcode
              0, // EndSwitchOpcode
/*4090*/    /*Scope*/ 55|128,1/*183*/, /*->4275*/
/*4092*/      OPC_RecordChild1, // #1 = $addr
/*4093*/      OPC_CheckChild1Type, MVT::i64,
/*4095*/      OPC_CheckPredicate, 5, // Predicate_unindexedload
/*4097*/      OPC_Scope, 18, /*->4117*/ // 5 children in Scope
/*4099*/        OPC_CheckPredicate, 6, // Predicate_load
/*4101*/        OPC_CheckType, MVT::i64,
/*4103*/        OPC_EmitMergeInputChains1_0,
/*4104*/        OPC_EmitInteger, MVT::i64, 0, 
/*4107*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDQ), 0|OPFL_Chain|OPFL_MemRefs,
                    2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 1, 
                // Src: (ld:i64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 4
                // Dst: (LDQ:i64:i64 0:i64, GPRC:i64:$addr)
/*4117*/      /*Scope*/ 20, /*->4138*/
/*4118*/        OPC_CheckPredicate, 7, // Predicate_sextload
/*4120*/        OPC_CheckPredicate, 8, // Predicate_sextloadi32
/*4122*/        OPC_CheckType, MVT::i64,
/*4124*/        OPC_EmitMergeInputChains1_0,
/*4125*/        OPC_EmitInteger, MVT::i64, 0, 
/*4128*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDL), 0|OPFL_Chain|OPFL_MemRefs,
                    2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 1, 
                // Src: (ld:i64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_sextload>><<P:Predicate_sextloadi32>> - Complexity = 4
                // Dst: (LDL:i64:i64 0:i64, GPRC:i64:$addr)
/*4138*/      /*Scope*/ 40, /*->4179*/
/*4139*/        OPC_CheckPredicate, 9, // Predicate_zextload
/*4141*/        OPC_CheckType, MVT::i64,
/*4143*/        OPC_Scope, 16, /*->4161*/ // 2 children in Scope
/*4145*/          OPC_CheckPredicate, 11, // Predicate_zextloadi16
/*4147*/          OPC_EmitMergeInputChains1_0,
/*4148*/          OPC_EmitInteger, MVT::i64, 0, 
/*4151*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDWU), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:i64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi16>> - Complexity = 4
                  // Dst: (LDWU:i64:i64 0:i64, GPRC:i64:$addr)
/*4161*/        /*Scope*/ 16, /*->4178*/
/*4162*/          OPC_CheckPredicate, 10, // Predicate_zextloadi8
/*4164*/          OPC_EmitMergeInputChains1_0,
/*4165*/          OPC_EmitInteger, MVT::i64, 0, 
/*4168*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDBU), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:i64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_zextload>><<P:Predicate_zextloadi8>> - Complexity = 4
                  // Dst: (LDBU:i64:i64 0:i64, GPRC:i64:$addr)
/*4178*/        0, /*End of Scope*/
/*4179*/      /*Scope*/ 57, /*->4237*/
/*4180*/        OPC_CheckPredicate, 12, // Predicate_extload
/*4182*/        OPC_CheckType, MVT::i64,
/*4184*/        OPC_Scope, 16, /*->4202*/ // 3 children in Scope
/*4186*/          OPC_CheckPredicate, 13, // Predicate_extloadi8
/*4188*/          OPC_EmitMergeInputChains1_0,
/*4189*/          OPC_EmitInteger, MVT::i64, 0, 
/*4192*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDBU), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:i64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi8>> - Complexity = 4
                  // Dst: (LDBU:i64:i64 0:i64, GPRC:i64:$addr)
/*4202*/        /*Scope*/ 16, /*->4219*/
/*4203*/          OPC_CheckPredicate, 14, // Predicate_extloadi16
/*4205*/          OPC_EmitMergeInputChains1_0,
/*4206*/          OPC_EmitInteger, MVT::i64, 0, 
/*4209*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDWU), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:i64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi16>> - Complexity = 4
                  // Dst: (LDWU:i64:i64 0:i64, GPRC:i64:$addr)
/*4219*/        /*Scope*/ 16, /*->4236*/
/*4220*/          OPC_CheckPredicate, 15, // Predicate_extloadi32
/*4222*/          OPC_EmitMergeInputChains1_0,
/*4223*/          OPC_EmitInteger, MVT::i64, 0, 
/*4226*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDL), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:i64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_extload>><<P:Predicate_extloadi32>> - Complexity = 4
                  // Dst: (LDL:i64:i64 0:i64, GPRC:i64:$addr)
/*4236*/        0, /*End of Scope*/
/*4237*/      /*Scope*/ 36, /*->4274*/
/*4238*/        OPC_CheckPredicate, 6, // Predicate_load
/*4240*/        OPC_SwitchType /*2 cases */, 14,  MVT::f64,// ->4257
/*4243*/          OPC_EmitMergeInputChains1_0,
/*4244*/          OPC_EmitInteger, MVT::i64, 0, 
/*4247*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDT), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::f64, MVT::i64, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:f64 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 4
                  // Dst: (LDT:f64:i64 0:i64, GPRC:i64:$addr)
                /*SwitchType*/ 14,  MVT::f32,// ->4273
/*4259*/          OPC_EmitMergeInputChains1_0,
/*4260*/          OPC_EmitInteger, MVT::i64, 0, 
/*4263*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDS), 0|OPFL_Chain|OPFL_MemRefs,
                      2/*#VTs*/, MVT::f32, MVT::i64, 2/*#Ops*/, 2, 1, 
                  // Src: (ld:f32 GPRC:i64:$addr)<<P:Predicate_unindexedload>><<P:Predicate_load>> - Complexity = 4
                  // Dst: (LDS:f32:i64 0:i64, GPRC:i64:$addr)
                0, // EndSwitchType
/*4274*/      0, /*End of Scope*/
/*4275*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 70|128,3/*454*/,  TARGET_OPCODE(ISD::STORE),// ->4734
/*4280*/    OPC_RecordMemRef,
/*4281*/    OPC_RecordNode,   // #0 = 'st' chained node
/*4282*/    OPC_RecordChild1, // #1 = $RA
/*4283*/    OPC_Scope, 3|128,2/*259*/, /*->4545*/ // 3 children in Scope
/*4286*/      OPC_CheckChild1Type, MVT::i64,
/*4288*/      OPC_Scope, 44|128,1/*172*/, /*->4463*/ // 2 children in Scope
/*4291*/        OPC_MoveChild, 2,
/*4293*/        OPC_SwitchOpcode /*2 cases */, 86,  TARGET_OPCODE(ISD::ADD),// ->4383
/*4297*/          OPC_RecordChild0, // #2 = $RB
/*4298*/          OPC_RecordChild1, // #3 = $DISP
/*4299*/          OPC_MoveChild, 1,
/*4301*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*4304*/          OPC_CheckPredicate, 4, // Predicate_immSExt16
/*4306*/          OPC_MoveParent,
/*4307*/          OPC_CheckType, MVT::i64,
/*4309*/          OPC_MoveParent,
/*4310*/          OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4312*/          OPC_Scope, 52, /*->4366*/ // 2 children in Scope
/*4314*/            OPC_CheckPredicate, 17, // Predicate_truncstore
/*4316*/            OPC_Scope, 15, /*->4333*/ // 3 children in Scope
/*4318*/              OPC_CheckPredicate, 18, // Predicate_truncstorei8
/*4320*/              OPC_EmitMergeInputChains1_0,
/*4321*/              OPC_EmitConvertToTarget, 3,
/*4323*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STB), 0|OPFL_Chain|OPFL_MemRefs,
                          1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 4, 2, 
                      // Src: (st GPRC:i64:$RA, (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 11
                      // Dst: (STB:i64 GPRC:i64:$RA, (imm:i64):$DISP, GPRC:i64:$RB)
/*4333*/            /*Scope*/ 15, /*->4349*/
/*4334*/              OPC_CheckPredicate, 19, // Predicate_truncstorei16
/*4336*/              OPC_EmitMergeInputChains1_0,
/*4337*/              OPC_EmitConvertToTarget, 3,
/*4339*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STW), 0|OPFL_Chain|OPFL_MemRefs,
                          1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 4, 2, 
                      // Src: (st GPRC:i64:$RA, (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 11
                      // Dst: (STW:i64 GPRC:i64:$RA, (imm:i64):$DISP, GPRC:i64:$RB)
/*4349*/            /*Scope*/ 15, /*->4365*/
/*4350*/              OPC_CheckPredicate, 20, // Predicate_truncstorei32
/*4352*/              OPC_EmitMergeInputChains1_0,
/*4353*/              OPC_EmitConvertToTarget, 3,
/*4355*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STL), 0|OPFL_Chain|OPFL_MemRefs,
                          1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 4, 2, 
                      // Src: (st GPRC:i64:$RA, (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei32>> - Complexity = 11
                      // Dst: (STL:i64 GPRC:i64:$RA, (imm:i64):$DISP, GPRC:i64:$RB)
/*4365*/            0, /*End of Scope*/
/*4366*/          /*Scope*/ 15, /*->4382*/
/*4367*/            OPC_CheckPredicate, 21, // Predicate_store
/*4369*/            OPC_EmitMergeInputChains1_0,
/*4370*/            OPC_EmitConvertToTarget, 3,
/*4372*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STQ), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 4, 2, 
                    // Src: (st GPRC:i64:$RA, (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 11
                    // Dst: (STQ:i64 GPRC:i64:$RA, (imm:i64):$DISP, GPRC:i64:$RB)
/*4382*/          0, /*End of Scope*/
                /*SwitchOpcode*/ 76,  TARGET_OPCODE(AlphaISD::GPRelLo),// ->4462
/*4386*/          OPC_RecordChild0, // #2 = $DISP
/*4387*/          OPC_MoveChild, 0,
/*4389*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetGlobalAddress),
/*4392*/          OPC_MoveParent,
/*4393*/          OPC_RecordChild1, // #3 = $RB
/*4394*/          OPC_CheckType, MVT::i64,
/*4396*/          OPC_MoveParent,
/*4397*/          OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4399*/          OPC_Scope, 46, /*->4447*/ // 2 children in Scope
/*4401*/            OPC_CheckPredicate, 17, // Predicate_truncstore
/*4403*/            OPC_Scope, 13, /*->4418*/ // 3 children in Scope
/*4405*/              OPC_CheckPredicate, 18, // Predicate_truncstorei8
/*4407*/              OPC_EmitMergeInputChains1_0,
/*4408*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STBr), 0|OPFL_Chain|OPFL_MemRefs,
                          1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
                      // Src: (st GPRC:i64:$RA, (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 10
                      // Dst: (STBr:i64 GPRC:i64:$RA, (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*4418*/            /*Scope*/ 13, /*->4432*/
/*4419*/              OPC_CheckPredicate, 19, // Predicate_truncstorei16
/*4421*/              OPC_EmitMergeInputChains1_0,
/*4422*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STWr), 0|OPFL_Chain|OPFL_MemRefs,
                          1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
                      // Src: (st GPRC:i64:$RA, (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 10
                      // Dst: (STWr:i64 GPRC:i64:$RA, (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*4432*/            /*Scope*/ 13, /*->4446*/
/*4433*/              OPC_CheckPredicate, 20, // Predicate_truncstorei32
/*4435*/              OPC_EmitMergeInputChains1_0,
/*4436*/              OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STLr), 0|OPFL_Chain|OPFL_MemRefs,
                          1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
                      // Src: (st GPRC:i64:$RA, (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei32>> - Complexity = 10
                      // Dst: (STLr:i64 GPRC:i64:$RA, (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*4446*/            0, /*End of Scope*/
/*4447*/          /*Scope*/ 13, /*->4461*/
/*4448*/            OPC_CheckPredicate, 21, // Predicate_store
/*4450*/            OPC_EmitMergeInputChains1_0,
/*4451*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STQr), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
                    // Src: (st GPRC:i64:$RA, (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 10
                    // Dst: (STQr:i64 GPRC:i64:$RA, (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
/*4461*/          0, /*End of Scope*/
                0, // EndSwitchOpcode
/*4463*/      /*Scope*/ 80, /*->4544*/
/*4464*/        OPC_RecordChild2, // #2 = $addr
/*4465*/        OPC_CheckChild2Type, MVT::i64,
/*4467*/        OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4469*/        OPC_Scope, 16, /*->4487*/ // 2 children in Scope
/*4471*/          OPC_CheckPredicate, 21, // Predicate_store
/*4473*/          OPC_EmitMergeInputChains1_0,
/*4474*/          OPC_EmitInteger, MVT::i64, 0, 
/*4477*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STQ), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 2, 
                  // Src: (st GPRC:i64:$DATA, GPRC:i64:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 4
                  // Dst: (STQ:i64 GPRC:i64:$DATA, 0:i64, GPRC:i64:$addr)
/*4487*/        /*Scope*/ 55, /*->4543*/
/*4488*/          OPC_CheckPredicate, 17, // Predicate_truncstore
/*4490*/          OPC_Scope, 16, /*->4508*/ // 3 children in Scope
/*4492*/            OPC_CheckPredicate, 20, // Predicate_truncstorei32
/*4494*/            OPC_EmitMergeInputChains1_0,
/*4495*/            OPC_EmitInteger, MVT::i64, 0, 
/*4498*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STL), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 2, 
                    // Src: (st GPRC:i64:$DATA, GPRC:i64:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei32>> - Complexity = 4
                    // Dst: (STL:i64 GPRC:i64:$DATA, 0:i64, GPRC:i64:$addr)
/*4508*/          /*Scope*/ 16, /*->4525*/
/*4509*/            OPC_CheckPredicate, 19, // Predicate_truncstorei16
/*4511*/            OPC_EmitMergeInputChains1_0,
/*4512*/            OPC_EmitInteger, MVT::i64, 0, 
/*4515*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STW), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 2, 
                    // Src: (st GPRC:i64:$DATA, GPRC:i64:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei16>> - Complexity = 4
                    // Dst: (STW:i64 GPRC:i64:$DATA, 0:i64, GPRC:i64:$addr)
/*4525*/          /*Scope*/ 16, /*->4542*/
/*4526*/            OPC_CheckPredicate, 18, // Predicate_truncstorei8
/*4528*/            OPC_EmitMergeInputChains1_0,
/*4529*/            OPC_EmitInteger, MVT::i64, 0, 
/*4532*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STB), 0|OPFL_Chain|OPFL_MemRefs,
                        1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 2, 
                    // Src: (st GPRC:i64:$DATA, GPRC:i64:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_truncstore>><<P:Predicate_truncstorei8>> - Complexity = 4
                    // Dst: (STB:i64 GPRC:i64:$DATA, 0:i64, GPRC:i64:$addr)
/*4542*/          0, /*End of Scope*/
/*4543*/        0, /*End of Scope*/
/*4544*/      0, /*End of Scope*/
/*4545*/    /*Scope*/ 93, /*->4639*/
/*4546*/      OPC_CheckChild1Type, MVT::f32,
/*4548*/      OPC_Scope, 66, /*->4616*/ // 2 children in Scope
/*4550*/        OPC_MoveChild, 2,
/*4552*/        OPC_SwitchOpcode /*2 cases */, 30,  TARGET_OPCODE(ISD::ADD),// ->4586
/*4556*/          OPC_RecordChild0, // #2 = $RB
/*4557*/          OPC_RecordChild1, // #3 = $DISP
/*4558*/          OPC_MoveChild, 1,
/*4560*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*4563*/          OPC_CheckPredicate, 4, // Predicate_immSExt16
/*4565*/          OPC_MoveParent,
/*4566*/          OPC_CheckType, MVT::i64,
/*4568*/          OPC_MoveParent,
/*4569*/          OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4571*/          OPC_CheckPredicate, 21, // Predicate_store
/*4573*/          OPC_EmitMergeInputChains1_0,
/*4574*/          OPC_EmitConvertToTarget, 3,
/*4576*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STS), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 4, 2, 
                  // Src: (st F4RC:f32:$RA, (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 11
                  // Dst: (STS:i64 F4RC:f32:$RA, (imm:i64):$DISP, GPRC:i64:$RB)
                /*SwitchOpcode*/ 26,  TARGET_OPCODE(AlphaISD::GPRelLo),// ->4615
/*4589*/          OPC_RecordChild0, // #2 = $DISP
/*4590*/          OPC_MoveChild, 0,
/*4592*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetGlobalAddress),
/*4595*/          OPC_MoveParent,
/*4596*/          OPC_RecordChild1, // #3 = $RB
/*4597*/          OPC_CheckType, MVT::i64,
/*4599*/          OPC_MoveParent,
/*4600*/          OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4602*/          OPC_CheckPredicate, 21, // Predicate_store
/*4604*/          OPC_EmitMergeInputChains1_0,
/*4605*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STSr), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
                  // Src: (st F4RC:f32:$RA, (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 10
                  // Dst: (STSr:i64 F4RC:f32:$RA, (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
                0, // EndSwitchOpcode
/*4616*/      /*Scope*/ 21, /*->4638*/
/*4617*/        OPC_RecordChild2, // #2 = $addr
/*4618*/        OPC_CheckChild2Type, MVT::i64,
/*4620*/        OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4622*/        OPC_CheckPredicate, 21, // Predicate_store
/*4624*/        OPC_EmitMergeInputChains1_0,
/*4625*/        OPC_EmitInteger, MVT::i64, 0, 
/*4628*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STS), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 2, 
                // Src: (st F4RC:f32:$DATA, GPRC:i64:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 4
                // Dst: (STS:i64 F4RC:f32:$DATA, 0:i64, GPRC:i64:$addr)
/*4638*/      0, /*End of Scope*/
/*4639*/    /*Scope*/ 93, /*->4733*/
/*4640*/      OPC_CheckChild1Type, MVT::f64,
/*4642*/      OPC_Scope, 66, /*->4710*/ // 2 children in Scope
/*4644*/        OPC_MoveChild, 2,
/*4646*/        OPC_SwitchOpcode /*2 cases */, 30,  TARGET_OPCODE(ISD::ADD),// ->4680
/*4650*/          OPC_RecordChild0, // #2 = $RB
/*4651*/          OPC_RecordChild1, // #3 = $DISP
/*4652*/          OPC_MoveChild, 1,
/*4654*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*4657*/          OPC_CheckPredicate, 4, // Predicate_immSExt16
/*4659*/          OPC_MoveParent,
/*4660*/          OPC_CheckType, MVT::i64,
/*4662*/          OPC_MoveParent,
/*4663*/          OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4665*/          OPC_CheckPredicate, 21, // Predicate_store
/*4667*/          OPC_EmitMergeInputChains1_0,
/*4668*/          OPC_EmitConvertToTarget, 3,
/*4670*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STT), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 4, 2, 
                  // Src: (st F8RC:f64:$RA, (add:i64 GPRC:i64:$RB, (imm:i64)<<P:Predicate_immSExt16>>:$DISP))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 11
                  // Dst: (STT:i64 F8RC:f64:$RA, (imm:i64):$DISP, GPRC:i64:$RB)
                /*SwitchOpcode*/ 26,  TARGET_OPCODE(AlphaISD::GPRelLo),// ->4709
/*4683*/          OPC_RecordChild0, // #2 = $DISP
/*4684*/          OPC_MoveChild, 0,
/*4686*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetGlobalAddress),
/*4689*/          OPC_MoveParent,
/*4690*/          OPC_RecordChild1, // #3 = $RB
/*4691*/          OPC_CheckType, MVT::i64,
/*4693*/          OPC_MoveParent,
/*4694*/          OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4696*/          OPC_CheckPredicate, 21, // Predicate_store
/*4698*/          OPC_EmitMergeInputChains1_0,
/*4699*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STTr), 0|OPFL_Chain|OPFL_MemRefs,
                      1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
                  // Src: (st F8RC:f64:$RA, (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB))<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 10
                  // Dst: (STTr:i64 F8RC:f64:$RA, (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
                0, // EndSwitchOpcode
/*4710*/      /*Scope*/ 21, /*->4732*/
/*4711*/        OPC_RecordChild2, // #2 = $addr
/*4712*/        OPC_CheckChild2Type, MVT::i64,
/*4714*/        OPC_CheckPredicate, 16, // Predicate_unindexedstore
/*4716*/        OPC_CheckPredicate, 21, // Predicate_store
/*4718*/        OPC_EmitMergeInputChains1_0,
/*4719*/        OPC_EmitInteger, MVT::i64, 0, 
/*4722*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::STT), 0|OPFL_Chain|OPFL_MemRefs,
                    1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 2, 
                // Src: (st F8RC:f64:$DATA, GPRC:i64:$addr)<<P:Predicate_unindexedstore>><<P:Predicate_store>> - Complexity = 4
                // Dst: (STT:i64 F8RC:f64:$DATA, 0:i64, GPRC:i64:$addr)
/*4732*/      0, /*End of Scope*/
/*4733*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 4|128,12/*1540*/,  TARGET_OPCODE(ISD::BRCOND),// ->6278
/*4738*/    OPC_RecordNode,   // #0 = 'brcond' chained node
/*4739*/    OPC_Scope, 102|128,11/*1510*/, /*->6252*/ // 2 children in Scope
/*4742*/      OPC_MoveChild, 1,
/*4744*/      OPC_SwitchOpcode /*2 cases */, 61|128,11/*1469*/,  TARGET_OPCODE(ISD::SETCC),// ->6218
/*4749*/        OPC_RecordChild0, // #1 = $RA
/*4750*/        OPC_Scope, 49|128,1/*177*/, /*->4930*/ // 5 children in Scope
/*4753*/          OPC_MoveChild, 1,
/*4755*/          OPC_CheckInteger, 0, 
/*4757*/          OPC_MoveParent,
/*4758*/          OPC_MoveChild, 2,
/*4760*/          OPC_Scope, 27, /*->4789*/ // 6 children in Scope
/*4762*/            OPC_CheckCondCode, ISD::SETEQ,
/*4764*/            OPC_MoveParent,
/*4765*/            OPC_MoveParent,
/*4766*/            OPC_RecordChild2, // #2 = $DISP
/*4767*/            OPC_MoveChild, 2,
/*4769*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4772*/            OPC_MoveParent,
/*4773*/            OPC_EmitMergeInputChains1_0,
/*4774*/            OPC_EmitInteger, MVT::i64, 0, 
/*4777*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4780*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 GPRC:i64:$RA, 0:i64, SETEQ:Other), (bb:Other):$DISP) - Complexity = 11
                    // Dst: (COND_BRANCH_I (immBRCond:i64 0:i64), GPRC:i64:$RA, (bb:Other):$DISP)
/*4789*/          /*Scope*/ 27, /*->4817*/
/*4790*/            OPC_CheckCondCode, ISD::SETGE,
/*4792*/            OPC_MoveParent,
/*4793*/            OPC_MoveParent,
/*4794*/            OPC_RecordChild2, // #2 = $DISP
/*4795*/            OPC_MoveChild, 2,
/*4797*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4800*/            OPC_MoveParent,
/*4801*/            OPC_EmitMergeInputChains1_0,
/*4802*/            OPC_EmitInteger, MVT::i64, 2, 
/*4805*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4808*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 GPRC:i64:$RA, 0:i64, SETGE:Other), (bb:Other):$DISP) - Complexity = 11
                    // Dst: (COND_BRANCH_I (immBRCond:i64 2:i64), GPRC:i64:$RA, (bb:Other):$DISP)
/*4817*/          /*Scope*/ 27, /*->4845*/
/*4818*/            OPC_CheckCondCode, ISD::SETGT,
/*4820*/            OPC_MoveParent,
/*4821*/            OPC_MoveParent,
/*4822*/            OPC_RecordChild2, // #2 = $DISP
/*4823*/            OPC_MoveChild, 2,
/*4825*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4828*/            OPC_MoveParent,
/*4829*/            OPC_EmitMergeInputChains1_0,
/*4830*/            OPC_EmitInteger, MVT::i64, 3, 
/*4833*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4836*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 GPRC:i64:$RA, 0:i64, SETGT:Other), (bb:Other):$DISP) - Complexity = 11
                    // Dst: (COND_BRANCH_I (immBRCond:i64 3:i64), GPRC:i64:$RA, (bb:Other):$DISP)
/*4845*/          /*Scope*/ 27, /*->4873*/
/*4846*/            OPC_CheckCondCode, ISD::SETLE,
/*4848*/            OPC_MoveParent,
/*4849*/            OPC_MoveParent,
/*4850*/            OPC_RecordChild2, // #2 = $DISP
/*4851*/            OPC_MoveChild, 2,
/*4853*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4856*/            OPC_MoveParent,
/*4857*/            OPC_EmitMergeInputChains1_0,
/*4858*/            OPC_EmitInteger, MVT::i64, 4, 
/*4861*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4864*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 GPRC:i64:$RA, 0:i64, SETLE:Other), (bb:Other):$DISP) - Complexity = 11
                    // Dst: (COND_BRANCH_I (immBRCond:i64 4:i64), GPRC:i64:$RA, (bb:Other):$DISP)
/*4873*/          /*Scope*/ 27, /*->4901*/
/*4874*/            OPC_CheckCondCode, ISD::SETLT,
/*4876*/            OPC_MoveParent,
/*4877*/            OPC_MoveParent,
/*4878*/            OPC_RecordChild2, // #2 = $DISP
/*4879*/            OPC_MoveChild, 2,
/*4881*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4884*/            OPC_MoveParent,
/*4885*/            OPC_EmitMergeInputChains1_0,
/*4886*/            OPC_EmitInteger, MVT::i64, 5, 
/*4889*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4892*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 GPRC:i64:$RA, 0:i64, SETLT:Other), (bb:Other):$DISP) - Complexity = 11
                    // Dst: (COND_BRANCH_I (immBRCond:i64 5:i64), GPRC:i64:$RA, (bb:Other):$DISP)
/*4901*/          /*Scope*/ 27, /*->4929*/
/*4902*/            OPC_CheckCondCode, ISD::SETNE,
/*4904*/            OPC_MoveParent,
/*4905*/            OPC_MoveParent,
/*4906*/            OPC_RecordChild2, // #2 = $DISP
/*4907*/            OPC_MoveChild, 2,
/*4909*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4912*/            OPC_MoveParent,
/*4913*/            OPC_EmitMergeInputChains1_0,
/*4914*/            OPC_EmitInteger, MVT::i64, 1, 
/*4917*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4920*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 GPRC:i64:$RA, 0:i64, SETNE:Other), (bb:Other):$DISP) - Complexity = 11
                    // Dst: (COND_BRANCH_I (immBRCond:i64 1:i64), GPRC:i64:$RA, (bb:Other):$DISP)
/*4929*/          0, /*End of Scope*/
/*4930*/        /*Scope*/ 6|128,4/*518*/, /*->5450*/
/*4932*/          OPC_CheckChild0Type, MVT::f64,
/*4934*/          OPC_MoveChild, 1,
/*4936*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::ConstantFP),
/*4939*/          OPC_CheckPredicate, 22, // Predicate_immFPZ
/*4941*/          OPC_MoveParent,
/*4942*/          OPC_MoveChild, 2,
/*4944*/          OPC_Scope, 27, /*->4973*/ // 18 children in Scope
/*4946*/            OPC_CheckCondCode, ISD::SETEQ,
/*4948*/            OPC_MoveParent,
/*4949*/            OPC_MoveParent,
/*4950*/            OPC_RecordChild2, // #2 = $DISP
/*4951*/            OPC_MoveChild, 2,
/*4953*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4956*/            OPC_MoveParent,
/*4957*/            OPC_EmitMergeInputChains1_0,
/*4958*/            OPC_EmitInteger, MVT::i64, 20, 
/*4961*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4964*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETEQ:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 20:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*4973*/          /*Scope*/ 27, /*->5001*/
/*4974*/            OPC_CheckCondCode, ISD::SETNE,
/*4976*/            OPC_MoveParent,
/*4977*/            OPC_MoveParent,
/*4978*/            OPC_RecordChild2, // #2 = $DISP
/*4979*/            OPC_MoveChild, 2,
/*4981*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*4984*/            OPC_MoveParent,
/*4985*/            OPC_EmitMergeInputChains1_0,
/*4986*/            OPC_EmitInteger, MVT::i64, 21, 
/*4989*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*4992*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETNE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5001*/          /*Scope*/ 27, /*->5029*/
/*5002*/            OPC_CheckCondCode, ISD::SETGE,
/*5004*/            OPC_MoveParent,
/*5005*/            OPC_MoveParent,
/*5006*/            OPC_RecordChild2, // #2 = $DISP
/*5007*/            OPC_MoveChild, 2,
/*5009*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5012*/            OPC_MoveParent,
/*5013*/            OPC_EmitMergeInputChains1_0,
/*5014*/            OPC_EmitInteger, MVT::i64, 22, 
/*5017*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5020*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETGE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 22:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5029*/          /*Scope*/ 27, /*->5057*/
/*5030*/            OPC_CheckCondCode, ISD::SETGT,
/*5032*/            OPC_MoveParent,
/*5033*/            OPC_MoveParent,
/*5034*/            OPC_RecordChild2, // #2 = $DISP
/*5035*/            OPC_MoveChild, 2,
/*5037*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5040*/            OPC_MoveParent,
/*5041*/            OPC_EmitMergeInputChains1_0,
/*5042*/            OPC_EmitInteger, MVT::i64, 23, 
/*5045*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5048*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETGT:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 23:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5057*/          /*Scope*/ 27, /*->5085*/
/*5058*/            OPC_CheckCondCode, ISD::SETLE,
/*5060*/            OPC_MoveParent,
/*5061*/            OPC_MoveParent,
/*5062*/            OPC_RecordChild2, // #2 = $DISP
/*5063*/            OPC_MoveChild, 2,
/*5065*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5068*/            OPC_MoveParent,
/*5069*/            OPC_EmitMergeInputChains1_0,
/*5070*/            OPC_EmitInteger, MVT::i64, 24, 
/*5073*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5076*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETLE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 24:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5085*/          /*Scope*/ 27, /*->5113*/
/*5086*/            OPC_CheckCondCode, ISD::SETLT,
/*5088*/            OPC_MoveParent,
/*5089*/            OPC_MoveParent,
/*5090*/            OPC_RecordChild2, // #2 = $DISP
/*5091*/            OPC_MoveChild, 2,
/*5093*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5096*/            OPC_MoveParent,
/*5097*/            OPC_EmitMergeInputChains1_0,
/*5098*/            OPC_EmitInteger, MVT::i64, 25, 
/*5101*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5104*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETLT:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 25:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5113*/          /*Scope*/ 27, /*->5141*/
/*5114*/            OPC_CheckCondCode, ISD::SETOEQ,
/*5116*/            OPC_MoveParent,
/*5117*/            OPC_MoveParent,
/*5118*/            OPC_RecordChild2, // #2 = $DISP
/*5119*/            OPC_MoveChild, 2,
/*5121*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5124*/            OPC_MoveParent,
/*5125*/            OPC_EmitMergeInputChains1_0,
/*5126*/            OPC_EmitInteger, MVT::i64, 20, 
/*5129*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5132*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETOEQ:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 20:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5141*/          /*Scope*/ 27, /*->5169*/
/*5142*/            OPC_CheckCondCode, ISD::SETUEQ,
/*5144*/            OPC_MoveParent,
/*5145*/            OPC_MoveParent,
/*5146*/            OPC_RecordChild2, // #2 = $DISP
/*5147*/            OPC_MoveChild, 2,
/*5149*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5152*/            OPC_MoveParent,
/*5153*/            OPC_EmitMergeInputChains1_0,
/*5154*/            OPC_EmitInteger, MVT::i64, 20, 
/*5157*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5160*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETUEQ:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 20:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5169*/          /*Scope*/ 27, /*->5197*/
/*5170*/            OPC_CheckCondCode, ISD::SETOGE,
/*5172*/            OPC_MoveParent,
/*5173*/            OPC_MoveParent,
/*5174*/            OPC_RecordChild2, // #2 = $DISP
/*5175*/            OPC_MoveChild, 2,
/*5177*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5180*/            OPC_MoveParent,
/*5181*/            OPC_EmitMergeInputChains1_0,
/*5182*/            OPC_EmitInteger, MVT::i64, 22, 
/*5185*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5188*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETOGE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 22:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5197*/          /*Scope*/ 27, /*->5225*/
/*5198*/            OPC_CheckCondCode, ISD::SETUGE,
/*5200*/            OPC_MoveParent,
/*5201*/            OPC_MoveParent,
/*5202*/            OPC_RecordChild2, // #2 = $DISP
/*5203*/            OPC_MoveChild, 2,
/*5205*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5208*/            OPC_MoveParent,
/*5209*/            OPC_EmitMergeInputChains1_0,
/*5210*/            OPC_EmitInteger, MVT::i64, 22, 
/*5213*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5216*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETUGE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 22:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5225*/          /*Scope*/ 27, /*->5253*/
/*5226*/            OPC_CheckCondCode, ISD::SETOGT,
/*5228*/            OPC_MoveParent,
/*5229*/            OPC_MoveParent,
/*5230*/            OPC_RecordChild2, // #2 = $DISP
/*5231*/            OPC_MoveChild, 2,
/*5233*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5236*/            OPC_MoveParent,
/*5237*/            OPC_EmitMergeInputChains1_0,
/*5238*/            OPC_EmitInteger, MVT::i64, 23, 
/*5241*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5244*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETOGT:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 23:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5253*/          /*Scope*/ 27, /*->5281*/
/*5254*/            OPC_CheckCondCode, ISD::SETUGT,
/*5256*/            OPC_MoveParent,
/*5257*/            OPC_MoveParent,
/*5258*/            OPC_RecordChild2, // #2 = $DISP
/*5259*/            OPC_MoveChild, 2,
/*5261*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5264*/            OPC_MoveParent,
/*5265*/            OPC_EmitMergeInputChains1_0,
/*5266*/            OPC_EmitInteger, MVT::i64, 23, 
/*5269*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5272*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETUGT:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 23:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5281*/          /*Scope*/ 27, /*->5309*/
/*5282*/            OPC_CheckCondCode, ISD::SETOLE,
/*5284*/            OPC_MoveParent,
/*5285*/            OPC_MoveParent,
/*5286*/            OPC_RecordChild2, // #2 = $DISP
/*5287*/            OPC_MoveChild, 2,
/*5289*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5292*/            OPC_MoveParent,
/*5293*/            OPC_EmitMergeInputChains1_0,
/*5294*/            OPC_EmitInteger, MVT::i64, 24, 
/*5297*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5300*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETOLE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 24:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5309*/          /*Scope*/ 27, /*->5337*/
/*5310*/            OPC_CheckCondCode, ISD::SETULE,
/*5312*/            OPC_MoveParent,
/*5313*/            OPC_MoveParent,
/*5314*/            OPC_RecordChild2, // #2 = $DISP
/*5315*/            OPC_MoveChild, 2,
/*5317*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5320*/            OPC_MoveParent,
/*5321*/            OPC_EmitMergeInputChains1_0,
/*5322*/            OPC_EmitInteger, MVT::i64, 24, 
/*5325*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5328*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETULE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 24:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5337*/          /*Scope*/ 27, /*->5365*/
/*5338*/            OPC_CheckCondCode, ISD::SETOLT,
/*5340*/            OPC_MoveParent,
/*5341*/            OPC_MoveParent,
/*5342*/            OPC_RecordChild2, // #2 = $DISP
/*5343*/            OPC_MoveChild, 2,
/*5345*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5348*/            OPC_MoveParent,
/*5349*/            OPC_EmitMergeInputChains1_0,
/*5350*/            OPC_EmitInteger, MVT::i64, 25, 
/*5353*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5356*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETOLT:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 25:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5365*/          /*Scope*/ 27, /*->5393*/
/*5366*/            OPC_CheckCondCode, ISD::SETULT,
/*5368*/            OPC_MoveParent,
/*5369*/            OPC_MoveParent,
/*5370*/            OPC_RecordChild2, // #2 = $DISP
/*5371*/            OPC_MoveChild, 2,
/*5373*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5376*/            OPC_MoveParent,
/*5377*/            OPC_EmitMergeInputChains1_0,
/*5378*/            OPC_EmitInteger, MVT::i64, 25, 
/*5381*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5384*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETULT:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 25:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5393*/          /*Scope*/ 27, /*->5421*/
/*5394*/            OPC_CheckCondCode, ISD::SETONE,
/*5396*/            OPC_MoveParent,
/*5397*/            OPC_MoveParent,
/*5398*/            OPC_RecordChild2, // #2 = $DISP
/*5399*/            OPC_MoveChild, 2,
/*5401*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5404*/            OPC_MoveParent,
/*5405*/            OPC_EmitMergeInputChains1_0,
/*5406*/            OPC_EmitInteger, MVT::i64, 21, 
/*5409*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5412*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETONE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5421*/          /*Scope*/ 27, /*->5449*/
/*5422*/            OPC_CheckCondCode, ISD::SETUNE,
/*5424*/            OPC_MoveParent,
/*5425*/            OPC_MoveParent,
/*5426*/            OPC_RecordChild2, // #2 = $DISP
/*5427*/            OPC_MoveChild, 2,
/*5429*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5432*/            OPC_MoveParent,
/*5433*/            OPC_EmitMergeInputChains1_0,
/*5434*/            OPC_EmitInteger, MVT::i64, 21, 
/*5437*/            OPC_EmitNodeXForm, 3, 3, // immBRCond
/*5440*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, (fpimm:f64)<<P:Predicate_immFPZ>>, SETUNE:Other), (bb:Other):$DISP) - Complexity = 10
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), F8RC:f64:$RA, (bb:Other):$DISP)
/*5449*/          0, /*End of Scope*/
/*5450*/        /*Scope*/ 49, /*->5500*/
/*5451*/          OPC_RecordChild1, // #2 = $L
/*5452*/          OPC_MoveChild, 1,
/*5454*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*5457*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*5459*/          OPC_MoveParent,
/*5460*/          OPC_MoveChild, 2,
/*5462*/          OPC_CheckCondCode, ISD::SETNE,
/*5464*/          OPC_MoveParent,
/*5465*/          OPC_MoveParent,
/*5466*/          OPC_RecordChild2, // #3 = $DISP
/*5467*/          OPC_MoveChild, 2,
/*5469*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5472*/          OPC_MoveParent,
/*5473*/          OPC_EmitMergeInputChains1_0,
/*5474*/          OPC_EmitInteger, MVT::i64, 0, 
/*5477*/          OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5480*/          OPC_EmitConvertToTarget, 2,
/*5482*/          OPC_EmitNode, TARGET_OPCODE(Alpha::CMPEQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 6,  // Results = #7 
/*5491*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 5, 7, 3, 
                  // Src: (brcond (setcc:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L, SETNE:Other), (bb:Other):$DISP) - Complexity = 10
                  // Dst: (COND_BRANCH_I (immBRCond:i64 0:i64), (CMPEQi:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L), (bb:Other):$DISP)
/*5500*/        /*Scope*/ 41, /*->5542*/
/*5501*/          OPC_CheckChild0Type, MVT::i64,
/*5503*/          OPC_RecordChild1, // #2 = $RB
/*5504*/          OPC_MoveChild, 2,
/*5506*/          OPC_CheckCondCode, ISD::SETNE,
/*5508*/          OPC_MoveParent,
/*5509*/          OPC_MoveParent,
/*5510*/          OPC_RecordChild2, // #3 = $DISP
/*5511*/          OPC_MoveChild, 2,
/*5513*/          OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5516*/          OPC_MoveParent,
/*5517*/          OPC_EmitMergeInputChains1_0,
/*5518*/          OPC_EmitInteger, MVT::i64, 0, 
/*5521*/          OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5524*/          OPC_EmitNode, TARGET_OPCODE(Alpha::CMPEQ), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5533*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                      0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                  // Src: (brcond (setcc:i64 GPRC:i64:$RA, GPRC:i64:$RB, SETNE:Other), (bb:Other):$DISP) - Complexity = 6
                  // Dst: (COND_BRANCH_I (immBRCond:i64 0:i64), (CMPEQ:i64 GPRC:i64:$RA, GPRC:i64:$RB), (bb:Other):$DISP)
/*5542*/        /*Scope*/ 33|128,5/*673*/, /*->6217*/
/*5544*/          OPC_CheckChild0Type, MVT::f64,
/*5546*/          OPC_RecordChild1, // #2 = $RB
/*5547*/          OPC_MoveChild, 2,
/*5549*/          OPC_Scope, 36, /*->5587*/ // 18 children in Scope
/*5551*/            OPC_CheckCondCode, ISD::SETEQ,
/*5553*/            OPC_MoveParent,
/*5554*/            OPC_MoveParent,
/*5555*/            OPC_RecordChild2, // #3 = $DISP
/*5556*/            OPC_MoveChild, 2,
/*5558*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5561*/            OPC_MoveParent,
/*5562*/            OPC_EmitMergeInputChains1_0,
/*5563*/            OPC_EmitInteger, MVT::i64, 21, 
/*5566*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5569*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5578*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETEQ:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5587*/          /*Scope*/ 36, /*->5624*/
/*5588*/            OPC_CheckCondCode, ISD::SETOEQ,
/*5590*/            OPC_MoveParent,
/*5591*/            OPC_MoveParent,
/*5592*/            OPC_RecordChild2, // #3 = $DISP
/*5593*/            OPC_MoveChild, 2,
/*5595*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5598*/            OPC_MoveParent,
/*5599*/            OPC_EmitMergeInputChains1_0,
/*5600*/            OPC_EmitInteger, MVT::i64, 21, 
/*5603*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5606*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5615*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOEQ:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5624*/          /*Scope*/ 36, /*->5661*/
/*5625*/            OPC_CheckCondCode, ISD::SETUEQ,
/*5627*/            OPC_MoveParent,
/*5628*/            OPC_MoveParent,
/*5629*/            OPC_RecordChild2, // #3 = $DISP
/*5630*/            OPC_MoveChild, 2,
/*5632*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5635*/            OPC_MoveParent,
/*5636*/            OPC_EmitMergeInputChains1_0,
/*5637*/            OPC_EmitInteger, MVT::i64, 21, 
/*5640*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5643*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5652*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUEQ:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5661*/          /*Scope*/ 36, /*->5698*/
/*5662*/            OPC_CheckCondCode, ISD::SETLT,
/*5664*/            OPC_MoveParent,
/*5665*/            OPC_MoveParent,
/*5666*/            OPC_RecordChild2, // #3 = $DISP
/*5667*/            OPC_MoveChild, 2,
/*5669*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5672*/            OPC_MoveParent,
/*5673*/            OPC_EmitMergeInputChains1_0,
/*5674*/            OPC_EmitInteger, MVT::i64, 21, 
/*5677*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5680*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5689*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETLT:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5698*/          /*Scope*/ 36, /*->5735*/
/*5699*/            OPC_CheckCondCode, ISD::SETOLT,
/*5701*/            OPC_MoveParent,
/*5702*/            OPC_MoveParent,
/*5703*/            OPC_RecordChild2, // #3 = $DISP
/*5704*/            OPC_MoveChild, 2,
/*5706*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5709*/            OPC_MoveParent,
/*5710*/            OPC_EmitMergeInputChains1_0,
/*5711*/            OPC_EmitInteger, MVT::i64, 21, 
/*5714*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5717*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5726*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOLT:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5735*/          /*Scope*/ 36, /*->5772*/
/*5736*/            OPC_CheckCondCode, ISD::SETULT,
/*5738*/            OPC_MoveParent,
/*5739*/            OPC_MoveParent,
/*5740*/            OPC_RecordChild2, // #3 = $DISP
/*5741*/            OPC_MoveChild, 2,
/*5743*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5746*/            OPC_MoveParent,
/*5747*/            OPC_EmitMergeInputChains1_0,
/*5748*/            OPC_EmitInteger, MVT::i64, 21, 
/*5751*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5754*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5763*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETULT:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLT:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5772*/          /*Scope*/ 36, /*->5809*/
/*5773*/            OPC_CheckCondCode, ISD::SETLE,
/*5775*/            OPC_MoveParent,
/*5776*/            OPC_MoveParent,
/*5777*/            OPC_RecordChild2, // #3 = $DISP
/*5778*/            OPC_MoveChild, 2,
/*5780*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5783*/            OPC_MoveParent,
/*5784*/            OPC_EmitMergeInputChains1_0,
/*5785*/            OPC_EmitInteger, MVT::i64, 21, 
/*5788*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5791*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5800*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETLE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5809*/          /*Scope*/ 36, /*->5846*/
/*5810*/            OPC_CheckCondCode, ISD::SETOLE,
/*5812*/            OPC_MoveParent,
/*5813*/            OPC_MoveParent,
/*5814*/            OPC_RecordChild2, // #3 = $DISP
/*5815*/            OPC_MoveChild, 2,
/*5817*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5820*/            OPC_MoveParent,
/*5821*/            OPC_EmitMergeInputChains1_0,
/*5822*/            OPC_EmitInteger, MVT::i64, 21, 
/*5825*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5828*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5837*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOLE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5846*/          /*Scope*/ 36, /*->5883*/
/*5847*/            OPC_CheckCondCode, ISD::SETULE,
/*5849*/            OPC_MoveParent,
/*5850*/            OPC_MoveParent,
/*5851*/            OPC_RecordChild2, // #3 = $DISP
/*5852*/            OPC_MoveChild, 2,
/*5854*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5857*/            OPC_MoveParent,
/*5858*/            OPC_EmitMergeInputChains1_0,
/*5859*/            OPC_EmitInteger, MVT::i64, 21, 
/*5862*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5865*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*5874*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETULE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLE:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*5883*/          /*Scope*/ 36, /*->5920*/
/*5884*/            OPC_CheckCondCode, ISD::SETGT,
/*5886*/            OPC_MoveParent,
/*5887*/            OPC_MoveParent,
/*5888*/            OPC_RecordChild2, // #3 = $DISP
/*5889*/            OPC_MoveChild, 2,
/*5891*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5894*/            OPC_MoveParent,
/*5895*/            OPC_EmitMergeInputChains1_0,
/*5896*/            OPC_EmitInteger, MVT::i64, 21, 
/*5899*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5902*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 1,  // Results = #6 
/*5911*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETGT:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA), (bb:Other):$DISP)
/*5920*/          /*Scope*/ 36, /*->5957*/
/*5921*/            OPC_CheckCondCode, ISD::SETOGT,
/*5923*/            OPC_MoveParent,
/*5924*/            OPC_MoveParent,
/*5925*/            OPC_RecordChild2, // #3 = $DISP
/*5926*/            OPC_MoveChild, 2,
/*5928*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5931*/            OPC_MoveParent,
/*5932*/            OPC_EmitMergeInputChains1_0,
/*5933*/            OPC_EmitInteger, MVT::i64, 21, 
/*5936*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5939*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 1,  // Results = #6 
/*5948*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOGT:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA), (bb:Other):$DISP)
/*5957*/          /*Scope*/ 36, /*->5994*/
/*5958*/            OPC_CheckCondCode, ISD::SETUGT,
/*5960*/            OPC_MoveParent,
/*5961*/            OPC_MoveParent,
/*5962*/            OPC_RecordChild2, // #3 = $DISP
/*5963*/            OPC_MoveChild, 2,
/*5965*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*5968*/            OPC_MoveParent,
/*5969*/            OPC_EmitMergeInputChains1_0,
/*5970*/            OPC_EmitInteger, MVT::i64, 21, 
/*5973*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*5976*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLT), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 1,  // Results = #6 
/*5985*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUGT:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLT:f64 F8RC:f64:$RB, F8RC:f64:$RA), (bb:Other):$DISP)
/*5994*/          /*Scope*/ 36, /*->6031*/
/*5995*/            OPC_CheckCondCode, ISD::SETGE,
/*5997*/            OPC_MoveParent,
/*5998*/            OPC_MoveParent,
/*5999*/            OPC_RecordChild2, // #3 = $DISP
/*6000*/            OPC_MoveChild, 2,
/*6002*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6005*/            OPC_MoveParent,
/*6006*/            OPC_EmitMergeInputChains1_0,
/*6007*/            OPC_EmitInteger, MVT::i64, 21, 
/*6010*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*6013*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 1,  // Results = #6 
/*6022*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETGE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA), (bb:Other):$DISP)
/*6031*/          /*Scope*/ 36, /*->6068*/
/*6032*/            OPC_CheckCondCode, ISD::SETOGE,
/*6034*/            OPC_MoveParent,
/*6035*/            OPC_MoveParent,
/*6036*/            OPC_RecordChild2, // #3 = $DISP
/*6037*/            OPC_MoveChild, 2,
/*6039*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6042*/            OPC_MoveParent,
/*6043*/            OPC_EmitMergeInputChains1_0,
/*6044*/            OPC_EmitInteger, MVT::i64, 21, 
/*6047*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*6050*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 1,  // Results = #6 
/*6059*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETOGE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA), (bb:Other):$DISP)
/*6068*/          /*Scope*/ 36, /*->6105*/
/*6069*/            OPC_CheckCondCode, ISD::SETUGE,
/*6071*/            OPC_MoveParent,
/*6072*/            OPC_MoveParent,
/*6073*/            OPC_RecordChild2, // #3 = $DISP
/*6074*/            OPC_MoveChild, 2,
/*6076*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6079*/            OPC_MoveParent,
/*6080*/            OPC_EmitMergeInputChains1_0,
/*6081*/            OPC_EmitInteger, MVT::i64, 21, 
/*6084*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*6087*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTLE), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 2, 1,  // Results = #6 
/*6096*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUGE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 21:i64), (CMPTLE:f64 F8RC:f64:$RB, F8RC:f64:$RA), (bb:Other):$DISP)
/*6105*/          /*Scope*/ 36, /*->6142*/
/*6106*/            OPC_CheckCondCode, ISD::SETNE,
/*6108*/            OPC_MoveParent,
/*6109*/            OPC_MoveParent,
/*6110*/            OPC_RecordChild2, // #3 = $DISP
/*6111*/            OPC_MoveChild, 2,
/*6113*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6116*/            OPC_MoveParent,
/*6117*/            OPC_EmitMergeInputChains1_0,
/*6118*/            OPC_EmitInteger, MVT::i64, 20, 
/*6121*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*6124*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*6133*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETNE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 20:i64), (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*6142*/          /*Scope*/ 36, /*->6179*/
/*6143*/            OPC_CheckCondCode, ISD::SETONE,
/*6145*/            OPC_MoveParent,
/*6146*/            OPC_MoveParent,
/*6147*/            OPC_RecordChild2, // #3 = $DISP
/*6148*/            OPC_MoveChild, 2,
/*6150*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6153*/            OPC_MoveParent,
/*6154*/            OPC_EmitMergeInputChains1_0,
/*6155*/            OPC_EmitInteger, MVT::i64, 20, 
/*6158*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*6161*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*6170*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETONE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 20:i64), (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*6179*/          /*Scope*/ 36, /*->6216*/
/*6180*/            OPC_CheckCondCode, ISD::SETUNE,
/*6182*/            OPC_MoveParent,
/*6183*/            OPC_MoveParent,
/*6184*/            OPC_RecordChild2, // #3 = $DISP
/*6185*/            OPC_MoveChild, 2,
/*6187*/            OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6190*/            OPC_MoveParent,
/*6191*/            OPC_EmitMergeInputChains1_0,
/*6192*/            OPC_EmitInteger, MVT::i64, 20, 
/*6195*/            OPC_EmitNodeXForm, 3, 4, // immBRCond
/*6198*/            OPC_EmitNode, TARGET_OPCODE(Alpha::CMPTEQ), 0,
                        1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 2,  // Results = #6 
/*6207*/            OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_F), 0|OPFL_Chain,
                        0/*#VTs*/, 3/*#Ops*/, 5, 6, 3, 
                    // Src: (brcond (setcc:i64 F8RC:f64:$RA, F8RC:f64:$RB, SETUNE:Other), (bb:Other):$DISP) - Complexity = 6
                    // Dst: (COND_BRANCH_F (immBRCond:i64 20:i64), (CMPTEQ:f64 F8RC:f64:$RA, F8RC:f64:$RB), (bb:Other):$DISP)
/*6216*/          0, /*End of Scope*/
/*6217*/        0, /*End of Scope*/
              /*SwitchOpcode*/ 30,  TARGET_OPCODE(ISD::AND),// ->6251
/*6221*/        OPC_RecordChild0, // #1 = $RA
/*6222*/        OPC_MoveChild, 1,
/*6224*/        OPC_CheckInteger, 1, 
/*6226*/        OPC_MoveParent,
/*6227*/        OPC_MoveParent,
/*6228*/        OPC_RecordChild2, // #2 = $DISP
/*6229*/        OPC_MoveChild, 2,
/*6231*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6234*/        OPC_MoveParent,
/*6235*/        OPC_EmitMergeInputChains1_0,
/*6236*/        OPC_EmitInteger, MVT::i64, 6, 
/*6239*/        OPC_EmitNodeXForm, 3, 3, // immBRCond
/*6242*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                    0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
                // Src: (brcond (and:i64 GPRC:i64:$RA, 1:i64), (bb:Other):$DISP) - Complexity = 11
                // Dst: (COND_BRANCH_I (immBRCond:i64 6:i64), GPRC:i64:$RA, (bb:Other):$DISP)
              0, // EndSwitchOpcode
/*6252*/    /*Scope*/ 24, /*->6277*/
/*6253*/      OPC_RecordChild1, // #1 = $RA
/*6254*/      OPC_RecordChild2, // #2 = $DISP
/*6255*/      OPC_MoveChild, 2,
/*6257*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*6260*/      OPC_MoveParent,
/*6261*/      OPC_EmitMergeInputChains1_0,
/*6262*/      OPC_EmitInteger, MVT::i64, 1, 
/*6265*/      OPC_EmitNodeXForm, 3, 3, // immBRCond
/*6268*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::COND_BRANCH_I), 0|OPFL_Chain,
                  0/*#VTs*/, 3/*#Ops*/, 4, 1, 2, 
              // Src: (brcond GPRC:i64:$RA, (bb:Other):$DISP) - Complexity = 3
              // Dst: (COND_BRANCH_I (immBRCond:i64 1:i64), GPRC:i64:$RA, (bb:Other):$DISP)
/*6277*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::CALLSEQ_END),// ->6307
/*6281*/    OPC_RecordNode,   // #0 = 'callseq_end' chained node
/*6282*/    OPC_CaptureFlagInput,
/*6283*/    OPC_RecordChild1, // #1 = $amt1
/*6284*/    OPC_MoveChild, 1,
/*6286*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*6289*/    OPC_MoveParent,
/*6290*/    OPC_RecordChild2, // #2 = $amt2
/*6291*/    OPC_MoveChild, 2,
/*6293*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*6296*/    OPC_MoveParent,
/*6297*/    OPC_EmitMergeInputChains1_0,
/*6298*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADJUSTSTACKDOWN), 0|OPFL_Chain|OPFL_FlagInput|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
            // Src: (callseq_end (timm:i64):$amt1, (timm:i64):$amt2) - Complexity = 9
            // Dst: (ADJUSTSTACKDOWN:i64 (timm:i64):$amt1, (timm:i64):$amt2)
          /*SwitchOpcode*/ 46|128,2/*302*/,  TARGET_OPCODE(ISD::MUL),// ->6613
/*6311*/    OPC_RecordChild0, // #0 = $RA
/*6312*/    OPC_Scope, 56, /*->6370*/ // 2 children in Scope
/*6314*/      OPC_MoveChild, 1,
/*6316*/      OPC_Scope, 12, /*->6330*/ // 4 children in Scope
/*6318*/        OPC_CheckInteger, 5, 
/*6320*/        OPC_MoveParent,
/*6321*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDQr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0, 
                // Src: (mul:i64 GPRC:i64:$RA, 5:i64) - Complexity = 9
                // Dst: (S4ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RA)
/*6330*/      /*Scope*/ 12, /*->6343*/
/*6331*/        OPC_CheckInteger, 9, 
/*6333*/        OPC_MoveParent,
/*6334*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8ADDQr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0, 
                // Src: (mul:i64 GPRC:i64:$RA, 9:i64) - Complexity = 9
                // Dst: (S8ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RA)
/*6343*/      /*Scope*/ 12, /*->6356*/
/*6344*/        OPC_CheckInteger, 3, 
/*6346*/        OPC_MoveParent,
/*6347*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4SUBQr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0, 
                // Src: (mul:i64 GPRC:i64:$RA, 3:i64) - Complexity = 9
                // Dst: (S4SUBQr:i64 GPRC:i64:$RA, GPRC:i64:$RA)
/*6356*/      /*Scope*/ 12, /*->6369*/
/*6357*/        OPC_CheckInteger, 7, 
/*6359*/        OPC_MoveParent,
/*6360*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S8SUBQr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0, 
                // Src: (mul:i64 GPRC:i64:$RA, 7:i64) - Complexity = 9
                // Dst: (S8SUBQr:i64 GPRC:i64:$RA, GPRC:i64:$RA)
/*6369*/      0, /*End of Scope*/
/*6370*/    /*Scope*/ 112|128,1/*240*/, /*->6612*/
/*6372*/      OPC_RecordChild1, // #1 = $imm
/*6373*/      OPC_Scope, 97|128,1/*225*/, /*->6601*/ // 2 children in Scope
/*6376*/        OPC_MoveChild, 1,
/*6378*/        OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*6381*/        OPC_Scope, 26, /*->6409*/ // 7 children in Scope
/*6383*/          OPC_CheckPredicate, 23, // Predicate_immRem1
/*6385*/          OPC_MoveParent,
/*6386*/          OPC_EmitConvertToTarget, 1,
/*6388*/          OPC_EmitNodeXForm, 4, 2, // nearP2X
/*6391*/          OPC_EmitNode, TARGET_OPCODE(Alpha::SLr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3,  // Results = #4 
/*6400*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 4, 0, 
                  // Src: (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immRem1>>:$imm) - Complexity = 8
                  // Dst: (ADDQr:i64 (SLr:i64 GPRC:i64:$RA, (nearP2X:i64 (imm:i64)<<P:Predicate_immRem1>>:$imm)), GPRC:i64:$RA)
/*6409*/        /*Scope*/ 26, /*->6436*/
/*6410*/          OPC_CheckPredicate, 24, // Predicate_immRem4
/*6412*/          OPC_MoveParent,
/*6413*/          OPC_EmitConvertToTarget, 1,
/*6415*/          OPC_EmitNodeXForm, 4, 2, // nearP2X
/*6418*/          OPC_EmitNode, TARGET_OPCODE(Alpha::SLr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3,  // Results = #4 
/*6427*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::S4ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 4, 
                  // Src: (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immRem4>>:$imm) - Complexity = 8
                  // Dst: (S4ADDQr:i64 GPRC:i64:$RA, (SLr:i64 GPRC:i64:$RA, (nearP2X:i64 (imm:i64)<<P:Predicate_immRem4>>:$imm)))
/*6436*/        /*Scope*/ 35, /*->6472*/
/*6437*/          OPC_CheckPredicate, 25, // Predicate_immRem2
/*6439*/          OPC_MoveParent,
/*6440*/          OPC_EmitConvertToTarget, 1,
/*6442*/          OPC_EmitNodeXForm, 4, 2, // nearP2X
/*6445*/          OPC_EmitNode, TARGET_OPCODE(Alpha::SLr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3,  // Results = #4 
/*6454*/          OPC_EmitNode, TARGET_OPCODE(Alpha::ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0,  // Results = #5 
/*6463*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 4, 5, 
                  // Src: (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immRem2>>:$imm) - Complexity = 8
                  // Dst: (ADDQr:i64 (SLr:i64 GPRC:i64:$RA, (nearP2X:i64 (imm:i64)<<P:Predicate_immRem2>>:$imm)), (ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RA))
/*6472*/        /*Scope*/ 35, /*->6508*/
/*6473*/          OPC_CheckPredicate, 26, // Predicate_immRem3
/*6475*/          OPC_MoveParent,
/*6476*/          OPC_EmitConvertToTarget, 1,
/*6478*/          OPC_EmitNodeXForm, 4, 2, // nearP2X
/*6481*/          OPC_EmitNode, TARGET_OPCODE(Alpha::SLr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3,  // Results = #4 
/*6490*/          OPC_EmitNode, TARGET_OPCODE(Alpha::S4SUBQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0,  // Results = #5 
/*6499*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 4, 5, 
                  // Src: (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immRem3>>:$imm) - Complexity = 8
                  // Dst: (ADDQr:i64 (SLr:i64 GPRC:i64:$RA, (nearP2X:i64 (imm:i64)<<P:Predicate_immRem3>>:$imm)), (S4SUBQr:i64 GPRC:i64:$RA, GPRC:i64:$RA))
/*6508*/        /*Scope*/ 35, /*->6544*/
/*6509*/          OPC_CheckPredicate, 27, // Predicate_immRem5
/*6511*/          OPC_MoveParent,
/*6512*/          OPC_EmitConvertToTarget, 1,
/*6514*/          OPC_EmitNodeXForm, 4, 2, // nearP2X
/*6517*/          OPC_EmitNode, TARGET_OPCODE(Alpha::SLr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3,  // Results = #4 
/*6526*/          OPC_EmitNode, TARGET_OPCODE(Alpha::S4ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 0,  // Results = #5 
/*6535*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 4, 5, 
                  // Src: (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immRem5>>:$imm) - Complexity = 8
                  // Dst: (ADDQr:i64 (SLr:i64 GPRC:i64:$RA, (nearP2X:i64 (imm:i64)<<P:Predicate_immRem5>>:$imm)), (S4ADDQr:i64 GPRC:i64:$RA, GPRC:i64:$RA))
/*6544*/        /*Scope*/ 40, /*->6585*/
/*6545*/          OPC_CheckPredicate, 28, // Predicate_immRemP2
/*6547*/          OPC_MoveParent,
/*6548*/          OPC_EmitConvertToTarget, 1,
/*6550*/          OPC_EmitNodeXForm, 4, 2, // nearP2X
/*6553*/          OPC_EmitNode, TARGET_OPCODE(Alpha::SLr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 3,  // Results = #4 
/*6562*/          OPC_EmitConvertToTarget, 1,
/*6564*/          OPC_EmitNodeXForm, 5, 5, // nearP2RemX
/*6567*/          OPC_EmitNode, TARGET_OPCODE(Alpha::SLi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 6,  // Results = #7 
/*6576*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDQr), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 4, 7, 
                  // Src: (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immRemP2>>:$imm) - Complexity = 8
                  // Dst: (ADDQr:i64 (SLr:i64 GPRC:i64:$RA, (nearP2X:i64 (imm:i64)<<P:Predicate_immRemP2>>:$imm)), (SLi:i64 GPRC:i64:$RA, (nearP2RemX:i64 (imm:i64)<<P:Predicate_immRemP2>>:$imm)))
/*6585*/        /*Scope*/ 14, /*->6600*/
/*6586*/          OPC_CheckPredicate, 0, // Predicate_immUExt8
/*6588*/          OPC_MoveParent,
/*6589*/          OPC_EmitConvertToTarget, 1,
/*6591*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::MULQi), 0,
                      1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
                  // Src: (mul:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
                  // Dst: (MULQi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*6600*/        0, /*End of Scope*/
/*6601*/      /*Scope*/ 9, /*->6611*/
/*6602*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::MULQr), 0,
                    1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
                // Src: (mul:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
                // Dst: (MULQr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*6611*/      0, /*End of Scope*/
/*6612*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SHL),// ->6650
/*6616*/    OPC_RecordChild0, // #0 = $RA
/*6617*/    OPC_RecordChild1, // #1 = $L
/*6618*/    OPC_Scope, 19, /*->6639*/ // 2 children in Scope
/*6620*/      OPC_MoveChild, 1,
/*6622*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*6625*/      OPC_CheckPredicate, 0, // Predicate_immUExt8
/*6627*/      OPC_MoveParent,
/*6628*/      OPC_EmitConvertToTarget, 1,
/*6630*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SLi), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
              // Src: (shl:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
              // Dst: (SLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*6639*/    /*Scope*/ 9, /*->6649*/
/*6640*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SLr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (shl:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
              // Dst: (SLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*6649*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SRA),// ->6687
/*6653*/    OPC_RecordChild0, // #0 = $RA
/*6654*/    OPC_RecordChild1, // #1 = $L
/*6655*/    OPC_Scope, 19, /*->6676*/ // 2 children in Scope
/*6657*/      OPC_MoveChild, 1,
/*6659*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*6662*/      OPC_CheckPredicate, 0, // Predicate_immUExt8
/*6664*/      OPC_MoveParent,
/*6665*/      OPC_EmitConvertToTarget, 1,
/*6667*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SRAi), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
              // Src: (sra:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
              // Dst: (SRAi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*6676*/    /*Scope*/ 9, /*->6686*/
/*6677*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SRAr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (sra:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
              // Dst: (SRAr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*6686*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::SRL),// ->6724
/*6690*/    OPC_RecordChild0, // #0 = $RA
/*6691*/    OPC_RecordChild1, // #1 = $L
/*6692*/    OPC_Scope, 19, /*->6713*/ // 2 children in Scope
/*6694*/      OPC_MoveChild, 1,
/*6696*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*6699*/      OPC_CheckPredicate, 0, // Predicate_immUExt8
/*6701*/      OPC_MoveParent,
/*6702*/      OPC_EmitConvertToTarget, 1,
/*6704*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SRLi), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
              // Src: (srl:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
              // Dst: (SRLi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*6713*/    /*Scope*/ 9, /*->6723*/
/*6714*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SRLr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (srl:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
              // Dst: (SRLr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*6723*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::MULHU),// ->6761
/*6727*/    OPC_RecordChild0, // #0 = $RA
/*6728*/    OPC_RecordChild1, // #1 = $L
/*6729*/    OPC_Scope, 19, /*->6750*/ // 2 children in Scope
/*6731*/      OPC_MoveChild, 1,
/*6733*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::Constant),
/*6736*/      OPC_CheckPredicate, 0, // Predicate_immUExt8
/*6738*/      OPC_MoveParent,
/*6739*/      OPC_EmitConvertToTarget, 1,
/*6741*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::UMULHi), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 2, 
              // Src: (mulhu:i64 GPRC:i64:$RA, (imm:i64)<<P:Predicate_immUExt8>>:$L) - Complexity = 7
              // Dst: (UMULHi:i64 GPRC:i64:$RA, (imm:i64):$L)
/*6750*/    /*Scope*/ 9, /*->6760*/
/*6751*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::UMULHr), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (mulhu:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
              // Dst: (UMULHr:i64 GPRC:i64:$RA, GPRC:i64:$RB)
/*6760*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 17,  TARGET_OPCODE(ISD::CALLSEQ_START),// ->6781
/*6764*/    OPC_RecordNode,   // #0 = 'callseq_start' chained node
/*6765*/    OPC_RecordChild1, // #1 = $amt
/*6766*/    OPC_MoveChild, 1,
/*6768*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::TargetConstant),
/*6771*/    OPC_MoveParent,
/*6772*/    OPC_EmitMergeInputChains1_0,
/*6773*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADJUSTSTACKUP), 0|OPFL_Chain|OPFL_FlagOutput,
                1/*#VTs*/, MVT::i64, 1/*#Ops*/, 1, 
            // Src: (callseq_start (timm:i64):$amt) - Complexity = 6
            // Dst: (ADJUSTSTACKUP:i64 (timm:i64):$amt)
          /*SwitchOpcode*/ 50,  TARGET_OPCODE(AlphaISD::GPRelLo),// ->6834
/*6784*/    OPC_RecordChild0, // #0 = $DISP
/*6785*/    OPC_MoveChild, 0,
/*6787*/    OPC_SwitchOpcode /*3 cases */, 12,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->6803
/*6791*/      OPC_MoveParent,
/*6792*/      OPC_RecordChild1, // #1 = $RB
/*6793*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDAr), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_gprello:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDAr:i64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
            /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetConstantPool),// ->6818
/*6806*/      OPC_MoveParent,
/*6807*/      OPC_RecordChild1, // #1 = $RB
/*6808*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDAr), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_gprello:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDAr:i64:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
            /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetJumpTable),// ->6833
/*6821*/      OPC_MoveParent,
/*6822*/      OPC_RecordChild1, // #1 = $RB
/*6823*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDAr), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_gprello:i64 (tjumptable:i64):$DISP, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDAr:i64:i64 (tjumptable:i64):$DISP, GPRC:i64:$RB)
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 50,  TARGET_OPCODE(AlphaISD::GPRelHi),// ->6887
/*6837*/    OPC_RecordChild0, // #0 = $DISP
/*6838*/    OPC_MoveChild, 0,
/*6840*/    OPC_SwitchOpcode /*3 cases */, 12,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->6856
/*6844*/      OPC_MoveParent,
/*6845*/      OPC_RecordChild1, // #1 = $RB
/*6846*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDAHr), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_gprelhi:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDAHr:i64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
            /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetConstantPool),// ->6871
/*6859*/      OPC_MoveParent,
/*6860*/      OPC_RecordChild1, // #1 = $RB
/*6861*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDAHr), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_gprelhi:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDAHr:i64:i64 (tconstpool:i64):$DISP, GPRC:i64:$RB)
            /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetJumpTable),// ->6886
/*6874*/      OPC_MoveParent,
/*6875*/      OPC_RecordChild1, // #1 = $RB
/*6876*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDAHr), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_gprelhi:i64 (tjumptable:i64):$DISP, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDAHr:i64:i64 (tjumptable:i64):$DISP, GPRC:i64:$RB)
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 35,  TARGET_OPCODE(AlphaISD::RelLit),// ->6925
/*6890*/    OPC_RecordChild0, // #0 = $DISP
/*6891*/    OPC_MoveChild, 0,
/*6893*/    OPC_SwitchOpcode /*2 cases */, 12,  TARGET_OPCODE(ISD::TargetGlobalAddress),// ->6909
/*6897*/      OPC_MoveParent,
/*6898*/      OPC_RecordChild1, // #1 = $RB
/*6899*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDQl), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_rellit:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDQl:i64:i64 (tglobaladdr:i64):$DISP, GPRC:i64:$RB)
            /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::TargetExternalSymbol),// ->6924
/*6912*/      OPC_MoveParent,
/*6913*/      OPC_RecordChild1, // #1 = $RB
/*6914*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDQl), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 0, 1, 
              // Src: (Alpha_rellit:i64 (texternalsym:i64):$ext, GPRC:i64:$RB) - Complexity = 6
              // Dst: (LDQl:i64:i64 (texternalsym:i64):$ext, GPRC:i64:$RB)
            0, // EndSwitchOpcode
          /*SwitchOpcode*/ 16|128,1/*144*/,  TARGET_OPCODE(ISD::Constant),// ->7073
/*6929*/    OPC_RecordNode,   // #0 = $imm
/*6930*/    OPC_Scope, 17, /*->6949*/ // 4 children in Scope
/*6932*/      OPC_CheckPredicate, 4, // Predicate_immSExt16
/*6934*/      OPC_EmitConvertToTarget, 0,
/*6936*/      OPC_EmitRegister, MVT::i64, Alpha::R31,
/*6939*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDA), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 1, 2, 
              // Src: (imm:i64)<<P:Predicate_immSExt16>>:$imm - Complexity = 4
              // Dst: (LDA:i64:i64 (imm:i64)<<P:Predicate_immSExt16>>:$imm, R31:i64)
/*6949*/    /*Scope*/ 35, /*->6985*/
/*6950*/      OPC_CheckPredicate, 29, // Predicate_immConst2Part
/*6952*/      OPC_EmitConvertToTarget, 0,
/*6954*/      OPC_EmitNodeXForm, 6, 1, // LL16
/*6957*/      OPC_EmitConvertToTarget, 0,
/*6959*/      OPC_EmitNodeXForm, 7, 3, // LH16
/*6962*/      OPC_EmitRegister, MVT::i64, Alpha::R31,
/*6965*/      OPC_EmitNode, TARGET_OPCODE(Alpha::LDAH), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 4, 5,  // Results = #6 #7 
/*6975*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LDA), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 6, 
              // Src: (imm:i64)<<P:Predicate_immConst2Part>>:$imm - Complexity = 4
              // Dst: (LDA:i64:i64 (LL16:i64 (imm:i64)<<P:Predicate_immConst2Part>>:$imm), (LDAH:i64:i64 (LH16:i64 (imm:i64)<<P:Predicate_immConst2Part>>:$imm), R31:i64))
/*6985*/    /*Scope*/ 32, /*->7018*/
/*6986*/      OPC_CheckPredicate, 30, // Predicate_immSExt16int
/*6988*/      OPC_EmitConvertToTarget, 0,
/*6990*/      OPC_EmitNodeXForm, 8, 1, // SExt16
/*6993*/      OPC_EmitRegister, MVT::i64, Alpha::R31,
/*6996*/      OPC_EmitNode, TARGET_OPCODE(Alpha::LDA), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 2, 3,  // Results = #4 #5 
/*7006*/      OPC_EmitInteger, MVT::i64, 15, 
/*7009*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ZAPNOTi), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 4, 6, 
              // Src: (imm:i64)<<P:Predicate_immSExt16int>><<X:SExt16>>:$imm - Complexity = 4
              // Dst: (ZAPNOTi:i64 (LDA:i64:i64 (SExt16:i64 (imm:i64)<<P:Predicate_immSExt16int>><<X:SExt16>>:$imm), R31:i64), 15:i64)
/*7018*/    /*Scope*/ 53, /*->7072*/
/*7019*/      OPC_CheckPredicate, 31, // Predicate_immConst2PartInt
/*7021*/      OPC_EmitConvertToTarget, 0,
/*7023*/      OPC_EmitNodeXForm, 9, 1, // SExt32
/*7026*/      OPC_EmitNodeXForm, 6, 2, // LL16
/*7029*/      OPC_EmitConvertToTarget, 0,
/*7031*/      OPC_EmitNodeXForm, 9, 4, // SExt32
/*7034*/      OPC_EmitNodeXForm, 7, 5, // LH16
/*7037*/      OPC_EmitRegister, MVT::i64, Alpha::R31,
/*7040*/      OPC_EmitNode, TARGET_OPCODE(Alpha::LDAH), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 6, 7,  // Results = #8 #9 
/*7050*/      OPC_EmitNode, TARGET_OPCODE(Alpha::LDA), 0,
                  2/*#VTs*/, MVT::i64, MVT::i64, 2/*#Ops*/, 3, 8,  // Results = #10 #11 
/*7060*/      OPC_EmitInteger, MVT::i64, 15, 
/*7063*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ZAPNOTi), 0,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 10, 12, 
              // Src: (imm:i64)<<P:Predicate_immConst2PartInt>><<X:SExt32>>:$imm - Complexity = 4
              // Dst: (ZAPNOTi:i64 (LDA:i64:i64 (LL16:i64 (SExt32:i64 (imm:i64)<<P:Predicate_immConst2PartInt>><<X:SExt32>>:$imm)), (LDAH:i64:i64 (LH16:i64 (SExt32:i64 (imm:i64)<<P:Predicate_immConst2PartInt>><<X:SExt32>>:$imm)), R31:i64)), 15:i64)
/*7072*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 37,  TARGET_OPCODE(ISD::ATOMIC_CMP_SWAP),// ->7113
/*7076*/    OPC_RecordMemRef,
/*7077*/    OPC_RecordNode,   // #0 = 'atomic_cmp_swap' chained node
/*7078*/    OPC_RecordChild1, // #1 = $ptr
/*7079*/    OPC_CheckChild1Type, MVT::i64,
/*7081*/    OPC_RecordChild2, // #2 = $cmp
/*7082*/    OPC_RecordChild3, // #3 = $swp
/*7083*/    OPC_Scope, 13, /*->7098*/ // 2 children in Scope
/*7085*/      OPC_CheckPredicate, 32, // Predicate_atomic_cmp_swap_32
/*7087*/      OPC_EmitMergeInputChains1_0,
/*7088*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CAS32), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
              // Src: (atomic_cmp_swap:i64 GPRC:i64:$ptr, GPRC:i64:$cmp, GPRC:i64:$swp)<<P:Predicate_atomic_cmp_swap_32>> - Complexity = 4
              // Dst: (CAS32:i64 GPRC:i64:$ptr, GPRC:i64:$cmp, GPRC:i64:$swp)
/*7098*/    /*Scope*/ 13, /*->7112*/
/*7099*/      OPC_CheckPredicate, 33, // Predicate_atomic_cmp_swap_64
/*7101*/      OPC_EmitMergeInputChains1_0,
/*7102*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CAS64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 2, 3, 
              // Src: (atomic_cmp_swap:i64 GPRC:i64:$ptr, GPRC:i64:$cmp, GPRC:i64:$swp)<<P:Predicate_atomic_cmp_swap_64>> - Complexity = 4
              // Dst: (CAS64:i64 GPRC:i64:$ptr, GPRC:i64:$cmp, GPRC:i64:$swp)
/*7112*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::ATOMIC_LOAD_ADD),// ->7150
/*7116*/    OPC_RecordMemRef,
/*7117*/    OPC_RecordNode,   // #0 = 'atomic_load_add' chained node
/*7118*/    OPC_RecordChild1, // #1 = $ptr
/*7119*/    OPC_CheckChild1Type, MVT::i64,
/*7121*/    OPC_RecordChild2, // #2 = $swp
/*7122*/    OPC_Scope, 12, /*->7136*/ // 2 children in Scope
/*7124*/      OPC_CheckPredicate, 34, // Predicate_atomic_load_add_32
/*7126*/      OPC_EmitMergeInputChains1_0,
/*7127*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LAS32), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
              // Src: (atomic_load_add:i64 GPRC:i64:$ptr, GPRC:i64:$swp)<<P:Predicate_atomic_load_add_32>> - Complexity = 4
              // Dst: (LAS32:i64 GPRC:i64:$ptr, GPRC:i64:$swp)
/*7136*/    /*Scope*/ 12, /*->7149*/
/*7137*/      OPC_CheckPredicate, 35, // Predicate_atomic_load_add_64
/*7139*/      OPC_EmitMergeInputChains1_0,
/*7140*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::LAS64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
              // Src: (atomic_load_add:i64 GPRC:i64:$ptr, GPRC:i64:$swp)<<P:Predicate_atomic_load_add_64>> - Complexity = 4
              // Dst: (LAS64:i64 GPRC:i64:$ptr, GPRC:i64:$swp)
/*7149*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 34,  TARGET_OPCODE(ISD::ATOMIC_SWAP),// ->7187
/*7153*/    OPC_RecordMemRef,
/*7154*/    OPC_RecordNode,   // #0 = 'atomic_swap' chained node
/*7155*/    OPC_RecordChild1, // #1 = $ptr
/*7156*/    OPC_CheckChild1Type, MVT::i64,
/*7158*/    OPC_RecordChild2, // #2 = $swp
/*7159*/    OPC_Scope, 12, /*->7173*/ // 2 children in Scope
/*7161*/      OPC_CheckPredicate, 36, // Predicate_atomic_swap_32
/*7163*/      OPC_EmitMergeInputChains1_0,
/*7164*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SWAP32), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
              // Src: (atomic_swap:i64 GPRC:i64:$ptr, GPRC:i64:$swp)<<P:Predicate_atomic_swap_32>> - Complexity = 4
              // Dst: (SWAP32:i64 GPRC:i64:$ptr, GPRC:i64:$swp)
/*7173*/    /*Scope*/ 12, /*->7186*/
/*7174*/      OPC_CheckPredicate, 37, // Predicate_atomic_swap_64
/*7176*/      OPC_EmitMergeInputChains1_0,
/*7177*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SWAP64), 0|OPFL_Chain|OPFL_MemRefs,
                  1/*#VTs*/, MVT::i64, 2/*#Ops*/, 1, 2, 
              // Src: (atomic_swap:i64 GPRC:i64:$ptr, GPRC:i64:$swp)<<P:Predicate_atomic_swap_64>> - Complexity = 4
              // Dst: (SWAP64:i64 GPRC:i64:$ptr, GPRC:i64:$swp)
/*7186*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::CTLZ),// ->7199
/*7190*/    OPC_RecordChild0, // #0 = $RB
/*7191*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CTLZ), 0,
                1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
            // Src: (ctlz:i64 GPRC:i64:$RB) - Complexity = 3
            // Dst: (CTLZ:i64 GPRC:i64:$RB)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::CTPOP),// ->7211
/*7202*/    OPC_RecordChild0, // #0 = $RB
/*7203*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CTPOP), 0,
                1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
            // Src: (ctpop:i64 GPRC:i64:$RB) - Complexity = 3
            // Dst: (CTPOP:i64 GPRC:i64:$RB)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::CTTZ),// ->7223
/*7214*/    OPC_RecordChild0, // #0 = $RB
/*7215*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CTTZ), 0,
                1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
            // Src: (cttz:i64 GPRC:i64:$RB) - Complexity = 3
            // Dst: (CTTZ:i64 GPRC:i64:$RB)
          /*SwitchOpcode*/ 94,  TARGET_OPCODE(ISD::FNEG),// ->7320
/*7226*/    OPC_Scope, 65, /*->7293*/ // 2 children in Scope
/*7228*/      OPC_MoveChild, 0,
/*7230*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::FCOPYSIGN),
/*7233*/      OPC_RecordChild0, // #0 = $RB
/*7234*/      OPC_RecordChild1, // #1 = $RA
/*7235*/      OPC_Scope, 27, /*->7264*/ // 2 children in Scope
/*7237*/        OPC_CheckChild1Type, MVT::f32,
/*7239*/        OPC_MoveParent,
/*7240*/        OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->7252
/*7243*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNS), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 1, 0, 
                  // Src: (fneg:f32 (fcopysign:f32 F4RC:f32:$RB, F4RC:f32:$RA)) - Complexity = 6
                  // Dst: (CPYSNS:f32 F4RC:f32:$RA, F4RC:f32:$RB)
                /*SwitchType*/ 9,  MVT::f64,// ->7263
/*7254*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNTs), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0, 
                  // Src: (fneg:f64 (fcopysign:f64 F8RC:f64:$RB, F4RC:f32:$RA)) - Complexity = 6
                  // Dst: (CPYSNTs:f64 F4RC:f32:$RA, F8RC:f64:$RB)
                0, // EndSwitchType
/*7264*/      /*Scope*/ 27, /*->7292*/
/*7265*/        OPC_CheckChild1Type, MVT::f64,
/*7267*/        OPC_MoveParent,
/*7268*/        OPC_SwitchType /*2 cases */, 9,  MVT::f64,// ->7280
/*7271*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNT), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0, 
                  // Src: (fneg:f64 (fcopysign:f64 F8RC:f64:$RB, F8RC:f64:$RA)) - Complexity = 6
                  // Dst: (CPYSNT:f64 F8RC:f64:$RA, F8RC:f64:$RB)
                /*SwitchType*/ 9,  MVT::f32,// ->7291
/*7282*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNSt), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 1, 0, 
                  // Src: (fneg:f32 (fcopysign:f32 F4RC:f32:$RB, F8RC:f64:$RA)) - Complexity = 6
                  // Dst: (CPYSNSt:f32 F8RC:f64:$RA, F4RC:f32:$RB)
                0, // EndSwitchType
/*7292*/      0, /*End of Scope*/
/*7293*/    /*Scope*/ 25, /*->7319*/
/*7294*/      OPC_RecordChild0, // #0 = $RB
/*7295*/      OPC_SwitchType /*2 cases */, 9,  MVT::f64,// ->7307
/*7298*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNT), 0,
                    1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 0, 
                // Src: (fneg:f64 F8RC:f64:$RB) - Complexity = 3
                // Dst: (CPYSNT:f64 F8RC:f64:$RB, F8RC:f64:$RB)
              /*SwitchType*/ 9,  MVT::f32,// ->7318
/*7309*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNS), 0,
                    1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 0, 
                // Src: (fneg:f32 F4RC:f32:$RB) - Complexity = 3
                // Dst: (CPYSNS:f32 F4RC:f32:$RB, F4RC:f32:$RB)
              0, // EndSwitchType
/*7319*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 124,  TARGET_OPCODE(ISD::FCOPYSIGN),// ->7447
/*7323*/    OPC_RecordChild0, // #0 = $A
/*7324*/    OPC_Scope, 62, /*->7388*/ // 2 children in Scope
/*7326*/      OPC_MoveChild, 1,
/*7328*/      OPC_CheckOpcode, TARGET_OPCODE(ISD::FNEG),
/*7331*/      OPC_RecordChild0, // #1 = $B
/*7332*/      OPC_SwitchType /*2 cases */, 25,  MVT::f32,// ->7360
/*7335*/        OPC_MoveParent,
/*7336*/        OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->7348
/*7339*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNS), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f32 F4RC:f32:$A, (fneg:f32 F4RC:f32:$B)) - Complexity = 6
                  // Dst: (CPYSNS:f32 F4RC:f32:$B, F4RC:f32:$A)
                /*SwitchType*/ 9,  MVT::f64,// ->7359
/*7350*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNTs), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f64 F8RC:f64:$A, (fneg:f32 F4RC:f32:$B)) - Complexity = 6
                  // Dst: (CPYSNTs:f64 F4RC:f32:$B, F8RC:f64:$A)
                0, // EndSwitchType
              /*SwitchType*/ 25,  MVT::f64,// ->7387
/*7362*/        OPC_MoveParent,
/*7363*/        OPC_SwitchType /*2 cases */, 9,  MVT::f64,// ->7375
/*7366*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNT), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f64 F8RC:f64:$A, (fneg:f64 F8RC:f64:$B)) - Complexity = 6
                  // Dst: (CPYSNT:f64 F8RC:f64:$B, F8RC:f64:$A)
                /*SwitchType*/ 9,  MVT::f32,// ->7386
/*7377*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSNSt), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f32 F4RC:f32:$A, (fneg:f64 F8RC:f64:$B)) - Complexity = 6
                  // Dst: (CPYSNSt:f32 F8RC:f64:$B, F4RC:f32:$A)
                0, // EndSwitchType
              0, // EndSwitchType
/*7388*/    /*Scope*/ 57, /*->7446*/
/*7389*/      OPC_RecordChild1, // #1 = $RA
/*7390*/      OPC_Scope, 26, /*->7418*/ // 2 children in Scope
/*7392*/        OPC_CheckChild1Type, MVT::f32,
/*7394*/        OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->7406
/*7397*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSS), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f32 F4RC:f32:$RB, F4RC:f32:$RA) - Complexity = 3
                  // Dst: (CPYSS:f32 F4RC:f32:$RA, F4RC:f32:$RB)
                /*SwitchType*/ 9,  MVT::f64,// ->7417
/*7408*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSTs), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f64 F8RC:f64:$RB, F4RC:f32:$RA) - Complexity = 3
                  // Dst: (CPYSTs:f64 F4RC:f32:$RA, F8RC:f64:$RB)
                0, // EndSwitchType
/*7418*/      /*Scope*/ 26, /*->7445*/
/*7419*/        OPC_CheckChild1Type, MVT::f64,
/*7421*/        OPC_SwitchType /*2 cases */, 9,  MVT::f64,// ->7433
/*7424*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYST), 0,
                      1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f64 F8RC:f64:$RB, F8RC:f64:$RA) - Complexity = 3
                  // Dst: (CPYST:f64 F8RC:f64:$RA, F8RC:f64:$RB)
                /*SwitchType*/ 9,  MVT::f32,// ->7444
/*7435*/          OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSSt), 0,
                      1/*#VTs*/, MVT::f32, 2/*#Ops*/, 1, 0, 
                  // Src: (fcopysign:f32 F4RC:f32:$RB, F8RC:f64:$RA) - Complexity = 3
                  // Dst: (CPYSSt:f32 F8RC:f64:$RA, F4RC:f32:$RB)
                0, // EndSwitchType
/*7445*/      0, /*End of Scope*/
/*7446*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(AlphaISD::RET_FLAG),// ->7459
/*7450*/    OPC_RecordNode,   // #0 = 'retflag' chained node
/*7451*/    OPC_CaptureFlagInput,
/*7452*/    OPC_EmitMergeInputChains1_0,
/*7453*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::RETDAGp), 0|OPFL_Chain|OPFL_FlagInput,
                0/*#VTs*/, 0/*#Ops*/, 
            // Src: (retflag) - Complexity = 3
            // Dst: (RETDAGp)
          /*SwitchOpcode*/ 12,  TARGET_OPCODE(ISD::BRIND),// ->7474
/*7462*/    OPC_RecordNode,   // #0 = 'brind' chained node
/*7463*/    OPC_RecordChild1, // #1 = $RS
/*7464*/    OPC_CheckChild1Type, MVT::i64,
/*7466*/    OPC_EmitMergeInputChains1_0,
/*7467*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::JMP), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (brind GPRC:i64:$RS) - Complexity = 3
            // Dst: (JMP GPRC:i64:$RS)
          /*SwitchOpcode*/ 54,  TARGET_OPCODE(ISD::BIT_CONVERT),// ->7531
/*7477*/    OPC_RecordChild0, // #0 = $RA
/*7478*/    OPC_Scope, 12, /*->7492*/ // 3 children in Scope
/*7480*/      OPC_CheckChild0Type, MVT::f32,
/*7482*/      OPC_CheckType, MVT::i64,
/*7484*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FTOIS), 0,
                  1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
              // Src: (bitconvert:i64 F4RC:f32:$RA) - Complexity = 3
              // Dst: (FTOIS:i64 F4RC:f32:$RA)
/*7492*/    /*Scope*/ 12, /*->7505*/
/*7493*/      OPC_CheckChild0Type, MVT::f64,
/*7495*/      OPC_CheckType, MVT::i64,
/*7497*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::FTOIT), 0,
                  1/*#VTs*/, MVT::i64, 1/*#Ops*/, 0, 
              // Src: (bitconvert:i64 F8RC:f64:$RA) - Complexity = 3
              // Dst: (FTOIT:i64 F8RC:f64:$RA)
/*7505*/    /*Scope*/ 24, /*->7530*/
/*7506*/      OPC_CheckChild0Type, MVT::i64,
/*7508*/      OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->7519
/*7511*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ITOFS), 0,
                    1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
                // Src: (bitconvert:f32 GPRC:i64:$RA) - Complexity = 3
                // Dst: (ITOFS:f32 GPRC:i64:$RA)
              /*SwitchType*/ 8,  MVT::f64,// ->7529
/*7521*/        OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ITOFT), 0,
                    1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
                // Src: (bitconvert:f64 GPRC:i64:$RA) - Complexity = 3
                // Dst: (ITOFT:f64 GPRC:i64:$RA)
              0, // EndSwitchType
/*7530*/    0, /*End of Scope*/
          /*SwitchOpcode*/ 16,  TARGET_OPCODE(ISD::BR),// ->7550
/*7534*/    OPC_RecordNode,   // #0 = 'br' chained node
/*7535*/    OPC_RecordChild1, // #1 = $DISP
/*7536*/    OPC_MoveChild, 1,
/*7538*/    OPC_CheckOpcode, TARGET_OPCODE(ISD::BasicBlock),
/*7541*/    OPC_MoveParent,
/*7542*/    OPC_EmitMergeInputChains1_0,
/*7543*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::BR), 0|OPFL_Chain,
                0/*#VTs*/, 1/*#Ops*/, 1, 
            // Src: (br (bb:Other):$DISP) - Complexity = 3
            // Dst: (BR (bb:Other):$DISP)
          /*SwitchOpcode*/ 55,  TARGET_OPCODE(ISD::MULHS),// ->7608
/*7553*/    OPC_RecordChild0, // #0 = $RA
/*7554*/    OPC_RecordChild1, // #1 = $RB
/*7555*/    OPC_EmitNode, TARGET_OPCODE(Alpha::UMULHr), 0,
                1/*#VTs*/, MVT::i64, 2/*#Ops*/, 0, 1,  // Results = #2 
/*7564*/    OPC_EmitRegister, MVT::i64, Alpha::R31,
/*7567*/    OPC_EmitNode, TARGET_OPCODE(Alpha::CMOVGEr), 0,
                1/*#VTs*/, MVT::i64, 3/*#Ops*/, 1, 3, 0,  // Results = #4 
/*7577*/    OPC_EmitRegister, MVT::i64, Alpha::R31,
/*7580*/    OPC_EmitNode, TARGET_OPCODE(Alpha::CMOVGEr), 0,
                1/*#VTs*/, MVT::i64, 3/*#Ops*/, 0, 5, 1,  // Results = #6 
/*7590*/    OPC_EmitNode, TARGET_OPCODE(Alpha::ADDQr), 0,
                1/*#VTs*/, MVT::i64, 2/*#Ops*/, 4, 6,  // Results = #7 
/*7599*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBQr), 0,
                1/*#VTs*/, MVT::i64, 2/*#Ops*/, 2, 7, 
            // Src: (mulhs:i64 GPRC:i64:$RA, GPRC:i64:$RB) - Complexity = 3
            // Dst: (SUBQr:i64 (UMULHr:i64 GPRC:i64:$RA, GPRC:i64:$RB), (ADDQr:i64 (CMOVGEr:i64 GPRC:i64:$RB, R31:i64, GPRC:i64:$RA), (CMOVGEr:i64 GPRC:i64:$RA, R31:i64, GPRC:i64:$RB)))
          /*SwitchOpcode*/ 23,  TARGET_OPCODE(ISD::FSQRT),// ->7634
/*7611*/    OPC_RecordChild0, // #0 = $RB
/*7612*/    OPC_SwitchType /*2 cases */, 8,  MVT::f32,// ->7623
/*7615*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SQRTS), 0,
                  1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
              // Src: (fsqrt:f32 F4RC:f32:$RB) - Complexity = 3
              // Dst: (SQRTS:f32 F4RC:f32:$RB)
            /*SwitchType*/ 8,  MVT::f64,// ->7633
/*7625*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SQRTT), 0,
                  1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
              // Src: (fsqrt:f64 F8RC:f64:$RB) - Complexity = 3
              // Dst: (SQRTT:f64 F8RC:f64:$RB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::FADD),// ->7663
/*7637*/    OPC_RecordChild0, // #0 = $RA
/*7638*/    OPC_RecordChild1, // #1 = $RB
/*7639*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->7651
/*7642*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDS), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fadd:f32 F4RC:f32:$RA, F4RC:f32:$RB) - Complexity = 3
              // Dst: (ADDS:f32 F4RC:f32:$RA, F4RC:f32:$RB)
            /*SwitchType*/ 9,  MVT::f64,// ->7662
/*7653*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::ADDT), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fadd:f64 F8RC:f64:$RA, F8RC:f64:$RB) - Complexity = 3
              // Dst: (ADDT:f64 F8RC:f64:$RA, F8RC:f64:$RB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::FSUB),// ->7692
/*7666*/    OPC_RecordChild0, // #0 = $RA
/*7667*/    OPC_RecordChild1, // #1 = $RB
/*7668*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->7680
/*7671*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBS), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fsub:f32 F4RC:f32:$RA, F4RC:f32:$RB) - Complexity = 3
              // Dst: (SUBS:f32 F4RC:f32:$RA, F4RC:f32:$RB)
            /*SwitchType*/ 9,  MVT::f64,// ->7691
/*7682*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::SUBT), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fsub:f64 F8RC:f64:$RA, F8RC:f64:$RB) - Complexity = 3
              // Dst: (SUBT:f64 F8RC:f64:$RA, F8RC:f64:$RB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::FDIV),// ->7721
/*7695*/    OPC_RecordChild0, // #0 = $RA
/*7696*/    OPC_RecordChild1, // #1 = $RB
/*7697*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->7709
/*7700*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::DIVS), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fdiv:f32 F4RC:f32:$RA, F4RC:f32:$RB) - Complexity = 3
              // Dst: (DIVS:f32 F4RC:f32:$RA, F4RC:f32:$RB)
            /*SwitchType*/ 9,  MVT::f64,// ->7720
/*7711*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::DIVT), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fdiv:f64 F8RC:f64:$RA, F8RC:f64:$RB) - Complexity = 3
              // Dst: (DIVT:f64 F8RC:f64:$RA, F8RC:f64:$RB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 26,  TARGET_OPCODE(ISD::FMUL),// ->7750
/*7724*/    OPC_RecordChild0, // #0 = $RA
/*7725*/    OPC_RecordChild1, // #1 = $RB
/*7726*/    OPC_SwitchType /*2 cases */, 9,  MVT::f32,// ->7738
/*7729*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::MULS), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 0, 1, 
              // Src: (fmul:f32 F4RC:f32:$RA, F4RC:f32:$RB) - Complexity = 3
              // Dst: (MULS:f32 F4RC:f32:$RA, F4RC:f32:$RB)
            /*SwitchType*/ 9,  MVT::f64,// ->7749
/*7740*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::MULT), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 0, 1, 
              // Src: (fmul:f64 F8RC:f64:$RA, F8RC:f64:$RB) - Complexity = 3
              // Dst: (MULT:f64 F8RC:f64:$RA, F8RC:f64:$RB)
            0, // EndSwitchType
          /*SwitchOpcode*/ 13,  TARGET_OPCODE(AlphaISD::CVTQS_),// ->7766
/*7753*/    OPC_RecordChild0, // #0 = $RB
/*7754*/    OPC_CheckChild0Type, MVT::f64,
/*7756*/    OPC_CheckType, MVT::f32,
/*7758*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CVTQS), 0,
                1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
            // Src: (Alpha_cvtqs:f32 F8RC:f64:$RB) - Complexity = 3
            // Dst: (CVTQS:f32 F8RC:f64:$RB)
          /*SwitchOpcode*/ 13,  TARGET_OPCODE(AlphaISD::CVTQT_),// ->7782
/*7769*/    OPC_RecordChild0, // #0 = $RB
/*7770*/    OPC_CheckChild0Type, MVT::f64,
/*7772*/    OPC_CheckType, MVT::f64,
/*7774*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CVTQT), 0,
                1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
            // Src: (Alpha_cvtqt:f64 F8RC:f64:$RB) - Complexity = 3
            // Dst: (CVTQT:f64 F8RC:f64:$RB)
          /*SwitchOpcode*/ 11,  TARGET_OPCODE(AlphaISD::CVTTQ_),// ->7796
/*7785*/    OPC_RecordChild0, // #0 = $RB
/*7786*/    OPC_CheckType, MVT::f64,
/*7788*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CVTTQ), 0,
                1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
            // Src: (Alpha_cvttq:f64 F8RC:f64:$RB) - Complexity = 3
            // Dst: (CVTTQ:f64 F8RC:f64:$RB)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::FP_EXTEND),// ->7808
/*7799*/    OPC_RecordChild0, // #0 = $RB
/*7800*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CVTST), 0,
                1/*#VTs*/, MVT::f64, 1/*#Ops*/, 0, 
            // Src: (fextend:f64 F4RC:f32:$RB) - Complexity = 3
            // Dst: (CVTST:f64 F4RC:f32:$RB)
          /*SwitchOpcode*/ 9,  TARGET_OPCODE(ISD::FP_ROUND),// ->7820
/*7811*/    OPC_RecordChild0, // #0 = $RB
/*7812*/    OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CVTTS), 0,
                1/*#VTs*/, MVT::f32, 1/*#Ops*/, 0, 
            // Src: (fround:f32 F8RC:f64:$RB) - Complexity = 3
            // Dst: (CVTTS:f32 F8RC:f64:$RB)
          /*SwitchOpcode*/ 31,  TARGET_OPCODE(ISD::FABS),// ->7854
/*7823*/    OPC_RecordChild0, // #0 = $RB
/*7824*/    OPC_SwitchType /*2 cases */, 12,  MVT::f64,// ->7839
/*7827*/      OPC_EmitRegister, MVT::f64, Alpha::F31,
/*7830*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYST), 0,
                  1/*#VTs*/, MVT::f64, 2/*#Ops*/, 1, 0, 
              // Src: (fabs:f64 F8RC:f64:$RB) - Complexity = 3
              // Dst: (CPYST:f64 F31:f64, F8RC:f64:$RB)
            /*SwitchType*/ 12,  MVT::f32,// ->7853
/*7841*/      OPC_EmitRegister, MVT::f32, Alpha::F31,
/*7844*/      OPC_MorphNodeTo, TARGET_OPCODE(Alpha::CPYSS), 0,
                  1/*#VTs*/, MVT::f32, 2/*#Ops*/, 1, 0, 
              // Src: (fabs:f32 F4RC:f32:$RB) - Complexity = 3
              // Dst: (CPYSS:f32 F31:f32, F4RC:f32:$RB)
            0, // EndSwitchType
          0, // EndSwitchOpcode
    0
  }; // Total Array size is 7856 bytes

  // Opcode Histogram:
  // #OPC_Scope                          = 98
  // #OPC_RecordNode                     = 14
  // #OPC_RecordChild                    = 317
  // #OPC_RecordMemRef                   = 5
  // #OPC_CaptureFlagInput               = 2
  // #OPC_MoveChild                      = 193
  // #OPC_MoveParent                     = 465
  // #OPC_CheckSame                      = 0
  // #OPC_CheckPatternPredicate          = 0
  // #OPC_CheckPredicate                 = 142
  // #OPC_CheckOpcode                    = 126
  // #OPC_SwitchOpcode                   = 12
  // #OPC_CheckType                      = 73
  // #OPC_SwitchType                     = 20
  // #OPC_CheckChildType                 = 25
  // #OPC_CheckInteger                   = 31
  // #OPC_CheckCondCode                  = 123
  // #OPC_CheckValueType                 = 22
  // #OPC_CheckComplexPat                = 0
  // #OPC_CheckAndImm                    = 5
  // #OPC_CheckOrImm                     = 0
  // #OPC_CheckFoldableChainNode         = 0
  // #OPC_EmitInteger                    = 68
  // #OPC_EmitStringInteger              = 0
  // #OPC_EmitRegister                   = 8
  // #OPC_EmitConvertToTarget            = 87
  // #OPC_EmitMergeInputChains           = 107
  // #OPC_EmitCopyToReg                  = 0
  // #OPC_EmitNode                       = 80
  // #OPC_EmitNodeXForm                  = 70
  // #OPC_MarkFlagResults                = 0
  // #OPC_CompleteMatch                  = 0
  // #OPC_MorphNodeTo                    = 324

  #undef TARGET_OPCODE
  return SelectCodeCommon(N, MatcherTable,sizeof(MatcherTable));
}

bool CheckNodePredicate(SDNode *Node, unsigned PredNo) const {
  switch (PredNo) {
  default: assert(0 && "Invalid predicate in table?");
  case 0: { // Predicate_immUExt8
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
 //imm fits in 8 bit zero extended field
  return (uint64_t)N->getZExtValue() == (uint8_t)N->getZExtValue();

  }
  case 1: { // Predicate_immUExt8inv
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
 //inverted imm fits in 8 bit zero extended field
  return (uint64_t)~N->getZExtValue() == (uint8_t)~N->getZExtValue();

  }
  case 2: { // Predicate_zappat
    SDNode *N = Node;

  ConstantSDNode *RHS = dyn_cast<ConstantSDNode>(N->getOperand(1));
  if (!RHS) return 0;
  uint64_t build = get_zapImm(N->getOperand(0), (uint64_t)RHS->getZExtValue());
  return build != 0;

  }
  case 3: { // Predicate_immUExt8neg
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
 //negated imm fits in 8 bit zero extended field
  return ((uint64_t)~N->getZExtValue() + 1) ==
         (uint8_t)((uint64_t)~N->getZExtValue() + 1);

  }
  case 4: { // Predicate_immSExt16
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
 //imm fits in 16 bit sign extended field
  return ((int64_t)N->getZExtValue() << 48) >> 48 ==
         (int64_t)N->getZExtValue();

  }
  case 5: { // Predicate_unindexedload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 6: { // Predicate_load
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::NON_EXTLOAD;

  }
  case 7: { // Predicate_sextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::SEXTLOAD;

  }
  case 8: { // Predicate_sextloadi32
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 9: { // Predicate_zextload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::ZEXTLOAD;

  }
  case 10: { // Predicate_zextloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 11: { // Predicate_zextloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 12: { // Predicate_extload
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getExtensionType() == ISD::EXTLOAD;

  }
  case 13: { // Predicate_extloadi8
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 14: { // Predicate_extloadi16
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 15: { // Predicate_extloadi32
    SDNode *N = Node;

  return cast<LoadSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 16: { // Predicate_unindexedstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getAddressingMode() == ISD::UNINDEXED;

  }
  case 17: { // Predicate_truncstore
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 18: { // Predicate_truncstorei8
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i8;

  }
  case 19: { // Predicate_truncstorei16
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i16;

  }
  case 20: { // Predicate_truncstorei32
    SDNode *N = Node;

  return cast<StoreSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 21: { // Predicate_store
    SDNode *N = Node;

  return !cast<StoreSDNode>(N)->isTruncatingStore();

  }
  case 22: { // Predicate_immFPZ
    ConstantFPSDNode*N = cast<ConstantFPSDNode>(Node);
 //the only fpconstant nodes are +/- 0.0
  (void)N; // silence warning.
  return true;

  }
  case 23: { // Predicate_immRem1
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
return chkRemNearPower2(N->getZExtValue(),1,0);
  }
  case 24: { // Predicate_immRem4
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
return chkRemNearPower2(N->getZExtValue(),4,0);
  }
  case 25: { // Predicate_immRem2
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
return chkRemNearPower2(N->getZExtValue(),2,0);
  }
  case 26: { // Predicate_immRem3
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
return chkRemNearPower2(N->getZExtValue(),3,0);
  }
  case 27: { // Predicate_immRem5
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
return chkRemNearPower2(N->getZExtValue(),5,0);
  }
  case 28: { // Predicate_immRemP2
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  return isPowerOf2_64(N->getZExtValue() -
                         getNearPower2((uint64_t)N->getZExtValue()));

  }
  case 29: { // Predicate_immConst2Part
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  //true if imm fits in a LDAH LDA pair
  int64_t val = (int64_t)N->getZExtValue();
  return (val <= IMM_FULLHIGH  && val >= IMM_FULLLOW);

  }
  case 30: { // Predicate_immSExt16int
    ConstantSDNode*N = cast<ConstantSDNode>(Node);
 //(int)imm fits in a 16 bit sign extended field
  return ((int64_t)N->getZExtValue() << 48) >> 48 ==
         ((int64_t)N->getZExtValue() << 32) >> 32;

  }
  case 31: { // Predicate_immConst2PartInt
    ConstantSDNode*N = cast<ConstantSDNode>(Node);

  //true if imm fits in a LDAH LDA pair with zeroext
  uint64_t uval = N->getZExtValue();
  int32_t val32 = (int32_t)uval;
  return ((uval >> 32) == 0 && //empty upper bits
          val32 <= IMM_FULLHIGH);
//          val32 >= IMM_FULLLOW  + IMM_LOW  * IMM_MULT); //Always True

  }
  case 32: { // Predicate_atomic_cmp_swap_32
    SDNode *N = Node;

  return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;

  }
  case 33: { // Predicate_atomic_cmp_swap_64
    SDNode *N = Node;

  return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;

  }
  case 34: { // Predicate_atomic_load_add_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 35: { // Predicate_atomic_load_add_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  case 36: { // Predicate_atomic_swap_32
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i32;
  
  }
  case 37: { // Predicate_atomic_swap_64
    SDNode *N = Node;

    return cast<AtomicSDNode>(N)->getMemoryVT() == MVT::i64;
  
  }
  }
}

SDValue RunSDNodeXForm(SDValue V, unsigned XFormNo) {
  switch (XFormNo) {
  default: assert(0 && "Invalid xform # in table?");
  case 0: {  // invX
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());
 //invert
  return getI64Imm(~N->getZExtValue());

  }
  case 1: {  // iZAPX
    SDNode *N = V.getNode();
 // get imm to ZAPi
  ConstantSDNode *RHS = cast<ConstantSDNode>(N->getOperand(1));
  return getI64Imm(get_zapImm(SDValue(), RHS->getZExtValue()));

  }
  case 2: {  // negX
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());
 //negate
  return getI64Imm(~N->getZExtValue() + 1);

  }
  case 3: {  // immBRCond
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  switch((uint64_t)N->getZExtValue()) {
    default: assert(0 && "Unknown branch type");
    case 0:  return getI64Imm(Alpha::BEQ);
    case 1:  return getI64Imm(Alpha::BNE);
    case 2:  return getI64Imm(Alpha::BGE);
    case 3:  return getI64Imm(Alpha::BGT);
    case 4:  return getI64Imm(Alpha::BLE);
    case 5:  return getI64Imm(Alpha::BLT);
    case 6:  return getI64Imm(Alpha::BLBS);
    case 7:  return getI64Imm(Alpha::BLBC);
    case 20: return getI64Imm(Alpha::FBEQ);
    case 21: return getI64Imm(Alpha::FBNE);
    case 22: return getI64Imm(Alpha::FBGE);
    case 23: return getI64Imm(Alpha::FBGT);
    case 24: return getI64Imm(Alpha::FBLE);
    case 25: return getI64Imm(Alpha::FBLT);
  }

  }
  case 4: {  // nearP2X
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  return getI64Imm(Log2_64(getNearPower2((uint64_t)N->getZExtValue())));

  }
  case 5: {  // nearP2RemX
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());

  uint64_t x =
    abs64(N->getZExtValue() - getNearPower2((uint64_t)N->getZExtValue()));
  return getI64Imm(Log2_64(x));

  }
  case 6: {  // LL16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());
 //lda part of constant
  return getI64Imm(get_lda16(N->getZExtValue()));

  }
  case 7: {  // LH16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());
 //ldah part of constant (or more if too big)
  return getI64Imm(get_ldah16(N->getZExtValue()));

  }
  case 8: {  // SExt16
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());
 //signed extend int to long
  return getI64Imm(((int64_t)N->getZExtValue() << 48) >> 48);

  }
  case 9: {  // SExt32
    ConstantSDNode *N = cast<ConstantSDNode>(V.getNode());
 //signed extend int to long
  return getI64Imm(((int64_t)N->getZExtValue() << 32) >> 32);

  }
  }
}

